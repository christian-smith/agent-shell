;;; agent-shell-a2a.el --- Local A2A gateway for agent shells -*- lexical-binding: t; -*-

;; This file is part of agent-shell and is licensed under the GPL-3.0-or-later.

;;; Commentary:
;; Opt-in A2A 1.0 JSON-RPC endpoints for existing shell conversations.
;; The listener binds to IPv4 loopback only and requires a bearer token.
;; Loading this library neither starts the listener nor exposes any shells.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--prompt-input-start "agent-shell-prompt")
(declare-function agent-shell--permission-pending-p "agent-shell")
(declare-function agent-shell-elicitation--pending-p "agent-shell-elicitation")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function iso8601-parse "iso8601")
(declare-function shell-maker-busy "shell-maker")
(defvar agent-shell--version)
(defvar agent-shell-codex-app-server-question-hook)

(defvar agent-shell-a2a--server nil
  "Running gateway state, or nil when stopped.")

(defconst agent-shell-a2a--states
  '((submitted . "TASK_STATE_SUBMITTED")
    (working . "TASK_STATE_WORKING")
    (input-required . "TASK_STATE_INPUT_REQUIRED")
    (completed . "TASK_STATE_COMPLETED")
    (failed . "TASK_STATE_FAILED")
    (canceled . "TASK_STATE_CANCELED"))
  "Checked internal task states and their A2A wire values.")

(defconst agent-shell-a2a--operations
  '(("SendMessage" . send) ("SendStreamingMessage" . stream)
    ("GetTask" . get) ("ListTasks" . list)
    ("CancelTask" . cancel) ("SubscribeToTask" . subscribe)
    ("CreateTaskPushNotificationConfig" . push)
    ("GetTaskPushNotificationConfig" . push)
    ("ListTaskPushNotificationConfigs" . push)
    ("DeleteTaskPushNotificationConfig" . push)
    ("GetExtendedAgentCard" . extended-card))
  "A2A wire methods mapped to internal operations at the boundary.")

(defconst agent-shell-a2a--max-body (* 1024 1024)
  "Maximum request body and accumulated task output size in bytes.")

(defconst agent-shell-a2a--max-task-messages 32
  "Maximum caller messages accepted into one task.")

(defconst agent-shell-a2a--max-retained-tasks 128
  "Maximum tasks retained per exposed shell.")

(defconst agent-shell-a2a--max-pending-tasks 16
  "Maximum queued A2A tasks per exposed shell.")

(defconst agent-shell-a2a--max-retained-bytes (* 32 1024 1024)
  "Approximate retained task text per exposed shell, in bytes.
Unfinished tasks are always retained.")

(defconst agent-shell-a2a--task-states
  '("TASK_STATE_UNSPECIFIED" "TASK_STATE_SUBMITTED" "TASK_STATE_WORKING"
    "TASK_STATE_COMPLETED" "TASK_STATE_FAILED" "TASK_STATE_CANCELED"
    "TASK_STATE_INPUT_REQUIRED" "TASK_STATE_REJECTED" "TASK_STATE_AUTH_REQUIRED")
  "Every A2A 1.0 TaskState wire value a ListTasks filter may name.")

(defconst agent-shell-a2a--reserved-inputs '("help" "clear" "config")
  "Inputs that `shell-maker' handles locally instead of sending to the agent.")

(define-error 'agent-shell-a2a-error "A2A request failed")

(defun agent-shell-a2a--reject (code message)
  "Reject a request with JSON-RPC CODE and MESSAGE."
  (signal 'agent-shell-a2a-error (list code message)))

(defun agent-shell-a2a--nonce ()
  "Return an OS-random 256-bit hex identifier, or fail closed."
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (unless (and (executable-find "openssl")
                 (zerop (call-process "openssl" nil t nil "rand" "-hex" "32"))
                 (string-match-p "\\`[0-9a-f]\\{64\\}\n?\\'" (buffer-string)))
      (user-error "A2A requires openssl rand for secure credentials"))
    (string-trim (buffer-string))))

(defun agent-shell-a2a--timestamp (&optional time)
  "Return TIME, or now, as a fixed-width UTC timestamp.
For example, \"2026-10-10T12:00:00.123456Z\".  The fixed width makes
string order match chronological order."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" time t))

(defun agent-shell-a2a--field (object key)
  "Return OBJECT's KEY, treating the ProtoJSON default \"\" as unset.
For example, ((tenant . \"\")) has no tenant."
  (let ((value (map-elt object key)))
    (unless (equal value "") value)))

(defun agent-shell-a2a--terminal-p (task)
  "Return non-nil when TASK has reached a terminal state."
  (memq (map-elt task :state) '(completed failed canceled)))

(defun agent-shell-a2a--settled-p (task)
  "Return non-nil when a blocking send can return TASK."
  (or (agent-shell-a2a--terminal-p task)
      (eq (map-elt task :state) 'input-required)))

(defun agent-shell-a2a--message (task text)
  "Make an agent status message for TASK containing TEXT.
Its messageId extends the random task ID, for example \"ID-status-2\"."
  (map-put! task :status-count (1+ (map-elt task :status-count)))
  `((messageId . ,(format "%s-status-%d" (map-elt task :id) (map-elt task :status-count)))
    (contextId . ,(map-elt task :context-id))
    (taskId . ,(map-elt task :id))
    (role . "ROLE_AGENT") (parts . [((text . ,text))])))

(defun agent-shell-a2a--status (task)
  "Serialize TASK's checked state and latest status message."
  `((state . ,(or (map-elt agent-shell-a2a--states (map-elt task :state))
                 (error "Unknown A2A task state")))
    (timestamp . ,(map-elt task :timestamp))
    ,@(when (map-elt task :status-message)
        `((message . ,(map-elt task :status-message))))))

(defun agent-shell-a2a--artifact (task &optional text)
  "Serialize TASK's response artifact, using TEXT for a streaming delta."
  `((artifactId . ,(map-elt task :id))
    (name . "response")
    (parts . [((text . ,(or text (map-elt task :output))))])))

(defun agent-shell-a2a--task-json (task &optional history-limit omit-artifacts)
  "Serialize TASK, limiting HISTORY-LIMIT and honoring OMIT-ARTIFACTS."
  `((id . ,(map-elt task :id))
    (contextId . ,(map-elt task :context-id))
    (status . ,(agent-shell-a2a--status task))
    ,@(unless omit-artifacts
        `((artifacts . ,(if (string-empty-p (map-elt task :output))
                           []
                         (vector (agent-shell-a2a--artifact task))))))
    ,@(unless (equal history-limit 0)
        `((history . ,(vconcat
                       (if history-limit
                           (last (map-elt task :history) history-limit)
                         (map-elt task :history))))))))

(defun agent-shell-a2a--close (process)
  "Close gateway connection PROCESS and cancel its deadline."
  (when-let* ((timer (process-get process :deadline)))
    (cancel-timer timer)
    (process-put process :deadline nil))
  (when (process-live-p process)
    (delete-process process)))

(defun agent-shell-a2a--write (process text)
  "Write UTF-8 TEXT to live PROCESS without propagating disconnect errors."
  (when (process-live-p process)
    (condition-case nil
        (progn (process-send-string process (encode-coding-string text 'utf-8 t)) t)
      (error (agent-shell-a2a--close process) nil))))

(defun agent-shell-a2a--http-response (process status object &optional headers)
  "Send HTTP STATUS with JSON OBJECT and extra HEADERS to PROCESS, then close it.
HEADERS is a string of CRLF-terminated lines, for example
\"WWW-Authenticate: Bearer\\r\\n\"."
  (let ((body (json-serialize object :null-object nil :false-object :false)))
    (agent-shell-a2a--write
     process
     (format "HTTP/1.1 %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\nCache-Control: no-store\r\nX-Content-Type-Options: nosniff\r\n%s\r\n%s"
             status (string-bytes body) (or headers "") body)))
  (agent-shell-a2a--close process))

(defun agent-shell-a2a--rpc-error (process id code message)
  "Return a JSON-RPC error with ID, CODE and MESSAGE to PROCESS."
  (agent-shell-a2a--http-response
   process "200 OK"
   `((jsonrpc . "2.0") (id . ,id)
     (error . ((code . ,code) (message . ,message))))))

(defun agent-shell-a2a--rpc-result (process id result)
  "Return JSON-RPC RESULT with ID to PROCESS."
  (agent-shell-a2a--http-response
   process "200 OK" `((jsonrpc . "2.0") (id . ,id) (result . ,result))))

(defun agent-shell-a2a--sse (process id result)
  "Send one JSON-RPC RESULT SSE event with ID to PROCESS."
  (agent-shell-a2a--write
   process (concat "data: "
                   (json-serialize `((jsonrpc . "2.0") (id . ,id)
                                     (result . ,result))
                                   :null-object nil :false-object :false)
                   "\n\n")))

(defun agent-shell-a2a--stream (process id task)
  "Attach PROCESS and request ID to TASK's SSE updates."
  (agent-shell-a2a--write
   process "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\nCache-Control: no-store\r\nX-Content-Type-Options: nosniff\r\n\r\n")
  (agent-shell-a2a--sse process id `((task . ,(agent-shell-a2a--task-json task))))
  (if (agent-shell-a2a--terminal-p task)
      (agent-shell-a2a--close process)
    (map-put! task :streams (cons (cons process id) (map-elt task :streams)))))

(defun agent-shell-a2a--publish (task result)
  "Publish streaming RESULT to TASK's connected subscribers."
  (map-put! task :streams
            (seq-filter
             (lambda (entry)
               (agent-shell-a2a--sse (car entry) (cdr entry) result))
             (map-elt task :streams))))

(defun agent-shell-a2a--set-state (task state &optional text)
  "Set TASK to checked STATE, optionally with status TEXT, and notify callers."
  (unless (map-contains-key agent-shell-a2a--states state)
    (error "Unknown A2A state: %s" state))
  (unless (agent-shell-a2a--terminal-p task)
    (map-put! task :state state)
    (map-put! task :timestamp (agent-shell-a2a--timestamp))
    (map-put! task :status-message
              (when text (agent-shell-a2a--message task text)))
    (agent-shell-a2a--publish
     task `((statusUpdate . ((taskId . ,(map-elt task :id))
                            (contextId . ,(map-elt task :context-id))
                            (status . ,(agent-shell-a2a--status task))))))
    (when (agent-shell-a2a--settled-p task)
      (dolist (waiter (map-elt task :waiters))
        (agent-shell-a2a--rpc-result
         (map-elt waiter :process) (map-elt waiter :id)
         `((task . ,(agent-shell-a2a--task-json task (map-elt waiter :history-length))))))
      (map-put! task :waiters nil))
    ;; Interrupted states keep streams open so subscribers see the task resume.
    (when (agent-shell-a2a--terminal-p task)
      (dolist (entry (map-elt task :streams))
        (agent-shell-a2a--close (car entry)))
      (map-put! task :streams nil))))

(defun agent-shell-a2a--finish-cancel (task)
  "Resolve cancellation callers once TASK's actual turn has ended."
  (dolist (entry (map-elt task :cancel-waiters))
    (if (eq (map-elt task :state) 'canceled)
        (agent-shell-a2a--rpc-result (car entry) (cdr entry) (agent-shell-a2a--task-json task))
      (agent-shell-a2a--rpc-error (car entry) (cdr entry) -32002 "Turn did not end with cancellation")))
  (map-put! task :cancel-waiters nil))

(defun agent-shell-a2a--end-task (agent task state &optional text)
  "End AGENT's TASK in terminal STATE with optional TEXT and release the shell."
  (agent-shell-a2a--set-state task state text)
  (agent-shell-a2a--finish-cancel task)
  (map-put! agent :pending-tasks (delq task (map-elt agent :pending-tasks)))
  (when (eq task (map-elt agent :active-task))
    (map-put! agent :active-task nil)))

(defun agent-shell-a2a--compose-draft-p ()
  "Return non-nil when the current shell's viewport holds a compose draft."
  (when-let* ((viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer)
                                                      :existing-only t)))
    (with-current-buffer viewport
      (and (derived-mode-p 'agent-shell-viewport-edit-mode)
           (not (string-empty-p (string-trim (buffer-string))))))))

(defun agent-shell-a2a--ready-p (agent)
  "Return non-nil when AGENT's current buffer can accept a new prompt.
The shell must be idle with a live, empty prompt, no local queued
prompts, and no viewport compose draft."
  (and (not (map-elt agent :active-task))
       (not (map-elt agent :dispatching))
       (not (shell-maker-busy))
       (not (map-elt (agent-shell--state) :pending-prompts))
       (not (agent-shell-a2a--compose-draft-p))
       (when-let* ((start (agent-shell--prompt-input-start)))
         (string-empty-p (string-trim (buffer-substring-no-properties start (point-max)))))))

(defun agent-shell-a2a--dispatch-task (agent task)
  "Submit AGENT's TASK through the normal prompt pipeline."
  (map-put! agent :pending-tasks (delq task (map-elt agent :pending-tasks)))
  (map-put! agent :active-task task)
  (map-put! agent :dispatching t)
  (map-put! agent :dispatched nil)
  (unwind-protect
      (condition-case err
          (progn
            (agent-shell-a2a--set-state task 'working)
            (agent-shell--insert-to-shell-buffer
             :shell-buffer (map-elt agent :buffer)
             :text (agent-shell-a2a--text (car (map-elt task :requests)))
             :submit t :no-focus t)
            (unless (map-elt agent :dispatched)
              (error "The shell did not submit the message to the agent")))
        (error (agent-shell-a2a--end-task agent task 'failed (error-message-string err))))
    (map-put! agent :dispatching nil)))

(defun agent-shell-a2a--withdraw-tasks (agent text)
  "Fail every unfinished task in AGENT with status TEXT."
  (dolist (task (append (map-elt agent :pending-tasks)
                       (when (map-elt agent :active-task) (list (map-elt agent :active-task)))))
    (agent-shell-a2a--end-task agent task 'failed text)))

(defun agent-shell-a2a--pump ()
  "Deliver one queued task per ready shell, preserving unrelated work and drafts.
Runs from the gateway's one-second timer.  A shell whose readiness check
fails has its queued tasks failed, so one shell cannot repeat the error."
  (dolist (agent (copy-sequence (map-elt agent-shell-a2a--server :agents)))
    (when (map-elt agent :pending-tasks)
      (condition-case err
          (if (not (buffer-live-p (map-elt agent :buffer)))
              (progn
                (agent-shell-a2a--withdraw-tasks agent "Shell was closed")
                (agent-shell-a2a--forget agent))
            (with-current-buffer (map-elt agent :buffer)
              (cond
               ((not (equal (map-nested-elt (agent-shell--state) '(:session :id))
                            (map-elt agent :session-id)))
                (agent-shell-a2a--withdraw-tasks agent "Shell session changed before queued delivery"))
               ((agent-shell-a2a--ready-p agent)
                (agent-shell-a2a--dispatch-task agent (car (map-elt agent :pending-tasks)))))))
        (error
         (dolist (task (copy-sequence (map-elt agent :pending-tasks)))
           (agent-shell-a2a--end-task agent task 'failed
                                      (format "Queued delivery failed: %s" (error-message-string err)))))))))

(defun agent-shell-a2a--open-questions (task)
  "Return TASK's forwarded Codex question contexts still awaiting an answer."
  (seq-filter (lambda (context) (memq (map-elt context :status) '(pending answering)))
              (map-elt task :questions)))

(defun agent-shell-a2a--resume (task)
  "Return input-required TASK to working unless the shell awaits a human.
Must run in TASK's shell buffer."
  (unless (or (not (eq (map-elt task :state) 'input-required))
              (agent-shell-a2a--open-questions task)
              (agent-shell--permission-pending-p)
              (agent-shell-elicitation--pending-p))
    (agent-shell-a2a--set-state task 'working)))

(defun agent-shell-a2a--retain (tasks)
  "Return newest-first TASKS within the retention limits.
TASKS is an alist of task IDs to tasks.  Unfinished tasks are kept."
  (let ((count 0) (bytes 0))
    (seq-filter (lambda (entry)
                  (let ((task (cdr entry)))
                    (setq count (1+ count)
                          bytes (+ bytes (map-elt task :input-bytes)
                                   (string-bytes (map-elt task :output))))
                    (or (not (agent-shell-a2a--terminal-p task))
                        (and (<= count agent-shell-a2a--max-retained-tasks)
                             (<= bytes agent-shell-a2a--max-retained-bytes)))))
                tasks)))

(defun agent-shell-a2a--forget (agent)
  "Drop AGENT, whose shell is closing, from the gateway directory."
  (when agent-shell-a2a--server
    (map-put! agent-shell-a2a--server :agents
              (delq agent (map-elt agent-shell-a2a--server :agents)))))

(defun agent-shell-a2a--on-event (agent event)
  "Route a shell EVENT to AGENT's owned task only.
Runs in AGENT's shell buffer, as `agent-shell--emit-event' does."
  (when-let* ((task (map-elt agent :active-task)))
    (if (not (equal (map-nested-elt (agent-shell--state) '(:session :id))
                    (map-elt agent :session-id)))
        (agent-shell-a2a--end-task agent task 'failed "Shell session changed")
      (pcase (map-elt event :event)
        ('agent-message-chunk
         (when-let* (((not (agent-shell-a2a--terminal-p task)))
                     (text (map-nested-elt event '(:data :text-chunk)))
                     ((stringp text)))
           (if (> (+ (string-bytes (map-elt task :output)) (string-bytes text))
                  agent-shell-a2a--max-body)
               (agent-shell-a2a--set-state task 'failed "Task output limit exceeded")
             (agent-shell-a2a--resume task)
             ;; The first chunk creates the artifact; later chunks extend it.
             (let ((append (if (string-empty-p (map-elt task :output)) :false t)))
               (map-put! task :output (concat (map-elt task :output) text))
               (agent-shell-a2a--publish
                task `((artifactUpdate . ((taskId . ,(map-elt task :id))
                                          (contextId . ,(map-elt task :context-id))
                                          (artifact . ,(agent-shell-a2a--artifact task text))
                                          (append . ,append)))))))))
        ('input-submitted
         (cond
          ((map-elt agent :dispatching)
           (map-put! agent :dispatched t))
          ;; Gateway input is never a human's; a late acknowledgement for an
          ;; earlier task must not fail this one.
          ((eq (map-nested-elt event '(:data :source)) 'a2a))
          ;; Codex marks a question `answering' while its answer is delivered.
          ((and (eq (map-nested-elt event '(:data :source)) 'codex-question)
                (seq-find (lambda (context) (eq (map-elt context :status) 'answering))
                          (map-elt task :questions)))
           (agent-shell-a2a--resume task))
          (t
           (agent-shell-a2a--set-state task 'failed "Shell input changed outside this A2A task"))))
        ('permission-request
         (agent-shell-a2a--set-state task 'input-required "Respond to the permission request in Emacs"))
        ('elicitation-request
         (agent-shell-a2a--set-state task 'input-required "Respond to the form in Emacs"))
        ((or 'permission-response 'elicitation-response)
         (agent-shell-a2a--resume task))
        ('turn-complete
         (unless (string-empty-p (map-elt task :output))
           (agent-shell-a2a--publish
            task `((artifactUpdate . ((taskId . ,(map-elt task :id))
                                      (contextId . ,(map-elt task :context-id))
                                      (artifact . ,(agent-shell-a2a--artifact task ""))
                                      (append . t) (lastChunk . t))))))
         (let ((reason (map-nested-elt event '(:data :stop-reason))))
           (pcase reason
             ("end_turn" (agent-shell-a2a--end-task agent task 'completed))
             ("cancelled" (agent-shell-a2a--end-task agent task 'canceled))
             (_ (agent-shell-a2a--end-task agent task 'failed
                                           (format "Agent stopped: %s" (or reason "unknown")))))))
        ('error
         (agent-shell-a2a--end-task agent task 'failed
                                    (or (map-nested-elt event '(:data :message)) "Agent failed")))
        ('clean-up
         (agent-shell-a2a--end-task agent task 'failed "Shell was closed")))))
  (when (eq (map-elt event :event) 'clean-up)
    (agent-shell-a2a--withdraw-tasks agent "Shell was closed")
    (agent-shell-a2a--forget agent)))

(defun agent-shell-a2a--on-codex-question (context)
  "Forward asynchronous Codex question CONTEXT to its owned A2A task."
  (when-let* ((buffer (map-nested-elt context '(:client :context-buffer)))
              (agent (seq-find (lambda (entry) (eq buffer (map-elt entry :buffer)))
                               (map-elt agent-shell-a2a--server :agents)))
              (task (map-elt agent :active-task))
              ((not (agent-shell-a2a--terminal-p task))))
    (map-put! task :questions (cons context (map-elt task :questions)))
    (agent-shell-a2a--set-state
     task 'input-required
     (mapconcat (lambda (question)
                  (concat (map-elt question 'title) "\n"
                          (mapconcat #'identity (map-elt question 'options) "\n")))
                (map-elt context :questions) "\n\n"))
    ;; Keep the question after later status updates replace this message.
    (map-put! task :history (append (map-elt task :history)
                                    (list (map-elt task :status-message))))
    (map-put! task :input-bytes (+ (map-elt task :input-bytes)
                                   (agent-shell-a2a--message-bytes (map-elt task :status-message))))))

(defun agent-shell-a2a--base-url (agent)
  "Return AGENT's local A2A base URL."
  (format "http://127.0.0.1:%s/agents/%s"
          (process-contact (map-elt agent-shell-a2a--server :listener) :service)
          (map-elt agent :id)))

(defun agent-shell-a2a--card (agent)
  "Return AGENT's A2A 1.0 Agent Card."
  `((name . ,(buffer-name (map-elt agent :buffer)))
    (description . "An explicitly exposed, shared Agent Shell conversation")
    (version . ,agent-shell--version)
    (supportedInterfaces . [((url . ,(concat (agent-shell-a2a--base-url agent) "/rpc"))
                            (protocolBinding . "JSONRPC") (protocolVersion . "1.0"))])
    (capabilities . ((streaming . t) (pushNotifications . :false)
                     (extendedAgentCard . :false)))
    (securitySchemes . ((local . ((httpAuthSecurityScheme . ((scheme . "Bearer")))))))
    (securityRequirements . [((schemes . ((local . ((list . []))))))])
    (defaultInputModes . ["text/plain"]) (defaultOutputModes . ["text/plain"])
    (skills . [((id . "conversation") (name . "Agent Shell conversation")
               (description . "Send a text task to this existing agent conversation")
               (tags . ["coding" "conversation"]))])))

;;;###autoload
(defun agent-shell-a2a-start (&optional port)
  "Start the authenticated loopback gateway on PORT, or an unused port.
For example, (agent-shell-a2a-start 9099).  No shells are exposed yet."
  (interactive)
  (when agent-shell-a2a--server
    (user-error "A2A gateway is already running"))
  (let ((token (agent-shell-a2a--nonce)))
    (setq agent-shell-a2a--server
          (list (cons :listener
                      (make-network-process
                       :name "agent-shell-a2a" :server t :host "127.0.0.1"
                       :family 'ipv4 :service (or port t) :noquery t
                       :coding 'binary :filter #'agent-shell-a2a--filter
                       :sentinel #'agent-shell-a2a--sentinel
                       :log #'agent-shell-a2a--accept))
                (cons :token token) (cons :agents nil) (cons :connections nil)
                (cons :pump-timer (run-at-time 1 1 #'agent-shell-a2a--pump)))))
  (add-hook 'agent-shell-codex-app-server-question-hook #'agent-shell-a2a--on-codex-question)
  (message "A2A gateway listening on 127.0.0.1:%s; expose a shell with agent-shell-a2a-expose"
           (process-contact (map-elt agent-shell-a2a--server :listener) :service)))

;;;###autoload
(defun agent-shell-a2a-expose (&optional shell-buffer)
  "Expose initialized SHELL-BUFFER, or the current shell, to local A2A callers.
Return its base URL.  For example, (agent-shell-a2a-expose (current-buffer))."
  (interactive)
  (unless agent-shell-a2a--server (agent-shell-a2a-start))
  (let ((buffer (or shell-buffer (current-buffer))))
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'agent-shell-mode)
                        (map-nested-elt (agent-shell--state) '(:session :id)))))
      (user-error "Select an initialized Agent Shell buffer"))
    (let ((agent (seq-find (lambda (entry) (eq buffer (map-elt entry :buffer)))
                           (map-elt agent-shell-a2a--server :agents))))
      (when (and agent
                 (not (equal (map-elt agent :session-id)
                             (with-current-buffer buffer
                               (map-nested-elt (agent-shell--state) '(:session :id))))))
        (user-error "Shell session changed; run agent-shell-a2a-unexpose before exposing it again"))
      (unless agent
        (setq agent (list (cons :id (agent-shell-a2a--nonce))
                          (cons :context-id (agent-shell-a2a--nonce))
                          (cons :buffer buffer)
                          (cons :session-id (with-current-buffer buffer
                                              (map-nested-elt (agent-shell--state) '(:session :id))))
                          (cons :tasks nil) (cons :active-task nil) (cons :pending-tasks nil)
                          (cons :dispatching nil) (cons :dispatched nil)
                          (cons :subscription nil)))
        (map-put! agent :subscription
                  (agent-shell-subscribe-to
                   :shell-buffer buffer :on-event (lambda (event)
                                                    (agent-shell-a2a--on-event agent event))))
        (map-put! agent-shell-a2a--server :agents
                  (cons agent (map-elt agent-shell-a2a--server :agents))))
      (when (called-interactively-p 'interactive)
        (message "Exposed %s at %s" (buffer-name buffer) (agent-shell-a2a--base-url agent)))
      (agent-shell-a2a--base-url agent))))

;;;###autoload
(defun agent-shell-a2a-unexpose (&optional shell-buffer)
  "Withdraw SHELL-BUFFER, or the current shell, without interrupting its agent."
  (interactive)
  (when-let* ((agent (seq-find
                      (lambda (entry) (eq (or shell-buffer (current-buffer))
                                           (map-elt entry :buffer)))
                      (map-elt agent-shell-a2a--server :agents))))
    (agent-shell-a2a--withdraw-tasks agent "Shell withdrawn from A2A")
    (when (buffer-live-p (map-elt agent :buffer))
      (with-current-buffer (map-elt agent :buffer)
        (agent-shell-unsubscribe :subscription (map-elt agent :subscription))))
    (map-put! agent-shell-a2a--server :agents
              (delq agent (map-elt agent-shell-a2a--server :agents)))))

;;;###autoload
(defun agent-shell-a2a-stop ()
  "Stop the gateway without stopping Emacs or interrupting any agent turns."
  (interactive)
  (when agent-shell-a2a--server
    (cancel-timer (map-elt agent-shell-a2a--server :pump-timer))
    (dolist (agent (copy-sequence (map-elt agent-shell-a2a--server :agents)))
      (agent-shell-a2a-unexpose (map-elt agent :buffer)))
    (dolist (connection (copy-sequence (map-elt agent-shell-a2a--server :connections)))
      (agent-shell-a2a--close connection))
    (delete-process (map-elt agent-shell-a2a--server :listener))
    (remove-hook 'agent-shell-codex-app-server-question-hook #'agent-shell-a2a--on-codex-question)
    (setq agent-shell-a2a--server nil)))

;;;###autoload
(defun agent-shell-a2a-copy-connection-info ()
  "Copy the gateway directory URL and bearer credential to the kill ring.
Share only with trusted local agents: it grants access to all exposed shells."
  (interactive)
  (unless agent-shell-a2a--server (user-error "A2A gateway is not running"))
  (kill-new (format "A2A directory: http://127.0.0.1:%s/agents\nAuthorization: Bearer %s\nA2A-Version: 1.0"
                    (process-contact (map-elt agent-shell-a2a--server :listener) :service)
                    (map-elt agent-shell-a2a--server :token)))
  (message "Copied A2A connection info; the credential grants access to all exposed shells"))

(defun agent-shell-a2a--history-length (object)
  "Validate and return OBJECT's optional historyLength."
  (let ((length (map-elt object 'historyLength)))
    (when (and length (not (and (integerp length) (>= length 0))))
      (agent-shell-a2a--reject -32602 "historyLength must be a nonnegative integer"))
    length))

(defun agent-shell-a2a--find-task (agent params)
  "Find AGENT's task identified by PARAMS, or reject the request."
  (or (and (stringp (map-elt params 'id))
           (map-elt (map-elt agent :tasks) (map-elt params 'id)))
      (agent-shell-a2a--reject -32001 "Task not found")))

(defun agent-shell-a2a--text (message)
  "Validate a text-only A2A MESSAGE and return its prompt."
  (unless (and (listp message) (stringp (map-elt message 'messageId))
               (not (string-empty-p (map-elt message 'messageId)))
               (equal (map-elt message 'role) "ROLE_USER")
               (vectorp (map-elt message 'parts))
               (> (length (map-elt message 'parts)) 0))
    (agent-shell-a2a--reject -32602 "Expected a ROLE_USER message with messageId and parts"))
  (mapconcat
   (lambda (part)
     (unless (and (listp part) (stringp (map-elt part 'text))
                  (not (seq-some (lambda (key) (map-contains-key part key)) '(raw url data)))
                  (member (agent-shell-a2a--field part 'mediaType) '(nil "text/plain")))
       (agent-shell-a2a--reject -32005 "Only text/plain parts are supported"))
     (map-elt part 'text))
   (map-elt message 'parts) "\n"))

(defun agent-shell-a2a--message-bytes (message)
  "Estimate retained bytes for caller MESSAGE, including metadata.
Count its JSON size twice, since a task keeps the message as sent and
normalized.  For example, a 40-byte message counts as 80 bytes."
  (* 2 (string-bytes (json-serialize message :null-object nil :false-object :false))))

(defun agent-shell-a2a--delivery (message)
  "Convert MESSAGE's optional agentShellDelivery metadata to a checked policy."
  (pcase (map-nested-elt message '(metadata agentShellDelivery))
    ((or 'nil "immediate") 'immediate)
    ("queue" 'queue)
    (_ (agent-shell-a2a--reject -32602 "agentShellDelivery must be immediate or queue"))))

(defun agent-shell-a2a--reply-task (process id task configuration streaming)
  "Reply to PROCESS with ID for TASK using CONFIGURATION and STREAMING."
  (cond
   (streaming (agent-shell-a2a--stream process id task))
   ((or (eq (map-elt configuration 'returnImmediately) t)
        (agent-shell-a2a--settled-p task))
    (agent-shell-a2a--rpc-result
     process id `((task . ,(agent-shell-a2a--task-json
                           task (agent-shell-a2a--history-length configuration))))))
   (t (map-put! task :waiters
                (cons `((:process . ,process) (:id . ,id)
                        (:history-length . ,(agent-shell-a2a--history-length configuration)))
                      (map-elt task :waiters))))))

(defun agent-shell-a2a--send (agent params process id streaming)
  "Send PARAMS to AGENT and reply on PROCESS with ID, optionally STREAMING."
  (let* ((message (map-elt params 'message))
         (prompt (agent-shell-a2a--text message))
         (configuration (map-elt params 'configuration))
         (buffer (map-elt agent :buffer))
         (task-id (agent-shell-a2a--field message 'taskId))
         (delivery (agent-shell-a2a--delivery message))
         (task (when task-id
                 (agent-shell-a2a--find-task agent `((id . ,task-id)))))
         (duplicate (seq-find
                     (lambda (entry)
                       (seq-some (lambda (old)
                                   (equal (map-elt old 'messageId) (map-elt message 'messageId)))
                                 (map-elt (cdr entry) :requests)))
                     (map-elt agent :tasks))))
    (unless (and (or (null configuration)
                     (and (listp configuration) (seq-every-p #'consp configuration)))
                 (member (map-elt configuration 'returnImmediately) '(nil t :false)))
      (agent-shell-a2a--reject -32602 "Invalid send configuration"))
    (agent-shell-a2a--history-length configuration)
    (when (map-elt configuration 'taskPushNotificationConfig)
      (agent-shell-a2a--reject -32003 "Push notifications are not supported"))
    (when-let* ((modes (map-elt configuration 'acceptedOutputModes)))
      (unless (and (vectorp modes)
                   (or (zerop (length modes)) (seq-contains-p modes "text/plain")))
        (agent-shell-a2a--reject -32005 "Output is text/plain")))
    (when (and (agent-shell-a2a--field message 'contextId)
               (not (equal (map-elt message 'contextId) (map-elt agent :context-id))))
      (agent-shell-a2a--reject -32602 "This shell has one shared conversation context"))
    (when (and task-id (eq delivery 'queue))
      (agent-shell-a2a--reject -32602 "Queue delivery starts a new task; omit taskId"))
    (cond
     (duplicate
      (unless (seq-some (lambda (old) (equal old message)) (map-elt (cdr duplicate) :requests))
        (agent-shell-a2a--reject -32602 "messageId already used with different content"))
      (agent-shell-a2a--reply-task process id (cdr duplicate) configuration streaming))
     (t
      (with-current-buffer buffer
        (unless (equal (map-nested-elt (agent-shell--state) '(:session :id))
                       (map-elt agent :session-id))
          (agent-shell-a2a--reject -32004 "Shell session changed; withdraw and expose it again"))
        (when (string-empty-p (string-trim prompt))
          (agent-shell-a2a--reject -32602 "Message text is empty"))
        (when (member (string-trim prompt) agent-shell-a2a--reserved-inputs)
          (agent-shell-a2a--reject -32602 "Message text is a reserved local shell command"))
        (if task
            (agent-shell-a2a--steer agent task message prompt process id configuration streaming)
          (when (and (eq delivery 'immediate)
                     (or (map-elt agent :pending-tasks) (not (agent-shell-a2a--ready-p agent))))
            (agent-shell-a2a--reject -32004 "Shell has unrelated work or draft input; use queue delivery or try when idle"))
          (when (>= (length (map-elt agent :pending-tasks)) agent-shell-a2a--max-pending-tasks)
            (agent-shell-a2a--reject -32004 "A2A task queue is full"))
          (setq task (list (cons :id (agent-shell-a2a--nonce))
                           (cons :context-id (map-elt agent :context-id))
                           (cons :state 'submitted) (cons :timestamp (agent-shell-a2a--timestamp))
                           (cons :output "") (cons :status-message nil) (cons :status-count 0)
                           (cons :history nil) (cons :requests (list message))
                           (cons :input-bytes (agent-shell-a2a--message-bytes message))
                           (cons :waiters nil) (cons :streams nil) (cons :steering nil)
                           (cons :questions nil)
                           (cons :canceling nil) (cons :cancel-waiters nil)))
          (map-put! task :history (list (agent-shell-a2a--normalize-message task message)))
          (map-put! agent :tasks (agent-shell-a2a--retain (cons (cons (map-elt task :id) task)
                                                               (map-elt agent :tasks))))
          (if (and (not (map-elt agent :pending-tasks)) (agent-shell-a2a--ready-p agent))
              (agent-shell-a2a--dispatch-task agent task)
            (map-put! agent :pending-tasks (append (map-elt agent :pending-tasks) (list task)))
            (agent-shell-a2a--set-state task 'submitted "Queued; waiting for an idle shell without draft input"))
          (agent-shell-a2a--reply-task process id task configuration streaming)))))))

(defun agent-shell-a2a--steer (agent task message prompt process id configuration streaming)
  "Deliver MESSAGE and PROMPT into AGENT's owned TASK, replying on PROCESS with ID.
CONFIGURATION controls blocking and history.  STREAMING selects SSE."
  (unless (and (eq task (map-elt agent :active-task))
               (not (agent-shell-a2a--terminal-p task)) (shell-maker-busy)
               (not (map-elt task :steering)))
    (agent-shell-a2a--reject -32004 "Task is not accepting active-turn input"))
  (let ((handler (map-nested-elt (agent-shell--state) '(:agent-config :busy-prompt-handler)))
        (bytes (agent-shell-a2a--message-bytes message))
        (buffer (current-buffer))
        answered)
    (when (or (>= (length (map-elt task :requests)) agent-shell-a2a--max-task-messages)
              (> (+ (map-elt task :input-bytes) bytes) (* 2 agent-shell-a2a--max-body)))
      (agent-shell-a2a--reject -32004 "Task message limit reached"))
    (unless (functionp handler)
      (agent-shell-a2a--reject -32004 "This agent does not acknowledge active-turn input"))
    (map-put! task :steering prompt)
    (let ((reject (lambda ()
                    (unless answered
                      (setq answered t)
                      (map-put! task :steering nil)
                      (agent-shell-a2a--rpc-error process id -32004 "Input was not accepted into this task"))))
          (accept (lambda ()
                    (unless answered
                      (setq answered t)
                      (map-put! task :steering nil)
                      (if (or (not (eq task (map-elt agent :active-task)))
                              (agent-shell-a2a--terminal-p task)
                              (not (buffer-live-p buffer)))
                          (agent-shell-a2a--rpc-error process id -32004 "Turn ended before input acknowledgement")
                        (map-put! task :requests (append (map-elt task :requests) (list message)))
                        (map-put! task :input-bytes (+ (map-elt task :input-bytes) bytes))
                        (map-put! task :history
                                  (append (map-elt task :history)
                                          (list (agent-shell-a2a--normalize-message task message))))
                        ;; The caller's reply answers any forwarded Codex question.
                        (dolist (context (agent-shell-a2a--open-questions task))
                          (map-put! context :status 'answered)
                          (when (buffer-live-p (map-elt context :buffer))
                            (kill-buffer (map-elt context :buffer))))
                        (with-current-buffer buffer
                          (agent-shell-a2a--resume task))
                        (agent-shell-a2a--reply-task process id task configuration streaming))))))
      (condition-case nil
          (unless (funcall handler `((:state . ,(agent-shell--state)) (:prompt . ,prompt) (:source . a2a)
                                     (:fallback . ,reject) (:on-success . ,accept)))
            (funcall reject))
        (error (funcall reject))))))

(defun agent-shell-a2a--normalize-message (task message)
  "Copy MESSAGE with TASK's ID and shared context, without duplicate fields.
For example, a message without taskId gains ((taskId . TASK-ID))."
  (map-insert (map-insert (copy-tree message t) 'contextId (map-elt task :context-id))
              'taskId (map-elt task :id)))

(defun agent-shell-a2a--after-time (value)
  "Return ISO 8601 VALUE in the task timestamp format, or reject it.
For example, \"2026-10-10T14:00:00+02:00\" becomes
\"2026-10-10T12:00:00.000000Z\"."
  (require 'iso8601)
  (or (and (stringp value)
           (ignore-errors (agent-shell-a2a--timestamp (encode-time (iso8601-parse value t)))))
      (agent-shell-a2a--reject -32602 "statusTimestampAfter must be an ISO 8601 timestamp")))

(defun agent-shell-a2a--list-tasks (agent params)
  "Return AGENT's filtered task page according to PARAMS.
Tasks are newest status first.  A page token names the last task's
timestamp and ID, so it stays valid after older tasks are evicted."
  (let* ((size (or (map-elt params 'pageSize) 50))
         (status (agent-shell-a2a--field params 'status))
         (state (unless (equal status "TASK_STATE_UNSPECIFIED")
                  (car (rassoc status agent-shell-a2a--states))))
         (context-id (agent-shell-a2a--field params 'contextId))
         (after (when-let* ((value (agent-shell-a2a--field params 'statusTimestampAfter)))
                  (agent-shell-a2a--after-time value)))
         (cursor (when-let* ((token (agent-shell-a2a--field params 'pageToken)))
                   (if (and (stringp token)
                            (string-match "\\`\\([0-9T:.Z-]+\\)~\\([0-9a-f]+\\)\\'" token))
                       (cons (match-string 1 token) (match-string 2 token))
                     (agent-shell-a2a--reject -32602 "Invalid pageToken"))))
         (history (agent-shell-a2a--history-length params))
         (before-p (lambda (task timestamp id)
                     (or (string< (map-elt task :timestamp) timestamp)
                         (and (equal (map-elt task :timestamp) timestamp)
                              (string< (map-elt task :id) id))))))
    (unless (and (integerp size) (<= 1 size 100)
                 (or (not status) (member status agent-shell-a2a--task-states)))
      (agent-shell-a2a--reject -32602 "Invalid pageSize or status"))
    (let* ((tasks (seq-sort
                   (lambda (a b) (funcall before-p b (map-elt a :timestamp) (map-elt a :id)))
                   (seq-filter
                    (lambda (task)
                      (and (or (not context-id) (equal context-id (map-elt task :context-id)))
                           (or (not status) (equal status "TASK_STATE_UNSPECIFIED")
                               (eq state (map-elt task :state)))
                           (or (not after) (not (string< (map-elt task :timestamp) after)))))
                    (mapcar #'cdr (map-elt agent :tasks)))))
           (rest (if cursor
                     (seq-filter (lambda (task) (funcall before-p task (car cursor) (cdr cursor))) tasks)
                   tasks))
           (page (seq-take rest size))
           (last-task (car (last page))))
      `((tasks . ,(vconcat (mapcar (lambda (task)
                                    (agent-shell-a2a--task-json task history
                                                             (not (eq (map-elt params 'includeArtifacts) t))))
                                  page)))
        (nextPageToken . ,(if (> (length rest) size)
                              (concat (map-elt last-task :timestamp) "~" (map-elt last-task :id))
                            ""))
        (pageSize . ,size) (totalSize . ,(length tasks))))))

(defun agent-shell-a2a--cancel (agent task process id)
  "Request cancellation of AGENT's owned TASK, answering PROCESS with ID.
The reply waits until the turn actually ends; queued tasks need no interrupt."
  (if (memq task (map-elt agent :pending-tasks))
      (progn
        (agent-shell-a2a--end-task agent task 'canceled "Canceled before queued delivery")
        (agent-shell-a2a--rpc-result process id (agent-shell-a2a--task-json task)))
    (unless (and (eq task (map-elt agent :active-task))
                 (not (agent-shell-a2a--terminal-p task)))
      (agent-shell-a2a--reject -32002 "Task cannot be canceled"))
    (with-current-buffer (map-elt agent :buffer)
      (unless (equal (map-nested-elt (agent-shell--state) '(:session :id))
                     (map-elt agent :session-id))
        (agent-shell-a2a--reject -32002 "Shell session changed"))
      (unless (map-elt task :canceling)
        (condition-case nil
            (agent-shell-interrupt t)
          (error (agent-shell-a2a--reject -32002 "The agent could not be asked to cancel")))
        (map-put! task :canceling t))
      (cond
       ((not (agent-shell-a2a--terminal-p task))
        (map-put! task :cancel-waiters
                  (cons (cons process id) (map-elt task :cancel-waiters))))
       ((eq (map-elt task :state) 'canceled)
        (agent-shell-a2a--rpc-result process id (agent-shell-a2a--task-json task)))
       (t (agent-shell-a2a--reject -32002 "Turn did not end with cancellation"))))))

(defun agent-shell-a2a--dispatch (agent request process headers)
  "Dispatch JSON-RPC REQUEST to AGENT on PROCESS using HTTP HEADERS."
  (unless (and (listp request) (seq-every-p #'consp request))
    (setq request nil))
  (let ((id (map-elt request 'id)) (params (map-elt request 'params)))
    (condition-case err
        (progn
          (unless (and (listp request) (equal (map-elt request 'jsonrpc) "2.0")
                       (or (stringp id) (integerp id))
                       (stringp (map-elt request 'method))
                       (or (null params) (and (listp params) (seq-every-p #'consp params))))
            (agent-shell-a2a--reject -32600 "Expected a JSON-RPC request object with id"))
          (unless (equal (map-elt headers "a2a-version") "1.0")
            (agent-shell-a2a--reject -32009 "This interface requires A2A-Version: 1.0"))
          (when (agent-shell-a2a--field params 'tenant)
            (agent-shell-a2a--reject -32602 "This interface has no tenant"))
          (pcase (map-elt agent-shell-a2a--operations (map-elt request 'method))
            ((or 'send 'stream)
             (agent-shell-a2a--send agent params process id
                                   (equal (map-elt request 'method) "SendStreamingMessage")))
            ('get
             (agent-shell-a2a--rpc-result
              process id (agent-shell-a2a--task-json (agent-shell-a2a--find-task agent params)
                                                    (agent-shell-a2a--history-length params))))
            ('list (agent-shell-a2a--rpc-result process id (agent-shell-a2a--list-tasks agent params)))
            ('subscribe
             (let ((task (agent-shell-a2a--find-task agent params)))
               (when (agent-shell-a2a--terminal-p task)
                 (agent-shell-a2a--reject -32004 "Cannot subscribe to a terminal task"))
               (agent-shell-a2a--stream process id task)))
            ('cancel
             (agent-shell-a2a--cancel agent (agent-shell-a2a--find-task agent params) process id))
            ('push (agent-shell-a2a--reject -32003 "Push notifications are not supported"))
            ('extended-card (agent-shell-a2a--reject -32004 "Extended cards are not supported"))
            (_ (agent-shell-a2a--reject -32601 "Method not found"))))
      (agent-shell-a2a-error
       (agent-shell-a2a--rpc-error process id (nth 1 err) (nth 2 err)))
      (error (agent-shell-a2a--rpc-error process id -32603 "Internal error")))))

(defun agent-shell-a2a--accept (_server process _message)
  "Initialize newly accepted gateway PROCESS; ignore _SERVER and _MESSAGE."
  (if (>= (length (map-elt agent-shell-a2a--server :connections)) 32)
      (agent-shell-a2a--http-response process "503 Service Unavailable" '((error . "Too many connections")))
    (set-process-query-on-exit-flag process nil)
    (process-put process :data "")
    (process-put process :deadline (run-at-time 30 nil #'agent-shell-a2a--close process))
    (map-put! agent-shell-a2a--server :connections
              (cons process (map-elt agent-shell-a2a--server :connections)))))

(defun agent-shell-a2a--sentinel (process _event)
  "Remove closed PROCESS from gateway subscriptions; ignore _EVENT."
  (unless (process-live-p process)
    (when-let* ((timer (process-get process :deadline))) (cancel-timer timer))
    (when agent-shell-a2a--server
      (map-put! agent-shell-a2a--server :connections
                (delq process (map-elt agent-shell-a2a--server :connections)))
      (dolist (agent (map-elt agent-shell-a2a--server :agents))
        (dolist (entry (map-elt agent :tasks))
          (let ((task (cdr entry)))
            (map-put! task :streams (seq-remove (lambda (stream) (eq process (car stream)))
                                                (map-elt task :streams)))
            (map-put! task :waiters (seq-remove (lambda (waiter) (eq process (map-elt waiter :process)))
                                                (map-elt task :waiters)))
            (map-put! task :cancel-waiters (seq-remove (lambda (waiter) (eq process (car waiter)))
                                                       (map-elt task :cancel-waiters)))))))))

(defun agent-shell-a2a--parse-head (text)
  "Parse bounded HTTP head TEXT, rejecting ambiguous request framing."
  (let* ((lines (split-string text "\r\n"))
         (line (pop lines)) headers)
    (unless (string-match "\\`\\(GET\\|POST\\) \\(/[^ ]*\\) HTTP/1\\.[01]\\'" line)
      (error "Invalid request line"))
    (let ((method (match-string 1 line)) (path (match-string 2 line)))
      (dolist (header lines)
        (unless (string-match "\\`\\([A-Za-z0-9-]+\\):[ \t]*\\([^\r\n]*\\)\\'" header)
          (error "Invalid header"))
        (let ((key (downcase (match-string 1 header)))
              (value (string-trim (match-string 2 header))))
          (when (map-contains-key headers key) (error "Duplicate header"))
          (push (cons key value) headers)))
      (when (map-contains-key headers "transfer-encoding") (error "Unsupported transfer encoding"))
      (let ((length (or (map-elt headers "content-length") "0")))
        (unless (and (string-match-p "\\`[0-9]+\\'" length)
                     (<= (length length) 7)
                     (<= (string-to-number length) agent-shell-a2a--max-body)
                     (or (equal method "GET") (map-contains-key headers "content-length")))
          (error "Invalid content length"))
        `((:method . ,method) (:path . ,path) (:headers . ,headers)
          (:length . ,(string-to-number length)))))))

(defun agent-shell-a2a--authorized-p (value)
  "Return non-nil when Authorization header VALUE carries the gateway token.
The scheme is case-insensitive, for example \"bearer TOKEN\"."
  (let ((case-fold-search t))
    (and (stringp value)
         (string-match "\\`bearer[ \t]+\\([^ \t]+\\)\\'" value)
         (equal (match-string 1 value) (map-elt agent-shell-a2a--server :token)))))

(defun agent-shell-a2a--refuse (process head)
  "Answer PROCESS and return non-nil unless HEAD is an authorized loopback request.
Runs before any body is buffered."
  (let ((headers (map-elt head :headers))
        (port (process-contact (map-elt agent-shell-a2a--server :listener) :service)))
    (cond
     ((or (map-contains-key headers "origin")
          (not (member (map-elt headers "host") (list (format "127.0.0.1:%s" port)
                                                       (format "localhost:%s" port)))))
      (agent-shell-a2a--http-response process "403 Forbidden" '((error . "Only direct loopback clients are accepted")))
      t)
     ((not (agent-shell-a2a--authorized-p (map-elt headers "authorization")))
      (agent-shell-a2a--http-response process "401 Unauthorized" '((error . "Bearer credential required"))
                                      "WWW-Authenticate: Bearer\r\n")
      t))))

(defun agent-shell-a2a--route (process head body)
  "Route parsed HTTP HEAD and BODY from authorized PROCESS."
  (let ((headers (map-elt head :headers))
        (path (map-elt head :path)))
    (cond
     ((and (equal (map-elt head :method) "GET") (equal path "/agents"))
      (agent-shell-a2a--http-response
       process "200 OK"
       `((agents . ,(vconcat
                     (mapcar (lambda (agent)
                               `((name . ,(buffer-name (map-elt agent :buffer)))
                                 (url . ,(agent-shell-a2a--base-url agent))))
                             (seq-filter (lambda (agent) (buffer-live-p (map-elt agent :buffer)))
                                         (map-elt agent-shell-a2a--server :agents))))))))
     ((string-match "\\`/agents/\\([0-9a-f]+\\)/\\(rpc\\|\\.well-known/agent-card\\.json\\)\\'" path)
      (let* ((agent-id (match-string 1 path)) (resource (match-string 2 path))
             (agent (seq-find (lambda (entry) (equal agent-id (map-elt entry :id)))
                              (map-elt agent-shell-a2a--server :agents))))
        (cond
         ((not (and agent (buffer-live-p (map-elt agent :buffer))))
          (agent-shell-a2a--http-response process "404 Not Found" '((error . "Agent not found"))))
         ((and (equal resource ".well-known/agent-card.json") (equal (map-elt head :method) "GET"))
          (agent-shell-a2a--http-response process "200 OK" (agent-shell-a2a--card agent)))
         ((and (equal resource "rpc") (equal (map-elt head :method) "POST")
               (equal (downcase (string-trim (car (split-string (or (map-elt headers "content-type") "") ";"))))
                      "application/json"))
          (let ((request (condition-case nil
                             (json-parse-string (decode-coding-string body 'utf-8 t)
                                                :object-type 'alist :array-type 'array
                                                :null-object nil :false-object :false)
                           (error :invalid))))
            (if (eq request :invalid)
                (agent-shell-a2a--rpc-error process nil -32700 "Parse error")
              (agent-shell-a2a--dispatch agent request process headers))))
         (t (agent-shell-a2a--http-response process "400 Bad Request" '((error . "Expected GET card or POST application/json RPC")))))))
     (t (agent-shell-a2a--http-response process "404 Not Found" '((error . "Resource not found")))))))

(defun agent-shell-a2a--filter (process chunk)
  "Accumulate bounded HTTP CHUNK on PROCESS and dispatch one complete request.
Unauthorized requests are answered from their head alone."
  (unless (process-get process :dispatched)
    (condition-case nil
        (let ((data (concat (process-get process :data) chunk))
              (head (process-get process :head)))
          (unless head
            (if-let* ((end (string-match "\r\n\r\n" data)))
                (progn
                  (when (> end 16384) (error "Headers too large"))
                  (setq head (agent-shell-a2a--parse-head (substring data 0 end))
                        data (substring data (+ end 4)))
                  (process-put process :head head)
                  (if (agent-shell-a2a--refuse process head)
                      (process-put process :dispatched t)
                    (when (and (equal (downcase (or (map-nested-elt head '(:headers "expect")) ""))
                                      "100-continue")
                               (< (length data) (map-elt head :length)))
                      (agent-shell-a2a--write process "HTTP/1.1 100 Continue\r\n\r\n"))))
              (when (> (length data) 16384) (error "Headers too large"))))
          (unless (process-get process :dispatched)
            (when (> (length data) agent-shell-a2a--max-body) (error "Body too large"))
            (process-put process :data data)
            (when (and head (>= (length data) (map-elt head :length)))
              (unless (= (length data) (map-elt head :length)) (error "Pipelining is not supported"))
              (process-put process :dispatched t)
              (process-put process :data nil)
              (cancel-timer (process-get process :deadline))
              (process-put process :deadline (run-at-time 300 nil #'agent-shell-a2a--close process))
              (agent-shell-a2a--route process head data))))
      (error (agent-shell-a2a--http-response process "400 Bad Request" '((error . "Invalid HTTP request")))))))

(provide 'agent-shell-a2a)
;;; agent-shell-a2a.el ends here
