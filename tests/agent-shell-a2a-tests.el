;;; agent-shell-a2a-tests.el --- A2A behavioral tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Real loopback HTTP connections and an in-memory shell backend.
;; No agent is launched and no session files are created.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)
(require 'agent-shell-codex-app-server)

(defvar-local agent-shell-a2a-tests--busy nil)
(defvar agent-shell-a2a-tests--sent nil)
(defvar agent-shell-a2a-tests--interrupted nil)

(defmacro agent-shell-a2a-tests--with-gateway (&rest body)
  "Run BODY with a loopback gateway and an in-memory exposed shell."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-a2a--server nil)
         (agent-shell-inhibit-system-sleep nil)
         (agent-shell-a2a-tests--sent nil)
         (agent-shell-a2a-tests--interrupted nil)
         (shell (generate-new-buffer " *a2a-test-shell*"))
         (nonce 0))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-a2a--nonce)
                    (lambda () (format "%064x" (cl-incf nonce))))
                   ((symbol-function 'shell-maker-busy)
                    (lambda () agent-shell-a2a-tests--busy))
                   ((symbol-function 'agent-shell--prompt-input-start)
                    (lambda () (point-min)))
                   ((symbol-function 'agent-shell--insert-to-shell-buffer)
                    (lambda (&rest args)
                      (with-current-buffer (plist-get args :shell-buffer)
                        (push (plist-get args :text) agent-shell-a2a-tests--sent)
                        (setq agent-shell-a2a-tests--busy t)
                        (agent-shell--emit-event :event 'input-submitted
                                                 :data `((:prompt . ,(plist-get args :text)))))))
                   ((symbol-function 'agent-shell-interrupt)
                    (lambda (&optional force)
                      (should force)
                      (setq agent-shell-a2a-tests--interrupted (current-buffer)))))
           (with-current-buffer shell
             (setq-local major-mode 'agent-shell-mode)
             (setq-local agent-shell--state
                         `((:buffer . ,shell) (:session . ((:id . "session-test")))
                           (:event-subscriptions . nil) (:agent-config . nil))))
           (agent-shell-a2a-start)
           (agent-shell-a2a-expose shell)
           ,@body)
       (agent-shell-a2a-stop)
       (when (buffer-live-p shell) (kill-buffer shell)))))

(defun agent-shell-a2a-tests--agent ()
  "Return the fixture's exposed agent."
  (car (map-elt agent-shell-a2a--server :agents)))

(defun agent-shell-a2a-tests--open (wire &optional fragments)
  "Open a real HTTP connection, sending WIRE in optional FRAGMENTS."
  (let* ((state (list (cons :wire "") (cons :process nil)))
         (process (make-network-process
                   :name "a2a-test-client" :host "127.0.0.1" :family 'ipv4
                   :service (process-contact (map-elt agent-shell-a2a--server :listener) :service)
                   :noquery t :coding 'binary
                   :filter (lambda (_process chunk)
                             (map-put! state :wire (concat (map-elt state :wire) chunk))))))
    (map-put! state :process process)
    (dolist (fragment (or fragments (list wire)))
      (process-send-string process (encode-coding-string fragment 'utf-8 t))
      (accept-process-output nil 0.01))
    state))

(defun agent-shell-a2a-tests--settle (connection &optional closed)
  "Wait boundedly for CONNECTION's response, optionally until CLOSED."
  (let ((deadline (+ (float-time) 2)))
    (while (and (< (float-time) deadline)
                (if closed (process-live-p (map-elt connection :process))
                  (not (string-match-p "\r\n\r\n" (map-elt connection :wire)))))
      (accept-process-output nil 0.01)))
  (map-elt connection :wire))

(defun agent-shell-a2a-tests--wire (path &optional request headers method)
  "Build a fixture HTTP request for PATH using REQUEST, HEADERS and METHOD."
  (let ((body (if request (json-serialize request :false-object :false :null-object nil) ""))
        (port (process-contact (map-elt agent-shell-a2a--server :listener) :service)))
    (format "%s %s HTTP/1.1\r\nHost: 127.0.0.1:%s\r\nAuthorization: Bearer %s\r\nA2A-Version: 1.0\r\nContent-Type: application/json\r\nContent-Length: %s\r\n%s\r\n%s"
            (or method (if request "POST" "GET")) path port
            (map-elt agent-shell-a2a--server :token) (string-bytes body)
            (or headers "") body)))

(defun agent-shell-a2a-tests--rpc (method &optional params headers)
  "Send A2A METHOD with PARAMS and optional HEADERS over real HTTP."
  (agent-shell-a2a-tests--open
   (agent-shell-a2a-tests--wire
    (concat "/agents/" (map-elt (agent-shell-a2a-tests--agent) :id) "/rpc")
    `((jsonrpc . "2.0") (id . 1) (method . ,method) (params . ,params)) headers)))

(defun agent-shell-a2a-tests--json (connection)
  "Read CONNECTION's JSON response after the server closes it."
  (let ((wire (agent-shell-a2a-tests--settle connection t)))
    (should (string-match "\r\n\r\n" wire))
    (json-parse-string (decode-coding-string (substring wire (match-end 0)) 'utf-8)
                      :object-type 'alist :array-type 'array :false-object :false)))

(defun agent-shell-a2a-tests--send-params (&optional message-id task-id configuration text)
  "Make text send parameters using MESSAGE-ID, TASK-ID, CONFIGURATION and TEXT."
  `((message . ((messageId . ,(or message-id "message-1")) (role . "ROLE_USER")
               (parts . [((text . ,(or text "Please inspect the parser")))])
               ,@(when task-id `((taskId . ,task-id)))))
    (configuration . ,(or configuration '((returnImmediately . t))))))

(defun agent-shell-a2a-tests--task (&optional params)
  "Send PARAMS and return the created task's wire representation."
  (map-nested-elt (agent-shell-a2a-tests--json
                   (agent-shell-a2a-tests--rpc "SendMessage"
                                              (or params (agent-shell-a2a-tests--send-params))))
                  '(result task)))

(defun agent-shell-a2a-tests--event (shell event &optional data)
  "Emit EVENT with DATA from fixture SHELL."
  (with-current-buffer shell (agent-shell--emit-event :event event :data data)))

(ert-deftest agent-shell-a2a-is-opt-in-and-loopback-test ()
  (should-not agent-shell-a2a--server)
  (agent-shell-a2a-tests--with-gateway
    (should (equal (seq-take (process-contact (map-elt agent-shell-a2a--server :listener) :local) 4)
                   [127 0 0 1]))
    (agent-shell-a2a-unexpose shell)
    (should-not (map-elt agent-shell-a2a--server :agents))
    (should-not agent-shell-a2a-tests--interrupted)))

(ert-deftest agent-shell-a2a-authentication-and-browser-boundary-test ()
  (agent-shell-a2a-tests--with-gateway
    (dolist (change '(credential host origin))
      (let ((wire (agent-shell-a2a-tests--wire "/agents")))
        (setq wire (pcase change
                     ('credential (replace-regexp-in-string "Authorization: [^\r\n]*" "Authorization: Bearer wrong" wire))
                     ('host (replace-regexp-in-string "Host: [^\r\n]*" "Host: arbitrary.example" wire))
                     ('origin (replace-regexp-in-string "A2A-Version:" "Origin: http://example.com\r\nA2A-Version:" wire))))
        (should (string-match-p (if (eq change 'credential) "401 Unauthorized" "403 Forbidden")
                                 (agent-shell-a2a-tests--settle (agent-shell-a2a-tests--open wire) t)))))
    (should-not agent-shell-a2a-tests--sent)))

(ert-deftest agent-shell-a2a-discovery-contract-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((directory (agent-shell-a2a-tests--json
                       (agent-shell-a2a-tests--open (agent-shell-a2a-tests--wire "/agents"))))
           (card (agent-shell-a2a-tests--json
                  (agent-shell-a2a-tests--open
                   (agent-shell-a2a-tests--wire
                    (concat "/agents/" (map-elt (agent-shell-a2a-tests--agent) :id)
                            "/.well-known/agent-card.json"))))))
      (should (= 1 (length (map-elt directory 'agents))))
      (should (equal (map-nested-elt (aref (map-elt card 'supportedInterfaces) 0) '(protocolVersion)) "1.0"))
      (should (equal (map-nested-elt card '(securitySchemes local httpAuthSecurityScheme scheme)) "Bearer"))
      (should (eq (map-nested-elt card '(capabilities pushNotifications)) :false)))))

(ert-deftest agent-shell-a2a-send-poll-and-completion-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (should (equal agent-shell-a2a-tests--sent '("Please inspect the parser")))
      (should (equal (map-nested-elt task '(status state)) "TASK_STATE_WORKING"))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "The parser is correct.")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let ((result (map-elt (agent-shell-a2a-tests--json
                             (agent-shell-a2a-tests--rpc "GetTask" `((id . ,(map-elt task 'id)) (historyLength . 0))))
                            'result)))
        (should (equal (map-nested-elt result '(status state)) "TASK_STATE_COMPLETED"))
        (should-not (map-contains-key result 'history))
        (should (equal (map-nested-elt (aref (map-elt result 'artifacts) 0) '(parts))
                       [((text . "The parser is correct."))]))))))

(ert-deftest agent-shell-a2a-blocking-send-is-asynchronous-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                (agent-shell-a2a-tests--send-params nil nil '((returnImmediately . :false))))))
      (should (string-empty-p (map-elt connection :wire)))
      (should (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :waiters))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task status state))
                     "TASK_STATE_COMPLETED")))))

(ert-deftest agent-shell-a2a-refuses-unrelated-work-and-drafts-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "SendMessage" (agent-shell-a2a-tests--send-params)))
                                    '(error code))))
    (with-current-buffer shell
      (setq agent-shell-a2a-tests--busy nil)
      (insert "Unsubmitted draft"))
    (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "SendMessage" (agent-shell-a2a-tests--send-params)))
                                    '(error code))))
    (should-not agent-shell-a2a-tests--sent)
    (should (equal (with-current-buffer shell (buffer-string)) "Unsubmitted draft"))))

(defun agent-shell-a2a-tests--queue-params (id text)
  "Make a queued task request with message ID and TEXT."
  (let ((params (agent-shell-a2a-tests--send-params id nil nil text)))
    (map-put! params 'message (map-insert (map-elt params 'message) 'metadata
                                        '((agentShellDelivery . "queue"))))
    params))

(ert-deftest agent-shell-a2a-queued-task-waits-for-idle-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let ((task (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params "queued-1" "Ping"))))
      (should (equal (map-nested-elt task '(status state)) "TASK_STATE_SUBMITTED"))
      (should-not agent-shell-a2a-tests--sent)
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Unrelated output")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))
      (agent-shell-a2a--pump)
      (should (equal agent-shell-a2a-tests--sent '("Ping")))
      (should (equal (map-elt (agent-shell-a2a-tests--agent) :pending-tasks) nil))
      (should (string-empty-p (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :output)))
      (should (equal (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "GetTask" `((id . ,(map-elt task 'id)))))
                                    '(result status state)) "TASK_STATE_WORKING")))))

(ert-deftest agent-shell-a2a-queued-task-preserves-draft-and-local-queue-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (insert "Human draft"))
    (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params "queued-1" "Ping"))
    (agent-shell-a2a--pump)
    (should-not agent-shell-a2a-tests--sent)
    (should (equal (with-current-buffer shell (buffer-string)) "Human draft"))
    (with-current-buffer shell
      (erase-buffer)
      (setq agent-shell--state (map-insert agent-shell--state :pending-prompts '("Human queued input"))))
    (agent-shell-a2a--pump)
    (should-not agent-shell-a2a-tests--sent)
    (should (equal (with-current-buffer shell (map-elt agent-shell--state :pending-prompts))
                   '("Human queued input")))
    (with-current-buffer shell (map-put! agent-shell--state :pending-prompts nil))
    (agent-shell-a2a--pump)
    (should (equal agent-shell-a2a-tests--sent '("Ping")))))

(ert-deftest agent-shell-a2a-queued-tasks-are-fifo-and-idempotent-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let* ((params (agent-shell-a2a-tests--queue-params "queued-1" "First"))
           (first (agent-shell-a2a-tests--task params)))
      (should (equal (map-elt first 'id) (map-elt (agent-shell-a2a-tests--task params) 'id)))
      (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params "queued-2" "Second"))
      (should (= 2 (length (map-elt (agent-shell-a2a-tests--agent) :pending-tasks))))
      (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))
      (agent-shell-a2a--pump)
      (agent-shell-a2a--pump)
      (should (equal agent-shell-a2a-tests--sent '("First")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))
      (agent-shell-a2a--pump)
      (should (equal (reverse agent-shell-a2a-tests--sent) '("First" "Second"))))))

(ert-deftest agent-shell-a2a-queued-task-can-be-canceled-without-interruption-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let ((task (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params "queued-1" "Ping"))))
      (should (equal (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task 'id)))))
                                    '(result status state)) "TASK_STATE_CANCELED"))
      (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))
      (agent-shell-a2a--pump)
      (should-not agent-shell-a2a-tests--sent)
      (should-not agent-shell-a2a-tests--interrupted))))

(ert-deftest agent-shell-a2a-queued-task-fails-on-session-change-or-withdrawal-test ()
  (dolist (reason '(session withdrawn closed))
    (agent-shell-a2a-tests--with-gateway
      (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
      (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params "queued-1" "Ping"))
      (let ((task (car (map-elt (agent-shell-a2a-tests--agent) :pending-tasks))))
        (pcase reason
          ('session
           (with-current-buffer shell (map-put! (map-elt agent-shell--state :session) :id "changed"))
           (agent-shell-a2a--pump))
          ('withdrawn (agent-shell-a2a-unexpose shell))
          ('closed (agent-shell-a2a-tests--event shell 'clean-up)))
        (should (eq (map-elt task :state) 'failed))
        (should-not agent-shell-a2a-tests--sent)
        (should-not agent-shell-a2a-tests--interrupted)))))

(ert-deftest agent-shell-a2a-queue-capacity-and-policy-are-checked-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let ((agent-shell-a2a--max-pending-tasks 1))
      (let ((task (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params "queued-1" "Ping"))))
        (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json
                                          (agent-shell-a2a-tests--rpc "SendMessage"
                                                                    (agent-shell-a2a-tests--queue-params "queued-2" "Second")))
                                         '(error code))))
        (let ((params (agent-shell-a2a-tests--queue-params "queued-3" "Follow-up")))
          (map-put! params 'message (map-insert (map-elt params 'message) 'taskId (map-elt task 'id)))
          (should (= -32602 (map-nested-elt (agent-shell-a2a-tests--json
                                            (agent-shell-a2a-tests--rpc "SendMessage" params)) '(error code)))))))
    (should-not agent-shell-a2a-tests--sent)))

(defun agent-shell-a2a-tests--queued (shell id text)
  "Queue a task with message ID and TEXT while fixture SHELL is busy."
  (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
  (agent-shell-a2a-tests--task (agent-shell-a2a-tests--queue-params id text)))

(defun agent-shell-a2a-tests--idle (shell)
  "Make fixture SHELL idle and run one readiness check."
  (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))
  (agent-shell-a2a--pump))

(ert-deftest agent-shell-a2a-queue-is-not-overtaken-by-immediate-send-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--queued shell "queued-1" "First")
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))
    (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc
                                      "SendMessage" (agent-shell-a2a-tests--send-params "immediate-1" nil nil "Second")))
                                    '(error code))))
    (agent-shell-a2a--pump)
    (should (equal agent-shell-a2a-tests--sent '("First")))))

(ert-deftest agent-shell-a2a-queue-waits-for-compose-draft-and-live-prompt-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((viewport (get-buffer-create (concat (buffer-name shell) agent-shell-viewport--suffix))))
      (unwind-protect
          (progn
            (with-current-buffer viewport
              (setq-local major-mode 'agent-shell-viewport-edit-mode)
              (insert "Human compose draft"))
            (agent-shell-a2a-tests--queued shell "queued-1" "Ping")
            (agent-shell-a2a-tests--idle shell)
            (should-not agent-shell-a2a-tests--sent)
            (should (equal (with-current-buffer viewport (buffer-string)) "Human compose draft"))
            (with-current-buffer viewport (erase-buffer))
            (cl-letf (((symbol-function 'agent-shell--prompt-input-start) #'ignore))
              (agent-shell-a2a--pump))
            (should-not agent-shell-a2a-tests--sent)
            (agent-shell-a2a--pump)
            (should (equal agent-shell-a2a-tests--sent '("Ping"))))
        (kill-buffer viewport)))))

(ert-deftest agent-shell-a2a-queued-blocking-send-collects-only-its-turn-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let ((connection (agent-shell-a2a-tests--rpc
                       "SendMessage"
                       (let ((params (agent-shell-a2a-tests--queue-params "queued-1" "Ping")))
                         (map-put! params 'configuration '((returnImmediately . :false)))
                         params))))
      (accept-process-output nil 0.05)
      (should (string-empty-p (map-elt connection :wire)))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Unrelated output")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (should (string-empty-p (map-elt connection :wire)))
      (agent-shell-a2a-tests--idle shell)
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Pong")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let ((task (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task))))
        (should (equal (map-nested-elt task '(status state)) "TASK_STATE_COMPLETED"))
        (should (equal (map-nested-elt (aref (map-elt task 'artifacts) 0) '(parts))
                       [((text . "Pong"))]))))))

(ert-deftest agent-shell-a2a-queued-stream-reports-submission-then-delivery-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let ((connection (agent-shell-a2a-tests--rpc "SendStreamingMessage"
                                                (agent-shell-a2a-tests--queue-params "queued-1" "Ping"))))
      (agent-shell-a2a-tests--settle connection)
      (agent-shell-a2a-tests--idle shell)
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let ((events (seq-keep (lambda (line)
                                (when (string-prefix-p "data: " line)
                                  (json-parse-string (substring line 6) :object-type 'alist)))
                              (split-string (agent-shell-a2a-tests--settle connection t) "\n"))))
        (should (equal (map-nested-elt (car events) '(result task status state)) "TASK_STATE_SUBMITTED"))
        (should (equal (seq-keep (lambda (event) (map-nested-elt event '(result statusUpdate status state)))
                                 (cdr events))
                       '("TASK_STATE_WORKING" "TASK_STATE_COMPLETED")))))))

(ert-deftest agent-shell-a2a-canceled-queued-task-answers-blocked-sender-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (setq agent-shell-a2a-tests--busy t))
    (let* ((connection (agent-shell-a2a-tests--rpc
                        "SendMessage"
                        (let ((params (agent-shell-a2a-tests--queue-params "queued-1" "Ping")))
                          (map-put! params 'configuration '((returnImmediately . :false)))
                          params)))
           (task (progn (accept-process-output nil 0.05)
                        (car (map-elt (agent-shell-a2a-tests--agent) :pending-tasks)))))
      (agent-shell-a2a-tests--json (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task :id)))))
      (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task status state))
                     "TASK_STATE_CANCELED"))
      (should-not agent-shell-a2a-tests--interrupted))))

(ert-deftest agent-shell-a2a-failed-queued-submission-advances-queue-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--queued shell "queued-1" "First")
    (agent-shell-a2a-tests--queued shell "queued-2" "Second")
    (let ((first (car (map-elt (agent-shell-a2a-tests--agent) :pending-tasks))))
      (cl-letf (((symbol-function 'agent-shell--insert-to-shell-buffer) #'ignore))
        (agent-shell-a2a-tests--idle shell))
      (should (eq (map-elt first :state) 'failed))
      (should-not (map-elt (agent-shell-a2a-tests--agent) :active-task))
      (agent-shell-a2a--pump)
      (should (equal agent-shell-a2a-tests--sent '("Second"))))))

(ert-deftest agent-shell-a2a-queue-failures-are-contained-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--queued shell "queued-1" "Ping")
    (let ((task (car (map-elt (agent-shell-a2a-tests--agent) :pending-tasks))))
      (cl-letf (((symbol-function 'agent-shell-a2a--ready-p) (lambda (_agent) (error "Readiness broke"))))
        (agent-shell-a2a--pump))
      (should (eq (map-elt task :state) 'failed))
      (should-not (map-elt (agent-shell-a2a-tests--agent) :pending-tasks)))
    (agent-shell-a2a-tests--queued shell "queued-2" "Ping")
    (let ((task (car (map-elt (agent-shell-a2a-tests--agent) :pending-tasks))))
      (kill-buffer shell)
      (agent-shell-a2a--pump)
      (should (eq (map-elt task :state) 'failed))
      (should-not (map-elt agent-shell-a2a--server :agents)))))

(ert-deftest agent-shell-a2a-stop-cancels-readiness-timer-and-fails-queue-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--queued shell "queued-1" "Ping")
    (let ((task (car (map-elt (agent-shell-a2a-tests--agent) :pending-tasks)))
          (timer (map-elt agent-shell-a2a--server :pump-timer)))
      (should (memq timer timer-list))
      (agent-shell-a2a-stop)
      (should-not (memq timer timer-list))
      (should (eq (map-elt task :state) 'failed))
      (should-not agent-shell-a2a-tests--interrupted))))

(ert-deftest agent-shell-a2a-task-and-context-isolation-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((params (agent-shell-a2a-tests--send-params)))
      (setf (map-elt params 'message)
            (map-insert (map-elt params 'message) 'contextId "other-conversation"))
      (should (= -32602 (map-nested-elt (agent-shell-a2a-tests--json
                                       (agent-shell-a2a-tests--rpc "SendMessage" params)) '(error code)))))
    (should (= -32001 (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "GetTask" '((id . "missing")))) '(error code))))
    (let ((task (agent-shell-a2a-tests--task))
          (other (generate-new-buffer " *a2a-other-shell*")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (setq-local major-mode 'agent-shell-mode)
              (setq-local agent-shell--state `((:buffer . ,other) (:session . ((:id . "other")))
                                               (:event-subscriptions . nil))))
            (agent-shell-a2a-expose other)
            (should (= -32001 (map-nested-elt (agent-shell-a2a-tests--json
                                             (agent-shell-a2a-tests--rpc "GetTask" `((id . ,(map-elt task 'id))))) '(error code)))))
        (agent-shell-a2a-unexpose other) (kill-buffer other)))))

(ert-deftest agent-shell-a2a-message-idempotency-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (should (equal (map-elt task 'id) (map-elt (agent-shell-a2a-tests--task) 'id)))
      (should (= 1 (length agent-shell-a2a-tests--sent)))
      (should (= -32602 (map-nested-elt
                        (agent-shell-a2a-tests--json
                         (agent-shell-a2a-tests--rpc "SendMessage"
                                                    (agent-shell-a2a-tests--send-params nil nil nil "Changed text")))
                        '(error code)))))))

(ert-deftest agent-shell-a2a-steers-only-owned-task-after-acknowledgement-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((task (agent-shell-a2a-tests--task)) callback
           (handler (lambda (event) (setq callback event) t)))
      (with-current-buffer shell (map-put! agent-shell--state :agent-config `((:busy-prompt-handler . ,handler))))
      (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                  (agent-shell-a2a-tests--send-params "message-2" (map-elt task 'id)))))
        (should (equal (map-elt callback :prompt) "Please inspect the parser"))
        (should (string-empty-p (map-elt connection :wire)))
        (should (= 1 (length agent-shell-a2a-tests--sent)))
        (agent-shell--active-input-accepted callback)
        (funcall (map-elt callback :on-success))
        (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task id)) (map-elt task 'id)))
        (should (= 2 (length (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :history))))))))

(ert-deftest agent-shell-a2a-steer-keeps-pending-permission-visible-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)) callback)
      (with-current-buffer shell
        (map-put! agent-shell--state :agent-config `((:busy-prompt-handler . ,(lambda (event) (setq callback event) t))))
        (setq agent-shell--state
              (map-insert agent-shell--state :tool-calls '(("tool" . ((:permission-request-id . 7)))))))
      (agent-shell-a2a-tests--event shell 'permission-request)
      (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                  (agent-shell-a2a-tests--send-params "message-2" (map-elt task 'id)))))
        (agent-shell--active-input-accepted callback)
        (funcall (map-elt callback :on-success))
        (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task status state))
                       "TASK_STATE_INPUT_REQUIRED"))))))

(ert-deftest agent-shell-a2a-refused-steer-never-queues-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)) callback)
      (with-current-buffer shell
        (map-put! agent-shell--state :agent-config `((:busy-prompt-handler . ,(lambda (event) (setq callback event) t)))))
      (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                  (agent-shell-a2a-tests--send-params "message-2" (map-elt task 'id)))))
        (funcall (map-elt callback :fallback))
        (funcall (map-elt callback :on-success))
        (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json connection) '(error code))))
        (should-not (with-current-buffer shell (map-elt agent-shell--state :pending-prompts)))
        (should (= 1 (length (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :history))))))))

(ert-deftest agent-shell-a2a-stale-steer-is-not-accepted-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)) callback)
      (with-current-buffer shell
        (map-put! agent-shell--state :agent-config `((:busy-prompt-handler . ,(lambda (event) (setq callback event) t)))))
      (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                  (agent-shell-a2a-tests--send-params "message-2" (map-elt task 'id)))))
        (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
        (funcall (map-elt callback :on-success))
        (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json connection) '(error code))))))))

(ert-deftest agent-shell-a2a-streams-artifacts-and-status-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendStreamingMessage" (agent-shell-a2a-tests--send-params))))
      (agent-shell-a2a-tests--settle connection)
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "First chunk")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let* ((wire (agent-shell-a2a-tests--settle connection t))
             (events (mapcar (lambda (line) (json-parse-string (substring line 6) :object-type 'alist))
                             (seq-filter (lambda (line) (string-prefix-p "data: " line)) (split-string wire "\n")))))
        (should (string-match-p "Content-Type: text/event-stream" wire))
        (should (map-nested-elt (car events) '(result task)))
        (should (equal (map-nested-elt (nth 1 events) '(result artifactUpdate artifact parts))
                       [((text . "First chunk"))]))
        (should (eq (map-nested-elt (nth 2 events) '(result artifactUpdate lastChunk)) t))
        (should (equal (map-nested-elt (car (last events)) '(result statusUpdate status state)) "TASK_STATE_COMPLETED"))))))

(ert-deftest agent-shell-a2a-disconnect-does-not-cancel-task-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendStreamingMessage" (agent-shell-a2a-tests--send-params))))
      (agent-shell-a2a-tests--settle connection)
      (delete-process (map-elt connection :process))
      (accept-process-output nil 0.02)
      (should-not agent-shell-a2a-tests--interrupted)
      (should-not (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :streams))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Still working")))
      (should (equal (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :output) "Still working")))))

(ert-deftest agent-shell-a2a-local-input-prevents-output-leak-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (agent-shell-a2a-tests--event shell 'input-submitted '((:prompt . "Unrelated user steer")))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Private reply")))
      (let ((result (map-elt (agent-shell-a2a-tests--json
                             (agent-shell-a2a-tests--rpc "GetTask" `((id . ,(map-elt task 'id))))) 'result)))
        (should (equal (map-nested-elt result '(status state)) "TASK_STATE_FAILED"))
        (should (equal (map-elt result 'artifacts) []))))))

(ert-deftest agent-shell-a2a-cancel-owned-task-only-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (let ((connection (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task 'id))))))
        (should (string-empty-p (map-elt connection :wire)))
        (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "cancelled")))
        (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result status state))
                       "TASK_STATE_CANCELED")))
      (should (eq agent-shell-a2a-tests--interrupted shell))
      (should (= -32002 (map-nested-elt (agent-shell-a2a-tests--json
                                       (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task 'id))))) '(error code))))
      (should-not (map-elt (agent-shell-a2a-tests--agent) :active-task)))))

(ert-deftest agent-shell-a2a-cancel-losing-race-is-not-success-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((task (agent-shell-a2a-tests--task))
           (connection (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task 'id))))))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (should (= -32002 (map-nested-elt (agent-shell-a2a-tests--json connection) '(error code)))))))

(ert-deftest agent-shell-a2a-permissions-remain-interactive-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                (agent-shell-a2a-tests--send-params nil nil '((returnImmediately . :false))))))
      (agent-shell-a2a-tests--event shell 'permission-request)
      (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task status state))
                     "TASK_STATE_INPUT_REQUIRED"))
      (should-not agent-shell-a2a-tests--interrupted))))

(ert-deftest agent-shell-a2a-list-pagination-and-artifact-filter-test ()
  (agent-shell-a2a-tests--with-gateway
    (dotimes (index 3)
      (agent-shell-a2a-tests--task (agent-shell-a2a-tests--send-params (format "message-%s" index)))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Done")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil)))
    (let* ((page (map-elt (agent-shell-a2a-tests--json
                          (agent-shell-a2a-tests--rpc "ListTasks" '((pageSize . 2)))) 'result))
           (next (map-elt (agent-shell-a2a-tests--json
                          (agent-shell-a2a-tests--rpc "ListTasks" `((pageSize . 2) (pageToken . ,(map-elt page 'nextPageToken)) (includeArtifacts . t)))) 'result)))
      (should (= 3 (map-elt page 'totalSize)))
      (should (= 2 (length (map-elt page 'tasks))))
      (should-not (map-contains-key (aref (map-elt page 'tasks) 0) 'artifacts))
      (should (= 1 (length (map-elt next 'tasks))))
      (should (map-contains-key (aref (map-elt next 'tasks) 0) 'artifacts))
      (should (equal (map-elt next 'nextPageToken) "")))))

(ert-deftest agent-shell-a2a-rejects-obsolete-protocol-and-unsupported-features-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((wire (agent-shell-a2a-tests--wire
                 (concat "/agents/" (map-elt (agent-shell-a2a-tests--agent) :id) "/rpc")
                 '((jsonrpc . "2.0") (id . 1) (method . "ListTasks")))))
      (setq wire (replace-regexp-in-string "A2A-Version: 1.0" "A2A-Version: 0.3" wire))
      (should (= -32009 (map-nested-elt (agent-shell-a2a-tests--json (agent-shell-a2a-tests--open wire)) '(error code)))))
    (should (= -32601 (map-nested-elt (agent-shell-a2a-tests--json (agent-shell-a2a-tests--rpc "message/send")) '(error code))))
    (should (= -32003 (map-nested-elt (agent-shell-a2a-tests--json (agent-shell-a2a-tests--rpc "CreateTaskPushNotificationConfig")) '(error code))))))

(ert-deftest agent-shell-a2a-rejects-nontext-and-invalid-config-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((params (agent-shell-a2a-tests--send-params)))
      (map-put! (map-elt params 'message) 'parts [((url . "file:///private"))])
      (should (= -32005 (map-nested-elt (agent-shell-a2a-tests--json (agent-shell-a2a-tests--rpc "SendMessage" params)) '(error code)))))
    (should (= -32602 (map-nested-elt
                      (agent-shell-a2a-tests--json
                       (agent-shell-a2a-tests--rpc "SendMessage" (agent-shell-a2a-tests--send-params nil nil '((historyLength . -1)))))
                      '(error code))))
    (should-not agent-shell-a2a-tests--sent)))

(ert-deftest agent-shell-a2a-fragmented-unicode-request-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((wire (agent-shell-a2a-tests--wire
                  (concat "/agents/" (map-elt (agent-shell-a2a-tests--agent) :id) "/rpc")
                  `((jsonrpc . "2.0") (id . 1) (method . "SendMessage")
                    (params . ,(agent-shell-a2a-tests--send-params nil nil nil "Inspect \u03bb")))))
           (connection (agent-shell-a2a-tests--open wire (list (substring wire 0 19) (substring wire 19 230) (substring wire 230)))))
      (should (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task)))
      (should (equal agent-shell-a2a-tests--sent '("Inspect \u03bb"))))))

(ert-deftest agent-shell-a2a-rejects-ambiguous-framing-test ()
  (agent-shell-a2a-tests--with-gateway
    (dolist (headers '("Content-Length: 0\r\n" "Transfer-Encoding: chunked\r\n" "Content-Length: 99999999\r\n"))
      (should (string-match-p "400 Bad Request"
                               (agent-shell-a2a-tests--settle
                                (agent-shell-a2a-tests--open (agent-shell-a2a-tests--wire "/agents" nil headers)) t))))
    (should-not agent-shell-a2a-tests--sent)))

(ert-deftest agent-shell-a2a-session-change-is-refused-test ()
  (agent-shell-a2a-tests--with-gateway
    (with-current-buffer shell (map-put! (map-elt agent-shell--state :session) :id "replacement-session"))
    (should (= -32004 (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "SendMessage" (agent-shell-a2a-tests--send-params))) '(error code))))
    (should-error (agent-shell-a2a-expose shell) :type 'user-error)
    (should-not agent-shell-a2a-tests--sent)))

(ert-deftest agent-shell-a2a-withdraw-does-not-interrupt-agent-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                (agent-shell-a2a-tests--send-params nil nil '((returnImmediately . :false))))))
      (agent-shell-a2a-unexpose shell)
      (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task status state)) "TASK_STATE_FAILED"))
      (should-not agent-shell-a2a-tests--interrupted)
      (should-not (with-current-buffer shell (map-elt agent-shell--state :event-subscriptions))))))

(ert-deftest agent-shell-a2a-adapters-emit-accepted-input-test ()
  "Every native adapter reports the same acknowledgement and ownership event."
  (dolist (provider '(codex claude grok))
    (agent-shell-a2a-tests--with-gateway
      (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
            request event accepted)
        (map-put! client :thread-id "thread")
        (map-put! client :pending-prompt '((:turn-id . "turn")))
        (with-current-buffer shell
          (map-put! agent-shell--state :agent-config
                    `((:busy-prompt-handler . ,(pcase provider
                                                ('codex #'agent-shell-codex-app-server-handle-busy-prompt)
                                                ('claude #'agent-shell-anthropic--handle-busy-prompt)
                                                ('grok #'agent-shell-xai--handle-busy-prompt)))))
          (setq agent-shell--state (map-insert agent-shell--state :client client))
          (setq agent-shell--state (map-insert agent-shell--state :active-requests '(((:method . "session/prompt"))))))
        (agent-shell-subscribe-to :shell-buffer shell :event 'input-submitted
                                  :on-event (lambda (value) (setq event value)))
        (cl-letf (((symbol-function 'agent-shell--build-content-blocks)
                   (lambda (text) `[((type . "text") (text . ,text))]))
                  ((symbol-function 'acp-send-request)
                   (lambda (&rest args) (setq request args)))
                  ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
                   (lambda (&rest args) (setq request args))))
          (let ((input `((:state . ,(with-current-buffer shell agent-shell--state))
                         (:prompt . "Update the plan") (:source . a2a)
                         (:fallback . ,(lambda () (ert-fail "Accepted input fell back")))
                         (:on-success . ,(lambda () (setq accepted t))))))
            (with-current-buffer shell
              (should (funcall (map-nested-elt agent-shell--state '(:agent-config :busy-prompt-handler)) input)))
            (should-not accepted)
            (should-not event)
            (funcall (plist-get request :on-success)
                     (pcase provider
                       ('codex '((turnId . "turn")))
                       ('claude '((outcome . "injected")))
                       ('grok '((result . ((status . "queued")))))))
            (should accepted)
            (should (equal (map-nested-elt event '(:data :prompt)) "Update the plan"))
            (should (eq (map-nested-elt event '(:data :source)) 'a2a))))))))

(ert-deftest agent-shell-a2a-session-change-during-output-is-not-forwarded-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (with-current-buffer shell (map-put! (map-elt agent-shell--state :session) :id "replacement-session"))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Another session's output")))
      (let ((result (map-elt (agent-shell-a2a-tests--json
                             (agent-shell-a2a-tests--rpc "GetTask" `((id . ,(map-elt task 'id))))) 'result)))
        (should (equal (map-nested-elt result '(status state)) "TASK_STATE_FAILED"))
        (should (equal (map-elt result 'artifacts) []))))))

(ert-deftest agent-shell-a2a-output-bound-is-enforced-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--task)
    (let ((agent-shell-a2a--max-body 10))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "More than ten bytes"))))
    (let ((task (map-elt (agent-shell-a2a-tests--agent) :active-task)))
      (should (eq (map-elt task :state) 'failed))
      (should (string-empty-p (map-elt task :output)))
      (should-not agent-shell-a2a-tests--interrupted))))

(ert-deftest agent-shell-a2a-codex-question-reaches-caller-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                (agent-shell-a2a-tests--send-params nil nil '((returnImmediately . :false))))))
      (run-hook-with-args 'agent-shell-codex-app-server-question-hook
                          `((:client . ((:context-buffer . ,shell)))
                            (:questions . [((title . "Which file?") (options . ["parser.el" "tests.el"]))])))
      (let ((task (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task))))
        (should (equal (map-nested-elt task '(status state)) "TASK_STATE_INPUT_REQUIRED"))
        (should (equal (map-nested-elt (map-nested-elt task '(status message)) '(parts))
                       [((text . "Which file?\nparser.el\ntests.el"))]))))))

(ert-deftest agent-shell-a2a-refuses-reserved-shell-input-test ()
  (agent-shell-a2a-tests--with-gateway
    (should (= -32602 (map-nested-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc
                                      "SendMessage" (agent-shell-a2a-tests--send-params nil nil nil " clear\n")))
                                    '(error code))))
    (should-not agent-shell-a2a-tests--sent)
    (should-not (map-elt (agent-shell-a2a-tests--agent) :active-task))))

(ert-deftest agent-shell-a2a-unacknowledged-dispatch-fails-and-releases-test ()
  (agent-shell-a2a-tests--with-gateway
    (cl-letf (((symbol-function 'agent-shell--insert-to-shell-buffer)
               (lambda (&rest args) (push (plist-get args :text) agent-shell-a2a-tests--sent))))
      (should (equal (map-nested-elt (agent-shell-a2a-tests--task) '(status state)) "TASK_STATE_FAILED")))
    (should-not (map-elt (agent-shell-a2a-tests--agent) :active-task))))

(ert-deftest agent-shell-a2a-stream-survives-input-required-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendStreamingMessage" (agent-shell-a2a-tests--send-params))))
      (agent-shell-a2a-tests--settle connection)
      (agent-shell-a2a-tests--event shell 'elicitation-request)
      (accept-process-output nil 0.05)
      (should (process-live-p (map-elt connection :process)))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Form answered")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let ((states (seq-keep (lambda (line)
                                (when (string-prefix-p "data: " line)
                                  (map-nested-elt (json-parse-string (substring line 6) :object-type 'alist)
                                                  '(result statusUpdate status state))))
                              (split-string (agent-shell-a2a-tests--settle connection t) "\n"))))
        (should (equal states '("TASK_STATE_INPUT_REQUIRED" "TASK_STATE_WORKING" "TASK_STATE_COMPLETED")))))))

(defun agent-shell-a2a-tests--codex-question (shell title)
  "Forward a pending Codex question with TITLE from SHELL and return its context."
  (let ((context (list (cons :client `((:context-buffer . ,shell)))
                       (cons :buffer nil) (cons :status 'pending)
                       (cons :questions `[((title . ,title) (options . ["parser.el"]))]))))
    (run-hook-with-args 'agent-shell-codex-app-server-question-hook context)
    context))

(defun agent-shell-a2a-tests--codex-answer (shell context)
  "Deliver CONTEXT's answer from SHELL the way the Codex adapter does."
  (map-put! context :status 'answering)
  (agent-shell--active-input-accepted
   `((:state . ,(with-current-buffer shell agent-shell--state))
     (:prompt . "answer") (:source . codex-question)))
  (map-put! context :status 'answered))

(ert-deftest agent-shell-a2a-codex-answer-in-emacs-keeps-task-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (agent-shell-a2a-tests--codex-answer shell (agent-shell-a2a-tests--codex-question shell "Which file?"))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Using parser.el")))
      (let ((result (map-elt (agent-shell-a2a-tests--json
                             (agent-shell-a2a-tests--rpc "GetTask" `((id . ,(map-elt task 'id))))) 'result)))
        (should (equal (map-nested-elt result '(status state)) "TASK_STATE_WORKING"))
        (should (equal (map-elt (aref (map-elt result 'history) 1) 'role) "ROLE_AGENT"))
        (should (equal (map-nested-elt (aref (map-elt result 'artifacts) 0) '(parts))
                       [((text . "Using parser.el"))]))))))

(ert-deftest agent-shell-a2a-foreign-codex-answer-fails-task-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((old (list (cons :status 'pending))))
      (agent-shell-a2a-tests--task)
      (agent-shell-a2a-tests--codex-question shell "Which file?")
      (agent-shell-a2a-tests--codex-answer shell old)
      (should (eq (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :state) 'failed)))))

(ert-deftest agent-shell-a2a-caller-answer-closes-codex-question-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((task (agent-shell-a2a-tests--task))
           (context (agent-shell-a2a-tests--codex-question shell "Which file?"))
           callback)
      (with-current-buffer shell
        (map-put! agent-shell--state :agent-config `((:busy-prompt-handler . ,(lambda (event) (setq callback event) t)))))
      (let ((connection (agent-shell-a2a-tests--rpc "SendMessage"
                                                  (agent-shell-a2a-tests--send-params "message-2" (map-elt task 'id)))))
        (agent-shell--active-input-accepted callback)
        (funcall (map-elt callback :on-success))
        (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result task status state))
                       "TASK_STATE_WORKING"))
        (should (eq (map-elt context :status) 'answered))))))

(ert-deftest agent-shell-a2a-first-artifact-chunk-creates-artifact-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((connection (agent-shell-a2a-tests--rpc "SendStreamingMessage" (agent-shell-a2a-tests--send-params))))
      (agent-shell-a2a-tests--settle connection)
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "One")))
      (agent-shell-a2a-tests--event shell 'agent-message-chunk '((:text-chunk . "Two")))
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let ((appends (seq-keep (lambda (line)
                                 (when-let* (((string-prefix-p "data: " line))
                                             (update (map-nested-elt (json-parse-string (substring line 6)
                                                                                        :object-type 'alist
                                                                                        :false-object :false)
                                                                     '(result artifactUpdate))))
                                   (map-elt update 'append)))
                               (split-string (agent-shell-a2a-tests--settle connection t) "\n"))))
        (should (equal appends '(:false t t)))))))

(ert-deftest agent-shell-a2a-answered-form-resumes-task-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--task)
    (agent-shell-a2a-tests--event shell 'elicitation-request)
    (should (eq (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :state) 'input-required))
    (agent-shell-a2a-tests--event shell 'elicitation-response)
    (should (eq (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task) :state) 'working))))

(ert-deftest agent-shell-a2a-retention-is-byte-bounded-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((output "Forty bytes of retained task output....")
           (message-bytes (agent-shell-a2a--message-bytes
                           (map-elt (agent-shell-a2a-tests--send-params "message-0") 'message)))
           ;; Room for the newest task and one finished task, not two.
           (agent-shell-a2a--max-retained-bytes (+ (* 2 message-bytes) (string-bytes output))))
      (dotimes (index 3)
        (agent-shell-a2a-tests--task (agent-shell-a2a-tests--send-params (format "message-%s" index)))
        (agent-shell-a2a-tests--event shell 'agent-message-chunk `((:text-chunk . ,output)))
        (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
        (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil)))
      (should (= 2 (length (map-elt (agent-shell-a2a-tests--agent) :tasks)))))))

(ert-deftest agent-shell-a2a-message-limit-counts-metadata-test ()
  (agent-shell-a2a-tests--with-gateway
    (let* ((params (agent-shell-a2a-tests--send-params))
           (message (map-insert (map-elt params 'message) 'metadata
                               `((note . ,(make-string 600000 ?x))))))
      (map-put! params 'message message)
      (let ((task (agent-shell-a2a-tests--task params))
            reached)
        ;; A handler error would also be refused, so record the call instead.
        (with-current-buffer shell
          (map-put! agent-shell--state :agent-config
                    `((:busy-prompt-handler . ,(lambda (_event) (setq reached t) nil)))))
        (map-put! message 'messageId "message-2")
        (setq message (map-insert message 'taskId (map-elt task 'id)))
        (map-put! params 'message message)
        (should (= -32004 (map-nested-elt
                          (agent-shell-a2a-tests--json
                           (agent-shell-a2a-tests--rpc "SendMessage" params))
                          '(error code))))
        (should-not reached)
        (should (= 1 (length (map-elt (map-elt (agent-shell-a2a-tests--agent) :active-task)
                                     :requests))))))))

(ert-deftest agent-shell-a2a-retention-counts-metadata-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((agent-shell-a2a--max-retained-bytes 1000))
      (dotimes (index 2)
        (let ((params (agent-shell-a2a-tests--send-params (format "message-%s" index))))
          (map-put! params 'message (map-insert (map-elt params 'message) 'metadata
                                                `((note . ,(make-string 600 ?x)))))
          (agent-shell-a2a-tests--task params))
        (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
        (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil)))
      (should (= 1 (length (map-elt (agent-shell-a2a-tests--agent) :tasks)))))))

(ert-deftest agent-shell-a2a-list-filters-and-stable-cursor-test ()
  (agent-shell-a2a-tests--with-gateway
    (dotimes (index 3)
      (agent-shell-a2a-tests--task (agent-shell-a2a-tests--send-params (format "message-%s" index)))
      (unless (= index 2)
        (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
        (with-current-buffer shell (setq agent-shell-a2a-tests--busy nil))))
    (let* ((page (map-elt (agent-shell-a2a-tests--json
                          (agent-shell-a2a-tests--rpc "ListTasks" '((pageSize . 2) (tenant . "") (contextId . "")
                                                                     (status . "TASK_STATE_UNSPECIFIED"))))
                         'result))
           (first-ids (seq-map (lambda (task) (map-elt task 'id)) (map-elt page 'tasks))))
      (should (= 3 (map-elt page 'totalSize)))
      ;; The newest task changes status between pages and must not reappear.
      (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "end_turn")))
      (let* ((next (map-elt (agent-shell-a2a-tests--json
                             (agent-shell-a2a-tests--rpc "ListTasks" `((pageSize . 2) (pageToken . ,(map-elt page 'nextPageToken)))))
                            'result))
             (ids (append first-ids (seq-map (lambda (task) (map-elt task 'id)) (map-elt next 'tasks)))))
        (should (= 3 (length (seq-uniq ids))))
        (should (equal (map-elt next 'nextPageToken) ""))))
    (let ((newest (map-elt (map-elt (agent-shell-a2a-tests--json
                                     (agent-shell-a2a-tests--rpc "ListTasks" '((pageSize . 1))))
                                    'result)
                           'tasks)))
      (dolist (after (list (map-nested-elt (aref newest 0) '(status timestamp))
                           ;; Whole seconds precede fractional timestamps in that second.
                           (concat (substring (map-nested-elt (aref newest 0) '(status timestamp)) 0 19) "Z")
                           (format-time-string "%Y-%m-%dT%H:%M:%S+00:00" (time-subtract nil 60) t)))
        (should (<= 1 (map-nested-elt (agent-shell-a2a-tests--json
                                       (agent-shell-a2a-tests--rpc "ListTasks" `((statusTimestampAfter . ,after))))
                                      '(result totalSize)))))
      (should (= 0 (map-nested-elt (agent-shell-a2a-tests--json
                                    (agent-shell-a2a-tests--rpc "ListTasks" '((statusTimestampAfter . "2999-01-01T00:00:00Z"))))
                                   '(result totalSize))))
      (should (= -32602 (map-nested-elt (agent-shell-a2a-tests--json
                                         (agent-shell-a2a-tests--rpc "ListTasks" '((pageToken . "unknown"))))
                                        '(error code)))))))

(ert-deftest agent-shell-a2a-failed-cancel-request-is-retryable-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((task (agent-shell-a2a-tests--task)))
      (cl-letf (((symbol-function 'agent-shell-interrupt) (lambda (&optional _force) (error "Unavailable"))))
        (should (= -32002 (map-nested-elt (agent-shell-a2a-tests--json
                                           (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task 'id)))))
                                          '(error code)))))
      (let ((connection (agent-shell-a2a-tests--rpc "CancelTask" `((id . ,(map-elt task 'id))))))
        (should (eq agent-shell-a2a-tests--interrupted shell))
        (agent-shell-a2a-tests--event shell 'turn-complete '((:stop-reason . "cancelled")))
        (should (equal (map-nested-elt (agent-shell-a2a-tests--json connection) '(result status state))
                       "TASK_STATE_CANCELED"))))))

(ert-deftest agent-shell-a2a-closed-shell-leaves-directory-test ()
  (agent-shell-a2a-tests--with-gateway
    (agent-shell-a2a-tests--task)
    (let ((task (map-elt (agent-shell-a2a-tests--agent) :active-task)))
      (agent-shell-a2a-tests--event shell 'clean-up)
      (should-not (map-elt agent-shell-a2a--server :agents))
      (should (eq (map-elt task :state) 'failed))
      (should-not agent-shell-a2a-tests--interrupted))))

(ert-deftest agent-shell-a2a-http-client-compatibility-test ()
  (agent-shell-a2a-tests--with-gateway
    (let ((wire (replace-regexp-in-string "Authorization: Bearer" "Authorization: bearer"
                                          (agent-shell-a2a-tests--wire "/agents"))))
      (should (string-match-p "200 OK" (agent-shell-a2a-tests--settle (agent-shell-a2a-tests--open wire) t))))
    (let ((connection (agent-shell-a2a-tests--open
                       (replace-regexp-in-string
                        "Content-Length: 0" "Content-Length: 5000"
                        (replace-regexp-in-string "Authorization: [^\r\n]*" "Authorization: Bearer wrong"
                                                  (agent-shell-a2a-tests--wire "/agents"))))))
      (should (string-match-p "401 Unauthorized" (agent-shell-a2a-tests--settle connection t))))
    (should (string-match-p "WWW-Authenticate: Bearer"
                            (agent-shell-a2a-tests--settle
                             (agent-shell-a2a-tests--open
                              (replace-regexp-in-string "Authorization: [^\r\n]*" "Authorization: Bearer wrong"
                                                        (agent-shell-a2a-tests--wire "/agents")))
                             t)))
    (let* ((wire (agent-shell-a2a-tests--wire
                  (concat "/agents/" (map-elt (agent-shell-a2a-tests--agent) :id) "/rpc")
                  '((jsonrpc . "2.0") (id . 1) (method . "ListTasks"))
                  "Expect: 100-continue\r\n"))
           (split (string-match "\r\n\r\n" wire))
           (connection (agent-shell-a2a-tests--open (substring wire 0 (+ split 4)))))
      (should (string-match-p "\\`HTTP/1.1 100 Continue\r\n\r\n"
                              (agent-shell-a2a-tests--settle connection)))
      (process-send-string (map-elt connection :process) (substring wire (+ split 4)))
      (should (string-match-p "\"totalSize\":0" (agent-shell-a2a-tests--settle connection t))))))

(provide 'agent-shell-a2a-tests)
;;; agent-shell-a2a-tests.el ends here
