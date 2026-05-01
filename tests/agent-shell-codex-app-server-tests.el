;;; agent-shell-codex-app-server-tests.el --- Tests for Codex app-server ACP bridge -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'acp)
(require 'agent-shell-codex-app-server)

(ert-deftest agent-shell-codex-app-server-routes-acp-send-request ()
  "App-server clients should be handled via the ACP public API."
  (let ((client (agent-shell-codex-app-server-make-client
                 :command "sh"
                 :context-buffer (current-buffer)))
        called)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server-send-request)
               (lambda (&rest args)
                 (setq called args)
                 'ok)))
      (should (eq (acp-send-request
                   :client client
                   :request '((:method . "authenticate")))
                  'ok))
      (should (equal (map-elt (plist-get called :request) :method)
                     "authenticate")))))

(ert-deftest agent-shell-codex-app-server-initialize-uses-distinct-client-name ()
  "Initialize should report Agent Shell's distinct client name to Codex."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        request-args
        notification-args)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq request-args args)
                 (when-let* ((on-success (plist-get args :on-success)))
                   (funcall on-success '()))))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-notification)
               (lambda (&rest args)
                 (setq notification-args args))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "initialize")))
      (should (equal (map-nested-elt (plist-get request-args :params)
                                     '(clientInfo name))
                     agent-shell-codex-app-server--client-name))
      (should (equal (map-nested-elt (plist-get request-args :params)
                                     '(clientInfo title))
                     "Emacs Agent Shell"))
      (should (equal (map-elt notification-args :method) "initialized")))))

(ert-deftest agent-shell-codex-app-server-ensures-session-title-slot ()
  "App-server sessions should have a title slot for core title updates."
  (let* ((target-buffer (generate-new-buffer " *agent-shell-codex-session*"))
         (client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :context-buffer target-buffer)))
    (unwind-protect
        (with-current-buffer target-buffer
          (setq-local agent-shell--state
                      `((:session . ((:id . "thread-1")
                                      (:mode-id . "reasoning:high")))))
          (agent-shell-codex-app-server--ensure-session-title-slot client)
          (should (assoc :title (map-elt agent-shell--state :session)))
          (map-put! (map-elt agent-shell--state :session) :title "hello")
          (should (equal (map-nested-elt agent-shell--state '(:session :title))
                         "hello")))
      (kill-buffer target-buffer))))

(ert-deftest agent-shell-codex-app-server-defers-notification-callbacks ()
  "Translated notifications should be delivered off the process filter stack."
  (let* ((target-buffer (generate-new-buffer " *agent-shell-codex-app-server-test*"))
         (client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :context-buffer target-buffer))
         delivered
         callback-buffer)
    (unwind-protect
        (progn
          (acp-subscribe-to-notifications
           :client client
           :buffer target-buffer
           :on-notification (lambda (notification)
                              (setq delivered notification
                                    callback-buffer (current-buffer))))
          (agent-shell-codex-app-server--process-filter
           client
           "{\"method\":\"turn/plan/updated\",\"params\":{\"entries\":[]}}\n")
          (should-not delivered)
          (sleep-for 0.01)
          (should (equal (map-elt delivered 'method) "session/update"))
          (should (eq callback-buffer target-buffer)))
      (kill-buffer target-buffer))))

(ert-deftest agent-shell-codex-app-server-plan-updates-normalize-step-content ()
  "Plan updates should expose ACP-style `content' text for each step."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         delivered)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq delivered notification)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/plan/updated")
       (params . ((plan . ((entries . (((step . "Inspect transport")
                                        (status . "completed"))
                                       ((step . "Normalize plan entries")
                                        (status . "in_progress"))))))))))
    (should (equal (mapcar (lambda (entry) (map-elt entry 'content))
                           (map-nested-elt delivered '(params update entries)))
                   '("Inspect transport" "Normalize plan entries")))))

(ert-deftest agent-shell-codex-app-server-prompt-completion-runs-in-shell-buffer ()
  "Prompt completions should run in the shell buffer, not timer temp buffers."
  (let* ((target-buffer (generate-new-buffer " *agent-shell-codex-app-server-shell*"))
         (client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :context-buffer target-buffer))
         callback-buffer
         stop-reason)
    (unwind-protect
        (progn
          (map-put! client :pending-prompt
                    `((:turn-id . "turn-1")
                      (:buffer . ,target-buffer)
                      (:on-success . ,(lambda (response)
                                        (setq callback-buffer (current-buffer)
                                              stop-reason (map-elt response 'stopReason))))))
          (with-temp-buffer
            (emacs-lisp-mode)
            (agent-shell-codex-app-server--respond-to-pending-prompt
             client
             '((id . "turn-1")
               (status . "completed")))))
      (kill-buffer target-buffer))
    (should (eq callback-buffer target-buffer))
    (should (equal stop-reason "end_turn"))
    (should-not (map-elt client :pending-prompt))))

(ert-deftest agent-shell-codex-app-server-ignores-pty-echoed-requests ()
  "Echoed PTY input should be discarded before JSON-RPC routing."
  (let ((client (agent-shell-codex-app-server-make-client
                 :command "sh"
                 :connection-type 'pty))
        (errors '()))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--track-pty-echo-p)
               (lambda (_client)
                 t)))
      (acp-subscribe-to-errors
       :client client
       :on-error (lambda (error)
                   (push error errors)))
      (map-put! client :echo-lines
                '("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}"))
      (agent-shell-codex-app-server--process-filter
       client
       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}\r\n")
      (should-not (map-elt client :echo-lines))
      (should-not (map-elt client :message-queue))
      (should-not errors))))

(ert-deftest agent-shell-codex-app-server-structured-errors-use-inner-message ()
  "Structured error payloads should surface a human-readable message."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (agent-shell-codex-app-server-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (setq delivered error)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "error")
       (params . ((message . ((message . "You've hit your usage limit.")
                              (codexErrorInfo . "usageLimitExceeded")
                              (additionalDetails)))))))
    (should (equal (map-elt delivered 'message)
                   "You've hit your usage limit."))))

(ert-deftest agent-shell-codex-app-server-save-tool-entry-adds-status ()
  "New tool entries should accept status updates without `map-not-inplace'."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "echo hi")
                 (cwd . "/tmp")))
         (entry (agent-shell-codex-app-server--save-tool-entry
                 client item "inProgress")))
    (should (equal (map-elt entry :status) "in_progress"))
    (should (equal (map-elt (gethash "tool-1" (map-elt client :tool-items))
                            :status)
                   "in_progress"))))

(ert-deftest agent-shell-codex-app-server-execute-tools-use-command-label ()
  "Execute tools should use the command as their visible description."
  (let* ((item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "/bin/zsh -lc \"npm test\"")
                 (cwd . "/tmp")))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :kind) "execute"))
    (should (equal (map-elt entry :description) "/bin/zsh -lc \"npm test\""))
    (should (equal (map-elt (map-elt entry :raw-input) 'description)
                   "/bin/zsh -lc \"npm test\""))
    (should (equal (map-elt (map-elt entry :raw-input) 'cwd)
                   "/tmp"))))

(ert-deftest agent-shell-codex-app-server-execute-tools-strip-run-prefix ()
  "Execute tool labels should not repeat a leading run verb."
  (let* ((item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "Run cargo fmt")
                 (cwd . "/tmp")))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :kind) "execute"))
    (should (equal (map-elt entry :title) "cargo fmt"))
    (should (equal (map-elt entry :description) "cargo fmt"))
    (should (equal (map-elt entry :command) "Run cargo fmt"))))

(ert-deftest agent-shell-codex-app-server-read-commands-use-read-kind ()
  "Read-only command actions should render as `read' instead of `execute'."
  (let* ((item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "/bin/zsh -lc \"sed -n '1,220p' foo.txt\"")
                 (commandActions . (((type . "read"))))
                 (cwd . "/tmp")))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :kind) "read"))
    (should (equal (map-elt entry :description)
                   "/bin/zsh -lc \"sed -n '1,220p' foo.txt\""))))

(ert-deftest agent-shell-codex-app-server-mixed-command-actions-use-execute-kind ()
  "Mixed or unknown command actions should stay `execute'."
  (let* ((item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "/bin/zsh -lc \"cat foo.txt && npm test\"")
                 (commandActions . (((type . "read"))
                                    ((type . "unknown"))))
                 (cwd . "/tmp")))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :kind) "execute"))))

(ert-deftest agent-shell-codex-app-server-read-command-approvals-use-read-kind ()
  "Read-only command approvals should render as `read'."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           '((method . "item/commandExecution/requestApproval")
             (id . 31)
             (params . ((itemId . "call-1")
                        (command . "/bin/zsh -lc \"sed -n '1,220p' foo.txt\"")
                        (commandActions . (((type . "read"))))
                        (cwd . "/tmp")))))))
    (should (equal (map-nested-elt translated '(params toolCall kind))
                   "read"))))

(ert-deftest agent-shell-codex-app-server-execute-approvals-strip-run-prefix ()
  "Execute approval labels should not repeat a leading run verb."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           '((method . "item/commandExecution/requestApproval")
             (id . 32)
             (params . ((itemId . "call-1")
                        (command . "Run cargo fmt")
                        (cwd . "/tmp")))))))
    (should (equal (map-nested-elt translated '(params toolCall kind))
                   "execute"))
    (should (equal (map-nested-elt translated '(params toolCall title))
                   "cargo fmt"))
    (should (equal (map-nested-elt translated '(params toolCall rawInput description))
                   "cargo fmt"))))

(ert-deftest agent-shell-codex-app-server-coalesces-command-output-deltas ()
  "Command output deltas should be coalesced before notifying the UI."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "rg foo")
                 (cwd . "/tmp")))
         notifications
         timers)
    (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (let ((agent-shell-codex-app-server--output-flush-interval 0.05))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args)
                   (push args timers)
                   'fake-tool-output-timer)))
        (should-not
         (agent-shell-codex-app-server--translate-command-output
          client '((itemId . "tool-1")
                   (delta . "alpha"))))
        (should-not
         (agent-shell-codex-app-server--translate-command-output
          client '((itemId . "tool-1")
                   (delta . "beta"))))
        (should-not notifications)
        (should (= (length timers) 1))
        (agent-shell-codex-app-server--flush-tool-output-updates client)))
    (should (equal (map-nested-elt (car (map-nested-elt (car notifications)
                                                         '(params update content)))
                                   '(content text))
                   "alphabeta"))
    (should (zerop (hash-table-count (map-elt client :pending-tool-output-items))))
    (should-not (map-elt client :tool-output-flush-timer))))

(ert-deftest agent-shell-codex-app-server-completed-items-flush-buffered-output ()
  "Completed items should render buffered output even before debounce fires."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "rg foo")
                 (cwd . "/tmp")))
         (completed '((method . "item/completed")
                      (params . ((item . ((id . "tool-1")
                                          (type . "commandExecution")
                                          (command . "rg foo")
                                          (cwd . "/tmp")
                                          (status . "completed")))))))
         notifications
         cancelled-timer)
    (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (let ((agent-shell-codex-app-server--output-flush-interval 0.05))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   'fake-tool-output-timer))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (setq cancelled-timer timer))))
        (agent-shell-codex-app-server--translate-command-output
         client '((itemId . "tool-1")
                  (delta . "alphabeta")))
        (agent-shell-codex-app-server--handle-notification client completed)))
    (should (equal cancelled-timer 'fake-tool-output-timer))
    (should (equal (map-nested-elt (car (map-nested-elt (car notifications)
                                                         '(params update content)))
                                   '(content text))
                   "alphabeta"))
    (should (equal (map-nested-elt (car notifications) '(params update status))
                   "completed"))
    (should (equal (gethash "tool-1" (map-elt client :tool-outputs))
                   "alphabeta"))
    (should-not (gethash "tool-1" (map-elt client :tool-output-chunks)))
    (should (zerop (hash-table-count (map-elt client :pending-tool-output-items))))))

(ert-deftest agent-shell-codex-app-server-mcp-tools-render-result-and-description ()
  "MCP tool calls should expose a description and render text results."
  (let* ((item '((id . "tool-1")
                 (type . "mcpToolCall")
                 (server . "codex")
                 (tool . "find")
                 (arguments . ((pattern . "Codex")))
                 (result . ((content . (((type . "text")
                                         (text . "README.org:69"))))
                            (structuredContent . nil)))))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item))
         (content (agent-shell-codex-app-server--tool-content
                   (agent-shell-codex-app-server-make-client :command "sh")
                   item)))
    (should (equal (map-elt entry :title) "codex/find"))
    (should (equal (map-elt entry :description) "find"))
    (should (equal (map-elt (map-elt entry :raw-input) 'description) "find"))
    (should (equal (map-nested-elt (car content) '(content text))
                   "README.org:69"))))

(ert-deftest agent-shell-codex-app-server-dynamic-tools-fallback-to-name ()
  "Dynamic tool calls should use name/title fallbacks when tool is missing."
  (let* ((item '((id . "tool-1")
                 (type . "dynamicToolCall")
                 (name . "find")
                 (input . ((path . "README.org")))))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :title) "find"))
    (should (equal (map-elt entry :description) "find"))
    (should (equal (map-elt (map-elt entry :raw-input) 'description) "find"))))

(ert-deftest agent-shell-codex-app-server-web-search-carries-description ()
  "Web search items should keep a fallback description even without a query."
  (let* ((item '((id . "tool-1")
                 (type . "webSearch")
                 (query . "")
                 (action . ((type . "openPage")
                            (url . "https://example.com")))))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item))
         (content (agent-shell-codex-app-server--tool-content
                   (agent-shell-codex-app-server-make-client :command "sh")
                   item)))
    (should (equal (map-elt entry :description) "openPage"))
    (should (equal (map-elt (map-elt entry :raw-input) 'description) "openPage"))
    (should (equal (map-nested-elt (car content) '(content text))
                   "https://example.com"))))

(ert-deftest agent-shell-codex-app-server-permissions-support-session-scope ()
  "Permission requests should expose turn and session-scoped grant options."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           '((method . "item/permissions/requestApproval")
             (id . 61)
             (params . ((threadId . "thr-1")
                        (turnId . "turn-1")
                        (itemId . "call-1")
                        (reason . "Need write access")
                        (permissions . ((fileSystem . ((write . ("/tmp/project")))))))))))
         (options (map-nested-elt translated '(params options)))
         captured-result)
    (should (equal (mapcar (lambda (option) (map-elt option 'kind)) options)
                   '("allow_once" "allow_always" "reject_once")))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 61
       :option-id "grantForSession"))
    (should (equal (map-elt captured-result 'scope) "session"))
    (should (equal (map-nested-elt captured-result '(permissions fileSystem write))
                   '("/tmp/project")))))

(ert-deftest agent-shell-codex-app-server-permissions-reject-with-empty-object ()
  "Rejected permission requests should return an empty permissions object."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         captured-result)
    (agent-shell-codex-app-server--translate-request
     client
     '((method . "item/permissions/requestApproval")
       (id . 62)
       (params . ((threadId . "thr-1")
                  (turnId . "turn-1")
                  (itemId . "call-1")
                  (permissions . ((network . ((enabled . t)))))))))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 62
       :option-id "decline"))
    (should (equal (map-elt captured-result 'scope) "turn"))
    (should (hash-table-p (map-elt captured-result 'permissions)))))

(ert-deftest agent-shell-codex-app-server-structured-decisions-round-trip ()
  "Structured availableDecisions should survive request/response translation."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (decision '((acceptWithExecpolicyAmendment
                      . ((execpolicy_amendment
                          . ((add_rules . (((command . "curl .*"))))))))))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           `((method . "execCommandApproval")
             (id . 71)
             (params . ((callId . "call-1")
                        (command . "curl https://example.com")
                        (cwd . "/tmp")
                        (availableDecisions . (,decision "abort")))))))
         (options (map-nested-elt translated '(params options)))
         captured-result)
    (should (equal (mapcar (lambda (option) (map-elt option 'name)) options)
                   '("Allow via Policy" "Cancel")))
    (should (equal (mapcar (lambda (option) (map-elt option 'kind)) options)
                   '("allow_always" "allow_once")))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 71
       :option-id "decision-0"))
    (should (equal captured-result `((decision . ,decision))))))

(ert-deftest agent-shell-codex-app-server-decline-and-cancel-stay-distinct ()
  "Decline and cancel decisions should remain separate ACP actions."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           '((method . "item/commandExecution/requestApproval")
             (id . 72)
             (params . ((itemId . "call-1")
                        (command . "rm foo")
                        (cwd . "/tmp")
                        (availableDecisions . ("decline" "cancel")))))))
         (options (map-nested-elt translated '(params options))))
    (should (equal (mapcar (lambda (option) (map-elt option 'kind)) options)
                   '("reject_once" "allow_once")))
    (should (equal (mapcar (lambda (option) (map-elt option 'name)) options)
                   '("Reject" "Cancel")))))

(ert-deftest agent-shell-codex-app-server-file-change-cancel-uses-interrupt-path ()
  "File change approvals should rely on interrupt for cancel semantics."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           '((method . "item/fileChange/requestApproval")
             (id . 73)
             (params . ((itemId . "call-1")
                        (reason . "Apply patch")
                        (grantRoot . "/tmp"))))))
         (options (map-nested-elt translated '(params options)))
         captured-result)
    (should (equal (mapcar (lambda (option) (map-elt option 'name)) options)
                   '("Allow" "Always Allow" "Reject")))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 73
       :cancelled t))
    (should (equal captured-result '((decision . "cancel"))))))

(ert-deftest agent-shell-codex-app-server-cancelled-missing-permission-is-ignored ()
  "Cancelling an already-cleared permission request should be a no-op."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        rpc-sent)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest _args)
                 (setq rpc-sent t))))
      (should-not
       (condition-case nil
           (progn
             (agent-shell-codex-app-server-send-permission-response
              :client client
              :request-id 12
              :cancelled t)
             nil)
         (error t))))
    (should-not rpc-sent)))

(ert-deftest agent-shell-codex-app-server-duplicate-permission-response-is-ignored ()
  "A duplicate response for a cleared request should be a no-op."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        responses)
    (agent-shell-codex-app-server--translate-request
     client
     '((method . "item/commandExecution/requestApproval")
       (id . 74)
       (params . ((itemId . "call-1")
                  (command . "ls")
                  (cwd . "/tmp")))))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (push args responses))))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 74
       :option-id "grant")
      (agent-shell-codex-app-server-send-response
       :client client
       :response '((:request-id . 74)
                   (:result . ((outcome . ((outcome . "cancelled"))))))))
    (should (= (length responses) 1))))

(ert-deftest agent-shell-codex-app-server-completed-item-clears-pending-permission ()
  "Completed tool items should clear any matching pending permission."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (agent-shell-codex-app-server--translate-request
     client
     '((method . "item/commandExecution/requestApproval")
       (id . 75)
       (params . ((itemId . "call-1")
                  (command . "ls")
                  (cwd . "/tmp")))))
    (should (gethash 75 (map-elt client :pending-permissions)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/completed")
       (params . ((item . ((id . "call-1")
                           (type . "commandExecution")
                           (status . "completed")))))))
    (should-not (gethash 75 (map-elt client :pending-permissions)))))

(ert-deftest agent-shell-codex-app-server-turn-completion-clears-pending-permissions ()
  "Turn completion should discard orphaned pending permissions."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (agent-shell-codex-app-server--translate-request
     client
     '((method . "item/permissions/requestApproval")
       (id . 76)
       (params . ((permissions . ((network . ((reason . "Need network")))))))))
    (should (gethash 76 (map-elt client :pending-permissions)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-1")
                           (status . "completed")))))))
    (should-not (gethash 76 (map-elt client :pending-permissions)))))

(ert-deftest agent-shell-codex-app-server-wraps-pty-processes-in-raw-shell ()
  "PTY clients should disable terminal echo/canonical mode before exec."
  (let* ((client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :command-params '("app-server")
                  :connection-type 'pty))
         (command (agent-shell-codex-app-server--process-command client)))
    (should (equal (cadr command) "-lc"))
    (should (string-match-p "stty raw -echo < /dev/tty && exec sh app-server"
                            (caddr command)))
    (should-not (agent-shell-codex-app-server--track-pty-echo-p client))))

(ert-deftest agent-shell-codex-app-server-keeps-direct-command-for-pipes ()
  "Pipe clients should keep the original command list."
  (let* ((client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :command-params '("app-server")
                  :connection-type 'pipe))
         (command (agent-shell-codex-app-server--process-command client)))
    (should (equal command '("sh" "app-server")))
    (should-not (agent-shell-codex-app-server--track-pty-echo-p client))))

(ert-deftest agent-shell-codex-app-server-session-list-fetches-all-pages ()
  "Session listing should follow nextCursor until all pages are loaded."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         requests
         response)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (push (plist-get args :params) requests)
                 (funcall
                  (plist-get args :on-success)
                  (if (map-elt (plist-get args :params) 'cursor)
                      '((data . (((id . "thr-2")
                                  (preview . "Second session")
                                  (cwd . "/tmp")
                                  (createdAt . 2)
                                  (updatedAt . 3))))
                        (nextCursor . nil))
                    '((data . (((id . "thr-1")
                                (preview . "First session")
                                (cwd . "/tmp")
                                (createdAt . 1)
                                (updatedAt . 2))))
                      (nextCursor . "page-2")))))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/list")
                  (:params . ((cwd . "/tmp"))))
       :on-success (lambda (result)
                     (setq response result))))
    (should (= (length requests) 2))
    (should-not (map-elt (cadr requests) 'cursor))
    (should (equal (map-elt (car requests) 'cursor) "page-2"))
    (should (equal (mapcar (lambda (session) (map-elt session 'sessionId))
                           (map-elt response 'sessions))
                   '("thr-1" "thr-2")))))

(ert-deftest agent-shell-codex-app-server-model-list-fetches-all-pages ()
  "Model listing should follow nextCursor until all pages are loaded."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         requests
         completed)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (push (plist-get args :params) requests)
                 (funcall
                  (plist-get args :on-success)
                  (if (map-elt (plist-get args :params) 'cursor)
                      '((data . (((id . "model-2")
                                  (model . "gpt-5.2")
                                  (displayName . "GPT-5.2")
                                  (description . "Second page")
                                  (isDefault . nil))))
                        (nextCursor . nil))
                    '((data . (((id . "model-1")
                                (model . "gpt-5.1")
                                (displayName . "GPT-5.1")
                                (description . "First page")
                                (isDefault . t))))
                      (nextCursor . "page-2")))))))
      (agent-shell-codex-app-server--fetch-models
       client
       (lambda ()
         (setq completed t))))
    (should completed)
    (should (= (length requests) 2))
    (should-not (map-elt (cadr requests) 'cursor))
    (should (equal (map-elt (car requests) 'cursor) "page-2"))
    (should (equal (mapcar (lambda (model) (map-elt model 'model))
                           (map-elt client :available-models))
                   '("gpt-5.1" "gpt-5.2")))))

(ert-deftest agent-shell-codex-app-server-session-response-includes-reasoning-modes ()
  "Session responses should expose synthetic reasoning-effort modes."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (map-put! client :available-models
              '(((id . "model-1")
                 (model . "gpt-5.4")
                 (displayName . "GPT-5.4")
                 (description . "Main model")
                 (supportedReasoningEfforts . (((reasoningEffort . "low")
                                                (description . "Faster"))
                                               ((reasoningEffort . "medium")
                                                (description . "Balanced"))
                                               ((reasoningEffort . "xhigh")
                                                (description . "Deepest"))))
                 (defaultReasoningEffort . "medium")
                 (isDefault . t))))
    (let ((response
           (agent-shell-codex-app-server--session-response
            client
            '((thread . ((id . "thread-1")))
              (model . "gpt-5.4")
              (reasoningEffort . "xhigh")))))
      (should (equal (map-nested-elt response '(modes currentModeId))
                     "reasoning:xhigh"))
      (should (equal (mapcar (lambda (mode) (map-elt mode 'id))
                             (map-nested-elt response '(modes availableModes)))
                     '("reasoning:low" "reasoning:medium" "reasoning:xhigh")))
      (should (equal (mapcar (lambda (mode) (map-elt mode 'name))
                             (map-nested-elt response '(modes availableModes)))
                     '("Low" "Medium" "XHigh"))))))

(ert-deftest agent-shell-codex-app-server-session-set-mode-updates-reasoning-effort ()
  "Session mode changes should update the local reasoning effort."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         response)
    (map-put! client :available-models
              '(((id . "model-1")
                 (model . "gpt-5.4")
                 (displayName . "GPT-5.4")
                 (supportedReasoningEfforts . (((reasoningEffort . "low")
                                                (description . "Faster"))
                                               ((reasoningEffort . "medium")
                                                (description . "Balanced"))
                                               ((reasoningEffort . "high")
                                                (description . "Deeper"))))
                 (defaultReasoningEffort . "medium")
                 (isDefault . t))))
    (map-put! client :current-model-id "gpt-5.4")
    (agent-shell-codex-app-server-send-request
     :client client
     :request '((:method . "session/set_mode")
                (:params . ((modeId . "reasoning:high"))))
     :on-success (lambda (result)
                   (setq response result)))
    (should (equal (map-elt client :reasoning-effort) "high"))
    (should (equal (map-elt response 'modeId) "reasoning:high"))))

(ert-deftest agent-shell-codex-app-server-session-set-mode-rejects-unsupported-effort ()
  "Session mode changes should fail when the current model does not support the effort."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         delivered-error)
    (map-put! client :available-models
              '(((id . "model-1")
                 (model . "gpt-5.4-mini")
                 (displayName . "GPT-5.4 Mini")
                 (supportedReasoningEfforts . (((reasoningEffort . "low")
                                                (description . "Faster"))
                                               ((reasoningEffort . "medium")
                                                (description . "Balanced"))))
                 (defaultReasoningEffort . "low")
                 (isDefault . t))))
    (map-put! client :current-model-id "gpt-5.4-mini")
    (agent-shell-codex-app-server-send-request
     :client client
     :request '((:method . "session/set_mode")
                (:params . ((modeId . "reasoning:xhigh"))))
     :on-failure (lambda (error _raw)
                   (setq delivered-error error)))
    (should (string-match-p "not supported"
                            (map-elt delivered-error 'message)))
    (should-not (equal (map-elt client :reasoning-effort) "xhigh"))))

(ert-deftest agent-shell-codex-app-server-session-set-model-adjusts-invalid-effort ()
  "Model changes should fall back to a supported reasoning effort."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         notifications
         response)
    (map-put! client :available-models
              '(((id . "model-1")
                 (model . "gpt-5.4")
                 (displayName . "GPT-5.4")
                 (supportedReasoningEfforts . (((reasoningEffort . "medium")
                                                (description . "Balanced"))
                                               ((reasoningEffort . "high")
                                                (description . "Deeper"))))
                 (defaultReasoningEffort . "medium")
                 (isDefault . t))
                ((id . "model-2")
                 (model . "gpt-5.4-mini")
                 (displayName . "GPT-5.4 Mini")
                 (supportedReasoningEfforts . (((reasoningEffort . "low")
                                                (description . "Faster"))
                                               ((reasoningEffort . "medium")
                                                (description . "Balanced"))))
                 (defaultReasoningEffort . "low")
                 (isDefault . nil))))
    (map-put! client :current-model-id "gpt-5.4")
    (map-put! client :reasoning-effort "high")
    (agent-shell-codex-app-server-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server-send-request
     :client client
     :request '((:method . "session/set_model")
                (:params . ((modelId . "gpt-5.4-mini"))))
     :on-success (lambda (result)
                   (setq response result)))
    (should (equal (map-elt response 'modelId) "gpt-5.4-mini"))
    (should (equal (map-elt client :reasoning-effort) "low"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update currentModeId))
                   "reasoning:low"))))

(ert-deftest agent-shell-codex-app-server-rejects-unsupported-server-requests ()
  "Unsupported server requests should receive a JSON-RPC error."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         dispatched
         error-message
         rejected-id
         rejected-code
         rejected-message
         (request
          (list (cons 'method "item/tool/requestUserInput")
                (cons 'id 91)
                (cons 'params
                      (list (cons 'threadId "thr-1")
                            (cons 'turnId "turn-1")
                            (cons 'itemId "call-1")
                            (cons 'questions
                                  (list (list (cons 'id "name")
                                              (cons 'question "Name?")
                                              (cons 'header "Name")
                                              (cons 'options '())))))))))
    (agent-shell-codex-app-server-subscribe-to-requests
     :client client
     :on-request (lambda (_request)
                   (setq dispatched t)))
    (agent-shell-codex-app-server-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (setq error-message (map-elt error 'message))))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-error)
               (lambda (&rest args)
                 (setq rejected-id (plist-get args :request-id)
                       rejected-code (plist-get args :code)
                       rejected-message (plist-get args :message)))))
      (agent-shell-codex-app-server--route-message
       client
       request))
    (should-not dispatched)
    (should (equal rejected-id 91))
    (should (= rejected-code -32601))
    (should (equal rejected-message
                   "Unsupported Codex app-server request: item/tool/requestUserInput"))
    (should (equal error-message rejected-message))))

(ert-deftest agent-shell-codex-app-server-ignores-stale-turn-completion ()
  "A late completion for an older turn should not resolve the current prompt."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         current-result)
    (map-put! client :active-turn-id "turn-2")
    (map-put! client :pending-prompt
              `((:turn-id . "turn-2")
                (:buffer . ,(current-buffer))
                (:on-success . ,(lambda (result)
                                  (setq current-result result)))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-1")
                           (status . "completed")))))))
    (should (equal (map-elt client :active-turn-id) "turn-2"))
    (should (map-elt client :pending-prompt))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-2")
                           (status . "completed")))))))
    (should (equal (map-elt current-result 'stopReason) "end_turn"))
    (should-not (map-elt client :active-turn-id))
    (should-not (map-elt client :pending-prompt))))

(ert-deftest agent-shell-codex-app-server-interrupt-resolves-pending-prompt ()
  "Interrupt should finish the current prompt locally and interrupt remotely."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         interrupted-turn-id
         prompt-result)
    (map-put! client :thread-id "thread-1")
    (map-put! client :process t)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq interrupted-turn-id (map-elt (plist-get args :params) 'turnId))
                 (should (equal (plist-get args :method) "turn/interrupt")))))
      (map-put! client :active-turn-id "turn-1")
      (map-put! client :pending-prompt
                `((:turn-id . "turn-1")
                  (:buffer . ,(current-buffer))
                  (:on-success . ,(lambda (result)
                                    (setq prompt-result result)))))
      (agent-shell-codex-app-server-interrupt client))
    (should (equal interrupted-turn-id "turn-1"))
    (should (equal (map-elt prompt-result 'stopReason) "cancelled"))
    (should (member "turn-1" (map-elt client :dismissed-turn-ids)))
    (should-not (map-elt client :active-turn-id))
    (should-not (map-elt client :pending-prompt))))

(ert-deftest agent-shell-codex-app-server-interrupt-cancels-pending-turn-start ()
  "Interrupt should reject an in-flight turn/start and cancel the next turn if needed."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         failure-message
         interrupted-turn-id)
    (puthash 7 `((:method . "turn/start")
                 (:buffer . ,(current-buffer))
                 (:on-failure . ,(lambda (error _raw)
                                   (setq failure-message (map-elt error 'message)))))
             (map-elt client :pending-requests))
    (map-put! client :thread-id "thread-1")
    (map-put! client :process t)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq interrupted-turn-id (map-elt (plist-get args :params) 'turnId))
                 (should (equal (plist-get args :method) "turn/interrupt")))))
      (agent-shell-codex-app-server-interrupt client)
      (should (equal failure-message "Task cancelled"))
      (should (map-elt client :interrupt-next-turn))
      (should-not (gethash 7 (map-elt client :pending-requests)))
      (agent-shell-codex-app-server--handle-notification
       client
       '((method . "turn/started")
         (params . ((turn . ((id . "turn-late"))))))))
    (should (equal interrupted-turn-id "turn-late"))
    (should-not (map-elt client :interrupt-next-turn))
    (should-not (map-elt client :active-turn-id))))

(ert-deftest agent-shell-codex-app-server-completes-on-raw-task-complete ()
  "Raw Codex task-complete events should resolve pending prompts."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         prompt-result)
    (map-put! client :active-turn-id "turn-1")
    (map-put! client :pending-prompt
              `((:turn-id . "turn-1")
                (:buffer . ,(current-buffer))
                (:on-success . ,(lambda (result)
                                  (setq prompt-result result)))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "codex/event/task_complete")
       (id . "turn-1")
       (msg . ((type . "task_complete")
               (turn_id . "turn-1")))))
    (should (equal (map-elt prompt-result 'stopReason) "end_turn"))
    (should-not (map-elt client :active-turn-id))
    (should-not (map-elt client :pending-prompt))))

(ert-deftest agent-shell-codex-app-server-queue-survives-handler-errors ()
  "A bad message should not prevent later queued messages from being processed."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (calls 0)
         error-message)
    (acp-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (setq error-message (map-elt error 'message))))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--route-message)
               (lambda (_client message)
                 (setq calls (1+ calls))
                 (when (equal message 'bad)
                   (error "Boom")))))
      (map-put! client :message-queue '(bad good))
      (agent-shell-codex-app-server--drain-message-queue client)
      (sleep-for 0.01))
    (should (= calls 2))
    (should (equal error-message "Failed to handle app-server message: Boom"))
    (should-not (map-elt client :message-queue))
    (should-not (map-elt client :message-queue-busy))))

(provide 'agent-shell-codex-app-server-tests)
;;; agent-shell-codex-app-server-tests.el ends here
