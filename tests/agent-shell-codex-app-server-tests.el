;;; agent-shell-codex-app-server-tests.el --- Tests for Codex app-server ACP bridge -*- lexical-binding: t; -*-

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

(ert-deftest agent-shell-codex-app-server-read-commands-use-read-kind ()
  "Read-only shell commands should render as `read' instead of `execute'."
  (let* ((item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "/bin/zsh -lc \"sed -n '1,220p' foo.txt\"")
                 (cwd . "/tmp")))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :kind) "read"))
    (should (equal (map-elt entry :description)
                   "/bin/zsh -lc \"sed -n '1,220p' foo.txt\""))))

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
                   (error "boom")))))
      (map-put! client :message-queue '(bad good))
      (agent-shell-codex-app-server--drain-message-queue client)
      (sleep-for 0.01))
    (should (= calls 2))
    (should (equal error-message "Failed to handle app-server message: boom"))
    (should-not (map-elt client :message-queue))
    (should-not (map-elt client :message-queue-busy))))

(provide 'agent-shell-codex-app-server-tests)
;;; agent-shell-codex-app-server-tests.el ends here
