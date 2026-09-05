;;; agent-shell-codex-app-server-tests.el --- Tests for Codex app-server ACP bridge -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'acp)
(require 'agent-shell-codex-app-server)

(defvar agent-shell--version)
(defvar agent-shell--state)

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

(ert-deftest agent-shell-codex-app-server-routes-acp-subscriptions ()
  "App-server subscriptions should use the ACP public API."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (dolist (entry '((acp-subscribe-to-errors
                      agent-shell-codex-app-server-subscribe-to-errors
                      :on-error)
                     (acp-subscribe-to-notifications
                      agent-shell-codex-app-server-subscribe-to-notifications
                      :on-notification)
                     (acp-subscribe-to-requests
                      agent-shell-codex-app-server-subscribe-to-requests
                      :on-request)))
      (let (called)
        (cl-letf (((symbol-function (nth 1 entry))
                   (lambda (&rest args)
                     (setq called args)
                     'subscribed)))
          (should (eq (funcall (nth 0 entry)
                               :client client
                               (nth 2 entry) #'ignore)
                      'subscribed))
          (should (eq (plist-get called :client) client)))))))

(ert-deftest agent-shell-codex-app-server-steers-active-turn-test ()
  "Busy prompt input should use `turn/steer' for the active Codex turn."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (state `((:buffer . ,(current-buffer))
                  (:client . ,client)))
         request-args
         fallback-called)
    (map-put! client :thread-id "thread-1")
    (map-put! client :active-turn-id "turn-1")
    (map-put! client :pending-prompt '((:turn-id . "turn-1")))
    (cl-letf (((symbol-function 'agent-shell--build-content-blocks)
               (lambda (_prompt)
                 '(((type . "text") (text . "steer left")))))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq request-args args))))
      (should
       (agent-shell-codex-app-server-handle-busy-prompt
        `((:state . ,state)
          (:prompt . "steer left")
          (:fallback . ,(lambda ()
                          (setq fallback-called t))))))
      (should (equal (plist-get request-args :method) "turn/steer"))
      (should (equal (map-elt (plist-get request-args :params) 'threadId)
                     "thread-1"))
      (should (equal (map-elt (plist-get request-args :params) 'expectedTurnId)
                     "turn-1"))
      (should (equal (map-elt (seq-first (map-elt (plist-get request-args :params)
                                                  'input))
                              'text)
                     "steer left"))
      (funcall (plist-get request-args :on-success) '((turnId . "turn-1")))
      (should-not fallback-called)
      (funcall (plist-get request-args :on-failure) nil nil)
      (should fallback-called))))

(ert-deftest agent-shell-codex-app-server-does-not-steer-stale-turn-test ()
  "Busy prompt input should queue normally when Codex has no live prompt."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (state `((:buffer . ,(current-buffer))
                  (:client . ,client))))
    (map-put! client :thread-id "thread-1")
    (map-put! client :active-turn-id "turn-1")
    (should-not
     (agent-shell-codex-app-server-handle-busy-prompt
      `((:state . ,state)
        (:prompt . "next turn")
        (:fallback . ,#'ignore))))))

(ert-deftest agent-shell-codex-app-server-initialize-uses-distinct-client-name ()
  "Initialize should report Agent Shell's distinct client name to Codex."
  (let ((agent-shell--version nil)
        (client (agent-shell-codex-app-server-make-client :command "sh"))
        request-args
        notification-args
        response)
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
       :request '((:method . "initialize"))
       :on-success (lambda (result)
                     (setq response result)))
      (should (equal (map-nested-elt (plist-get request-args :params)
                                     '(clientInfo name))
                     agent-shell-codex-app-server--client-name))
      (should (equal (map-nested-elt (plist-get request-args :params)
                                     '(clientInfo title))
                     "Emacs Agent Shell"))
      (should (equal (map-nested-elt (plist-get request-args :params)
                                     '(clientInfo version))
                     "unknown"))
      (should (equal (map-elt notification-args :method) "initialized"))
      (should (eq (map-nested-elt response '(sessionCapabilities fork)) t)))))

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

(ert-deftest agent-shell-codex-app-server-thread-name-updates-session-title ()
  "Thread name updates should update the matching shell session title."
  (let* ((target-buffer (generate-new-buffer " *agent-shell-codex-session*"))
         (client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :context-buffer target-buffer)))
    (unwind-protect
        (progn
          (map-put! client :thread-id "thread-1")
          (with-current-buffer target-buffer
            (setq-local agent-shell--state
                        `((:session . ((:id . "thread-1")
                                       (:title . nil))))))
          (agent-shell-codex-app-server--handle-notification
           client
           '((method . "thread/name/updated")
             (params . ((threadId . "other-thread")
                        (threadName . "Wrong title")))))
          (with-current-buffer target-buffer
            (should-not (map-nested-elt agent-shell--state '(:session :title))))
          (agent-shell-codex-app-server--handle-notification
           client
           '((method . "thread/name/updated")
             (params . ((threadId . "thread-1")
                        (threadName . "Useful title")))))
          (with-current-buffer target-buffer
            (should (equal (map-nested-elt agent-shell--state '(:session :title))
                           "Useful title"))))
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
  "Plan updates should expose ACP-style `content' and snake_case statuses.

Codex sends `TurnPlanUpdatedNotification' with the steps directly under
`params.plan', naming the text `step' and using camelCase statuses."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         delivered)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq delivered notification)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/plan/updated")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (plan . (((step . "Inspect transport")
                            (status . "completed"))
                           ((step . "Normalize plan entries")
                            (status . "inProgress"))
                           ((step . "Run tests")
                            (status . "pending"))))))))
    (should (equal (map-nested-elt delivered '(params update entries))
                   '(((content . "Inspect transport")
                      (status . "completed"))
                     ((content . "Normalize plan entries")
                      (status . "in_progress"))
                     ((content . "Run tests")
                      (status . "pending")))))))

(ert-deftest agent-shell-codex-app-server-completed-plan-item-is-visible ()
  "Completed Codex plan items should render as agent text."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq delivered notification)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/completed")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (item . ((id . "turn-1-plan")
                           (type . "plan")
                           (text . "# Final plan\n- Inspect\n- Test")))))))
    (should (equal (map-nested-elt delivered '(params update sessionUpdate))
                   "agent_message_chunk"))
    (should (equal (map-nested-elt delivered '(params update content text))
                   "# Final plan\n- Inspect\n- Test"))))

(ert-deftest agent-shell-codex-app-server-message-deltas-use-typed-text-content ()
  "Message deltas should translate to ACP typed text content blocks."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        notifications)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/agentMessage/delta")
       (params . ((delta . "hello")))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/reasoning/textDelta")
       (params . ((delta . "thinking")))))
    (should (equal (map-nested-elt (cadr notifications)
                                   '(params update sessionUpdate))
                   "agent_message_chunk"))
    (should (equal (map-nested-elt (cadr notifications)
                                   '(params update content type))
                   "text"))
    (should (equal (map-nested-elt (cadr notifications)
                                   '(params update content text))
                   "hello"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update sessionUpdate))
                   "agent_thought_chunk"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update content type))
                   "text"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update content text))
                   "thinking"))))

(ert-deftest agent-shell-codex-app-server-ignores-child-thread-notifications ()
  "Child thread output should not enter the parent Agent Shell view."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        notifications)
    (map-put! client :thread-id "thread-root")
    (map-put! client :active-turn-id "turn-root")
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "thread/started")
       (params . ((thread . ((id . "thread-child")
                             (parentThreadId . "thread-root")))))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/agentMessage/delta")
       (params . ((threadId . "thread-child")
                  (turnId . "turn-child")
                  (itemId . "message-child")
                  (delta . "child output")))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/started")
       (params . ((threadId . "thread-child")
                  (turnId . "turn-child")
                  (item . ((id . "command-child")
                           (type . "commandExecution")
                           (command . "printf child")))))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/started")
       (params . ((threadId . "thread-child")
                  (turn . ((id . "turn-child")))))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((threadId . "thread-child")
                  (turn . ((id . "turn-child")
                           (status . "completed")))))))
    (should (equal (map-elt client :thread-id) "thread-root"))
    (should (equal (map-elt client :active-turn-id) "turn-root"))
    (should-not (gethash "command-child" (map-elt client :tool-items)))
    (should-not notifications)))

(ert-deftest agent-shell-codex-app-server-renders-root-collaboration-updates ()
  "Root collaboration updates should remain visible when children are filtered."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (map-put! client :thread-id "thread-root")
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq delivered notification)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/started")
       (params . ((threadId . "thread-root")
                  (turnId . "turn-root")
                  (item . ((id . "collab-root")
                           (type . "collabAgentToolCall")
                           (tool . "spawnAgent")
                           (prompt . "Review the adapter")
                           (receiverThreadIds . ("thread-child"))))))))
    (should (equal (map-nested-elt delivered
                                   '(params update sessionUpdate))
                   "tool_call"))
    (should (equal (map-nested-elt delivered
                                   '(params update toolCallId))
                   "collab-root"))))

(ert-deftest agent-shell-codex-app-server-renders-late-sub-agent-completion ()
  "A child completion may arrive after its parent turn has completed."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        notifications)
    (map-put! client :thread-id "thread-root")
    (agent-shell-codex-app-server--dismiss-turn client "turn-root")
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (dolist (method '("item/started" "item/completed"))
      (agent-shell-codex-app-server--handle-notification
       client
       `((method . ,method)
         (params . ((threadId . "thread-root")
                    (turnId . "turn-root")
                    (item . ((id . "activity-1")
                             (type . "subAgentActivity")
                             (kind . "completed")
                             (agentThreadId . "thread-child")
                             (agentPath . "/root/reviewer"))))))))
    (should (= (length notifications) 2))
    (should (equal (map-nested-elt (cadr notifications)
                                   '(params update sessionUpdate))
                   "tool_call"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update sessionUpdate))
                   "tool_call_update"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update status))
                   "completed"))))

(ert-deftest agent-shell-codex-app-server-buffers-leading-markdown-heading ()
  "A heading-only first delta should remain visible when content follows."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        notifications)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/agentMessage/delta")
       (params . ((itemId . "message-1")
                  (delta . "# Review Request\n")))))
    (should-not notifications)
    (should (equal (map-nested-elt client '(:pending-agent-message :text))
                   "# Review Request\n"))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/agentMessage/delta")
       (params . ((itemId . "message-1")
                  (delta . "\nReview these changes.")))))
    (should-not (map-elt client :pending-agent-message))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update content text))
                   "# Review Request\n\nReview these changes."))))

(ert-deftest agent-shell-codex-app-server-renders-async-agent-messages ()
  "Async agent messages should render without a streaming delta."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq delivered notification)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/completed")
       (params . ((threadId . "thread-root")
                  (turnId . "turn-root")
                  (item . ((id . "message-1")
                           (type . "agentMessage")
                           (text . "Still investigating.")
                           (phase . "finalAnswer")
                           (delivery . "async")))))))
    (should (equal (map-nested-elt delivered
                                   '(params update sessionUpdate))
                   "agent_message_chunk"))
    (should (equal (map-nested-elt delivered '(params update content text))
                   "Still investigating."))))

(ert-deftest agent-shell-codex-app-server-async-questions-do-not-block ()
  "Structured questions display once without reading from the minibuffer."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        (item '((id . "question-1") (type . "agentMessage") (delivery . "async")
                (text . "Which color?")
                (questions . (((title . "Which color?") (options . ("Red" "Blue")))))))
        displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (push buffer displayed)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Notification handler blocked"))))
          (dotimes (_ 2)
            (agent-shell-codex-app-server--handle-notification
             client `((method . "item/completed") (params . ((item . ,item))))))
          (should (= (length displayed) 1))
          (with-current-buffer (car displayed)
            (should (string-match-p "Which color?" (buffer-string)))
            (should (string-match-p "Blue" (buffer-string)))
            (search-forward "Dismiss")
            (button-activate (button-at (1- (point))))))
      (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) displayed))
    (should (eq (map-elt (gethash "question-1" (map-elt client :async-questions)) :status)
                'dismissed))))

(ert-deftest agent-shell-codex-app-server-async-answer-uses-originating-queue ()
  "Free text answers go to the originating shell once, even after a turn ends."
  (require 'agent-shell-prompt-queue)
  (with-temp-buffer
    (let* ((shell (current-buffer))
           (client (agent-shell-codex-app-server-make-client :command "sh" :context-buffer shell))
           (agent-shell--state `((:client . ,client)))
           (context `((:client . ,client) (:status . pending)
                      (:questions . (((title . "Which color?") (options . ("Red" "Blue")))))))
           sent)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Green"))
                ((symbol-function 'agent-shell-prompt-queue)
                 (lambda (text) (push (cons (current-buffer) text) sent))))
        (agent-shell-codex-app-server--answer-async-questions context)
        (should (equal sent (list (cons shell "Which color?\nGreen"))))
        (should-error (agent-shell-codex-app-server--answer-async-questions context)
                      :type 'user-error)
        (should (= (length sent) 1))))))

(ert-deftest agent-shell-codex-app-server-async-answer-quit-keeps-question ()
  "Cancelling answer entry leaves questions available and does not interrupt."
  (let ((context `((:client . ,(agent-shell-codex-app-server-make-client :command "sh"))
                   (:status . pending) (:questions . (((title . "Why?")))))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil
          (agent-shell-codex-app-server--answer-async-questions context)
        (quit nil)))
    (should (eq (map-elt context :status) 'pending))))

(ert-deftest agent-shell-codex-app-server-async-answer-rejects-stale-session ()
  "Questions cannot send answers into a replacement session."
  (with-temp-buffer
    (let* ((client (agent-shell-codex-app-server-make-client
                    :command "sh" :context-buffer (current-buffer)))
           (agent-shell--state '((:client . other)))
           (context `((:client . ,client) (:status . pending)
                      (:questions . (((title . "Why?")))))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Because")))
        (should-error (agent-shell-codex-app-server--answer-async-questions context)
                      :type 'user-error))
      (should (eq (map-elt context :status) 'pending)))))

(ert-deftest agent-shell-codex-app-server-network-policy-decision-labels ()
  "Network policy denial must not be offered as approval."
  (dolist (action '("allow" "deny" "unknown"))
    (let* ((decision `((applyNetworkPolicyAmendment
                       . ((network_policy_amendment . ((host . "example.com") (action . ,action)))))))
           (spec (agent-shell-codex-app-server--decision-option-spec decision)))
      (if (equal action "unknown")
          (should-not spec)
        (should (equal (map-elt spec :payload) decision))
        (should (equal (map-elt spec :kind)
                       (if (equal action "deny") "reject_always" "allow_always")))
        (should (equal (map-elt spec :name)
                       (if (equal action "deny") "Deny Network: example.com" "Allow Network: example.com")))))))

(ert-deftest agent-shell-codex-app-server-dynamic-tool-result-content ()
  "Dynamic results include text without dumping encoded media."
  (should (equal (agent-shell-codex-app-server--result-text
                  '((type . "dynamicToolCall")
                    (contentItems . (((type . "inputText") (text . "Result"))
                                     ((type . "inputImage") (imageUrl . "data:secret"))))))
                 "Result\n\n[Image output]")))

(ert-deftest agent-shell-codex-app-server-flushes-heading-only-message ()
  "A heading-only message should flush visibly before turn completion."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        notifications)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/agentMessage/delta")
       (params . ((itemId . "message-1")
                  (delta . "# Review Request\n")))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-1")
                           (status . "completed")))))))
    (should-not (map-elt client :pending-agent-message))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update content text))
                   "# Review Request"))))

(ert-deftest agent-shell-codex-app-server-separates-reasoning-summary-parts ()
  "Reasoning summary parts should remain visually distinct."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        notifications)
    (agent-shell-codex-app-server-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/reasoning/summaryPartAdded")
       (params . ((turnId . "turn-1")
                  (summaryIndex . 0)))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/reasoning/summaryPartAdded")
       (params . ((turnId . "turn-1")
                  (summaryIndex . 1)))))
    (should (= (length notifications) 1))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update content text))
                   "\n\n"))))

(ert-deftest agent-shell-codex-app-server-file-change-content-uses-acp-diffs ()
  "File change content should use ACP diff content blocks."
  (let* ((changes '(((path . "new.txt")
                     (kind . ((type . "add")))
                     (diff . "hello\n"))
                    ((path . "old.txt")
                     (kind . ((type . "delete")))
                     (diff . "bye\n"))
                    ((path . "changed.txt")
                     (kind . ((type . "update")
                              (move_path . nil)))
                     (diff . "@@ -1 +1 @@\n-old\n+new\n"))))
         (content (agent-shell-codex-app-server--tool-content
                   (agent-shell-codex-app-server-make-client :command "sh")
                   `((id . "edit-1")
                     (type . "fileChange")
                     (changes . ,changes)))))
    (should (vectorp content))
    (should (= (length content) 3))
    (should (equal (map-nested-elt (elt content 0) '(type)) "diff"))
    (should (equal (map-nested-elt (elt content 0) '(path)) "new.txt"))
    (should (equal (map-nested-elt (elt content 0) '(oldText)) ""))
    (should (equal (map-nested-elt (elt content 0) '(newText)) "hello\n"))
    (should (equal (map-nested-elt (elt content 1) '(path)) "old.txt"))
    (should (equal (map-nested-elt (elt content 1) '(oldText)) "bye\n"))
    (should (equal (map-nested-elt (elt content 1) '(newText)) ""))
    (should (equal (map-nested-elt (elt content 2) '(path)) "changed.txt"))
    (should (equal (map-nested-elt (elt content 2) '(oldText)) "old"))
    (should (equal (map-nested-elt (elt content 2) '(newText)) "new"))))

(ert-deftest agent-shell-codex-app-server-patch-updates-render-edit-diffs ()
  "Patch updates should emit tool updates with ACP diff content."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        (changes '(((path . "new.txt")
                    (kind . ((type . "add")))
                    (diff . "hello\n"))))
        notifications)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/started")
       (params . ((item . ((id . "edit-1")
                           (type . "fileChange")
                           (changes . nil)))))))
    (agent-shell-codex-app-server--handle-notification
     client
     `((method . "item/fileChange/patchUpdated")
       (params . ((itemId . "edit-1")
                  (changes . ,changes)))))
    (let ((update (car notifications)))
      (should (equal (map-nested-elt update '(params update sessionUpdate))
                     "tool_call_update"))
      (should (equal (map-nested-elt update '(params update toolCallId))
                     "edit-1"))
      (should (equal (map-nested-elt update '(params update title))
                     "new.txt"))
      (should (equal (map-nested-elt update '(params update kind))
                     "edit"))
      (let ((diff (elt (map-nested-elt update '(params update content)) 0)))
        (should (equal (map-elt diff 'type) "diff"))
        (should (equal (map-elt diff 'path) "new.txt"))
        (should (equal (map-elt diff 'oldText) ""))
        (should (equal (map-elt diff 'newText) "hello\n"))))))

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

(ert-deftest agent-shell-codex-app-server-notice-notifications-use-error-handlers ()
  "Warning-style app-server notifications should surface as notices."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (agent-shell-codex-app-server-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (push (map-elt error 'message) delivered)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "warning")
       (params . ((message . "Network may be restricted")))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "configWarning")
       (params . ((summary . "Bad config")
                  (details . "Using defaults")))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "deprecationNotice")
       (params . ((summary . "Old setting")
                  (details . :json-false)))))
    (map-put! client :current-model-id "gpt-5.6-codex")
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "model/rerouted")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (fromModel . "gpt-5.6-codex")
                  (toModel . "gpt-5.5-codex")
                  (reason . "highRiskCyberActivity")))))
    (should (equal (nreverse delivered)
                   '("Network may be restricted"
                     "Bad config: Using defaults"
                     "Old setting"
                     "Codex rerouted this turn from gpt-5.6-codex to gpt-5.5-codex (highRiskCyberActivity)")))
    (should (equal (map-elt client :current-model-id) "gpt-5.6-codex"))))

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

(ert-deftest agent-shell-codex-app-server-write-stdin-approvals-use-callback-id ()
  "Terminal-input approvals should use their distinct callback identity."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (translated
          (agent-shell-codex-app-server--translate-request
           client
           '((method . "item/commandExecution/requestApproval")
             (id . 33)
             (params . ((kind . "writeStdin")
                        (itemId . "command-1")
                        (approvalId . "approval-1")
                        (reason . "Send input to the running command")
                        (cwd . "/tmp")))))))
    (should (equal (map-nested-elt translated '(params toolCall toolCallId))
                   "approval-1"))
    (should (equal (map-nested-elt translated '(params toolCall title))
                   "Send input to the running command"))
    (should (equal (map-nested-elt translated '(params toolCall kind))
                   "execute"))
    (should (equal (map-nested-elt translated
                                   '(params toolCall rawInput approvalId))
                   "approval-1"))
    (should-not (map-nested-elt translated '(params toolCall rawInput command)))))

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
    (should (vectorp (map-nested-elt (car notifications)
                                     '(params update content))))
    (should (json-serialize (car notifications)))
    (should (equal (map-nested-elt (elt (map-nested-elt (car notifications)
                                                        '(params update content))
                                        0)
                                   '(content type))
                   "text"))
    (should (equal (map-nested-elt (elt (map-nested-elt (car notifications)
                                                        '(params update content))
                                        0)
                                   '(content text))
                   "alphabeta"))
    (should (zerop (hash-table-count (map-elt client :pending-tool-output-items))))
    (should-not (map-elt client :tool-output-flush-timer))))

(ert-deftest agent-shell-codex-app-server-bounds-streamed-command-output ()
  "Streamed command output should retain full state but render a bounded tail."
  (let* ((agent-shell-codex-app-server--tool-output-display-limit 5)
         (client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "printf abcdefghi")
                 (cwd . "/tmp")))
         translated)
    (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
    (let ((agent-shell-codex-app-server--output-flush-interval nil))
      (agent-shell-codex-app-server--translate-command-output
       client '((itemId . "tool-1")
                (delta . "abc")))
      (setq translated
            (agent-shell-codex-app-server--translate-command-output
             client '((itemId . "tool-1")
                      (delta . "defghi")))))
    (should
     (equal (map-nested-elt
             (elt (map-nested-elt translated '(params update content)) 0)
             '(content text))
            (concat agent-shell-codex-app-server--tool-output-truncated-prefix
                    "efghi")))
    (should (equal (agent-shell-codex-app-server--tool-output-text
                    client
                    "tool-1")
                   "abcdefghi"))))

(ert-deftest agent-shell-codex-app-server-completed-command-output-is-complete ()
  "Completed command output should not inherit the streaming display bound."
  (let* ((agent-shell-codex-app-server--tool-output-display-limit 5)
         (client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "printf abcdefghi")
                 (cwd . "/tmp"))))
    (puthash "tool-1" "abcdefghi" (map-elt client :tool-outputs))
    (should
     (equal (map-nested-elt
             (elt (agent-shell-codex-app-server--tool-content client item t) 0)
             '(content text))
            "abcdefghi"))
    (should (equal (gethash "tool-1" (map-elt client :tool-outputs))
                   "abcdefghi"))))

(ert-deftest agent-shell-codex-app-server-completed-items-flush-buffered-output ()
  "Completed items should render buffered output even before debounce fires."
  (let* ((agent-shell-codex-app-server--tool-output-display-limit 5)
         (client (agent-shell-codex-app-server-make-client :command "sh"))
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
    (should (vectorp (map-nested-elt (car notifications)
                                     '(params update content))))
    (should (json-serialize (car notifications)))
    (should (equal (map-nested-elt (elt (map-nested-elt (car notifications)
                                                        '(params update content))
                                        0)
                                   '(content type))
                   "text"))
    (should (equal (map-nested-elt (elt (map-nested-elt (car notifications)
                                                        '(params update content))
                                        0)
                                   '(content text))
                   "alphabeta"))
    (should (equal (map-nested-elt (car notifications) '(params update status))
                   "completed"))
    (should-not (gethash "tool-1" (map-elt client :tool-items)))
    (should-not (gethash "tool-1" (map-elt client :tool-outputs)))
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
    (should (vectorp content))
    (should (equal (map-nested-elt (elt content 0) '(content type))
                   "text"))
    (should (equal (map-nested-elt (elt content 0) '(content text))
                   "README.org:69"))))

(ert-deftest agent-shell-codex-app-server-mcp-read-only-tools-use-read-kind ()
  "MCP tools with a read-only hint should render as read operations."
  (let ((entry
         (agent-shell-codex-app-server--tool-entry-from-item
          '((id . "tool-1")
            (type . "mcpToolCall")
            (server . "codex")
            (tool . "find")
            (readOnlyHint . t)))))
    (should (equal (map-elt entry :kind) "read"))))

(ert-deftest agent-shell-codex-app-server-sleep-items-render-as-tools ()
  "Sleep lifecycle items should render with their requested duration."
  (let ((entry
         (agent-shell-codex-app-server--tool-entry-from-item
          '((id . "sleep-1")
            (type . "sleep")
            (durationMs . 1500)))))
    (should (equal (map-elt entry :title) "Wait 1.5s"))
    (should (equal (map-elt entry :kind) "other"))
    (should (= (map-nested-elt entry '(:raw-input durationMs)) 1500))))

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

(ert-deftest agent-shell-codex-app-server-renders-collaboration-items ()
  "Collaboration items should render as ordinary tool calls."
  (let* ((item '((id . "collab-1")
                 (type . "collabAgentToolCall")
                 (tool . "spawnAgent")
                 (status . "completed")
                 (senderThreadId . "thread-1")
                 (prompt . "Review the adapter")
                 (model . "gpt-5.4")
                 (reasoningEffort . "high")
                 (receiverThreadIds . ("thread-2"))
                 (agentsStates . (("thread-2" . ((status . "completed")
                                                  (message . nil)))))))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :title) "Spawn agent"))
    (should (equal (map-elt entry :kind) "other"))
    (should (equal (map-elt entry :description) "Review the adapter"))
    (should (equal (map-nested-elt entry '(:raw-input receiverThreadIds))
                   '("thread-2")))))

(ert-deftest agent-shell-codex-app-server-omits-empty-collaboration-fields ()
  "Absent collaboration details should not render as JSON null values."
  (let* ((item '((id . "collab-1")
                 (type . "collabAgentToolCall")
                 (tool . "wait")
                 (status . "completed")
                 (senderThreadId . "thread-1")
                 (receiverThreadIds . nil)
                 (prompt . nil)
                 (model . nil)
                 (reasoningEffort . nil)
                 (agentsStates . nil)))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (should (equal (map-elt entry :title) "Wait for agents"))
    (should (equal (map-elt entry :raw-input)
                   '((description . "Wait for agents"))))))

(ert-deftest agent-shell-codex-app-server-recognizes-current-collaboration-tools ()
  "Current private collaboration tool variants should have useful titles."
  (dolist (spec '(("sendMessage" . "Send agent message")
                  ("followupTask" . "Follow up with agent")
                  ("interruptAgent" . "Interrupt agent")
                  ("listAgents" . "List agents")))
    (let ((entry
           (agent-shell-codex-app-server--tool-entry-from-item
            `((id . "collab-1")
              (type . "collabAgentToolCall")
              (tool . ,(car spec))
              (status . "completed")
              (senderThreadId . "thread-1")
              (receiverThreadIds . nil)
              (agentsStates . nil)))))
      (should (equal (map-elt entry :title) (cdr spec))))))

(ert-deftest agent-shell-codex-app-server-renders-completed-sub-agent-activity ()
  "Multi-Agent V2 completion activity should have a specific label."
  (let ((entry
         (agent-shell-codex-app-server--tool-entry-from-item
          '((id . "activity-1")
            (type . "subAgentActivity")
            (kind . "completed")
            (agentThreadId . "thread-2")
            (agentPath . "/root/reviewer")))))
    (should (equal (map-elt entry :title) "Completed /root/reviewer"))
    (should (equal (map-elt entry :description) "Completed"))))

(ert-deftest agent-shell-codex-app-server-normalizes-interrupted-tool-status ()
  "Interrupted collaboration calls should use a supported terminal status."
  (should (equal (agent-shell-codex-app-server--normalize-status "interrupted")
                 "failed")))

(ert-deftest agent-shell-codex-app-server-renders-image-generation-without-payload ()
  "Image generation should show its saved path without embedding base64 output."
  (let* ((item '((id . "image-1")
                 (type . "imageGeneration")
                 (status . "completed")
                 (revisedPrompt . "A precise diagram")
                 (result . "large-base64-payload")
                 (savedPath . "/tmp/diagram.png")))
         (content (agent-shell-codex-app-server--tool-content
                   (agent-shell-codex-app-server-make-client :command "sh")
                   item)))
    (should (equal (map-nested-elt (elt content 0) '(content text))
                   "/tmp/diagram.png"))
    (should-not (string-match-p "base64" (json-serialize content)))))

(ert-deftest agent-shell-codex-app-server-renders-mcp-progress ()
  "MCP progress notifications should update the active tool call."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (agent-shell-codex-app-server--save-tool-entry
     client
     '((id . "mcp-1")
       (type . "mcpToolCall")
       (server . "browser")
       (tool . "navigate"))
     "inProgress")
    (agent-shell-codex-app-server-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (setq delivered notification)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/mcpToolCall/progress")
       (params . ((turnId . "turn-1")
                  (itemId . "mcp-1")
                  (message . "Waiting for page load")))))
    (should (equal (map-nested-elt
                    (elt (map-nested-elt delivered '(params update content)) 0)
                    '(content text))
                   "Waiting for page load"))))

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
    (should (vectorp content))
    (should (equal (map-nested-elt (elt content 0) '(content type))
                   "text"))
    (should (equal (map-nested-elt (elt content 0) '(content text))
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

(ert-deftest agent-shell-codex-app-server-routes-mcp-elicitation-requests ()
  "MCP elicitations should become permission prompts, not unsupported errors."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         dispatched
         rpc-error
         captured-result
         (request
          '((method . "mcpServer/elicitation/request")
            (id . 63)
            (params . ((threadId . "thr-1")
                       (turnId . "turn-1")
                       (serverName . "playwright")
                       (mode . "form")
                       (_meta . nil)
                       (message . "Allow browser navigation?")
                       (requestedSchema . ((type . "object")
                                           (properties . nil)
                                           (required . nil))))))))
    (agent-shell-codex-app-server-subscribe-to-requests
     :client client
     :on-request (lambda (translated)
                   (setq dispatched translated)))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-error)
               (lambda (&rest _args)
                 (setq rpc-error t)))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server--route-message client request)
      (should-not rpc-error)
      (should (equal (map-elt dispatched 'method) "session/request_permission"))
      (should (equal (map-elt dispatched 'id) 63))
      (should (equal (map-nested-elt dispatched '(params toolCall title))
                     "Allow browser navigation?"))
      (should (equal (map-nested-elt dispatched '(params toolCall kind))
                     "other"))
      (should (equal (mapcar (lambda (option)
                               (map-elt option 'optionId))
                             (map-nested-elt dispatched '(params options)))
                     '("accept" "decline" "cancel")))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 63
       :option-id "accept"))
    (should (equal (map-elt captured-result 'action) "accept"))
    (should-not (map-elt captured-result 'content))
    (should-not (map-elt captured-result '_meta))))

(ert-deftest agent-shell-codex-app-server-mcp-elicitations-expose-persistence ()
  "Approval elicitations should expose advertised persistence scopes."
  (let* ((translated
          (agent-shell-codex-app-server--translate-request
           (agent-shell-codex-app-server-make-client :command "sh")
           '((method . "mcpServer/elicitation/request")
             (id . 64)
             (params . ((serverName . "playwright")
                        (mode . "form")
                        (_meta . ((persist . ("session" "always"))))
                        (message . "Allow browser navigation?")
                        (requestedSchema . ((type . "object")
                                            (properties . nil))))))))
         (options (map-nested-elt translated '(params options))))
    (should (equal (mapcar (lambda (option) (map-elt option 'optionId)) options)
                   '("accept" "acceptForSession" "acceptAlways"
                     "decline" "cancel")))))

(ert-deftest agent-shell-codex-app-server-collects-typed-mcp-form-content ()
  "Standard MCP form fields should retain their protocol value types.

Every prompt is also attributed to the requesting MCP server, since the
minibuffer is the only place the user sees who is asking."
  (cl-letf (((symbol-function 'read-string)
             (lambda (prompt &rest _args)
               (should (string-prefix-p "[docs] " prompt))
               (if (string-prefix-p "[docs] Count" prompt) "3" "Ada")))
            ((symbol-function 'completing-read)
             (lambda (prompt &rest _args)
               (cond
                ((string-prefix-p "[docs] Enabled" prompt) "No")
                ((string-prefix-p "[docs] Color" prompt) "Green")
                (t (error "Unexpected prompt: %s" prompt)))))
            ((symbol-function 'completing-read-multiple)
             (lambda (prompt &rest _args)
               (should (string-prefix-p "[docs] " prompt))
               '("Read" "Write"))))
    (let ((content
           (agent-shell-codex-app-server--collect-mcp-form-content
            '((serverName . "docs")
              (requestedSchema
               . ((type . "object")
                  (required . ("name" "count" "enabled" "color" "scopes"))
                  (properties
                   . ((name . ((type . "string") (title . "Name")))
                      (count . ((type . "integer") (title . "Count")))
                      (enabled . ((type . "boolean") (title . "Enabled")))
                      (color . ((type . "string")
                                (title . "Color")
                                (oneOf . (((const . "green")
                                           (title . "Green"))))))
                      (scopes . ((type . "array")
                                 (title . "Scopes")
                                 (items . ((anyOf
                                            . (((const . "read")
                                                (title . "Read"))
                                               ((const . "write")
                                                (title . "Write"))))))))))))))))
      (should (equal (map-elt content 'name) "Ada"))
      (should (= (map-elt content 'count) 3))
      (should (eq (map-elt content 'enabled) :json-false))
      (should (equal (map-elt content 'color) "green"))
      (should (equal (map-elt content 'scopes) ["read" "write"])))))

(ert-deftest agent-shell-codex-app-server-omits-empty-optional-mcp-form-fields ()
  "Optional MCP form fields left empty should stay out of the response.

Sending them as empty strings would fail the server's typed schema."
  (cl-letf (((symbol-function 'read-string)
             ;; Mirror `read-string': empty input yields DEFAULT-VALUE.
             (lambda (_prompt &optional _initial _history default &rest _args)
               (or default "")))
            ((symbol-function 'completing-read) (lambda (&rest _args) ""))
            ((symbol-function 'completing-read-multiple) (lambda (&rest _args) nil)))
    (let ((content
           (agent-shell-codex-app-server--collect-mcp-form-content
            '((requestedSchema
               . ((type . "object")
                  (required . ("kept"))
                  (properties
                   . ((kept . ((type . "string") (default . "value")))
                      (note . ((type . "string")))
                      (count . ((type . "integer")))
                      (enabled . ((type . "boolean")))
                      (color . ((type . "string") (enum . ("red" "green"))))
                      (scopes . ((type . "array")
                                 (items . ((type . "string")
                                           (enum . ("read"))))))))))))))
      (should (equal (map-keys content) '(kept)))
      (should (equal (map-elt content 'kept) "value")))))

(ert-deftest agent-shell-codex-app-server-revalidates-out-of-range-mcp-numbers ()
  "Numeric MCP form fields should re-prompt until the schema is satisfied."
  (let ((answers '("not-a-number" "99" "5"))
        prompts)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) (pop answers)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) prompts))))
      (should (= (agent-shell-codex-app-server--read-mcp-form-value
                  "" "count"
                  '((type . "integer") (minimum . 1) (maximum . 10))
                  t)
                 5)))
    (should (equal (nreverse prompts)
                   '("Enter a valid integer"
                     "Value is outside the allowed range")))))

(ert-deftest agent-shell-codex-app-server-reads-legacy-enum-labels ()
  "Legacy `enum'/`enumNames' schemas should map labels back to values."
  (let (offered)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq offered collection)
                 "Green")))
      (should (equal (agent-shell-codex-app-server--read-mcp-form-value
                      "" "color"
                      '((type . "string")
                        (enum . ("red" "green"))
                        (enumNames . ("Red" "Green")))
                      t)
                     "green")))
    (should (equal offered '("Red" "Green")))))

(ert-deftest agent-shell-codex-app-server-declines-unsupported-mcp-form-fields ()
  "An unsupported field type should decline the form rather than answer it."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        captured-result
        error-message)
    (agent-shell-codex-app-server-subscribe-to-errors
     :client client
     :on-error (lambda (error) (setq error-message (map-elt error 'message))))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server--route-message
       client
       '((method . "mcpServer/elicitation/request")
         (id . 67)
         (params . ((serverName . "docs")
                    (mode . "form")
                    (requestedSchema
                     . ((type . "object")
                        (properties . ((blob . ((type . "object"))))))))))))
    (should (equal (map-elt captured-result 'action) "decline"))
    (should (string-prefix-p "Failed to render MCP form" error-message))))

(ert-deftest agent-shell-codex-app-server-routes-typed-mcp-forms-directly ()
  "Non-empty standard MCP forms should return typed content directly."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        dispatched
        captured-result)
    (agent-shell-codex-app-server-subscribe-to-requests
     :client client
     :on-request (lambda (request) (setq dispatched request)))
    (cl-letf (((symbol-function
                'agent-shell-codex-app-server--collect-mcp-form-content)
               (lambda (_params) '((name . "Ada"))))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server--route-message
       client
       '((method . "mcpServer/elicitation/request")
         (id . 65)
         (params . ((mode . "form")
                    (requestedSchema . ((type . "object")
                                        (properties
                                         . ((name . ((type . "string"))))))))))))
    (should-not dispatched)
    (should (equal (map-elt captured-result 'action) "accept"))
    (should (equal (map-nested-elt captured-result '(content name)) "Ada"))))

(ert-deftest agent-shell-codex-app-server-declines-unadvertised-openai-forms ()
  "OpenAI extension forms should be declined when not negotiated."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        captured-result)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server--route-message
       client
       '((method . "mcpServer/elicitation/request")
         (id . 66)
         (params . ((mode . "openai/form")
                    (requestedSchema . ((custom . t))))))))
    (should (equal (map-elt captured-result 'action) "decline"))))

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

(ert-deftest agent-shell-codex-app-server-legacy-denials-include-reason ()
  "Legacy approval denials should include Codex's required rejection reason."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         captured-result)
    (agent-shell-codex-app-server--translate-request
     client
     '((method . "execCommandApproval")
       (id . 70)
       (params . ((callId . "call-1")
                  (command . "rm foo")
                  (availableDecisions . ("denied"))))))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq captured-result (plist-get args :result)))))
      (agent-shell-codex-app-server-send-permission-response
       :client client
       :request-id 70
       :option-id "decision-0"))
    (should (equal (map-nested-elt captured-result
                                   '(decision denied rejection))
                   "Denied by user"))))

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

(ert-deftest agent-shell-codex-app-server-server-request-resolved-clears-pending-permission ()
  "A resolved server request should clear the matching pending permission."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (agent-shell-codex-app-server--translate-request
     client
     '((method . "item/permissions/requestApproval")
       (id . 77)
       (params . ((permissions . ((network . ((reason . "Need network")))))))))
    (should (gethash 77 (map-elt client :pending-permissions)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "serverRequest/resolved")
       (params . ((threadId . "thread-1")
                  (requestId . 77)))))
    (should-not (gethash 77 (map-elt client :pending-permissions)))))

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

(ert-deftest agent-shell-codex-app-server-resume-excludes-turn-payloads ()
  "Session resume responses should omit historical turn payloads."
  (dolist (method '("session/resume" "session/load"))
    (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
          request-method
          request-params
          response)
      (cl-letf (((symbol-function 'agent-shell-codex-app-server--fetch-models)
                 (lambda (_client on-success)
                   (funcall on-success)))
                ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
                 (lambda (&rest args)
                   (setq request-method (plist-get args :method)
                         request-params (plist-get args :params))
                   (funcall (plist-get args :on-success)
                            '((thread . ((id . "thread-1")))
                              (model . "gpt-5"))))))
        (agent-shell-codex-app-server-send-request
         :client client
         :request `((:method . ,method)
                    (:params . ((sessionId . "thread-1")
                                (cwd . "/tmp"))))
         :on-success (lambda (result)
                       (setq response result))))
      (should (equal request-method "thread/resume"))
      (should (equal (map-elt request-params 'threadId) "thread-1"))
      (should (eq (map-elt request-params 'excludeTurns) t))
      (should-not (map-contains-key request-params 'experimentalRawEvents))
      (should-not (map-contains-key request-params 'persistExtendedHistory))
      (should (equal (map-elt request-params 'cwd) "/tmp"))
      (should (equal (map-elt response 'sessionId) "thread-1")))))

(ert-deftest agent-shell-codex-app-server-resume-disables-visual-wrap ()
  "Session resume should avoid visual-wrap loops in large shell buffers."
  (let* ((target-buffer (generate-new-buffer " *agent-shell-codex-resume*"))
         (client (agent-shell-codex-app-server-make-client
                  :command "sh"
                  :context-buffer target-buffer))
         disabled)
    (unwind-protect
        (progn
          (with-current-buffer target-buffer
            (setq-local visual-wrap-prefix-mode t))
          (cl-letf (((symbol-function 'visual-wrap-prefix-mode)
                     (lambda (value)
                       (setq disabled value)
                       (setq visual-wrap-prefix-mode nil)))
                    ((symbol-function 'agent-shell-codex-app-server--fetch-models)
                     (lambda (_client _on-success))))
            (agent-shell-codex-app-server-send-request
             :client client
             :request '((:method . "session/resume")
                        (:params . ((sessionId . "thread-1")
                                    (cwd . "/tmp"))))))
          (should (= disabled -1))
          (with-current-buffer target-buffer
            (should-not visual-wrap-prefix-mode)))
      (kill-buffer target-buffer))))

(ert-deftest agent-shell-codex-app-server-forks-sessions ()
  "Session forks should use thread/fork and return the new session."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        request-method
        request-params
        response)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--fetch-models)
               (lambda (_client on-success)
                 (funcall on-success)))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq request-method (plist-get args :method)
                       request-params (plist-get args :params))
                 (funcall (plist-get args :on-success)
                          '((thread . ((id . "thread-fork")))
                            (model . "gpt-5"))))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/fork")
                  (:params . ((sessionId . "thread-source")
                              (cwd . "/tmp"))))
       :on-success (lambda (result)
                     (setq response result))))
    (should (equal request-method "thread/fork"))
    (should (equal (map-elt request-params 'threadId) "thread-source"))
    (should (eq (map-elt request-params 'excludeTurns) t))
    (should (equal (map-elt request-params 'cwd) "/tmp"))
    (should (equal (map-elt response 'sessionId) "thread-fork"))
    (should (equal (map-elt client :thread-id) "thread-fork"))))

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
  "Session mode changes should persist the reasoning effort with app-server."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         request-args
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
    (map-put! client :thread-id "thread-1")
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq request-args args))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/set_mode")
                  (:params . ((sessionId . "thread-1")
                              (modeId . "reasoning:high"))))
       :on-success (lambda (result)
                     (setq response result))))
    (should (equal (plist-get request-args :method) "thread/settings/update"))
    (should (equal (plist-get request-args :params)
                   '((threadId . "thread-1")
                     (effort . "high"))))
    (should-not (equal (map-elt client :reasoning-effort) "high"))
    (funcall (plist-get request-args :on-success) '())
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
  "Model changes should persist a compatible reasoning effort with app-server."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         notifications
         request-args
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
    (map-put! client :thread-id "thread-1")
    (agent-shell-codex-app-server-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq request-args args))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/set_model")
                  (:params . ((sessionId . "thread-1")
                              (modelId . "gpt-5.4-mini"))))
       :on-success (lambda (result)
                     (setq response result))))
    (should (equal (plist-get request-args :method) "thread/settings/update"))
    (should (equal (plist-get request-args :params)
                   '((threadId . "thread-1")
                     (model . "gpt-5.4-mini")
                     (effort . "low"))))
    (should (equal (map-elt client :current-model-id) "gpt-5.4"))
    (should (equal (map-elt client :reasoning-effort) "high"))
    (funcall (plist-get request-args :on-success) '())
    (should (equal (map-elt response 'modelId) "gpt-5.4-mini"))
    (should (equal (map-elt client :current-model-id) "gpt-5.4-mini"))
    (should (equal (map-elt client :reasoning-effort) "low"))
    (should (equal (map-nested-elt (car notifications)
                                   '(params update currentModeId))
                   "reasoning:low"))))

(ert-deftest agent-shell-codex-app-server-thread-settings-update-syncs-client ()
  "Server thread settings should authoritatively update the adapter state."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         notification)
    (map-put! client :thread-id "thread-1")
    (map-put! client :current-model-id "gpt-5.4")
    (map-put! client :reasoning-effort "medium")
    (agent-shell-codex-app-server-subscribe-to-notifications
     :client client
     :on-notification (lambda (update)
                        (setq notification update)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "thread/settings/updated")
       (params . ((threadId . "thread-1")
                  (threadSettings . ((model . "gpt-5.4-mini")
                                     (effort . "low")))))))
    (should (equal (map-elt client :current-model-id) "gpt-5.4-mini"))
    (should (equal (map-elt client :reasoning-effort) "low"))
    (should (equal (map-nested-elt notification
                                   '(params update currentModeId))
                   "reasoning:low"))))

(ert-deftest agent-shell-codex-app-server-ignores-other-thread-settings ()
  "Settings notifications for another loaded thread should be ignored."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (map-put! client :thread-id "thread-1")
    (map-put! client :current-model-id "gpt-5.4")
    (map-put! client :reasoning-effort "medium")
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "thread/settings/updated")
       (params . ((threadId . "thread-old")
                  (threadSettings . ((model . "gpt-5.4-mini")
                                     (effort . "low")))))))
    (should (equal (map-elt client :current-model-id) "gpt-5.4"))
    (should (equal (map-elt client :reasoning-effort) "medium"))))

(ert-deftest agent-shell-codex-app-server-records-mcp-server-startup-status ()
  "MCP startup notifications should retain the latest server status."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (map-put! client :thread-id "thread-1")
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "mcpServer/startupStatus/updated")
       (params . ((threadId . "thread-1")
                  (name . "playwright")
                  (status . "starting")))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "mcpServer/startupStatus/updated")
       (params . ((threadId . "thread-1")
                  (name . "playwright")
                  (status . "ready")))))
    (should (equal (map-elt (map-elt client :mcp-server-statuses) "playwright")
                   '((:status . "ready")
                     (:error)
                     (:failure-reason))))))

(ert-deftest agent-shell-codex-app-server-reports-mcp-server-startup-failures ()
  "Failed MCP startup notifications should appear in the notices UI."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (map-put! client :thread-id "thread-1")
    (agent-shell-codex-app-server-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (setq delivered error)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "mcpServer/startupStatus/updated")
       (params . ((threadId . "thread-1")
                  (name . "playwright")
                  (status . "failed")
                  (error . "Could not find Chromium")
                  (failureReason . "startupTimeout")))))
    (should (equal (map-elt delivered 'message)
                   "MCP server playwright failed to start: Could not find Chromium"))
    (should (equal (map-elt (map-elt client :mcp-server-statuses) "playwright")
                   '((:status . "failed")
                     (:error . "Could not find Chromium")
                     (:failure-reason . "startupTimeout"))))))

(ert-deftest agent-shell-codex-app-server-ignores-other-thread-mcp-server-status ()
  "MCP startup state for another loaded thread should be ignored."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        delivered)
    (map-put! client :thread-id "thread-1")
    (agent-shell-codex-app-server-subscribe-to-errors
     :client client
     :on-error (lambda (error)
                 (setq delivered error)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "mcpServer/startupStatus/updated")
       (params . ((threadId . "thread-old")
                  (name . "playwright")
                  (status . "failed")
                  (error . "Should be ignored")))))
    (should-not (map-elt client :mcp-server-statuses))
    (should-not delivered)))

(ert-deftest agent-shell-codex-app-server-rejected-model-update-keeps-settings ()
  "A rejected settings request should leave the adapter state unchanged."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         failure-callback
         delivered-error)
    (map-put! client :thread-id "thread-1")
    (map-put! client :current-model-id "gpt-5.4")
    (map-put! client :reasoning-effort "high")
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq failure-callback (plist-get args :on-failure)))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/set_model")
                  (:params . ((sessionId . "thread-1")
                              (modelId . "gpt-5.4-mini"))))
       :on-failure (lambda (error _raw)
                     (setq delivered-error error))))
    (funcall failure-callback '((message . "Rejected")) nil)
    (should delivered-error)
    (should (equal (map-elt client :current-model-id) "gpt-5.4"))
    (should (equal (map-elt client :reasoning-effort) "high"))))

(ert-deftest agent-shell-codex-app-server-model-update-keeps-active-turn ()
  "Changing next-turn settings should not mutate active-turn tracking."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         request-args)
    (map-put! client :thread-id "thread-1")
    (map-put! client :active-turn-id "turn-1")
    (map-put! client :current-model-id "gpt-5.4")
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (setq request-args args))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/set_model")
                  (:params . ((sessionId . "thread-1")
                              (modelId . "gpt-5.4-mini"))))))
    (should (equal (plist-get request-args :method) "thread/settings/update"))
    (should (equal (map-elt client :active-turn-id) "turn-1"))))

(ert-deftest agent-shell-codex-app-server-collects-structured-user-input ()
  "User-input requests should return option and free-form answers."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         prompts
         response-id
         response-result
         (request
          (list (cons 'method "item/tool/requestUserInput")
                (cons 'id 91)
                (cons 'params
                      (list (cons 'threadId "thr-1")
                            (cons 'turnId "turn-1")
                            (cons 'itemId "call-1")
                            (cons 'questions
                                  (list
                                   (list (cons 'id "target")
                                         (cons 'question "Which target?")
                                         (cons 'header "Target")
                                         (cons 'isOther t)
                                         (cons 'options
                                               '(((label . "Core")
                                                  (description . "Inspect core"))
                                                 ((label . "TUI")
                                                  (description . "Inspect TUI")))))
                                   (list (cons 'id "details")
                                         (cons 'question "Anything else?")
                                         (cons 'header "Details")
                                         (cons 'isOther t)
                                         (cons 'options '())))))))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (push (car args) prompts)
                 "TUI"))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _args)
                 (push prompt prompts)
                 "include snapshots"))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq response-id (plist-get args :request-id)
                       response-result (plist-get args :result)))))
      (agent-shell-codex-app-server--route-message
       client
       request))
    (should (= response-id 91))
    (should (equal (map-nested-elt response-result '(answers "target" answers))
                   '("TUI")))
    (should (equal (map-nested-elt response-result '(answers "details" answers))
                   '("user_note: include snapshots")))
    (should (equal (nreverse prompts)
                   '("Target: Which target? " "Details: Anything else? ")))))

(ert-deftest agent-shell-codex-app-server-user-input-supports-other-and-secret ()
  "User-input prompts should support free-form and masked answers."
  (let ((params '((questions . (((id . "target")
                                 (header . "Target")
                                 (question . "Which target?")
                                 (isOther . t)
                                 (options . (((label . "Core")
                                              (description . "Inspect core")))))
                                ((id . "token")
                                 (header . "Token")
                                 (question . "API token?")
                                 (isSecret . t))))))
        (answers '("Other (free-form)" "custom target" "token-value"))
        read-passwd-called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (pop answers)))
              ((symbol-function 'read-string)
               (lambda (&rest _args)
                 (pop answers)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _args)
                 (setq read-passwd-called t)
                 (pop answers))))
      (let ((result
             (agent-shell-codex-app-server--collect-user-input-answers params)))
        (should (equal (map-nested-elt result '(answers "target" answers))
                       '("user_note: custom target")))
        (should (equal (map-nested-elt result '(answers "token" answers))
                       '("user_note: token-value")))
        (should read-passwd-called)))))

(ert-deftest agent-shell-codex-app-server-user-input-quit-interrupts-turn ()
  "Quitting a user-input prompt should interrupt the active turn."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        interrupted
        responded)
    (cl-letf (((symbol-function
                'agent-shell-codex-app-server--collect-user-input-answers)
               (lambda (_params)
                 (signal 'quit nil)))
              ((symbol-function 'agent-shell-codex-app-server-interrupt)
               (lambda (_client)
                 (setq interrupted t)))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest _args)
                 (setq responded t))))
      (agent-shell-codex-app-server--handle-user-input-request
       client
       '((id . 92)
         (params . ((questions . nil))))))
    (should interrupted)
    (should-not responded)))

(ert-deftest agent-shell-codex-app-server-user-input-auto-resolves-empty ()
  "Timed-out user-input requests should return an empty answers object."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        response-result)
    (cl-letf (((symbol-function
                'agent-shell-codex-app-server--collect-user-input-answers)
               (lambda (_params)
                 agent-shell-codex-app-server--auto-resolved))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq response-result (plist-get args :result)))))
      (agent-shell-codex-app-server--handle-user-input-request
       client
       '((id . 93)
         (params . ((autoResolutionMs . 60000)
                    (questions . nil))))))
    (should (hash-table-p (map-elt response-result 'answers)))
    (should (zerop (hash-table-count (map-elt response-result 'answers))))))

(ert-deftest agent-shell-codex-app-server-blocking-user-input-ignores-timeout ()
  "Only explicitly non-blocking user input should auto-resolve."
  (should-not
   (agent-shell-codex-app-server--user-input-timeout-ms
    '((isBlocking . t)
      (autoResolutionMs . 60000))))
  (should
   (= (agent-shell-codex-app-server--user-input-timeout-ms
       '((isBlocking . nil)
         (autoResolutionMs . 60000)))
      60000))
  (should
   (= (agent-shell-codex-app-server--user-input-timeout-ms
       '((isBlocking . nil)))
      agent-shell-codex-app-server--user-input-timeout))
  (should-not
   (agent-shell-codex-app-server--user-input-timeout-ms
    '((autoResolutionMs . 60000)))))

(ert-deftest agent-shell-codex-app-server-reports-cache-write-tokens ()
  "Prompt responses should include current cache-write token usage."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (map-put! client :latest-token-usage
              '((total . ((totalTokens . 20)
                          (inputTokens . 10)
                          (outputTokens . 5)
                          (reasoningOutputTokens . 2)
                          (cachedInputTokens . 4)
                          (cacheWriteInputTokens . 3)))))
    (should (= (map-nested-elt
                (agent-shell-codex-app-server--prompt-response
                 client '((status . "completed")))
                '(usage cachedWriteTokens))
               3))))

(ert-deftest agent-shell-codex-app-server-answers-current-time-requests ()
  "Current-time requests should receive whole Unix seconds."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        response-id
        response-result)
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 1234.75))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-response)
               (lambda (&rest args)
                 (setq response-id (plist-get args :request-id)
                       response-result (plist-get args :result)))))
      (agent-shell-codex-app-server--route-message
       client
       '((method . "currentTime/read")
         (id . 92)
         (params . ((threadId . "thread-1"))))))
    (should (= response-id 92))
    (should (= (map-elt response-result 'currentTimeAt) 1234))))

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

(ert-deftest agent-shell-codex-app-server-turn-completion-clears-tool-state ()
  "Turn completion should release provider-side tool translation state."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (puthash "tool-1" '((:status . "in_progress"))
             (map-elt client :tool-items))
    (puthash "tool-1" "complete output"
             (map-elt client :tool-outputs))
    (puthash "tool-2" '("partial output")
             (map-elt client :tool-output-chunks))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-1")
                           (status . "completed")))))))
    (should (zerop (hash-table-count (map-elt client :tool-items))))
    (should (zerop (hash-table-count (map-elt client :tool-outputs))))
    (should (zerop (hash-table-count (map-elt client :tool-output-chunks))))))

(ert-deftest agent-shell-codex-app-server-interrupt-resolves-pending-prompt ()
  "Interrupt should finish the current prompt locally and interrupt remotely."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         interrupted-turn-id
         prompt-result
         cancelled-timer)
    (map-put! client :thread-id "thread-1")
    (map-put! client :process t)
    (map-put! client :tool-output-flush-timer 'fake-tool-output-timer)
    (puthash "tool-1" '((:status . "in_progress"))
             (map-elt client :tool-items))
    (puthash "tool-1" "partial output"
             (map-elt client :tool-outputs))
    (puthash "tool-1" t (map-elt client :pending-tool-output-items))
    (puthash "tool-1" '("partial output") (map-elt client :tool-output-chunks))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled-timer timer)))
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
    (should-not (map-elt client :pending-prompt))
    (should (equal cancelled-timer 'fake-tool-output-timer))
    (should-not (map-elt client :tool-output-flush-timer))
    (should (zerop (hash-table-count (map-elt client :tool-items))))
    (should (zerop (hash-table-count (map-elt client :tool-outputs))))
    (should (zerop (hash-table-count (map-elt client :pending-tool-output-items))))
    (should (zerop (hash-table-count (map-elt client :tool-output-chunks))))))

(ert-deftest agent-shell-codex-app-server-ignores-dismissed-turn-updates ()
  "Late updates from a dismissed turn should not reach the ACP renderer."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "npm run lint")
                 (cwd . "/tmp")))
         notifications)
    (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
    (agent-shell-codex-app-server--dismiss-turn client "turn-1")
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-1")
                           (status . "completed")))))))
    (should (member "turn-1" (map-elt client :dismissed-turn-ids)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/commandExecution/outputDelta")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (itemId . "tool-1")
                  (delta . "lint output")))))
    (agent-shell-codex-app-server--handle-notification
     client
     `((method . "item/completed")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (item . ,item)))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/agentMessage/delta")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (itemId . "msg-1")
                  (delta . "The")))))
    (should-not notifications)
    (should-not (gethash "tool-1" (map-elt client :tool-output-chunks)))
    (should (member "turn-1" (map-elt client :dismissed-turn-ids)))))

(ert-deftest agent-shell-codex-app-server-ignores-completed-turn-output ()
  "Late command output after turn completion should not reach the ACP renderer."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (item '((id . "tool-1")
                 (type . "commandExecution")
                 (command . "pnpm dev --host 127.0.0.1")
                 (cwd . "/tmp")))
         notifications
         prompt-result)
    (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
    (map-put! client :active-turn-id "turn-1")
    (map-put! client :pending-prompt
              `((:turn-id . "turn-1")
                (:buffer . ,(current-buffer))
                (:on-success . ,(lambda (result)
                                  (setq prompt-result result)))))
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (notification)
                        (push notification notifications)))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "turn/completed")
       (params . ((turn . ((id . "turn-1")
                           (status . "completed")))))))
    (agent-shell-codex-app-server--handle-notification
     client
     '((method . "item/commandExecution/outputDelta")
       (params . ((threadId . "thread-1")
                  (turnId . "turn-1")
                  (itemId . "tool-1")
                  (delta . "vite output")))))
    (should (equal (map-elt prompt-result 'stopReason) "end_turn"))
    (should-not notifications)
    (should-not (gethash "tool-1" (map-elt client :tool-output-chunks)))
    (should (member "turn-1" (map-elt client :dismissed-turn-ids)))))

(ert-deftest agent-shell-codex-app-server-interrupt-cancels-pending-turn-start ()
  "Interrupt should reject an in-flight turn/start and cancel the next turn if needed."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         failure-message
         interrupted-turn-ids)
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
                 (push (map-elt (plist-get args :params) 'turnId)
                       interrupted-turn-ids)
                 (should (equal (plist-get args :method) "turn/interrupt")))))
      (agent-shell-codex-app-server-interrupt client)
      (should (equal failure-message "Task cancelled"))
      (should (map-elt client :interrupt-next-turn))
      (agent-shell-codex-app-server--handle-notification
       client
       '((method . "turn/started")
         (params . ((turn . ((id . "turn-late")))))))
      ;; The turn/start reply still arrives and must not interrupt twice.
      (agent-shell-codex-app-server--handle-response
       client
       '((id . 7)
         (result . ((turn . ((id . "turn-late"))))))))
    (should (equal interrupted-turn-ids '("turn-late")))
    (should (agent-shell-codex-app-server--dismissed-turn-id-p client "turn-late"))
    (should-not (map-elt client :interrupt-next-turn))
    (should-not (map-elt client :active-turn-id))
    (should-not (map-elt client :pending-prompt))))

(ert-deftest agent-shell-codex-app-server-early-turn-start-reply-clears-active-turn ()
  "A turn/start reply beating turn/started should not strand the active turn."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh"))
        interrupted-turn-ids)
    (puthash 7 `((:method . "turn/start")
                 (:buffer . ,(current-buffer))
                 (:on-failure . ,(lambda (_error _raw) nil)))
             (map-elt client :pending-requests))
    (map-put! client :thread-id "thread-1")
    (map-put! client :process t)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args)
                 (push (map-elt (plist-get args :params) 'turnId)
                       interrupted-turn-ids))))
      (agent-shell-codex-app-server-interrupt client)
      ;; Reply first, notification second.
      (agent-shell-codex-app-server--handle-response
       client
       '((id . 7)
         (result . ((turn . ((id . "turn-late")))))))
      (agent-shell-codex-app-server--handle-notification
       client
       '((method . "turn/started")
         (params . ((turn . ((id . "turn-late"))))))))
    (should (equal interrupted-turn-ids '("turn-late")))
    (should-not (map-elt client :interrupt-next-turn))
    (should-not (map-elt client :active-turn-id))))

(ert-deftest agent-shell-codex-app-server-failed-turn-start-disarms-interrupt ()
  "A turn/start that fails after interrupt must not arm the next turn.

Leaving `:interrupt-next-turn' set would silently interrupt and dismiss
the next turn the user starts, whose turn/completed is then ignored and
the prompt never resolves."
  (let ((client (agent-shell-codex-app-server-make-client :command "sh")))
    (puthash 7 `((:method . "turn/start")
                 (:buffer . ,(current-buffer))
                 (:on-failure . ,(lambda (_error _raw) nil)))
             (map-elt client :pending-requests))
    (map-put! client :thread-id "thread-1")
    (map-put! client :process t)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest _args)
                 (error "No turn should be interrupted"))))
      (agent-shell-codex-app-server-interrupt client)
      (should (map-elt client :interrupt-next-turn))
      (agent-shell-codex-app-server--handle-response
       client
       '((id . 7)
         (error . ((message . "thread is busy")))))
      (should-not (map-elt client :interrupt-next-turn))
      ;; A later, legitimate turn now runs untouched.
      (agent-shell-codex-app-server--handle-notification
       client
       '((method . "turn/started")
         (params . ((turn . ((id . "turn-next"))))))))
    (should (equal (map-elt client :active-turn-id) "turn-next"))
    (should-not (agent-shell-codex-app-server--dismissed-turn-id-p
                 client "turn-next"))))

(ert-deftest agent-shell-codex-app-server-dismissed-turn-start-resolves-prompt ()
  "A turn/start reply for an already dismissed turn should resolve the prompt.

Otherwise no turn/completed remains to answer it and the shell stays
busy forever."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         prompt-result
         start-request)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
               (lambda (&rest args) (setq start-request args))))
      (agent-shell-codex-app-server-send-request
       :client client
       :request '((:method . "session/prompt")
                  (:params . ((sessionId . "thread-1")
                              (prompt . (((type . "text") (text . "hi")))))))
       :buffer (current-buffer)
       :on-success (lambda (result) (setq prompt-result result))))
    (should (equal (plist-get start-request :method) "turn/start"))
    (agent-shell-codex-app-server--dismiss-turn client "turn-1")
    (funcall (plist-get start-request :on-success)
             '((turn . ((id . "turn-1")))))
    (should (equal (map-elt prompt-result 'stopReason) "cancelled"))
    (should-not (map-elt client :pending-prompt))
    (should-not (map-elt client :active-turn-id))))

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

(ert-deftest agent-shell-codex-app-server-queue-recovers-from-cancelled-drain-timer ()
  "A cancelled drain timer should not prevent later messages from being handled."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (real-run-at-time (symbol-function 'run-at-time))
         timers
         (scheduled 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (setq scheduled (1+ scheduled))
                 (let ((timer (funcall real-run-at-time 60 nil #'ignore)))
                   (push timer timers)
                   timer))))
      (unwind-protect
          (progn
            (agent-shell-codex-app-server--schedule-message-drain client)
            (should (= scheduled 1))
            (agent-shell-codex-app-server--schedule-message-drain client)
            (should (= scheduled 1))
            (cancel-timer (map-elt client :message-drain-timer))
            (agent-shell-codex-app-server--schedule-message-drain client)
            (should (= scheduled 2))
            (should (memq (map-elt client :message-drain-timer) timer-list)))
        (mapc #'cancel-timer timers)))))

(ert-deftest agent-shell-codex-app-server-queue-reschedules-after-quit ()
  "Quitting a handler should leave the remaining messages scheduled."
  (let* ((client (agent-shell-codex-app-server-make-client :command "sh"))
         (real-run-at-time (symbol-function 'run-at-time))
         timers)
    (map-put! client :message-queue '(quit later))
    (cl-letf (((symbol-function 'agent-shell-codex-app-server--route-message)
               (lambda (_client message)
                 (when (eq message 'quit)
                   (signal 'quit nil))))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 (let ((timer (funcall real-run-at-time 60 nil #'ignore)))
                   (push timer timers)
                   timer))))
      (unwind-protect
          (progn
            (condition-case nil
                (agent-shell-codex-app-server--drain-message-queue client)
              (quit nil))
            (should-not (map-elt client :message-queue-busy))
            (should (equal (map-elt client :message-queue) '(later)))
            (should (memq (map-elt client :message-drain-timer) timer-list)))
        (mapc #'cancel-timer timers)))))

(provide 'agent-shell-codex-app-server-tests)
;;; agent-shell-codex-app-server-tests.el ends here
