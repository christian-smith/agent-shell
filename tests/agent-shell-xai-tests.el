;;; agent-shell-xai-tests.el --- Tests for agent-shell-xai -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-xai)

;;; Code:

(ert-deftest agent-shell-xai-session-meta-follows-disabled-global-fs-test ()
  "Test `agent-shell-xai--session-meta' disables ACP filesystem access."
  (let ((agent-shell-xai-yolo-mode nil)
        (agent-shell-xai-auto-mode nil)
        (agent-shell-text-file-capabilities nil)
        (agent-shell-xai-default-model-id nil))
    (should (equal (agent-shell-xai--session-meta)
                   '((clientFsRead . :false)
                     (clientFsWrite . :false))))))

(ert-deftest agent-shell-xai-session-meta-can-enable-fs-test ()
  "Test Grok session metadata can enable ACP filesystem access."
  (let ((agent-shell-xai-yolo-mode nil)
        (agent-shell-xai-auto-mode nil)
        (agent-shell-text-file-capabilities t)
        (agent-shell-xai-default-model-id nil))
    (should (equal (agent-shell-xai--session-meta)
                   '((clientFsRead . t)
                     (clientFsWrite . t))))))

(ert-deftest agent-shell-xai-session-meta-yolo-and-model-test ()
  "Test `agent-shell-xai--session-meta' includes yoloMode and modelId."
  (let ((agent-shell-xai-yolo-mode t)
        (agent-shell-xai-auto-mode t)
        (agent-shell-text-file-capabilities nil)
        (agent-shell-xai-default-model-id "grok-build"))
    (should (equal (agent-shell-xai--session-meta)
                   '((clientFsRead . :false)
                     (clientFsWrite . :false)
                     (yoloMode . t)
                     (modelId . "grok-build"))))))

(ert-deftest agent-shell-xai-session-meta-auto-mode-test ()
  "Test `agent-shell-xai--session-meta' sends autoMode when yolo is off."
  (let ((agent-shell-xai-yolo-mode nil)
        (agent-shell-xai-auto-mode t)
        (agent-shell-text-file-capabilities nil)
        (agent-shell-xai-default-model-id nil))
    (should (equal (agent-shell-xai--session-meta)
                   '((clientFsRead . :false)
                     (clientFsWrite . :false)
                     (autoMode . t))))))

(ert-deftest agent-shell-xai-session-meta-model-function-test ()
  "Test `agent-shell-xai--session-meta' calls a function default model."
  (let ((agent-shell-xai-yolo-mode nil)
        (agent-shell-xai-auto-mode nil)
        (agent-shell-text-file-capabilities nil)
        (agent-shell-xai-default-model-id (lambda () "dynamic-model")))
    (should (equal (agent-shell-xai--session-meta)
                   '((clientFsRead . :false)
                     (clientFsWrite . :false)
                     (modelId . "dynamic-model"))))))

(ert-deftest agent-shell-xai-config-wires-hooks-test ()
  "Test Grok config installs session meta and adapters."
  (let ((agent-shell-xai-yolo-mode t)
        (agent-shell-xai-auto-mode nil)
        (agent-shell-text-file-capabilities nil)
        (agent-shell-xai-default-model-id "grok-build"))
    (let ((config (agent-shell-xai-make-grok-config)))
      (should (equal (map-elt config :session-meta)
                     '((clientFsRead . :false)
                       (clientFsWrite . :false)
                       (yoloMode . t)
                       (modelId . "grok-build"))))
      (should (functionp (map-elt config :notification-adapter)))
      (should (functionp (map-elt config :outgoing-request-decorator)))
      (should (functionp (map-elt config :session-response-adapter)))
      (should (functionp (map-elt config :request-handler)))
      (should (functionp (map-elt config :prompt-response-adapter)))
      (should (functionp (map-elt config :busy-prompt-handler)))
      (should (equal (map-nested-elt (acp-make-session-new-request
                                      :cwd "/tmp"
                                      :meta (map-elt config :session-meta))
                                     '(:params _meta))
                     (map-elt config :session-meta))))))

(ert-deftest agent-shell-xai-busy-prompt-interjects-active-turn-test ()
  "Busy Grok input should use the `x.ai/interject' extension."
  (let* ((client 'test-client)
         (state `((:buffer . ,(current-buffer))
                  (:client . ,client)
                  (:session . ((:id . "session-1")))
                  (:active-requests . (((:method . "session/prompt"))))))
         request-args
         fallback-called)
    (cl-letf (((symbol-function 'agent-shell--build-content-blocks)
               (lambda (_prompt)
                 '(((type . "text") (text . "look here")))))
              ((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq request-args args))))
      (should
       (agent-shell-xai--handle-busy-prompt
        `((:state . ,state)
          (:prompt . "look here")
          (:fallback . ,(lambda ()
                          (setq fallback-called t))))))
      (should (eq (plist-get request-args :client) client))
      (should (equal (map-elt (plist-get request-args :request) :method)
                     "_x.ai/interject"))
      (should (equal (map-nested-elt (plist-get request-args :request)
                                     '(:params sessionId))
                     "session-1"))
      (should (equal (map-nested-elt (plist-get request-args :request)
                                     '(:params text))
                     "look here"))
      (should (string-prefix-p
               "agent-shell-"
               (map-nested-elt (plist-get request-args :request)
                               '(:params interjectionId))))
      (should-not (map-nested-elt (plist-get request-args :request)
                                  '(:params content)))
      (funcall (plist-get request-args :on-success)
               '((result . ((status . "queued")))))
      (should-not fallback-called))))

(ert-deftest agent-shell-xai-busy-prompt-queues-unacknowledged-interjection-test ()
  "An unacknowledged Grok interjection should fall back to the next turn."
  (let* ((state `((:buffer . ,(current-buffer))
                  (:client . test-client)
                  (:session . ((:id . "session-1")))
                  (:active-requests . (((:method . "session/prompt"))))))
         request-args
         fallback-called)
    (cl-letf (((symbol-function 'agent-shell--build-content-blocks)
               (lambda (_prompt) nil))
              ((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq request-args args))))
      (should
       (agent-shell-xai--handle-busy-prompt
        `((:state . ,state)
          (:prompt . "do this next")
          (:fallback . ,(lambda ()
                          (setq fallback-called t))))))
      (funcall (plist-get request-args :on-success) '((status . "ignored")))
      (should fallback-called))))

(ert-deftest agent-shell-xai-busy-prompt-requires-text-test ()
  "A non-text busy prompt must remain available to the normal queue."
  (let ((state '((:client . test-client)
                 (:session . ((:id . "session-1")))
                 (:active-requests . (((:method . "session/prompt")))))))
    (should-not
     (agent-shell-xai--handle-busy-prompt
      `((:state . ,state)
        (:prompt . (:not "text"))
        (:fallback . ,#'ignore))))))

(ert-deftest agent-shell-xai-busy-prompt-requires-active-turn-test ()
  "Grok interjection should not claim input without an active prompt."
  (let ((state '((:client . test-client)
                 (:session . ((:id . "session-1")))
                 (:active-requests . nil))))
    (should-not
     (agent-shell-xai--handle-busy-prompt
      `((:state . ,state)
        (:prompt . "next turn")
        (:fallback . ,#'ignore))))))

(ert-deftest agent-shell-xai-notification-adapter-hides-synthetic-user-echo-test ()
  "Test Grok user echoes marked hidden do not reach scrollback."
  (should-not
   (agent-shell-xai--notification-adapter
    :acp-notification
    '((method . "session/update")
      (params . ((update . ((sessionUpdate . "user_message_chunk")
                            (content . ((type . "text")
                                        (text . "<system-reminder>done</system-reminder>")))
                            (_meta . ((hideFromScrollback . t)))))))))))

(ert-deftest agent-shell-xai-notification-adapter-keeps-visible-updates-test ()
  "Test Grok updates without a true hide marker remain visible."
  (dolist (notification
           '(((method . "session/update")
              (params . ((update . ((sessionUpdate . "user_message_chunk")
                                    (_meta . ((hideFromScrollback . :false))))))))
             ((method . "session/update")
              (params . ((update . ((sessionUpdate . "agent_message_chunk")
                                    (_meta . ((hideFromScrollback . t))))))))))
    (should (eq (agent-shell-xai--notification-adapter
                 :acp-notification notification)
                notification))))

(ert-deftest agent-shell-xai-decorate-initialize-adds-client-identifier-test ()
  "Test initialize requests get `_meta.clientIdentifier'."
  (let ((decorated (agent-shell-xai--outgoing-request-decorator
                    '((:method . "initialize")
                      (:params . ((protocolVersion . 1)))))))
    (should (equal (map-nested-elt decorated '(:params _meta clientIdentifier))
                   "agent-shell"))
    (should (equal (map-nested-elt decorated '(:params protocolVersion)) 1))))

(ert-deftest agent-shell-xai-decorate-initialize-keeps-existing-meta-test ()
  "Test initialize decorator preserves existing `_meta' keys."
  (let ((decorated (agent-shell-xai--outgoing-request-decorator
                    '((:method . "initialize")
                      (:params . ((protocolVersion . 1)
                                  (_meta . ((foo . "bar")))))))))
    (should (equal (map-nested-elt decorated '(:params _meta clientIdentifier))
                   "agent-shell"))
    (should (equal (map-nested-elt decorated '(:params _meta foo)) "bar"))))

(ert-deftest agent-shell-xai-decorate-initialize-disables-fs-capabilities-test ()
  "Test initialize does not advertise Emacs file-buffer capabilities."
  (let* ((agent-shell-text-file-capabilities nil)
         (request '((:method . "initialize")
                    (:params . ((protocolVersion . 1)
                                (clientCapabilities
                                 (fs (readTextFile . t)
                                     (writeTextFile . t)))))))
         (decorated (agent-shell-xai--outgoing-request-decorator request)))
      (should (eq (map-nested-elt decorated
                                  '(:params clientCapabilities fs readTextFile))
                  :false))
      (should (eq (map-nested-elt decorated
                                  '(:params clientCapabilities fs writeTextFile))
                  :false))
      (should (eq (map-nested-elt request
                                  '(:params clientCapabilities fs readTextFile))
                  t))
      (should-not agent-shell-text-file-capabilities)))

(ert-deftest agent-shell-xai-decorate-initialize-can-enable-fs-capabilities-test ()
  "Test initialize can opt back into Emacs file-buffer capabilities."
  (let ((agent-shell-text-file-capabilities t))
    (let ((decorated (agent-shell-xai--outgoing-request-decorator
                      '((:method . "initialize")
                        (:params . ((protocolVersion . 1)
                                    (clientCapabilities
                                     (fs (readTextFile . :false)
                                         (writeTextFile . :false)))))))))
      (should (eq (map-nested-elt decorated
                                  '(:params clientCapabilities fs readTextFile))
                  t))
      (should (eq (map-nested-elt decorated
                                  '(:params clientCapabilities fs writeTextFile))
                  t)))))

(ert-deftest agent-shell-xai-session-modes-test ()
  "Test Grok session modes are default, plan, and ask."
  (let ((modes (agent-shell-xai--session-modes)))
    (should (equal (mapcar (lambda (mode) (map-elt mode 'id)) modes)
                   '("default" "plan" "ask")))))

(ert-deftest agent-shell-xai-adapt-session-response-seeds-modes-test ()
  "Test session response adapter seeds modes when the agent omitted them."
  (let ((adapted (agent-shell-xai--adapt-session-response
                  :acp-response '((sessionId . "s1")
                                  (models (currentModelId . "grok-build"))))))
    (should (equal (map-nested-elt adapted '(modes currentModeId)) "default"))
    (should (equal (mapcar (lambda (mode) (map-elt mode 'id))
                           (map-nested-elt adapted '(modes availableModes)))
                   '("default" "plan" "ask")))))

(ert-deftest agent-shell-xai-adapt-session-response-keeps-existing-modes-test ()
  "Test session response adapter does not overwrite advertised modes."
  (let ((adapted (agent-shell-xai--adapt-session-response
                  :acp-response '((modes (currentModeId . "plan")
                                         (availableModes . (((id . "plan")
                                                             (name . "Plan")))))))))
    (should (equal (map-nested-elt adapted '(modes currentModeId)) "plan"))
    (should (equal (mapcar (lambda (mode) (map-elt mode 'id))
                           (map-nested-elt adapted '(modes availableModes)))
                   '("plan")))))

(ert-deftest agent-shell-xai-native-effort-is-preserved-test ()
  "Native effort configuration takes precedence over synthetic metadata."
  (let* ((option '((id . "reasoning_effort") (category . "thought_level")
                   (type . "select") (currentValue . "high")
                   (options . (((value . "high") (name . "High"))))))
         (response `((configOptions . (,option))
                     (_meta (x.ai/sessionConfig
                             (options . (((id . "low") (category . "mode")
                                          (label . "Low") (selected . t))))))))
         (request '((:method . "session/set_config_option")
                    (:params . ((sessionId . "s1") (configId . "reasoning_effort")
                                (value . "high"))))))
    (should (equal (map-elt (agent-shell-xai--adapt-session-response :acp-response response)
                           'configOptions)
                   (list option)))
    (should (equal (agent-shell-xai--outgoing-request-decorator request) request))))

(ert-deftest agent-shell-xai-usage-from-prompt-response-test ()
  "Test PromptResponse `_meta' maps onto ACP usage fields."
  (should (equal (agent-shell-xai--usage-from-prompt-response
                  '((stopReason . "end_turn")
                    (_meta (totalTokens . 1200)
                           (inputTokens . 900)
                           (outputTokens . 200)
                           (reasoningTokens . 100)
                           (cachedReadTokens . 50))))
                 '((totalTokens . 1200)
                   (inputTokens . 900)
                   (outputTokens . 200)
                   (thoughtTokens . 100)
                   (cachedReadTokens . 50))))
  (should-not (agent-shell-xai--usage-from-prompt-response
               '((stopReason . "end_turn")))))

(ert-deftest agent-shell-xai-usage-prefers-whole-prompt-aggregate-test ()
  "Test Grok's `_meta.usage' aggregate wins over last-call counters."
  (should
   (equal
    (agent-shell-xai--usage-from-prompt-response
     '((_meta (totalTokens . 10)
              (inputTokens . 8)
              (usage (totalTokens . 100)
                     (inputTokens . 80)
                     (outputTokens . 20)
                     (reasoningTokens . 7)
                     (cachedReadTokens . 40)
                     (cacheCreationTokens . 3)))))
    '((totalTokens . 100)
      (inputTokens . 80)
      (outputTokens . 20)
      (thoughtTokens . 7)
      (cachedReadTokens . 40)
      (cachedWriteTokens . 3)))))

(ert-deftest agent-shell-xai-ext-method-normalizes-underscore-prefix-test ()
  "Test `_x.ai/...' reverse-request methods unwrap to `x.ai/...'."
  (should (equal (agent-shell-xai--ext-method
                  '((method . "_x.ai/ask_user_question")))
                 "x.ai/ask_user_question"))
  (should (equal (agent-shell-xai--ext-method
                  '((method . "x.ai/exit_plan_mode")))
                 "x.ai/exit_plan_mode")))

(ert-deftest agent-shell-xai-request-params-unwraps-nested-test ()
  "Test nested gateway params unwrap to the inner payload."
  (should (equal (agent-shell-xai--request-params
                  '((method . "_x.ai/ask_user_question")
                    (params (method . "x.ai/ask_user_question")
                            (params (sessionId . "s1")
                                    (toolCallId . "tc-1")))))
                 '((sessionId . "s1")
                   (toolCallId . "tc-1"))))
  (should (equal (agent-shell-xai--request-params
                  '((method . "x.ai/exit_plan_mode")
                    (params (sessionId . "s1")
                            (planContent . "# Plan"))))
                 '((sessionId . "s1")
                   (planContent . "# Plan")))))

(ert-deftest agent-shell-xai-ask-user-question-accepted-result-test ()
  "Test accepted answers serialize in Grok's ext-method shape.

Asserts on JSON rather than the alist because `acp-send-response' hands
the result to `json-serialize', which rejects string alist keys.  Grok
keys answers by question text, so this is the shape that actually has to
survive the wire."
  (should (equal (json-serialize
                  (agent-shell-xai--ask-user-question-result
                   :outcome 'accepted
                   :answers '(("Which database?" . ("Redis")))))
                 "{\"outcome\":\"accepted\",\"answers\":{\"Which database?\":[\"Redis\"]}}"))
  ;; Multi-select answers stay arrays.
  (should (equal (json-serialize
                  (agent-shell-xai--ask-user-question-result
                   :outcome 'accepted
                   :answers '(("Pick some?" . ("A" "B")))))
                 "{\"outcome\":\"accepted\",\"answers\":{\"Pick some?\":[\"A\",\"B\"]}}"))
  ;; `answers' is required by Grok's serde, so it must be {} and never null.
  (should (equal (json-serialize
                  (agent-shell-xai--ask-user-question-result
                   :outcome 'accepted
                   :answers nil))
                 "{\"outcome\":\"accepted\",\"answers\":{}}"))
  (should (equal (json-serialize
                  (agent-shell-xai--ask-user-question-result
                   :outcome 'accepted
                   :answers '(("Which database?" . ("Other")))
                   :annotations '(("Which database?" . ((notes . "SQLite"))))))
                 "{\"outcome\":\"accepted\",\"answers\":{\"Which database?\":[\"Other\"]},\"annotations\":{\"Which database?\":{\"notes\":\"SQLite\"}}}")))

(ert-deftest agent-shell-xai-read-question-supports-other-test ()
  "Test Grok's implicit Other option captures a free-form annotation."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "Other"))
            ((symbol-function 'read-string)
             (lambda (&rest _) "SQLite")))
    (should (equal (agent-shell-xai--read-question
                    '((question . "Which database?")
                      (options . (((label . "Redis"))))))
                   '((:text . "Which database?")
                     (:labels . ("Other"))
                     (:annotation . ((notes . "SQLite"))))))))

(ert-deftest agent-shell-xai-read-question-preserves-preview-test ()
  "Test a single-select answer returns its option preview annotation."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "Redis")))
    (should (equal (agent-shell-xai--read-question
                    '((question . "Which database?")
                      (options . (((label . "Redis")
                                   (preview . "Fast cache"))))))
                   '((:text . "Which database?")
                     (:labels . ("Redis"))
                     (:annotation . ((preview . "Fast cache"))))))))

(ert-deftest agent-shell-xai-ask-user-question-plan-outcomes-test ()
  "Test plan-mode outcomes serialize without answers."
  (should (equal (json-serialize (agent-shell-xai--ask-user-question-result
                                  :outcome 'chat-about-this))
                 "{\"outcome\":\"chat_about_this\",\"partial_answers\":{}}"))
  (should (equal (json-serialize (agent-shell-xai--ask-user-question-result
                                  :outcome 'skip-interview))
                 "{\"outcome\":\"skip_interview\",\"partial_answers\":{}}"))
  (should (equal (json-serialize (agent-shell-xai--ask-user-question-result
                                  :outcome 'cancelled))
                 "{\"outcome\":\"cancelled\"}")))

(ert-deftest agent-shell-xai-exit-plan-mode-result-test ()
  "Test plan-approval outcomes serialize in Grok's ext-method shape."
  (should (equal (json-serialize (agent-shell-xai--exit-plan-mode-result
                                  :outcome "approved"))
                 "{\"outcome\":\"approved\"}"))
  (should (equal (json-serialize (agent-shell-xai--exit-plan-mode-result
                                  :outcome "cancelled"
                                  :feedback "Add tests"))
                 "{\"outcome\":\"cancelled\",\"feedback\":\"Add tests\"}"))
  ;; Grok omits feedback rather than sending null.
  (should (equal (json-serialize (agent-shell-xai--exit-plan-mode-result
                                  :outcome "cancelled"
                                  :feedback ""))
                 "{\"outcome\":\"cancelled\"}")))

(ert-deftest agent-shell-xai-on-request-handles-ask-user-question-test ()
  "Test `agent-shell-xai--on-request' answers ask_user_question."
  (let ((sent nil)
        (state '((:client . test-client)
                 (:buffer . nil))))
    (cl-letf (((symbol-function 'agent-shell-xai--read-ask-user-question)
               (lambda (&rest _)
                 (agent-shell-xai--ask-user-question-result
                  :outcome 'accepted
                  :answers '(("Which database?" . ("Redis"))))))
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (setq sent args))))
      (should (agent-shell-xai--on-request
               :state state
               :acp-request '((id . "req-1")
                              (method . "x.ai/ask_user_question")
                              (params (sessionId . "s1")
                                      (toolCallId . "tc-1")
                                      (questions . (((question . "Which database?")
                                                     (options . (((label . "Redis")))))))
                                      (mode . "default")))))
      (should (equal (plist-get sent :client) 'test-client))
      (should (equal (map-elt (plist-get sent :response) :request-id) "req-1"))
      (should (equal (map-nested-elt (plist-get sent :response)
                                     '(:result outcome))
                     "accepted")))))

(ert-deftest agent-shell-xai-on-request-handles-exit-plan-mode-test ()
  "Test `agent-shell-xai--on-request' answers exit_plan_mode."
  (let ((sent nil)
        (state '((:client . test-client)
                 (:buffer . nil))))
    (cl-letf (((symbol-function 'agent-shell-xai--read-exit-plan-mode)
               (lambda (&rest _)
                 (agent-shell-xai--exit-plan-mode-result :outcome "approved")))
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (setq sent args))))
      (should (agent-shell-xai--on-request
               :state state
               :acp-request '((id . 7)
                              (method . "_x.ai/exit_plan_mode")
                              (params (method . "x.ai/exit_plan_mode")
                                      (params (sessionId . "s1")
                                              (toolCallId . "tc-2")
                                              (planContent . "# Plan"))))))
      (should (equal (map-elt (plist-get sent :response) :request-id) 7))
      (should (equal (map-nested-elt (plist-get sent :response)
                                     '(:result outcome))
                     "approved")))))

(ert-deftest agent-shell-xai-on-request-handles-mcp-elicitation-test ()
  "Test `agent-shell-xai--on-request' answers MCP elicitations."
  (let (sent)
    (cl-letf (((symbol-function 'agent-shell-xai--read-mcp-elicitation)
               (lambda (&rest _)
                 (agent-shell-xai--mcp-elicitation-result
                  :outcome "accept"
                  :content (agent-shell-xai--json-object
                            '(("email" . "user@example.com"))))))
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (setq sent args))))
      (should
       (agent-shell-xai--on-request
        :state '((:client . test-client))
        :acp-request '((id . "req-3")
                       (method . "_x.ai/mcp/elicit")
                       (params (method . "x.ai/mcp/elicit")
                               (params (sessionId . "s1")
                                       (toolCallId . "mcp-elicit-1")
                                       (serverName . "docs")
                                       (mode . "form")))))))
    (should
     (equal
      (json-serialize (map-elt (plist-get sent :response) :result))
      "{\"outcome\":\"accept\",\"content\":{\"email\":\"user@example.com\"}}"))))

(ert-deftest agent-shell-xai-mcp-elicitation-quit-cancels-test ()
  "Test quitting a Grok MCP elicitation releases the blocking request."
  (cl-letf (((symbol-function 'agent-shell-xai--read-mcp-elicitation)
             (lambda (&rest _) (signal 'quit nil))))
    (should
     (equal
      (agent-shell-xai--handle-mcp-elicitation
       :acp-request '((params (mode . "form"))))
      '((outcome . "cancel"))))))

(ert-deftest agent-shell-xai-mcp-form-collects-typed-content-test ()
  "Test Grok MCP forms preserve typed values and omit optional fields."
  (let ((answers '("Ada" "" "No")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _)
                 (prog1 (car answers)
                   (setq answers (cdr answers)))))
              ((symbol-function 'completing-read)
               (lambda (&rest _)
                 (prog1 (car answers)
                   (setq answers (cdr answers))))))
      (should
       (equal
        (json-serialize
         (agent-shell-xai--read-mcp-form
          '((serverName . "docs")
            (requestedSchema
             (type . "object")
             (required . ("name" "enabled"))
             (properties
              (name (type . "string"))
              (note (type . "string"))
              (enabled (type . "boolean")))))))
        "{\"name\":\"Ada\",\"enabled\":false}")))))

(ert-deftest agent-shell-xai-mcp-url-rejects-embedded-credentials-test ()
  "Test Grok URL elicitations reject credential-bearing URLs."
  (let (prompted opened)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (setq prompted t)))
              ((symbol-function 'browse-url)
               (lambda (&rest _)
                 (setq opened t))))
      (should
       (equal
        (agent-shell-xai--read-mcp-elicitation
         :params '((mode . "url")
                   (url . "https://user:secret@example.com/authorize")))
        '((outcome . "decline"))))
      (should-not prompted)
      (should-not opened))))

(ert-deftest agent-shell-xai-on-request-ignores-other-methods-test ()
  "Test `agent-shell-xai--on-request' returns nil for unrelated methods."
  (should-not (agent-shell-xai--on-request
               :state '((:client . test-client))
               :acp-request '((id . "req-1")
                              (method . "session/request_permission")))))

(ert-deftest agent-shell-xai-on-request-answers-serializable-result-test ()
  "Test a handled reverse-request produces a payload `acp.el' can send.

The result travels through `json-serialize', so a shape that only looks
right as an alist would still hang Grok on the wire."
  (let (payload)
    (cl-letf (((symbol-function 'agent-shell-xai--read-ask-user-question)
               (lambda (&rest _)
                 (agent-shell-xai--ask-user-question-result
                  :outcome 'accepted
                  :answers '(("Which database?" . ("Redis"))))))
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (setq payload (map-elt (plist-get args :response) :result)))))
      (should (agent-shell-xai--on-request
               :state '((:client . test-client))
               :acp-request '((id . "req-1")
                              (method . "x.ai/ask_user_question")
                              (params (sessionId . "s1"))))))
    (should (equal (json-serialize payload)
                   "{\"outcome\":\"accepted\",\"answers\":{\"Which database?\":[\"Redis\"]}}"))))

(ert-deftest agent-shell-xai-on-request-quit-cancels-instead-of-hanging-test ()
  "Test `keyboard-quit' at a prompt still answers the reverse-request.

Grok blocks its tool call on the reply, so an unanswered request hangs
the turn forever."
  (let (results)
    (cl-letf (((symbol-function 'agent-shell-xai--read-ask-user-question)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'agent-shell-xai--read-exit-plan-mode)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (push (map-elt (plist-get args :response) :result) results))))
      (should (agent-shell-xai--on-request
               :state '((:client . test-client))
               :acp-request '((id . "req-1")
                              (method . "x.ai/ask_user_question")
                              (params (sessionId . "s1")))))
      (should (agent-shell-xai--on-request
               :state '((:client . test-client))
               :acp-request '((id . "req-2")
                              (method . "x.ai/exit_plan_mode")
                              (params (sessionId . "s1"))))))
    (should (equal (mapcar (lambda (result) (map-elt result 'outcome))
                           (nreverse results))
                   '("cancelled" "abandoned")))))

(ert-deftest agent-shell-xai-on-request-error-sends-jsonrpc-error-test ()
  "Test a failing reverse-request handler answers with a JSON-RPC error."
  (let (response)
    (cl-letf (((symbol-function 'agent-shell-xai--read-exit-plan-mode)
               (lambda (&rest _) (error "Boom")))
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (setq response (plist-get args :response)))))
      (should (agent-shell-xai--on-request
               :state '((:client . test-client))
               :acp-request '((id . 9)
                              (method . "x.ai/exit_plan_mode")
                              (params (sessionId . "s1"))))))
    (should (equal (map-elt response :request-id) 9))
    (should-not (map-elt response :result))
    (should (string-match-p "Boom" (map-nested-elt response '(:error message))))))

(ert-deftest agent-shell-xai-non-thought-level-config-writes-pass-through-test ()
  "Test unrelated config-option writes are left alone by the decorator."
  (let* ((agent-shell--state '((:session (:id . "s1"))))
         (request '((:method . "session/set_config_option")
                    (:params . ((sessionId . "s1")
                                (configId . "mode")
                                (value . "plan"))))))
    (should (equal (agent-shell-xai--outgoing-request-decorator request) request))))

(provide 'agent-shell-xai-tests)
;;; agent-shell-xai-tests.el ends here
