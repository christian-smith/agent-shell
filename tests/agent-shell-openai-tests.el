;;; agent-shell-openai-tests.el --- Tests for agent-shell-openai -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-openai)

;;; Code:

(ert-deftest agent-shell-openai-default-model-id-test ()
  "Test that Codex config exposes default model id."
  (let ((default-model-id-fn
         (map-elt (agent-shell-openai-make-codex-config) :default-model-id)))

    (let ((agent-shell-openai-default-model-id nil))
      (should (null (funcall default-model-id-fn))))

    (let ((agent-shell-openai-default-model-id "gpt-5.4/low"))
      (should (string= (funcall default-model-id-fn) "gpt-5.4/low")))

    (let ((agent-shell-openai-default-model-id (lambda () "gpt-5.4/low")))
      (should (string= (funcall default-model-id-fn) "gpt-5.4/low")))))

(ert-deftest agent-shell-openai-default-session-mode-id-test ()
  "Test that Codex config exposes default session mode id."
  (let ((default-session-mode-id-fn
         (map-elt (agent-shell-openai-make-codex-config) :default-session-mode-id)))

    (let ((agent-shell-openai-default-session-mode-id nil))
      (should (null (funcall default-session-mode-id-fn))))

    (let ((agent-shell-openai-default-session-mode-id "full-access"))
      (should (string= (funcall default-session-mode-id-fn) "full-access")))))

(ert-deftest agent-shell-openai-codex-does-not-eagerly-authenticate-test ()
  "Test that Codex lets codex-acp decide when auth is needed."
  (let ((config (agent-shell-openai-make-codex-config)))
    (should-not (map-elt config :needs-authentication))
    (should-not (map-elt config :authenticate-request-maker))))

(ert-deftest agent-shell-openai-codex-login-default-auth-request-test ()
  "Test that Codex login auth uses the current chat-gpt method id."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :login t))
         (request (json-parse-string (agent-shell-openai--codex-default-auth-request)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "chat-gpt"))))

(ert-deftest agent-shell-openai-codex-api-key-default-auth-request-test ()
  "Test that Codex API key auth sends key metadata."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :api-key "openai-secret"))
         (request (json-parse-string (agent-shell-openai--codex-default-auth-request)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "api-key"))
    (should (string= (map-nested-elt request '(_meta api-key apiKey))
                     "openai-secret"))))

(ert-deftest agent-shell-openai-codex-key-default-auth-request-test ()
  "Test that Codex-specific API key auth sends key metadata."
  (let* ((agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :codex-api-key "codex-secret"))
         (request (json-parse-string (agent-shell-openai--codex-default-auth-request)
                                     :object-type 'alist)))
    (should (string= (map-elt request 'methodId) "api-key"))
    (should (string= (map-nested-elt request '(_meta api-key apiKey))
                     "codex-secret"))))

(ert-deftest agent-shell-openai-codex-acp-client-keeps-default-auth-request-test ()
  "Test that the ACP client retains deferred authentication metadata."
  (let ((agent-shell-openai-authentication
         (agent-shell-openai-make-authentication :login t))
        (agent-shell-openai-codex-transport 'acp)
        client-args)
    (cl-letf (((symbol-function 'agent-shell--make-acp-client)
               (lambda (&rest args)
                 (setq client-args args))))
      (agent-shell-openai-make-codex-client :buffer (current-buffer)))
    (should (member "OPENAI_API_KEY="
                    (plist-get client-args :environment-variables)))
    (should (seq-some
             (lambda (entry)
               (string-prefix-p "DEFAULT_AUTH_REQUEST=" entry))
             (plist-get client-args :environment-variables)))))

(ert-deftest agent-shell-openai-codex-app-server-client-test ()
  "Test that app-server transport creates the native client."
  (let ((agent-shell-openai-authentication
         (agent-shell-openai-make-authentication :login t))
        (agent-shell-openai-codex-transport 'app-server)
        client-args)
    (cl-letf (((symbol-function 'agent-shell-codex-app-server-make-client)
               (lambda (&rest args)
                 (setq client-args args))))
      (agent-shell-openai-make-codex-client :buffer (current-buffer)))
    (should (equal (plist-get client-args :command) "codex"))
    (should (equal (plist-get client-args :command-params) '("app-server")))
    (should (eq (plist-get client-args :context-buffer) (current-buffer)))))

(ert-deftest agent-shell-openai-app-server-config-enables-busy-input-test ()
  "Test that app-server Codex config enables active-turn steering."
  (let ((agent-shell-openai-codex-transport 'app-server))
    (should (eq (map-elt (agent-shell-openai-make-codex-config)
                         :busy-prompt-handler)
                #'agent-shell-codex-app-server-handle-busy-prompt))))

(ert-deftest agent-shell-openai-acp-config-keeps-busy-input-queued-test ()
  "Test that ACP Codex config retains the normal next-turn queue."
  (let ((agent-shell-openai-codex-transport 'acp))
    (should-not (map-elt (agent-shell-openai-make-codex-config)
                         :busy-prompt-handler))))

(ert-deftest agent-shell-openai-app-server-start-ignores-empty-region-test ()
  "Test that app-server startup does not capture an empty active region."
  (let ((agent-shell-openai-codex-transport 'app-server)
        region-active-during-start)
    (with-temp-buffer
      (insert "first\nsecond\n")
      (goto-char (line-beginning-position 2))
      (set-mark (point))
      (activate-mark)
      (cl-letf (((symbol-function 'agent-shell--dwim)
                 (lambda (&rest _args)
                   (setq region-active-during-start (region-active-p)))))
        (agent-shell-openai-start-codex))
      (should-not region-active-during-start)
      (should (region-active-p)))))

(ert-deftest agent-shell-openai-app-server-start-keeps-selected-region-test ()
  "Test that app-server startup preserves a non-empty selected region."
  (let ((agent-shell-openai-codex-transport 'app-server)
        region-active-during-start)
    (with-temp-buffer
      (insert "selected")
      (set-mark (point-min))
      (activate-mark)
      (cl-letf (((symbol-function 'agent-shell--dwim)
                 (lambda (&rest _args)
                   (setq region-active-during-start (region-active-p)))))
        (agent-shell-openai-start-codex))
      (should region-active-during-start))))

(ert-deftest agent-shell-openai-acp-start-keeps-empty-region-test ()
  "Test that ACP startup retains its existing empty-region behavior."
  (let ((agent-shell-openai-codex-transport 'acp)
        region-active-during-start)
    (with-temp-buffer
      (insert "text")
      (set-mark (point))
      (activate-mark)
      (cl-letf (((symbol-function 'agent-shell--dwim)
                 (lambda (&rest _args)
                   (setq region-active-during-start (region-active-p)))))
        (agent-shell-openai-start-codex))
      (should region-active-during-start))))

(provide 'agent-shell-openai-tests)
;;; agent-shell-openai-tests.el ends here
