;;; agent-shell-opencode-tests.el --- Tests for agent-shell-opencode -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-opencode)

;;; Code:

(ert-deftest agent-shell-opencode-default-model-id-test ()
  "Test that OpenCode config exposes default model id."
  (let ((default-model-id-fn
         (map-elt (agent-shell-opencode-make-agent-config) :default-model-id)))

    (let ((agent-shell-opencode-default-model-id nil))
      (should (null (funcall default-model-id-fn))))

    (let ((agent-shell-opencode-default-model-id "anthropic/claude-opus-4-5"))
      (should (string= (funcall default-model-id-fn) "anthropic/claude-opus-4-5")))))

(ert-deftest agent-shell-opencode-default-config-options-test ()
  "Test that OpenCode config exposes default config options."
  (let ((default-config-options-fn
         (map-elt (agent-shell-opencode-make-agent-config) :default-config-options)))

    (let ((agent-shell-opencode-default-config-options nil))
      (should (null (funcall default-config-options-fn))))

    (let ((agent-shell-opencode-default-config-options
           '(("model" . "anthropic/claude-opus-4-5")
             ("effort" . "high"))))
      (should (equal (funcall default-config-options-fn)
                     '(("model" . "anthropic/claude-opus-4-5")
                       ("effort" . "high")))))))

(ert-deftest agent-shell-opencode-default-session-mode-id-test ()
  "Test that OpenCode config exposes default session mode id."
  (let ((default-session-mode-id-fn
         (map-elt (agent-shell-opencode-make-agent-config) :default-session-mode-id)))

    (let ((agent-shell-opencode-default-session-mode-id nil))
      (should (null (funcall default-session-mode-id-fn))))

    (let ((agent-shell-opencode-default-session-mode-id "plan"))
      (should (string= (funcall default-session-mode-id-fn) "plan")))))

(provide 'agent-shell-opencode-tests)
;;; agent-shell-opencode-tests.el ends here
