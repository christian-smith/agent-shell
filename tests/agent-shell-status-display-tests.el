;;; agent-shell-status-display-tests.el --- Status display tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-shell)

(ert-deftest agent-shell-status-display-preserves-identity-and-accounting ()
  "Short labels change neither provider/session identity nor work accounting."
  (with-temp-buffer
    (rename-buffer "Codex Agent @ status-fixture" t)
    (setq major-mode 'agent-shell-mode)
    (let* ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)
                                                       :agent-config '((:identifier . openai-codex))))
           (shell-maker--config (make-shell-maker-config :name "agent"))
           (shell-maker--busy t)
           (client (agent-shell-codex-app-server-make-client :command "sh"))
           (session (map-elt agent-shell--state :session))
           (requests (list '((:method . "session/prompt"))))
           (name (buffer-name)))
      (map-put! agent-shell--state :client client)
      (map-put! session :id "same-session")
      (map-put! agent-shell--state :active-requests requests)
      (agent-shell-status-display--setup)
      (agent-shell-status-display--setup)
      (should (= 1 (length (map-elt agent-shell--state :event-subscriptions))))
      (should (equal (substring-no-properties (agent-shell-status-display--name))
                     "Codex @ status-fixture"))
      (should (eq (get-text-property 0 'face (agent-shell-status-display--name))
                  'agent-shell-status-active))
      (should (eq (agent-shell-status) 'busy))
      (should (equal (buffer-name) name))
      (should (equal (map-nested-elt agent-shell--state '(:agent-config :identifier)) 'openai-codex))
      (should (eq (map-elt agent-shell--state :active-requests) requests))
      (should (eq (map-elt agent-shell--state :session) session))
      (should (equal (map-elt session :id) "same-session"))
      (should (eq (map-elt agent-shell--state :client) client)))))

(ert-deftest agent-shell-status-display-attention-is-not-work-state ()
  "An asynchronous question changes color without pretending work has stopped."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let* ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
           (shell-maker--config (make-shell-maker-config :name "agent"))
           (shell-maker--busy t)
           (client (agent-shell-codex-app-server-make-client :command "sh"))
           (question (generate-new-buffer " *status-question*"))
           (context (list (cons :status 'pending) (cons :buffer question))))
      (unwind-protect
          (progn
            (map-put! agent-shell--state :client client)
            (puthash "question" context (map-elt client :async-questions))
            (should (eq (agent-shell-status-display--status) 'input))
            (should (eq (agent-shell-status) 'busy))
            (should (eq (get-text-property 0 'face (agent-shell-status-display--name))
                        'agent-shell-status-input))
            (map-put! context :status 'answering)
            (should (eq (agent-shell-status-display--status) 'input))
            (map-put! context :status 'answered)
            (should (eq (agent-shell-status-display--status) 'active))
            (map-put! context :status 'pending)
            (setq shell-maker--busy nil)
            (should (eq (agent-shell-status-display--status) 'input))
            (should (eq (agent-shell-status) 'ready))
            (kill-buffer question)
            (map-put! agent-shell--state :init-finished t)
            (should (eq (agent-shell-status-display--status) 'ready)))
        (when (buffer-live-p question) (kill-buffer question))))))

(ert-deftest agent-shell-status-display-permissions-forms-and-failures ()
  "Pending answers outrank failures; completed and cancelled turns clear failure."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
          (shell-maker--config (make-shell-maker-config :name "agent"))
          (shell-maker--busy nil))
      (map-put! agent-shell--state :init-finished t)
      (agent-shell-status-display--on-event '((:event . turn-failed)))
      (should (eq (agent-shell-status-display--status) 'failed))
      (map-put! agent-shell--state :tool-calls '(("tool" . ((:permission-request-id . 3)))))
      (should (eq (agent-shell-status-display--status) 'input))
      (map-put! agent-shell--state :tool-calls nil)
      (map-put! agent-shell--state :elicitations '((3 . ((:status . pending)))))
      (should (eq (agent-shell-status-display--status) 'input))
      (map-put! agent-shell--state :elicitations nil)
      (agent-shell-status-display--on-event '((:event . input-submitted)))
      (should (eq (agent-shell-status-display--status) 'ready))
      (agent-shell-status-display--on-event '((:event . turn-complete) (:data . ((:stop-reason . "refusal")))))
      (should (eq (agent-shell-status-display--status) 'failed))
      (dolist (reason '("end_turn" "cancelled"))
        (agent-shell-status-display--on-event `((:event . turn-complete) (:data . ((:stop-reason . ,reason)))))
        (should (eq (agent-shell-status-display--status) 'ready))))))

(ert-deftest agent-shell-status-display-does-not-rename-custom-names ()
  "Only the default Codex Agent prefix is shortened in the displayed label."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let ((agent-shell--state (agent-shell--make-state))
          (shell-maker--config (make-shell-maker-config :name "agent"))
          (shell-maker--busy nil))
      (dolist (name '("Research Agent" "Claude Agent @ app" "Codex experiment" "Codex Agent"))
        (rename-buffer name t)
        (should (equal (substring-no-properties (agent-shell-status-display--name))
                       (if (equal name "Codex Agent") "Codex" name)))
        (should (equal (buffer-name) name))))))

(ert-deftest agent-shell-status-display-quotes-mode-line-percent ()
  "Literal percent characters in project names are not mode-line directives."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (rename-buffer "Codex Agent @ 100% app" t)
    (let ((agent-shell--state (agent-shell--make-state))
          (shell-maker--config (make-shell-maker-config :name "agent"))
          (shell-maker--busy nil))
      (should (equal (substring-no-properties (agent-shell-status-display--name))
                     "Codex @ 100%% app")))))

(ert-deftest agent-shell-status-display-completion-scroll-policy ()
  "Completion scrolls only eligible windows and preserves focus and draft text."
  (dolist (policy '(when-following always nil))
    (dolist (following '(nil t))
      (save-window-excursion
        (with-temp-buffer
          (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
                (agent-shell-scroll-on-turn-end policy)
                (window (selected-window))
                (comint-last-prompt nil)
                (recentered 0))
            (set-window-buffer window (current-buffer))
            (insert "Response\n")
            (set-window-point window (if following (point-max) (point-min)))
            (cl-letf (((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) following))
                      ((symbol-function 'recenter) (lambda (&rest _) (setq recentered (1+ recentered)))))
              (agent-shell-status-display--prepare-completion)
              ;; Completion inserts output and a prompt after the old end.
              (goto-char (point-max))
              (insert "Final output\nCodex> ")
              (setq comint-last-prompt (cons (copy-marker (- (point-max) 7)) (copy-marker (point-max))))
              (set-window-point window (point-min))
              (agent-shell-status-display--finish-completion (agent-shell-status-display--take-completion))
              (if (or (eq policy 'always) (and (eq policy 'when-following) following))
                  (progn (should (= recentered 1)) (should (= (window-point window) (point-max))))
                (should (= recentered 0))
                (should (= (window-point window) (point-min))))
              (should (eq (selected-window) window))
              (should-not (map-elt agent-shell--state :completion-followers)))))))))

(ert-deftest agent-shell-status-display-completion-preserves-draft-cursor ()
  "Always-scroll reveals the prompt without moving a draft cursor or changing text."
  (save-window-excursion
    (with-temp-buffer
      (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
            (agent-shell-scroll-on-turn-end 'always)
            (comint-last-prompt nil)
            (window (selected-window)))
        (set-window-buffer window (current-buffer))
        (insert "Codex> ")
        (setq comint-last-prompt (cons (copy-marker 1) (copy-marker (point-max))))
        (insert "draft input")
        (set-window-point window 10)
        (agent-shell-status-display--prepare-completion)
        (let ((before (buffer-string)))
          (cl-letf (((symbol-function 'recenter) #'ignore))
            (agent-shell-status-display--finish-completion (agent-shell-status-display--take-completion)))
          (should (equal before (buffer-string)))
          (should (= (window-point window) 10)))))))

(ert-deftest agent-shell-status-display-scrolls-without-selecting-shell ()
  "A background window can reveal completion without stealing the selected window."
  (save-window-excursion
    (let ((selected (selected-window))
          (shell (generate-new-buffer " *status-background*")))
      (unwind-protect
          (let ((window (split-window-right)))
            (set-window-buffer window shell)
            (with-current-buffer shell
              (let ((agent-shell--state (agent-shell--make-state :buffer shell))
                    (agent-shell-scroll-on-turn-end 'always)
                    (comint-last-prompt nil))
                (insert (make-string 200 ?x))
                (set-window-point window 1)
                (agent-shell-status-display--prepare-completion)
                (agent-shell-status-display--finish-completion (agent-shell-status-display--take-completion))
                (should (eq selected (selected-window)))
                (should (= (window-point window) (point-max))))))
        (kill-buffer shell)))))

(ert-deftest agent-shell-status-display-failure-without-extra-events ()
  "Recording a failed completion does not emit clock events or change work state."
  (with-temp-buffer
    (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer))))
      (cl-letf (((symbol-function 'agent-shell--emit-event)
                 (lambda (&rest _) (ert-fail "Display code emitted a lifecycle event"))))
        (agent-shell-status-display--prepare-completion t)
        (should (map-elt agent-shell--state :status-display-failed))
        (agent-shell-status-display--finish-completion (agent-shell-status-display--take-completion))))))

(ert-deftest agent-shell-status-display-narrowed-reading-position ()
  "Completion preserves both the restriction and reading position when narrowed."
  (save-window-excursion
    (with-temp-buffer
      (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
            (agent-shell-scroll-on-turn-end 'always)
            (comint-last-prompt nil)
            (window (selected-window)))
        (set-window-buffer window (current-buffer))
        (dotimes (_ 100) (insert "Older output\n"))
        (narrow-to-region 1 100)
        (goto-char 20)
        (agent-shell-status-display--prepare-completion)
        (agent-shell-status-display--finish-completion (agent-shell-status-display--take-completion))
        (should (= (point-min) 1))
        (should (= (point-max) 100))
        (should (= (point) 20))))))

(ert-deftest agent-shell-status-display-stale-prompt-is-not-a-draft ()
  "An old prompt followed by output cannot retain a transcript cursor as a draft."
  (save-window-excursion
    (with-temp-buffer
      (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
            (agent-shell-scroll-on-turn-end 'always)
            (comint-last-prompt nil)
            (window (selected-window)))
        (set-window-buffer window (current-buffer))
        (insert "Codex> ")
        (setq comint-last-prompt (cons (copy-marker 1) (copy-marker (point-max))))
        (insert (propertize "New output\nMore output\n" 'field 'output))
        (set-window-point window 10)
        (agent-shell-status-display--prepare-completion)
        (agent-shell-status-display--finish-completion (agent-shell-status-display--take-completion))
        (should (= (window-point window) (point-max)))))))

(ert-deftest agent-shell-status-display-nested-completion-keeps-followers ()
  "A finish hook completing another turn cannot consume the outer scroll snapshot."
  (save-window-excursion
    (with-temp-buffer
      (setq major-mode 'agent-shell-mode)
      (let* ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
             (shell-maker--config (make-shell-maker-config :name "agent" :prompt "Codex> "))
             (shell-maker--busy t)
             (comint-last-prompt nil)
             (comint-last-output-start (make-marker))
             (comint-prompt-regexp "^Codex> ")
             (comint-use-prompt-regexp nil)
             (agent-shell-scroll-on-turn-end 'when-following)
             (window (selected-window))
             (process (make-pipe-process :name "status-nested-test" :buffer (current-buffer) :noquery t))
             (recentered 0)
             (shell-maker-finish-output-hook
              (list (lambda ()
                      (let ((shell-maker-finish-output-hook nil))
                        (agent-shell-status-display--prepare-completion)
                        (agent-shell--finish-output :config shell-maker--config :success nil))))))
        (unwind-protect
            (cl-letf (((symbol-function 'shell-maker-buffer) (lambda (_) (current-buffer)))
                      ((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) t))
                      ((symbol-function 'recenter) (lambda (&rest _) (setq recentered (1+ recentered)))))
              (set-window-buffer window (current-buffer))
              (insert "Response\n")
              (set-window-point window (point-max))
              (agent-shell-status-display--prepare-completion)
              (insert "Footer\n")
              (set-window-point window 1)
              (agent-shell--finish-output :config shell-maker--config :success nil)
              (should (= recentered 1))
              (should-not (map-elt agent-shell--state :completion-followers)))
          (delete-process process))))))

(ert-deftest agent-shell-status-display-completion-cleans-up-before-hook-error ()
  "An exceptional finish hook cannot leave a snapshot for a later turn."
  (with-temp-buffer
    (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
          (shell-maker--busy t)
          (shell-maker-finish-output-hook (list (lambda () (error "Hook failed")))))
      (cl-letf (((symbol-function 'agent-shell--live-input-prompt-p) (lambda (_) t)))
        (agent-shell-status-display--prepare-completion)
        (should-error (agent-shell--finish-output :config nil :success nil))
        (should-not (map-elt agent-shell--state :completion-followers))
        (should-not shell-maker--busy)))))

(ert-deftest agent-shell-status-display-redisplay-is-buffer-local ()
  "Rendering reuses its mouse map and status changes invalidate only this buffer."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let ((agent-shell--state (agent-shell--make-state))
          (shell-maker--config (make-shell-maker-config :name "agent"))
          (shell-maker--busy nil)
          (calls 0))
      (should (eq (get-text-property 0 'local-map (agent-shell-status-display--name))
                  (get-text-property 0 'local-map (agent-shell-status-display--name))))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&optional all)
                   (should-not all)
                   (setq calls (1+ calls)))))
        (agent-shell-status-display--on-event '((:event . input-submitted)))
        (should (= calls 1))))))

(provide 'agent-shell-status-display-tests)

;;; agent-shell-status-display-tests.el ends here
