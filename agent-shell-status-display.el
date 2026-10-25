;;; agent-shell-status-display.el --- Shell status colors and completion scrolling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christian Smith
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Color the mode-line name without changing buffer or session identity.

;;; Code:

(require 'map)
(require 'seq)
(require 'shell-maker)
(require 'agent-shell-faces)

(defvar agent-shell--state)
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--permission-pending-p "agent-shell")
(declare-function agent-shell-elicitation--pending-p "agent-shell-elicitation")
(declare-function agent-shell-elicitation-jump-to-latest-form "agent-shell-elicitation")
(declare-function agent-shell-jump-to-latest-permission-button-row "agent-shell")
(declare-function agent-shell--live-input-prompt-p "agent-shell-prompt")

(defvar agent-shell-status-display--name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'agent-shell-status-display--click)
    map)
  "Shared mouse binding for the mode-line agent name.")

(defcustom agent-shell-scroll-on-turn-end 'when-following
  "When to reveal the input prompt at the end of an agent turn.
`when-following' scrolls windows already following the buffer end before
completion rendering.  `always' scrolls every window displaying the shell.
Nil preserves all reading positions.  Never display a hidden shell or
select a different window.  Existing draft cursor positions are preserved."
  :type '(choice (const :tag "When following output" when-following)
                 (const :tag "Always" always)
                 (const :tag "Never" nil))
  :group 'agent-shell)

(defface agent-shell-status-active
  '((((class color) (background dark)) :foreground "DeepSkyBlue")
    (((class color) (background light)) :foreground "blue3")
    (t :inherit mode-line-buffer-id))
  "Agent name while the agent is working."
  :group 'agent-shell-faces)

(defface agent-shell-status-ready
  '((((class color) (background dark)) :foreground "white" :background "#165b33")
    (((class color) (background light)) :foreground "white" :background "#006b35")
    (t :inherit mode-line-buffer-id))
  "Agent name when the shell is ready for input."
  :group 'agent-shell-faces)

(defface agent-shell-status-input
  '((((class color)) :foreground "#111111" :background "#f1c40f")
    (t :inherit mode-line-buffer-id))
  "Agent name when a question or approval needs an answer."
  :group 'agent-shell-faces)

(defface agent-shell-status-failed
  '((((class color)) :foreground "white" :background "#8b1a1a")
    (t :inherit mode-line-buffer-id))
  "Agent name after a failed turn, until the next submission."
  :group 'agent-shell-faces)

(defun agent-shell-status-display--question ()
  "Return an outstanding Codex question context, or nil."
  (when-let* ((questions (map-nested-elt agent-shell--state '(:client :async-questions))))
    (catch 'question
      (maphash (lambda (_id context)
                 (when (and (memq (map-elt context :status) '(pending answering))
                            (buffer-live-p (map-elt context :buffer)))
                   (throw 'question context)))
               questions)
      nil)))

(defun agent-shell-status-display--status ()
  "Return the display state, independently of work and clock accounting."
  (cond
   ((or (agent-shell-status-display--question)
        (agent-shell--permission-pending-p)
        (agent-shell-elicitation--pending-p)) 'input)
   ((map-elt agent-shell--state :status-display-failed) 'failed)
   ((shell-maker-busy) 'active)
   ((map-elt agent-shell--state :init-finished) 'ready)
   (t 'active)))

(defun agent-shell-status-display--name ()
  "Return the color-only mode-line name, without renaming the buffer.
For example, display `Codex Agent @ app' as `Codex @ app'."
  (let* ((name (buffer-name))
         (status (agent-shell-status-display--status))
         (face (pcase status
                 ('active 'agent-shell-status-active)
                 ('ready 'agent-shell-status-ready)
                 ('input 'agent-shell-status-input)
                 ('failed 'agent-shell-status-failed))))
    (when (string-match "\\`Codex Agent\\( @.*\\|<[0-9]+>\\|\\)\\'" name)
      (setq name (concat "Codex" (match-string 1 name))))
    (propertize (replace-regexp-in-string "%" "%%" name t t) 'face face
                'help-echo "Jump to the pending question or input prompt"
                'mouse-face 'mode-line-highlight
                'local-map agent-shell-status-display--name-map)))

(defun agent-shell-status-display--click (event)
  "Jump to the question or prompt in the window clicked by EVENT."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (select-window window)
      (agent-shell-jump-to-input))))

(defun agent-shell-jump-to-input ()
  "Jump to the pending question, approval, form, or input prompt."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Use this command in an Agent Shell buffer"))
  (cond
   ((agent-shell-status-display--question)
    (pop-to-buffer (map-elt (agent-shell-status-display--question) :buffer)))
   ((agent-shell--permission-pending-p)
    (agent-shell-jump-to-latest-permission-button-row))
   ((agent-shell-elicitation--pending-p)
    (agent-shell-elicitation-jump-to-latest-form))
   (t (goto-char (point-max)) (recenter -1))))

(defun agent-shell-status-display--on-event (event)
  "Update display-only failure state after shell EVENT."
  (pcase (map-elt event :event)
    ('input-submitted (map-put! agent-shell--state :status-display-failed nil))
    ('turn-failed (map-put! agent-shell--state :status-display-failed t))
    ('turn-complete
     (map-put! agent-shell--state :status-display-failed
               (not (member (map-nested-elt event '(:data :stop-reason))
                            '("end_turn" "cancelled"))))))
  (when (memq (map-elt event :event)
              '(input-submitted turn-complete turn-failed permission-request
                permission-response elicitation-request elicitation-response init-finished))
    (force-mode-line-update)))

(defun agent-shell-status-display--setup ()
  "Install display-only status without changing shell identity or events."
  (setq-local mode-line-buffer-identification
              '((:eval (agent-shell-status-display--name))))
  (unless (map-elt agent-shell--state :status-display-subscription)
    (map-put! agent-shell--state :status-display-subscription
              (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :on-event #'agent-shell-status-display--on-event))))

(defun agent-shell-status-display--prepare-completion (&optional failed)
  "Remember following windows before rendering, recording FAILED if non-nil."
  (when failed (map-put! agent-shell--state :status-display-failed t))
  (save-restriction
    (widen)
    (map-put! agent-shell--state :completion-followers
              (cons t
                    (seq-filter (lambda (window)
                                  (and (= (window-point window) (point-max))
                                       (pos-visible-in-window-p (point-max) window)))
                                (get-buffer-window-list (current-buffer) nil t))))))

(defun agent-shell-status-display--take-completion ()
  "Take ownership of the pending completion snapshot before finish hooks run."
  (let ((followers (map-elt agent-shell--state :completion-followers)))
    (when followers (map-put! agent-shell--state :completion-followers nil))
    followers))

(defun agent-shell-status-display--finish-completion (followers)
  "Reveal the settled prompt for FOLLOWERS without stealing focus.
Keep destinations outside the original buffer restriction inaccessible."
  (when followers
    (let ((minimum (point-min)) (maximum (point-max)))
      (save-restriction
        (widen)
        (dolist (window (pcase agent-shell-scroll-on-turn-end
                          ('always (get-buffer-window-list (current-buffer) nil t))
                          ('when-following (cdr followers))))
          (when (and (window-live-p window) (eq (window-buffer window) (current-buffer)))
            (let* ((prompt-end (and (agent-shell--live-input-prompt-p comint-last-prompt)
                                    (marker-position (cdr comint-last-prompt))))
                   (destination (if (and prompt-end (>= (window-point window) prompt-end))
                                    (window-point window)
                                  (point-max))))
              (when (<= minimum destination maximum)
                (set-window-point window destination)
                (with-selected-window window (recenter -1))))))))))

(provide 'agent-shell-status-display)

;;; agent-shell-status-display.el ends here
