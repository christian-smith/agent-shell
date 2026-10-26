;;; agent-shell-session-name.el --- Persistent session names -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christian Smith
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep persistent session names distinct from local-only buffer names.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'shell-maker)

(defvar agent-shell--state)
(defvar agent-shell-viewport--suffix)
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell--set-session-title "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--update-header-and-mode-line "agent-shell")
(declare-function agent-shell--session-choice-label "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")

(defvar agent-shell-session-name--pending (make-hash-table :test #'equal)
  "Pending naming requests keyed by provider, handler and session id.")

(defun agent-shell-session-name--handler ()
  "Return the current agent's persistent naming handler, if supported."
  (let ((handler (map-nested-elt agent-shell--state '(:agent-config :session-name-handler))))
    (and (functionp handler) handler)))

(defun agent-shell-session-name--validate (name)
  "Return trimmed NAME, rejecting empty names and control characters."
  (unless (and (stringp name)
               (not (string-match-p "[[:cntrl:]]" name))
               (not (string-empty-p (string-trim name))))
    (user-error "Session name must be non-empty text on one line"))
  (string-trim name))

(defun agent-shell-session-name--set-buffer-label (label)
  "Set the shell's LABEL and preserve its existing viewport relationship."
  (let ((shell (current-buffer))
        (viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer) :existing-only t))
        (candidate label)
        (index 1))
    (while (or (let ((existing (get-buffer candidate)))
                 (and existing (not (eq existing shell))))
               (let ((existing (get-buffer (concat candidate agent-shell-viewport--suffix))))
                 (and existing (not (eq existing viewport)))))
      (setq index (1+ index)
            candidate (format "%s<%d>" label index)))
    (shell-maker-set-buffer-name shell candidate)
    (when viewport
      (with-current-buffer viewport
        (rename-buffer (concat candidate agent-shell-viewport--suffix))))))

(defun agent-shell-session-name--apply (session-id name &optional observed)
  "Apply confirmed NAME to the current shell if it still owns SESSION-ID.
OBSERVED records native metadata so older acknowledgements cannot replace it.
For example, name `opp-ts' displays `Codex @ OppFinderRust / opp-ts'."
  (when (and (derived-mode-p 'agent-shell-mode)
             (equal (map-nested-elt agent-shell--state '(:session :id)) session-id)
             (or (null name)
                 (and (stringp name)
                      (not (string-empty-p (string-trim name)))
                      (not (string-match-p "[[:cntrl:]]" name)))))
    (when observed
      (map-put! agent-shell--state :session-name-update (list session-id name)))
    (if name
        (agent-shell--set-session-title name)
      (when (map-nested-elt agent-shell--state '(:session :title))
        (map-put! (map-elt agent-shell--state :session) :title nil)
        (agent-shell--emit-event :event 'session-title-changed :data '((:title)))))
    (agent-shell-session-name--set-buffer-label
     (concat (format "%s @ %s"
                     (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
                     (file-name-nondirectory (directory-file-name (agent-shell-cwd))))
             (when name (concat " / " name))))
    (agent-shell--update-header-and-mode-line)))

(defun agent-shell-session-name--request (session-id name &optional on-complete)
  "Persist NAME for SESSION-ID, then call ON-COMPLETE after success or failure.
Only confirmed names update matching live shells.  Never submit a prompt."
  (let* ((name (agent-shell-session-name--validate name))
         (handler (agent-shell-session-name--handler))
         (state agent-shell--state)
         (buffer (current-buffer))
         (client (map-elt state :client))
         (provider (map-nested-elt state '(:agent-config :identifier)))
         (key (list provider handler session-id))
         (token (list buffer client))
         (release
          (lambda ()
            (when (eq (gethash key agent-shell-session-name--pending) token)
              (remhash key agent-shell-session-name--pending))))
         (shells (seq-keep
                  (lambda (shell)
                    (with-current-buffer shell
                      (when (and (eq provider (map-nested-elt agent-shell--state '(:agent-config :identifier)))
                                 (eq handler (agent-shell-session-name--handler))
                                 (equal session-id (map-nested-elt agent-shell--state '(:session :id))))
                        (list shell agent-shell--state (map-elt agent-shell--state :session)
                              (map-elt agent-shell--state :session-name-update)
                              (map-elt agent-shell--state :client)))))
                  (agent-shell-buffers)))
         (finished nil)
         (complete
          (lambda ()
            (when (and on-complete (buffer-live-p buffer)
                       (eq (buffer-local-value 'agent-shell--state buffer) state)
                       (eq (map-elt state :client) client))
              (with-current-buffer buffer (funcall on-complete)))))
         (success
          (lambda ()
            (unless finished
              (setq finished t)
              (funcall release)
              (dolist (entry shells)
                (when (buffer-live-p (car entry))
                  (with-current-buffer (car entry)
                    (when (and (eq agent-shell--state (nth 1 entry))
                               (eq (map-elt agent-shell--state :session) (nth 2 entry))
                               (eq (map-elt agent-shell--state :session-name-update) (nth 3 entry))
                               (eq (map-elt agent-shell--state :client) (nth 4 entry)))
                      (agent-shell-session-name--apply session-id name)))))
              (message "Session named %s" name)
              (funcall complete))))
         (failure
          (lambda (reason)
            (unless finished
              (setq finished t)
              (funcall release)
              (message "Could not rename session: %s" reason)
              (funcall complete)))))
    (condition-case err
        (progn
          (unless handler
            (user-error "This agent does not support persistent session names"))
          (unless (and (stringp session-id) (not (string-empty-p session-id)))
            (user-error "No initialized session to rename"))
          (when (gethash key agent-shell-session-name--pending)
            (user-error "Wait for this session's pending rename to finish")))
      (user-error
       (if on-complete (funcall failure (error-message-string err))
         (signal (car err) (cdr err)))))
    (unless finished
      (puthash key token agent-shell-session-name--pending)
      (condition-case err
          (funcall handler `((:state . ,state) (:session-id . ,session-id)
                            (:name . ,name) (:on-success . ,success)
                            (:on-failure . ,failure)))
        (error (if finished (signal (car err) (cdr err))
                 (funcall failure (error-message-string err))))))))

;;;###autoload
(defun agent-shell-rename-session (&optional name)
  "Persist NAME for the current session and update its live buffer label.
Unlike `agent-shell-rename-buffer', this name is shown when resuming later.
For example, use `opp-ts' for a scoped session in a shared workspace root."
  (interactive)
  (unless (and (derived-mode-p 'agent-shell-mode)
               (agent-shell-session-name--handler))
    (user-error "Use this command in a shell supporting persistent names"))
  (let ((session-id (map-nested-elt agent-shell--state '(:session :id))))
    (unless session-id (user-error "No initialized session to rename"))
    (agent-shell-session-name--request
     session-id
     (or name (read-string "Session name: "
                           (map-nested-elt agent-shell--state '(:session :title)))))))

(defun agent-shell-session-name--rename-saved (sessions on-complete)
  "Choose one of SESSIONS to rename without resuming, then call ON-COMPLETE."
  (let* ((choices (seq-map
                   (lambda (session)
                     (cons (format "%s [%s]"
                                   (agent-shell--session-choice-label :acp-session session)
                                   (map-elt session 'sessionId))
                           session))
                   sessions))
         (session (cdr (assoc (completing-read "Rename saved session: " choices nil t)
                              choices)))
         (name (condition-case err
                   (agent-shell-session-name--validate
                    (read-string "Session name: " (map-elt session 'title)))
                 (user-error (message "%s" (error-message-string err)) nil)
                 (quit nil))))
    (unless session (user-error "No saved session selected"))
    (if name
        (agent-shell-session-name--request (map-elt session 'sessionId) name on-complete)
      (funcall on-complete))))

(provide 'agent-shell-session-name)
;;; agent-shell-session-name.el ends here
