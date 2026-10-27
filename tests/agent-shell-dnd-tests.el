;;; agent-shell-dnd-tests.el --- Tests for agent-shell drag and drop -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'yank-media)
(require 'agent-shell)
(require 'agent-shell-dnd)
(require 'agent-shell-viewport)

;;; Code:

(defconst agent-shell-dnd-test--no-temp-dir "/nonexistent/agent-shell-dnd-tmp/"
  "A value for the variable `temporary-file-directory' nothing lives under.
Bound while a test drops a real temp file that must not count as a
transient drop.  It need not exist: `file-in-directory-p' is nil for a
missing parent.")

(defmacro agent-shell-dnd-test--with-drop-target (inserted &rest body)
  "Run BODY as a drop on a buffer whose shell is the current buffer.
`agent-shell-insert' is stubbed to store its :text in INSERTED, the
shell is idle, and the variable `temporary-file-directory' points
nowhere so test files are attached in place."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell-insert)
              (lambda (&rest args)
                (setq ,inserted (plist-get args :text))))
             ((symbol-function 'agent-shell--shell-buffer)
              (lambda (&rest _) (current-buffer)))
             ((symbol-function 'shell-maker-busy) #'ignore))
     (let ((temporary-file-directory agent-shell-dnd-test--no-temp-dir))
       ,@body)))

(ert-deftest agent-shell--dnd-handle-file-url-inserts-file-context-test ()
  "A dropped file: URL is inserted as file context, relative to the shell.
Percent escapes in the URL are decoded before lookup."
  (let* ((dir (make-temp-file "agent-shell-dnd" t))
         (file (expand-file-name "my file.txt" dir))
         (inserted nil))
    (unwind-protect
        (agent-shell-dnd-test--with-drop-target inserted
          (with-temp-file file (insert "dropped"))
          (should (eq (agent-shell--dnd-handle-file-url
                       (concat "file://" (string-replace " " "%20" file)) 'copy)
                      'private))
          (should (equal inserted
                         (agent-shell--get-files-context
                          :files (list file)
                          :agent-cwd (agent-shell-cwd)))))
      (delete-directory dir t))))

(ert-deftest agent-shell--dnd-handle-file-url-attaches-every-file-test ()
  "Several files dropped together land in one insertion, in drop order."
  (let ((a (make-temp-file "agent-shell-dnd-a" nil ".txt" "a"))
        (b (make-temp-file "agent-shell-dnd-b" nil ".txt" "b"))
        (inserted nil))
    (unwind-protect
        (agent-shell-dnd-test--with-drop-target inserted
          (agent-shell--dnd-handle-file-url
           (list (concat "file://" a) (concat "file://" b)) 'copy)
          (should (equal inserted
                         (agent-shell--get-files-context
                          :files (list a b)
                          :agent-cwd (agent-shell-cwd)))))
      (delete-file a)
      (delete-file b))))

(ert-deftest agent-shell--dnd-handle-file-url-unreadable-file-test ()
  "An unreadable file refuses the whole drop before anything is copied."
  (let ((transient (make-temp-file "agent-shell-dnd" nil ".png" "pixels"))
        (screenshots-dir (make-temp-file "agent-shell-dnd-shots" t))
        (inserted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-insert)
                   (lambda (&rest args)
                     (setq inserted (plist-get args :text))))
                  ((symbol-function 'agent-shell--shell-buffer)
                   (lambda (&rest _) (current-buffer)))
                  ((symbol-function 'shell-maker-busy) #'ignore)
                  ((symbol-function 'agent-shell--dot-subdir)
                   (lambda (_subdir) screenshots-dir)))
          (should-error (agent-shell--dnd-handle-file-url
                         (list (concat "file://" transient)
                               "file:///nonexistent/agent-shell-dnd.txt")
                         'copy)
                        :type 'user-error)
          (should-not inserted)
          (should-not (directory-files screenshots-dir nil "^dropped-")))
      (delete-file transient)
      (delete-directory screenshots-dir t))))

(ert-deftest agent-shell--dnd-handle-file-url-directory-test ()
  "A dropped directory is refused: it is readable but not attachable."
  (let ((dir (make-temp-file "agent-shell-dnd" t))
        (inserted nil))
    (unwind-protect
        (agent-shell-dnd-test--with-drop-target inserted
          (should-error (agent-shell--dnd-handle-file-url (concat "file://" dir) 'copy)
                        :type 'user-error)
          (should-not inserted))
      (delete-directory dir t))))

(ert-deftest agent-shell--dnd-handle-file-url-busy-shell-queues-test ()
  "A drop mid-turn is queued, like `agent-shell-send-region'.
The drop target does not matter: what is busy is the shell, so a drop
on a viewport buffer queues too."
  (let ((file (make-temp-file "agent-shell-dnd" nil ".txt" "dropped"))
        (inserted nil)
        (queued nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted (plist-get args :text))))
                    ((symbol-function 'agent-shell--shell-buffer)
                     (lambda (&rest _) (current-buffer)))
                    ((symbol-function 'shell-maker-busy) (lambda () t))
                    ((symbol-function 'agent-shell--prompt-queue-read)
                     (lambda (&rest args) (plist-get args :initial)))
                    ((symbol-function 'agent-shell-prompt-queue)
                     (lambda (prompt) (setq queued prompt))))
            (let ((temporary-file-directory agent-shell-dnd-test--no-temp-dir))
              (agent-shell--dnd-handle-file-url (concat "file://" file) 'copy))
            (should-not inserted)
            (should (equal queued
                           (concat (agent-shell--get-files-context
                                    :files (list file)
                                    :agent-cwd (agent-shell-cwd))
                                   "\n\n")))))
      (delete-file file))))

(ert-deftest agent-shell--dnd-handle-file-url-busy-live-prompt-attaches-test ()
  "A drop mid-turn attaches to the live prompt when there is one.
Only a busy shell with nowhere to type falls back to the queue."
  (let ((file (make-temp-file "agent-shell-dnd" nil ".txt" "dropped"))
        (inserted nil)
        (queued nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted (plist-get args :text))))
                    ((symbol-function 'agent-shell--shell-buffer)
                     (lambda (&rest _) (current-buffer)))
                    ((symbol-function 'shell-maker-busy) (lambda () t))
                    ((symbol-function 'agent-shell--prompt-input-start)
                     (lambda () (point-max)))
                    ((symbol-function 'agent-shell-prompt-queue)
                     (lambda (prompt) (setq queued prompt))))
            (let ((temporary-file-directory agent-shell-dnd-test--no-temp-dir))
              (agent-shell--dnd-handle-file-url (concat "file://" file) 'copy))
            (should-not queued)
            (should (equal inserted
                           (agent-shell--get-files-context
                            :files (list file)
                            :agent-cwd (agent-shell-cwd))))))
      (delete-file file))))

(ert-deftest agent-shell--dnd-handle-file-url-copies-transient-file-test ()
  "A drop from the temp directory attaches the copy, not the vanishing original."
  (let ((file (make-temp-file "agent-shell-dnd" nil ".png" "pixels"))
        (screenshots-dir (make-temp-file "agent-shell-dnd-shots" t))
        (inserted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-insert)
                   (lambda (&rest args)
                     (setq inserted (plist-get args :text))))
                  ((symbol-function 'agent-shell--shell-buffer)
                   (lambda (&rest _) (current-buffer)))
                  ((symbol-function 'shell-maker-busy) #'ignore)
                  ((symbol-function 'agent-shell--dot-subdir)
                   (lambda (_subdir) screenshots-dir)))
          (agent-shell--dnd-handle-file-url (concat "file://" file) 'copy)
          (let ((copy (seq-first (directory-files screenshots-dir t "^dropped-"))))
            (should copy)
            (should (equal inserted
                           (agent-shell--get-files-context
                            :files (list copy)
                            :agent-cwd (agent-shell-cwd))))
            ;; The original is macOS's to delete, not ours.
            (should (file-exists-p file))))
      (delete-file file)
      (delete-directory screenshots-dir t))))

(ert-deftest agent-shell--dnd-keep-file-test ()
  "A transient file is copied with its extension, other files stay put.
Transient means under the variable `temporary-file-directory', unless it
is a file of a project that itself lives there."
  (let ((file (make-temp-file "agent-shell-dnd" nil ".png" "pixels"))
        (screenshots-dir (make-temp-file "agent-shell-dnd-shots" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--dot-subdir)
                   (lambda (_subdir) screenshots-dir)))
          (let ((copy (agent-shell--dnd-keep-file file)))
            (should (string-prefix-p (file-name-as-directory screenshots-dir) copy))
            (should (string-match-p "/dropped-[0-9]\\{8\\}-[0-9]\\{6\\}-.*\\.png\\'" copy))
            (should (equal (with-temp-buffer (insert-file-contents copy) (buffer-string))
                           "pixels")))
          (should (equal (agent-shell--dnd-keep-file file (file-name-directory file))
                         file))
          (let ((temporary-file-directory agent-shell-dnd-test--no-temp-dir))
            (should (equal (agent-shell--dnd-keep-file file) file))))
      (delete-file file)
      (delete-directory screenshots-dir t))))

(ert-deftest agent-shell--enable-dnd-installs-buffer-local-handler-test ()
  "Enabling drops routes the local file: forms to this buffer's handler.
The handler goes ahead of Emacs's own, the host form still reaches
`dnd-open-file', and the global value is left alone."
  (let ((global-alist (default-value 'dnd-protocol-alist)))
    (with-temp-buffer
      (agent-shell--enable-dnd)
      (should (local-variable-p 'dnd-protocol-alist))
      (dolist (url '("file:///tmp/a.png" "file:/tmp/a.png" "file:tmp/a.png"))
        (should (eq (cdr (seq-find (lambda (entry) (string-match-p (car entry) url))
                                   dnd-protocol-alist))
                    'agent-shell--dnd-handle-file-url)))
      (should (eq (cdr (seq-find (lambda (entry)
                                   (string-match-p (car entry) "file://localhost/tmp/a.png"))
                                 dnd-protocol-alist))
                  'dnd-open-file))
      ;; Re-entering the mode routes the buffer once, not twice.
      (let ((entries (length dnd-protocol-alist)))
        (agent-shell--enable-dnd)
        (should (= (length dnd-protocol-alist) entries))))
    (should (equal (default-value 'dnd-protocol-alist) global-alist))
    (should-not (rassq 'agent-shell--dnd-handle-file-url
                       (default-value 'dnd-protocol-alist)))))

(ert-deftest agent-shell-viewport-modes-enable-dnd-test ()
  "Both viewport buffers handle dropped files themselves."
  (cl-letf (((symbol-function 'agent-shell-viewport--update-header)
             (lambda (&rest _)))
            (agent-shell-file-completion-enabled nil))
    (with-temp-buffer
      (agent-shell-viewport-edit-mode)
      (should (rassq 'agent-shell--dnd-handle-file-url dnd-protocol-alist)))
    (with-temp-buffer
      (agent-shell-viewport-view-mode)
      (should (rassq 'agent-shell--dnd-handle-file-url dnd-protocol-alist)))))

(ert-deftest agent-shell--yank-media-image-inserts-file-context-test ()
  "A yanked image is saved byte for byte and inserted as file context."
  (let ((screenshots-dir (make-temp-file "agent-shell-yank-media" t))
        (data (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n #x00 #xff))
        (inserted nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted (plist-get args :text))))
                    ((symbol-function 'agent-shell--shell-buffer)
                     (lambda (&rest _) (current-buffer)))
                    ((symbol-function 'shell-maker-busy) #'ignore)
                    ((symbol-function 'agent-shell--dot-subdir)
                     (lambda (_subdir) screenshots-dir)))
            (agent-shell--yank-media-image 'image/png data)
            (let ((file (seq-first (directory-files screenshots-dir t "^clipboard-.*\\.png\\'"))))
              (should file)
              (should (equal (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally file)
                               (buffer-string))
                             data))
              (should (equal inserted
                             (agent-shell--get-files-context
                              :files (list file)
                              :agent-cwd (agent-shell-cwd)))))))
      (delete-directory screenshots-dir t))))

(ert-deftest agent-shell--yank-media-image-busy-shell-queues-test ()
  "An image yanked mid-turn with no live prompt is queued."
  (let ((screenshots-dir (make-temp-file "agent-shell-yank-media" t))
        (inserted nil)
        (queued nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'agent-shell-insert)
                     (lambda (&rest args)
                       (setq inserted (plist-get args :text))))
                    ((symbol-function 'agent-shell--shell-buffer)
                     (lambda (&rest _) (current-buffer)))
                    ((symbol-function 'shell-maker-busy) (lambda () t))
                    ((symbol-function 'agent-shell--dot-subdir)
                     (lambda (_subdir) screenshots-dir))
                    ((symbol-function 'agent-shell--prompt-queue-read)
                     (lambda (&rest args) (plist-get args :initial)))
                    ((symbol-function 'agent-shell-prompt-queue)
                     (lambda (prompt) (setq queued prompt))))
            (agent-shell--yank-media-image 'image/png "pixels")
            (should-not inserted)
            (should (equal queued
                           (concat (agent-shell--get-files-context
                                    :files (directory-files screenshots-dir t "^clipboard-")
                                    :agent-cwd (agent-shell-cwd))
                                   "\n\n")))))
      (delete-directory screenshots-dir t))))

(ert-deftest agent-shell--yank-media-image-extension-test ()
  "The MIME subtype becomes the extension, except SVG's."
  (should (equal (agent-shell--yank-media-image-extension 'image/png) "png"))
  (should (equal (agent-shell--yank-media-image-extension 'image/jpeg) "jpeg"))
  (should (equal (agent-shell--yank-media-image-extension 'image/svg+xml) "svg")))

(ert-deftest agent-shell-viewport-modes-register-yank-media-test ()
  "Both viewport buffers accept images from `yank-media'."
  (cl-letf (((symbol-function 'agent-shell-viewport--update-header)
             (lambda (&rest _)))
            (agent-shell-file-completion-enabled nil))
    (with-temp-buffer
      (agent-shell-viewport-edit-mode)
      (should (eq (map-elt yank-media--registered-handlers "image/.*")
                  #'agent-shell--yank-media-image)))
    (with-temp-buffer
      (agent-shell-viewport-view-mode)
      (should (eq (map-elt yank-media--registered-handlers "image/.*")
                  #'agent-shell--yank-media-image)))))

(provide 'agent-shell-dnd-tests)
;;; agent-shell-dnd-tests.el ends here
