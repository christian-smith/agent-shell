;;; agent-shell-transcript-recovery-tests.el --- Transcript retry tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

(ert-deftest agent-shell-transcript-retries-append-after-write-error ()
  "A transient append failure retains the path and existing data."
  (let* ((root (make-temp-file "transcript-retry-" t))
         (file (expand-file-name "transcript.md" root)))
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'agent-shell-mode)
          (setq default-directory (file-name-as-directory root))
          (setq-local agent-shell--transcript-file file)
          (agent-shell--append-transcript :text "original\n" :file-path file)
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (signal 'file-error '("No space left on device")))))
            (agent-shell--append-transcript :text "failed\n" :file-path file))
          (should (equal agent-shell--transcript-file file))
          (agent-shell--append-transcript :text "resumed\n" :file-path file)
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-suffix-p "original\nresumed\n" (buffer-string)))))
      (delete-directory root t))))

(ert-deftest agent-shell-transcript-retries-header-after-write-error ()
  "A failed header write is retried before any later entry is appended."
  (let* ((root (make-temp-file "transcript-header-retry-" t))
         (file (expand-file-name "transcripts/transcript.md" root)))
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'agent-shell-mode)
          (setq default-directory (file-name-as-directory root))
          (setq-local agent-shell--transcript-file file)
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (signal 'file-error '("No space left on device")))))
            (agent-shell--append-transcript :text "failed\n" :file-path file))
          (should (equal agent-shell--transcript-file file))
          (should-not (file-exists-p file))
          (agent-shell--append-transcript :text "resumed\n" :file-path file)
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-prefix-p "---\nagent: " (buffer-string)))
            (should (string-suffix-p "# Agent Shell Transcript\n\nresumed\n"
                                     (buffer-string)))))
      (delete-directory root t))))

(provide 'agent-shell-transcript-recovery-tests)
;;; agent-shell-transcript-recovery-tests.el ends here
