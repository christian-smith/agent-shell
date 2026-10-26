;;; agent-shell-session-name-tests.el --- Persistent naming tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-shell)

(defmacro agent-shell-session-name-tests--with-shell (&rest body)
  "Run BODY with an initialized native Codex shell and no real process."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (rename-buffer "Codex Agent @ OppFinderRust" t)
     (setq major-mode 'agent-shell-mode)
     (let* ((agent-shell-openai-codex-transport 'app-server)
            (shell (current-buffer))
            (client (agent-shell-codex-app-server-make-client :command "sh" :context-buffer shell))
            (agent-shell--state (agent-shell--make-state
                                :buffer shell :agent-config (agent-shell-openai-make-codex-config)))
            (shell-maker--config (make-shell-maker-config :name "agent"))
            (shell-maker--busy t)
            request)
       (map-put! agent-shell--state :client client)
       (map-put! (map-elt agent-shell--state :session) :id "thread-1")
       (map-put! client :thread-id "thread-1")
       (map-put! client :active-turn-id "turn-1")
       (cl-letf (((symbol-function 'agent-shell-cwd) (lambda (&rest _) "/workspace/OppFinderRust/"))
                 ((symbol-function 'agent-shell-buffers) (lambda (&rest _) (list shell)))
                 ((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore)
                 ((symbol-function 'agent-shell-codex-app-server--client-started-p) (lambda (_) t))
                 ((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
                  (lambda (&rest args) (setq request args)))
                 ((symbol-function 'agent-shell--insert-to-shell-buffer)
                  (lambda (&rest _) (ert-fail "Naming submitted input"))))
         ,@body))))

(ert-deftest agent-shell-session-name-confirmed-rename-preserves-active-shell ()
  "Naming waits for confirmation and preserves draft, turn and clock identity."
  (agent-shell-session-name-tests--with-shell
    (insert "Transcript\nDraft")
    (let ((name (buffer-name)) (tick (buffer-chars-modified-tick))
          (cwd default-directory) events)
      (agent-shell-subscribe-to :shell-buffer shell
                                :on-event (lambda (event) (push (map-elt event :event) events)))
      (agent-shell-rename-session " opp-ts ")
      (should (equal (buffer-name) name))
      (should (equal (plist-get request :method) "thread/name/set"))
      (should (equal (plist-get request :params) '((threadId . "thread-1") (name . "opp-ts"))))
      (funcall (plist-get request :on-success) nil)
      (should (equal (buffer-name) "Codex @ OppFinderRust / opp-ts"))
      (should (equal shell-maker--buffer-name-override (buffer-name)))
      (should (equal (map-nested-elt agent-shell--state '(:session :title)) "opp-ts"))
      (should (equal (map-nested-elt agent-shell--state '(:session :id)) "thread-1"))
      (should (eq (map-elt agent-shell--state :client) client))
      (should (equal (map-elt client :active-turn-id) "turn-1"))
      (should shell-maker--busy)
      (should (= tick (buffer-chars-modified-tick)))
      (should (equal cwd default-directory))
      (should (equal events '(session-title-changed)))
      (setq request nil)
      (shell-maker-set-buffer-name shell "Local label")
      (should-not request)
      (should (equal (map-nested-elt agent-shell--state '(:session :title)) "opp-ts")))))

(ert-deftest agent-shell-session-name-failure-and-invalid-input-do-not-rename ()
  "Rejected names and connection failures preserve the existing label."
  (agent-shell-session-name-tests--with-shell
    (let ((original (buffer-name)))
      (dolist (invalid '("" "   " "line\nbreak" "tab\tname"))
        (should-error (agent-shell-rename-session invalid) :type 'user-error)
        (should-not request))
      (agent-shell-rename-session "opp-miner")
      (funcall (plist-get request :on-failure) '((message . "Refused")))
      (should (equal (buffer-name) original))
      (should-not (map-nested-elt agent-shell--state '(:session :title)))
      (setq request nil)
      (map-put! client :reconnecting t)
      (agent-shell-rename-session "opp-miner")
      (should-not request)
      (should (equal (buffer-name) original)))))

(ert-deftest agent-shell-session-name-restores-only-native-names-and-current-client ()
  "Native names restore labels; previews and stale clients cannot rename them."
  (agent-shell-session-name-tests--with-shell
    (let ((original (buffer-name)))
      (agent-shell-codex-app-server--restore-session-name
       client '((thread . ((id . "thread-1") (preview . "Initial sentences")))))
      (should (equal (buffer-name) original))
      (agent-shell-codex-app-server--restore-session-name
       client '((thread . ((id . "thread-2") (name . "Wrong session")))))
      (should (equal (buffer-name) original))
      (let ((stale (agent-shell-codex-app-server-make-client :command "sh" :context-buffer shell)))
        (agent-shell-codex-app-server--restore-session-name
         stale '((thread . ((id . "thread-1") (name . "Stale client")))))
        (should (equal (buffer-name) original)))
      (agent-shell-codex-app-server--restore-session-name
       client '((thread . ((id . "thread-1") (name . "opp-miner")))))
      (should (equal (buffer-name) "Codex @ OppFinderRust / opp-miner"))
      (agent-shell-codex-app-server--handle-notification
       client '((method . "thread/name/updated")
                (params . ((threadId . "thread-1") (threadName . "opp-ts")))))
      (should (equal (buffer-name) "Codex @ OppFinderRust / opp-ts"))
      (agent-shell-codex-app-server--restore-session-name
       client '((thread . ((id . "thread-1") (name)))))
      (should (equal (buffer-name) "Codex @ OppFinderRust"))
      (should-not (map-nested-elt agent-shell--state '(:session :title)))
      (agent-shell-codex-app-server--restore-session-name
       client '((thread . ((id . "thread-1") (name . "opp-ts")))))
      (agent-shell-codex-app-server--handle-notification
       client '((method . "thread/name/updated")
                (params . ((threadId . "thread-1") (threadName)))))
      (should (equal (buffer-name) "Codex @ OppFinderRust"))
      (should-not (map-nested-elt agent-shell--state '(:session :title))))))

(ert-deftest agent-shell-session-name-native-resume-restores-confirmed-name ()
  "Loading and resuming through the adapter restore the saved name afterward."
  (dolist (method '("session/load" "session/resume"))
    (agent-shell-session-name-tests--with-shell
      (cl-letf (((symbol-function 'agent-shell-codex-app-server--fetch-models)
                 (lambda (_client success &optional _) (funcall success))))
        (agent-shell-codex-app-server-send-request
         :client client :buffer shell
         :request `((:method . ,method) (:params . ((sessionId . "thread-1"))))
         :on-success (lambda (response)
                       (agent-shell--set-session-from-response
                        :acp-response response :acp-session-id "thread-1")))
        (should (equal (plist-get request :method) "thread/resume"))
        (funcall (plist-get request :on-success)
                 '((thread . ((id . "thread-1") (name . "opp-ts")))))
        (should (equal (buffer-name) "Codex @ OppFinderRust / opp-ts"))
        (should (equal (map-nested-elt agent-shell--state '(:session :title)) "opp-ts"))))))

(ert-deftest agent-shell-session-name-picker-refreshes-without-loading-renamed-session ()
  "The startup naming action refreshes the picker only after the RPC finishes."
  (agent-shell-session-name-tests--with-shell
    (let ((agent-shell-session-strategy 'prompt)
          (sessions '(((sessionId . "older") (title . "Initial preview"))))
          (list-calls 0) completed)
      (cl-letf (((symbol-function 'agent-shell--list-sessions)
                 (lambda (&rest args)
                   (setq list-calls (1+ list-calls))
                   (when (= list-calls 1)
                     (funcall (plist-get args :on-success) sessions))))
                ((symbol-function 'agent-shell--prompt-select-session)
                 (lambda (_) :rename-session))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (caar choices)))
                ((symbol-function 'read-string) (lambda (&rest _) "opp-ts"))
                ((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
                ((symbol-function 'agent-shell--send-request)
                 (lambda (&rest _) (ert-fail "Naming resumed a thread"))))
        (agent-shell--initiate-session-list-and-load
         :shell-buffer shell :on-session-init (lambda () (setq completed t)))
        (should (= list-calls 1))
        (should (equal (plist-get request :params) '((threadId . "older") (name . "opp-ts"))))
        (funcall (plist-get request :on-success) nil)
        (should (= list-calls 2))
        (should-not completed)))))

(ert-deftest agent-shell-session-name-saved-rename-does-not-resume ()
  "Saved sessions are selected by id and named without resuming any thread."
  (agent-shell-session-name-tests--with-shell
    (let ((original (buffer-name)) completed)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (should (= (length choices) 2))
                   (should-not (equal (caar choices) (caadr choices)))
                   (caadr choices)))
                ((symbol-function 'read-string) (lambda (&rest _) "opp-ts")))
        (agent-shell-session-name--rename-saved
         '(((sessionId . "older-1") (title . "Same preview"))
           ((sessionId . "older-2") (title . "Same preview")))
         (lambda () (setq completed t))))
      (should (equal (plist-get request :method) "thread/name/set"))
      (should (equal (plist-get request :params) '((threadId . "older-2") (name . "opp-ts"))))
      (should-not completed)
      (funcall (plist-get request :on-success) nil)
      (should completed)
      (should (equal (buffer-name) original))
      (should (equal (map-elt client :thread-id) "thread-1")))))

(ert-deftest agent-shell-session-name-picker-offers-naming-only-when-supported ()
  "The normal session picker exposes a naming action only for capable agents."
  (agent-shell-session-name-tests--with-shell
    (let ((noninteractive nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (should (member "Rename saved session..." (all-completions "" table)))
                   "Rename saved session...")))
        (should (eq (agent-shell--prompt-select-session '(((sessionId . "older")))) :rename-session)))
      (map-put! (map-elt agent-shell--state :agent-config) :session-name-handler nil)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (should-not (member "Rename saved session..." (all-completions "" table)))
                   "New shell")))
        (should-not (agent-shell--prompt-select-session '(((sessionId . "older")))))))))

(ert-deftest agent-shell-session-name-acp-does-not-advertise-naming ()
  "The ACP transport retains its existing unsupported naming behavior."
  (let ((agent-shell-openai-codex-transport 'acp))
    (should-not (map-elt (agent-shell-openai-make-codex-config) :session-name-handler))))

(ert-deftest agent-shell-session-name-resume-picker-uses-native-names-not-previews ()
  "Same-root sessions can be selected by their persistent scope names."
  (agent-shell-session-name-tests--with-shell
    (let* ((noninteractive nil)
           (response (agent-shell-codex-app-server--session-list-response
                      '((data . (((id . "ts-session") (cwd . "/workspace/OppFinderRust")
                                  (name . "opp-ts") (preview . "First opening sentence"))
                                 ((id . "miner-session") (cwd . "/workspace/OppFinderRust")
                                  (name . "opp-miner") (preview . "Second opening sentence"))))))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (let ((labels (all-completions "" table)))
                     (should (seq-some (lambda (label) (string-match-p "opp-miner" label)) labels))
                     (should-not (seq-some (lambda (label) (string-match-p "opening sentence" label)) labels))
                     (seq-find (lambda (label) (string-match-p "opp-ts" label)) labels)))))
        (should (equal (map-elt (agent-shell--prompt-select-session (map-elt response 'sessions)) 'sessionId)
                       "ts-session"))))))

(ert-deftest agent-shell-session-name-late-acknowledgement-preserves-newer-native-name ()
  "An older acknowledgement cannot replace a newer name observed from Codex."
  (agent-shell-session-name-tests--with-shell
    (agent-shell-rename-session "older-name")
    (agent-shell-codex-app-server--handle-notification
     client '((method . "thread/name/updated")
              (params . ((threadId . "thread-1") (threadName . "newer-name")))))
    (funcall (plist-get request :on-success) nil)
    (should (equal (map-nested-elt agent-shell--state '(:session :title)) "newer-name"))
    (should (equal (buffer-name) "Codex @ OppFinderRust / newer-name"))))

(ert-deftest agent-shell-session-name-overlapping-requests-are-rejected-across-shells ()
  "Only one metadata rename can be in flight for a session across buffers."
  (agent-shell-session-name-tests--with-shell
    (let ((agent-shell-session-name--pending (make-hash-table :test #'equal))
          (origin-state agent-shell--state))
      (agent-shell-rename-session "first-name")
      (let ((first-request request))
        (with-temp-buffer
          (let ((agent-shell--state (agent-shell--make-state
                                    :buffer (current-buffer)
                                    :agent-config (agent-shell-openai-make-codex-config))))
            (map-put! agent-shell--state :client client)
            (should-error (agent-shell-session-name--request "thread-1" "second-name") :type 'user-error)))
        (should (eq request first-request))
        (funcall (plist-get first-request :on-success) nil)
        (should (= (hash-table-count agent-shell-session-name--pending) 0))
        (agent-shell-rename-session "second-name")
        (let ((second-request request))
          (funcall (plist-get second-request :on-success) nil)
          (funcall (plist-get first-request :on-success) nil)
          (should (equal (map-nested-elt origin-state '(:session :title)) "second-name")))))))

(ert-deftest agent-shell-session-name-pending-request-releases-after-send-error ()
  "A synchronous send error releases the rename lock and allows retry."
  (agent-shell-session-name-tests--with-shell
    (let ((agent-shell-session-name--pending (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'agent-shell-codex-app-server--send-rpc-request)
                 (lambda (&rest _) (error "Connection lost"))))
        (agent-shell-rename-session "opp-ts"))
      (should (= (hash-table-count agent-shell-session-name--pending) 0))
      (agent-shell-rename-session "opp-ts")
      (funcall (plist-get request :on-failure) '((message . "Refused")))
      (should (= (hash-table-count agent-shell-session-name--pending) 0))
      (agent-shell-rename-session "opp-miner")
      (funcall (plist-get request :on-success) nil)
      (should (equal (buffer-name) "Codex @ OppFinderRust / opp-miner")))))

(ert-deftest agent-shell-session-name-pending-saved-rename-returns-to-picker ()
  "The saved picker recovers from an existing rename without releasing it."
  (agent-shell-session-name-tests--with-shell
    (let ((agent-shell-session-name--pending (make-hash-table :test #'equal))
          completed)
      (agent-shell-rename-session "first-name")
      (let ((first-request request))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices)))
                  ((symbol-function 'read-string) (lambda (&rest _) "second-name")))
          (agent-shell-session-name--rename-saved
           '(((sessionId . "thread-1") (title . "Initial preview")))
           (lambda () (setq completed t))))
        (should completed)
        (should (eq request first-request))
        (should (= (hash-table-count agent-shell-session-name--pending) 1))
        (funcall (plist-get first-request :on-success) nil)
        (should (= (hash-table-count agent-shell-session-name--pending) 0))))))

(ert-deftest agent-shell-session-name-invalid-saved-name-returns-to-picker ()
  "Invalid saved names send nothing and return control to the startup picker."
  (agent-shell-session-name-tests--with-shell
    (let (completed)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (caar choices)))
                ((symbol-function 'read-string) (lambda (&rest _) "   ")))
        (agent-shell-session-name--rename-saved
         '(((sessionId . "older") (title . "Initial preview")))
         (lambda () (setq completed t))))
      (should completed)
      (should-not request))))

(ert-deftest agent-shell-session-name-late-acknowledgement-ignores-replaced-session ()
  "A callback for an old client or session cannot rename its replacement."
  (dolist (replacement '(client session))
    (agent-shell-session-name-tests--with-shell
      (let ((original (buffer-name)))
        (agent-shell-rename-session "old-session-name")
        (if (eq replacement 'client)
            (map-put! agent-shell--state :client
                      (agent-shell-codex-app-server-make-client :command "sh" :context-buffer shell))
          (map-put! agent-shell--state :session (copy-tree (map-elt agent-shell--state :session))))
        (funcall (plist-get request :on-success) nil)
        (should (equal (buffer-name) original))
        (should-not (map-nested-elt agent-shell--state '(:session :title)))))))

(ert-deftest agent-shell-session-name-label-collisions-preserve-viewport-routing ()
  "Another buffer with the desired viewport name is never reused or renamed."
  (agent-shell-session-name-tests--with-shell
    (let ((viewport (generate-new-buffer (concat (buffer-name) agent-shell-viewport--suffix)))
          (unrelated (generate-new-buffer "Codex @ OppFinderRust / opp-ts [viewport]")))
      (unwind-protect
          (let ((unrelated-name (buffer-name unrelated)))
            (agent-shell-rename-session "opp-ts")
            (funcall (plist-get request :on-success) nil)
            (should (equal (buffer-name) "Codex @ OppFinderRust / opp-ts<2>"))
            (should (equal (buffer-name unrelated) unrelated-name))
            (should (eq shell (agent-shell-viewport--shell-buffer viewport))))
        (kill-buffer viewport)
        (kill-buffer unrelated)))))

(ert-deftest agent-shell-session-name-preserves-viewport-draft-and-routing ()
  "Renaming carries an existing viewport along without changing its draft."
  (agent-shell-session-name-tests--with-shell
    (let ((viewport (generate-new-buffer (concat (buffer-name) agent-shell-viewport--suffix))))
      (unwind-protect
          (progn
            (with-current-buffer viewport
              (setq major-mode 'agent-shell-viewport-edit-mode)
              (insert "Unsent viewport draft"))
            (agent-shell-rename-session "opp-ts")
            (funcall (plist-get request :on-success) nil)
            (should (eq viewport (agent-shell-viewport--buffer :shell-buffer shell :existing-only t)))
            (should (eq shell (agent-shell-viewport--shell-buffer viewport)))
            (with-current-buffer viewport
              (should (equal (buffer-string) "Unsent viewport draft"))))
        (kill-buffer viewport)))))

(provide 'agent-shell-session-name-tests)
;;; agent-shell-session-name-tests.el ends here
