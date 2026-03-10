;;; agent-shell-codex-app-server.el --- Codex app-server transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christian Smith

;; Author: Christian Smith
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Native Codex app-server transport.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'json)
(require 'map)
(require 'subr-x)
(require 'url-util)

(defconst agent-shell-codex-app-server--jsonrpc-version "2.0")

(defvar agent-shell--version)
(defvar agent-shell-codex-app-server--instance-count 0)

(defun agent-shell-codex-app-server--next-instance-count ()
  "Return the next unique client instance id."
  (setq agent-shell-codex-app-server--instance-count
        (1+ agent-shell-codex-app-server--instance-count)))

(cl-defun agent-shell-codex-app-server-make-client (&key command
                                                         command-params
                                                         environment-variables
                                                         context-buffer
                                                         approval-policy
                                                         sandbox-mode
                                                         persist-extended-history)
  "Create a Codex app-server client."
  (unless command
    (error ":command is required"))
  (unless (executable-find command)
    (error "\"%s\" command line utility not found. Please install it" command))
  (list (cons :backend 'codex-app-server)
        (cons :instance-count (agent-shell-codex-app-server--next-instance-count))
        (cons :process nil)
        (cons :stderr-buffer nil)
        (cons :command command)
        (cons :command-params command-params)
        (cons :environment-variables environment-variables)
        (cons :context-buffer context-buffer)
        (cons :partial-output "")
        (cons :request-id 0)
        (cons :pending-requests (make-hash-table :test #'equal))
        (cons :notification-handlers nil)
        (cons :request-handlers nil)
        (cons :error-handlers nil)
        (cons :pending-permissions (make-hash-table :test #'equal))
        (cons :tool-items (make-hash-table :test #'equal))
        (cons :tool-outputs (make-hash-table :test #'equal))
        (cons :thread-id nil)
        (cons :active-turn-id nil)
        (cons :current-model-id nil)
        (cons :available-models nil)
        (cons :reasoning-effort "medium")
        (cons :latest-token-usage nil)
        (cons :pending-prompt nil)
        (cons :approval-policy (or approval-policy "on-request"))
        (cons :sandbox-mode (or sandbox-mode "workspace-write"))
        (cons :persist-extended-history (if (null persist-extended-history) t
                                          persist-extended-history))
        (cons :shutting-down nil)))

(defun agent-shell-codex-app-server-client-p (client)
  "Return non-nil if CLIENT is a Codex app-server client."
  (eq (map-elt client :backend) 'codex-app-server))

(defun agent-shell-codex-app-server--client-started-p (client)
  "Return non-nil if CLIENT has a live process."
  (and (map-elt client :process)
       (process-live-p (map-elt client :process))))

(defun agent-shell-codex-app-server--decode-message (line)
  "Parse LINE as JSON-RPC data."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil))
    (json-read-from-string line)))

(defun agent-shell-codex-app-server--make-error (message &optional data)
  "Build an error object with MESSAGE and optional DATA."
  (append (list (cons 'message message))
          (when data
            (list (cons 'data data)))))

(defun agent-shell-codex-app-server--json-bool (value)
  "Return VALUE encoded as a JSON boolean."
  (if value t :json-false))

(defun agent-shell-codex-app-server--json-empty-object ()
  "Return an empty JSON object."
  (make-hash-table :test #'equal))

(defun agent-shell-codex-app-server--call-error-handlers (client message &optional data)
  "Forward MESSAGE and optional DATA to CLIENT error handlers."
  (dolist (handler (map-elt client :error-handlers))
    (funcall handler (agent-shell-codex-app-server--make-error message data))))

(defun agent-shell-codex-app-server--write-message (client payload)
  "Write JSON-RPC PAYLOAD to CLIENT."
  (unless (agent-shell-codex-app-server--client-started-p client)
    (error "Codex app-server process is not running"))
  (process-send-string
   (map-elt client :process)
   (concat (json-encode payload) "\n")))

(defun agent-shell-codex-app-server--reject-pending-requests (client message)
  "Reject all pending requests for CLIENT with MESSAGE."
  (let ((pending-requests (map-elt client :pending-requests)))
    (maphash
     (lambda (_id pending)
       (when-let ((on-failure (map-elt pending :on-failure)))
         (funcall on-failure
                  (agent-shell-codex-app-server--make-error message)
                  nil)))
     pending-requests)
    (clrhash pending-requests)))

(defun agent-shell-codex-app-server--make-stderr-buffer-name (client)
  "Return a stderr buffer name for CLIENT."
  (format "*agent-shell-codex-app-server-stderr-%s*"
          (map-elt client :instance-count)))

(defun agent-shell-codex-app-server--start-client (client)
  "Start CLIENT."
  (unless (map-elt client :command)
    (error "Client missing :command"))
  (unless (executable-find (map-elt client :command))
    (error "\"%s\" command line utility not found. Please install it"
           (map-elt client :command)))
  (when (agent-shell-codex-app-server--client-started-p client)
    (error "Client already started"))
  (let* ((process-environment (append (map-elt client :environment-variables)
                                      process-environment))
         (stderr-buffer (get-buffer-create
                         (agent-shell-codex-app-server--make-stderr-buffer-name client)))
         (process
          (make-process
           :name (format "agent-shell-codex-app-server-%s"
                         (map-elt client :instance-count))
           :command (cons (map-elt client :command)
                          (map-elt client :command-params))
           :buffer nil
           :coding 'utf-8-unix
           :connection-type 'pipe
           :stderr stderr-buffer
           :filter (lambda (_process output)
                     (agent-shell-codex-app-server--process-filter client output))
           :sentinel (lambda (_process event)
                       (agent-shell-codex-app-server--process-sentinel client event)))))
    (set-process-query-on-exit-flag process nil)
    (map-put! client :stderr-buffer stderr-buffer)
    (map-put! client :process process)
    client))

(defun agent-shell-codex-app-server--ensure-started (client)
  "Start CLIENT if needed, then return it."
  (unless (agent-shell-codex-app-server--client-started-p client)
    (agent-shell-codex-app-server--start-client client))
  client)

(defun agent-shell-codex-app-server--dispatch-notification (client notification)
  "Dispatch translated NOTIFICATION to CLIENT handlers."
  (dolist (handler (map-elt client :notification-handlers))
    (funcall handler notification)))

(defun agent-shell-codex-app-server--dispatch-request (client request)
  "Dispatch translated REQUEST to CLIENT handlers."
  (dolist (handler (map-elt client :request-handlers))
    (funcall handler request)))

(defun agent-shell-codex-app-server--next-request-id (client)
  "Increment and return CLIENT request id."
  (let ((next-id (1+ (map-elt client :request-id))))
    (map-put! client :request-id next-id)
    next-id))

(cl-defun agent-shell-codex-app-server--send-rpc-request (&key client
                                                               method
                                                               params
                                                               on-success
                                                               on-failure)
  "Send raw JSON-RPC METHOD with PARAMS via CLIENT."
  (agent-shell-codex-app-server--ensure-started client)
  (let* ((id (agent-shell-codex-app-server--next-request-id client))
         (pending `((:method . ,method)
                    (:on-success . ,on-success)
                    (:on-failure . ,on-failure))))
    (puthash id pending (map-elt client :pending-requests))
    (agent-shell-codex-app-server--write-message
     client
     `((jsonrpc . ,agent-shell-codex-app-server--jsonrpc-version)
       (id . ,id)
       (method . ,method)
       (params . ,(or params '()))))))

(cl-defun agent-shell-codex-app-server--send-rpc-response (&key client
                                                                request-id
                                                                result)
  "Send a JSON-RPC response for REQUEST-ID with RESULT via CLIENT."
  (agent-shell-codex-app-server--ensure-started client)
  (agent-shell-codex-app-server--write-message
   client
   `((jsonrpc . ,agent-shell-codex-app-server--jsonrpc-version)
     (id . ,request-id)
     (result . ,result))))

(cl-defun agent-shell-codex-app-server--send-rpc-notification (&key client
                                                                    method
                                                                    params)
  "Send a JSON-RPC notification METHOD with PARAMS via CLIENT."
  (agent-shell-codex-app-server--ensure-started client)
  (agent-shell-codex-app-server--write-message
   client
   `((jsonrpc . ,agent-shell-codex-app-server--jsonrpc-version)
     (method . ,method)
     (params . ,(or params '())))))

(defun agent-shell-codex-app-server--normalize-status (status)
  "Translate app-server STATUS into an agent-shell status string."
  (pcase status
    ("inProgress" "in_progress")
    ("completed" "completed")
    ("failed" "failed")
    ("declined" "failed")
    (_ (or status "in_progress"))))

(defun agent-shell-codex-app-server--normalize-stop-reason (turn)
  "Translate TURN status to an ACP-like stop reason."
  (pcase (map-elt turn 'status)
    ("completed" "end_turn")
    ("failed"
     (if (string-match-p "cancel" (or (map-nested-elt turn '(error message)) ""))
         "cancelled"
       "refusal"))
    (_ "cancelled")))

(defun agent-shell-codex-app-server--format-timestamp (seconds)
  "Format unix SECONDS as ISO-8601 UTC."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (seconds-to-time seconds)
                      t))

(defun agent-shell-codex-app-server--file-uri-to-path (uri)
  "Turn file URI into a local path."
  (when (and (stringp uri)
             (string-prefix-p "file://" uri))
    (url-unhex-string (string-remove-prefix "file://" uri))))

(defun agent-shell-codex-app-server--translate-models (models)
  "Translate MODELS to the shape used by agent-shell."
  (mapcar (lambda (model)
            `((modelId . ,(or (map-elt model 'model)
                              (map-elt model 'id)))
              (name . ,(or (map-elt model 'displayName)
                           (map-elt model 'model)
                           (map-elt model 'id)))
              (description . ,(map-elt model 'description))))
          models))

(defun agent-shell-codex-app-server--session-response (client result)
  "Translate RESULT into an ACP-like session response."
  (let* ((thread (map-elt result 'thread))
         (thread-id (map-elt thread 'id))
         (model-id (or (map-elt result 'model)
                       (map-elt client :current-model-id)
                       (map-elt (seq-find (lambda (model)
                                            (map-elt model 'isDefault))
                                          (map-elt client :available-models))
                                'model))))
    (map-put! client :thread-id thread-id)
    (map-put! client :current-model-id model-id)
    (map-put! client :reasoning-effort (or (map-elt result 'reasoningEffort)
                                           (map-elt client :reasoning-effort)))
    `((sessionId . ,thread-id)
      (models . ((currentModelId . ,model-id)
                 (availableModels . ,(agent-shell-codex-app-server--translate-models
                                      (or (map-elt client :available-models) '()))))))))

(defun agent-shell-codex-app-server--session-list-response (result)
  "Translate thread/list RESULT into an ACP-like session list response."
  `((sessions . ,(mapcar (lambda (thread)
                           `((sessionId . ,(map-elt thread 'id))
                             (title . ,(or (map-elt thread 'name)
                                           (map-elt thread 'preview)
                                           "Untitled"))
                             (cwd . ,(map-elt thread 'cwd))
                             (createdAt . ,(agent-shell-codex-app-server--format-timestamp
                                            (map-elt thread 'createdAt)))
                             (updatedAt . ,(agent-shell-codex-app-server--format-timestamp
                                            (map-elt thread 'updatedAt)))))
                         (or (map-elt result 'data) '())))
      (nextCursor . ,(map-elt result 'nextCursor))))

(defun agent-shell-codex-app-server--extract-first-file-change (changes)
  "Extract the first relevant change from CHANGES."
  (let ((change (car (or changes '()))))
    (when change
      (let ((diff (map-elt change 'diff))
            (path (map-elt change 'path)))
        (append (list (cons 'path path))
                (when diff
                  (list (cons 'diff diff))))))))

(defun agent-shell-codex-app-server--tool-entry-from-item (item)
  "Build a normalized tool entry from ITEM."
  (pcase (map-elt item 'type)
    ("commandExecution"
     `((:title . ,(or (map-elt item 'command) "Run command"))
       (:kind . "execute")
       (:command . ,(map-elt item 'command))
       (:description . ,(map-elt item 'cwd))
       (:raw-input . ((command . ,(map-elt item 'command))
                      (description . ,(map-elt item 'cwd))))))
    ("fileChange"
     (let* ((change (agent-shell-codex-app-server--extract-first-file-change
                     (map-elt item 'changes)))
            (path (map-elt change 'path)))
       `((:title . ,(or path "File change"))
         (:kind . "edit")
         (:description . ,path)
         (:raw-input . ,change))))
    ("mcpToolCall"
     `((:title . ,(format "%s/%s"
                          (or (map-elt item 'server) "mcp")
                          (or (map-elt item 'tool) "tool")))
       (:kind . "tool")
       (:description . ,(map-elt item 'tool))
       (:raw-input . ((arguments . ,(map-elt item 'arguments))))))
    ("dynamicToolCall"
     `((:title . ,(or (map-elt item 'tool) "Tool"))
       (:kind . "tool")
       (:description . ,(map-elt item 'tool))
       (:raw-input . ((arguments . ,(map-elt item 'arguments))))))
    ("webSearch"
     `((:title . ,(or (map-elt item 'query) "Web search"))
       (:kind . "search")
       (:description . ,(map-elt item 'query))
       (:raw-input . ((query . ,(map-elt item 'query))))))
    (_
     `((:title . ,(or (map-elt item 'type) "Tool"))
       (:kind . "tool")
       (:raw-input . ())))))

(defun agent-shell-codex-app-server--tool-content (client item)
  "Build tool-call content for ITEM using CLIENT state."
  (let* ((item-id (map-elt item 'id))
         (output (or (gethash item-id (map-elt client :tool-outputs))
                     (map-elt item 'aggregatedOutput))))
    (append (when (and output (not (string-empty-p output)))
              (list `((content . ((text . ,output))))))
            (when (and (equal (map-elt item 'type) "fileChange")
                       (map-elt item 'changes))
              (let* ((change (car (map-elt item 'changes)))
                     (diff (map-elt change 'diff))
                     (path (map-elt change 'path)))
                (when (and diff path)
                  (let ((parsed (agent-shell-codex-app-server--parse-unified-diff diff)))
                    (list `((type . "diff")
                            (path . ,path)
                            (oldText . ,(car parsed))
                            (newText . ,(cdr parsed)))))))))))

(defun agent-shell-codex-app-server--parse-unified-diff (diff-string)
  "Parse unified DIFF-STRING into old and new text."
  (let (old-lines new-lines in-hunk)
    (dolist (line (split-string (or diff-string "") "\n"))
      (cond
       ((string-match "^@@.*@@" line)
        (setq in-hunk t))
       ((and in-hunk (string-prefix-p " " line))
        (push (substring line 1) old-lines)
        (push (substring line 1) new-lines))
       ((and in-hunk (string-prefix-p "-" line))
        (push (substring line 1) old-lines))
       ((and in-hunk (string-prefix-p "+" line))
        (push (substring line 1) new-lines))))
    (cons (string-join (nreverse old-lines) "\n")
          (string-join (nreverse new-lines) "\n"))))

(defun agent-shell-codex-app-server--save-tool-entry (client item status)
  "Store ITEM metadata with STATUS in CLIENT."
  (let* ((item-id (map-elt item 'id))
         (entry (agent-shell-codex-app-server--tool-entry-from-item item)))
    (map-put! entry :status (agent-shell-codex-app-server--normalize-status status))
    (puthash item-id entry (map-elt client :tool-items))
    entry))

(defun agent-shell-codex-app-server--get-tool-entry (client item-id)
  "Return stored tool entry for ITEM-ID."
  (gethash item-id (map-elt client :tool-items)))

(defun agent-shell-codex-app-server--translate-tool-notification (session-update
                                                                  client
                                                                  item
                                                                  status)
  "Translate ITEM and STATUS into an ACP-like tool notification."
  (let* ((item-id (map-elt item 'id))
         (entry (or (agent-shell-codex-app-server--get-tool-entry client item-id)
                    (agent-shell-codex-app-server--save-tool-entry client item status))))
    (map-put! entry :status (agent-shell-codex-app-server--normalize-status status))
    (puthash item-id entry (map-elt client :tool-items))
    `((method . "session/update")
      (params . ((update . ((sessionUpdate . ,session-update)
                            (toolCallId . ,item-id)
                            (title . ,(map-elt entry :title))
                            (status . ,(map-elt entry :status))
                            (kind . ,(map-elt entry :kind))
                            (rawInput . ,(or (map-elt entry :raw-input) '()))
                            (content . ,(agent-shell-codex-app-server--tool-content client item)))))))))

(defun agent-shell-codex-app-server--translate-command-output (client params)
  "Translate command output PARAMS to a tool_call_update."
  (let* ((item-id (map-elt params 'itemId))
         (delta (or (map-elt params 'delta) ""))
         (outputs (map-elt client :tool-outputs))
         (current (gethash item-id outputs "")))
    (puthash item-id (concat current delta) outputs)
    (when-let ((entry (agent-shell-codex-app-server--get-tool-entry client item-id)))
      `((method . "session/update")
        (params . ((update . ((sessionUpdate . "tool_call_update")
                              (toolCallId . ,item-id)
                              (title . ,(map-elt entry :title))
                              (status . ,(map-elt entry :status))
                              (kind . ,(map-elt entry :kind))
                              (rawInput . ,(or (map-elt entry :raw-input) '()))
                              (content . (((content . ((text . ,(gethash item-id outputs)))))))))))))))

(defun agent-shell-codex-app-server--usage-notification (token-usage)
  "Translate TOKEN-USAGE to an ACP-like usage update."
  (when token-usage
    `((method . "session/update")
      (params . ((update . ((sessionUpdate . "usage_update")
                            (used . ,(map-nested-elt token-usage '(total totalTokens)))
                            (size . ,(map-elt token-usage 'modelContextWindow)))))))))

(defun agent-shell-codex-app-server--prompt-response (client turn)
  "Build an ACP-like prompt response for TURN using CLIENT."
  `((stopReason . ,(agent-shell-codex-app-server--normalize-stop-reason turn))
    (usage . ((totalTokens . ,(map-nested-elt (map-elt client :latest-token-usage) '(total totalTokens)))
              (inputTokens . ,(map-nested-elt (map-elt client :latest-token-usage) '(total inputTokens)))
              (outputTokens . ,(map-nested-elt (map-elt client :latest-token-usage) '(total outputTokens)))
              (thoughtTokens . ,(map-nested-elt (map-elt client :latest-token-usage) '(total reasoningOutputTokens)))
              (cachedReadTokens . ,(map-nested-elt (map-elt client :latest-token-usage) '(total cachedInputTokens)))))))

(defun agent-shell-codex-app-server--translate-prompt-block (block)
  "Translate a single ACP content BLOCK into a Codex user input item."
  (pcase (map-elt block 'type)
    ("text"
     `((type . "text")
       (text . ,(or (map-elt block 'text) ""))
       (text_elements . [])))
    ("image"
     (if-let ((path (agent-shell-codex-app-server--file-uri-to-path
                     (map-elt block 'uri))))
         `((type . "localImage")
           (path . ,path))
       `((type . "image")
         (url . ,(map-elt block 'uri)))))
    ("resource"
     (if-let* ((resource (map-elt block 'resource))
               (uri (map-elt resource 'uri))
               (path (agent-shell-codex-app-server--file-uri-to-path uri)))
         `((type . "mention")
           (name . ,(file-name-nondirectory path))
           (path . ,path))
       `((type . "text")
         (text . "")
         (text_elements . []))))
    ("resource_link"
     (if-let ((path (agent-shell-codex-app-server--file-uri-to-path
                     (map-elt block 'uri))))
         `((type . "mention")
           (name . ,(or (map-elt block 'name)
                        (file-name-nondirectory path)))
           (path . ,path))
       `((type . "text")
         (text . "")
         (text_elements . []))))
    (_
     `((type . "text")
       (text . "")
       (text_elements . [])))))

(defun agent-shell-codex-app-server--translate-prompt-blocks (prompt-blocks)
  "Translate ACP PROMPT-BLOCKS to Codex app-server user input."
  (mapcar #'agent-shell-codex-app-server--translate-prompt-block
          (append prompt-blocks nil)))

(defun agent-shell-codex-app-server--request-options (method params)
  "Translate app-server METHOD and PARAMS into ACP permission options."
  (let ((decisions
         (or (map-elt params 'availableDecisions)
             (pcase method
               ((or "item/commandExecution/requestApproval"
                    "item/fileChange/requestApproval")
                '("accept" "acceptForSession" "decline" "cancel"))
               ("execCommandApproval"
                '("approved" "approved_for_session" "denied" "abort"))
               ("applyPatchApproval"
                '("approved" "approved_for_session" "denied" "abort"))
               ("item/permissions/requestApproval"
                '("grant" "cancel"))
               (_ nil)))))
    (delq nil
          (mapcar (lambda (decision)
                    (pcase decision
                      ((or "accept" "approved")
                       '((kind . "allow_once")
                         (name . "Allow")
                         (optionId . "accept")))
                      ((or "acceptForSession" "approved_for_session")
                       '((kind . "allow_always")
                         (name . "Always Allow")
                         (optionId . "acceptForSession")))
                      ((or "decline" "denied")
                       '((kind . "reject_once")
                         (name . "Reject")
                         (optionId . "decline")))
                      ((or "cancel" "abort")
                       '((kind . "reject_once")
                         (name . "Reject")
                         (optionId . "cancel")))
                      ("grant"
                       '((kind . "allow_once")
                         (name . "Allow")
                         (optionId . "grant")))
                      (_ nil)))
                  decisions))))

(defun agent-shell-codex-app-server--approval-title (method params)
  "Build a permission title for METHOD with PARAMS."
  (pcase method
    ((or "item/commandExecution/requestApproval" "execCommandApproval")
     (or (map-elt params 'command)
         (and (listp (map-elt params 'command))
              (string-join (map-elt params 'command) " "))
         (map-elt params 'reason)
         "Run command"))
    ((or "item/fileChange/requestApproval" "applyPatchApproval")
     (or (map-elt params 'reason)
         (map-elt params 'grantRoot)
         "Apply patch"))
    ("item/permissions/requestApproval"
     (or (map-elt params 'reason)
         "Grant additional permissions"))
    (_ (or (map-elt params 'reason)
           method))))

(defun agent-shell-codex-app-server--approval-kind (method)
  "Return an ACP-like tool kind for app-server METHOD."
  (pcase method
    ((or "item/commandExecution/requestApproval" "execCommandApproval") "execute")
    ((or "item/fileChange/requestApproval" "applyPatchApproval") "edit")
    (_ "tool")))

(defun agent-shell-codex-app-server--approval-raw-input (method params)
  "Build ACP-like raw input for METHOD with PARAMS."
  (pcase method
    ((or "item/commandExecution/requestApproval" "execCommandApproval")
     `((command . ,(or (map-elt params 'command)
                       (and (listp (map-elt params 'command))
                            (string-join (map-elt params 'command) " "))))
       (description . ,(map-elt params 'reason))))
    ((or "item/fileChange/requestApproval" "applyPatchApproval")
     (let ((file-changes (map-elt params 'fileChanges)))
       (cond
        ((and (hash-table-p file-changes) (> (hash-table-count file-changes) 0))
         (let (first)
           (maphash (lambda (path change)
                      (unless first
                        (setq first
                              `((path . ,path)
                                (diff . ,(map-elt change 'unified_diff)))))
                      nil)
                    file-changes)
           first))
        ((map-elt params 'grantRoot)
         `((path . ,(map-elt params 'grantRoot))))
        (t nil))))
    (_ nil)))

(defun agent-shell-codex-app-server--translate-request (client request)
  "Translate app-server REQUEST for CLIENT."
  (let* ((method (map-elt request 'method))
         (params (or (map-elt request 'params) '()))
         (request-id (map-elt request 'id))
         (tool-call-id (or (map-elt params 'itemId)
                           (map-elt params 'callId)
                           request-id))
         (translated
          `((id . ,request-id)
            (method . "session/request_permission")
            (params . ((toolCall . ((toolCallId . ,tool-call-id)
                                    (title . ,(agent-shell-codex-app-server--approval-title method params))
                                    (status . "in_progress")
                                    (kind . ,(agent-shell-codex-app-server--approval-kind method))
                                    (rawInput . ,(or (agent-shell-codex-app-server--approval-raw-input method params)
                                                     '()))))
                       (options . ,(agent-shell-codex-app-server--request-options method params)))))))
    (puthash request-id request (map-elt client :pending-permissions))
    translated))

(defun agent-shell-codex-app-server--respond-to-pending-prompt (client turn)
  "Resolve the active prompt in CLIENT using TURN."
  (when-let ((pending (map-elt client :pending-prompt)))
    (map-put! client :pending-prompt nil)
    (map-put! client :active-turn-id nil)
    (when-let ((on-success (map-elt pending :on-success)))
      (funcall on-success
               (agent-shell-codex-app-server--prompt-response client turn)))))

(defun agent-shell-codex-app-server--handle-notification (client notification)
  "Handle app-server NOTIFICATION for CLIENT."
  (pcase (map-elt notification 'method)
    ("error"
     (let* ((params (or (map-elt notification 'params) '()))
            (message (or (map-elt params 'message)
                         (map-elt params 'error)
                         "Codex app-server error")))
       (agent-shell-codex-app-server--call-error-handlers client message params)))
    ("thread/started"
     (map-put! client :thread-id
               (or (map-nested-elt notification '(params thread id))
                   (map-nested-elt notification '(params threadId)))))
    ("turn/started"
     (map-put! client :active-turn-id
               (map-nested-elt notification '(params turn id))))
    ("thread/tokenUsage/updated"
     (let ((token-usage (map-nested-elt notification '(params tokenUsage))))
       (map-put! client :latest-token-usage token-usage)
       (when-let ((translated
                   (agent-shell-codex-app-server--usage-notification token-usage)))
         (agent-shell-codex-app-server--dispatch-notification client translated))))
    ("turn/plan/updated"
     (agent-shell-codex-app-server--dispatch-notification
      client
      `((method . "session/update")
        (params . ((update . ((sessionUpdate . "plan")
                              (entries . ,(or (map-nested-elt notification '(params entries))
                                              (map-nested-elt notification '(params plan entries))
                                              (map-nested-elt notification '(params plan))
                                              '())))))))))
    ("item/agentMessage/delta"
     (agent-shell-codex-app-server--dispatch-notification
      client
      `((method . "session/update")
        (params . ((update . ((sessionUpdate . "agent_message_chunk")
                              (content . ((text . ,(or (map-nested-elt notification '(params delta)) "")))))))))))
    ("item/reasoning/textDelta"
     (agent-shell-codex-app-server--dispatch-notification
      client
      `((method . "session/update")
        (params . ((update . ((sessionUpdate . "agent_thought_chunk")
                              (content . ((text . ,(or (map-nested-elt notification '(params delta)) "")))))))))))
    ("item/reasoning/summaryTextDelta"
     (agent-shell-codex-app-server--dispatch-notification
      client
      `((method . "session/update")
        (params . ((update . ((sessionUpdate . "agent_thought_chunk")
                              (content . ((text . ,(or (map-nested-elt notification '(params delta)) "")))))))))))
    ("item/started"
     (let ((item (map-nested-elt notification '(params item))))
       (when (member (map-elt item 'type)
                     '("commandExecution" "fileChange" "mcpToolCall" "dynamicToolCall" "webSearch"))
         (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
         (agent-shell-codex-app-server--dispatch-notification
          client
          (agent-shell-codex-app-server--translate-tool-notification
           "tool_call" client item "inProgress")))))
    ("item/commandExecution/outputDelta"
     (when-let ((translated
                 (agent-shell-codex-app-server--translate-command-output
                  client (map-elt notification 'params))))
       (agent-shell-codex-app-server--dispatch-notification client translated)))
    ("item/fileChange/outputDelta"
     (when-let ((translated
                 (agent-shell-codex-app-server--translate-command-output
                  client (map-elt notification 'params))))
       (agent-shell-codex-app-server--dispatch-notification client translated)))
    ("item/completed"
     (let ((item (map-nested-elt notification '(params item))))
       (when (member (map-elt item 'type)
                     '("commandExecution" "fileChange" "mcpToolCall" "dynamicToolCall" "webSearch"))
         (when-let ((output (map-elt item 'aggregatedOutput)))
           (puthash (map-elt item 'id) output (map-elt client :tool-outputs)))
         (agent-shell-codex-app-server--dispatch-notification
          client
          (agent-shell-codex-app-server--translate-tool-notification
           "tool_call_update"
           client
           item
           (or (map-elt item 'status) "completed"))))))
    ("turn/completed"
     (agent-shell-codex-app-server--respond-to-pending-prompt
      client
      (map-nested-elt notification '(params turn))))
    (_ nil)))

(defun agent-shell-codex-app-server--handle-response (client response)
  "Handle raw JSON-RPC RESPONSE for CLIENT."
  (let* ((id (map-elt response 'id))
         (pending (gethash id (map-elt client :pending-requests))))
    (when pending
      (remhash id (map-elt client :pending-requests))
      (if-let ((error (map-elt response 'error)))
          (when-let ((on-failure (map-elt pending :on-failure)))
            (funcall on-failure
                     (agent-shell-codex-app-server--make-error
                      (or (map-elt error 'message)
                          "Codex app-server request failed")
                      error)
                     response))
        (when-let ((on-success (map-elt pending :on-success)))
          (funcall on-success (map-elt response 'result)))))))

(defun agent-shell-codex-app-server--route-message (client message)
  "Route decoded MESSAGE for CLIENT."
  (cond
   ((and (map-contains-key message 'method)
         (map-contains-key message 'id))
    (agent-shell-codex-app-server--dispatch-request
     client
     (agent-shell-codex-app-server--translate-request client message)))
   ((map-contains-key message 'method)
    (agent-shell-codex-app-server--handle-notification client message))
   ((map-contains-key message 'id)
    (agent-shell-codex-app-server--handle-response client message))
   (t
    (agent-shell-codex-app-server--call-error-handlers
     client
     "Received malformed JSON-RPC payload"
     message))))

(defun agent-shell-codex-app-server--process-filter (client output)
  "Handle process OUTPUT for CLIENT."
  (let ((pending (concat (map-elt client :partial-output) output)))
    (while (string-match "\n" pending)
      (let* ((line (substring pending 0 (match-beginning 0)))
             (rest (substring pending (match-end 0))))
        (setq pending rest)
        (unless (string-empty-p (string-trim line))
          (condition-case err
              (agent-shell-codex-app-server--route-message
               client
               (agent-shell-codex-app-server--decode-message line))
            (error
             (agent-shell-codex-app-server--call-error-handlers
              client
              (format "Failed to decode app-server payload: %s"
                      (error-message-string err))
              line))))))
    (map-put! client :partial-output pending)))

(defun agent-shell-codex-app-server--process-sentinel (client event)
  "Handle CLIENT process EVENT."
  (unless (process-live-p (map-elt client :process))
    (when-let ((pending (map-elt client :pending-prompt))
               (on-failure (map-elt pending :on-failure)))
      (funcall on-failure
               (agent-shell-codex-app-server--make-error
                (format "Codex app-server exited: %s" (string-trim event)))
               nil))
    (map-put! client :pending-prompt nil)
    (map-put! client :active-turn-id nil)
    (unless (map-elt client :shutting-down)
      (agent-shell-codex-app-server--reject-pending-requests
       client
       (format "Codex app-server exited: %s" (string-trim event)))
      (agent-shell-codex-app-server--call-error-handlers
       client
       (format "Codex app-server exited: %s" (string-trim event))))))

(cl-defun agent-shell-codex-app-server-subscribe-to-errors (&key client on-error _buffer)
  "Subscribe CLIENT to errors using ON-ERROR."
  (unless on-error
    (error ":on-error is required"))
  (push on-error (alist-get :error-handlers client))
  on-error)

(cl-defun agent-shell-codex-app-server-subscribe-to-notifications (&key client on-notification _buffer)
  "Subscribe CLIENT to translated notifications using ON-NOTIFICATION."
  (unless on-notification
    (error ":on-notification is required"))
  (push on-notification (alist-get :notification-handlers client))
  on-notification)

(cl-defun agent-shell-codex-app-server-subscribe-to-requests (&key client on-request _buffer)
  "Subscribe CLIENT to translated requests using ON-REQUEST."
  (unless on-request
    (error ":on-request is required"))
  (push on-request (alist-get :request-handlers client))
  on-request)

(defun agent-shell-codex-app-server--fetch-models (client on-success)
  "Refresh model metadata for CLIENT, then call ON-SUCCESS."
  (agent-shell-codex-app-server--send-rpc-request
   :client client
   :method "model/list"
   :params (agent-shell-codex-app-server--json-empty-object)
   :on-success (lambda (result)
                 (map-put! client :available-models (or (map-elt result 'data) '()))
                 (when on-success
                   (funcall on-success)))
   :on-failure (lambda (_error _raw)
                 (map-put! client :available-models '())
                 (when on-success
                   (funcall on-success)))))

(defun agent-shell-codex-app-server--thread-params (client cwd)
  "Return common thread parameters for CLIENT using CWD."
  `((cwd . ,cwd)
    (approvalPolicy . ,(map-elt client :approval-policy))
    (sandbox . ,(map-elt client :sandbox-mode))
    (experimentalRawEvents . ,(agent-shell-codex-app-server--json-bool nil))
    (persistExtendedHistory . ,(agent-shell-codex-app-server--json-bool
                                (map-elt client :persist-extended-history)))
    ,@(when-let ((model-id (map-elt client :current-model-id)))
        (list (cons 'model model-id)))))

(cl-defun agent-shell-codex-app-server-send-request (&key client
                                                       request
                                                       buffer
                                                       on-success
                                                       on-failure
                                                       sync)
  "Send translated ACP REQUEST through app-server CLIENT."
  (ignore buffer)
  (when sync
    (error "Synchronous requests are not supported by codex app-server transport"))
  (let* ((method (map-elt request :method))
         (params (or (map-elt request :params) '())))
    (pcase method
      ("initialize"
       (agent-shell-codex-app-server--send-rpc-request
        :client client
        :method "initialize"
        :params `((clientInfo . ((name . "agent-shell")
                                 (title . "Emacs Agent Shell")
                                 (version . ,agent-shell--version)))
                  (capabilities . ((experimentalApi . t))))
        :on-success (lambda (_result)
                      (agent-shell-codex-app-server--send-rpc-notification
                       :client client
                       :method "initialized"
                       :params nil)
                      (when on-success
                        (funcall on-success
                                 '((sessionCapabilities . ((list . t)
                                                          (resume . t)))
                                   (agentCapabilities . ((promptCapabilities . ((image . t)
                                                                                (embeddedContext . nil)))))))))
        :on-failure on-failure))
      ("authenticate"
       (when on-success
         (funcall on-success '())))
      ("session/new"
       (agent-shell-codex-app-server--fetch-models
        client
        (lambda ()
          (agent-shell-codex-app-server--send-rpc-request
           :client client
           :method "thread/start"
           :params (agent-shell-codex-app-server--thread-params
                    client
                    (map-elt params 'cwd))
           :on-success (lambda (result)
                         (when on-success
                           (funcall on-success
                                    (agent-shell-codex-app-server--session-response
                                     client result))))
           :on-failure on-failure))))
      ("session/list"
       (agent-shell-codex-app-server--send-rpc-request
        :client client
        :method "thread/list"
        :params `((cwd . ,(map-elt params 'cwd))
                  (archived . ,(agent-shell-codex-app-server--json-bool nil))
                  (sortKey . "updated_at")
                  (limit . 25))
        :on-success (lambda (result)
                      (when on-success
                        (funcall on-success
                                 (agent-shell-codex-app-server--session-list-response
                                  result))))
        :on-failure on-failure))
      ((or "session/resume" "session/load")
       (agent-shell-codex-app-server--fetch-models
        client
        (lambda ()
          (agent-shell-codex-app-server--send-rpc-request
           :client client
           :method "thread/resume"
           :params (append (list (cons 'threadId (map-elt params 'sessionId)))
                           (agent-shell-codex-app-server--thread-params
                            client
                            (map-elt params 'cwd)))
           :on-success (lambda (result)
                         (when on-success
                           (funcall on-success
                                    (agent-shell-codex-app-server--session-response
                                     client result))))
           :on-failure on-failure))))
      ("session/set_model"
       (map-put! client :current-model-id (map-elt params 'modelId))
       (when on-success
         (funcall on-success
                  `((modelId . ,(map-elt params 'modelId))))))
      ("session/set_mode"
       (if on-failure
           (funcall on-failure
                    (agent-shell-codex-app-server--make-error
                     "Codex app-server does not expose session modes")
                    nil)
         (agent-shell-codex-app-server--call-error-handlers
          client
          "Codex app-server does not expose session modes")))
      ("session/prompt"
       (if (map-elt client :pending-prompt)
           (if on-failure
               (funcall on-failure
                        (agent-shell-codex-app-server--make-error
                         "A prompt is already in progress")
                        nil)
             (agent-shell-codex-app-server--call-error-handlers
              client
              "A prompt is already in progress"))
         (agent-shell-codex-app-server--send-rpc-request
          :client client
          :method "turn/start"
          :params `((threadId . ,(or (map-elt client :thread-id)
                                     (map-elt params 'sessionId)))
                    (input . ,(agent-shell-codex-app-server--translate-prompt-blocks
                               (map-elt params 'prompt)))
                    ,@(when-let ((model-id (map-elt client :current-model-id)))
                        (list (cons 'model model-id)))
                    ,@(when-let ((effort (map-elt client :reasoning-effort)))
                        (list (cons 'effort effort))))
          :on-success (lambda (result)
                        (map-put! client :active-turn-id
                                  (map-nested-elt result '(turn id)))
                        (map-put! client :pending-prompt
                                  `((:turn-id . ,(map-nested-elt result '(turn id)))
                                    (:on-success . ,on-success)
                                    (:on-failure . ,on-failure))))
          :on-failure on-failure)))
      (_
       (if on-failure
           (funcall on-failure
                    (agent-shell-codex-app-server--make-error
                     (format "Unsupported ACP method for Codex app-server: %s" method))
                    nil)
         (agent-shell-codex-app-server--call-error-handlers
          client
          (format "Unsupported ACP method for Codex app-server: %s" method)))))))

(defun agent-shell-codex-app-server--grant-permissions (permissions)
  "Convert requested PERMISSIONS to a granted permission payload."
  `((network . ,(map-elt permissions 'network))
    (fileSystem . ,(map-elt permissions 'fileSystem))
    (macos . ,(map-elt permissions 'macos))))

(cl-defun agent-shell-codex-app-server-send-permission-response (&key client
                                                                   request-id
                                                                   option-id
                                                                   cancelled)
  "Respond to a pending app-server permission request."
  (let ((request (gethash request-id (map-elt client :pending-permissions))))
    (unless request
      (error "Unknown pending permission request: %s" request-id))
    (remhash request-id (map-elt client :pending-permissions))
    (let* ((method (map-elt request 'method))
           (params (or (map-elt request 'params) '()))
           (decision
            (cond
             (cancelled "cancel")
             ((equal option-id "cancel") "cancel")
             ((equal option-id "decline") "decline")
             ((equal option-id "grant") "grant")
             ((equal option-id "acceptForSession") "acceptForSession")
             (t "accept")))
           (result
            (pcase method
              ("item/commandExecution/requestApproval"
               `((decision . ,decision)))
              ("item/fileChange/requestApproval"
               `((decision . ,decision)))
              ("item/permissions/requestApproval"
               `((permissions . ,(if (member decision '("accept" "grant" "acceptForSession"))
                                     (agent-shell-codex-app-server--grant-permissions
                                      (map-elt params 'permissions))
                                   '()))))
              ("execCommandApproval"
               `((decision . ,(pcase decision
                                ("accept" "approved")
                                ("acceptForSession" "approved_for_session")
                                ("decline" "denied")
                                (_ "abort")))))
              ("applyPatchApproval"
               `((decision . ,(pcase decision
                                ("accept" "approved")
                                ("acceptForSession" "approved_for_session")
                                ("decline" "denied")
                                (_ "abort")))))
              (_
               `((decision . ,decision))))))
      (agent-shell-codex-app-server--send-rpc-response
       :client client
       :request-id request-id
       :result result))))

(defun agent-shell-codex-app-server-interrupt (client)
  "Interrupt the current turn for CLIENT."
  (when (and (agent-shell-codex-app-server--client-started-p client)
             (map-elt client :thread-id)
             (map-elt client :active-turn-id))
    (agent-shell-codex-app-server--send-rpc-request
     :client client
     :method "turn/interrupt"
     :params `((threadId . ,(map-elt client :thread-id))
               (turnId . ,(map-elt client :active-turn-id)))
     :on-success (lambda (_result) nil)
     :on-failure (lambda (_error _raw) nil))))

(cl-defun agent-shell-codex-app-server-shutdown (&key client)
  "Shut down CLIENT."
  (when client
    (map-put! client :shutting-down t)
    (map-put! client :pending-prompt nil)
    (map-put! client :active-turn-id nil)
    (agent-shell-codex-app-server--reject-pending-requests
     client
     "Codex app-server shut down")
    (when-let ((process (map-elt client :process)))
      (when (process-live-p process)
        (delete-process process))
      (map-put! client :process nil))))

(provide 'agent-shell-codex-app-server)

;;; agent-shell-codex-app-server.el ends here
