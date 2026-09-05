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
(require 'acp)
(require 'button)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(defconst agent-shell-codex-app-server--jsonrpc-version "2.0")
(defconst agent-shell-codex-app-server--client-name "agent-shell"
  "Client name reported to Codex during app-server initialization.

Keep this distinct from first-party Codex clients so app-server usage
and compliance logs identify Agent Shell separately.")

(defconst agent-shell-codex-app-server--reasoning-effort-order
  '("none" "minimal" "low" "medium" "high" "xhigh")
  "Preferred display order for Codex reasoning efforts.")

(defvar agent-shell--state)
(defvar agent-shell--version)
(defvar agent-shell-codex-app-server--instance-count 0)
(defvar agent-shell-codex-app-server--output-flush-interval 0.25
  "Seconds to debounce streamed tool output updates.")

(declare-function agent-shell--build-content-blocks "agent-shell")
(declare-function agent-shell-prompt-queue "agent-shell-prompt-queue")

(defconst agent-shell-codex-app-server--tool-output-display-limit
  (* 256 1024)
  "Maximum number of tool output characters sent to the UI.")

(defconst agent-shell-codex-app-server--tool-output-truncated-prefix
  "[... earlier tool output omitted ...]\n"
  "Prefix added to truncated tool output.")

(defconst agent-shell-codex-app-server--auto-resolved
  'agent-shell-codex-app-server--auto-resolved
  "Sentinel returned when a user-input request times out.")

(defconst agent-shell-codex-app-server--user-input-timeout 120000
  "Milliseconds before a non-blocking user-input request auto-resolves.")

(defconst agent-shell-codex-app-server--omitted
  'agent-shell-codex-app-server--omitted
  "Sentinel returned for an omitted optional form value.")

(defvar agent-shell-codex-app-server-connection-type 'pty
  "Connection type used for Codex app-server processes.

Codex app-server currently flushes JSON-RPC responses reliably over
PTYs, while pipe-based startup can stall during initialization.")

(defun agent-shell-codex-app-server--prepare-resume-buffer (client)
  "Prepare CLIENT's shell buffer to resume a potentially large thread."
  (when-let* ((buffer (map-elt client :context-buffer))
              (_ (buffer-live-p buffer)))
    (with-current-buffer buffer
      ;; Emacs can loop in `visual-wrap-prefix-function' when redisplay
      ;; reaches an unsafe display property at point-max.
      (when (and (bound-and-true-p visual-wrap-prefix-mode)
                 (fboundp 'visual-wrap-prefix-mode))
        (funcall 'visual-wrap-prefix-mode -1)))))

(defun agent-shell-codex-app-server--next-instance-count ()
  "Return the next unique client instance id."
  (setq agent-shell-codex-app-server--instance-count
        (1+ agent-shell-codex-app-server--instance-count)))

(defun agent-shell-codex-app-server--ensure-session-title-slot (client)
  "Ensure CLIENT's shell session has a mutable `:title' slot.

Core agent-shell updates session titles with `map-put!', which can update an
existing alist key but cannot add a missing key in place."
  (when-let* ((buffer (map-elt client :context-buffer))
              (_ (buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (and (boundp 'agent-shell--state)
                 (map-elt agent-shell--state :session)
                 (not (assoc :title (map-elt agent-shell--state :session))))
        (map-put! agent-shell--state
                  :session
                  (map-insert (map-elt agent-shell--state :session)
                              :title
                              nil))))))

(defun agent-shell-codex-app-server--current-session-id (client)
  "Return the current Agent Shell session id for CLIENT."
  (or (when-let* ((buffer (map-elt client :context-buffer))
                  (_ (buffer-live-p buffer)))
        (with-current-buffer buffer
          (when (boundp 'agent-shell--state)
            (map-nested-elt agent-shell--state '(:session :id)))))
      (map-elt client :thread-id)))

(defun agent-shell-codex-app-server--update-session-title (client params)
  "Apply a thread title update from PARAMS to CLIENT's shell state."
  (let ((thread-id (map-elt params 'threadId)))
    (when (and thread-id
               (equal thread-id
                      (agent-shell-codex-app-server--current-session-id client))
               (map-contains-key params 'threadName))
      (when-let* ((buffer (map-elt client :context-buffer))
                  (_ (buffer-live-p buffer)))
        (with-current-buffer buffer
          (when (and (boundp 'agent-shell--state)
                     (map-elt agent-shell--state :session))
            (agent-shell-codex-app-server--ensure-session-title-slot client)
            (map-put! (map-elt agent-shell--state :session)
                      :title
                      (map-elt params 'threadName))
            (when (and (derived-mode-p 'agent-shell-mode)
                       (fboundp 'agent-shell--update-header-and-mode-line))
              (funcall 'agent-shell--update-header-and-mode-line))))))))

;;;###autoload
(cl-defun agent-shell-codex-app-server-make-client (&key command
                                                         command-params
                                                         environment-variables
                                                         context-buffer
                                                         approval-policy
                                                         sandbox-mode
                                                         connection-type)
  "Create a Codex app-server client.

Use COMMAND, COMMAND-PARAMS, ENVIRONMENT-VARIABLES, CONTEXT-BUFFER,
APPROVAL-POLICY, SANDBOX-MODE, and CONNECTION-TYPE."
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
        (cons :connection-type (or connection-type
                                   agent-shell-codex-app-server-connection-type))
        (cons :environment-variables environment-variables)
        (cons :context-buffer context-buffer)
        (cons :partial-output "")
        (cons :echo-lines nil)
        (cons :message-queue nil)
        (cons :message-queue-busy nil)
        (cons :message-drain-timer nil)
        (cons :request-id 0)
        (cons :pending-requests (make-hash-table :test #'equal))
        (cons :notification-handlers nil)
        (cons :request-handlers nil)
        (cons :error-handlers nil)
        (cons :mcp-server-statuses nil)
        (cons :pending-permissions (make-hash-table :test #'equal))
        (cons :async-questions (make-hash-table :test #'equal))
        (cons :tool-items (make-hash-table :test #'equal))
        (cons :tool-outputs (make-hash-table :test #'equal))
        (cons :tool-output-chunks (make-hash-table :test #'equal))
        (cons :pending-tool-output-items (make-hash-table :test #'equal))
        (cons :tool-output-flush-timer nil)
        (cons :thread-id nil)
        (cons :active-turn-id nil)
        (cons :current-model-id nil)
        (cons :available-models nil)
        (cons :reasoning-effort "medium")
        (cons :latest-token-usage nil)
        (cons :pending-agent-message nil)
        (cons :pending-prompt nil)
        (cons :dismissed-turn-ids nil)
        (cons :interrupt-next-turn nil)
        (cons :approval-policy (or approval-policy "on-request"))
        (cons :sandbox-mode (or sandbox-mode "workspace-write"))
        (cons :shutting-down nil)))

;;;###autoload
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

(defun agent-shell-codex-app-server--approval-request-method-p (method)
  "Return non-nil when METHOD is a supported approval request."
  (member method '("item/commandExecution/requestApproval"
                   "item/fileChange/requestApproval"
                   "item/permissions/requestApproval"
                   "mcpServer/elicitation/request"
                   "execCommandApproval"
                   "applyPatchApproval")))

(defun agent-shell-codex-app-server--user-input-prompt (question)
  "Return a minibuffer prompt for QUESTION."
  (let ((header (map-elt question 'header))
        (text (or (map-elt question 'question) "Answer required")))
    (if (and (stringp header) (not (string-empty-p header)))
        (format "%s: %s " header text)
      (format "%s " text))))

(defun agent-shell-codex-app-server--read-user-input-text (question)
  "Read a non-empty free-form answer for QUESTION."
  (let (answer)
    (while (string-empty-p (or answer ""))
      (setq answer
            (if (map-elt question 'isSecret)
                (read-passwd (agent-shell-codex-app-server--user-input-prompt
                              question))
              (read-string (agent-shell-codex-app-server--user-input-prompt
                            question)))))
    (concat "user_note: " answer)))

(defun agent-shell-codex-app-server--read-user-input-option (question options)
  "Read an answer to QUESTION from OPTIONS or free-form input."
  (let* ((other-label "Other (free-form)")
         (labels (seq-map (lambda (option)
                            (map-elt option 'label))
                          options))
         (choices (if (map-elt question 'isOther)
                      (append labels (list other-label))
                    labels))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (candidate)
               (when-let* ((option
                            (seq-find
                             (lambda (entry)
                               (equal candidate (map-elt entry 'label)))
                             options))
                           (description (map-elt option 'description))
                           ((not (string-empty-p description))))
                 (concat "  " description)))))
         (answer (completing-read
                  (agent-shell-codex-app-server--user-input-prompt question)
                  choices nil t)))
    (if (equal answer other-label)
        (agent-shell-codex-app-server--read-user-input-text question)
      answer)))

(defun agent-shell-codex-app-server--collect-user-input-answers (params)
  "Prompt for and return structured answers to PARAMS."
  (let (answers)
    (dolist (question (map-elt params 'questions))
      (when-let* ((id (map-elt question 'id)))
        (let ((options (seq-filter
                        (lambda (option)
                          (stringp (map-elt option 'label)))
                        (map-elt question 'options))))
          (push (cons id
                      `((answers . (,(if options
                                         (agent-shell-codex-app-server--read-user-input-option
                                          question options)
                                       (agent-shell-codex-app-server--read-user-input-text
                                        question))))))
                answers))))
    `((answers . ,(if answers
                      (nreverse answers)
                    (agent-shell-codex-app-server--json-empty-object))))))

(defun agent-shell-codex-app-server--user-input-timeout-ms (params)
  "Return the auto-resolution timeout from PARAMS when applicable.

For example, `((isBlocking . t))' returns nil, while
`((isBlocking . nil))' returns
`agent-shell-codex-app-server--user-input-timeout'.  A request without
`isBlocking' uses the current protocol's blocking compatibility default."
  (when (and (map-contains-key params 'isBlocking)
             (not (map-elt params 'isBlocking)))
    (or (map-elt params 'autoResolutionMs)
        agent-shell-codex-app-server--user-input-timeout)))

(defun agent-shell-codex-app-server--handle-user-input-request (client request)
  "Prompt for answers to app-server user-input REQUEST for CLIENT."
  (let ((params (or (map-elt request 'params) '()))
        (request-id (map-elt request 'id)))
    (condition-case err
        (let ((result
               (agent-shell-codex-app-server--call-with-buffer
                client nil
                (lambda ()
                  (if-let* ((timeout-ms
                             (agent-shell-codex-app-server--user-input-timeout-ms
                              params)))
                      (with-timeout
                          ((/ timeout-ms 1000.0)
                           agent-shell-codex-app-server--auto-resolved)
                        (agent-shell-codex-app-server--collect-user-input-answers
                         params))
                    (agent-shell-codex-app-server--collect-user-input-answers
                     params))))))
          (agent-shell-codex-app-server--send-rpc-response
           :client client
           :request-id request-id
           :result (if (eq result agent-shell-codex-app-server--auto-resolved)
                       `((answers . ,(agent-shell-codex-app-server--json-empty-object)))
                     result)))
      (quit
       (agent-shell-codex-app-server-interrupt client))
      (error
       (agent-shell-codex-app-server--send-rpc-error
        :client client
        :request-id request-id
        :code -32603
        :message (error-message-string err))
       (agent-shell-codex-app-server--call-error-handlers
        client
        (format "Failed to request user input: %s" (error-message-string err))
        request)))))

(defun agent-shell-codex-app-server--mcp-form-request-p (params)
  "Return non-nil when PARAMS contain a non-empty standard MCP form."
  (and (equal (map-elt params 'mode) "form")
       (map-nested-elt params '(requestedSchema properties))))

(defun agent-shell-codex-app-server--mcp-form-prefix (params)
  "Return a prompt prefix attributing an MCP form in PARAMS to its server.

Form fields are collected straight from the minibuffer, so the prefix is
the only place the user sees who is asking for the value.  For example,
`((serverName . \"docs\"))' returns \"[docs] \" while PARAMS without a
server name returns an empty string."
  (if-let* ((server-name (map-elt params 'serverName))
            ((stringp server-name))
            ((not (string-empty-p server-name))))
      (format "[%s] " server-name)
    ""))

(defun agent-shell-codex-app-server--mcp-form-prompt (prefix name schema required)
  "Return a minibuffer prompt for MCP form NAME using SCHEMA and REQUIRED.

PREFIX attributes the form to its MCP server.  For example, prefix
\"[docs] \", name \"path\" and SCHEMA `((description . \"Doc path\"))'
with REQUIRED nil returns \"[docs] path (optional) - Doc path: \"."
  (let ((title (or (map-elt schema 'title) name))
        (description (map-elt schema 'description)))
    (format "%s%s%s%s: "
            prefix
            title
            (if required "" " (optional)")
            (if (and (stringp description)
                     (not (string-empty-p description)))
                (format " - %s" description)
              ""))))

(defun agent-shell-codex-app-server--mcp-form-choice-pairs (schema)
  "Return display/value pairs for an enum form SCHEMA."
  (let ((options (or (map-elt schema 'oneOf)
                     (map-elt schema 'anyOf))))
    (cond
     (options
      (seq-map (lambda (option)
                 (cons (or (map-elt option 'title)
                           (map-elt option 'const))
                       (map-elt option 'const)))
               options))
     ((map-elt schema 'enum)
      (let ((names (map-elt schema 'enumNames)))
        (seq-map-indexed
         (lambda (value index)
           (cons (or (and names
                          (< index (length names))
                          (seq-elt names index))
                     value)
                 value))
         (map-elt schema 'enum)))))))

(defun agent-shell-codex-app-server--mcp-form-default-label (schema choices)
  "Return the display label for SCHEMA's default among CHOICES."
  (when-let* (((map-contains-key schema 'default))
              (choice
               (seq-find (lambda (entry)
                           (equal (cdr entry) (map-elt schema 'default)))
                         choices)))
    (car choice)))

(defun agent-shell-codex-app-server--read-mcp-form-choice (prompt schema required)
  "Read one MCP form choice using PROMPT, SCHEMA, and REQUIRED."
  (let* ((choices (agent-shell-codex-app-server--mcp-form-choice-pairs schema))
         (labels (seq-map #'car choices)))
    (unless choices
      (error "MCP form choice has no supported values"))
    (let ((answer (completing-read
                   prompt
                   (if required labels (cons "" labels))
                   nil t nil nil
                   (agent-shell-codex-app-server--mcp-form-default-label
                    schema choices))))
      (if (string-empty-p answer)
          agent-shell-codex-app-server--omitted
        (map-elt choices answer)))))

(defun agent-shell-codex-app-server--mcp-form-number-valid-p (value schema)
  "Return non-nil when numeric VALUE satisfies form SCHEMA."
  (and (or (not (map-contains-key schema 'minimum))
           (>= value (map-elt schema 'minimum)))
       (or (not (map-contains-key schema 'maximum))
           (<= value (map-elt schema 'maximum)))))

(defun agent-shell-codex-app-server--read-mcp-form-number (prompt schema required)
  "Read a numeric MCP form value using PROMPT, SCHEMA, and REQUIRED."
  (let ((regexp (if (equal (map-elt schema 'type) "integer")
                    "\\`[+-]?[0-9]+\\'"
                  "\\`[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'"))
        value
        done)
    (while (not done)
      (let ((answer
             (read-string prompt nil nil
                          (when (map-contains-key schema 'default)
                            (number-to-string (map-elt schema 'default))))))
        (cond
         ((and (not required) (string-empty-p answer))
          (setq value agent-shell-codex-app-server--omitted
                done t))
         ((string-match-p regexp answer)
          (let ((number (string-to-number answer)))
            (if (agent-shell-codex-app-server--mcp-form-number-valid-p
                 number schema)
                (setq value number
                      done t)
              (message "Value is outside the allowed range"))))
         (t
          (message "Enter a valid %s" (map-elt schema 'type))))))
    value))

(defun agent-shell-codex-app-server--mcp-form-string-valid-p (value schema)
  "Return non-nil when string VALUE satisfies form SCHEMA."
  (and (or (not (map-contains-key schema 'minLength))
           (>= (length value) (map-elt schema 'minLength)))
       (or (not (map-contains-key schema 'maxLength))
           (<= (length value) (map-elt schema 'maxLength)))))

(defun agent-shell-codex-app-server--read-mcp-form-string (prompt schema required)
  "Read a string MCP form value using PROMPT, SCHEMA, and REQUIRED."
  (let (value done)
    (while (not done)
      (let ((answer (read-string prompt nil nil (map-elt schema 'default))))
        (cond
         ((and (not required) (string-empty-p answer))
          (setq value agent-shell-codex-app-server--omitted
                done t))
         ((and required (string-empty-p answer))
          (message "A value is required"))
         ((agent-shell-codex-app-server--mcp-form-string-valid-p answer schema)
          (setq value answer
                done t))
         (t
          (message "Text does not satisfy the requested length")))))
    value))

(defun agent-shell-codex-app-server--read-mcp-form-boolean (prompt schema required)
  "Read a boolean MCP form value using PROMPT, SCHEMA, and REQUIRED."
  (let* ((choices (if required '("Yes" "No") '("" "Yes" "No")))
         (default (when (map-contains-key schema 'default)
                    (if (map-elt schema 'default) "Yes" "No")))
         (answer (completing-read prompt choices nil t nil nil default)))
    (pcase answer
      ("Yes" t)
      ("No" (agent-shell-codex-app-server--json-bool nil))
      (_ agent-shell-codex-app-server--omitted))))

(defun agent-shell-codex-app-server--read-mcp-form-array (prompt schema required)
  "Read an array MCP form value using PROMPT, SCHEMA, and REQUIRED."
  (let* ((item-schema (map-elt schema 'items))
         (choices (agent-shell-codex-app-server--mcp-form-choice-pairs item-schema))
         (labels (seq-map #'car choices))
         (minimum (or (map-elt schema 'minItems) (if required 1 0)))
         (maximum (map-elt schema 'maxItems))
         value
         done)
    (unless choices
      (error "MCP form array has no supported values"))
    (while (not done)
      (let* ((answers (completing-read-multiple prompt labels nil t))
             (values (seq-map (lambda (answer)
                                (map-elt choices answer))
                              answers)))
        (cond
         ((and (not required) (null values))
          (setq value agent-shell-codex-app-server--omitted
                done t))
         ((< (length values) minimum)
          (message "Select at least %s value(s)" minimum))
         ((and maximum (> (length values) maximum))
          (message "Select at most %s value(s)" maximum))
         (t
          (setq value (vconcat values)
                done t)))))
    value))

(defun agent-shell-codex-app-server--read-mcp-form-value (prefix name schema required)
  "Read MCP form field NAME from SCHEMA, honoring PREFIX and REQUIRED."
  (let ((prompt (agent-shell-codex-app-server--mcp-form-prompt
                 prefix name schema required)))
    (cond
     ((seq-contains-p '("integer" "number") (map-elt schema 'type))
      (agent-shell-codex-app-server--read-mcp-form-number
       prompt schema required))
     ((equal (map-elt schema 'type) "boolean")
      (agent-shell-codex-app-server--read-mcp-form-boolean
       prompt schema required))
     ((equal (map-elt schema 'type) "array")
      (agent-shell-codex-app-server--read-mcp-form-array
       prompt schema required))
     ((agent-shell-codex-app-server--mcp-form-choice-pairs schema)
      (agent-shell-codex-app-server--read-mcp-form-choice
       prompt schema required))
     ((equal (map-elt schema 'type) "string")
      (agent-shell-codex-app-server--read-mcp-form-string
       prompt schema required))
     (t
      (error "Unsupported MCP form field type: %s"
             (map-elt schema 'type))))))

(defun agent-shell-codex-app-server--collect-mcp-form-content (params)
  "Read and return structured MCP form content from PARAMS."
  (let ((properties (map-nested-elt params '(requestedSchema properties)))
        (required-fields (map-nested-elt params '(requestedSchema required)))
        (prefix (agent-shell-codex-app-server--mcp-form-prefix params))
        content)
    (dolist (property properties)
      (let* ((name (symbol-name (car property)))
             (value (agent-shell-codex-app-server--read-mcp-form-value
                     prefix
                     name
                     (cdr property)
                     (seq-contains-p required-fields name))))
        (unless (eq value agent-shell-codex-app-server--omitted)
          (push (cons (car property) value) content))))
    (or (nreverse content)
        (agent-shell-codex-app-server--json-empty-object))))

(defun agent-shell-codex-app-server--send-mcp-elicitation-result (client request action content)
  "Respond to MCP elicitation REQUEST through CLIENT with ACTION and CONTENT."
  (agent-shell-codex-app-server--send-rpc-response
   :client client
   :request-id (map-elt request 'id)
   :result `((action . ,action)
             (content . ,content)
             (_meta . nil))))

(defun agent-shell-codex-app-server--handle-mcp-form-request (client request)
  "Collect and respond to a standard MCP form REQUEST for CLIENT."
  (condition-case err
      (agent-shell-codex-app-server--send-mcp-elicitation-result
       client
       request
       "accept"
       (agent-shell-codex-app-server--call-with-buffer
        client nil
        (lambda ()
          (agent-shell-codex-app-server--collect-mcp-form-content
           (map-elt request 'params)))))
    (quit
     (agent-shell-codex-app-server--send-mcp-elicitation-result
      client request "cancel" nil))
    (error
     (agent-shell-codex-app-server--send-mcp-elicitation-result
      client request "decline" nil)
     (agent-shell-codex-app-server--call-error-handlers
      client
      (format "Failed to render MCP form: %s" (error-message-string err))
      request))))

(defun agent-shell-codex-app-server--decline-unsupported-mcp-form (client request)
  "Decline unsupported MCP form REQUEST for CLIENT."
  (agent-shell-codex-app-server--send-mcp-elicitation-result
   client request "decline" nil))

(defun agent-shell-codex-app-server--unsupported-request-message (method)
  "Return an error message for unsupported server request METHOD."
  (format "Unsupported Codex app-server request: %s" method))

(defun agent-shell-codex-app-server--read-command-action-p (action)
  "Return non-nil when ACTION represents a read-only command action."
  (member (map-elt action 'type) '("read" "listFiles" "search")))

(defun agent-shell-codex-app-server--command-actions-kind (actions)
  "Return the tool kind that best matches command ACTIONS."
  (if (and actions
           (seq-every-p #'agent-shell-codex-app-server--read-command-action-p
                        actions))
      "read"
    "execute"))

(defun agent-shell-codex-app-server--command-kind (item)
  "Return the tool kind that best matches command execution ITEM."
  (agent-shell-codex-app-server--command-actions-kind
   (map-elt item 'commandActions)))

(defun agent-shell-codex-app-server--command-text (command)
  "Return COMMAND as a displayable string."
  (cond
   ((stringp command) command)
   ((listp command) (string-join command " "))
   (t nil)))

(defun agent-shell-codex-app-server--display-kind-prefixes (kind)
  "Return display prefixes that should be stripped for KIND."
  (pcase kind
    ("execute" '("execute" "run"))
    ("search" '("search" "find"))
    ("read" '("read"))
    ("edit" '("edit"))
    (_ (delq nil (list kind)))))

(defun agent-shell-codex-app-server--strip-kind-prefix (text kind)
  "Return TEXT without redundant leading KIND prefix."
  (if (not (stringp text))
      text
    (let ((trimmed text)
          (case-fold-search t))
      (dolist (prefix (agent-shell-codex-app-server--display-kind-prefixes kind))
        (when (and prefix
                   (string-match (concat "\\`" (regexp-quote prefix) "[[:space:]]+")
                                 trimmed))
          (setq trimmed (string-trim-left
                         (substring trimmed (match-end 0))))))
      trimmed)))

(defun agent-shell-codex-app-server--callback-buffer (client &optional buffer)
  "Return a live callback buffer for CLIENT, preferring BUFFER."
  (or (and (buffer-live-p buffer) buffer)
      (let ((context-buffer (map-elt client :context-buffer)))
        (and (buffer-live-p context-buffer) context-buffer))))

(defun agent-shell-codex-app-server--call-with-buffer (client buffer callback &rest args)
  "Invoke CALLBACK for CLIENT in BUFFER context with ARGS."
  (if-let* ((target-buffer (agent-shell-codex-app-server--callback-buffer
                            client buffer)))
      (with-current-buffer target-buffer
        (apply callback args))
    (apply callback args)))

(defun agent-shell-codex-app-server--call-error-handlers (client message &optional data)
  "Forward MESSAGE and optional DATA to CLIENT error handlers."
  (dolist (handler (map-elt client :error-handlers))
    (funcall handler (agent-shell-codex-app-server--make-error message data))))

(defun agent-shell-codex-app-server--pty-wrapper-shell ()
  "Return a POSIX shell path for PTY-wrapped app-server commands."
  (or (executable-find "sh")
      (when (and shell-file-name
                 (file-executable-p shell-file-name))
        shell-file-name)))

(defun agent-shell-codex-app-server--pty-wrapper-command (client)
  "Return a raw-mode PTY wrapper command for CLIENT.

The wrapper disables canonical mode and terminal echo before `exec'-ing
the actual Codex command.  This avoids long JSON-RPC request lines being
held up or dropped by the PTY line discipline."
  (when-let* ((shell (agent-shell-codex-app-server--pty-wrapper-shell)))
    (list shell
          "-lc"
          (format "stty raw -echo < /dev/tty && exec %s"
                  (mapconcat #'shell-quote-argument
                             (cons (map-elt client :command)
                                   (map-elt client :command-params))
                             " ")))))

(defun agent-shell-codex-app-server--process-command (client)
  "Return the command list used to start CLIENT."
  (or (and (eq (map-elt client :connection-type) 'pty)
           (agent-shell-codex-app-server--pty-wrapper-command client))
      (cons (map-elt client :command)
            (map-elt client :command-params))))

(defun agent-shell-codex-app-server--track-pty-echo-p (client)
  "Return non-nil when CLIENT should track PTY echoed input."
  (and (eq (map-elt client :connection-type) 'pty)
       (not (agent-shell-codex-app-server--pty-wrapper-command client))))

(defun agent-shell-codex-app-server--write-message (client payload)
  "Write JSON-RPC PAYLOAD to CLIENT."
  (unless (agent-shell-codex-app-server--client-started-p client)
    (error "Codex app-server process is not running"))
  (let ((line (json-encode payload)))
    (when (agent-shell-codex-app-server--track-pty-echo-p client)
      (map-put! client :echo-lines
                (nconc (map-elt client :echo-lines)
                       (list line))))
    (process-send-string
     (map-elt client :process)
     (concat line "\n"))))

(defun agent-shell-codex-app-server--consume-echoed-line (client line)
  "Return non-nil when LINE matches the next echoed request for CLIENT."
  (let ((echo-lines (map-elt client :echo-lines)))
    (when (and (agent-shell-codex-app-server--track-pty-echo-p client)
               echo-lines
               (equal (string-trim-right line "\r+")
                      (seq-first echo-lines)))
      (map-put! client :echo-lines (seq-rest echo-lines))
      t)))

(defun agent-shell-codex-app-server--reject-pending-requests (client message)
  "Reject all pending requests for CLIENT with MESSAGE."
  (let ((pending-requests (map-elt client :pending-requests)))
    (maphash
     (lambda (_id pending)
       (when-let* ((on-failure (map-elt pending :on-failure)))
         (agent-shell-codex-app-server--call-with-buffer
          client
          (map-elt pending :buffer)
          on-failure
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
           :command (agent-shell-codex-app-server--process-command client)
           :buffer nil
           :coding 'utf-8-unix
           :connection-type (map-elt client :connection-type)
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
                                                               buffer
                                                               on-success
                                                               on-failure)
  "Send raw JSON-RPC METHOD with PARAMS via CLIENT."
  (agent-shell-codex-app-server--ensure-started client)
  (let* ((id (agent-shell-codex-app-server--next-request-id client))
         (pending `((:method . ,method)
                    (:buffer . ,buffer)
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

(cl-defun agent-shell-codex-app-server--send-rpc-error (&key client
                                                             request-id
                                                             code
                                                             message
                                                             data)
  "Send a JSON-RPC error for REQUEST-ID via CLIENT."
  (agent-shell-codex-app-server--ensure-started client)
  (let ((error `((code . ,code)
                 (message . ,message))))
    (when data
      (setq error (append error `((data . ,data)))))
    (agent-shell-codex-app-server--write-message
     client
     `((jsonrpc . ,agent-shell-codex-app-server--jsonrpc-version)
       (id . ,request-id)
       (error . ,error)))))

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
    ("interrupted" "failed")
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

(defun agent-shell-codex-app-server--model-id (model)
  "Return the canonical identifier for MODEL."
  (or (map-elt model 'model)
      (map-elt model 'id)))

(defun agent-shell-codex-app-server--find-model (client &optional model-id)
  "Return CLIENT model matching MODEL-ID, or the current model."
  (let ((target-id (or model-id
                       (map-elt client :current-model-id))))
    (seq-find (lambda (model)
                (equal (agent-shell-codex-app-server--model-id model)
                       target-id))
              (map-elt client :available-models))))

(defun agent-shell-codex-app-server--reasoning-effort-supported-p (client effort &optional model-id)
  "Return non-nil when EFFORT is supported for CLIENT MODEL-ID."
  (when effort
    (if-let* ((model (agent-shell-codex-app-server--find-model client model-id))
              (options (append (or (map-elt model 'supportedReasoningEfforts) '()) nil)))
        (seq-some (lambda (option)
                    (equal (map-elt option 'reasoningEffort) effort))
                  options)
      t)))

(defun agent-shell-codex-app-server--default-reasoning-effort (client &optional model-id)
  "Return the default reasoning effort for CLIENT MODEL-ID."
  (or (map-elt (agent-shell-codex-app-server--find-model client model-id)
               'defaultReasoningEffort)
      (map-elt client :reasoning-effort)
      "medium"))

(defun agent-shell-codex-app-server--resolve-reasoning-effort (client &optional model-id effort)
  "Return the best reasoning effort for CLIENT MODEL-ID and EFFORT."
  (let ((candidate (or effort
                       (map-elt client :reasoning-effort))))
    (if (agent-shell-codex-app-server--reasoning-effort-supported-p
         client candidate model-id)
        candidate
      (agent-shell-codex-app-server--default-reasoning-effort
       client model-id))))

(defun agent-shell-codex-app-server--reasoning-mode-id (effort)
  "Return the synthetic mode identifier for EFFORT."
  (when effort
    (format "reasoning:%s" effort)))

(defun agent-shell-codex-app-server--mode-id-to-reasoning-effort (mode-id)
  "Return the reasoning effort encoded in MODE-ID."
  (when (and (stringp mode-id)
             (string-prefix-p "reasoning:" mode-id))
    (string-remove-prefix "reasoning:" mode-id)))

(defun agent-shell-codex-app-server--reasoning-mode-name (effort)
  "Return the display name for reasoning EFFORT."
  (pcase effort
    ("xhigh" "XHigh")
    (_ (capitalize (or effort "")))))

(defun agent-shell-codex-app-server--reasoning-mode-description (client effort)
  "Return a mode description for CLIENT reasoning EFFORT."
  (or (seq-some
       (lambda (model)
         (when-let* ((option
                      (seq-find (lambda (entry)
                                  (equal (map-elt entry 'reasoningEffort)
                                         effort))
                                (append (or (map-elt model 'supportedReasoningEfforts) '())
                                        nil))))
           (map-elt option 'description)))
       (map-elt client :available-models))
      (format "Reasoning effort: %s"
              (downcase (agent-shell-codex-app-server--reasoning-mode-name effort)))))

(defun agent-shell-codex-app-server--translate-modes (client &optional model-id effort)
  "Translate CLIENT reasoning settings into ACP-style session modes.

Use MODEL-ID and EFFORT to resolve the current mode."
  (let* ((current-effort
          (agent-shell-codex-app-server--resolve-reasoning-effort
           client model-id effort))
         (available-modes
          (delq nil
                (mapcar
                 (lambda (supported-effort)
                   (when (seq-some
                          (lambda (model)
                            (agent-shell-codex-app-server--reasoning-effort-supported-p
                             client supported-effort
                             (agent-shell-codex-app-server--model-id model)))
                          (map-elt client :available-models))
                     `((id . ,(agent-shell-codex-app-server--reasoning-mode-id
                               supported-effort))
                       (name . ,(agent-shell-codex-app-server--reasoning-mode-name
                                 supported-effort))
                       (description . ,(agent-shell-codex-app-server--reasoning-mode-description
                                        client supported-effort)))))
                 agent-shell-codex-app-server--reasoning-effort-order))))
    `((currentModeId . ,(agent-shell-codex-app-server--reasoning-mode-id
                         current-effort))
      (availableModes . ,(or available-modes
                             (when current-effort
                               (list `((id . ,(agent-shell-codex-app-server--reasoning-mode-id
                                               current-effort))
                                       (name . ,(agent-shell-codex-app-server--reasoning-mode-name
                                                 current-effort))
                                       (description . ,(agent-shell-codex-app-server--reasoning-mode-description
                                                        client current-effort))))))))))

(defun agent-shell-codex-app-server--dispatch-current-mode-update (client effort)
  "Notify CLIENT that the current reasoning EFFORT changed."
  (agent-shell-codex-app-server--dispatch-notification
   client
   `((method . "session/update")
     (params . ((update . ((sessionUpdate . "current_mode_update")
                           (currentModeId . ,(agent-shell-codex-app-server--reasoning-mode-id
                                              effort)))))))))

(defun agent-shell-codex-app-server--translate-models (models)
  "Translate MODELS to the shape used by agent-shell."
  (mapcar (lambda (model)
            `((modelId . ,(agent-shell-codex-app-server--model-id model))
              (name . ,(or (map-elt model 'displayName)
                           (map-elt model 'model)
                           (map-elt model 'id)))
              (description . ,(map-elt model 'description))))
          models))

(defun agent-shell-codex-app-server--session-response (client result)
  "Translate RESULT for CLIENT into an ACP-like session response."
  (let* ((thread (map-elt result 'thread))
         (thread-id (map-elt thread 'id))
         (model-id (or (map-elt result 'model)
                       (map-elt client :current-model-id)
                       (map-elt (seq-find (lambda (model)
                                            (map-elt model 'isDefault))
                                          (map-elt client :available-models))
                                'model)))
         (reasoning-effort
          (agent-shell-codex-app-server--resolve-reasoning-effort
           client
           model-id
           (or (map-elt result 'reasoningEffort)
               (map-elt client :reasoning-effort)))))
    (map-put! client :thread-id thread-id)
    (map-put! client :current-model-id model-id)
    (map-put! client :reasoning-effort reasoning-effort)
    `((sessionId . ,thread-id)
      (modes . ,(agent-shell-codex-app-server--translate-modes
                 client model-id reasoning-effort))
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
  (let ((change (seq-first changes)))
    (when change
      (let ((diff (map-elt change 'diff))
            (path (map-elt change 'path)))
        (append (list (cons 'path path))
                (when diff
                  (list (cons 'diff diff))))))))

(defun agent-shell-codex-app-server--file-change-kind-type (change)
  "Return CHANGE kind as a string."
  (let ((kind (map-elt change 'kind)))
    (cond
     ((stringp kind)
      kind)
     ((listp kind)
      (map-elt kind 'type)))))

(defun agent-shell-codex-app-server--file-change-texts (change)
  "Return (OLD-TEXT . NEW-TEXT) for a Codex file CHANGE."
  (let ((diff (or (map-elt change 'diff) ""))
        (kind (agent-shell-codex-app-server--file-change-kind-type change)))
    (pcase kind
      ("add"
       (cons "" diff))
      ("delete"
       (cons diff ""))
      ("update"
       (agent-shell-codex-app-server--parse-unified-diff diff))
      (_
       (let ((parsed (agent-shell-codex-app-server--parse-unified-diff diff)))
         (if (or (not (string-empty-p (car parsed)))
                 (not (string-empty-p (cdr parsed))))
             parsed
           (cons "" diff)))))))

(defun agent-shell-codex-app-server--file-change-content-block (change)
  "Return an ACP diff content block for Codex file CHANGE."
  (when-let* ((path (map-elt change 'path))
              (texts (agent-shell-codex-app-server--file-change-texts change)))
    `((type . "diff")
      (path . ,path)
      (oldText . ,(car texts))
      (newText . ,(cdr texts)))))

(defun agent-shell-codex-app-server--file-change-content-blocks (changes)
  "Return ACP diff content blocks for Codex file CHANGES."
  (seq-keep #'agent-shell-codex-app-server--file-change-content-block
            (or changes '())))

(defun agent-shell-codex-app-server--file-change-title (changes)
  "Return a display title for Codex file CHANGES."
  (let ((changes (or changes '())))
    (cond
     ((= (length changes) 1)
      (or (map-elt (seq-first changes) 'path) "File change"))
     ((> (length changes) 1)
      (format "%d files" (length changes)))
     (t
      "File change"))))

(defun agent-shell-codex-app-server--alist-put (alist key value)
  "Return ALIST with KEY set to VALUE."
  (if-let* ((cell (assoc key alist)))
      (progn
        (setcdr cell value)
        alist)
    (cons (cons key value) alist)))

(defun agent-shell-codex-app-server--tool-entry-from-item (item)
  "Build a normalized tool entry from ITEM."
  (pcase (map-elt item 'type)
    ("commandExecution"
     (let* ((kind (agent-shell-codex-app-server--command-kind item))
            (command (or (agent-shell-codex-app-server--command-text
                          (map-elt item 'command))
                         "Run command"))
            (display-command (agent-shell-codex-app-server--strip-kind-prefix
                              command kind)))
       `((:title . ,display-command)
         (:kind . ,kind)
         (:command . ,command)
         (:description . ,display-command)
         (:raw-input . ((command . ,command)
                        (description . ,display-command)
                        (cwd . ,(map-elt item 'cwd)))))))
    ("fileChange"
     (let* ((changes (map-elt item 'changes))
            (change (agent-shell-codex-app-server--extract-first-file-change
                     changes))
            (path (map-elt change 'path)))
       `((:title . ,(agent-shell-codex-app-server--file-change-title changes))
         (:kind . "edit")
         (:description . ,(or path (agent-shell-codex-app-server--file-change-title changes)))
         (:raw-input . ,change))))
    ("mcpToolCall"
     (let ((tool-name (or (map-elt item 'tool)
                          (map-elt item 'name)
                          "tool")))
       `((:title . ,(format "%s/%s"
                            (or (map-elt item 'server) "mcp")
                            tool-name))
         (:kind . ,(if (map-elt item 'readOnlyHint)
                       "read"
                     "tool"))
         (:description . ,tool-name)
         (:raw-input . ((description . ,tool-name)
                        (arguments . ,(map-elt item 'arguments)))))))
    ("dynamicToolCall"
     (let ((tool-name (or (map-elt item 'tool)
                          (map-elt item 'name)
                          (map-elt item 'title)
                          "Tool")))
       `((:title . ,tool-name)
         (:kind . "tool")
         (:description . ,tool-name)
         (:raw-input . ((description . ,tool-name)
                        (arguments . ,(or (map-elt item 'arguments)
                                          (map-elt item 'input))))))))
    ("collabAgentToolCall"
     (let* ((title (pcase (map-elt item 'tool)
                     ("spawnAgent" "Spawn agent")
                     ("sendInput" "Send agent input")
                     ("resumeAgent" "Resume agent")
                     ("wait" "Wait for agents")
                     ("closeAgent" "Close agent")
                     ("sendMessage" "Send agent message")
                     ("followupTask" "Follow up with agent")
                     ("interruptAgent" "Interrupt agent")
                     ("listAgents" "List agents")
                     (_ "Agent task")))
            (raw-input
             (delq nil
                   (list (cons 'description title)
                         (when-let* ((prompt (map-elt item 'prompt)))
                           (cons 'prompt prompt))
                         (when-let* ((model (map-elt item 'model)))
                           (cons 'model model))
                         (when-let* ((effort (map-elt item 'reasoningEffort)))
                           (cons 'reasoningEffort effort))
                         (when-let* ((receivers (map-elt item 'receiverThreadIds)))
                           (cons 'receiverThreadIds receivers))))))
       `((:title . ,title)
         (:kind . "other")
         (:description . ,(or (map-elt item 'prompt) title))
         (:raw-input . ,raw-input))))
    ("subAgentActivity"
     (let ((agent-path (or (map-elt item 'agentPath) "agent"))
           (action (pcase (map-elt item 'kind)
                     ("started" "Started")
                     ("interacted" "Contacted")
                     ("interrupted" "Interrupted")
                     ("completed" "Completed")
                     (_ "Agent activity"))))
       `((:title . ,(format "%s %s" action agent-path))
         (:kind . "other")
         (:description . ,action)
         (:raw-input . ((description . ,action)
                        (agentPath . ,agent-path)
                        (agentThreadId . ,(map-elt item 'agentThreadId)))))))
    ("imageView"
     (let ((path (map-elt item 'path)))
       `((:title . ,(or path "View image"))
         (:kind . "read")
         (:description . "View image")
         (:raw-input . ((description . "View image")
                        (path . ,path))))))
    ("imageGeneration"
     `((:title . "Image generation")
       (:kind . "other")
       (:description . ,(or (map-elt item 'revisedPrompt)
                            "Generate image"))
       (:raw-input . ((description . "Generate image")
                      (prompt . ,(map-elt item 'revisedPrompt))))))
    ("sleep"
     (let ((duration-ms (or (map-elt item 'durationMs) 0)))
       `((:title . ,(format "Wait %gs" (/ duration-ms 1000.0)))
         (:kind . "other")
         (:description . "Wait")
         (:raw-input . ((description . "Wait")
                        (durationMs . ,duration-ms))))))
    ("webSearch"
     (let ((query (map-elt item 'query))
           (action-type (map-nested-elt item '(action type))))
       `((:title . ,(if (and query (not (string-empty-p query)))
                        query
                      "Web search"))
         (:kind . "search")
         (:description . ,(if (and query (not (string-empty-p query)))
                              query
                            (or action-type "Web search")))
         (:raw-input . ((description . ,(if (and query (not (string-empty-p query)))
                                            query
                                          (or action-type "Web search")))
                        (query . ,query)
                        (action . ,(map-elt item 'action)))))))
    (_
     `((:title . ,(or (map-elt item 'type) "Tool"))
       (:kind . "tool")
       (:raw-input . ())))))

(defun agent-shell-codex-app-server--render-json (value)
  "Return VALUE as a compact pretty JSON string when possible."
  (when value
    (condition-case nil
        (let ((json-encoding-pretty-print t))
          (json-encode value))
      (error
       (format "%S" value)))))

(defun agent-shell-codex-app-server--error-message-text (value)
  "Return a human-readable string for app-server error VALUE."
  (cond
   ((stringp value) value)
   ((or (hash-table-p value)
        (and (listp value) value))
    (or (let ((message (map-elt value 'message)))
          (and (stringp message) message))
        (let ((error (map-elt value 'error)))
          (and (stringp error) error))
        (agent-shell-codex-app-server--render-json value)))
   ((null value) nil)
   (t
    (format "%s" value))))

(defun agent-shell-codex-app-server--answer-async-questions (context)
  "Collect and submit answers for the question CONTEXT.

Answers use the originating shell's prompt queue, not a server RPC reply."
  (let ((client (map-elt context :client))
        (enable-recursive-minibuffers t))
    (unless (eq (map-elt context :status) 'pending)
      (user-error "These questions are already answered or dismissed"))
    (setf (map-elt context :status) 'answering)
    (unwind-protect
        (let ((answers
               (seq-map
                (lambda (question)
                  (let ((answer ""))
                    (while (string-empty-p (string-trim answer))
                      (setq answer
                            (completing-read
                             (concat (map-elt question 'title) " ")
                             (map-elt question 'options) nil nil)))
                    (concat (map-elt question 'title) "\n" answer)))
                (map-elt context :questions)))
              (shell (map-elt client :context-buffer)))
          (unless (and (eq (map-elt context :status) 'answering)
                       (buffer-live-p shell)
                       (not (map-elt client :shutting-down))
                       (with-current-buffer shell
                         (eq (map-elt agent-shell--state :client) client)))
            (user-error "The originating Codex session is no longer available"))
          (require 'agent-shell-prompt-queue)
          (with-current-buffer shell
            (agent-shell-prompt-queue (string-join answers "\n\n")))
          (setf (map-elt context :status) 'answered)
          (when (buffer-live-p (map-elt context :buffer))
            (kill-buffer (map-elt context :buffer))))
      (when (eq (map-elt context :status) 'answering)
        (setf (map-elt context :status) 'pending)))))

(defun agent-shell-codex-app-server--show-async-questions (client item)
  "Display ITEM's questions for CLIENT without blocking notification handling."
  (when-let* ((questions (map-elt item 'questions))
              (id (map-elt item 'id))
              ((not (gethash id (map-elt client :async-questions)))))
    (let* ((buffer (generate-new-buffer
                    (format "*Codex questions: %s*"
                            (or (map-elt client :context-buffer) id))))
           (context `((:client . ,client)
                      (:buffer . ,buffer)
                      (:questions . ,questions)
                      (:status . pending))))
      (puthash id context (map-elt client :async-questions))
      (with-current-buffer buffer
        (special-mode)
        (let ((inhibit-read-only t))
          (seq-doseq (question questions)
            (insert (map-elt question 'title) "\n")
            (seq-doseq (option (map-elt question 'options))
              (insert "  - " option "\n"))
            (insert "\n"))
          (insert-text-button
           "Answer" 'follow-link t
           'action (lambda (_button)
                     (agent-shell-codex-app-server--answer-async-questions context)))
          (insert "  ")
          (insert-text-button
           "Dismiss" 'follow-link t
           'action (lambda (_button)
                     (setf (map-elt context :status) 'dismissed)
                     (kill-buffer buffer)))
          (goto-char (point-min))))
      (display-buffer buffer))))

(defun agent-shell-codex-app-server--result-text (item)
  "Extract human-readable result text from ITEM."
  (let ((result (map-elt item 'result)))
    (cond
     ((equal (map-elt item 'type) "imageGeneration")
      (or (map-elt item 'savedPath)
          (map-elt item 'revisedPrompt)
          (map-elt item 'status)))
     ((equal (map-elt item 'type) "collabAgentToolCall")
      (agent-shell-codex-app-server--render-json
       (map-elt item 'agentsStates)))
     ((equal (map-elt item 'type) "dynamicToolCall")
      (string-join
       (seq-map (lambda (entry)
                  (pcase (map-elt entry 'type)
                    ("inputText" (or (map-elt entry 'text) ""))
                    ("inputImage" "[Image output]")
                    ("inputAudio" "[Audio output]")
                    (_ "[Unsupported tool output]")))
                (map-elt item 'contentItems))
       "\n\n"))
     (t
      (or
       (when-let* ((content (map-elt result 'content)))
         (string-join
          (delq nil
                (mapcar (lambda (entry)
                          (cond
                           ((stringp entry) entry)
                           ((equal (map-elt entry 'type) "text")
                            (map-elt entry 'text))
                           (t
                            (agent-shell-codex-app-server--render-json entry))))
                        content))
          "\n\n"))
       (when-let* ((structured (map-elt result 'structuredContent)))
         (agent-shell-codex-app-server--render-json structured))
       (when-let* ((error (map-elt item 'error)))
         (agent-shell-codex-app-server--render-json error))
       (when (and (equal (map-elt item 'type) "webSearch")
                  (map-nested-elt item '(action url)))
         (map-nested-elt item '(action url))))))))

(defun agent-shell-codex-app-server--tool-content (client item &optional complete)
  "Build tool-call content for ITEM using CLIENT state.

When COMPLETE is non-nil, return the complete result for final ACP
notification and transcript handling.  Otherwise bound streamed command
output for redisplay."
  (let* ((item-id (map-elt item 'id))
         (output
          (if complete
              (or (agent-shell-codex-app-server--tool-output-text client item-id)
                  (map-elt item 'aggregatedOutput)
                  (agent-shell-codex-app-server--result-text item))
            (or (agent-shell-codex-app-server--tool-output-preview client item-id)
                (agent-shell-codex-app-server--tool-output-display-text
                 (map-elt item 'aggregatedOutput))
                (agent-shell-codex-app-server--tool-output-display-text
                 (agent-shell-codex-app-server--result-text item)))))
         blocks)
    (when (and output (not (string-empty-p output)))
      (push `((content . ((type . "text")
                          (text . ,output)))) blocks))
    (when (and (equal (map-elt item 'type) "fileChange")
               (map-elt item 'changes))
      (setq blocks
            (append (reverse (agent-shell-codex-app-server--file-change-content-blocks
                              (map-elt item 'changes)))
                    blocks)))
    (vconcat (nreverse blocks))))

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
         (entry (agent-shell-codex-app-server--alist-put
                 (agent-shell-codex-app-server--tool-entry-from-item item)
                 :status
                 (agent-shell-codex-app-server--normalize-status status))))
    (puthash item-id entry (map-elt client :tool-items))
    entry))

(defun agent-shell-codex-app-server--get-tool-entry (client item-id)
  "Return the stored tool entry for CLIENT and ITEM-ID."
  (gethash item-id (map-elt client :tool-items)))

(defun agent-shell-codex-app-server--translate-tool-notification (session-update
                                                                  client
                                                                  item
                                                                  status
                                                                  &optional complete)
  "Translate a tool notification for CLIENT.

Use SESSION-UPDATE, ITEM, and STATUS to build the ACP-like payload.
When COMPLETE is non-nil, include complete output for transcript parity."
  (let* ((item-id (map-elt item 'id))
         (entry (or (agent-shell-codex-app-server--get-tool-entry client item-id)
                    (agent-shell-codex-app-server--save-tool-entry client item status))))
    (setq entry
          (agent-shell-codex-app-server--alist-put
           entry
           :status
           (agent-shell-codex-app-server--normalize-status status)))
    (puthash item-id entry (map-elt client :tool-items))
    `((method . "session/update")
      (params . ((update . ((sessionUpdate . ,session-update)
                            (toolCallId . ,item-id)
                            (title . ,(map-elt entry :title))
                            (status . ,(map-elt entry :status))
                            (kind . ,(map-elt entry :kind))
                            (rawInput . ,(or (map-elt entry :raw-input) '()))
                            (content . ,(agent-shell-codex-app-server--tool-content
                                         client item complete)))))))))

(defun agent-shell-codex-app-server--translate-command-output (client params)
  "Translate CLIENT command output PARAMS to a tool_call_update."
  (let* ((item-id (map-elt params 'itemId))
         (delta (or (map-elt params 'delta) "")))
    (unless (string-empty-p delta)
      (puthash item-id
               (cons delta (gethash item-id (map-elt client :tool-output-chunks)))
               (map-elt client :tool-output-chunks)))
    (if (or (null agent-shell-codex-app-server--output-flush-interval)
            (<= agent-shell-codex-app-server--output-flush-interval 0))
        (agent-shell-codex-app-server--tool-output-update client item-id)
      (puthash item-id t (map-elt client :pending-tool-output-items))
      (agent-shell-codex-app-server--schedule-tool-output-flush client)
      nil)))

(defun agent-shell-codex-app-server--tool-output-text (client item-id)
  "Return accumulated output text for ITEM-ID in CLIENT."
  (or (gethash item-id (map-elt client :tool-outputs))
      (when-let* ((chunks (gethash item-id (map-elt client :tool-output-chunks))))
        (mapconcat #'identity (reverse chunks) ""))))

(defun agent-shell-codex-app-server--tool-output-display-text (text)
  "Return a bounded tail of tool output TEXT.

For example, oversized TEXT retains its newest
`agent-shell-codex-app-server--tool-output-display-limit' characters and
gains `agent-shell-codex-app-server--tool-output-truncated-prefix'."
  (if (or (not (stringp text))
          (<= (length text)
              agent-shell-codex-app-server--tool-output-display-limit))
      text
    (concat
     agent-shell-codex-app-server--tool-output-truncated-prefix
     (substring text
                (- (length text)
                   agent-shell-codex-app-server--tool-output-display-limit)))))

(defun agent-shell-codex-app-server--tool-output-chunks-preview (chunks)
  "Return a bounded output tail from newest-first CHUNKS.

For example, CHUNKS (\"new\" \"old\") produce \"oldnew\" when both fit.
Older chunks are not concatenated once the display limit is reached."
  (let ((remaining agent-shell-codex-app-server--tool-output-display-limit)
        pieces
        truncated)
    (while (and chunks (> remaining 0))
      (let* ((chunk (seq-first chunks))
             (chunk-length (length chunk)))
        (if (<= chunk-length remaining)
            (progn
              (push chunk pieces)
              (setq remaining (- remaining chunk-length)))
          (push (substring chunk (- chunk-length remaining)) pieces)
          (setq remaining 0
                truncated t)))
      (setq chunks (seq-rest chunks)))
    (when chunks
      (setq truncated t))
    (concat
     (when truncated
       agent-shell-codex-app-server--tool-output-truncated-prefix)
     (mapconcat #'identity pieces ""))))

(defun agent-shell-codex-app-server--tool-output-preview (client item-id)
  "Return bounded output for ITEM-ID from CLIENT state.

Completed output uses the stored text.  Streaming output walks only enough
newest-first chunks to fill the display limit."
  (if-let* ((output (gethash item-id (map-elt client :tool-outputs))))
      (agent-shell-codex-app-server--tool-output-display-text output)
    (when-let* ((chunks (gethash item-id
                                 (map-elt client :tool-output-chunks))))
      (agent-shell-codex-app-server--tool-output-chunks-preview chunks))))

(defun agent-shell-codex-app-server--tool-text-update (client item-id text)
  "Build a tool update for ITEM-ID in CLIENT containing TEXT."
  (when-let* ((entry (agent-shell-codex-app-server--get-tool-entry client item-id)))
    `((method . "session/update")
      (params . ((update . ((sessionUpdate . "tool_call_update")
                            (toolCallId . ,item-id)
                            (title . ,(map-elt entry :title))
                            (status . ,(map-elt entry :status))
                            (kind . ,(map-elt entry :kind))
                            (rawInput . ,(or (map-elt entry :raw-input) '()))
                            (content . ,(vector
                                         `((content . ((type . "text")
                                                       (text . ,(or text ""))))))))))))))

(defun agent-shell-codex-app-server--tool-output-update (client item-id)
  "Build a bounded streamed output update for ITEM-ID in CLIENT."
  (agent-shell-codex-app-server--tool-text-update
   client
   item-id
   (agent-shell-codex-app-server--tool-output-preview client item-id)))

(defun agent-shell-codex-app-server--cancel-tool-output-flush (client)
  "Cancel any pending streamed tool output flush for CLIENT."
  (when-let* ((timer (map-elt client :tool-output-flush-timer)))
    (cancel-timer timer)
    (map-put! client :tool-output-flush-timer nil)))

(defun agent-shell-codex-app-server--clear-pending-tool-output (client)
  "Discard pending streamed tool output updates for CLIENT."
  (agent-shell-codex-app-server--cancel-tool-output-flush client)
  (clrhash (map-elt client :pending-tool-output-items))
  (clrhash (map-elt client :tool-output-chunks)))

(defun agent-shell-codex-app-server--clear-tool-item (client item-id)
  "Discard adapter state for completed ITEM-ID in CLIENT."
  (remhash item-id (map-elt client :tool-items))
  (remhash item-id (map-elt client :tool-outputs))
  (remhash item-id (map-elt client :tool-output-chunks))
  (remhash item-id (map-elt client :pending-tool-output-items))
  (when (zerop (hash-table-count (map-elt client :pending-tool-output-items)))
    (agent-shell-codex-app-server--cancel-tool-output-flush client)))

(defun agent-shell-codex-app-server--clear-tool-state (client)
  "Discard all provider-side tool translation state for CLIENT."
  (agent-shell-codex-app-server--clear-pending-tool-output client)
  (clrhash (map-elt client :tool-items))
  (clrhash (map-elt client :tool-outputs)))

(defun agent-shell-codex-app-server--flush-tool-output-updates (client)
  "Dispatch all queued streamed tool output updates for CLIENT."
  (let ((pending-items (map-elt client :pending-tool-output-items))
        item-ids)
    (map-put! client :tool-output-flush-timer nil)
    (maphash (lambda (item-id _value)
               (push item-id item-ids))
             pending-items)
    (clrhash pending-items)
    (dolist (item-id (nreverse item-ids))
      (when-let* ((notification
                   (agent-shell-codex-app-server--tool-output-update client item-id)))
        (agent-shell-codex-app-server--dispatch-notification client notification)))))

(defun agent-shell-codex-app-server--schedule-tool-output-flush (client)
  "Schedule a debounced streamed tool output flush for CLIENT."
  (unless (map-elt client :tool-output-flush-timer)
    (map-put! client :tool-output-flush-timer
              (run-at-time agent-shell-codex-app-server--output-flush-interval
                           nil
                           #'agent-shell-codex-app-server--flush-tool-output-updates
                           client))))

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
    (usage . ((totalTokens . ,(map-nested-elt client '(:latest-token-usage total totalTokens)))
              (inputTokens . ,(map-nested-elt client '(:latest-token-usage total inputTokens)))
              (outputTokens . ,(map-nested-elt client '(:latest-token-usage total outputTokens)))
              (thoughtTokens . ,(map-nested-elt client '(:latest-token-usage total reasoningOutputTokens)))
              (cachedReadTokens . ,(map-nested-elt client '(:latest-token-usage total cachedInputTokens)))
              (cachedWriteTokens . ,(map-nested-elt client '(:latest-token-usage total cacheWriteInputTokens)))))))

(defun agent-shell-codex-app-server--cancelled-turn (&optional turn-id)
  "Return a synthetic cancelled turn using TURN-ID."
  `((id . ,turn-id)
    (status . "failed")
    (error . ((message . "Task cancelled")))))

(defun agent-shell-codex-app-server--current-turn-id (client)
  "Return the currently tracked turn id for CLIENT."
  (or (map-nested-elt client '(:pending-prompt :turn-id))
      (map-elt client :active-turn-id)))

;;;###autoload
(defun agent-shell-codex-app-server-handle-busy-prompt (event)
  "Steer Codex's active turn using busy-prompt EVENT.

EVENT contains `:state', `:prompt', and `:fallback'.  Return non-nil
after claiming the prompt.  A failed or stale steer calls the fallback
so Agent Shell submits the text as a later turn instead of losing it."
  (when-let* ((state (map-elt event :state))
              (client (map-elt state :client))
              ((agent-shell-codex-app-server-client-p client))
              (thread-id (map-elt client :thread-id))
              (turn-id (agent-shell-codex-app-server--current-turn-id client))
              (pending-turn-id (map-nested-elt client '(:pending-prompt :turn-id)))
              ((equal turn-id pending-turn-id))
              (prompt (map-elt event :prompt))
              (fallback (map-elt event :fallback)))
    (condition-case nil
        (progn
          (agent-shell-codex-app-server--send-rpc-request
           :client client
           :method "turn/steer"
           :buffer (map-elt state :buffer)
           :params `((threadId . ,thread-id)
                     (input . ,(agent-shell-codex-app-server--translate-prompt-blocks
                                (agent-shell--build-content-blocks prompt)))
                     (expectedTurnId . ,turn-id))
           :on-success (lambda (result)
                         (unless (equal (map-elt result 'turnId) turn-id)
                           (funcall fallback)))
           :on-failure (lambda (&rest _)
                         (funcall fallback)))
          t)
      (error nil))))

(defun agent-shell-codex-app-server--dismiss-turn (client turn-id)
  "Remember TURN-ID as dismissed for CLIENT."
  (when (and turn-id
             (not (member turn-id (map-elt client :dismissed-turn-ids))))
    (map-put! client :dismissed-turn-ids
              (seq-take (cons turn-id (map-elt client :dismissed-turn-ids))
                        20))))

(defun agent-shell-codex-app-server--dismissed-turn-id-p (client turn-id)
  "Return non-nil when TURN-ID is currently dismissed for CLIENT."
  (and turn-id
       (member turn-id (map-elt client :dismissed-turn-ids))))

(defun agent-shell-codex-app-server--find-pending-request (client method)
  "Return the first pending request in CLIENT matching METHOD."
  (catch 'found
    (maphash
     (lambda (request-id pending)
       (when (equal (map-elt pending :method) method)
         (throw 'found (cons request-id pending))))
     (map-elt client :pending-requests))
    nil))

(defun agent-shell-codex-app-server--cancel-pending-turn-start (client)
  "Cancel any queued turn/start request for CLIENT.

The request is already on the wire, so its reply is repurposed rather
than dropped: a successful reply names the turn Codex just started and
is interrupted directly, while a failed reply disarms
`:interrupt-next-turn'.  Dropping the reply instead would leave the
flag armed forever whenever `turn/start' fails, silently killing the
next turn the user starts."
  (when-let* ((pending-request
               (agent-shell-codex-app-server--find-pending-request client "turn/start"))
              (request-id (car pending-request))
              (pending (cdr pending-request)))
    (map-put! client :interrupt-next-turn t)
    (puthash request-id
             `((:method . "turn/start")
               (:buffer . ,(map-elt pending :buffer))
               (:on-success . ,(lambda (result)
                                 (map-put! client :interrupt-next-turn nil)
                                 (agent-shell-codex-app-server--interrupt-turn
                                  client
                                  (map-nested-elt result '(turn id)))))
               (:on-failure . ,(lambda (_error _raw)
                                 (map-put! client :interrupt-next-turn nil))))
             (map-elt client :pending-requests))
    (when-let* ((on-failure (map-elt pending :on-failure)))
      (agent-shell-codex-app-server--call-with-buffer
       client
       (map-elt pending :buffer)
       on-failure
       (agent-shell-codex-app-server--make-error "Task cancelled")
       nil))
    t))

(defun agent-shell-codex-app-server--interrupt-turn (client turn-id)
  "Best-effort interrupt TURN-ID for CLIENT.

Dismissed turns are skipped so repeated interrupts, and a `turn/started'
notification racing the `turn/start' reply, send at most one
`turn/interrupt' per turn."
  (when (and (agent-shell-codex-app-server--client-started-p client)
             (map-elt client :thread-id)
             turn-id
             (not (agent-shell-codex-app-server--dismissed-turn-id-p client turn-id)))
    (agent-shell-codex-app-server--dismiss-turn client turn-id)
    (agent-shell-codex-app-server--send-rpc-request
     :client client
     :method "turn/interrupt"
     :params `((threadId . ,(map-elt client :thread-id))
               (turnId . ,turn-id))
     :on-success (lambda (_result) nil)
     :on-failure (lambda (_error _raw) nil))))

(defun agent-shell-codex-app-server--translate-prompt-block (block)
  "Translate a single ACP content BLOCK into a Codex user input item."
  (pcase (map-elt block 'type)
    ("text"
     `((type . "text")
       (text . ,(or (map-elt block 'text) ""))
       (textElements . [])))
    ("image"
     (if-let* ((path (agent-shell-codex-app-server--file-uri-to-path
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
         (textElements . []))))
    ("resource_link"
     (if-let* ((path (agent-shell-codex-app-server--file-uri-to-path
                      (map-elt block 'uri))))
         `((type . "mention")
           (name . ,(or (map-elt block 'name)
                        (file-name-nondirectory path)))
           (path . ,path))
       `((type . "text")
         (text . "")
         (textElements . []))))
    (_
     `((type . "text")
       (text . "")
       (textElements . [])))))

(defun agent-shell-codex-app-server--translate-prompt-blocks (prompt-blocks)
  "Translate ACP PROMPT-BLOCKS to Codex app-server user input."
  (mapcar #'agent-shell-codex-app-server--translate-prompt-block
          (append prompt-blocks nil)))

(defun agent-shell-codex-app-server--make-option (kind name option-id)
  "Return an ACP-like permission option for KIND, NAME, and OPTION-ID."
  `((kind . ,kind)
    (name . ,name)
    (optionId . ,option-id)))

(defun agent-shell-codex-app-server--decision-option-spec (decision)
  "Return UI metadata for app-server DECISION."
  (cond
   ((stringp decision)
    (pcase decision
      ((or "accept" "approved")
       `((:kind . "allow_once")
         (:name . "Allow")
         (:payload . ,decision)))
      ((or "acceptForSession" "approved_for_session")
       `((:kind . "allow_always")
         (:name . "Always Allow")
         (:payload . ,decision)))
      ((or "decline" "denied")
       `((:kind . "reject_once")
         (:name . "Reject")
         (:payload . ,decision)))
      ((or "cancel" "abort")
       `((:kind . "allow_once")
         (:name . "Cancel")
         (:payload . ,decision)))
      ("grant"
       `((:kind . "allow_once")
         (:name . "Allow")
         (:payload . ,decision)))
      (_ nil)))
   ((map-contains-key decision 'acceptWithExecpolicyAmendment)
    `((:kind . "allow_always")
      (:name . "Allow via Policy")
      (:payload . ,decision)))
   ((map-contains-key decision 'applyNetworkPolicyAmendment)
    (let* ((amendment (map-nested-elt decision
                                       '(applyNetworkPolicyAmendment network_policy_amendment)))
           (action (map-elt amendment 'action)))
      (when (member action '("allow" "deny"))
        `((:kind . ,(if (equal action "deny") "reject_always" "allow_always"))
          (:name . ,(format "%s Network: %s"
                           (if (equal action "deny") "Deny" "Allow")
                           (map-elt amendment 'host)))
          (:payload . ,decision)))))
   (t nil)))

(defun agent-shell-codex-app-server--empty-granted-permissions ()
  "Return an empty granted permissions object."
  (agent-shell-codex-app-server--json-empty-object))

(defun agent-shell-codex-app-server--permissions-request-options (params)
  "Return ACP-like options and response payloads for permissions PARAMS."
  (let* ((granted (or (agent-shell-codex-app-server--grant-permissions
                       (map-elt params 'permissions))
                      (agent-shell-codex-app-server--empty-granted-permissions)))
         (options (list
                   (agent-shell-codex-app-server--make-option
                    "allow_once" "Allow" "grant")
                   (agent-shell-codex-app-server--make-option
                    "allow_always" "Always Allow" "grantForSession")
                   (agent-shell-codex-app-server--make-option
                    "reject_once" "Reject" "decline"))))
    `((:options . ,options)
      (:payloads . (("grant" . ((permissions . ,granted)
                                (scope . "turn")))
                    ("grantForSession" . ((permissions . ,granted)
                                          (scope . "session")))
                    ("decline" . ((permissions . ,(agent-shell-codex-app-server--empty-granted-permissions))
                                  (scope . "turn"))))))))

(defun agent-shell-codex-app-server--elicitation-persist-supported-p (params scope)
  "Return non-nil when elicitation PARAMS support persistence SCOPE."
  (let ((persist (map-nested-elt params '(_meta persist))))
    (if (listp persist)
        (seq-contains-p persist scope)
      (equal persist scope))))

(defun agent-shell-codex-app-server--elicitation-request-options (params)
  "Return ACP-like options and response payloads for elicitation PARAMS."
  (let ((options (list
                  (agent-shell-codex-app-server--make-option
                   "allow_once" "Allow" "accept")))
        (payloads '(("accept" . ((action . "accept")
                                 (content . nil)
                                 (_meta . nil))))))
    (when (agent-shell-codex-app-server--elicitation-persist-supported-p
           params "session")
      (setq options
            (append options
                    (list (agent-shell-codex-app-server--make-option
                           "allow_always" "Allow for this session"
                           "acceptForSession")))
            payloads
            (append payloads
                    '(("acceptForSession" . ((action . "accept")
                                             (content . nil)
                                             (_meta . ((persist . "session")))))))))
    (when (agent-shell-codex-app-server--elicitation-persist-supported-p
           params "always")
      (setq options
            (append options
                    (list (agent-shell-codex-app-server--make-option
                           "allow_always" "Always allow" "acceptAlways")))
            payloads
            (append payloads
                    '(("acceptAlways" . ((action . "accept")
                                         (content . nil)
                                         (_meta . ((persist . "always")))))))))
    (setq options
          (append options
                  (list (agent-shell-codex-app-server--make-option
                         "reject_once" "Decline" "decline")
                        (agent-shell-codex-app-server--make-option
                         "reject_once" "Cancel" "cancel")))
          payloads
          (append payloads
                  '(("decline" . ((action . "decline")
                                  (content . nil)
                                  (_meta . nil)))
                    ("cancel" . ((action . "cancel")
                                 (content . nil)
                                 (_meta . nil))))))
    `((:options . ,options)
      (:payloads . ,payloads))))

(defun agent-shell-codex-app-server--decision-response (method decision)
  "Return the response value for approval METHOD and DECISION."
  (if (and (seq-contains-p '("execCommandApproval" "applyPatchApproval") method)
           (equal decision "denied"))
      '((denied . ((rejection . "Denied by user"))))
    decision))

(defun agent-shell-codex-app-server--decision-options (method decisions)
  "Return ACP-like options and response payloads for METHOD and DECISIONS."
  (let ((index 0)
        options
        payloads)
    (dolist (decision decisions)
      (when-let* ((spec (agent-shell-codex-app-server--decision-option-spec
                         decision))
                  (option-id (format "decision-%s" index)))
        (push (agent-shell-codex-app-server--make-option
               (map-elt spec :kind)
               (map-elt spec :name)
               option-id)
              options)
        (push (cons option-id
                    `((decision . ,(agent-shell-codex-app-server--decision-response
                                    method
                                    (map-elt spec :payload)))))
              payloads))
      (setq index (1+ index)))
    `((:options . ,(nreverse options))
      (:payloads . ,(nreverse payloads)))))

(defun agent-shell-codex-app-server--request-decisions (method params)
  "Return supported decision options for METHOD and PARAMS.

File-change approvals do not surface explicit cancel buttons because
`agent-shell' diff acceptance resolves the first `allow_once' action,
which would otherwise collide with a transport-local cancel shim."
  (let ((decisions
         (or (map-elt params 'availableDecisions)
             (pcase method
               ((or "item/commandExecution/requestApproval"
                    "item/fileChange/requestApproval")
                '("accept" "acceptForSession" "decline" "cancel"))
               ((or "execCommandApproval" "applyPatchApproval")
                '("approved" "approved_for_session" "denied" "abort"))
               (_ nil)))))
    (if (member method '("item/fileChange/requestApproval" "applyPatchApproval"))
        (seq-remove (lambda (decision)
                      (and (stringp decision)
                           (member decision '("cancel" "abort"))))
                    decisions)
      decisions)))

(defun agent-shell-codex-app-server--request-options (method params)
  "Translate app-server METHOD and PARAMS into ACP permission options.

Return an alist containing `:options' and `:payloads'."
  (pcase method
    ("item/permissions/requestApproval"
     (agent-shell-codex-app-server--permissions-request-options params))
    ("mcpServer/elicitation/request"
     (agent-shell-codex-app-server--elicitation-request-options params))
    (_
     (agent-shell-codex-app-server--decision-options
      method
      (agent-shell-codex-app-server--request-decisions method params)))))

(defun agent-shell-codex-app-server--approval-title (method params)
  "Build a permission title for METHOD with PARAMS."
  (pcase method
    ((or "item/commandExecution/requestApproval" "execCommandApproval")
     (if (equal (map-elt params 'kind) "writeStdin")
         (or (map-elt params 'reason) "Send terminal input")
       (agent-shell-codex-app-server--strip-kind-prefix
        (or (agent-shell-codex-app-server--command-text
             (map-elt params 'command))
            (map-elt params 'reason)
            "Run command")
        (agent-shell-codex-app-server--approval-kind method params))))
    ((or "item/fileChange/requestApproval" "applyPatchApproval")
     (or (map-elt params 'reason)
         (map-elt params 'grantRoot)
         "Apply patch"))
    ("item/permissions/requestApproval"
     (or (map-elt params 'reason)
         "Grant additional permissions"))
    ("mcpServer/elicitation/request"
     (or (map-elt params 'message)
         (when-let* ((server-name (map-elt params 'serverName)))
           (format "%s needs your approval" server-name))
         "MCP server needs your approval"))
    (_ (or (map-elt params 'reason)
           method))))

(defun agent-shell-codex-app-server--approval-kind (method params)
  "Return an ACP-like tool kind for app-server METHOD and PARAMS."
  (pcase method
    ((or "item/commandExecution/requestApproval" "execCommandApproval")
     (if (equal (map-elt params 'kind) "writeStdin")
         "execute"
       (agent-shell-codex-app-server--command-actions-kind
        (map-elt params 'commandActions))))
    ((or "item/fileChange/requestApproval" "applyPatchApproval") "edit")
    ("mcpServer/elicitation/request" "other")
    (_ "tool")))

(defun agent-shell-codex-app-server--approval-raw-input (method params)
  "Build ACP-like raw input for METHOD with PARAMS."
  (pcase method
    ((or "item/commandExecution/requestApproval" "execCommandApproval")
     (let ((command (agent-shell-codex-app-server--command-text
                     (map-elt params 'command))))
       (delq nil
             (list (when (and command (not (string-empty-p command)))
                     (cons 'command command))
                   (when-let* ((kind (map-elt params 'kind)))
                     (cons 'kind kind))
                   (when-let* ((approval-id (map-elt params 'approvalId)))
                     (cons 'approvalId approval-id))
                   (cons 'description
                         (if (equal (map-elt params 'kind) "writeStdin")
                             (or (map-elt params 'reason) "Send terminal input")
                           (agent-shell-codex-app-server--strip-kind-prefix
                            (or (map-elt params 'reason)
                                command)
                            (agent-shell-codex-app-server--approval-kind
                             method params))))))))
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
    ("mcpServer/elicitation/request"
     (delq nil `((serverName . ,(map-elt params 'serverName))
                 (mode . ,(map-elt params 'mode))
                 (message . ,(map-elt params 'message))
                 (url . ,(map-elt params 'url))
                 (elicitationId . ,(map-elt params 'elicitationId))
                 (requestedSchema . ,(map-elt params 'requestedSchema)))))
    (_ nil)))

(defun agent-shell-codex-app-server--translate-request (client request)
  "Translate app-server REQUEST for CLIENT."
  (let* ((method (map-elt request 'method))
         (params (or (map-elt request 'params) '()))
         (request-id (map-elt request 'id))
         (request-options (agent-shell-codex-app-server--request-options method params))
         (tool-call-id (or (map-elt params 'approvalId)
                           (map-elt params 'itemId)
                           (map-elt params 'callId)
                           request-id))
         (translated
          `((id . ,request-id)
            (method . "session/request_permission")
            (params . ((toolCall . ((toolCallId . ,tool-call-id)
                                    (title . ,(agent-shell-codex-app-server--approval-title method params))
                                    (status . "in_progress")
                                    (kind . ,(agent-shell-codex-app-server--approval-kind method params))
                                    (rawInput . ,(or (agent-shell-codex-app-server--approval-raw-input method params)
                                                     '()))))
                       (options . ,(map-elt request-options :options)))))))
    (puthash request-id `((:request . ,request)
                          (:tool-call-id . ,tool-call-id)
                          (:payloads . ,(map-elt request-options :payloads)))
             (map-elt client :pending-permissions))
    translated))

(defun agent-shell-codex-app-server--clear-pending-permission (client request-id)
  "Remove pending permission REQUEST-ID from CLIENT."
  (remhash request-id (map-elt client :pending-permissions)))

(defun agent-shell-codex-app-server--clear-pending-permissions-for-tool-call (client tool-call-id)
  "Remove pending permissions for TOOL-CALL-ID from CLIENT."
  (when tool-call-id
    (let (request-ids)
      (maphash (lambda (request-id pending)
                 (when (equal (map-elt pending :tool-call-id) tool-call-id)
                   (push request-id request-ids)))
               (map-elt client :pending-permissions))
      (dolist (request-id request-ids)
        (agent-shell-codex-app-server--clear-pending-permission client request-id)))))

(defun agent-shell-codex-app-server--clear-all-pending-permissions (client)
  "Remove all pending permissions from CLIENT."
  (clrhash (map-elt client :pending-permissions)))

(defun agent-shell-codex-app-server--respond-to-pending-prompt (client turn)
  "Resolve the active prompt in CLIENT using TURN."
  (when-let* ((pending (map-elt client :pending-prompt)))
    (map-put! client :pending-prompt nil)
    (map-put! client :active-turn-id nil)
    (when-let* ((on-success (map-elt pending :on-success)))
      (agent-shell-codex-app-server--call-with-buffer
       client
       (map-elt pending :buffer)
       on-success
       (agent-shell-codex-app-server--prompt-response client turn)))))

(defun agent-shell-codex-app-server--normalize-plan-entries (entries)
  "Return app-server plan ENTRIES in ACP-compatible shape.

Codex names the step text `step' and reports camelCase statuses, while
agent-shell renders the ACP `content' key and snake_case statuses.  For
example, ENTRIES

  \\='(((step . \"Run tests\") (status . \"inProgress\")))

returns

  \\='(((content . \"Run tests\") (status . \"in_progress\")))"
  (seq-map (lambda (entry)
             `((content . ,(or (map-elt entry 'content)
                               (map-elt entry 'step)))
               (status . ,(agent-shell-codex-app-server--normalize-status
                           (map-elt entry 'status)))))
           entries))

(defun agent-shell-codex-app-server--notice-message (params fallback)
  "Return a displayable notice message from PARAMS or FALLBACK."
  (let ((summary (map-elt params 'summary))
        (details (map-elt params 'details))
        (message (map-elt params 'message)))
    (cond
     ((and (stringp summary)
           (stringp details)
           (not (string-empty-p details)))
      (format "%s: %s" summary details))
     ((and (stringp summary) (not (string-empty-p summary)))
      summary)
     ((and (stringp message) (not (string-empty-p message)))
      message)
     ((and (stringp details) (not (string-empty-p details)))
      details)
     (t fallback))))

(defun agent-shell-codex-app-server--notification-for-current-thread-p (client params)
  "Return non-nil when PARAMS apply to CLIENT's current thread."
  (let ((thread-id (or (map-elt params 'threadId)
                       (map-nested-elt params '(thread id)))))
    (or (not thread-id)
        (not (map-elt client :thread-id))
        (equal thread-id (map-elt client :thread-id)))))

(defun agent-shell-codex-app-server--record-mcp-server-status (client params)
  "Record MCP server startup PARAMS for CLIENT and report startup failures."
  (let ((name (map-elt params 'name))
        (status (map-elt params 'status))
        (error-message (map-elt params 'error))
        (failure-reason (map-elt params 'failureReason)))
    (when (and (stringp name) (stringp status))
      (let ((statuses (map-elt client :mcp-server-statuses))
            (server-status `((:status . ,status)
                             (:error . ,error-message)
                             (:failure-reason . ,failure-reason))))
        (if (map-contains-key statuses name)
            (map-put! statuses name server-status)
          (setq statuses (map-insert statuses name server-status)))
        (map-put! client :mcp-server-statuses statuses))
      (when (equal status "failed")
        (agent-shell-codex-app-server--call-error-handlers
         client
         (if (and (stringp error-message)
                  (not (string-empty-p error-message)))
             (format "MCP server %s failed to start: %s" name error-message)
           (format "MCP server %s failed to start" name))
         params)))))

(defun agent-shell-codex-app-server--markdown-headings-only-p (text)
  "Return non-nil when TEXT contains only Markdown headings and blank lines."
  (when-let* ((lines (split-string text "\n" t "[ \t]+")))
    (seq-every-p
     (lambda (line)
       (string-match-p "\\`[ \t]*#+[ \t]+[^ \t\n]" line))
     lines)))

(defun agent-shell-codex-app-server--dispatch-agent-message (client text)
  "Dispatch agent message TEXT from CLIENT as an ACP update."
  (unless (string-empty-p text)
    (agent-shell-codex-app-server--dispatch-notification
     client
     `((method . "session/update")
       (params . ((update . ((sessionUpdate . "agent_message_chunk")
                             (content . ((type . "text")
                                         (text . ,text)))))))))))

(defun agent-shell-codex-app-server--flush-pending-agent-message (client)
  "Dispatch CLIENT's pending heading-only agent message, if any."
  (when-let* ((pending (map-elt client :pending-agent-message))
              (text (map-elt pending :text)))
    (map-put! client :pending-agent-message nil)
    ;; A trailing newline on a heading-only first chunk is hidden by the
    ;; incremental renderer.  Without it, the incomplete heading stays visible.
    (agent-shell-codex-app-server--dispatch-agent-message
     client
     (string-trim-right text))))

(defun agent-shell-codex-app-server--handle-agent-message-delta (client params)
  "Translate an agent message delta in PARAMS for CLIENT."
  (let* ((item-id (map-elt params 'itemId))
         (delta (or (map-elt params 'delta) ""))
         (pending (map-elt client :pending-agent-message)))
    (when (and pending
               (not (equal item-id (map-elt pending :item-id))))
      (agent-shell-codex-app-server--flush-pending-agent-message client)
      (setq pending nil))
    (cond
     (pending
      (map-put! client :pending-agent-message nil)
      (agent-shell-codex-app-server--dispatch-agent-message
       client
       (concat (map-elt pending :text) delta)))
     ((agent-shell-codex-app-server--markdown-headings-only-p delta)
      (map-put! client :pending-agent-message
                `((:item-id . ,item-id)
                  (:text . ,delta))))
     (t
      (agent-shell-codex-app-server--dispatch-agent-message client delta)))))

(defun agent-shell-codex-app-server--turn-bound-notification-method-p (method)
  "Return non-nil when METHOD is scoped to a single app-server turn."
  (member method '("turn/plan/updated"
                   "model/rerouted"
                   "item/agentMessage/delta"
                   "item/reasoning/textDelta"
                   "item/reasoning/summaryTextDelta"
                   "item/reasoning/summaryPartAdded"
                   "item/started"
                   "item/commandExecution/outputDelta"
                   "item/fileChange/outputDelta"
                   "item/mcpToolCall/progress"
                   "item/completed")))

(defun agent-shell-codex-app-server--dismissed-turn-notification-p (client
                                                                    notification)
  "Return non-nil when NOTIFICATION belongs to a dismissed turn for CLIENT."
  (let* ((method (map-elt notification 'method))
         (item (map-nested-elt notification '(params item)))
         (late-sub-agent-completion
          (and (member method '("item/started" "item/completed"))
               (equal (map-elt item 'type) "subAgentActivity")
               (equal (map-elt item 'kind) "completed"))))
    (and (agent-shell-codex-app-server--turn-bound-notification-method-p method)
         (not late-sub-agent-completion)
         (agent-shell-codex-app-server--dismissed-turn-id-p
          client
          (map-nested-elt notification '(params turnId))))))

(defun agent-shell-codex-app-server--handle-notification (client notification)
  "Handle app-server NOTIFICATION for CLIENT."
  (if (or (not (agent-shell-codex-app-server--notification-for-current-thread-p
                client
                (or (map-elt notification 'params) '())))
          (agent-shell-codex-app-server--dismissed-turn-notification-p
           client
           notification))
      nil
    (pcase (map-elt notification 'method)
      ("error"
       (let* ((params (or (map-elt notification 'params) '()))
              (message (or (agent-shell-codex-app-server--error-message-text
                            (map-elt params 'message))
                           (agent-shell-codex-app-server--error-message-text
                            (map-elt params 'error))
                           "Codex app-server error")))
         (agent-shell-codex-app-server--call-error-handlers client message params)))
      ((or "warning" "guardianWarning")
       (let ((params (or (map-elt notification 'params) '())))
         (agent-shell-codex-app-server--call-error-handlers
          client
          (agent-shell-codex-app-server--notice-message
           params
           "Codex app-server warning")
          params)))
      ((or "configWarning" "deprecationNotice")
       (let ((params (or (map-elt notification 'params) '())))
         (agent-shell-codex-app-server--call-error-handlers
          client
          (agent-shell-codex-app-server--notice-message
           params
           "Codex app-server notice")
          params)))
      ("model/rerouted"
       (let ((params (or (map-elt notification 'params) '())))
         (when (agent-shell-codex-app-server--notification-for-current-thread-p
                client params)
           (agent-shell-codex-app-server--call-error-handlers
            client
            (format "Codex rerouted this turn from %s to %s (%s)"
                    (or (map-elt params 'fromModel) "an unknown model")
                    (or (map-elt params 'toModel) "an unknown model")
                    (or (map-elt params 'reason) "unspecified reason"))
            params))))
      ("thread/started"
       (map-put! client :thread-id
                 (or (map-nested-elt notification '(params thread id))
                     (map-nested-elt notification '(params threadId)))))
      ("thread/name/updated"
       (agent-shell-codex-app-server--update-session-title
        client
        (or (map-elt notification 'params) '())))
      ("thread/settings/updated"
       (let ((params (or (map-elt notification 'params) '())))
         (when (agent-shell-codex-app-server--notification-for-current-thread-p
                client params)
           (let* ((settings (map-nested-elt notification '(params threadSettings)))
                  (model-id (map-elt settings 'model))
                  (effort (agent-shell-codex-app-server--resolve-reasoning-effort
                           client
                           model-id
                           (map-elt settings 'effort)))
                  (previous-effort (map-elt client :reasoning-effort)))
             (when model-id
               (map-put! client :current-model-id model-id))
             (when effort
               (map-put! client :reasoning-effort effort))
             (when (and effort
                        (not (equal effort previous-effort)))
                (agent-shell-codex-app-server--dispatch-current-mode-update
                 client
                 effort))))))
      ("mcpServer/startupStatus/updated"
       (let ((params (or (map-elt notification 'params) '())))
         (when (agent-shell-codex-app-server--notification-for-current-thread-p
                client params)
           (agent-shell-codex-app-server--record-mcp-server-status client params))))
      ("serverRequest/resolved"
       (agent-shell-codex-app-server--clear-pending-permission
        client
        (map-nested-elt notification '(params requestId))))
      ("turn/started"
       (when-let* ((turn-id (map-nested-elt notification '(params turn id))))
         (cond
          ;; The turn/start reply already interrupted this turn.  Leaving it
          ;; as the active turn would strand an id no turn/completed clears.
          ((agent-shell-codex-app-server--dismissed-turn-id-p client turn-id)
           (map-put! client :interrupt-next-turn nil))
          ((map-elt client :interrupt-next-turn)
           (map-put! client :interrupt-next-turn nil)
           (agent-shell-codex-app-server--interrupt-turn client turn-id))
          (t
           (map-put! client :active-turn-id turn-id)))))
      ("thread/tokenUsage/updated"
       (let ((token-usage (map-nested-elt notification '(params tokenUsage))))
         (map-put! client :latest-token-usage token-usage)
         (when-let* ((translated
                      (agent-shell-codex-app-server--usage-notification token-usage)))
           (agent-shell-codex-app-server--dispatch-notification client translated))))
      ("turn/plan/updated"
       (agent-shell-codex-app-server--dispatch-notification
        client
        `((method . "session/update")
          (params . ((update . ((sessionUpdate . "plan")
                                (entries . ,(agent-shell-codex-app-server--normalize-plan-entries
                                             (or (map-nested-elt notification '(params plan))
                                                 '()))))))))))
      ("item/agentMessage/delta"
       (agent-shell-codex-app-server--handle-agent-message-delta
        client
        (map-elt notification 'params)))
      ("item/reasoning/textDelta"
       (agent-shell-codex-app-server--dispatch-notification
        client
        `((method . "session/update")
          (params . ((update . ((sessionUpdate . "agent_thought_chunk")
                                (content . ((type . "text")
                                            (text . ,(or (map-nested-elt notification '(params delta)) "")))))))))))
      ("item/reasoning/summaryTextDelta"
       (agent-shell-codex-app-server--dispatch-notification
        client
        `((method . "session/update")
          (params . ((update . ((sessionUpdate . "agent_thought_chunk")
                                (content . ((type . "text")
                                            (text . ,(or (map-nested-elt notification '(params delta)) "")))))))))))
      ("item/reasoning/summaryPartAdded"
       (when (> (or (map-nested-elt notification '(params summaryIndex)) 0) 0)
         (agent-shell-codex-app-server--dispatch-notification
          client
          '((method . "session/update")
            (params . ((update . ((sessionUpdate . "agent_thought_chunk")
                                  (content . ((type . "text")
                                              (text . "\n\n")))))))))))
      ("item/started"
       (agent-shell-codex-app-server--flush-pending-agent-message client)
       (let ((item (map-nested-elt notification '(params item))))
         (when (member (map-elt item 'type)
                       '("commandExecution" "fileChange" "mcpToolCall"
                         "dynamicToolCall" "collabAgentToolCall"
                         "subAgentActivity" "webSearch" "imageView" "sleep"
                         "imageGeneration"))
           (agent-shell-codex-app-server--save-tool-entry client item "inProgress")
           (agent-shell-codex-app-server--dispatch-notification
            client
            (agent-shell-codex-app-server--translate-tool-notification
             "tool_call" client item "inProgress")))))
      ("item/commandExecution/outputDelta"
       (when-let* ((translated
                    (agent-shell-codex-app-server--translate-command-output
                     client (map-elt notification 'params))))
         (agent-shell-codex-app-server--dispatch-notification client translated)))
      ("item/fileChange/outputDelta"
       (when-let* ((translated
                    (agent-shell-codex-app-server--translate-command-output
                     client (map-elt notification 'params))))
         (agent-shell-codex-app-server--dispatch-notification client translated)))
      ("item/mcpToolCall/progress"
       (when-let* ((params (map-elt notification 'params))
                   (translated
                    (agent-shell-codex-app-server--tool-text-update
                     client
                     (map-elt params 'itemId)
                     (map-elt params 'message))))
         (agent-shell-codex-app-server--dispatch-notification client translated)))
      ("item/fileChange/patchUpdated"
       (let* ((params (map-elt notification 'params))
              (item-id (map-elt params 'itemId)))
         (when item-id
           (let* ((item `((id . ,item-id)
                          (type . "fileChange")
                          (changes . ,(or (map-elt params 'changes) '()))))
                  (entry (agent-shell-codex-app-server--get-tool-entry client item-id))
                  (status (or (map-elt entry :status) "inProgress")))
             (agent-shell-codex-app-server--save-tool-entry client item status)
             (agent-shell-codex-app-server--dispatch-notification
              client
              (agent-shell-codex-app-server--translate-tool-notification
               "tool_call_update" client item status))))))
      ("item/completed"
       (agent-shell-codex-app-server--flush-pending-agent-message client)
       (let ((item (map-nested-elt notification '(params item))))
         (cond
          ((and (equal (map-elt item 'type) "agentMessage")
                (equal (map-elt item 'delivery) "async"))
           (agent-shell-codex-app-server--dispatch-agent-message
            client
            (or (map-elt item 'text) ""))
           (agent-shell-codex-app-server--show-async-questions client item))
          ((equal (map-elt item 'type) "plan")
           (agent-shell-codex-app-server--dispatch-agent-message
            client
            (map-elt item 'text)))
          ((member (map-elt item 'type)
                   '("commandExecution" "fileChange" "mcpToolCall"
                     "dynamicToolCall" "collabAgentToolCall"
                     "subAgentActivity" "webSearch" "imageView" "sleep"
                     "imageGeneration"))
           (let ((item-id (map-elt item 'id)))
             (agent-shell-codex-app-server--clear-pending-permissions-for-tool-call
              client item-id)
             (if-let* ((output (map-elt item 'aggregatedOutput)))
                 (puthash item-id output (map-elt client :tool-outputs))
               (when-let* ((output
                            (agent-shell-codex-app-server--tool-output-text
                             client item-id)))
                 (puthash item-id output (map-elt client :tool-outputs))))
             (unwind-protect
                 (agent-shell-codex-app-server--dispatch-notification
                  client
                  (agent-shell-codex-app-server--translate-tool-notification
                   "tool_call_update"
                   client
                   item
                   (or (map-elt item 'status) "completed")
                   t))
               (agent-shell-codex-app-server--clear-tool-item client item-id)))))))
      ("turn/completed"
       (agent-shell-codex-app-server--flush-pending-agent-message client)
       (let* ((turn (map-nested-elt notification '(params turn)))
              (turn-id (map-elt turn 'id))
              (current-turn-id (agent-shell-codex-app-server--current-turn-id client)))
         (unless (or (agent-shell-codex-app-server--dismissed-turn-id-p client turn-id)
                     (and turn-id
                          current-turn-id
                          (not (equal turn-id current-turn-id))))
           (agent-shell-codex-app-server--clear-all-pending-permissions client)
           (unless (map-elt client :pending-prompt)
             (map-put! client :active-turn-id nil))
           (agent-shell-codex-app-server--respond-to-pending-prompt
            client
            turn)
           (agent-shell-codex-app-server--clear-tool-state client))
         (agent-shell-codex-app-server--dismiss-turn client turn-id)))
      (_ nil))))

(defun agent-shell-codex-app-server--handle-response (client response)
  "Handle raw JSON-RPC RESPONSE for CLIENT."
  (let* ((id (map-elt response 'id))
         (pending (gethash id (map-elt client :pending-requests))))
    (when pending
      (remhash id (map-elt client :pending-requests))
      (if-let* ((error (map-elt response 'error)))
          (when-let* ((on-failure (map-elt pending :on-failure)))
            (agent-shell-codex-app-server--call-with-buffer
             client
             (map-elt pending :buffer)
             on-failure
             (agent-shell-codex-app-server--make-error
              (or (map-elt error 'message)
                  "Codex app-server request failed")
              error)
             response))
        (when-let* ((on-success (map-elt pending :on-success)))
          (agent-shell-codex-app-server--call-with-buffer
           client
           (map-elt pending :buffer)
           on-success
           (map-elt response 'result)))))))

(defun agent-shell-codex-app-server--route-message (client message)
  "Route decoded MESSAGE for CLIENT."
  (cond
   ((and (map-contains-key message 'method)
         (map-contains-key message 'id))
    (cond
     ((equal (map-elt message 'method) "currentTime/read")
      (agent-shell-codex-app-server--send-rpc-response
       :client client
       :request-id (map-elt message 'id)
       :result `((currentTimeAt . ,(floor (float-time))))))
     ((equal (map-elt message 'method) "item/tool/requestUserInput")
      (agent-shell-codex-app-server--handle-user-input-request client message))
     ((equal (map-elt message 'method) "mcpServer/elicitation/request")
      (let ((params (or (map-elt message 'params) '())))
        (cond
         ((agent-shell-codex-app-server--mcp-form-request-p params)
          (agent-shell-codex-app-server--handle-mcp-form-request client message))
         ((equal (map-elt params 'mode) "openai/form")
          (agent-shell-codex-app-server--decline-unsupported-mcp-form
           client message))
         (t
          (agent-shell-codex-app-server--dispatch-request
           client
           (agent-shell-codex-app-server--translate-request client message))))))
     ((agent-shell-codex-app-server--approval-request-method-p
       (map-elt message 'method))
      (agent-shell-codex-app-server--dispatch-request
       client
       (agent-shell-codex-app-server--translate-request client message)))
     (t
      (let ((error-message
             (agent-shell-codex-app-server--unsupported-request-message
              (map-elt message 'method))))
        (agent-shell-codex-app-server--send-rpc-error
         :client client
         :request-id (map-elt message 'id)
         :code -32601
         :message error-message)
        (agent-shell-codex-app-server--call-error-handlers
         client
         error-message
         message)))))
   ((map-contains-key message 'method)
    (agent-shell-codex-app-server--handle-notification client message))
   ((map-contains-key message 'id)
    (agent-shell-codex-app-server--handle-response client message))
   (t
    (agent-shell-codex-app-server--call-error-handlers
     client
     "Received malformed JSON-RPC payload"
     message))))

(defun agent-shell-codex-app-server--drain-message-queue (client)
  "Process queued decoded messages for CLIENT."
  (unless (map-elt client :message-queue-busy)
    (when-let* ((timer (map-elt client :message-drain-timer))
                ((memq timer timer-list)))
      (cancel-timer timer))
    (map-put! client :message-drain-timer nil)
    (unwind-protect
        (progn
          (map-put! client :message-queue-busy t)
          (while-let ((queue (map-elt client :message-queue)))
            (let ((message (seq-first queue)))
              (map-put! client :message-queue (seq-rest queue))
              (condition-case err
                  (agent-shell-codex-app-server--route-message client message)
                (error
                 (run-at-time
                  0 nil
                  (lambda ()
                    (agent-shell-codex-app-server--call-error-handlers
                     client
                     (format "Failed to handle app-server message: %s"
                             (error-message-string err))
                     message))))))))
      (map-put! client :message-queue-busy nil)
      (when (map-elt client :message-queue)
        (agent-shell-codex-app-server--schedule-message-drain client)))))

(defun agent-shell-codex-app-server--schedule-message-drain (client)
  "Schedule queued message processing for CLIENT."
  ;; Migrate clients created before drain timers were tracked separately.
  (unless (assq :message-drain-timer client)
    (nconc client (list (cons :message-drain-timer nil))))
  (when-let* ((timer (map-elt client :message-drain-timer))
              ((not (memq timer timer-list))))
    (map-put! client :message-drain-timer nil))
  (unless (or (map-elt client :message-queue-busy)
              (map-elt client :message-drain-timer))
    (map-put! client :message-drain-timer
              (run-at-time
               0 nil
               #'agent-shell-codex-app-server--drain-message-queue
               client))))

(defun agent-shell-codex-app-server--enqueue-message (client message)
  "Queue decoded MESSAGE for CLIENT."
  (map-put! client :message-queue
            (nconc (map-elt client :message-queue)
                   (list message)))
  (agent-shell-codex-app-server--schedule-message-drain client))

(defun agent-shell-codex-app-server--process-filter (client output)
  "Handle process OUTPUT for CLIENT."
  (let ((pending (concat (map-elt client :partial-output) output)))
    (while (string-match "\n" pending)
      (let* ((line (substring pending 0 (match-beginning 0)))
             (rest (substring pending (match-end 0))))
        (setq pending rest)
        (unless (or (string-empty-p (string-trim line))
                    (agent-shell-codex-app-server--consume-echoed-line client line))
          (condition-case err
              (agent-shell-codex-app-server--enqueue-message
               client
               (agent-shell-codex-app-server--decode-message line))
            (error
             (run-at-time
              0 nil
              (lambda ()
                (agent-shell-codex-app-server--call-error-handlers
                 client
                 (format "Failed to decode app-server payload: %s"
                         (error-message-string err))
                 line))))))))
    (map-put! client :partial-output pending)))

(defun agent-shell-codex-app-server--process-sentinel (client event)
  "Handle CLIENT process EVENT."
  (unless (process-live-p (map-elt client :process))
    (agent-shell-codex-app-server--clear-tool-state client)
    (when-let* ((pending (map-elt client :pending-prompt))
                (on-failure (map-elt pending :on-failure)))
      (agent-shell-codex-app-server--call-with-buffer
       client
       (map-elt pending :buffer)
       on-failure
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

;;;###autoload
(cl-defun agent-shell-codex-app-server-subscribe-to-errors (&key client on-error buffer)
  "Subscribe CLIENT to errors using ON-ERROR in BUFFER."
  (unless on-error
    (error ":on-error is required"))
  (push (lambda (error)
          (agent-shell-codex-app-server--call-with-buffer
           client buffer on-error error))
        (alist-get :error-handlers client))
  on-error)

;;;###autoload
(cl-defun agent-shell-codex-app-server-subscribe-to-notifications (&key client on-notification buffer)
  "Subscribe CLIENT to translated notifications using ON-NOTIFICATION in BUFFER."
  (unless on-notification
    (error ":on-notification is required"))
  (push (lambda (notification)
          (agent-shell-codex-app-server--call-with-buffer
           client buffer on-notification notification))
        (alist-get :notification-handlers client))
  on-notification)

;;;###autoload
(cl-defun agent-shell-codex-app-server-subscribe-to-requests (&key client on-request buffer)
  "Subscribe CLIENT to translated requests using ON-REQUEST in BUFFER."
  (unless on-request
    (error ":on-request is required"))
  (push (lambda (request)
          (agent-shell-codex-app-server--call-with-buffer
           client buffer on-request request))
        (alist-get :request-handlers client))
  on-request)

(cl-defun agent-shell-codex-app-server--fetch-models-page (&key client
                                                                cursor
                                                                collected
                                                                on-success)
  "Fetch one `model/list' page for CLIENT.

Reuse CURSOR, COLLECTED, and ON-SUCCESS until all pages are loaded."
  (agent-shell-codex-app-server--send-rpc-request
   :client client
   :method "model/list"
   :params (if cursor
               `((cursor . ,cursor))
             (agent-shell-codex-app-server--json-empty-object))
   :on-success (lambda (result)
                 (let* ((page (append (or (map-elt result 'data) '()) nil))
                        (all-models (append collected page))
                        (next-cursor (map-elt result 'nextCursor)))
                   (if next-cursor
                       (agent-shell-codex-app-server--fetch-models-page
                        :client client
                        :cursor next-cursor
                        :collected all-models
                        :on-success on-success)
                     (map-put! client :available-models all-models)
                     (when on-success
                       (funcall on-success)))))
   :on-failure (lambda (_error _raw)
                 (map-put! client :available-models (or collected '()))
                 (when on-success
                   (funcall on-success)))))

(defun agent-shell-codex-app-server--fetch-models (client on-success)
  "Refresh model metadata for CLIENT, then call ON-SUCCESS."
  (agent-shell-codex-app-server--fetch-models-page
   :client client
   :collected nil
   :on-success on-success))

(defun agent-shell-codex-app-server--thread-params (client cwd)
  "Return common thread parameters for CLIENT using CWD."
  `((cwd . ,cwd)
    (approvalPolicy . ,(map-elt client :approval-policy))
    (sandbox . ,(map-elt client :sandbox-mode))
    ,@(when-let* ((model-id (map-elt client :current-model-id)))
        (list (cons 'model model-id)))))

(cl-defun agent-shell-codex-app-server--list-threads-page (&key client
                                                                cwd
                                                                buffer
                                                                cursor
                                                                collected
                                                                on-success
                                                                on-failure)
  "Fetch one `thread/list' page for CLIENT in CWD.

Use BUFFER, CURSOR, COLLECTED, ON-SUCCESS, and ON-FAILURE until all
pages are loaded."
  (agent-shell-codex-app-server--send-rpc-request
   :client client
   :method "thread/list"
   :buffer buffer
   :params (append `((cwd . ,cwd)
                     (archived . ,(agent-shell-codex-app-server--json-bool nil))
                     (sortKey . "updated_at")
                     (limit . 25))
                   (when cursor
                     (list (cons 'cursor cursor))))
   :on-success (lambda (result)
                 (let* ((page (append (or (map-elt result 'data) '()) nil))
                        (all-threads (append collected page))
                        (next-cursor (map-elt result 'nextCursor)))
                   (if next-cursor
                       (agent-shell-codex-app-server--list-threads-page
                        :client client
                        :cwd cwd
                        :buffer buffer
                        :cursor next-cursor
                        :collected all-threads
                        :on-success on-success
                        :on-failure on-failure)
                     (when on-success
                       (funcall on-success
                                `((data . ,all-threads)
                                  (nextCursor . nil)))))))
   :on-failure on-failure))

;;;###autoload
(cl-defun agent-shell-codex-app-server-send-request (&key client
                                                          request
                                                          buffer
                                                          on-success
                                                          on-failure
                                                          sync)
  "Send translated ACP REQUEST through app-server CLIENT."
  (when sync
    (error "Synchronous requests are not supported by codex app-server transport"))
  (let* ((method (map-elt request :method))
         (params (or (map-elt request :params) '())))
    (pcase method
      ("initialize"
       (agent-shell-codex-app-server--send-rpc-request
        :client client
        :method "initialize"
        :buffer buffer
        :params `((clientInfo . ((name . ,agent-shell-codex-app-server--client-name)
                                 (title . "Emacs Agent Shell")
                                 (version . ,(or (bound-and-true-p agent-shell--version)
                                                 "unknown"))))
                  (capabilities . ((experimentalApi . t))))
        :on-success (lambda (_result)
                      (agent-shell-codex-app-server--send-rpc-notification
                       :client client
                       :method "initialized"
                       :params nil)
                      (when on-success
                        (funcall on-success
                                 '((sessionCapabilities . ((list . t)
                                                           (resume . t)
                                                           (fork . t)))
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
           :buffer buffer
           :params (agent-shell-codex-app-server--thread-params
                    client
                    (map-elt params 'cwd))
           :on-success (lambda (result)
                         (when on-success
                           (funcall on-success
                                    (agent-shell-codex-app-server--session-response
                                     client result))
                           (agent-shell-codex-app-server--ensure-session-title-slot
                            client)))
           :on-failure on-failure))))
      ("session/list"
       (agent-shell-codex-app-server--list-threads-page
        :client client
        :buffer buffer
        :cwd (map-elt params 'cwd)
        :collected nil
        :on-success (lambda (result)
                      (when on-success
                        (funcall on-success
                                 (agent-shell-codex-app-server--session-list-response
                                  result))))
        :on-failure on-failure))
      ((or "session/resume" "session/load")
       (agent-shell-codex-app-server--prepare-resume-buffer client)
       (agent-shell-codex-app-server--fetch-models
        client
        (lambda ()
          (agent-shell-codex-app-server--send-rpc-request
           :client client
           :method "thread/resume"
           :buffer buffer
           ;; Codex restores model history independently of response turns.
           :params (append (list (cons 'threadId (map-elt params 'sessionId))
                                 (cons 'excludeTurns t))
                           (agent-shell-codex-app-server--thread-params
                            client
                            (map-elt params 'cwd)))
           :on-success (lambda (result)
                         (when on-success
                           (funcall on-success
                                    (agent-shell-codex-app-server--session-response
                                     client result))
                           (agent-shell-codex-app-server--ensure-session-title-slot
                            client)))
           :on-failure on-failure))))
      ("session/fork"
       (agent-shell-codex-app-server--fetch-models
        client
        (lambda ()
          (agent-shell-codex-app-server--send-rpc-request
           :client client
           :method "thread/fork"
           :buffer buffer
           :params (append
                    (list (cons 'threadId (map-elt params 'sessionId))
                          (cons 'excludeTurns t))
                    (agent-shell-codex-app-server--thread-params
                     client
                     (map-elt params 'cwd)))
           :on-success (lambda (result)
                         (when on-success
                           (funcall on-success
                                    (agent-shell-codex-app-server--session-response
                                     client result))
                           (agent-shell-codex-app-server--ensure-session-title-slot
                            client)))
           :on-failure on-failure))))
      ("session/set_model"
       (let* ((model-id (map-elt params 'modelId))
              (resolved-effort
               (agent-shell-codex-app-server--resolve-reasoning-effort
                client model-id (map-elt client :reasoning-effort))))
         (agent-shell-codex-app-server--send-rpc-request
          :client client
          :method "thread/settings/update"
          :buffer buffer
          :params `((threadId . ,(or (map-elt client :thread-id)
                                     (map-elt params 'sessionId)))
                    (model . ,model-id)
                    (effort . ,resolved-effort))
          :on-success (lambda (_result)
                        (let ((previous-effort
                               (map-elt client :reasoning-effort)))
                          (map-put! client :current-model-id model-id)
                          (map-put! client :reasoning-effort resolved-effort)
                          (when on-success
                            (funcall on-success `((modelId . ,model-id))))
                          (unless (equal resolved-effort previous-effort)
                            (agent-shell-codex-app-server--dispatch-current-mode-update
                             client
                             resolved-effort))))
          :on-failure on-failure)))
      ("session/set_mode"
       (let* ((mode-id (map-elt params 'modeId))
              (effort (agent-shell-codex-app-server--mode-id-to-reasoning-effort
                       mode-id))
              (model-id (map-elt client :current-model-id)))
         (cond
          ((not effort)
           (let ((message
                  (format "Unsupported Codex app-server session mode: %s"
                          mode-id)))
             (if on-failure
                 (funcall on-failure
                          (agent-shell-codex-app-server--make-error message)
                          nil)
               (agent-shell-codex-app-server--call-error-handlers
                client
                message))))
          ((not (agent-shell-codex-app-server--reasoning-effort-supported-p
                 client effort model-id))
           (let ((message
                  (format "Reasoning effort %s is not supported by %s"
                          effort
                          (or (map-elt (agent-shell-codex-app-server--find-model
                                        client model-id)
                                       'displayName)
                              model-id
                              "the current model"))))
             (if on-failure
                 (funcall on-failure
                          (agent-shell-codex-app-server--make-error message)
                          nil)
               (agent-shell-codex-app-server--call-error-handlers
                client
                message))))
          (t
           (agent-shell-codex-app-server--send-rpc-request
            :client client
            :method "thread/settings/update"
            :buffer buffer
            :params `((threadId . ,(or (map-elt client :thread-id)
                                       (map-elt params 'sessionId)))
                      (effort . ,effort))
            :on-success (lambda (_result)
                          (map-put! client :reasoning-effort effort)
                          (when on-success
                            (funcall on-success `((modeId . ,mode-id)))))
            :on-failure on-failure)))))
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
          :buffer buffer
          :params `((threadId . ,(or (map-elt client :thread-id)
                                     (map-elt params 'sessionId)))
                    (input . ,(agent-shell-codex-app-server--translate-prompt-blocks
                               (map-elt params 'prompt)))
                    ,@(when-let* ((model-id (map-elt client :current-model-id)))
                        (list (cons 'model model-id)))
                    ,@(when-let* ((effort (map-elt client :reasoning-effort)))
                        (list (cons 'effort effort))))
          :on-success (lambda (result)
                        (let ((turn-id (map-nested-elt result '(turn id))))
                          (if (agent-shell-codex-app-server--dismissed-turn-id-p
                               client turn-id)
                              ;; The turn was interrupted or completed before
                              ;; its start reply arrived, so no turn/completed
                              ;; is left to resolve the prompt.
                              (when on-success
                                (funcall on-success
                                         (agent-shell-codex-app-server--prompt-response
                                          client
                                          (agent-shell-codex-app-server--cancelled-turn
                                           turn-id))))
                            (map-put! client :active-turn-id turn-id)
                            (map-put! client :pending-prompt
                                      `((:turn-id . ,turn-id)
                                        (:buffer . ,buffer)
                                        (:on-success . ,on-success)
                                        (:on-failure . ,on-failure))))))
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
  (delq nil
        (list (when-let* ((network (map-elt permissions 'network)))
                (cons 'network network))
              (when-let* ((file-system (map-elt permissions 'fileSystem)))
                (cons 'fileSystem file-system))
              (when-let* ((macos (map-elt permissions 'macos)))
                (cons 'macos macos)))))

(defun agent-shell-codex-app-server--default-permission-response (method
                                                                  params
                                                                  option-id
                                                                  cancelled)
  "Build a fallback response payload for METHOD, PARAMS, OPTION-ID, and CANCELLED."
  (let ((decision
         (cond
          (cancelled "cancel")
          ((equal option-id "cancel") "cancel")
          ((equal option-id "decline") "decline")
          ((equal option-id "grant") "grant")
          ((equal option-id "grantForSession") "acceptForSession")
          ((equal option-id "acceptForSession") "acceptForSession")
          (t "accept"))))
    (pcase method
      ((or "item/commandExecution/requestApproval"
           "item/fileChange/requestApproval")
       `((decision . ,decision)))
      ("item/permissions/requestApproval"
       `((permissions . ,(if (member decision '("accept" "grant" "acceptForSession"))
                             (or (agent-shell-codex-app-server--grant-permissions
                                  (map-elt params 'permissions))
                                 (agent-shell-codex-app-server--empty-granted-permissions))
                           (agent-shell-codex-app-server--empty-granted-permissions)))
         (scope . ,(if (equal decision "acceptForSession")
                       "session"
                     "turn"))))
      ("mcpServer/elicitation/request"
       `((action . ,(pcase decision
                      ("decline" "decline")
                      ("cancel" "cancel")
                      (_ "accept")))
         (content . nil)
         (_meta . nil)))
      ("execCommandApproval"
       `((decision . ,(pcase decision
                        ("accept" "approved")
                        ("acceptForSession" "approved_for_session")
                        ("decline" '((denied . ((rejection . "Denied by user")))))
                        (_ "abort")))))
      ("applyPatchApproval"
       `((decision . ,(pcase decision
                        ("accept" "approved")
                        ("acceptForSession" "approved_for_session")
                        ("decline" '((denied . ((rejection . "Denied by user")))))
                        (_ "abort")))))
      (_
       `((decision . ,decision))))))

;;;###autoload
(cl-defun agent-shell-codex-app-server-send-permission-response (&key client
                                                                      request-id
                                                                      option-id
                                                                      cancelled)
  "Respond to a pending app-server permission request for CLIENT.

Use REQUEST-ID with OPTION-ID or CANCELLED to pick the response."
  (let ((request (gethash request-id (map-elt client :pending-permissions))))
    (when request
      (agent-shell-codex-app-server--clear-pending-permission client request-id)
      (let* ((original-request (or (map-elt request :request) request))
             (payloads (map-elt request :payloads))
             (method (map-elt original-request 'method))
             (params (or (map-elt original-request 'params) '()))
             (result (or (and option-id
                              (cdr (assoc option-id payloads)))
                         (agent-shell-codex-app-server--default-permission-response
                          method params option-id cancelled))))
        (agent-shell-codex-app-server--send-rpc-response
         :client client
         :request-id request-id
         :result result)))))

(cl-defun agent-shell-codex-app-server-send-response (&key client response)
  "Translate ACP RESPONSE for CLIENT into an app-server response."
  (unless response
    (error ":response is required"))
  (when-let* ((result (map-elt response :result))
              (outcome-info (map-elt result 'outcome))
              (request-id (map-elt response :request-id)))
    (agent-shell-codex-app-server-send-permission-response
     :client client
     :request-id request-id
     :option-id (map-elt outcome-info 'optionId)
     :cancelled (equal (map-elt outcome-info 'outcome) "cancelled"))))

(cl-defun agent-shell-codex-app-server-send-notification (&key client notification sync)
  "Translate ACP NOTIFICATION for CLIENT into an app-server notification."
  (when sync
    (error "Synchronous notifications are not supported by codex app-server transport"))
  (pcase (map-elt notification :method)
    ("session/cancel"
     (agent-shell-codex-app-server-interrupt client))
    (_
     (error "Unsupported ACP notification for Codex app-server: %s"
            (map-elt notification :method)))))

;;;###autoload
(defun agent-shell-codex-app-server-interrupt (client)
  "Interrupt the current turn for CLIENT."
  (let* ((pending (map-elt client :pending-prompt))
         (turn-id (agent-shell-codex-app-server--current-turn-id client)))
    (map-put! client :pending-agent-message nil)
    (agent-shell-codex-app-server--cancel-pending-turn-start client)
    (agent-shell-codex-app-server--clear-tool-state client)
    (map-put! client :pending-prompt nil)
    (map-put! client :active-turn-id nil)
    (when pending
      (when-let* ((on-success (map-elt pending :on-success)))
        (agent-shell-codex-app-server--call-with-buffer
         client
         (map-elt pending :buffer)
         on-success
         (agent-shell-codex-app-server--prompt-response
          client
          (agent-shell-codex-app-server--cancelled-turn turn-id)))))
    (agent-shell-codex-app-server--interrupt-turn client turn-id)))

(cl-defun agent-shell-codex-app-server-shutdown (&key client)
  "Shut down CLIENT."
  (when client
    (map-put! client :shutting-down t)
    (map-put! client :pending-agent-message nil)
    (map-put! client :pending-prompt nil)
    (map-put! client :active-turn-id nil)
    (agent-shell-codex-app-server--clear-tool-state client)
    (agent-shell-codex-app-server--reject-pending-requests
     client
     "Codex app-server shut down")
    (when-let* ((process (map-elt client :process)))
      (when (process-live-p process)
        (delete-process process))
      (map-put! client :process nil))))

(defun agent-shell-codex-app-server--acp-dispatch (original-fn custom-fn args)
  "Call CUSTOM-FN with ARGS when client uses the app-server backend.
Otherwise call ORIGINAL-FN with ARGS."
  (if (agent-shell-codex-app-server-client-p (plist-get args :client))
      (apply custom-fn args)
    (apply original-fn args)))

(defun agent-shell-codex-app-server--around-acp-send-request (original-fn &rest args)
  "Route `acp-send-request' ARGS for app-server clients.

Call ORIGINAL-FN when the client uses another backend."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-send-request args))

(defun agent-shell-codex-app-server--around-acp-send-notification (original-fn &rest args)
  "Route `acp-send-notification' ARGS for app-server clients.

Call ORIGINAL-FN when the client uses another backend."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-send-notification args))

(defun agent-shell-codex-app-server--around-acp-send-response (original-fn &rest args)
  "Route `acp-send-response' ARGS for app-server clients.

Call ORIGINAL-FN when the client uses another backend."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-send-response args))

(defun agent-shell-codex-app-server--around-acp-subscribe-to-errors (original-fn &rest args)
  "Route `acp-subscribe-to-errors' ARGS for app-server clients.

Call ORIGINAL-FN when the client uses another backend."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-subscribe-to-errors args))

(defun agent-shell-codex-app-server--around-acp-subscribe-to-notifications (original-fn &rest args)
  "Route `acp-subscribe-to-notifications' for app-server clients.

Pass ARGS through ORIGINAL-FN when the client is not app-server-backed."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-subscribe-to-notifications args))

(defun agent-shell-codex-app-server--around-acp-subscribe-to-requests (original-fn &rest args)
  "Route `acp-subscribe-to-requests' for app-server clients.

Pass ARGS through ORIGINAL-FN when the client is not app-server-backed."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-subscribe-to-requests args))

(defun agent-shell-codex-app-server--around-acp-shutdown (original-fn &rest args)
  "Route `acp-shutdown' ARGS for app-server clients.

Call ORIGINAL-FN when the client uses another backend."
  (agent-shell-codex-app-server--acp-dispatch
   original-fn #'agent-shell-codex-app-server-shutdown args))

(dolist (entry '((acp-send-request . agent-shell-codex-app-server--around-acp-send-request)
                 (acp-send-notification . agent-shell-codex-app-server--around-acp-send-notification)
                 (acp-send-response . agent-shell-codex-app-server--around-acp-send-response)
                 (acp-subscribe-to-errors . agent-shell-codex-app-server--around-acp-subscribe-to-errors)
                 (acp-subscribe-to-notifications . agent-shell-codex-app-server--around-acp-subscribe-to-notifications)
                 (acp-subscribe-to-requests . agent-shell-codex-app-server--around-acp-subscribe-to-requests)
                 (acp-shutdown . agent-shell-codex-app-server--around-acp-shutdown)))
  (unless (advice-member-p (cdr entry) (car entry))
    (advice-add (car entry) :around (cdr entry))))

(provide 'agent-shell-codex-app-server)

;;; agent-shell-codex-app-server.el ends here
