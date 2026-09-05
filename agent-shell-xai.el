;;; agent-shell-xai.el --- xAI Grok Build agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eddie Jesinsky

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
;; This file includes xAI Grok Build-specific configurations.
;;
;; Grok Build speaks ACP over stdio via `grok agent stdio'.  Auth is the
;; local CLI login flow (~/.grok/auth.json); no separate API-key env is
;; required for the default setup.
;;
;; This module maps Grok-specific `_meta', session modes, reasoning
;; effort, usage, and reverse-requests (`x.ai/ask_user_question',
;; `x.ai/exit_plan_mode', `x.ai/mcp/elicit') onto agent-shell's existing
;; ACP UI.
;;

;;; Code:

(defconst agent-shell-xai-icon-name
  "xai.png"
  "Icon name for xAI / Grok Build (from lobe-icons).")

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'shell-maker)
(require 'acp)
(require 'url-parse)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--build-content-blocks "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function browse-url "browse-url")
(defvar agent-shell--state)
(defvar agent-shell-text-file-capabilities)

(defcustom agent-shell-xai-acp-command
  '("grok" "agent" "stdio")
  "Command and parameters for the Grok Build ACP client.

The first element is the command name, and the rest are command parameters.

Examples:

  (\"grok\" \"agent\" \"stdio\")
  (\"grok\" \"agent\" \"-m\" \"grok-build\" \"stdio\")
  (\"grok\" \"agent\" \"--always-approve\" \"stdio\")"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-xai-environment
  nil
  "Environment variables for the Grok Build ACP client.

This should be a list of environment variables to be used when
starting the Grok Build client process.

Example usage to set custom environment variables:

  (setq agent-shell-xai-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-xai-default-model-id
  nil
  "Default Grok Build model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell.

Can be set to either a string or a function that returns a string."
  :type '(choice (const nil) string function)
  :group 'agent-shell)

(defcustom agent-shell-xai-default-session-mode-id
  nil
  "Default Grok Build session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell.

Grok session modes are `default', `plan', and `ask'."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-xai-yolo-mode
  nil
  "When non-nil, start Grok sessions in always-approve mode.

Sent as `yoloMode' on `session/new' `_meta'.  This is the per-session
equivalent of `grok agent --always-approve stdio'."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-xai-auto-mode
  nil
  "When non-nil, start Grok sessions in auto permission mode.

Sent as `autoMode' on `session/new' `_meta'.  Ignored when
`agent-shell-xai-yolo-mode' is non-nil."
  :type 'boolean
  :group 'agent-shell)

(defconst agent-shell-xai--client-identifier
  "agent-shell"
  "Client identifier reported to Grok on `initialize' `_meta'.")

(defconst agent-shell-xai--omitted 'agent-shell-xai--omitted
  "Sentinel for an omitted optional Grok MCP form field.")

(defun agent-shell-xai--session-meta ()
  "Return Grok `session/new' `_meta' from current settings.

For example:

  (let ((agent-shell-xai-yolo-mode t)
        (agent-shell-xai-default-model-id \"grok-build\"))
    (agent-shell-xai--session-meta))
  => ((clientFsRead . :false) (clientFsWrite . :false)
      (yoloMode . t) (modelId . \"grok-build\"))"
  (let ((file-capability (if agent-shell-text-file-capabilities t :false)))
    (append
     `((clientFsRead . ,file-capability)
       (clientFsWrite . ,file-capability))
     (when agent-shell-xai-yolo-mode
       '((yoloMode . t)))
     (when (and agent-shell-xai-auto-mode (not agent-shell-xai-yolo-mode))
       '((autoMode . t)))
     (when-let* ((model-id (if (functionp agent-shell-xai-default-model-id)
                               (funcall agent-shell-xai-default-model-id)
                             agent-shell-xai-default-model-id)))
       `((modelId . ,model-id))))))

(defun agent-shell-xai-make-grok-config ()
  "Create a Grok Build agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'grok-build
   :mode-line-name "Grok"
   :buffer-name "Grok"
   :shell-prompt "Grok> "
   :shell-prompt-regexp "Grok> "
   :icon-name agent-shell-xai-icon-name
   :welcome-function #'agent-shell-xai--welcome-message
   ;; Keep the explicit cached-token handshake for compatibility with Grok
   ;; releases that reject session/new before authenticate.  Current Grok
   ;; falls through server-side when another unpinned auth method is active.
   :needs-authentication t
   :authenticate-request-maker (lambda ()
                                 (acp-make-authenticate-request
                                  :method-id "cached_token"))
   :client-maker (lambda (buffer)
                   (agent-shell-xai-make-client :buffer buffer))
   :default-model-id (lambda () (if (functionp agent-shell-xai-default-model-id)
                                    (funcall agent-shell-xai-default-model-id)
                                  agent-shell-xai-default-model-id))
   :default-session-mode-id (lambda () agent-shell-xai-default-session-mode-id)
   :session-meta (agent-shell-xai--session-meta)
   :notification-adapter #'agent-shell-xai--notification-adapter
   :outgoing-request-decorator #'agent-shell-xai--outgoing-request-decorator
   :session-response-adapter #'agent-shell-xai--adapt-session-response
   :request-handler #'agent-shell-xai--on-request
   :prompt-response-adapter #'agent-shell-xai--prompt-response-adapter
   :busy-prompt-handler #'agent-shell-xai--handle-busy-prompt
   :install-instructions
   "Install the Grok Build CLI so `grok' is on PATH (typically ~/.grok/bin).
Authenticate once via the CLI login flow (stores ~/.grok/auth.json).
See https://docs.x.ai/docs/overview and https://zed.dev/acp/agent/grok-build.
ACP over stdio: grok agent stdio"))

(defun agent-shell-xai--active-prompt-p (state)
  "Return non-nil when STATE has a Grok `session/prompt' in flight."
  (seq-find (lambda (request)
              (equal (map-elt request :method) "session/prompt"))
            (map-elt state :active-requests)))

(defun agent-shell-xai--interjection-queued-p (result)
  "Return non-nil when Grok accepted an interjection RESULT.

`x.ai/interject' is fire-and-forget from the model's perspective: Grok
adds the text to its pending interjection buffer and applies it at its next
safe point.  The RPC response is still the only acknowledgement that the
server accepted that buffer entry."
  (equal (map-nested-elt result '(result status)) "queued"))

(defun agent-shell-xai--handle-busy-prompt (event)
  "Send busy-prompt EVENT to Grok as a mid-turn interjection."
  (when-let* ((state (map-elt event :state))
              ((agent-shell-xai--active-prompt-p state))
              (client (map-elt state :client))
              (session-id (map-nested-elt state '(:session :id)))
              (prompt (map-elt event :prompt))
              ((stringp prompt))
              (fallback (map-elt event :fallback)))
    (condition-case nil
        (let* ((content-blocks (agent-shell--build-content-blocks prompt))
               (image-content-p
                (seq-find (lambda (block)
                            (equal (map-elt block 'type) "image"))
                          content-blocks)))
          (acp-send-request
           :client client
           :request `((:method . "_x.ai/interject")
                      (:params . ((sessionId . ,session-id)
                                  (text . ,(substring-no-properties prompt))
                                  (interjectionId . ,(make-temp-name "agent-shell-"))
                                  ,@(when image-content-p
                                      `((content . ,(vconcat content-blocks)))))))
           :buffer (map-elt state :buffer)
           :on-success (lambda (result)
                         (unless (agent-shell-xai--interjection-queued-p result)
                           (funcall fallback)))
           :on-failure (lambda (&rest _)
                         (funcall fallback)))
          t)
      (error nil))))

(defun agent-shell-xai-start-grok ()
  "Start an interactive Grok Build agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-xai-make-grok-config)
                     :new-shell t))

(cl-defun agent-shell-xai-make-client (&key buffer)
  "Create a Grok Build ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-xai-acp-command)
                                :command-params (cdr agent-shell-xai-acp-command)
                                :environment-variables agent-shell-xai-environment
                                :context-buffer buffer))

(cl-defun agent-shell-xai--notification-adapter (&key acp-notification)
  "Return ACP-NOTIFICATION unless Grok marks its user echo as hidden.

Grok emits synthetic `user_message_chunk' updates for background and
automatic turns.  Their `hideFromScrollback' metadata instructs clients
not to render the synthetic prompt, while later agent output remains
visible."
  (unless (and (equal (map-elt acp-notification 'method) "session/update")
               (equal (map-nested-elt acp-notification
                                      '(params update sessionUpdate))
                      "user_message_chunk")
               (eq (map-nested-elt acp-notification
                                   '(params update _meta hideFromScrollback))
                   t))
    acp-notification))

(defun agent-shell-xai--outgoing-request-decorator (request)
  "Decorate outgoing Grok REQUEST alists.

Adds `clientIdentifier' to `initialize' `_meta' and sets Grok file
capabilities from `agent-shell-text-file-capabilities'."
  (pcase (map-elt request :method)
    ("initialize"
     (agent-shell-xai--decorate-initialize request))
    (_ request)))

(defun agent-shell-xai--decorate-initialize (request)
  "Return REQUEST with Grok initialize `_meta' and file capabilities applied."
  (let* ((params (copy-alist (map-elt request :params)))
         (capabilities (copy-alist (map-elt params 'clientCapabilities)))
         (fs (copy-alist (map-elt capabilities 'fs)))
         (flag (if agent-shell-text-file-capabilities t :false)))
    (setf (alist-get '_meta params)
          (append `((clientIdentifier . ,agent-shell-xai--client-identifier))
                  (map-elt params '_meta)))
    (setf (alist-get 'readTextFile fs) flag)
    (setf (alist-get 'writeTextFile fs) flag)
    (setf (alist-get 'fs capabilities) fs)
    (setf (alist-get 'clientCapabilities params) capabilities)
    `((:method . ,(map-elt request :method))
      (:params . ,params))))

(defun agent-shell-xai--session-modes ()
  "Return Grok ACP session modes.

  (agent-shell-xai--session-modes)
  => ((id . \"default\") (name . \"Default\") ...) ..."
  '(((id . "default")
     (name . "Default")
     (description . "Normal agent mode"))
    ((id . "plan")
     (name . "Plan")
     (description . "Explore and write a plan before editing"))
    ((id . "ask")
     (name . "Ask")
     (description . "Ask before acting"))))

(cl-defun agent-shell-xai--adapt-session-response (&key acp-response)
  "Adapt Grok ACP-RESPONSE by seeding omitted session modes.

Native `configOptions', including reasoning effort, pass through unchanged."
  (let ((adapted (copy-alist acp-response)))
    (unless (map-nested-elt adapted '(modes availableModes))
      (setf (alist-get 'modes adapted)
            `((currentModeId . ,(or (map-nested-elt adapted '(modes currentModeId))
                                    "default"))
              (availableModes . ,(agent-shell-xai--session-modes)))))
    adapted))

(cl-defun agent-shell-xai--prompt-response-adapter (&key acp-response)
  "Return ACP usage mapped from Grok PromptResponse ACP-RESPONSE `_meta'."
  (agent-shell-xai--usage-from-prompt-response acp-response))

(defun agent-shell-xai--usage-from-prompt-response (acp-response)
  "Return an ACP `usage' alist from Grok ACP-RESPONSE `_meta', or nil.

Prefers the whole-prompt `_meta.usage' aggregate and falls back to the
legacy last-model-call fields.  Maps `reasoningTokens' onto
`thoughtTokens' and `cacheCreationTokens' onto `cachedWriteTokens'."
  (when-let* ((meta (map-elt acp-response '_meta))
              (usage (or (map-elt meta 'usage) meta))
              ((or (map-contains-key usage 'totalTokens)
                   (map-contains-key usage 'inputTokens))))
    (append
     `((totalTokens . ,(or (map-elt usage 'totalTokens) 0)))
     (when (map-contains-key usage 'inputTokens)
       `((inputTokens . ,(map-elt usage 'inputTokens))))
     (when (map-contains-key usage 'outputTokens)
       `((outputTokens . ,(map-elt usage 'outputTokens))))
     (when-let* ((thought (or (map-elt usage 'thoughtTokens)
                              (map-elt usage 'reasoningTokens))))
       `((thoughtTokens . ,thought)))
     (when (map-contains-key usage 'cachedReadTokens)
       `((cachedReadTokens . ,(map-elt usage 'cachedReadTokens))))
     (when (map-contains-key usage 'cacheCreationTokens)
       `((cachedWriteTokens . ,(map-elt usage 'cacheCreationTokens)))))))

(defun agent-shell-xai--ext-method (acp-request)
  "Return the Grok ext-method name for ACP-REQUEST.

Strips a leading underscore used by some ACP gateways.

  (agent-shell-xai--ext-method \\='((method . \"_x.ai/ask_user_question\")))
  => \"x.ai/ask_user_question\""
  (when-let* ((method (map-elt acp-request 'method)))
    (if (string-prefix-p "_" method)
        (substring method 1)
      method)))

(defun agent-shell-xai--request-params (acp-request)
  "Return the inner params alist from a Grok ext-method ACP-REQUEST.

Gateway-wrapped requests nest the payload at `params.params'."
  (let ((params (map-elt acp-request 'params)))
    (if (and (map-elt params 'method)
             (map-elt params 'params))
        (map-elt params 'params)
      params)))

(defun agent-shell-xai--json-object (pairs)
  "Return a JSON object hash table built from PAIRS.

PAIRS is an alist keyed by strings.  `json-serialize' only accepts
symbols as alist keys, so a JSON object keyed by arbitrary text has to be
a hash table.  Grok keys answer maps by question text, which is
agent-authored prose.

  (gethash \"Which database?\"
           (agent-shell-xai--json-object \\='((\"Which database?\" . [\"Redis\"]))))
  => [\"Redis\"]"
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair pairs)
      (puthash (car pair) (cdr pair) table))
    table))

(cl-defun agent-shell-xai--ask-user-question-result (&key outcome answers annotations)
  "Return a Grok `x.ai/ask_user_question' result for OUTCOME.

OUTCOME is `accepted', `chat-about-this', `skip-interview', or `cancelled'.
ANSWERS is an alist of question text to selected label lists.  ANNOTATIONS
maps question text to optional `preview' and `notes' alists.

  (json-serialize
   (agent-shell-xai--ask-user-question-result
    :outcome \\='accepted :answers \\='((\"Which database?\" . (\"Redis\")))))
  => \"{\\\"outcome\\\":\\\"accepted\\\",\\\"answers\\\":{\\\"Which database?\\\":[\\\"Redis\\\"]}}\""
  (pcase outcome
    ('accepted
     (append
      `((outcome . "accepted")
        (answers . ,(agent-shell-xai--json-object
                     (mapcar (lambda (entry)
                               (cons (car entry) (vconcat (cdr entry))))
                             answers))))
      (when annotations
        `((annotations . ,(agent-shell-xai--json-object annotations))))))
    ('chat-about-this
     `((outcome . "chat_about_this")
       (partial_answers . ,(agent-shell-xai--json-object nil))))
    ('skip-interview
     `((outcome . "skip_interview")
       (partial_answers . ,(agent-shell-xai--json-object nil))))
    ('cancelled
     '((outcome . "cancelled")))))

(cl-defun agent-shell-xai--exit-plan-mode-result (&key outcome feedback)
  "Return a Grok `x.ai/exit_plan_mode' result.

OUTCOME is \"approved\", \"cancelled\", or \"abandoned\".
FEEDBACK is optional text for a cancelled outcome."
  (append
   `((outcome . ,outcome))
   (when (and feedback (not (string-empty-p feedback)))
     `((feedback . ,feedback)))))

(cl-defun agent-shell-xai--mcp-elicitation-result (&key outcome content)
  "Return a Grok `x.ai/mcp/elicit' result for OUTCOME and CONTENT.

For example:

  (agent-shell-xai--mcp-elicitation-result
   :outcome \"accept\" :content \='((email . \"user@example.com\")))
  => ((outcome . \"accept\") (content (email . \"user@example.com\")))"
  (append `((outcome . ,outcome))
          (when content
            `((content . ,content)))))

(defun agent-shell-xai--mcp-form-choice-pairs (schema)
  "Return display/value pairs for a Grok MCP form SCHEMA."
  (if-let* ((options (or (map-elt schema 'oneOf)
                         (map-elt schema 'anyOf))))
      (mapcar (lambda (option)
                (cons (format "%s" (or (map-elt option 'title)
                                        (map-elt option 'const)))
                      (map-elt option 'const)))
              options)
    (when-let* ((values (map-elt schema 'enum)))
      (let ((names (map-elt schema 'enumNames)))
        (seq-map-indexed
         (lambda (value index)
           (cons (format "%s" (or (and names
                                        (< index (length names))
                                        (seq-elt names index))
                                   value))
                 value))
         values)))))

(defun agent-shell-xai--mcp-form-prompt (params name schema required)
  "Return a minibuffer prompt for Grok MCP PARAMS field NAME using SCHEMA.

REQUIRED is non-nil when the field cannot be omitted."
  (format "[%s] %s%s%s: "
          (or (map-elt params 'serverName) "MCP")
          (or (map-elt schema 'title) name)
          (if required "" " (optional)")
          (if-let* ((description (map-elt schema 'description))
                    ((not (string-empty-p description))))
              (format " - %s" description)
            "")))

(defun agent-shell-xai--mcp-number-valid-p (value schema)
  "Return non-nil when numeric VALUE satisfies Grok MCP form SCHEMA."
  (and (or (not (map-contains-key schema 'minimum))
           (>= value (map-elt schema 'minimum)))
       (or (not (map-contains-key schema 'maximum))
           (<= value (map-elt schema 'maximum)))))

(defun agent-shell-xai--read-mcp-number (prompt schema required)
  "Read a Grok MCP numeric value using PROMPT, SCHEMA, and REQUIRED."
  (let ((regexp (if (equal (map-elt schema 'type) "integer")
                    "\\`[+-]?[0-9]+\\'"
                  "\\`[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\'"))
        value
        done)
    (while (not done)
      (let ((answer (read-string
                     prompt nil nil
                     (when (map-contains-key schema 'default)
                       (format "%s" (map-elt schema 'default))))))
        (cond
         ((and (not required) (string-empty-p answer))
          (setq value agent-shell-xai--omitted
                done t))
         ((string-match-p regexp answer)
          (let ((number (string-to-number answer)))
            (if (agent-shell-xai--mcp-number-valid-p number schema)
                (setq value number
                      done t)
              (message "Value is outside the allowed range"))))
         (t
          (message "Enter a valid %s" (map-elt schema 'type))))))
    value))

(defun agent-shell-xai--read-mcp-string (prompt schema required)
  "Read a Grok MCP string value using PROMPT, SCHEMA, and REQUIRED."
  (let (value done)
    (while (not done)
      (let ((answer (read-string prompt nil nil (map-elt schema 'default))))
        (cond
         ((and (not required) (string-empty-p answer))
          (setq value agent-shell-xai--omitted
                done t))
         ((and required (string-empty-p answer))
          (message "A value is required"))
         ((and (or (not (map-contains-key schema 'minLength))
                   (>= (length answer) (map-elt schema 'minLength)))
               (or (not (map-contains-key schema 'maxLength))
                   (<= (length answer) (map-elt schema 'maxLength))))
          (setq value answer
                done t))
         (t
          (message "Text does not satisfy the requested length")))))
    value))

(defun agent-shell-xai--read-mcp-choice (prompt schema required)
  "Read one Grok MCP choice using PROMPT, SCHEMA, and REQUIRED."
  (let* ((choices (agent-shell-xai--mcp-form-choice-pairs schema))
         (labels (mapcar #'car choices))
         (answer (completing-read prompt
                                  (if required labels (cons "" labels))
                                  nil t)))
    (if (string-empty-p answer)
        agent-shell-xai--omitted
      (map-elt choices answer))))

(defun agent-shell-xai--read-mcp-array (prompt schema required)
  "Read a Grok MCP array using PROMPT, SCHEMA, and REQUIRED."
  (let* ((choices (agent-shell-xai--mcp-form-choice-pairs
                   (map-elt schema 'items)))
         (labels (mapcar #'car choices))
         (minimum (or (map-elt schema 'minItems) 0))
         (maximum (map-elt schema 'maxItems))
         value
         done)
    (unless choices
      (error "MCP form array has no supported values"))
    (while (not done)
      (let* ((answers (completing-read-multiple prompt labels nil t))
             (values (mapcar (lambda (answer)
                               (map-elt choices answer))
                             answers)))
        (cond
         ((and (not required) (null values))
          (setq value agent-shell-xai--omitted
                done t))
         ((< (length values) minimum)
          (message "Select at least %s value(s)" minimum))
         ((and maximum (> (length values) maximum))
          (message "Select at most %s value(s)" maximum))
         (t
          (setq value (vconcat values)
                done t)))))
    value))

(defun agent-shell-xai--read-mcp-form-value (params name schema required)
  "Read Grok MCP field NAME from SCHEMA and PARAMS, honoring REQUIRED."
  (let ((prompt (agent-shell-xai--mcp-form-prompt
                 params name schema required)))
    (cond
     ((member (map-elt schema 'type) '("integer" "number"))
      (agent-shell-xai--read-mcp-number prompt schema required))
     ((equal (map-elt schema 'type) "boolean")
      (pcase (completing-read prompt
                              (if required '("Yes" "No") '("" "Yes" "No"))
                              nil t)
        ("Yes" t)
        ("No" :false)
        (_ agent-shell-xai--omitted)))
     ((equal (map-elt schema 'type) "array")
      (agent-shell-xai--read-mcp-array prompt schema required))
     ((agent-shell-xai--mcp-form-choice-pairs schema)
      (agent-shell-xai--read-mcp-choice prompt schema required))
     ((equal (map-elt schema 'type) "string")
      (agent-shell-xai--read-mcp-string prompt schema required))
     (required
      (error "Unsupported MCP form field type: %s" (map-elt schema 'type)))
     (t agent-shell-xai--omitted))))

(defun agent-shell-xai--read-mcp-form (params)
  "Read and return structured Grok MCP form content from PARAMS."
  (let ((required-fields (map-nested-elt params '(requestedSchema required)))
        content)
    (dolist (property (map-nested-elt params '(requestedSchema properties)))
      (let* ((name (format "%s" (car property)))
             (value (agent-shell-xai--read-mcp-form-value
                     params name (cdr property)
                     (seq-contains-p required-fields name))))
        (unless (eq value agent-shell-xai--omitted)
          (push (cons name value) content))))
    (agent-shell-xai--json-object (nreverse content))))

(defun agent-shell-xai--mcp-elicitation-consented-p (params action)
  "Return non-nil when the user permits MCP PARAMS to perform ACTION."
  (yes-or-no-p
   (format "[%s] %s %s? "
           (or (map-elt params 'serverName) "MCP")
           (or (map-elt params 'message) "Input requested.")
           action)))

(defun agent-shell-xai--safe-mcp-url-p (url)
  "Return non-nil when URL is an HTTP(S) URL without embedded credentials."
  (when (stringp url)
    (let ((parsed (url-generic-parse-url url)))
      (and (member (url-type parsed) '("http" "https"))
           (url-host parsed)
           (not (url-user parsed))))))

(cl-defun agent-shell-xai--read-mcp-elicitation (&key params)
  "Read and return a Grok MCP elicitation response for PARAMS."
  (pcase (map-elt params 'mode)
    ("form"
     (if (agent-shell-xai--mcp-elicitation-consented-p params "Continue")
         (agent-shell-xai--mcp-elicitation-result
          :outcome "accept"
          :content (agent-shell-xai--read-mcp-form params))
       (agent-shell-xai--mcp-elicitation-result :outcome "decline")))
    ("url"
     (let ((url (map-elt params 'url)))
       (if (and (agent-shell-xai--safe-mcp-url-p url)
                (agent-shell-xai--mcp-elicitation-consented-p
                 params (format "Open %s" url)))
           (progn
             (browse-url url)
             (agent-shell-xai--mcp-elicitation-result :outcome "accept"))
         (agent-shell-xai--mcp-elicitation-result :outcome "decline"))))
    (_
     (agent-shell-xai--mcp-elicitation-result :outcome "decline"))))

(cl-defun agent-shell-xai--on-request (&key state acp-request)
  "Handle Grok reverse-requests on STATE for ACP-REQUEST.

Returns non-nil when the request was handled.

Grok blocks its tool call on the reply, so a handled request always
answers exactly once.  Reading the answer can fail in two ways that must
not escape: `quit' (the user pressed \\[keyboard-quit] at a prompt) is the
protocol's own cancel outcome, and any other error becomes a JSON-RPC
error.  Letting either propagate would leave Grok waiting forever."
  (when-let* ((handler (pcase (agent-shell-xai--ext-method acp-request)
                         ("x.ai/ask_user_question"
                          #'agent-shell-xai--handle-ask-user-question)
                         ("x.ai/exit_plan_mode"
                          #'agent-shell-xai--handle-exit-plan-mode)
                         ("x.ai/mcp/elicit"
                          #'agent-shell-xai--handle-mcp-elicitation))))
    (condition-case error
        (agent-shell-xai--respond
         :state state
         :acp-request acp-request
         :result (funcall handler :state state :acp-request acp-request))
      (error
       (agent-shell-xai--respond-error
        :state state
        :acp-request acp-request
        :message (error-message-string error))))
    t))

(cl-defun agent-shell-xai--handle-ask-user-question (&key acp-request &allow-other-keys)
  "Return an ask_user_question result for ACP-REQUEST."
  (condition-case nil
      (agent-shell-xai--read-ask-user-question
       :params (agent-shell-xai--request-params acp-request))
    (quit
     (agent-shell-xai--ask-user-question-result :outcome 'cancelled))))

(cl-defun agent-shell-xai--handle-exit-plan-mode (&key state acp-request)
  "Return an exit_plan_mode result for ACP-REQUEST, rendering plan in STATE."
  (condition-case nil
      (agent-shell-xai--read-exit-plan-mode
       :state state
       :params (agent-shell-xai--request-params acp-request))
    (quit
     (agent-shell-xai--exit-plan-mode-result :outcome "abandoned"))))

(cl-defun agent-shell-xai--handle-mcp-elicitation (&key acp-request &allow-other-keys)
  "Return an MCP elicitation result for ACP-REQUEST."
  (condition-case error
      (agent-shell-xai--read-mcp-elicitation
       :params (agent-shell-xai--request-params acp-request))
    (quit
     (agent-shell-xai--mcp-elicitation-result :outcome "cancel"))
    (error
     (message "Grok MCP elicitation declined: %s" (error-message-string error))
     (agent-shell-xai--mcp-elicitation-result :outcome "decline"))))

(cl-defun agent-shell-xai--respond (&key state acp-request result)
  "Send RESULT for ACP-REQUEST using STATE's ACP client."
  (acp-send-response
   :client (map-elt state :client)
   :response `((:request-id . ,(map-elt acp-request 'id))
               (:result . ,result))))

(cl-defun agent-shell-xai--respond-error (&key state acp-request message)
  "Send a JSON-RPC error MESSAGE for ACP-REQUEST using STATE's ACP client."
  (acp-send-response
   :client (map-elt state :client)
   :response `((:request-id . ,(map-elt acp-request 'id))
               (:error . ,(acp-make-error :code -32603
                                          :message message)))))

(cl-defun agent-shell-xai--read-ask-user-question (&key params)
  "Read answers for a Grok ask_user_question PARAMS payload.

Uses the minibuffer.  Plan mode also offers chat-about-this and
skip-interview actions."
  (let ((questions (append (map-elt params 'questions) nil))
        (plan-action (and (equal (map-elt params 'mode) "plan")
                          (completing-read "Grok question: "
                                           '("Answer" "Chat about this"
                                             "Skip interview" "Cancel")
                                           nil t nil nil "Answer"))))
    (cond
     ((equal plan-action "Chat about this")
      (agent-shell-xai--ask-user-question-result :outcome 'chat-about-this))
     ((equal plan-action "Skip interview")
      (agent-shell-xai--ask-user-question-result :outcome 'skip-interview))
     ((equal plan-action "Cancel")
      (agent-shell-xai--ask-user-question-result :outcome 'cancelled))
     (t
      (condition-case nil
          (let ((responses (mapcar #'agent-shell-xai--read-question questions)))
            (agent-shell-xai--ask-user-question-result
             :outcome 'accepted
             :answers (mapcar (lambda (response)
                                (cons (map-elt response :text)
                                      (map-elt response :labels)))
                              responses)
             :annotations (seq-keep
                           (lambda (response)
                             (when-let* ((annotation (map-elt response :annotation)))
                               (cons (map-elt response :text) annotation)))
                           responses)))
        (quit
         (agent-shell-xai--ask-user-question-result :outcome 'cancelled)))))))

(defun agent-shell-xai--read-question (question)
  "Read one Grok QUESTION and return its text, labels, and annotation.

For example, selecting a previewed option returns:

  ((:text . \"Which database?\")
   (:labels . (\"Redis\"))
   (:annotation . ((preview . \"Fast cache\"))))"
  (let* ((text (map-elt question 'question))
         (options (append (map-elt question 'options) nil))
         (labels (seq-uniq
                  (append (mapcar (lambda (option)
                                    (map-elt option 'label))
                                  options)
                          '("Other"))
                  #'equal))
         (multi-select (or (map-elt question 'multiSelect)
                           (map-elt question 'multi_select)))
         (selected (if multi-select
                       (completing-read-multiple (format "%s: " text)
                                                 labels nil t)
                     (list (completing-read (format "%s: " text)
                                            labels
                                            nil t nil nil (car labels)))))
         (other (member "Other" selected))
         (selected-option (and (not multi-select)
                               (seq-find (lambda (option)
                                           (equal (map-elt option 'label)
                                                  (car selected)))
                                         options)))
         (preview (map-elt selected-option 'preview))
         (notes (when other
                  (agent-shell-xai--read-other-answer text))))
    `((:text . ,text)
      (:labels . ,selected)
      ,@(when (or preview notes)
          `((:annotation . (,@(when preview
                                `((preview . ,preview)))
                              ,@(when notes
                                  `((notes . ,notes))))))))))

(defun agent-shell-xai--read-other-answer (question)
  "Read a non-empty free-form answer for QUESTION."
  (let ((answer ""))
    (while (string-empty-p answer)
      (setq answer (string-trim
                    (read-string (format "%s (Other): " question)))))
    answer))

(cl-defun agent-shell-xai--read-exit-plan-mode (&key state params)
  "Read a Grok exit_plan_mode decision for PARAMS, showing plan in STATE."
  (when-let* ((plan (map-elt params 'planContent))
              ((not (string-empty-p plan)))
              (buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (agent-shell--update-fragment
       :state (or (and (boundp 'agent-shell--state) agent-shell--state)
                  state)
       :block-id (format "grok-plan-%s" (or (map-elt params 'toolCallId) "plan"))
       :label-left (propertize "Proposed plan" 'font-lock-face 'agent-shell-section-heading)
       :body plan
       :expanded t
       :above-last-prompt t)))
  (pcase (completing-read "Grok plan: "
                          '("Approve" "Request changes" "Abandon")
                          nil t nil nil "Approve")
    ("Approve"
     (agent-shell-xai--exit-plan-mode-result :outcome "approved"))
    ("Request changes"
     (agent-shell-xai--exit-plan-mode-result
      :outcome "cancelled"
      :feedback (read-string "Plan feedback: ")))
    (_
     (agent-shell-xai--exit-plan-mode-result :outcome "abandoned"))))

(defun agent-shell-xai--welcome-message (config)
  "Return Grok Build welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-xai--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-xai--ascii-art ()
  "Grok Build ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
 ██████╗ ██████╗   ██████╗  ██╗  ██╗
██╔════╝ ██╔══██╗ ██╔═══██╗ ██║ ██╔╝
██║  ███╗██████╔╝ ██║   ██║ █████╔╝
██║   ██║██╔══██╗ ██║   ██║ ██╔═██╗
╚██████╔╝██║  ██║ ╚██████╔╝ ██║  ██╗
 ╚═════╝ ╚═╝  ╚═╝  ╚═════╝  ╚═╝  ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#a78bfa" :inherit fixed-pitch)
                                       '(:foreground "#6d28d9" :inherit fixed-pitch)))))

(provide 'agent-shell-xai)

;;; agent-shell-xai.el ends here
