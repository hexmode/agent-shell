;;; agent-shell.el --- An agent shell powered by ACP -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell
;; Version: 0.10.2
;; Package-Requires: ((shell-maker "0.82.2")(acp "0.6.1"))

(defconst agent-shell--version "0.10.2")

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
;; agent-shell is driven by ACP (Agent Client Protocol) as per spec
;; https://agentclientprotocol.com
;;
;; Note: This package is in the very early stage and is likely
;; incomplete or may have some rough edges.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'acp)
(eval-when-compile
  (require 'cl-lib))
(require 'json)
(require 'map)
(unless (require 'markdown-overlays nil 'noerror)
  (error "Please update 'shell-maker' to v0.82.2 or newer"))
(require 'shell-maker)
(require 'sui)
(require 'svg nil :noerror)
(require 'quick-diff)
(require 'agent-shell-anthropic)
(require 'agent-shell-google)
(require 'agent-shell-goose)
(require 'agent-shell-openai)
(require 'agent-shell-qwen)

(defcustom agent-shell-permission-icon "⚠"
  "Icon displayed when shell commands require permission to execute.

You may use \"􀇾\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-icon "💡"
  "Icon displayed during the AI's thought process.

You may use \"􁷘\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-show-config-icons t
  "Whether to show icons in agent config selection."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-path-resolver-function nil
  "Function for resolving remote paths on the local file-system, and vice versa.

Expects a function that takes the path as its single argument, and
returns the resolved path.  Set to nil to disable mapping."
  :type 'function
  :group 'agent-shell)

(defcustom agent-shell-text-file-capabilities t
  "Whether agents are initialized with read/write text file capabilities.

See `acp-make-initialize-request' for details."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-display-action
  '(display-buffer-same-window)
  "Display action for agent shell buffers.
See `display-buffer' for the format of display actions."
  :type '(cons (repeat function) alist)
  :group 'agent-shell)

(defcustom agent-shell-file-completion-enabled nil
  "Non-nil automatically enables file completion when starting shells."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-embed-file-size-limit 102400
  "Maximum file size in bytes for embedding with ContentBlock::Resource.
Files larger than this will use ContentBlock::ResourceLink instead.
Default is 100KB (102400 bytes)."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-header-style (if (display-graphic-p) 'graphical 'text)
  "Style for agent shell buffer headers.

Can be one of:

 \='graphical: Display header with icon and styled text.
 \='text: Display simple text-only header.
 nil: Display no header."
  :type '(choice (const :tag "Graphical" graphical)
                 (const :tag "Text only" text)
                 (const :tag "No header" nil))
  :group 'agent-shell)

(defcustom agent-shell-show-welcome-message t
  "Non-nil to show welcome message."
  :type 'boolean
  :group 'agent-shell)

(cl-defun agent-shell-make-agent-config (&key mode-line-name welcome-function
                                              buffer-name shell-prompt shell-prompt-regexp
                                              client-maker
                                              needs-authentication
                                              authenticate-request-maker
                                              icon-name
                                              install-instructions)
  "Create an agent configuration alist.

Keyword arguments:
- MODE-LINE-NAME: Name to display in the mode line
- WELCOME-FUNCTION: Function to call for welcome message
- BUFFER-NAME: Name of the agent buffer
- SHELL-PROMPT: The shell prompt string
- SHELL-PROMPT-REGEXP: Regexp to match the shell prompt
- CLIENT-MAKER: Function to create the client
- NEEDS-AUTHENTICATION: Non-nil authentication is required
- AUTHENTICATE-REQUEST-MAKER: Function to create authentication requests
- ICON-NAME: Name of the icon to use
- INSTALL-INSTRUCTIONS: Instructions to show when executable is not found

Returns an alist with all specified values."
  `((:mode-line-name . ,mode-line-name)
    (:welcome-function . ,welcome-function)
    (:buffer-name . ,buffer-name)
    (:shell-prompt . ,shell-prompt)
    (:shell-prompt-regexp . ,shell-prompt-regexp)
    (:client-maker . ,client-maker)
    (:needs-authentication . ,needs-authentication)
    (:authenticate-request-maker . ,authenticate-request-maker)
    (:icon-name . ,icon-name)
    (:install-instructions . ,install-instructions)))

(defun agent-shell--make-default-agent-configs ()
  "Create a list of default agent configs.

This function aggregates agents from OpenAI, Anthropic, Google, and Goose."
  (list (agent-shell-anthropic-make-claude-code-config)
        (agent-shell-google-make-gemini-config)
        (agent-shell-goose-make-agent-config)
        (agent-shell-openai-make-codex-config)
        (agent-shell-qwen-make-agent-config)))

(defcustom agent-shell-agent-configs
  (agent-shell--make-default-agent-configs)
  "The list of known agent configurations.

See `agent-shell-*-make-*-config' for details."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'agent-shell)

(cl-defun agent-shell--make-state (&key buffer client-maker needs-authentication authenticate-request-maker)
  "Construct shell agent state with BUFFER.

Shell state is provider-dependent and needs CLIENT-MAKER, NEEDS-AUTHENTICATION
and AUTHENTICATE-REQUEST-MAKER."
  (list (cons :buffer buffer)
        (cons :client nil)
        (cons :client-maker client-maker)
        (cons :initialized nil)
        (cons :needs-authentication needs-authentication)
        (cons :authenticate-request-maker authenticate-request-maker)
        (cons :authenticated nil)
        (cons :session (list (cons :id nil)
                             (cons :mode-id nil)
                             (cons :modes nil)))
        (cons :last-entry-type nil)
        (cons :chunked-group-count 0)
        (cons :request-count 0)
        (cons :tool-calls nil)
        (cons :available-commands nil)
        (cons :agent-supports-embedded-context nil)))

(defvar-local agent-shell--state
    (agent-shell--make-state))

(defvar agent-shell--shell-maker-config nil)

;;;###autoload
(defun agent-shell (&optional new-shell)
  "Start or reuse an existing agent shell.
With prefix argument NEW-SHELL, force start a new shell."
  (interactive "P")
  (if (and (not new-shell)
           (seq-first (agent-shell-buffers)))
      (agent-shell--display-buffer (seq-first (agent-shell-buffers)))
    (agent-shell-start :config (or (agent-shell-select-config
                                    :prompt "Start new agent: ")
                                   (error "No agent config found")))))

;;;###autoload
(defun agent-shell-toggle ()
  "Toggle agent shell display."
  (interactive)
  (let ((shell-buffer (seq-first (agent-shell-project-buffers))))
    (unless shell-buffer
      (user-error "No agent shell buffers available for current project"))
    (if-let ((window (get-buffer-window shell-buffer)))
        (if (> (count-windows) 1)
            (delete-window window)
          (switch-to-prev-buffer))
      (agent-shell--display-buffer shell-buffer))))

(cl-defun agent-shell-start (&key config)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format."
  (agent-shell--start :config config
                      :no-focus nil
                      :new-session t))

(cl-defun agent-shell--config-icon (&key config)
  "Create icon string for CONFIG if available and icons are enabled.
Returns an empty string if no icon should be displayed."
  (if-let* ((graphics-capable (display-graphic-p))
            (icon-name (map-elt config :icon-name))
            (icon-filename (agent-shell--fetch-agent-icon icon-name)))
      (with-temp-buffer
        (insert-image (create-image icon-filename nil nil
                                    :ascent 'center
                                    :height (frame-char-height)))
        (buffer-string))
    ""))

(cl-defun agent-shell-select-config (&key prompt)
  "Display PROMPT to select an agent config from `agent-shell-agent-configs'."
  (let* ((configs agent-shell-agent-configs)
         (choices (mapcar (lambda (config)
                            (let ((display-name (or (map-elt config :mode-line-name)
                                                    (map-elt config :buffer-name)
                                                    "Unknown Agent"))
                                  (icon (when agent-shell-show-config-icons
                                          (agent-shell--config-icon :config config))))
                              (cons (format "%s%s%s" icon (when icon " ") display-name)
                                    config)))
                          configs))
         (selected-name (completing-read (or prompt "Select agent: ") choices nil t)))
    (map-elt choices selected-name)))

(defun agent-shell-buffers ()
  "Return all shell buffers."
  (seq-map #'buffer-name
           (seq-filter (lambda (buffer)
                         (with-current-buffer buffer
                           (derived-mode-p 'agent-shell-mode)))
                       (buffer-list))))

(defun agent-shell-version ()
  "Show `agent-shell' mode version."
  (interactive)
  (message "agent-shell v%s" agent-shell--version))

(defun agent-shell-interrupt ()
  "Interrupt in-progress request and reject all pending permissions."
  (interactive)
  (unless (eq major-mode 'agent-shell-mode)
    (error "Not in a shell"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (error "No active session"))
  (when (y-or-n-p "Abort?")
    ;; First cancel all pending permission requests
    (map-do
     (lambda (tool-call-id tool-call-data)
       (if (map-elt tool-call-data :permission-request-id)
           (agent-shell--send-permission-response
            :client (map-elt (agent-shell--state) :client)
            :request-id (map-elt tool-call-data :permission-request-id)
            :cancelled t
            :state (agent-shell--state)
            :tool-call-id tool-call-id)
         (message "Couldn't cancel pending tool call. Please file a bug.")))
     (map-elt (agent-shell--state) :tool-calls))
    ;; Then send the cancel notification
    (acp-send-notification
     :client (map-elt (agent-shell--state) :client)
     :notification (acp-make-session-cancel-notification
                    :session-id (map-nested-elt (agent-shell--state) '(:session :id))
                    :reason "User cancelled"))))

(cl-defun agent-shell--make-shell-maker-config (&key prompt prompt-regexp)
  "Create `shell-maker' configuration with PROMPT and PROMPT-REGEXP."
  (make-shell-maker-config
   :name "agent"
   :prompt prompt
   :prompt-regexp prompt-regexp
   :execute-command
   (lambda (command shell)
     (agent-shell--handle
      :command command
      :shell shell))))

(defvar-keymap agent-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `agent-shell-mode'."
  "TAB" #'agent-shell-next-item
  "<tab>" #'agent-shell-next-item
  "<backtab>" #'agent-shell-previous-item
  "S-TAB" #'agent-shell-previous-item
  "C-c C-c" #'agent-shell-interrupt)

(shell-maker-define-major-mode (agent-shell--make-shell-maker-config) agent-shell-mode-map)

(cl-defun agent-shell--handle (&key command shell)
  "Handle SHELL COMMAND (and lazy initialize the ACP stack).

SHELL is from `shell-maker'.

Flow:

  Before a shell COMMAND can be sent as a prompt to the agent, a
  handful of ACP initialization steps must take place (some asynchronously).
  Once all initialization steps are cleared, only then the COMMAND
  can be sent to the agent as a prompt (thus recursive nature of this function).

  -> Initialize ACP client
      |-> Subscribe to ACP events
           |-> Initiate handshake (ie.  initialize RPC)
                |-> Authenticate (optional)
                     |-> Start prompt session
                          |-> Send COMMAND/prompt (finally!)"
  (with-current-buffer (map-elt shell :buffer)
    (unless (eq major-mode 'agent-shell-mode)
      (error "Not in a shell"))
    (map-put! (agent-shell--state) :request-count
              ;; TODO: Make public in shell-maker.
              (shell-maker--current-request-id))
    (cond ((not (map-elt (agent-shell--state) :client))
           (when (agent-shell--initialize-client :shell shell)
             (agent-shell--handle :command command :shell shell)))
          ((or (not (map-nested-elt (agent-shell--state) '(:client :request-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :notification-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :error-handlers))))
           (when (agent-shell--initialize-subscriptions :shell shell)
             (agent-shell--handle :command command :shell shell)))
          ((not (map-elt (agent-shell--state) :initialized))
           (agent-shell--initiate-handshake
            :shell shell
            :on-initiated (lambda ()
                            (map-put! (agent-shell--state) :initialized t)
                            (agent-shell--handle :command command :shell shell))))
          ((and (map-elt (agent-shell--state) :needs-authentication)
                (not (map-elt (agent-shell--state) :authenticated)))
           (agent-shell--authenticate
            :shell shell
            :on-authenticated (lambda ()
                                (map-put! (agent-shell--state) :authenticated t)
                                (agent-shell--handle :command command :shell shell))))
          ((not (map-nested-elt (agent-shell--state) '(:session :id)))
           (agent-shell--initiate-session
            :shell shell
            :on-session-init (lambda ()
                               (agent-shell--handle :command command :shell shell))))
          (t
           (agent-shell--send-command :prompt command :shell shell)))))

(cl-defun agent-shell--on-error (&key state error)
  "Handle ERROR with SHELL an STATE."
  (let-alist error
    (agent-shell--update-dialog-block
     :state state
     :block-id "Error"
     :body (or .message "Some error ¯\\_ (ツ)_/¯")
     :create-new t
     :navigation 'never)))

(cl-defun agent-shell--on-notification (&key state notification)
  "Handle incoming notification using SHELL, STATE, and NOTIFICATION."
  (let-alist notification
    (cond ((equal .method "session/update")
           (let ((update (map-elt (map-elt notification 'params) 'update)))
             (cond
              ((equal (map-elt update 'sessionUpdate) "tool_call")
               (agent-shell--save-tool-call
                state
                (map-elt update 'toolCallId)
                (append (list (cons :title (map-elt update 'title))
                              (cons :status (map-elt update 'status))
                              (cons :kind (map-elt update 'kind))
                              (cons :command (map-nested-elt update '(rawInput command)))
                              (cons :description (map-nested-elt update '(rawInput description)))
                              (cons :content (map-elt update 'content)))
                        (when-let ((diff (agent-shell--make-diff-info (map-elt update 'content))))
                          (list (cons :diff diff)))))
               (let ((tool-call-labels (agent-shell-make-tool-call-label
                                        state (map-elt update 'toolCallId))))
                 (agent-shell--update-dialog-block
                  :state state
                  :block-id (map-elt update 'toolCallId)
                  :label-left (map-elt tool-call-labels :status)
                  :label-right (map-elt tool-call-labels :title)))
               (map-put! state :last-entry-type "tool_call"))
              ((equal (map-elt update 'sessionUpdate) "agent_thought_chunk")
               (let-alist update
                 ;; (message "agent_thought_chunk: last-type=%s, will-append=%s"
                 ;;          (map-elt state :last-entry-type)
                 ;;          (equal (map-elt state :last-entry-type) "agent_thought_chunk"))
                 (unless (equal (map-elt state :last-entry-type)
                                "agent_thought_chunk")
                   (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count))))
                 (agent-shell--update-dialog-block
                  :state state
                  :block-id (format "%s-agent_thought_chunk"
                                    (map-elt state :chunked-group-count))
                  :label-left  (concat
                                agent-shell-thought-process-icon
                                " "
                                (propertize "Thought process" 'font-lock-face font-lock-doc-markup-face))
                  :body .content.text
                  :append (equal (map-elt state :last-entry-type)
                                 "agent_thought_chunk")))
               (map-put! state :last-entry-type "agent_thought_chunk"))
              ((equal (map-elt update 'sessionUpdate) "agent_message_chunk")
               (unless (equal (map-elt state :last-entry-type) "agent_message_chunk")
                 (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count))))
               (let-alist update
                 (agent-shell--update-dialog-block
                  :state state
                  :block-id (format "%s-agent_message_chunk"
                                    (map-elt state :chunked-group-count))
                  :body .content.text
                  :create-new (not (equal (map-elt state :last-entry-type)
                                          "agent_message_chunk"))
                  :append t
                  :navigation 'never))
               (map-put! state :last-entry-type "agent_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "plan")
               (let-alist update
                 (agent-shell--update-dialog-block
                  :state state
                  :block-id "plan"
                  :label-left (propertize "Plan" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-plan .entries)
                  :expanded t))
               (map-put! state :last-entry-type "plan"))
              ((equal (map-elt update 'sessionUpdate) "tool_call_update")
               (let-alist update
                 ;; Update stored tool call data with new status and content
                 (agent-shell--save-tool-call
                  state
                  .toolCallId
                  (append (list (cons :status (map-elt update 'status))
                                (cons :content (map-elt update 'content)))
                          (when-let ((diff (agent-shell--make-diff-info (map-elt update 'content))))
                            (list (cons :diff diff)))))
                 (let ((output (concat
                                "\n\n"
                                ;; TODO: Consider if there are other
                                ;; types of content to display.
                                (mapconcat (lambda (item)
                                             (let-alist item
                                               .content.text))
                                           .content
                                           "\n\n")
                                "\n\n")))
                   ;; Hide permission after sending response.
                   ;; Status and permission are no longer pending. User
                   ;; likely selected one of: accepted/rejected/always.
                   ;; Remove stale permission dialog.
                   (when (and (map-elt update 'status)
                              (not (equal (map-elt update 'status) "pending")))
                     ;; block-id must be the same as the one used as
                     ;; agent-shell--update-dialog-block param by "session/request_permission".
                     (agent-shell--delete-dialog-block :state state :block-id (format "permission-%s" .toolCallId)))
                   (let ((tool-call-labels (agent-shell-make-tool-call-label
                                            state .toolCallId)))
                     (agent-shell--update-dialog-block
                      :state state
                      :block-id .toolCallId
                      :label-left (map-elt tool-call-labels :status)
                      :label-right (map-elt tool-call-labels :title)
                      :body (string-trim output)))))
               (map-put! state :last-entry-type "tool_call_update"))
              ((equal (map-elt update 'sessionUpdate) "available_commands_update")
               (let-alist update
                 (map-put! state :available-commands (map-elt update 'availableCommands))
                 (agent-shell--update-dialog-block
                  :state state
                  :block-id "available_commands_update"
                  :label-left (propertize "Available commands" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-available-commands (map-elt update 'availableCommands))))
               (map-put! state :last-entry-type "available_commands_update"))
              ((equal (map-elt update 'sessionUpdate) "current_mode_update")
               (let ((updated-session (map-elt state :session))
                     (new-mode-id (map-elt update 'currentModeId)))
                 (map-put! updated-session :mode-id new-mode-id)
                 (map-put! state :session updated-session)
                 (message "Session mode: %s"
                          (agent-shell--resolve-session-mode-name
                           new-mode-id
                           (map-nested-elt (agent-shell--state)
                                           '(:session :modes))))
                 ;; Note: No need to set :last-entry-type as no text was inserted.
                 (force-mode-line-update)))
              (t
               (agent-shell--update-dialog-block
                :state state
                :block-id "Session Update - fallback"
                :body (format "%s" notification)
                :create-new t
                :navigation 'never)
               (map-put! state :last-entry-type nil)))))
          (t
           (agent-shell--update-dialog-block
            :state state
            :block-id "Notification - fallback"
            :body (format "Unhandled (please file an issue): %s" notification)
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--on-request (&key state request)
  "Handle incoming request using SHELL, STATE, and REQUEST."
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (agent-shell--save-tool-call
            state .params.toolCall.toolCallId
            (append (list (cons :title .params.toolCall.title)
                          (cons :status .params.toolCall.status)
                          (cons :kind .params.toolCall.kind)
                          (cons :permission-request-id .id))
                    (when-let ((diff (agent-shell--make-diff-info .params.toolCall.content)))
                      (list (cons :diff diff)))))
           (agent-shell--update-dialog-block
            :state state
            ;; block-id must be the same as the one used
            ;; in agent-shell--delete-dialog-block param.
            :block-id (format "permission-%s" .params.toolCall.toolCallId)
            :body (with-current-buffer (map-elt state :buffer)
                    (agent-shell--make-tool-call-permission-text
                     :request request
                     :client (map-elt state :client)
                     :state state))
            :expanded t
            :navigation 'never)
           (agent-shell-jump-to-latest-permission-button-row)
           (map-put! state :last-entry-type "session/request_permission"))
          ((equal .method "fs/read_text_file")
           (agent-shell--on-fs-read-text-file-request
            :state state
            :request request))
          ((equal .method "fs/write_text_file")
           (agent-shell--on-fs-write-text-file-request
            :state state
            :request request))
          (t
           (agent-shell--update-dialog-block
            :state state
            :block-id "Unhandled Incoming Request"
            :body (format "⚠ Unhandled incoming request: \"%s\"" .method)
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--extract-buffer-text (&key buffer line limit)
  "Extract text from BUFFER starting from LINE with optional LIMIT.
LINE defaults to 1, LIMIT defaults to nil (read to end)."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (and line (> line 1))
        ;; Seems odd to use forward-line but
        ;; that's what `goto-line' recommends.
        (forward-line (1- line)))
      (let ((start (point)))
        (if limit
            ;; Seems odd to use forward-line but
            ;; that's what `goto-line' recommends.
            (forward-line limit)
          (goto-char (point-max)))
        (buffer-substring-no-properties start (point))))))

(cl-defun agent-shell--on-fs-read-text-file-request (&key state request)
  "Handle fs/read_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (line (or .params.line 1))
               (limit .params.limit)
               (existing-buffer (find-buffer-visiting path))
               (content (if existing-buffer
                            ;; Read from open buffer (includes unsaved changes)
                            (agent-shell--extract-buffer-text :buffer existing-buffer :line line :limit limit)
                          ;; No open buffer, read from file
                          (with-temp-buffer
                            (insert-file-contents path)
                            (agent-shell--extract-buffer-text :buffer (current-buffer) :line line :limit limit)))))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-read-text-file-response
                      :request-id .id
                      :content content)))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(cl-defun agent-shell--on-fs-write-text-file-request (&key state request)
  "Handle fs/write_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (content .params.content)
               (dir (file-name-directory path))
               (buffer (find-buffer-visiting path)))
          (when (and dir (not (file-exists-p dir)))
            (make-directory dir t))
          (if buffer
              ;; Buffer is open, write and save.
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert content)
                  (basic-save-buffer)))
            ;; No open buffer, write to file directly.
            (with-temp-file path
              (insert content)))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-write-text-file-response
                      :request-id .id)))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(defun agent-shell--resolve-path (path)
  "Resolve PATH using `agent-shell-path-resolver-function'."
  (funcall (or agent-shell-path-resolver-function #'identity) path))

(defun agent-shell--get-devcontainer-workspace-path (cwd)
  "Return devcontainer workspaceFolder for CWD; signal error if none found.

See https://containers.dev for more information on devcontainers."
  (let ((devcontainer-config-file-name (expand-file-name ".devcontainer/devcontainer.json" cwd)))
    (condition-case _err
        (or
         (map-elt (json-read-file devcontainer-config-file-name) 'workspaceFolder)
         (error "No workspace folder defined in %s" devcontainer-config-file-name))
      (file-missing (error "Not found: %s" devcontainer-config-file-name))
      (permission-denied (error "Not readable: %s" devcontainer-config-file-name))
      (json-string-format (error "No valid JSON: %s" devcontainer-config-file-name)))))

(defun agent-shell--resolve-devcontainer-path (path)
  "Resolve PATH from a devcontainer in the local filesystem, and vice versa.

For example:

- /workspace/README.md => /home/xenodium/projects/kitchen-sink/README.md
- /home/xenodium/projects/kitchen-sink/README.md => /workspace/README.md"
  (let* ((cwd (agent-shell-cwd))
         (devcontainer-path (agent-shell--get-devcontainer-workspace-path cwd)))
    (if (string-prefix-p cwd path)
        (string-replace cwd devcontainer-path path)
      (if agent-shell-text-file-capabilities
          (if-let* ((is-dev-container (string-prefix-p devcontainer-path path))
                    (local-path (expand-file-name (string-replace devcontainer-path cwd path))))
              (or
               (and (file-in-directory-p local-path cwd) local-path)
               (error "Resolves to path outside of working directory: %s" path))
            (error "Unexpected path outside of workspace folder: %s" path))
        (error "Refuse to resolve to local filesystem with text file capabilities disabled: %s" path)))))

(defun agent-shell--stop-reason-description (stop-reason)
  "Return a human-readable text description for STOP-REASON.

https://agentclientprotocol.com/protocol/schema#param-stop-reason"
  (pcase stop-reason
    ("end_turn" "Finished")
    ("max_tokens" "Max token limit reached")
    ("max_turn_requests" "Exceeded request limit")
    ("refusal" "Refused")
    ("cancelled" "Cancelled")
    (_ (format "Stop for unknown reason: %s" stop-reason))))

(defun agent-shell--format-available-commands (commands)
  "Format COMMANDS for shell rendering."
  (let ((max-name-length (seq-reduce (lambda (acc cmd)
                                       (max acc (length (map-elt cmd 'name))))
                                     commands
                                     0)))
    (mapconcat
     (lambda (cmd)
       (let ((name (map-elt cmd 'name))
             (desc (map-elt cmd 'description)))
         (concat
          ;; For commands to be executable, they start with /
          (propertize (format (format "/%%-%ds" max-name-length) name)
                      'font-lock-face 'font-lock-function-name-face)
          "  "
          (propertize desc 'font-lock-face 'font-lock-comment-face))))
     commands
     "\n")))

(defun agent-shell--make-diff-info (acp-content)
  "Make diff information from ACP's tool_call_update's ACP-CONTENT.

CONTENT is of the the type ToolCallContent as per ACP spec:

https://agentclientprotocol.com/protocol/schema#toolcallcontent

Returns in the form:

 `((:old . old-text)
   (:new . new-text)
   (:file . file-path))."
  (when-let* ((diff-item (cond
                          ;; Single diff object
                          ((and acp-content (equal (map-elt acp-content 'type) "diff"))
                           acp-content)
                          ;; TODO: Is this needed?
                          ;; Isn't acp-content always an alist?
                          ;; Vector/array acp-content - find diff item
                          ((vectorp acp-content)
                           (seq-find (lambda (item)
                                       (equal (map-elt item 'type) "diff"))
                                     acp-content))
                          ;; TODO: Is this needed?
                          ;; Isn't acp-content always an alist?
                          ;; List acp-content - find diff item
                          ((listp acp-content)
                           (seq-find (lambda (item)
                                       (equal (map-elt item 'type) "diff"))
                                     acp-content))))
              (old-text (map-elt diff-item 'oldText))
              (new-text (map-elt diff-item 'newText))
              (file-path (map-elt diff-item 'path)))
    (append (list (cons :old old-text)
                  (cons :new new-text))
            (when file-path
              (list (cons :file file-path))))))

(cl-defun agent-shell--make-error-handler (&key state shell)
  "Create ACP error handler with SHELL STATE."
  (lambda (error raw-message)
    (let-alist error
      (with-current-buffer (map-elt state :buffer)
        (agent-shell--update-dialog-block
         :state (agent-shell--state)
         :block-id (format "failed-%s-id:%s-code:%s"
                           (map-elt state :request-count)
                           (or .id "?")
                           (or .code "?"))
         :body (agent-shell--make-error-dialog-text
                :code .code
                :message .message
                :raw-message raw-message)
         :create-new t)))
    ;; TODO: Mark buffer command with shell failure.
    (with-current-buffer (map-elt state :buffer)
      (funcall (map-elt shell :finish-output) t))))

(defun agent-shell--save-tool-call (state tool-call-id tool-call)
  "Store TOOL-CALL with TOOL-CALL-ID in STATE's :tool-calls alist."
  (let* ((tool-calls (map-elt state :tool-calls))
         (old-tool-call (map-elt tool-calls tool-call-id))
         (updated-tools (copy-alist tool-calls))
         (tool-call-overrides (seq-filter (lambda (pair)
                                            (cdr pair))
                                          tool-call)))
    (setf (map-elt updated-tools tool-call-id)
          (if old-tool-call
              (map-merge 'alist old-tool-call tool-call-overrides)
            tool-call-overrides))
    (map-put! state :tool-calls updated-tools)))

(cl-defun agent-shell--make-error-dialog-text (&key code message raw-message)
  "Create formatted error dialog text with CODE, MESSAGE, and RAW-MESSAGE."
  (format "╭─

  %s Error (%s) %s

  %s

  %s

╰─"
          (propertize "⚠" 'font-lock-face 'error)
          (or code "?")
          (propertize "⚠" 'font-lock-face 'error)
          (or message "¯\\_ (ツ)_/¯")
          (agent-shell--make-button
           :text "Details" :help "Details" :kind 'error
           :action (lambda ()
                     (interactive)
                     (agent-shell--view-as-error
                      (with-temp-buffer
                        (let ((print-circle t))
                          (pp raw-message (current-buffer))
                          (buffer-string))))))))

(defun agent-shell--view-as-error (text)
  "Display TEXT in a `read-only' error buffer."
  (let ((buf (get-buffer-create "*acp error*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (read-only-mode 1))
    (display-buffer buf)))

(defun agent-shell--clean-up ()
  "Clean up resources.

For example, shut down ACP client."
  (unless (eq major-mode 'agent-shell-mode)
    (error "Not in a shell"))
  (when (map-elt (agent-shell--state) :client)
    (acp-shutdown :client (map-elt (agent-shell--state) :client))))

(defun agent-shell--status-label (status)
  "Convert STATUS codes to user-visible labels."
  (let* ((config (pcase status
                   ("pending" '("pending" font-lock-comment-face))
                   ("in_progress" '("in progress" warning))
                   ("completed" '("completed" success))
                   ("failed" '("failed" error))
                   (_ '("unknown" warning))))
         (label (car config))
         (face (cadr config))
         (color (face-foreground face nil t)))
    (agent-shell--add-text-properties
     (propertize (format " %s " label) 'font-lock-face 'default)
     'font-lock-face
     `(:foreground ,color :box (:color ,color)))))

(defun agent-shell--shorten-paths (text)
  "Shorten file paths in TEXT relative to project root.

\"/path/to/project/file.txt\" -> \"file.txt\""
  (when text
    (let ((cwd (string-remove-suffix "/" (agent-shell-cwd))))
      (replace-regexp-in-string (concat (regexp-quote cwd) "/")
                                ""
                                (or text "")))))

(defun agent-shell-make-tool-call-label (state tool-call-id)
  "Create tool call label from STATE using TOOL-CALL-ID.

Returns propertized labels in :status and :title propertized."
  (when-let ((tool-call (map-nested-elt state `(:tool-calls ,tool-call-id))))
    `((:status . ,(let ((status (when (map-elt tool-call :status)
                                  (agent-shell--status-label (map-elt tool-call :status))))
                        (kind (when (map-elt tool-call :kind)
                                (agent-shell--add-text-properties
                                 (propertize (format " %s " (map-elt tool-call :kind))
                                             'font-lock-face 'default)
                                 'font-lock-face
                                 `(:box t)))))
                    (concat
                     (when status
                       status)
                     (when (and status kind)
                       " ")
                     (when kind
                       kind))))
      (:title . ,(let* ((title (when (map-elt tool-call :title)
                                 (agent-shell--shorten-paths (map-elt tool-call :title))))
                        (description (when (map-elt tool-call :description)
                                       (agent-shell--shorten-paths (map-elt tool-call :description)))))
                   (cond ((and title description
                               (not (equal (string-remove-prefix "`" (string-remove-suffix "`" (string-trim title)))
                                           (string-remove-prefix "`" (string-remove-suffix "`" (string-trim description))))))
                          (concat
                           (propertize title 'font-lock-face 'font-lock-doc-markup-face)
                           " "
                           (propertize description 'font-lock-face 'font-lock-doc-face)))
                         (title
                          (propertize title 'font-lock-face 'font-lock-doc-markup-face))
                         (description
                          (propertize description 'font-lock-face 'font-lock-doc-markup-face))))))))

(defun agent-shell--format-plan (entries)
  "Format plan ENTRIES for shell rendering."
  (let* ((max-label-width
          (apply #'max (cons 0 (mapcar (lambda (entry)
                                         (length (agent-shell--status-label
                                                  (map-elt entry 'status))))
                                       entries)))))
    (mapconcat
     (lambda (entry)
       (let-alist entry
         (let* ((status-label (agent-shell--status-label .status))
                (label-length (length status-label))
                (padding (make-string
                          (max 1 (- max-label-width label-length))
                          ?\s)))
           (concat
            status-label
            padding
            .content))))
     entries
     "\n")))

(cl-defun agent-shell--make-button (&key text help kind action keymap)
  "Make button with TEXT, HELP text, KIND, KEYMAP, and ACTION."
  (let ((button (propertize
                 (format " %s " text)
                 'font-lock-face '(:box t)
                 'help-echo help
                 'pointer 'hand
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1] action)
                           (define-key map (kbd "RET") action)
                           (define-key map [remap self-insert-command] 'ignore)
                           (when keymap
                             (set-keymap-parent map keymap))
                           map)
                 'button kind)))
    button))

(defun agent-shell--add-text-properties (string &rest properties)
  "Add text PROPERTIES to entire STRING and return the propertized string.
PROPERTIES should be a plist of property-value pairs."
  (let ((str (copy-sequence string))
        (len (length string)))
    (while properties
      (let ((prop (car properties))
            (value (cadr properties)))
        (if (memq prop '(face font-lock-face))
            ;; Merge face properties
            (let ((existing (get-text-property 0 prop str)))
              (put-text-property 0 len prop
                                 (if existing
                                     (list value existing)
                                   value)
                                 str))
          ;; Regular property replacement
          (put-text-property 0 len prop value str))
        (setq properties (cddr properties))))
    str))

(cl-defun agent-shell--apply (&key function alist)
  "Apply keyword ALIST to FUNCTION.

ALIST should be a list of keyword-value pairs like (:foo 1 :bar 2).
FUNCTION should be a function accepting keyword arguments (&key ...)."
  (unless function
    (error "Missing required argument: :function"))
  (unless alist
    (error "Missing required argument: :alist"))
  (apply function
         (mapcan (lambda (pair)
                   (list (car pair) (cdr pair)))
                 alist)))

;;;###autoload
(cl-defun agent-shell--start (&key config no-focus new-session)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format.

Set NO-FOCUS to start in background.
Set NEW-SESSION to start a separate new session."
  (unless (version<= "0.83.1" shell-maker-version)
    (error "Please update shell-maker to version 0.83.1 or newer"))
  (unless (version<= "0.6.1" acp-package-version)
    (error "Please update acp.el to version 0.6.1 or newer"))
  (with-temp-buffer ;; client-maker needs a buffer (use a temp one)
    (unless (and (map-elt config :client-maker)
                 (funcall (map-elt config :client-maker) (current-buffer)))
      (error "No way to create a new client"))
    (agent-shell--ensure-executable
     (map-elt (funcall (map-elt config :client-maker) (current-buffer)) :command)
     (map-elt config :install-instructions)))
  (let* ((shell-maker-config (agent-shell--make-shell-maker-config
                              :prompt (map-elt config :shell-prompt)
                              :prompt-regexp (map-elt config :shell-prompt-regexp)))
         (agent-shell--shell-maker-config shell-maker-config)
         (default-directory (agent-shell-cwd))
         (shell-buffer
          (shell-maker-start agent-shell--shell-maker-config
                             no-focus
                             (when agent-shell-show-welcome-message
                               (map-elt config :welcome-function))
                             new-session
                             (concat (map-elt config :buffer-name)
                                     " Agent @ "
                                     (file-name-nondirectory
                                      (string-remove-suffix "/" default-directory)))
                             (map-elt config :mode-line-name))))
    (with-current-buffer shell-buffer
      ;; Initialize buffer-local state
      (setq-local agent-shell--state (agent-shell--make-state
                                      :buffer shell-buffer
                                      :client-maker (map-elt config :client-maker)
                                      :needs-authentication (map-elt config :needs-authentication)
                                      :authenticate-request-maker (map-elt config :authenticate-request-maker)))
      ;; Initialize buffer-local shell-maker-config
      (setq-local agent-shell--shell-maker-config shell-maker-config)
      (setq header-line-format (agent-shell--make-header
                                :icon-name (map-elt config :icon-name)
                                :title (concat (map-elt config :buffer-name) " Agent")
                                :location (string-remove-suffix "/" (abbreviate-file-name default-directory))))
      (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
      (sui-mode +1)
      (when agent-shell-file-completion-enabled
        (agent-shell-completion-mode +1))
      (agent-shell--setup-modeline))
    shell-buffer))

(cl-defun agent-shell--delete-dialog-block (&key state block-id)
  "Delete dialog block with STATE and BLOCK-ID."
  (with-current-buffer (map-elt state :buffer)
    (unless (and (eq major-mode 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (sui-delete-dialog-block :namespace-id (map-elt state :request-count) :block-id block-id)))

(cl-defun agent-shell--update-dialog-block (&key state block-id label-left label-right body append create-new navigation expanded)
  "Update dialog block in the shell buffer.

Creates or updates existing dialog using STATE's request count as namespace.
BLOCK-ID uniquely identifies the block.

Dialog can have LABEL-LEFT, LABEL-RIGHT, and BODY.

Optional flags: APPEND text to existing content, CREATE-NEW block,
NAVIGATION for navigation style, EXPANDED to show block expanded
by default."
  (with-current-buffer (map-elt state :buffer)
    (unless (and (eq major-mode 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (shell-maker-with-auto-scroll-edit
     (when-let* ((range (sui-update-dialog-block
                         (sui-make-dialog-block-model
                          :namespace-id (map-elt state :request-count)
                          :block-id block-id
                          :label-left label-left
                          :label-right label-right
                          :body body)
                         :navigation navigation
                         :append append
                         :create-new create-new
                         :expanded expanded))
                 (padding-start (map-nested-elt range '(:padding :start)))
                 (padding-end (map-nested-elt range '(:padding :end)))
                 (block-start (map-nested-elt range '(:block :start)))
                 (block-end (map-nested-elt range '(:block :end))))
       (save-restriction
         ;; TODO: Move this to shell-maker?
         (let ((inhibit-read-only t))
           ;; comint relies on field property to
           ;; derive `comint-next-prompt'.
           ;; Marking as field to avoid false positives in
           ;; `agent-shell-next-item' and `agent-shell-previous-item'.
           (add-text-properties (or padding-start block-start)
                                (or padding-end block-end) '(field output)))
         ;; Apply markdown overlay to body.
         (when-let ((body-start (map-nested-elt range '(:body :start)))
                    (body-end (map-nested-elt range '(:body :end))))
           (narrow-to-region body-start body-end)
           (markdown-overlays-put)
           (widen))
         ;;
         ;; Note: For now, we're skipping applying markdown overlays
         ;; on left labels as they currently carry propertized text
         ;; for statuses (ie. boxed).
         ;;
         ;; Apply markdown overlay to right label.
         (when-let ((label-right-start (map-nested-elt range '(:label-right :start)))
                    (label-right-end (map-nested-elt range '(:label-right :end))))
           (narrow-to-region label-right-start label-right-end)
           (markdown-overlays-put)
           (widen)))))))

(defun agent-shell-toggle-logging ()
  "Toggle logging."
  (interactive)
  (setq acp-logging-enabled (not acp-logging-enabled))
  (message "Logging: %s" (if acp-logging-enabled "ON" "OFF")))

(defun agent-shell-reset-logs ()
  "Reset all log buffers."
  (interactive)
  (acp-reset-logs :client (map-elt (agent-shell--state) :client))
  (message "Logs reset"))

(defun agent-shell-next-item ()
  "Go to next item.

Could be a prompt or an expandable item."
  (interactive)
  (unless (eq major-mode 'agent-shell-mode)
    (error "Not in a shell"))
  (let* ((prompt-pos (save-mark-and-excursion
                       (when (comint-next-prompt 1)
                         (point))))
         (block-pos (save-mark-and-excursion
                      (sui-forward-block)))
         (button-pos (save-mark-and-excursion
                       (agent-shell-next-permission-button)))
         (next-pos (apply 'min (delq nil (list prompt-pos
                                               block-pos
                                               button-pos)))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos)
      (when (eq next-pos prompt-pos)
        (comint-skip-prompt)))))

(defun agent-shell-previous-item ()
  "Go to previous item.

Could be a prompt or an expandable item."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let* ((current-pos (point))
         (prompt-pos (save-mark-and-excursion
                       (when (comint-next-prompt (- 1))
                         (let ((pos (point)))
                           (when (< pos current-pos)
                             pos)))))
         (block-pos (save-mark-and-excursion
                      (let ((pos (sui-backward-block)))
                        (when (and pos (< pos current-pos))
                          pos))))
         (button-pos (save-mark-and-excursion
                       (let ((pos (agent-shell-previous-permission-button)))
                         (when (and pos (< pos current-pos))
                           pos))))
         (positions (delq nil (list prompt-pos
                                    block-pos
                                    button-pos)))
         (next-pos (when positions
                     (apply 'max positions))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos)
      (when (eq next-pos prompt-pos)
        (comint-skip-prompt)))))

(cl-defun agent-shell-make-environment-variables (&rest vars &key inherit-env load-env &allow-other-keys)
  "Return VARS in the form expected by `process-environment'.

With `:INHERIT-ENV' t, also inherit system environment (as per `setenv')
With `:LOAD-ENV' PATH-OR-PATHS, load .env files from given path(s).

For example:

  (agent-shell-make-environment-variables
    \"PATH\" \"/usr/bin\"
    \"HOME\" \"/home/user\"
    :load-env \"~/.env\")

Returns:

   (\"PATH=/usr/bin\"
    \"HOME=/home/user\")."
  (unless (zerop (mod (length vars) 2))
    (error "`agent-shell-make-environment' must receive complete pairs"))
  (append (mapcan (lambda (pair)
                    (unless (keywordp (car pair))
                      (list (format "%s=%s" (car pair) (cadr pair)))))
                  (seq-partition vars 2))
          (when load-env
            (let ((paths (if (listp load-env) load-env (list load-env))))
              (mapcan (lambda (path)
                        (unless (file-exists-p path)
                          (error "File not found: %s" path))
                        (with-temp-buffer
                          (insert-file-contents path)
                          (let (result)
                            (dolist (line (mapcar #'string-trim (split-string (buffer-string) "\n" t)))
                              (unless (or (string-empty-p line)
                                          (string-prefix-p "#" line))
                                (if (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
                                    (push line result)
                                  (error "Malformed line in %s: %s" path line))))
                            (nreverse result))))
                      paths)))
          (when inherit-env
            process-environment)))

(cl-defun agent-shell--make-header (&key icon-name title location)
  "Return header text.
ICON-NAME is the name of the icon to display (gemini.png).
TITLE is the title text to show.
LOCATION is the location information to include."
  (unless icon-name
    (error ":icon-name is required"))
  (unless title
    (error ":title is required"))
  (unless location
    (error ":location is required"))
  (let ((text-header (format " %s @ %s" title location)))
    (pcase agent-shell-header-style
      ((or 'none (pred null)) "")
      ('text text-header)
      ('graphical
       (if (display-graphic-p)
           (let* ((image-height (* 3 (default-font-height)))
                  (image-width image-height)
                  (text-height 25)
                  (svg (svg-create (frame-pixel-width) (+ image-height 10)))
                  (icon-filename (agent-shell--fetch-agent-icon icon-name))
                  (image-type (let ((ext (file-name-extension icon-name)))
                                (cond
                                 ((member ext '("png" "PNG")) "image/png")
                                 ((member ext '("jpg" "jpeg" "JPG" "JPEG")) "image/jpeg")
                                 ((member ext '("gif" "GIF")) "image/gif")
                                 ((member ext '("webp" "WEBP")) "image/webp")
                                 ((member ext '("svg" "SVG")) "image/svg+xml")
                                 (t "image/png")))))
             (when (and icon-filename image-type)
               (svg-embed svg icon-filename
                          image-type nil
                          :x 0 :y 0 :width image-width :height image-height))
             (svg-text svg title
                       :x (+ image-width 10) :y text-height
                       :fill (face-attribute 'font-lock-variable-name-face :foreground))
             (svg-text svg location
                       :x (+ image-width 10) :y (* 2 text-height)
                       :fill (face-attribute 'font-lock-string-face :foreground))
             (format " %s" (with-temp-buffer
                             (svg-insert-image svg)
                             (buffer-string))))
         text-header))
      (_ text-header))))

(defun agent-shell--fetch-agent-icon (icon-name)
  "Download icon with ICON-NAME from GitHub, only if it exists, and save as binary.

Names can be found at https://github.com/lobehub/lobe-icons/tree/master/packages/static-png

Icon names starting with https:// are downloaded directly from that location."
  (when icon-name
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (url (if (string-prefix-p "https://" (downcase icon-name))
                    icon-name
                  (concat "https://raw.githubusercontent.com/lobehub/lobe-icons/refs/heads/master/packages/static-png/"
                          mode "/" icon-name)))
           (filename (file-name-nondirectory url))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((buffer (url-retrieve-synchronously url t t 5.0)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (if (re-search-forward "^HTTP/1.1 200 OK" nil t)
                  (progn
                    (re-search-forward "\r?\n\r?\n")
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region (point) (point-max) cache-path)))
                (message "Icon fetch failed: %s" url)))
            (kill-buffer buffer))))
      (when (file-exists-p cache-path)
        cache-path))))

(defun agent-shell-view-traffic ()
  "View agent shell traffic buffer."
  (interactive)
  (unless (eq major-mode 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((traffic-buffer (acp-traffic-buffer :client (map-elt (agent-shell--state) :client))))
    (when (with-current-buffer traffic-buffer
            (= (buffer-size) 0))
      (error "No traffic logs available.  Try M-x agent-shell-toggle-logging?"))
    (pop-to-buffer traffic-buffer)))

(defun agent-shell--indent-string (n str)
  "Indent STR lines by N spaces."
  (mapconcat (lambda (line)
               (concat (make-string n ?\s) line))
             (split-string str "\n")
             "\n"))

(defun agent-shell--interpolate-gradient (colors progress)
  "Interpolate between gradient COLORS based on PROGRESS (0.0 to 1.0)."
  (let* ((segments (1- (length colors)))
         (segment-size (/ 1.0 segments))
         (segment (min (floor (/ progress segment-size)) (1- segments)))
         (local-progress (/ (- progress (* segment segment-size)) segment-size))
         (from-color (nth segment colors))
         (to-color (nth (1+ segment) colors)))
    (agent-shell--mix-colors from-color to-color local-progress)))

(defun agent-shell--mix-colors (color1 color2 ratio)
  "Mix two hex colors by RATIO (0.0 = COLOR1, 1.0 = COLOR2)."
  (let* ((r1 (string-to-number (substring color1 1 3) 16))
         (g1 (string-to-number (substring color1 3 5) 16))
         (b1 (string-to-number (substring color1 5 7) 16))
         (r2 (string-to-number (substring color2 1 3) 16))
         (g2 (string-to-number (substring color2 3 5) 16))
         (b2 (string-to-number (substring color2 5 7) 16))
         (r (round (+ (* r1 (- 1 ratio)) (* r2 ratio))))
         (g (round (+ (* g1 (- 1 ratio)) (* g2 ratio))))
         (b (round (+ (* b1 (- 1 ratio)) (* b2 ratio)))))
    (format "#%02x%02x%02x" r g b)))

(defun agent-shell--ensure-executable (executable &optional error-message &rest format-args)
  "Ensure EXECUTABLE exists in PATH or signal error.
ERROR-MESSAGE defaults to \"Executable %s not found\".
FORMAT-ARGS are passed to `format' with ERROR-FORMAT."
  (unless (executable-find executable)
    (apply #'error (concat (format "Executable \"%s\" not found.  Do you need (add-to-list 'exec-path \"another/path/to/consider/\")?" executable)
                           (when error-message
                             "  ")
                           error-message) format-args)))

(defun agent-shell--display-buffer (shell-buffer)
  "Toggle agent SHELL-BUFFER display."
  (interactive)
  (if (get-buffer-window shell-buffer)
      (select-window (get-buffer-window shell-buffer))
    (select-window (display-buffer shell-buffer agent-shell-display-action))))

(defun agent-shell--state ()
  "Get shell state or fail in an incompatible buffer."
  (unless (eq major-mode 'agent-shell-mode)
    (error "Processed outside shell: %s" major-mode))
  (unless agent-shell--state
    (error "No shell state available"))
  agent-shell--state)

;;; Initialization

(cl-defun agent-shell--initialize-client (&key shell)
  "Initialize ACP client with SHELL."
  (agent-shell--update-dialog-block
   :state (agent-shell--state)
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "Creating client..."
   :create-new t)
  (if (map-elt (agent-shell--state) :client-maker)
      (progn
        (map-put! (agent-shell--state)
                  :client (funcall (map-elt agent-shell--state :client-maker)
                                   (map-elt agent-shell--state :buffer)))
        t)
    (funcall (map-elt shell :write-output) "No :client-maker found")
    (funcall (map-elt shell :finish-output) nil)
    nil))

(cl-defun agent-shell--initialize-subscriptions (&key shell)
  "Initialize ACP client subscriptions with SHELL.."
  (agent-shell--update-dialog-block
   :state agent-shell--state
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "\n\nSubscribing..."
   :append t)
  (if (map-elt agent-shell--state :client)
      (progn
        (agent-shell--subscribe-to-client-events :state agent-shell--state)
        t)
    (funcall (map-elt shell :write-output) "No :client found")
    (funcall (map-elt shell :finish-output) nil)
    nil))

(cl-defun agent-shell--initiate-handshake (&key shell on-initiated)
  "Initiate ACP handshake with SHELL.

Must provide ON-INITIATED (lambda ())."
  (unless on-initiated
    (error "Missing required argument: :on-initialized"))
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-dialog-block
     :state agent-shell--state
     :block-id "starting"
     :body "\n\nInitializing..."
     :append t))
  (acp-send-request
   :client (map-elt agent-shell--state :client)
   :request (acp-make-initialize-request
             :protocol-version 1
             :read-text-file-capability agent-shell-text-file-capabilities
             :write-text-file-capability agent-shell-text-file-capabilities)
   :on-success (lambda (response)
                 (with-current-buffer (map-elt shell :buffer)
                   ;; Check if agent supports embeddedContext
                   (let ((embedded-context-supported
                          (map-nested-elt response '(agentCapabilities promptCapabilities embeddedContext))))
                     (map-put! agent-shell--state :agent-supports-embedded-context
                               (eq embedded-context-supported t))))
                 (funcall on-initiated))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell shell)))

(cl-defun agent-shell--authenticate (&key shell on-authenticated)
  "Initiate ACP authentication with SHELL.

Must provide ON-AUTHENTICATED (lambda ())."
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-dialog-block
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nAuthenticating..."
     :append t))
  (if (map-elt (agent-shell--state) :authenticate-request-maker)
      (acp-send-request
       :client (map-elt (agent-shell--state) :client)
       :request (funcall (map-elt agent-shell--state :authenticate-request-maker))
       :on-success (lambda (_response)
                     ;; TODO: More to be handled?
                     (funcall on-authenticated))
       :on-failure (agent-shell--make-error-handler
                    :state (agent-shell--state) :shell shell))
    (funcall (map-elt shell :write-output) "No :authenticate-request-maker")
    (funcall (map-elt shell :finish-output) nil)))

(cl-defun agent-shell--initiate-session (&key shell on-session-init)
  "Initiate ACP session creation with SHELL.

Must provide ON-SESSION-INIT (lambda ())."
  (unless on-session-init
    (error "Missing required argument: :on-session-init"))
  (with-current-buffer (map-elt (agent-shell--state) :buffer)
    (agent-shell--update-dialog-block
     :state (agent-shell--state)
     :block-id "starting"
     :body "\n\nCreating session..."
     :append t))
  (acp-send-request
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-new-request :cwd (agent-shell--resolve-path (agent-shell-cwd)))
   :buffer (current-buffer)
   :on-success (lambda (response)
                 (map-put! agent-shell--state
                           :session (list (cons :id (map-elt response 'sessionId))
                                          (cons :mode-id (map-nested-elt response '(modes currentModeId)))
                                          (cons :modes (map-nested-elt response '(modes availableModes)))))
                 (force-mode-line-update)
                 (agent-shell--update-dialog-block
                  :state agent-shell--state
                  :block-id "starting"
                  :label-left (format "%s %s"
                                      (agent-shell--status-label "completed")
                                      (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
                  :body "\n\nReady"
                  :append t)
                 (when (map-nested-elt response '(modes availableModes))
                   (agent-shell--update-dialog-block
                    :state agent-shell--state
                    :block-id "available_modes"
                    :label-left (propertize "Available modes" 'font-lock-face 'font-lock-doc-markup-face)
                    :body (agent-shell--format-available-modes
                           (map-nested-elt response '(modes availableModes))
                           (map-nested-elt response '(modes currentModeId)))))
                 (funcall on-session-init))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell shell)))

(cl-defun agent-shell--subscribe-to-client-events (&key state)
  "Subscribe SHELL and STATE to ACP events."
  (acp-subscribe-to-errors
   :client (map-elt state :client)
   :on-error (lambda (error)
               (agent-shell--on-error :state state :error error)))
  (acp-subscribe-to-notifications
   :client (map-elt state :client)
   :on-notification (lambda (notification)
                      (agent-shell--on-notification :state state :notification notification)))
  (acp-subscribe-to-requests
   :client (map-elt state :client)
   :on-request (lambda (request)
                 (agent-shell--on-request :state state :request request))))

(defun agent-shell--parse-file-mentions (prompt)
  "Parse @ file mentions from PROMPT string.
Returns list of alists with :start, :end, and :path for each mention."
  (let ((mentions '())
        (pos 0))
    (while (string-match "\\(?:^\\|[[:space:]]\\)@\\(?:\"\\([^\"]+\\)\"\\|\\([^[:space:]]+\\)\\)" prompt pos)
      (push `((:start . ,(match-beginning 0))
              (:end . ,(match-end 0))
              (:path . ,(or (match-string 1 prompt) (match-string 2 prompt))))
            mentions)
      (setq pos (match-end 0)))
    (nreverse mentions)))

(cl-defun agent-shell--build-content-blocks (prompt)
  "Build content blocks from the PROMPT."
  (let* ((supports-embedded-context (map-elt agent-shell--state :agent-supports-embedded-context))
         (mentions (agent-shell--parse-file-mentions prompt))
         (content-blocks '())
         (pos 0))
    (dolist (mention mentions)
      (let* ((start (map-elt mention :start))
             (end (map-elt mention :end))
             (path (map-elt mention :path))
             (resolved-path (expand-file-name path (agent-shell-cwd))))

        ;; Add text before mention
        (when (> start pos)
          (push `((type . "text")
                  (text . ,(substring-no-properties prompt pos start)))
                content-blocks))

        ;; Try to embed or link file
        (condition-case nil
            (if (file-readable-p resolved-path)
              (let ((file-size (file-attribute-size (file-attributes resolved-path))))
                ;; Agent supports embeddedContext and file is small - embed full content
                (if (and supports-embedded-context
                         file-size
                         (< file-size agent-shell-embed-file-size-limit))
                    (let ((content (with-temp-buffer
                                     (insert-file-contents resolved-path)
                                     (buffer-string))))
                      (push `((type . "resource")
                              (resource . ((uri . ,(concat "file://" resolved-path))
                                           (text . ,content)
                                           ;; TODO: Determine mimetype instead of hardcoding
                                           (mimeType . "text/plain"))))
                            content-blocks))
                  ;; File too large or agent doesn't support embeddedContext - use resource link
                  ;; so agent can fetch via fs/read_text_file RPC if needed
                  (push `((type . "resource_link")
                          (uri . ,(concat "file://" resolved-path))
                          (name . ,path)
                          ;; TODO: Determine mimetype instead of hardcoding
                          (mimeType . "text/plain")
                          (size . ,file-size))
                        content-blocks)))
              ;; File's not readable, so also keep it as text
              (push `((type . "text")
                      (text . ,(substring-no-properties prompt start end)))
                    content-blocks))
          (error
           ;; On error, just keep the mention as text
           (push `((type . "text")
                   (text . ,(substring-no-properties prompt start end)))
                 content-blocks)))

        (setq pos end)))

    ;; Add remaining text
    (when (< pos (length prompt))
      (push `((type . "text")
              (text . ,(substring-no-properties prompt pos)))
            content-blocks))

    (nreverse content-blocks)))

(cl-defun agent-shell--collect-attached-files (content-blocks)
  "Collect attached resource uris from CONTENT-BLOCKS."
  (mapcan
   (lambda (content-block)
     (let ((type (map-elt content-block 'type)))
       (cond
        ((equal type "resource") (list (map-nested-elt content-block '(resource uri))))
        ((equal type "resource_link") (list (map-elt content-block 'uri)))
        (t nil))))
   content-blocks))

(cl-defun agent-shell--display-attached-files (uris)
  "Display the attached URIS in the buffer."
  (agent-shell--update-dialog-block
   :state agent-shell--state
   :block-id "attached-files"
   :label-left (format "%d file%s attached"
                       (length uris)
                       (if (= (length uris) 1) "" "s"))
   :body (mapconcat (lambda (f) (format "• %s" f))
                    (nreverse uris)
                    "\n")
   :create-new t))

(cl-defun agent-shell--send-command (&key prompt shell)
  "Send PROMPT to agent using SHELL."
  (let* ((content-blocks (condition-case nil
                             (agent-shell--build-content-blocks prompt)
                           (error `[((type . "text")
                                     (text . ,(substring-no-properties prompt)))])))
         (attached-files (agent-shell--collect-attached-files content-blocks)))

    (when attached-files (agent-shell--display-attached-files attached-files))

    (acp-send-request
     :client (map-elt agent-shell--state :client)
     :request (acp-make-session-prompt-request
               :session-id (map-nested-elt agent-shell--state '(:session :id))
               :prompt content-blocks)
     :buffer (current-buffer)
     :on-success (lambda (response)
                   ;; Tool call details are no longer needed after
                   ;; a session prompt request is finished.
                   ;; Avoid accumulating them unnecessarily.
                   (map-put! (agent-shell--state) :tool-calls nil)
                   (let ((success (equal (map-elt response 'stopReason)
                                         "end_turn")))
                     (unless success
                       (funcall (map-elt shell :write-output)
                                (agent-shell--stop-reason-description
                                 (map-elt response 'stopReason))))
                     (funcall (map-elt shell :finish-output) t)))
     :on-failure (lambda (error raw-message)
                   (funcall (agent-shell--make-error-handler :state agent-shell--state :shell shell)
                            error raw-message)))))

;;; Projects

(defun agent-shell-project-buffers ()
  "Return all shell buffers in the same project as current buffer."
  (let ((project-root (agent-shell-cwd)))
    (seq-filter (lambda (buffer)
                  (with-current-buffer buffer
                    (string-prefix-p project-root
                                     (expand-file-name default-directory))))
                (agent-shell-buffers))))

(defun agent-shell-cwd ()
  "Return the CWD for this shell.

If in a project, use project root."
  (expand-file-name
   (or (when (fboundp 'projectile-project-root)
         (projectile-project-root))
       (when (fboundp 'project-root)
         (when-let ((proj (project-current)))
           (project-root proj)))
       default-directory
       (error "No CWD available"))))

;;; Shell

(defun agent-shell-insert-shell-command-output ()
  "Execute a shell command and insert output as a code block.

The command executes asynchronously.  When finished, the output is
inserted into the shell buffer prompt."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when (shell-maker-busy)
    (user-error "Busy, try later"))
  (let* ((command (read-string "insert command output: "))
         (shell-buffer (current-buffer))
         (output-buffer (with-current-buffer (generate-new-buffer (format "*%s*" command))
                          (insert "$ " command "\n\n")
                          (setq-local buffer-read-only t)
                          (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "q") #'quit-window)
                            (use-local-map map))
                          (current-buffer)))
         (window-config (current-window-configuration))
         (proc (make-process
                :name command
                :buffer output-buffer
                :command (list shell-file-name
                               shell-command-switch
                               ;; Merge stderr into stdout output
                               ;; (all into output buffer)
                               (format "%s 2>&1" command))
                :connection-type 'pipe
                :filter
                (lambda (proc output)
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert output)))))
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (message "Done")
                    (set-window-configuration window-config)
                    (save-excursion
                      (goto-char (point-max))
                      (with-current-buffer shell-buffer
                        (insert "\n\n" (format "```shell
%s
```" (with-current-buffer output-buffer
       (buffer-string))))))
                    (markdown-overlays-put)
                    (when (buffer-live-p output-buffer)
                      (kill-buffer output-buffer)))))))
    (set-process-query-on-exit-flag proc nil)
    (run-at-time "0.2 sec" nil
                 (lambda ()
                   (unless (equal (process-status proc) 'exit)
                     (agent-shell--display-buffer output-buffer))))))

;;; Completion

(defun agent-shell--project-files ()
  "Get project files using projectile or project.el."
  (cond
   ((and (boundp 'projectile-mode)
         projectile-mode
         (projectile-project-p))
    (mapcar (lambda (f)
              (file-relative-name f (projectile-project-root)))
            (projectile-current-project-files)))
   ((fboundp 'project-current)
    (when-let ((proj (project-current)))
      (mapcar (lambda (f) (file-relative-name f (project-root proj)))
              (project-files proj))))
   (t nil)))

(defun agent-shell--file-completion-at-point ()
  "Complete project files after @."
  (when-let* ((triggered (eq (char-before) ?@))
              (start (point))
              (end (save-excursion
                     (skip-chars-forward "[:alnum:]/_.-")
                     (point)))
              (prefix (buffer-substring start end))
              (files (agent-shell--project-files)))
    (list start end
          (seq-filter
           (lambda (f) (string-prefix-p prefix (file-name-nondirectory f)))
           files)
          :exclusive 'no
          :exit-function
          (lambda (_status _string)
            (insert " ")))))

(defun agent-shell--command-completion-at-point ()
  "Complete available commands after /."
  (when (eq (char-before) ?/)
    (let* ((start (point))
           (end (save-excursion
                  (skip-chars-forward "[:alnum:]_-")
                  (point)))
           (prefix (buffer-substring start end))
           (commands (map-elt agent-shell--state :available-commands)))
      (list start end
            (mapcar (lambda (command)
                      (map-elt command 'name))
                    (seq-filter
                     (lambda (command)
                       (string-prefix-p prefix (map-elt command 'name)))
                     commands))
            :exclusive t ; prevent / file completion fallback (already covered by @).
            :exit-function
            (lambda (_status _string)
              (insert " "))))))

(defun agent-shell--trigger-completion-at-point ()
  "Trigger completion when @ or / is typed."
  (when (or (eq (char-before) ?@)
            (eq (char-before) ?/))
    (completion-at-point)))

(define-minor-mode agent-shell-completion-mode
  "Toggle agent shell completion with @ or / prefix."
  :lighter " @/Compl"
  (if agent-shell-completion-mode
      (progn
        (add-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point nil t)
        (add-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point nil t)
        (add-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point nil t))
    (remove-hook 'completion-at-point-functions #'agent-shell--file-completion-at-point t)
    (remove-hook 'completion-at-point-functions #'agent-shell--command-completion-at-point t)
    (remove-hook 'post-self-insert-hook #'agent-shell--trigger-completion-at-point t)))

;;; Permissions

(cl-defun agent-shell--make-tool-call-permission-text (&key request client state)
  "Create text to render permission dialog using REQUEST, CLIENT, and STATE.

For example:

   ╭─

       ⚠ Tool Permission ⚠


       [ View (v) ] [ Allow (y) ] [ Reject (n) ] [ Always Allow (!) ]


   ╰─"
  (let* ((tool-call-id (map-nested-elt request '(params toolCall toolCallId)))
         (diff (map-nested-elt state `(:tool-calls ,tool-call-id :diff)))
         (actions (agent-shell--make-permission-actions (map-nested-elt request '(params options))))
         (keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (when-let ((char (map-elt action :char)))
                       (define-key map (vector char)
                                   (lambda ()
                                     (interactive)
                                     (agent-shell--send-permission-response
                                      :client client
                                      :request-id (map-elt request 'id)
                                      :option-id (map-elt action :option-id)
                                      :state state
                                      :tool-call-id tool-call-id
                                      :message-text (map-elt action :option))))))
                   ;; Add diff keybinding if diff info is available
                   (when diff
                     (define-key map "v" (agent-shell--make-diff-viewing-function
                                          :diff diff
                                          :actions actions
                                          :client client
                                          :request-id (map-elt request 'id)
                                          :state state
                                          :tool-call-id tool-call-id)))
                   map))
         (diff-button (when diff
                        (agent-shell--make-permission-button
                         :text "View (v)"
                         :help "Press v to view diff"
                         :action (agent-shell--make-diff-viewing-function
                                  :diff diff
                                  :actions actions
                                  :client client
                                  :request-id (map-elt request 'id)
                                  :state state
                                  :tool-call-id tool-call-id)
                         :keymap keymap
                         :navigatable t
                         :char ?v
                         :option "view diff"))))
    (format "╭─

    %s %s %s%s


    %s%s


╰─"
            (propertize agent-shell-permission-icon
                        'font-lock-face 'warning)
            (propertize "Tool Permission" 'font-lock-face 'bold)
            (propertize agent-shell-permission-icon
                        'font-lock-face 'warning)
            (if (map-nested-elt request '(params toolCall title))
                (propertize
                 (format "\n\n\n    %s" (map-nested-elt request '(params toolCall title)))
                 'font-lock-face 'comint-highlight-input)
              "")
            (if diff-button
                (concat diff-button " ")
              "")
            (mapconcat (lambda (action)
                         (agent-shell--make-permission-button
                          :text (map-elt action :label)
                          :help (map-elt action :label)
                          :action (lambda ()
                                    (interactive)
                                    (agent-shell--send-permission-response
                                     :client client
                                     :request-id (map-elt request 'id)
                                     :option-id (map-elt action :option-id)
                                     :state state
                                     :tool-call-id tool-call-id
                                     :message-text (format "Selected: %s" (map-elt action :option))))
                          :keymap keymap
                          :char (map-elt action :char)
                          :option (map-elt action :option)
                          :navigatable t))
                       actions
                       " "))))

(cl-defun agent-shell--send-permission-response (&key client request-id option-id cancelled state tool-call-id message-text)
  "Send permission response and cleanup dialog.

Uses CLIENT to send response with REQUEST-ID and OPTION-ID.
Cleans up dialog using STATE and TOOL-CALL-ID.
Displays MESSAGE-TEXT and moves cursor to end."
  (acp-send-response
   :client client
   :response (acp-make-session-request-permission-response
              :request-id request-id
              :cancelled cancelled
              :option-id option-id))
  ;; Hide permission after sending response.
  ;; block-id must be the same as the one used as
  ;; agent-shell--update-dialog-block param by "session/request_permission".
  (agent-shell--delete-dialog-block :state state :block-id (format "permission-%s" tool-call-id))
  (let ((updated-tool-calls (map-copy (map-elt state :tool-calls))))
    (map-delete updated-tool-calls tool-call-id)
    (map-put! state :tool-calls updated-tool-calls))
  (when message-text
    (message "%s" message-text))
  (goto-char (point-max)))

(cl-defun agent-shell--resolve-permission-choice-to-action (&key choice actions)
  "Resolve `quick-diff' CHOICE to corresponding permission action from ACTIONS.

CHOICE can be \\='accept or \\='reject.
Returns the matching action or nil if no match found."
  (cond
   ((equal choice 'accept)
    (seq-find (lambda (action)
                (string= (map-elt action :kind) "allow_once"))
              actions))
   ((equal choice 'reject)
    (seq-find (lambda (action)
                (string= (map-elt action :kind) "reject_once"))
              actions))
   (t nil)))

(cl-defun agent-shell--make-diff-viewing-function (&key diff actions client request-id state tool-call-id)
  "Create a diffing handler for the ACP CLIENT's REQUEST-ID and TOOL-CALL-ID.

DIFF as per `agent-shell--make-diff-info'.
ACTIONS as per `agent-shell--make-permission-action'."
  (lambda ()
    (interactive)
    (quick-diff
     :old (map-elt diff :old)
     :new (map-elt diff :new)
     :title (file-name-nondirectory (map-elt diff :file))
     :on-exit (lambda (choice)
                (if-let ((action (agent-shell--resolve-permission-choice-to-action
                                  :choice choice
                                  :actions actions)))
                    (agent-shell--send-permission-response
                     :client client
                     :request-id request-id
                     :option-id (map-elt action :option-id)
                     :state state
                     :tool-call-id tool-call-id
                     :message-text (map-elt action :option))
                  (message "Ignored"))))))

(cl-defun agent-shell--make-permission-button (&key text help action keymap navigatable char option)
  "Create a permission button with TEXT, HELP, ACTION, and KEYMAP.

For example:

  \"[ Allow (y) ]\"

When NAVIGATABLE is non-nil, make button character navigatable.
CHAR and OPTION are used for cursor sensor messages."
  (let ((button (agent-shell--make-button
                 :text text
                 :help help
                 :kind 'permission
                 :keymap keymap
                 :action action)))
    (when navigatable
      ;; Make the button character navigatable.
      ;;
      ;; For example, the "y" in:
      ;;
      ;; [ Allow (y) ]
      (put-text-property (- (length button) 3) (- (length button) 1)
                         'agent-shell-permission-button t button)
      (put-text-property (- (length button) 3) (- (length button) 1)
                         'cursor-sensor-functions
                         (list (lambda (_window _old-pos sensor-action)
                                 (when (eq sensor-action 'entered)
                                   (message "Press RET or %c to %s"
                                            char option))))
                         button))
    button))

(defun agent-shell--make-permission-actions (acp-options)
  "Make actions from ACP-OPTIONS for shell rendering.

See `agent-shell--make-permission-action' for ACP-OPTION and return schema."
  (seq-sort (lambda (a b)
              (< (length (map-elt a :label))
                 (length (map-elt b :label))))
            (delq nil (mapcar #'agent-shell--make-permission-action acp-options))))

(defun agent-shell--make-permission-action (acp-option)
  "Convert a single ACP-OPTION to an action alist.

ACP-OPTION should be a PermissionOption per ACP spec:

  https://agentclientprotocol.com/protocol/schema#permissionoption

  An alist of the form:

  ((\='kind . \"allow_once\")
   (\='name . \"Allow\")
   (\='optionId . \"allow\"))

Returns an alist of the form:

  ((:label . \"Allow (y)\")
   (:option . \"Allow\")
   (:char . ?y)
   (:kind . \"allow_once\")
   (:option-id . ...))

Returns nil if the ACP-OPTION kind is not recognized."
  (let* ((char-map '(("allow_always" . ?!)
                     ("allow_once" . ?y)
                     ("reject_once" . ?n)))
         (kind (map-elt acp-option 'kind))
         (char (map-elt char-map kind))
         (name (map-elt acp-option 'name)))
    (when char
      (map-into `((:label . ,(format "%s (%c)" name char))
                  (:option . ,name)
                  (:char . ,char)
                  (:kind . ,kind)
                  (:option-id . ,(map-elt acp-option 'optionId)))
                'alist))))

(defun agent-shell-jump-to-latest-permission-button-row ()
  "Jump to the latest permission button row."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when-let ((found (save-mark-and-excursion
                      (goto-char (point-max))
                      (agent-shell-previous-permission-button))))
    (deactivate-mark)
    (goto-char found)
    (beginning-of-line)
    (agent-shell-next-permission-button)
    (when-let ((window (get-buffer-window (current-buffer))))
      (set-window-point window (point)))))

(defun agent-shell-next-permission-button ()
  "Jump to the next button."
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let ((next-change (next-single-property-change (point) 'agent-shell-permission-button)))
                           (goto-char next-change)))
                       (when-let ((next (text-property-search-forward
                                         'agent-shell-permission-button t t)))
                         (prop-match-beginning next)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-previous-permission-button ()
  "Jump to the previous button."
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let ((prev-change (previous-single-property-change (point) 'agent-shell-permission-button)))
                           (goto-char prev-change)))
                       (when-let ((prev (text-property-search-backward
                                         'agent-shell-permission-button t t)))
                         (prop-match-beginning prev)))))
    (deactivate-mark)
    (goto-char found)
    found))

;;; Region

(cl-defun agent-shell-add-region ()
  "Add region to last accessed shell buffer in project."
  (interactive)
  (let* ((shell-buffer (or (seq-first (agent-shell-project-buffers))
                           (user-error "No agent shell buffers available for current project")))
         (region (or (agent-shell--get-region :deactivate t)
                     (user-error "No region selected")))
         (inhibit-read-only t)
         (insert-start (progn
                         ;; Displaying before with-current-buffer below
                         ;; ensures window is selected, thus window-point
                         ;; is also updated after insertion.
                         (agent-shell--display-buffer shell-buffer)
                         (point-max))))
    (with-current-buffer shell-buffer
      (when (shell-maker-busy)
        (user-error "Busy, try later"))
      (save-excursion
        (save-restriction
          (goto-char insert-start)
          (insert "\n\n")
          (insert (if (map-elt region :file)
                      (sui-add-action-to-text
                       (format "%s:%d-%d"
                               (file-relative-name (map-elt region :file)
                                                   (agent-shell-cwd))
                               (map-elt region :line-start)
                               (map-elt region :line-end))
                       (lambda ()
                         (interactive)
                         (if (and (map-elt region :file) (file-exists-p (map-elt region :file)))
                             (if-let ((window (when (get-file-buffer (map-elt region :file))
                                                (get-buffer-window (get-file-buffer (map-elt region :file))))))
                                 (progn
                                   (select-window window)
                                   (goto-char (point-min))
                                   (forward-line (1- (map-elt region :line-start)))
                                   (beginning-of-line)
                                   (push-mark (save-excursion
                                                (goto-char (point-min))
                                                (forward-line (1- (map-elt region :line-end)))
                                                (end-of-line)
                                                (point))
                                              t t))
                               (find-file (map-elt region :file))
                               (goto-char (point-min))
                               (forward-line (1- (map-elt region :line-start)))
                               (beginning-of-line)
                               (push-mark (save-excursion
                                            (goto-char (point-min))
                                            (forward-line (1- (map-elt region :line-end)))
                                            (end-of-line)
                                            (point))
                                          t t))
                           (message "File not found")))
                       (lambda ()
                         (message "Press RET to open file"))
                       'link)
                    (or (map-elt region :content)
                        "???")))
          (narrow-to-region insert-start (point))
          (markdown-overlays-put))))))

(cl-defun agent-shell--get-region (&key deactivate)
  "Get the active region as an alist.

When DEACTIVATE is non-nil, deactivate region/selection.

Available values:

 :file :language :char-start :char-end :line-start :line-end and :content."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end))
          (content (buffer-substring-no-properties (region-beginning) (region-end)))
          (language (cond ((listp mode-name)
                           (downcase (car mode-name)))
                          ((stringp mode-name)
                           (downcase mode-name))
                          (t
                           "")))
          (file (when-let ((buffer-file-name (buffer-file-name)))
                  (file-relative-name buffer-file-name (agent-shell-cwd)))))
      (when deactivate
        (deactivate-mark))
      `((:file . ,file)
        (:language . ,language)
        (:char-start . ,start)
        (:char-end . ,end)
        (:line-start . ,(save-excursion (goto-char start) (line-number-at-pos)))
        (:line-end . ,(save-excursion (goto-char end) (line-number-at-pos)))
        (:content . ,content)))))

(cl-defun agent-shell--get-decorated-region (&key deactivate)
  "Get the active region decorated with file path and Markdown code block.

When DEACTIVATE is non-nil, deactivate region/selection."
  (when-let ((region-data (agent-shell--get-region :deactivate deactivate)))
    (let ((file (map-elt region-data :file))
          (start (map-elt region-data :char-start))
          (end (map-elt region-data :char-end))
          (language (map-elt region-data :language))
          (content (map-elt region-data :content)))
      (concat (if file
                  (format "%s#C%d-C%d\n\n" file start end)
                "")
              "```"
              language
              "\n"
              content
              "\n"
              "```"))))

;;; Session modes

(defun agent-shell--resolve-session-mode-name (mode-id available-session-modes)
  "Get the name of the session mode with MODE-ID from AVAILABLE-SESSION-MODES.

AVAILABLE-SESSION-MODES is the list of mode objects from the ACP
session/new response.  Each mode has an `id' and `name' field.
We look up the mode by ID to get its display name.

See https://agentclientprotocol.com/protocol/session-modes for details."
  (when-let ((mode (seq-find (lambda (m)
                               (string= mode-id (map-elt m 'id)))
                             available-session-modes)))
    (map-elt mode 'name)))

(defun agent-shell--session-mode-line-format ()
  "Return the mode-line format for displaying the current session mode.

Returns a formatted string like \" [Accept Edits]\" for display in the modeline,
or nil if no session mode is available."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              (state (agent-shell--state))
              (mode-name (agent-shell--resolve-session-mode-name
                          (map-nested-elt state '(:session :mode-id))
                          (map-nested-elt state '(:session :modes)))))
    (propertize (format " [%s]" mode-name)
                'face 'font-lock-constant-face
                'help-echo (format "Session Mode: %s" mode-name))))

(defun agent-shell--setup-modeline ()
  "Set up the modeline to display session mode.
Uses :eval so the mode updates automatically when state changes."
  (setq-local mode-line-misc-info
              (append mode-line-misc-info
                      '((:eval (agent-shell--session-mode-line-format))))))

(defun agent-shell-cycle-session-mode ()
  "Cycle through available session modes for the current `agent-shell' session."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (map-nested-elt (agent-shell--state) '(:session :modes))
    (user-error "No session modes available"))
  (let* ((mode-ids (mapcar (lambda (mode)
                             (map-elt mode 'id))
                           (map-nested-elt (agent-shell--state) '(:session :modes))))
         (mode-idx (or (seq-position mode-ids
                                     (map-nested-elt (agent-shell--state) '(:session :mode-id))
                                     #'string=) -1))
         (next-mode-idx (mod (1+ mode-idx) (length mode-ids)))
         (next-mode-id (nth next-mode-idx mode-ids)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id next-mode-id)
     :buffer (current-buffer)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id next-mode-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Session mode: %s"
                              (agent-shell--resolve-session-mode-name
                               next-mode-id
                               (map-nested-elt (agent-shell--state)
                                               '(:session :modes)))))
                   (force-mode-line-update))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change session mode: %s" error)))))

(defun agent-shell-set-session-mode ()
  "Set session mode (if any available)."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (map-nested-elt (agent-shell--state) '(:session :modes))
    (user-error "No session modes available"))
  (let* ((current-mode-id (map-nested-elt (agent-shell--state) '(:session :mode-id)))
         (default-mode-name (and current-mode-id
                                 (agent-shell--resolve-session-mode-name
                                  current-mode-id
                                  (map-nested-elt (agent-shell--state) '(:session :modes)))))
         (mode-choices (mapcar (lambda (mode)
                                 (cons (map-elt mode 'name)
                                       (map-elt mode 'id)))
                               (map-nested-elt (agent-shell--state) '(:session :modes))))
         (selection (completing-read "Set session mode: "
                                     (mapcar #'car mode-choices)
                                     nil t nil nil default-mode-name))
         (selected-mode-id (cdr (seq-find (lambda (choice)
                                            (string= selection (car choice)))
                                          mode-choices))))
    (unless selected-mode-id
      (user-error "Unknown session mode: %s" selection))
    (when (and current-mode-id (string= selected-mode-id current-mode-id))
      (error "Session mode already %s" selection))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id selected-mode-id)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id selected-mode-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Session mode: %s"
                              (agent-shell--resolve-session-mode-name
                               selected-mode-id
                               (map-nested-elt (agent-shell--state)
                                               '(:session :modes)))))
                   (force-mode-line-update))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change session mode: %s" error)))))

(defun agent-shell--format-available-modes (modes &optional current-mode-id)
  "Format MODES for shell rendering.
If CURRENT-MODE-ID is provided, append \"(current)\" to the matching mode name."
  (let ((max-name-length (seq-reduce (lambda (acc mode)
                                       (let ((name (map-elt mode 'name))
                                             (is-current (and current-mode-id
                                                              (string= (map-elt mode 'id) current-mode-id))))
                                         (max acc (length (if is-current
                                                              (concat name " (current)")
                                                            name)))))
                                     modes
                                     0)))
    (mapconcat
     (lambda (mode)
       (let* ((name (map-elt mode 'name))
              (desc (map-elt mode 'description))
              (is-current (and current-mode-id
                               (string= (map-elt mode 'id) current-mode-id)))
              (display-name (if is-current
                                (concat name " (current)")
                              name)))
         (concat
          (propertize (format (format "%%-%ds" max-name-length) display-name)
                      'font-lock-face 'font-lock-function-name-face)
          "  "
          (propertize desc 'font-lock-face 'font-lock-comment-face))))
     modes
     "\n")))

(provide 'agent-shell)

;;; agent-shell.el ends here
