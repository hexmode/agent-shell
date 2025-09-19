;;; agent-shell.el --- An agent shell powered by ACP -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell
;; Version: 0.1.1

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
(require 'json)
(require 'map)
(require 'markdown-overlays)
(require 'shell-maker)
(require 'sui)
(require 'svg nil :noerror)

(defcustom agent-shell-google-key nil
  "Google API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'agent-shell)

(defcustom agent-shell-anthropic-key nil
  "Anthropic API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'agent-shell)

(defcustom agent-shell-permission-icon "⚠" ;; 􀇾
  "Icon displayed when shell commands require permission to execute."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-icon "💡" ;; 􁷘
  "Icon displayed during the AI's thought process."
  :type 'string
  :group 'agent-shell)

(cl-defun agent-shell--make-state (&key client-maker needs-authentication authenticate-request-maker)
  "Construct shell state.

Shell state is provider-dependent and needs CLIENT-MAKER, NEEDS-AUTHENTICATION
and AUTHENTICATE-REQUEST-MAKER."
  (list (cons :client nil)
        (cons :client-maker client-maker)
        (cons :initialized nil)
        (cons :needs-authentication needs-authentication)
        (cons :authenticate-request-maker authenticate-request-maker)
        (cons :authenticated nil)
        (cons :session-id nil)
        (cons :last-entry-type nil)
        (cons :request-count 0)
        (cons :tool-calls nil)))

(defvar-local agent-shell--state
    (agent-shell--make-state))

(defvar agent-shell--config nil)

(defun agent-shell-start-claude-code-agent ()
  "Start an interactive Claude Code agent shell."
  (interactive)
  (let ((api-key (agent-shell-anthropic-key)))
    (unless api-key
      (user-error "Please set your `agent-shell-anthropic-key'"))
    (with-current-buffer
        (agent-shell--start
         :new-session t
         :mode-line-name "Claude Code"
         :buffer-name "Claude Code"
         :shell-prompt "Claude Code> "
         :shell-prompt-regexp "Claude Code> "
         :icon-name "anthropic.png"
         :client-maker (lambda ()
                         (acp-make-claude-client :api-key api-key))))))

(defun agent-shell-start-gemini-agent ()
  "Start an interactive Gemini CLI agent shell."
  (interactive)
  (let ((api-key (agent-shell-google-key)))
    (unless api-key
      (user-error "Please set your `agent-shell-google-key'"))
    (with-current-buffer
        (agent-shell--start
         :new-session t
         :mode-line-name "Gemini"
         :buffer-name "Gemini"
         :shell-prompt "Gemini> "
         :shell-prompt-regexp "Gemini> "
         :icon-name "gemini.png"
         :needs-authentication t
         :authenticate-request-maker (lambda ()
                                       (acp-make-authenticate-request :method-id "gemini-api-key"))
         :client-maker (lambda ()
                         (acp-make-gemini-client :api-key api-key))))))

(defun agent-shell-interrupt ()
  "Interrupt in-progress request."
  (interactive)
  (unless (eq major-mode 'agent-shell-mode)
    (user-error "Not in a shell"))
  (unless (map-elt agent-shell--state :session-id)
    (user-error "No active session"))
  (acp-send-request
   :client (map-elt agent-shell--state :client)
   :request (acp-make-session-cancel-request
             :session-id (map-elt agent-shell--state :session-id)
             :reason "User cancelled")
   :on-success (lambda (_response)
                 (message "Cancelling..."))))

(cl-defun agent-shell--make-config (&key prompt prompt-regexp)
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

(shell-maker-define-major-mode (agent-shell--make-config) agent-shell-mode-map)

(cl-defun agent-shell--handle (&key command shell)
  "Handle COMMAND using `shell-maker' SHELL."
  (unless (eq major-mode 'agent-shell-mode)
    (user-error "Not in a shell"))
  (with-current-buffer (map-elt shell :buffer)
    (map-put! agent-shell--state :request-count
              ;; TODO: Make public in shell-maker.
              (shell-maker--current-request-id))
    (cond ((not (map-elt agent-shell--state :client))
           (agent-shell--update-dialog-block
            :shell shell
            :state agent-shell--state
            :block-id "starting"
            :label-left (propertize "Initialized" 'font-lock-face 'font-lock-doc-markup-face)
            :body "Creating client..."
            :create-new t
            :expanded t)
           (if (map-elt agent-shell--state :client-maker)
               (progn
                 (map-put! agent-shell--state
                           :client (funcall (map-elt agent-shell--state :client-maker)))
                 (agent-shell--subscribe-to-client-events
                  :shell shell :state agent-shell--state)
                 (agent-shell--handle :command command :shell shell))
             (funcall (map-elt shell :write-output) "No :client-maker found")
             (funcall (map-elt shell :finish-output) nil)))
          ((not (map-elt agent-shell--state :initialized))
           (with-current-buffer (map-elt shell :buffer)
             (agent-shell--update-dialog-block
              :shell shell
              :state agent-shell--state
              :block-id "starting"
              :body "\n\nInitializing..."
              :append t))
           (acp-send-request
            :client (map-elt agent-shell--state :client)
            :request (acp-make-initialize-request :protocol-version 1)
            :on-success (lambda (_response)
                          ;; TODO: More to be handled?
                          (with-current-buffer (map-elt shell :buffer)
                            (map-put! agent-shell--state :initialized t)
                            (agent-shell--handle :command command :shell shell)))
            :on-failure (agent-shell--make-error-handler
                         :state agent-shell--state :shell shell)))
          ((and (map-elt agent-shell--state :needs-authentication)
                (not (map-elt agent-shell--state :authenticated)))
           (with-current-buffer (map-elt shell :buffer)
             (agent-shell--update-dialog-block
              :shell shell
              :state agent-shell--state
              :block-id "starting"
              :body "\n\nAuthenticating..."
              :append t))
           (if (map-elt agent-shell--state :authenticate-request-maker)
               (acp-send-request
                :client (map-elt agent-shell--state :client)
                :request (funcall (map-elt agent-shell--state :authenticate-request-maker))
                :on-success (lambda (_response)
                              ;; TODO: More to be handled?
                              (with-current-buffer (map-elt shell :buffer)
                                (map-put! agent-shell--state :authenticated t)
                                (agent-shell--handle :command command :shell shell)))
                :on-failure (agent-shell--make-error-handler
                             :state agent-shell--state :shell shell))
             (funcall (map-elt shell :write-output) "No :authenticate-request-maker")
             (funcall (map-elt shell :finish-output) nil)))
          ((not (map-elt agent-shell--state :session-id))
           (agent-shell--update-dialog-block
            :shell shell
            :state agent-shell--state
            :block-id "starting"
            :body "\n\nCreating session..."
            :append t)
           (acp-send-request
            :client (map-elt agent-shell--state :client)
            :request (acp-make-session-new-request :cwd (agent-shell-cwd))
            :on-success (lambda (response)
                          (with-current-buffer (map-elt shell :buffer)
                            (map-put! agent-shell--state
                                      :session-id (map-elt response 'sessionId))
                            (with-current-buffer (map-elt shell :buffer)
                              (agent-shell--update-dialog-block
                               :shell shell
                               :state agent-shell--state
                               :block-id "starting"
                               :body "\n\nReady"
                               :append t))
                            (agent-shell--handle :command command :shell shell)))
            :on-failure (agent-shell--make-error-handler
                         :state agent-shell--state :shell shell)))
          (t
           (acp-send-request
            :client (map-elt agent-shell--state :client)
            :request (acp-make-session-prompt-request
                      :session-id (map-elt agent-shell--state :session-id)
                      :prompt `[((type . "text")
                                 (text . ,(substring-no-properties command)))])
            :on-success (lambda (response)
                          (with-current-buffer (map-elt shell :buffer)
                            (let ((success (equal (map-elt response 'stopReason)
                                                  "end_turn")))
                              (unless success
                                (funcall (map-elt shell :write-output)
                                         (agent-shell--stop-reason-description
                                          (map-elt response 'stopReason))))
                              (funcall (map-elt shell :finish-output) t))))
            :on-failure (agent-shell--make-error-handler
                         :state agent-shell--state :shell shell))))))

(cl-defun agent-shell--subscribe-to-client-events (&key shell state)
  "Subscribe SHELL and STATE to ACP events."
  (acp-subscribe-to-errors
   :client (map-elt state :client)
   :on-error (lambda (error)
               (agent-shell--on-error :shell shell :state state :error error)))
  (acp-subscribe-to-notifications
   :client (map-elt state :client)
   :on-notification (lambda (notification)
                      (agent-shell--on-notification :shell shell :state state :notification notification)))
  (acp-subscribe-to-requests
   :client (map-elt state :client)
   :on-request (lambda (request)
                 (agent-shell--on-request :shell shell :state state :request request))))

(cl-defun agent-shell--on-error (&key shell state error)
  "Handle ERROR with SHELL an STATE."
  (let-alist error
    (agent-shell--update-dialog-block
     :shell shell
     :state state
     :block-id "Error"
     :body (or .message "Some error ¯\\_ (ツ)_/¯")
     :create-new t
     :no-navigation t)))

(cl-defun agent-shell--on-notification (&key shell state notification)
  "Handle incoming notification using SHELL, STATE, and NOTIFICATION."
  (let-alist notification
    (cond ((equal .method "session/update")
           (let ((update (map-elt (map-elt notification 'params) 'update)))
             (cond
              ((equal (map-elt update 'sessionUpdate) "tool_call")
               (agent-shell--save-tool-call
                state
                (map-elt update 'toolCallId)
                (list (cons :title (map-elt update 'title))
                      (cons :status (map-elt update 'status))
                      (cons :kind (map-elt update 'kind))
                      (cons :command (map-nested-elt update '(rawInput command)))
                      (cons :description (map-nested-elt update '(rawInput description)))
                      (cons :content (map-elt update 'content))))
               (agent-shell--update-dialog-block
                :shell shell
                :state state
                :block-id (map-elt update 'toolCallId)
                :label-left (agent-shell-make-tool-call-label
                             state (map-elt update 'toolCallId)))
               (map-put! state :last-entry-type "tool_call"))
              ((equal (map-elt update 'sessionUpdate) "agent_thought_chunk")
               (let-alist update
                 ;; (message "agent_thought_chunk: last-type=%s, will-append=%s"
                 ;;          (map-elt state :last-entry-type)
                 ;;          (equal (map-elt state :last-entry-type) "agent_thought_chunk"))
                 (agent-shell--update-dialog-block
                  :shell shell
                  :state state
                  :block-id "agent_thought_chunk"
                  :label-left  (concat
                                agent-shell-thought-process-icon
                                " "
                                (propertize "Thought process" 'font-lock-face font-lock-doc-markup-face))
                  :body .content.text
                  :append (equal (map-elt state :last-entry-type)
                                 "agent_thought_chunk")))
               (map-put! state :last-entry-type "agent_thought_chunk"))
              ((equal (map-elt update 'sessionUpdate) "agent_message_chunk")
               (let-alist update
                 (agent-shell--update-dialog-block
                  :shell shell
                  :state state
                  :block-id "agent_message_chunk"
                  :label-left nil ;;
                  :body .content.text
                  :create-new (not (equal (map-elt state :last-entry-type)
                                          "agent_message_chunk"))
                  :append t
                  :no-navigation t))
               (map-put! state :last-entry-type "agent_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "plan")
               (let-alist update
                 (agent-shell--update-dialog-block
                  :shell shell
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
                  (list (cons :status (map-elt update 'status))
                        (cons :content (map-elt update 'content))))
                 (let ((output (concat
                                "\n\n"
                                (mapconcat (lambda (item)
                                             (let-alist item
                                               .content.text))
                                           .content
                                           "\n\n")
                                "\n\n")))
                   (agent-shell--update-dialog-block
                    :shell shell
                    :state state
                    :block-id .toolCallId
                    :label-left (agent-shell-make-tool-call-label
                                 state .toolCallId)
                    :body (string-trim output))))
               (map-put! state :last-entry-type "tool_call_update"))
              ((equal (map-elt update 'sessionUpdate) "available_commands_update")
               (let-alist update
                 (agent-shell--update-dialog-block
                  :shell shell
                  :state state
                  :block-id "available_commands_update"
                  :label-left (propertize "Available commands" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-available-commands (map-elt update 'availableCommands))))
               (map-put! state :last-entry-type "available_commands_update"))
              (t
               (agent-shell--update-dialog-block
                :shell shell
                :state state
                :block-id "Session Update - fallback"
                :body (format "%s" notification)
                :create-new t
                :no-navigation t)
               (map-put! state :last-entry-type nil)))))
          (t
           (agent-shell--update-dialog-block
            :shell shell
            :state state
            :block-id "Notification - fallback"
            :body (format "%s" notification)
            :create-new t
            :no-navigation t)
           (map-put! state :last-entry-type nil))))
  (with-current-buffer (map-elt shell :buffer)
    (markdown-overlays-put)))

(cl-defun agent-shell--on-request (&key shell state request)
  "Handle incoming request using SHELL, STATE, and REQUEST."
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (agent-shell--save-tool-call
            state .params.toolCall.toolCallId
            (list (cons :title .params.toolCall.title)
                  (cons :status .params.toolCall.status)
                  (cons :kind .params.toolCall.kind)))
           (agent-shell--update-dialog-block
            :shell shell
            :state state
            :label-left (agent-shell-make-tool-call-label
                         state .params.toolCall.toolCallId)
            :block-id .params.toolCall.toolCallId
            :body (with-current-buffer (map-elt shell :buffer)
                    (agent-shell--make-tool-call-permission-text
                     :request request
                     :client (map-elt state :client)
                     :state state))
            :expanded t)
           (agent-shell--prompt-for-permission
            :model (agent-shell--make-prompt-for-permission-model
                    :options .params.options
                    :tool-call (map-nested-elt state `(:tool-calls ,.params.toolCall.toolCallId)))
            :on-choice (lambda (option-id)
                         (acp-send-response
                          :client (map-elt state :client)
                          :response (acp-make-session-request-permission-response
                                     :request-id .id
                                     :option-id option-id))))
           (map-put! state :last-entry-type "session/request_permission"))
          (t
           (agent-shell--update-dialog-block
            :shell shell
            :state state
            :block-id "Unhandled Incoming Request"
            :body (format "⚠ Unhandled incoming request: \"%s\"" .method)
            :create-new t
            :no-navigation t)
           (map-put! state :last-entry-type nil))))
  (with-current-buffer (map-elt shell :buffer)
    (markdown-overlays-put)))

(defun agent-shell--stop-reason-description (stop-reason)
  "Return a human-readable text description for STOP-REASON.

https://agentclientprotocol.com/protocol/schema#param-stop-reason"
  (pcase stop-reason
    ("end_turn" "The language model finishes responding without requesting more tools")
    ("max_tokens" "Max token limit reached")
    ("max_turn_requests" "Exceeded request limit")
    ("refusal" "Refused")
    ("cancelled" "Cancelled")
    (_ (format "Stop for unknown reason: %s" stop-reason))))

(defun agent-shell--format-available-commands (commands)
  "Format COMMANDS for shell rendering."
  (let ((max-name-length (cl-reduce #'max commands
                                    :key (lambda (cmd)
                                           (length (alist-get 'name cmd)))
                                    :initial-value 0)))
    (mapconcat
     (lambda (cmd)
       (let ((name (alist-get 'name cmd))
             (desc (alist-get 'description cmd)))
         (concat
          ;; For commands to be executable, they start with /
          (propertize (format (format "/%%-%ds" max-name-length) name)
                      'font-lock-face 'font-lock-function-name-face)
          "  "
          (propertize desc 'font-lock-face 'font-lock-comment-face))))
     commands
     "\n")))

(cl-defun agent-shell--make-error-handler (&key state shell)
  "Create ACP error handler with SHELL STATE."
  (lambda (error raw-error)
    (let-alist error
      (with-current-buffer (map-elt shell :buffer)
        (agent-shell--update-dialog-block
         :shell shell
         :state agent-shell--state
         :block-id (format "failed-%s-id:%s-code:%s"
                           (map-elt state :request-count)
                           (or .id "?")
                           (or .code "?"))
         :body (agent-shell--make-error-dialog-text
                :code .code
                :message .message
                ;; TODO: Serialize to json and prettify
                :raw-error raw-error)
         :create-new t)))
    ;; TODO: Mark buffer command with shell failure.
    (with-current-buffer (map-elt shell :buffer)
      (funcall (map-elt shell :finish-output) t))))

(defun agent-shell--prepare-permission-actions (options)
  "Format permission OPTIONS for shell rendering."
  (let ((char-map '(("allow_always" . ?!)
                    ("allow_once" . ?y)
                    ("reject_once" . ?n))))
    (seq-sort (lambda (a b)
                (< (length (map-elt a :label))
                   (length (map-elt b :label))))
              (seq-map (lambda (opt)
                         (let* ((kind (map-elt opt 'kind))
                                (char (alist-get kind char-map nil nil #'string=))
                                (name (map-elt opt 'name)))
                           (when char
                             (map-into `((:label . ,(format "%s (%c)" name char))
                                         (:option . ,name)
                                         (:char . ,char)
                                         (:kind . ,kind)
                                         (:option-id . ,(map-elt opt 'optionId)))
                                       'alist))))
                       options))))

(defun agent-shell--make-tool-permission-header ()
  "Create header text for tool permission dialog."
  (concat
   (propertize agent-shell-permission-icon 'font-lock-face 'warning 'face 'warning) " "
   (propertize "Tool Permission" 'font-lock-face 'bold 'face 'bold) " "
   (propertize agent-shell-permission-icon 'font-lock-face 'warning 'face 'warning)))

(cl-defun agent-shell--make-tool-call-permission-text (&key request client state)
  "Create text to render permission dialog using REQUEST, CLIENT, and STATE."
  (let-alist request
    (let ((request-id .id)
          (tool-call-id .params.toolCall.toolCallId)
          (actions (agent-shell--prepare-permission-actions .params.options)))
      (let ((text (format "
%s %s %s%s

%s

"
                          (propertize agent-shell-permission-icon
                                      'font-lock-face 'warning)
                          (propertize "Tool Permission" 'font-lock-face 'bold)
                          (propertize agent-shell-permission-icon
                                      'font-lock-face 'warning)
                          (if .params.toolCall.title
                              (format "\n\n%s" .params.toolCall.title)
                            "")
                          (mapconcat (lambda (action)
                                       (agent-shell--make-button
                                        :text (map-elt action :label)
                                        :help (map-elt action :label)
                                        :kind 'permission
                                        :action (lambda ()
                                                  (interactive)
                                                  (acp-send-response
                                                   :client client
                                                   :response (acp-make-session-request-permission-response
                                                              :request-id request-id
                                                              :option-id (map-elt action :option-id)))
                                                  (sui-collapse-dialog-block-by-id (map-elt state :request-count) tool-call-id))))
                                     actions
                                     " "))))
        (font-lock-append-text-property 0 (length text)
                                        'font-lock-face
                                        `(:background ,(face-background 'next-error nil t) :extend t)
                                        text)
        text))))

(defun agent-shell--save-tool-call (state tool-call-id tool-call)
  "Store TOOL-CALL with TOOL-CALL-ID in STATE's :tool-calls alist."
  (let* ((tool-calls (map-elt state :tool-calls))
         (old-tool-call (map-elt tool-calls tool-call-id))
         (updated-tools (copy-alist tool-calls)))
    (setf (alist-get tool-call-id updated-tools nil nil #'equal)
          (if old-tool-call
              (map-merge 'alist old-tool-call tool-call)
            tool-call))
    (map-put! state :tool-calls updated-tools)))

(cl-defun agent-shell--prompt-for-permission (&key model on-choice)
  "Prompt user for permission using MODEL and invoke ON-CHOICE.

MODEL is of the form by `agent-shell--make-prompt-for-permission-model'.

ON-CHOICE is of the form: (lambda (choice))"
  (let* ((description (if (map-elt model :description)
                          (concat
                           "\n"
                           (agent-shell--make-tool-permission-header) "\n\n"
                           (propertize
                            (string-trim (map-elt model :description))
                            'face 'comint-highlight-input) "\n")
                        (concat (agent-shell--make-tool-permission-header) "\n")))
         (actions (map-elt model :actions))
         (transient-function (intern (format "agent-shell--permission-transient-%s"
                                             (gensym))))
         (cleanup-function (intern (format "%s-cleanup" transient-function))))
    ;; Since transients are defined at runtime, we need to
    ;; create unique interactive functions per invocation, but
    ;; also need to remove them after usage. Thus the cleanup
    ;; hook.
    (eval
     `(progn
        (defun ,cleanup-function ()
          "Cleanup function for transient."
          (fmakunbound ',transient-function)
          (fmakunbound ',cleanup-function)
          (remove-hook 'transient-exit-hook #',cleanup-function))
        (transient-define-prefix ,transient-function ()
          "Permission prompt"
          [:description
           (lambda ()
             ,description)
           :class transient-row
           ,@(mapcar (lambda (action)
                       (let ((option-id (map-elt action :option-id)))
                         `(,(char-to-string (map-elt action :char))
                           ,(map-elt action :label)
                           (lambda ()
                             (interactive)
                             (transient-quit-one)
                             (funcall ',on-choice ',option-id)))))
                     actions)])))

    (add-hook 'transient-exit-hook cleanup-function)

    (funcall transient-function)))

(cl-defun agent-shell--make-prompt-for-permission-model (&key options tool-call)
  "Create a permission prompt model from OPTIONS and optional TOOL-CALL.

Model is of the form:

  ((:description . \"The agent wants to run: git log --oneline -n 10\")
   (:actions . (((:label . \"No (n)\")
                   (:char . ?n)
                   (:kind . \"reject_once\")
                   (:option-id . \"opt-456\"))
                  ((:label . \"Yes (y)\")
                   (:char . ?y)
                   (:kind . \"allow_once\")
                   (:option-id . \"opt-123\"))
                  ((:label . \"Always Approve (!)\")
                   (:char . ?!)
                   (:kind . \"allow_always\")
                   (:option-id . \"opt-789\")))))"
  (let ((description (concat
                      (when (map-elt tool-call :title)
                        (map-elt tool-call :title))
                      (when (and (map-elt tool-call :title)
                                 (map-elt tool-call :description))
                        "\n\n")
                      (when (map-elt tool-call :description)
                        (map-elt tool-call :description))))
        (actions (agent-shell--prepare-permission-actions options)))
    `((:description . ,description)
      (:actions . ,actions))))

(cl-defun agent-shell--make-error-dialog-text (&key code message raw-error)
  "Create formatted error dialog text with CODE, MESSAGE, and RAW-ERROR."
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
                        (insert raw-error)
                        (json-pretty-print-buffer)
                        (buffer-string)))))))

(defun agent-shell--view-as-error (text)
  "Display TEXT in a read-only error buffer."
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
  (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
    (user-error "Not in an agent shell"))
  (when (map-elt agent-shell--state :client)
    (acp-shutdown :client (map-elt agent-shell--state :client))))

(map-elt agent-shell--state :client)

(defun agent-shell--status-label (status)
  "Convert STATUS codes to user-visible labels."
  (let* ((config (pcase status
                   ("pending" '("pending" warning))
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

(defun agent-shell-make-tool-call-label (state tool-call-id)
  "Create tool call label from STATE using TOOL-CALL-ID."
  (when-let ((tool-call (map-nested-elt state `(:tool-calls ,tool-call-id))))
    (let* ((status (map-elt tool-call :status))
           (status-label (when status
                           (agent-shell--status-label status)))
           (kind (map-elt tool-call :kind))
           (kind-label (when kind
                         (agent-shell--add-text-properties
                          (propertize (format " %s " kind) 'font-lock-face 'default)
                          'font-lock-face
                          `(:box t))))
           (title (map-elt tool-call :title))
           (description (map-elt tool-call :description)))
      (concat
       (when status-label
         status-label)
       (when (and status-label kind-label)
         " ")
       (when kind-label
         kind-label)
       (when (or title description)
         " ")
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
              (propertize description 'font-lock-face 'font-lock-doc-markup-face)))))))

(defun agent-shell--format-plan (entries)
  "Format plan ENTRIES for shell rendering."
  (let* ((max-label-width
          (apply #'max (cons 0 (mapcar (lambda (entry)
                                         (length (agent-shell--status-label
                                                  (alist-get 'status entry))))
                                       entries)))))
    (mapconcat
     (lambda (entry)
       (let-alist entry
         (let* ((status-label (agent-shell--status-label .status))
                (label-length (length status-label))
                ;; Add 2 spaces to compensate box padding.
                (padding (make-string
                          (max 1 (+ 2 (- max-label-width label-length)))
                          ?\s)))
           (concat
            status-label
            padding
            .content))))
     entries
     "\n")))

(cl-defun agent-shell--make-button (&key text help kind action)
  "Make button with TEXT, HELP text, KIND, ACTION and NO-BOX."
  (propertize
   (format " %s " text)
   'font-lock-face '(:box t)
   'help-echo help
   'pointer 'hand
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] action)
             (define-key map (kbd "RET") action)
             (define-key map [remap self-insert-command] 'ignore)
             map)
   'button kind))

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

(cl-defun agent-shell--start (&key no-focus new-session mode-line-name welcome-function
                                   buffer-name shell-prompt shell-prompt-regexp
                                   client-maker
                                   needs-authentication
                                   authenticate-request-maker
                                   icon-name)
  "Start an agent shell programmatically.

Set NO-FOCUS to start in background.
Set NEW-SESSION to start a separate new session.
Set MODE-LINE-NAME and BUFFER-NAME for display customization.
Set SHELL-PROMPT and SHELL-PROMPT-REGEXP for shell prompt display.
Set CLIENT-MAKER function to create the ACP client.
Set NEEDS-AUTHENTICATION if ACP agent requires client authentication.
Set AUTHENTICATE-REQUEST-MAKER to create authentication requests.
Set WELCOME-FUNCTION for custom welcome message.

Returns the shell buffer."
  (unless (and client-maker (funcall client-maker))
    (error "No way to create a new client"))
  (unless (version<= "0.82.2" shell-maker-version)
    (error "Please update shell-maker to version 0.82.2 or newer"))
  (let* ((config (agent-shell--make-config
                  :prompt shell-prompt
                  :prompt-regexp shell-prompt-regexp))
         (agent-shell--config config)
         (default-directory (agent-shell-cwd))
         (shell-buffer
          (shell-maker-start agent-shell--config
                             no-focus
                             (or welcome-function
                                 (lambda (_)
                                   (concat "\n" (shell-maker-welcome-message config))))
                             new-session
                             (concat buffer-name
                                     " Agent @ "
                                     (file-name-nondirectory
                                      (string-remove-suffix "/" default-directory)))
                             mode-line-name)))
    (with-current-buffer shell-buffer
      ;; Initialize buffer-local state
      (setq-local agent-shell--state (agent-shell--make-state
                                      :client-maker client-maker
                                      :needs-authentication needs-authentication
                                      :authenticate-request-maker authenticate-request-maker))
      ;; Initialize buffer-local config
      (setq-local agent-shell--config config)
      (setq header-line-format (agent-shell--make-header
                                :icon-name icon-name
                                :title (concat buffer-name " Agent")
                                :location (string-remove-suffix "/" (abbreviate-file-name default-directory))))
      (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
      (sui-mode +1))
    shell-buffer))

(cl-defun agent-shell--update-dialog-block (&key shell state block-id label-left label-right body append create-new no-navigation expanded)
  "Update dialog block in SHELL buffer.

Creates or updates existing dialog using STATE's request count as namespace.
BLOCK-ID uniquely identifies the block.

Dialog can have LABEL-LEFT, LABEL-RIGHT, and BODY.

Optional flags: APPEND text to existing content, CREATE-NEW block,
NO-NAVIGATION to skip navigation, EXPANDED to show block expanded
by default."
  (with-current-buffer (map-elt shell :buffer)
    ;; (message "agent-shell--update-dialog-block: %s" body)
    (shell-maker-with-auto-scroll-edit
     (sui-update-dialog-block
      (sui-make-dialog-block-model
       :namespace-id (map-elt state :request-count)
       :block-id block-id
       :label-left label-left
       :label-right label-right
       :body body)
      :no-navigation no-navigation
      :append append
      :create-new create-new
      :expanded expanded))))

(defun agent-shell--gemini-text ()
  "Colorized Gemini text with Google-branded colors."
  (let ((colors '("#4285F4" "#EA4335" "#FBBC04" "#4285F4" "#34A853" "#EA4335"))
        (text "Gemini")
        (result ""))
    (dotimes (i (length text))
      (setq result (concat result
                           (propertize (substring text i (1+ i))
                                       'font-lock-face `(:foreground ,(nth (mod i (length colors)) colors))))))
    result))

(defun agent-shell-toggle-logging ()
  "Toggle logging."
  (interactive)
  (setq acp-logging-enabled (not acp-logging-enabled))
  (message "Logging: %s" (if acp-logging-enabled "ON" "OFF")))

(defun agent-shell-reset-logs ()
  "Reset all log buffers."
  (interactive)
  (acp-reset-logs)
  (message "Logs reset"))

(defun agent-shell-google-key ()
  "Get the Google API key."
  (cond ((stringp agent-shell-google-key)
         agent-shell-google-key)
        ((functionp agent-shell-google-key)
         (condition-case _err
             (funcall agent-shell-google-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(defun agent-shell-anthropic-key ()
  "Get the Anthropic API key."
  (cond ((stringp agent-shell-anthropic-key)
         agent-shell-anthropic-key)
        ((functionp agent-shell-anthropic-key)
         (condition-case _err
             (funcall agent-shell-anthropic-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(defun agent-shell-next-item ()
  "Go to next item.

Could be a prompt or an expandable item."
  (interactive)
  (unless (eq major-mode 'agent-shell-mode)
    (user-error "Not in a shell"))
  (let* ((prompt-pos (save-excursion
                       (when (comint-next-prompt 1)
                         (point))))
         (block-pos (save-excursion
                      (sui-forward-block)))
         (next-pos (apply 'min (delq nil (list prompt-pos
                                               block-pos)))))
    (when next-pos
      (cond ((eq next-pos prompt-pos)
             (deactivate-mark)
             (goto-char prompt-pos))
            ((eq next-pos block-pos)
             (deactivate-mark)
             (goto-char block-pos))))))

(defun agent-shell-previous-item ()
  "Go to previous item.

Could be a prompt or an expandable item."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in a shell"))
  (let* ((prompt-pos (save-excursion
                       (when (comint-next-prompt (- 1))
                         (point))))
         (block-pos (save-excursion
                      (sui-backward-block)))
         (positions (delq nil (list prompt-pos
                                    block-pos)))
         (next-pos (when positions
                     (apply 'max positions))))
    (when next-pos
      (cond ((eq next-pos prompt-pos)
             (deactivate-mark)
             (goto-char prompt-pos)
             (comint-skip-prompt))
            ((eq next-pos block-pos)
             (deactivate-mark)
             (goto-char block-pos))))))

(defun agent-shell-cwd ()
  "Return the CWD for this shell.

If in a project, use project root."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when (fboundp 'project-root)
        (when-let ((proj (project-current)))
          (project-root proj)))
      default-directory))

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
  (if (display-graphic-p)
      (let* ((image-height (* 3 (default-font-height)))
             (image-width image-height)
             (text-height 25)
             (svg (svg-create (frame-pixel-width) (+ image-height 10)))
             (icon-filename (agent-shell--fetch-agent-icon icon-name)))
        (when icon-filename
          (svg-embed svg icon-filename
                     "image/png" nil
                     :x 0 :y 0 :width image-width :height image-height))
        (svg-text svg title
                  :x (+ image-width 10) :y text-height
                  :fill (face-attribute 'font-lock-variable-name-face :foreground))
        (svg-text svg location
                  :x (+ image-width 10) :y (* 2 text-height)
                  :fill (face-attribute 'font-lock-string-face :foreground))
        (propertize (format " %s" (with-temp-buffer
                                    (svg-insert-image svg)
                                    (buffer-string)))
                    'ignore t
                    'read-only t
                    'face font-lock-comment-face
                    'rear-nonsticky t))
    (format " %s @ %s" title location)))

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
           (cache-dir (file-name-concat (temporary-file-directory) "chatgpt-shell" mode))
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

(provide 'agent-shell)

;;; agent-shell.el ends here
