;;; agent-shell-prompt-compose.el --- Agent shell prompt compose buffer  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
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

;; Prompt compose buffers enable crafting more involved queries and
;; simplify both response navigation and follow-up queries.
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(require 'seq)
(require 'subr-x)

(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-insert "agent-shell")

(defvar agent-shell-preferred-agent-config)

(defvar agent-shell-prompt-compose--experimental-compose nil)

(defun agent-shell-prompt-compose--show-buffer ()
  "Show a compose buffer for the agent shell."
  (let ((compose-buffer (agent-shell-prompt-compose--buffer)))
    (pop-to-buffer compose-buffer)
    (agent-shell-prompt-compose-edit-mode)))

(defun agent-shell-prompt-compose-send ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (if agent-shell-prompt-compose--experimental-compose
      (agent-shell-prompt-compose-send-and-wait-for-response)
    (agent-shell-prompt-compose-send-and-kill)))

(defun agent-shell-prompt-compose-send-and-kill ()
  "Send the composed prompt to the agent shell and kill compose buffer."
  (interactive)
  (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
        (compose-buffer (current-buffer))
        (prompt (buffer-string)))
    (with-current-buffer shell-buffer
      (agent-shell-insert :text prompt
                          :submit t))
    (kill-buffer compose-buffer)
    (pop-to-buffer shell-buffer)))

(defun agent-shell-prompt-compose-send-and-wait-for-response ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (catch 'exit
    (unless (derived-mode-p 'agent-shell-prompt-compose-edit-mode)
      (user-error "Not in a shell compose buffer"))
    (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
          (compose-buffer (agent-shell-prompt-compose--buffer))
          (prompt (string-trim (buffer-string))))
      (with-current-buffer shell-buffer
        (when shell-maker--busy
          (unless (y-or-n-p "Interrupt?")
            (throw 'exit nil))
          (agent-shell-interrupt t)
          (with-current-buffer compose-buffer
            (agent-shell-prompt-compose-view-mode)
            (agent-shell-prompt-compose--initialize
             :prompt prompt))
          (user-error "Aborted")))
      (when (string-empty-p (string-trim prompt))
        (agent-shell-prompt-compose--initialize)
        (user-error "Nothing to send"))
      (if (derived-mode-p 'agent-shell-prompt-compose-view-mode)
          (progn
            (agent-shell-prompt-compose-edit-mode)
            (agent-shell-prompt-compose--initialize))
        (let ((inhibit-read-only t))
          (markdown-overlays-put))
        (agent-shell-prompt-compose-view-mode)
        (agent-shell-prompt-compose--initialize :prompt prompt)
        ;; (setq view-exit-action 'kill-buffer) TODO
        (when (string-equal prompt "clear")
          (agent-shell-prompt-compose-edit-mode)
          (agent-shell-prompt-compose--initialize))
        (agent-shell-insert :text prompt
                            :submit t
                            :no-focus t)
        ;; TODO: Point should go to beginning of response after submission.
        (let ((inhibit-read-only t))
          (markdown-overlays-put))))))

(defun agent-shell-prompt-compose-interrupt ()
  "Interrupt active agent shell request."
  (interactive)
  (catch 'exit
    (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
          (compose-buffer (agent-shell-prompt-compose--buffer)))
      (with-current-buffer shell-buffer
        (unless shell-maker--busy
          (user-error "No pending request"))
        (unless (y-or-n-p "Interrupt?")
          (throw 'exit nil))
        (agent-shell-interrupt t))
      (with-current-buffer compose-buffer
        (agent-shell-prompt-compose-edit-mode)
        (agent-shell-prompt-compose--initialize))
      (user-error "Aborted"))))

(cl-defun agent-shell-prompt-compose--initialize (&key prompt response)
  "Initialize compose buffer.

Optionally set its PROMPT and RESPONSE."
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when prompt
      (insert
       (if (derived-mode-p 'agent-shell-prompt-compose-view-mode)
           (propertize (concat prompt "\n\n")
                       'rear-nonsticky t
                       'agent-shell-prompt-compose-prompt t
                       'face 'font-lock-doc-face)
         prompt)))
    (when response
      (insert response))))

(defun agent-shell-prompt-compose--prompt ()
  "Return the buffer prompt."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-prompt-compose-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-prompt-compose-prompt)))
                (found (get-text-property start 'agent-shell-prompt-compose-prompt)))
      (string-trim
       (buffer-substring-no-properties
        start
        (or (next-single-property-change
             start 'agent-shell-prompt-compose-prompt)
            (point-max)))))))

(defun agent-shell-prompt-compose--response ()
  "Return the buffer response."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-prompt-compose-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-prompt-compose-prompt)))
                (found (get-text-property start 'agent-shell-prompt-compose-prompt))
                (end (next-single-property-change start 'agent-shell-prompt-compose-prompt)))
      (buffer-substring end (point-max)))))

(defun agent-shell-prompt-compose-cancel ()
  "Cancel prompt composition."
  (interactive)
  (when (or (string-empty-p (string-trim (buffer-string)))
            (y-or-n-p "Discard compose buffer? "))
    (kill-buffer (current-buffer))))

(defun agent-shell-prompt-compose-next-item ()
  "Go to next item."
  (interactive)
  (unless (derived-mode-p 'agent-shell-prompt-compose-view-mode)
    (error "Not in a compose buffer"))
  (let* ((block-pos (save-mark-and-excursion
                      (agent-shell-ui-forward-block)))
         (button-pos (save-mark-and-excursion
                       (agent-shell-next-permission-button)))
         (next-pos (when (or block-pos button-pos)
                     (apply #'min (delq nil (list block-pos
                                                  button-pos))))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos))))

(defun agent-shell-prompt-compose-previous-item ()
  "Go to previous item."
  (interactive)
  (unless (derived-mode-p 'agent-shell-prompt-compose-view-mode)
    (error "Not in a compose buffer"))
  (let* ((current-pos (point))
         (block-pos (save-mark-and-excursion
                      (let ((pos (agent-shell-ui-backward-block)))
                        (when (and pos (< pos current-pos))
                          pos))))
         (button-pos (save-mark-and-excursion
                       (let ((pos (agent-shell-previous-permission-button)))
                         (when (and pos (< pos current-pos))
                           pos))))
         (positions (delq nil (list block-pos
                                    button-pos)))
         (next-pos (when positions
                     (apply #'max positions))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos))))

(cl-defun agent-shell-prompt-compose--buffer (&key shell-buffer existing-only)
  "Get the compose buffer associated with a SHELL-BUFFER.

With EXISTING-ONLY, only return existing buffers without creating."
  (when-let ((shell-buffer (or shell-buffer
                               (agent-shell-prompt-compose--shell-buffer))))
    (with-current-buffer shell-buffer
      (let* ((compose-buffer-name (concat (buffer-name shell-buffer)
                                          " [compose]"))
             (compose-buffer (get-buffer compose-buffer-name)))
        (if compose-buffer
            compose-buffer
          (if existing-only
              nil
            (get-buffer-create compose-buffer-name)))))))

(defun agent-shell-prompt-compose-reply ()
  "Reply as a follow-up and compose another query."
  (interactive)
  (with-current-buffer (agent-shell-prompt-compose--shell-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait"))
    (goto-char (point-max)))
  (agent-shell-prompt-compose-edit-mode)
  (agent-shell-prompt-compose--initialize))

(cl-defun agent-shell-prompt-compose--shell-buffer (&key no-error)
  "Get an `agent-shell' buffer (create one if needed).

With NO-ERROR, return nil instead of raising an error."
  (get-buffer
   (or
    (seq-first (agent-shell-project-buffers))
    (if (y-or-n-p "No shells in project.  Start a new one? ")
        (agent-shell--start :config (or agent-shell-preferred-agent-config
                                        (agent-shell-select-config
                                         :prompt "Start new agent: ")
                                        (error "No agent config found"))
                            :no-focus t
                            :new-session t)
      (when no-error
        (error "No shell to compose on"))))))

(defvar agent-shell-prompt-compose-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-send)
    (define-key map (kbd "C-c C-k") #'agent-shell-prompt-compose-cancel)
    map)
  "Keymap for `agent-shell-prompt-compose-edit-mode'.")

(define-derived-mode agent-shell-prompt-compose-edit-mode text-mode "Agent Compose"
  "Major mode for composing agent shell prompts.

\\{agent-shell-prompt-compose-edit-mode-map}"
  (setq buffer-read-only nil)
  (setq-local header-line-format
              (concat
               " "
               (propertize (buffer-name (agent-shell-prompt-compose--buffer))
                           'face 'font-lock-variable-name-face)
               " "
               (propertize (key-description (where-is-internal 'agent-shell-prompt-compose-send agent-shell-prompt-compose-edit-mode-map t))
                           'face 'help-key-binding)
               " send"
               " "
               (propertize (key-description (where-is-internal 'agent-shell-prompt-compose-cancel agent-shell-prompt-compose-edit-mode-map t))
                           'face 'help-key-binding)
               " cancel"))
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defvar agent-shell-prompt-compose-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-interrupt)
    (define-key map (kbd "<tab>") #'agent-shell-prompt-compose-next-item)
    (define-key map (kbd "<backtab>") #'agent-shell-prompt-compose-previous-item)
    (define-key map (kbd "r") #'agent-shell-prompt-compose-reply)
    map)
  "Keymap for `agent-shell-prompt-compose-view-mode'.")

(define-derived-mode agent-shell-prompt-compose-view-mode text-mode "Agent Compose"
  "Major mode for viewing agent shell prompts (read-only).

\\{agent-shell-prompt-compose-view-mode-map}"
  (setq-local header-line-format
              (concat
               " "
               (propertize (buffer-name (agent-shell-prompt-compose--buffer))
                           'face 'font-lock-variable-name-face)
               " "
               (propertize (key-description (where-is-internal 'agent-shell-prompt-compose-next-item agent-shell-prompt-compose-view-mode-map t))
                           'face 'help-key-binding)
               " next"
               " "
               (propertize (key-description (where-is-internal 'agent-shell-prompt-compose-previous-item agent-shell-prompt-compose-view-mode-map t))
                           'face 'help-key-binding)
               " previous"
               " "
               (propertize (key-description (where-is-internal 'agent-shell-prompt-compose-reply agent-shell-prompt-compose-view-mode-map t))
                           'face 'help-key-binding)
               " reply"
               " "
               (propertize (key-description (where-is-internal 'agent-shell-prompt-compose-interrupt agent-shell-prompt-compose-view-mode-map t))
                           'face 'help-key-binding)
               " interrupt"))
  (setq buffer-read-only t))

(provide 'agent-shell-prompt-compose)

;;; agent-shell-prompt-compose.el ends here
