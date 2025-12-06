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

(defun agent-shell-prompt-compose--show-buffer ()
  "Show a compose buffer for the agent shell."
  (let ((compose-buffer (agent-shell-prompt-compose--buffer)))
    (pop-to-buffer compose-buffer)
    (agent-shell-prompt-compose-mode)))

(defun agent-shell-prompt-compose-send ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
        (prompt (buffer-string)))
    (with-current-buffer shell-buffer
      (agent-shell-insert :text prompt
                          :submit t))
    (kill-buffer (current-buffer))
    (pop-to-buffer shell-buffer)))

(defun agent-shell-prompt-compose-cancel ()
  "Cancel prompt composition."
  (interactive)
  (when (or (string-empty-p (string-trim (buffer-string)))
            (y-or-n-p "Discard compose buffer? "))
    (kill-buffer (current-buffer))))

(defun agent-shell-prompt-compose--buffer ()
  "Get the compose buffer associated with an `agent-shell'."
  (when-let ((shell-buffer (agent-shell-prompt-compose--shell-buffer)))
    (with-current-buffer shell-buffer
      (get-buffer-create (concat (buffer-name shell-buffer)
                                 " [compose]")))))

(defun agent-shell-prompt-compose--shell-buffer ()
  "Get an `agent-shell' buffer (create one if needed)."
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
      (error "No shell to compose on")))))

(defvar agent-shell-prompt-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-send)
    (define-key map (kbd "C-c C-k") #'agent-shell-prompt-compose-cancel)
    map)
  "Keymap for `agent-shell-prompt-compose-mode'.")

(define-derived-mode agent-shell-prompt-compose-mode text-mode "Agent Compose"
  "Major mode for composing agent shell prompts.

\\{agent-shell-prompt-compose-mode-map}"
  (setq-local header-line-format
              (concat
               " "
               (propertize (buffer-name (agent-shell-prompt-compose--buffer))
                           'face 'font-lock-variable-name-face)
               " "
               (propertize "C-c C-c" 'face 'help-key-binding)
               " send"
               " "
               (propertize "C-c C-k" 'face 'help-key-binding)
               " cancel")))

(provide 'agent-shell-prompt-compose)

;;; agent-shell-prompt-compose.el ends here
