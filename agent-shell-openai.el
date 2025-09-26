;;; agent-shell-openai.el --- OpenAI agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

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
;;
;; This file includes OpenAI-specific configurations.
;;

;;; Code:

(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--indent-string "agent-shell")

(defcustom agent-shell-openai-key nil
  "OpenAI API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'agent-shell)

(defun agent-shell-openai-start-codex ()
  "Start an interactive Claude Code agent shell."
  (interactive)
  (let ((api-key (agent-shell-openai-key)))
    (unless api-key
      (user-error "Please set your `agent-shell-openai-key'"))
    (agent-shell--start
     :new-session t
     :mode-line-name "Codex"
     :buffer-name "Codex"
     :shell-prompt "Codex> "
     :shell-prompt-regexp "Codex> "
     :welcome-function #'agent-shell-openai--codex-welcome-message
     :icon-name "openai.png"
     :client-maker (lambda ()
                     (acp-make-codex-client :api-key api-key)))))

(defun agent-shell-openai-key ()
  "Get the OpenAI API key."
  (cond ((stringp agent-shell-openai-key)
         agent-shell-openai-key)
        ((functionp agent-shell-openai-key)
         (condition-case _err
             (funcall agent-shell-openai-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(defun agent-shell-openai--codex-welcome-message (config)
  "Return Codex welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-openai--codex-ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-openai--codex-ascii-art ()
  "Codex ASCII art.

From https://github.com/openai/codex/blob/main/codex-rs/tui/frames/slug/frame_1.txt."
  (let* ((text (string-trim "
          d-dcottoottd
      dot5pot5tooeeod dgtd
    tepetppgde   egpegxoxeet
   cpdoppttd            5pecet
  odc5pdeoeoo            g-eoot
 xp te  ep5ceet           p-oeet
tdg-p    poep5ged          g e5e
eedee     t55ecep            gee
eoxpe    ceedoeg-xttttttdtt og e
 dxcp  dcte 5p egeddd-cttte5t5te
 oddgd dot-5e   edpppp dpg5tcd5
  pdt gt e              tp5pde
    doteotd          dodtedtg
      dptodgptccocc-optdtep
        epgpexxdddtdctpg
" "\n")))
    (propertize text 'font-lock-face 'font-lock-doc-face)))

(provide 'agent-shell-openai)

;;; agent-shell-openai.el ends here
