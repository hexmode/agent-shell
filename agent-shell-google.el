;;; agent-shell-google.el --- Google agent configurations -*- lexical-binding: t; -*-

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
;; This file includes Google-specific configurations.
;;

;;; Code:

(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--interpolate-gradient "agent-shell")

(defcustom agent-shell-google-key nil
  "Google API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'agent-shell)

(defun agent-shell-google-start-gemini ()
  "Start an interactive Gemini CLI agent shell."
  (interactive)
  (let ((api-key (agent-shell-google-key)))
    (unless api-key
      (user-error "Please set your `agent-shell-google-key'"))
    (agent-shell--start
     :new-session t
     :mode-line-name "Gemini"
     :buffer-name "Gemini"
     :shell-prompt "Gemini> "
     :shell-prompt-regexp "Gemini> "
     :icon-name "gemini.png"
     :welcome-function #'agent-shell-google--gemini-welcome-message
     :needs-authentication t
     :authenticate-request-maker (lambda ()
                                   (acp-make-authenticate-request :method-id "gemini-api-key"))
     :client-maker (lambda ()
                     (acp-make-gemini-client :api-key api-key)))))

(defun agent-shell-google--gemini-welcome-message (config)
  "Return Gemini CLI ASCII art as per own repo using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-google--gemini-ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-google--gemini-ascii-art ()
  "Generate Gemini CLI ASCII art, inspired by its codebase.

https://github.com/google-gemini/gemini-cli/tree/main/packages/cli/src/ui/components/Header.tsx
https://github.com/google-gemini/gemini-cli/tree/main/packages/cli/src/ui/components/AsciiArt.ts
https://github.com/google-gemini/gemini-cli/tree/main/packages/cli/src/ui/themes/theme.ts"
  (let* ((text (string-trim "
 ███            █████████  ██████████ ██████   ██████ █████ ██████   █████ █████
░░░███         ███░░░░░███░░███░░░░░█░░██████ ██████ ░░███ ░░██████ ░░███ ░░███
  ░░░███      ███     ░░░  ░███  █ ░  ░███░█████░███  ░███  ░███░███ ░███  ░███
    ░░░███   ░███          ░██████    ░███░░███ ░███  ░███  ░███░░███░███  ░███
     ███░    ░███    █████ ░███░░█    ░███ ░░░  ░███  ░███  ░███ ░░██████  ░███
   ███░      ░░███  ░░███  ░███ ░   █ ░███      ░███  ░███  ░███  ░░█████  ░███
 ███░         ░░█████████  ██████████ █████     █████ █████ █████  ░░█████ █████
░░░            ░░░░░░░░░  ░░░░░░░░░░ ░░░░░     ░░░░░ ░░░░░ ░░░░░    ░░░░░ ░░░░░" "\n"))
         (is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (gradient-colors (if is-dark
                              '("#4796E4" "#847ACE" "#C3677F")
                            '("#3B82F6" "#8B5CF6" "#DD4C4C")))
         (lines (split-string text "\n"))
         (result ""))
    (dolist (line lines)
      (let ((line-length (length line))
            (propertized-line ""))
        (dotimes (i line-length)
          (let* ((char (substring line i (1+ i)))
                 (progress (/ (float i) line-length))
                 (color (agent-shell--interpolate-gradient gradient-colors progress)))
            (setq propertized-line
                  (concat propertized-line
                          (propertize char 'font-lock-face `(:foreground ,color))))))
        (setq result (concat result propertized-line "\n"))))
    (string-trim-right result)))

(defun agent-shell-google--gemini-text ()
  "Colorized Gemini text with Google-branded colors."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (colors (if is-dark
                     '("#4796E4" "#6B82D9" "#847ACE" "#9E6FA8" "#B16C93" "#C3677F")
                   '("#3B82F6" "#5F6CF6" "#8B5CF6" "#A757D0" "#C354A0" "#DD4C4C")))
         (text "Gemini")
         (result ""))
    (dotimes (i (length text))
      (setq result (concat result
                           (propertize (substring text i (1+ i))
                                       'font-lock-face `(:foreground ,(nth (mod i (length colors)) colors))))))
    result))

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

(provide 'agent-shell-google)

;;; agent-shell-google.el ends here
