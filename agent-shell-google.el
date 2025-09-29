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

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell--interpolate-gradient "agent-shell")
(declare-function agent-shell--ensure-executable "agent-shell")

(cl-defun agent-shell-google-make-authentication (&key api-key login)
  "Create Google authentication configuration.

API-KEY is the Google API key string or function that returns it.
LOGIN when non-nil indicates to use login-based authentication.

Only one of API-KEY or LOGIN should be provided, never both."
  (when (and api-key login)
    (error "Cannot specify both :api-key and :login - choose one"))
  (unless (or api-key login)
    (error "Must specify either :api-key or :login"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (login `((:login . t)))))

(defcustom agent-shell-google-authentication
  (agent-shell-google-make-authentication :login t)
  "Configuration for Google authentication.

For login-based authentication (default):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))

For API key (string):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-google-gemini-command
  '("gemini" "--experimental-acp")
  "Command and parameters for the Gemini client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-google-start-gemini ()
  "Start an interactive Gemini CLI agent shell."
  (interactive)
  (when (and (boundp 'agent-shell-google-key) agent-shell-google-key)
    (user-error "Please migrate to use agent-shell-google-authentication and eval (setq agent-shell-google-key nil)"))
  (agent-shell--ensure-executable (car agent-shell-google-gemini-command)
                                  "See https://github.com/google-gemini/gemini-cli for installation.")
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
                                 (if (map-elt agent-shell-google-authentication :api-key)
                                     (acp-make-authenticate-request :method-id "gemini-api-key")
                                   (acp-make-authenticate-request :method-id "oauth-personal")))
   :client-maker #'agent-shell-google-make-gemini-client))

(defun agent-shell-google-make-gemini-client ()
  "Create a Gemini client using configured authentication.

Uses `agent-shell-google-authentication' for authentication configuration."
  (when (and (boundp 'agent-shell-google-key) agent-shell-google-key)
    (user-error "Please migrate to use agent-shell-google-authentication and eval (setq agent-shell-google-key nil)"))
  (cond
   ((map-elt agent-shell-google-authentication :api-key)
    (acp-make-client :command (car agent-shell-google-gemini-command)
                     :command-params (cdr agent-shell-google-gemini-command)
                     :environment-variables (when-let ((api-key (agent-shell-google-key)))
                                              (list (format "GEMINI_API_KEY=%s" api-key)))))
   ((map-elt agent-shell-google-authentication :login)
    (acp-make-client :command (car agent-shell-google-gemini-command)
                     :command-params (cdr agent-shell-google-gemini-command)))
   (t
    (error "Invalid authentication configuration"))))

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
  (cond ((stringp (map-elt agent-shell-google-authentication :api-key))
         (map-elt agent-shell-google-authentication :api-key))
        ((functionp (map-elt agent-shell-google-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-google-authentication :api-key))
           (error
            "Api key not found.  Check out `agent-shell-google-authentication'")))
        (t
         nil)))

(provide 'agent-shell-google)

;;; agent-shell-google.el ends here
