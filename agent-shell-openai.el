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

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell-start "agent-shell")

(cl-defun agent-shell-openai-make-authentication (&key api-key)
  "Create OpenAI authentication configuration.

API-KEY is the OpenAI API key string or function that returns it."
  (unless api-key
    (error "Must specify :api-key"))
  `((:api-key . ,api-key)))

(defcustom agent-shell-openai-authentication nil
  "Configuration for OpenAI authentication.
For API key (string):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :api-key (lambda () ...)))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-openai-codex-command
  '("codex-acp")
  "Command and parameters for the OpenAI Codex client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-openai-make-codex-config ()
  "Create a Codex agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (when (and (boundp 'agent-shell-openai-key) agent-shell-openai-key)
    (user-error "Please migrate to use agent-shell-openai-authentication and eval (setq agent-shell-openai-key nil)"))
  (agent-shell-make-agent-config
   :new-session t
   :mode-line-name "Codex"
   :buffer-name "Codex"
   :shell-prompt "Codex> "
   :shell-prompt-regexp "Codex> "
   :welcome-function #'agent-shell-openai--codex-welcome-message
   :icon-name "openai.png"
   :client-maker (lambda ()
                   (agent-shell-openai-make-codex-client))
   :install-instructions "See https://github.com/cola-io/codex-acp for installation."))

(defun agent-shell-openai-start-codex ()
  "Start an interactive Codex agent shell."
  (interactive)
  (agent-shell-start
   :config (agent-shell-openai-make-codex-config)))

(defun agent-shell-openai-make-codex-client ()
  "Create a Codex client using configured authentication.

Uses `agent-shell-openai-authentication' for authentication configuration."
  (let ((api-key (agent-shell-openai-key)))
    (unless api-key
      (user-error "Please set your `agent-shell-openai-authentication'"))
    (acp-make-client :command (car agent-shell-openai-codex-command)
                     :command-params (cdr agent-shell-openai-codex-command)
                     :environment-variables (list (format "OPENAI_API_KEY=%s" api-key)))))

(defun agent-shell-openai-key ()
  "Get the OpenAI API key."
  (cond ((stringp (map-elt agent-shell-openai-authentication :api-key))
         (map-elt agent-shell-openai-authentication :api-key))
        ((functionp (map-elt agent-shell-openai-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-openai-authentication :api-key))
           (error
            (error "Api key not found.  Check out `agent-shell-openai-authentication'"))))
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
