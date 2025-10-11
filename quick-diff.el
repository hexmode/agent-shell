;;; quick-diff.el --- A quick way to query/display a diff. -*- lexical-binding: t; -*-

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
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'diff-mode)

(cl-defun quick-diff (&key old new on-exit title)
  "Display a diff between OLD and NEW strings in a buffer with conflict markers.

Creates a new buffer showing the differences between OLD and NEW
using `smerge-mode' conflict markers.  The buffer is read-only with
the following key bindings:
  n - next conflict hunk
  p - previous conflict hunk
  q - kill buffer and exit

When the buffer is killed, prompts \"Accept changes?\" and calls
ON-EXIT with t or nil based on the response.

Arguments:
  :OLD       - Original string content
  :NEW       - Modified string content
  :ON-EXIT   - Function called with (t/nil) when buffer is killed
  :TITLE     - Optional title to display in header line
  :OLD-LABEL - Label for old content (default: \"before\")
  :NEW-LABEL - Label for new content (default: \"after\")"
  (let* ((diff-buffer (generate-new-buffer "*quick-diff*"))
         (calling-buffer (current-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "\n")
              (insert (quick-diff--make-diff old new))
              ;; Add overlays to hide scary text.
              (save-excursion
                (goto-char (point-min))
                ;; Hide --- and +++ lines
                (while (re-search-forward "^\\(---\\|\\+\\+\\+\\).*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put overlay 'category 'diff-header)
                    (overlay-put overlay 'display "")
                    (overlay-put overlay 'evaporate t)))
                ;; Replace @@ lines with "Changes"
                (goto-char (point-min))
                (while (re-search-forward "^@@.*@@.*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0)))
                        (face 'diff-hunk-header))  ; or any face you prefer
                    (overlay-put overlay 'category 'diff-header)
                    ;; Intended display is:
                    ;; ╭─────────╮
                    ;; │ changes │
                    ;; ╰─────────╯
                    ;; Using before-string so diff-hunk-next
                    ;; lands on "│" instead of "╭".
                    (overlay-put overlay 'before-string
                                 (propertize "\n╭─────────╮\n" 'face face))
                    (overlay-put overlay 'display
                                 (propertize "│ changes │\n╰─────────╯\n\n" 'face face))
                    (overlay-put overlay 'evaporate t)))))
            (diff-mode)
            (setq header-line-format
                  (concat
                   "  "
                   (when title
                     (concat (propertize title 'face 'mode-line-emphasis) " "))
                   (propertize "n" 'face 'help-key-binding)
                   " next hunk "
                   (propertize "p" 'face 'help-key-binding)
                   " previous hunk "
                   (propertize "q" 'face 'help-key-binding)
                   " exit"))
            (goto-char (point-min))
            (ignore-errors (smerge-next))
            (when on-exit
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (with-current-buffer calling-buffer
                            (funcall on-exit
                                     (condition-case nil
                                         (if (y-or-n-p "Accept changes?")
                                             'accept
                                           'reject)
                                       (quit 'ignore)))
                            ;; Make sure give focus back to calling buffer on exit.
                            (when-let ((calling-window (get-buffer-window calling-buffer)))
                              (select-window calling-window))))
                        nil t))
            (setq buffer-read-only t)
            (let ((map (make-sparse-keymap)))
              (set-keymap-parent map smerge-mode-map)
              (define-key map "n" #'diff-hunk-next)
              (define-key map "p" #'diff-hunk-prev)
              (define-key map "q" #'kill-current-buffer)
              (use-local-map map))))
      (pop-to-buffer diff-buffer '(display-buffer-use-some-window
                                   display-buffer-same-window)))))

(defun quick-diff--make-diff (old new)
  (let ((old-file (make-temp-file "old"))
        (new-file (make-temp-file "new")))
    (with-temp-file old-file (insert old))
    (with-temp-file new-file (insert new))
    (with-temp-buffer
      (call-process "diff" nil t nil "-U3" old-file new-file)
      (buffer-string))))

(provide 'quick-diff)

;;; quick-diff.el ends here
