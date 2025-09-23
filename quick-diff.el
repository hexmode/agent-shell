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
(require 'smerge-mode)

(cl-defun quick-diff (&key old new on-exit (old-label "before") (new-label "after"))
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
  :OLD-LABEL - Label for old content (default: \"before\")
  :NEW-LABEL - Label for new content (default: \"after\")"
  (let* ((buf (generate-new-buffer "*quick-diff*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (diff-mode)
              (insert "\n")
              (insert (let ((base-file (make-temp-file "base"))
                            (old-file (make-temp-file "old"))
                            (new-file (make-temp-file "new")))
                        (with-temp-file old-file
                          (insert old)
                          (unless (string-suffix-p "\n" old)
                            (insert "\n")))
                        (with-temp-file new-file
                          (insert new)
                          (unless (string-suffix-p "\n" new)
                            (insert "\n")))
                        (with-temp-buffer
                          (let ((retval (call-process "diff3" nil t nil "-m" old-file base-file new-file)))
                            (delete-file base-file)
                            (delete-file old-file)
                            (delete-file new-file)
                            ;; 0: No differences or no conflicts.
                            ;; 1: Merge conflicts.
                            ;; 2: Error occurred.
                            (when (= retval 2)
                              (error (buffer-substring-no-properties (point-min)
                                                                     (point-max))))
                            (goto-char (point-min))
                            (while (search-forward old-file nil t)
                              (replace-match (or old-label "old")))
                            (goto-char (point-min))
                            (while (search-forward new-file nil t)
                              (replace-match (or new-label "new")))
                            (goto-char (point-min))
                            (flush-lines "^|||||||")
                            (buffer-substring-no-properties (point-min)
                                                            (point-max))))))
              (goto-char (point-min))
              (save-excursion
                (while (re-search-forward
                        (concat
                         "^\\(<<<<<<<[ \t]*\\)" ;; begin marker
                         "\\(.*\\)\n"           ;; begin label
                         "\\(\\(?:.*\n\\)*?\\)"     ;; upper content
                         "\\(=======\n\\)"      ;; maker
                         "\\(\\(?:.*\n\\)*?\\)"     ;; lower content
                         "\\(>>>>>>>[ \t]*\\)"  ;; end marker
                         "\\(.*\\)\n")          ;; end label
                        nil t)
                  (let ((overlay (make-overlay (match-beginning 1)
                                               (match-end 2))))
                    (overlay-put overlay 'category 'conflict-marker)
                    (overlay-put overlay 'display
                                 (concat (propertize (concat " " (match-string 2) " ")
                                                     'face '(:inherit default :box t))
                                         "\n"))
                    (overlay-put overlay 'evaporate t))
                  (let ((overlay (make-overlay (match-beginning 4)
                                               (match-end 4))))
                    (overlay-put overlay 'category 'conflict-marker)
                    (overlay-put overlay 'display
                                 (concat "\n" (propertize (concat " " (match-string 7) " ")
                                                          'face '(:inherit default :box t)) "\n\n"))
                    (overlay-put overlay 'evaporate t))
                  (let ((overlay (make-overlay (match-beginning 6)
                                               (match-end 7))))
                    (overlay-put overlay 'category 'conflict-marker)
                    (overlay-put overlay 'display "")
                    (overlay-put overlay 'face 'warning)
                    (overlay-put overlay 'evaporate t)))))
            (goto-char (point-min))
            (smerge-mode 1)
            (smerge-next)
            ;; Add kill-buffer-hook
            (when on-exit
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (funcall on-exit (y-or-n-p "Accept changes?")))
                        nil t))
            (setq buffer-read-only t)
            (let ((map (make-sparse-keymap)))
              (set-keymap-parent map smerge-mode-map)
              (define-key map "n" #'smerge-next)
              (define-key map "p" #'smerge-prev)
              (define-key map "q" #'kill-current-buffer)
              (use-local-map map))))
      (pop-to-buffer buf))))

(provide 'quick-diff)

;;; quick-diff.el ends here
