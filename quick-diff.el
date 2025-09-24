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
  (let* ((diff-buffer (generate-new-buffer "*quick-diff*"))
         (calling-buffer (current-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t))
              (setq header-line-format " n: next hunk p: previous hunk q: when done")
              (erase-buffer)
              (diff-mode)
              (insert "\n")
              (insert (quick-diff--generate-conflict-markers old new
                                                  (or old-label "before")
                                                  (or new-label "after")))
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
            (smerge-mode 1)
            (setq header-line-format " n: next hunk p: previous hunk q: exit")
            (goto-char (point-min))
            (ignore-errors (smerge-next))
            (when on-exit
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (with-current-buffer calling-buffer
                            (funcall on-exit (y-or-n-p "Accept changes?"))))
                        nil t))
            (setq buffer-read-only t)
            (let ((map (make-sparse-keymap)))
              (set-keymap-parent map smerge-mode-map)
              (define-key map "n" #'smerge-next)
              (define-key map "p" #'smerge-prev)
              (define-key map "q" #'kill-current-buffer)
              (use-local-map map))))
      (pop-to-buffer diff-buffer))))

;; TODO: Find a better way to generate a browsable patch.
(defun quick-diff--generate-conflict-markers (old new old-label new-label)
  "Generate conflict markers from diff between OLD and NEW strings.
OLD-LABEL and NEW-LABEL are used in the conflict marker headers.
Returns a string with git-style conflict markers for differences."
  (let ((old-file (make-temp-file "old"))
        (new-file (make-temp-file "new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert new))
          (with-temp-buffer
            ;; Get unified diff
            (call-process "diff" nil t nil "-U0" old-file new-file)
            (goto-char (point-min))
            (let ((old-lines (split-string old "\n" t))
                  (new-lines (split-string new "\n" t))
                  (result "")
                  (last-pos 0)
                  hunks)
              ;; Collect all hunks first
              (while (re-search-forward "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" nil t)
                (push (list (1- (string-to-number (match-string 1)))  ; old-start
                            (if (match-string 2)                       ; old-count
                                (string-to-number (match-string 2))
                              1)
                            (1- (string-to-number (match-string 3)))   ; new-start
                            (if (match-string 4)                       ; new-count
                                (string-to-number (match-string 4))
                              1))
                      hunks))
              (setq hunks (nreverse hunks))
              ;; Process hunks
              (dolist (hunk hunks)
                (let ((old-start (nth 0 hunk))
                      (old-count (nth 1 hunk))
                      (new-start (nth 2 hunk))
                      (new-count (nth 3 hunk)))
                  ;; Add unchanged lines before this hunk
                  (while (< last-pos old-start)
                    (when (< last-pos (length old-lines))
                      (setq result (concat result (nth last-pos old-lines) "\n")))
                    (setq last-pos (1+ last-pos)))
                  ;; Add conflict marker
                  (setq result (concat result "<<<<<<< " old-label "\n"))
                  ;; Add old content
                  (dotimes (i old-count)
                    (when (< (+ old-start i) (length old-lines))
                      (setq result (concat result (nth (+ old-start i) old-lines) "\n"))))
                  (setq result (concat result "=======\n"))
                  ;; Add new content
                  (dotimes (i new-count)
                    (when (< (+ new-start i) (length new-lines))
                      (setq result (concat result (nth (+ new-start i) new-lines) "\n"))))
                  (setq result (concat result ">>>>>>> " new-label "\n"))
                  (setq last-pos (+ old-start old-count))))
              ;; Add remaining unchanged lines
              (while (< last-pos (length old-lines))
                (setq result (concat result (nth last-pos old-lines) "\n"))
                (setq last-pos (1+ last-pos)))
              ;; If no hunks found, texts are identical
              (if (string= result "")
                  old
                result))))
      (delete-file old-file)
      (delete-file new-file))))

(provide 'quick-diff)

;;; quick-diff.el ends here
