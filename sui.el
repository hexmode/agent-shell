;;; sui.el --- Interactive shell UI elements -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;;; Commentary:
;;
;; A library for creating interactive shell UI elements.
;;
;; Note: This package is in very early stages and likely has
;; rough edges.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'cursor-sensor)

(cl-defun sui-make-dialog-block-model (&key (namespace-id "global") (block-id "1") label-left label-right body)
  "Create a dialog block model alist.
NAMESPACE-ID, BLOCK-ID, LABEL-LEFT, LABEL-RIGHT, and BODY are the keys."
  (list (cons :namespace-id namespace-id)
        (cons :block-id block-id)
        (cons :label-left (sui--string-or-nil label-left))
        (cons :label-right (sui--string-or-nil label-right))
        (cons :body (sui--string-or-nil body))))

(cl-defun sui-update-dialog-block (model &key append create-new on-post-process no-navigation expanded)
  "Update or add a dialog block using MODEL.

When APPEND is non-nil, append to body instead of replacing.
When CREATE-NEW is non-nil, create new block.
When ON-POST-PROCESS is non-nil, call this function after updating.
When NO-NAVIGATION is non-nil, block won't be TAB navigatable.
When EXPANDED is non-nil, body will be expanded by default.

For existing blocks, the current expansion state is preserved unless overridden."
  (save-excursion
    (let* ((inhibit-read-only t)
           (namespace-id (map-elt model :namespace-id))
           (block-id (format "%s-%s" namespace-id (map-elt model :block-id)))
           (new-label-left (map-elt model :label-left))
           (new-label-right (map-elt model :label-right))
           (new-body (map-elt model :body))
           (match (progn
                    (goto-char (point-max))
                    (text-property-search-backward
                     'sui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :block-id) block-id))
                     t))))
      (when match
        (goto-char (prop-match-beginning match)))
      (if (and match (not create-new))
          ;; Found existing block - delete and regenerate
          (let* ((existing-model (sui--read-dialog-block-at-point))
                 (state (get-text-property (point) 'sui-state))
                 (existing-body (map-elt existing-model :body))
                 (block-start (prop-match-beginning match))
                 (block-end (prop-match-end match))
                 (final-body (if new-body
                                 (if (and append existing-body)
                                     (concat existing-body new-body)
                                   new-body)
                               existing-body))
                 (final-model (list (cons :namespace-id namespace-id)
                                    (cons :block-id (map-elt model :block-id))
                                    (cons :label-left (or new-label-left
                                                          (map-elt existing-model :label-left)))
                                    (cons :label-right (or new-label-right
                                                           (map-elt existing-model :label-right)))
                                    (cons :body final-body))))

            ;; Delete existing block
            (delete-region block-start block-end)
            (goto-char block-start)

            ;; Regenerate from final-model
            (sui--insert-dialog-block final-model block-id
                                      (not (map-elt state :collapsed))
                                      no-navigation))

        ;; Not found or create-new - insert new block
        (goto-char (point-max))
        (insert (sui--required-newlines 2))
        (sui--insert-dialog-block model block-id expanded no-navigation)
        (insert "\n\n"))
      (when on-post-process
        (funcall on-post-process)))))

(defun sui--read-dialog-block-at (block-start block-id)
  "Read dialog block between BLOCK-START and BLOCK-END with BLOCK-ID into a model."
  (let ((namespace-id nil)
        (id nil)
        (label-left nil)
        (label-right nil)
        (body nil)
        (collapsed nil)
        (state (get-text-property block-start 'sui-state)))

    ;; Extract namespace-id from block-id if it contains a dash
    (when (string-match "^\\(.+\\)-\\(.+\\)$" block-id)
      (setq namespace-id (match-string 1 block-id))
      (setq id (match-string 2 block-id)))

    (save-excursion
      (goto-char block-start)
      (setq collapsed (map-elt state :collapsed))
      (when (and (map-elt state :label-left-start)
                 (map-elt state :label-left-end))
        (setq label-left (buffer-substring (map-elt state :label-left-start)
                                           (map-elt state :label-left-end))))
      (when (and (map-elt state :label-right-start)
                 (map-elt state :label-right-end))
        (setq label-right (buffer-substring (map-elt state :label-right-start)
                                            (map-elt state :label-right-end))))
      (setq body (map-elt state :body)))
    (delq nil (list (when namespace-id
                      (cons :namespace-id namespace-id))
                    (when id
                      (cons :block-id id))
                    (when label-left
                      (cons :label-left label-left))
                    (when label-right
                      (cons :label-right label-right))
                    (when body
                      (cons :body body))
                    (cons :collapsed collapsed)))))

(defun sui--read-dialog-block-at-point ()
  "Read dialog block at point, returning model or nil if none found."
  (when-let ((state (get-text-property (point) 'sui-state)))
    (sui--read-dialog-block-at (map-elt state :block-start)
                               (map-elt state :block-id))))

(defun sui--insert-dialog-block (model block-id &optional expanded no-navigation)
  "Insert dialog block from MODEL with BLOCK-ID text properties.
EXPANDED determines initial state (default nil for collapsed).
NO-NAVIGATION omits sui-navigatable property to exclude from navigation."
  (let ((block-start (point))
        (block-end)
        (label-left (map-elt model :label-left))
        (label-right (map-elt model :label-right))
        (body (map-elt model :body))
        (indicator-start)
        (indicator-end)
        (label-left-start)
        (label-left-end)
        (label-right-start)
        (label-right-end)
        (body-start)
        (body-end)
        (collapsable nil)
        (need-space nil))

    ;; Insert collapse indicator if body exists
    (when (and body (or label-left label-right))
      (setq collapsable t)
      (setq indicator-start (point))
      (insert (sui-add-action-to-text
               (if expanded "▼ " "▶ ")
               (lambda ()
                 (interactive)
                 (sui-toggle-dialog-block-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      ;; (put-text-property indicator-start (point) 'sui-block-id block-id)
      ;; (put-text-property indicator-start (point) 'sui-collapsed (not expanded))
      ;; (put-text-property indicator-start (point) 'sui-indicator t)
      (setq indicator-end (point))
      (put-text-property indicator-start (point) 'sui-section 'indicator)
      (put-text-property indicator-start (point) 'keymap (sui-make-action-keymap
                                                          (lambda ()
                                                            (interactive)
                                                            (sui-toggle-dialog-block-at-point))))
      (put-text-property indicator-start (point) 'read-only t)
      (put-text-property indicator-start (point) 'front-sticky '(read-only)))

    (when label-left
      (setq label-left-start (point))
      (insert (sui-add-action-to-text
               label-left
               (lambda ()
                 (interactive)
                 (sui-toggle-dialog-block-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-left-end (point))
      (put-text-property label-left-start (point) 'sui-section 'label-left)
      ;; (put-text-property label-left-start (point) 'sui-block-id block-id)
      (put-text-property label-left-start (point) 'help-echo block-id)
      (put-text-property label-left-start (point) 'read-only t)
      (put-text-property label-left-start (point) 'front-sticky '(read-only))
      (setq need-space t))

    (when label-right
      (when need-space (insert " "))
      (setq label-right-start (point))
      (insert (sui-add-action-to-text
               label-right
               (lambda ()
                 (interactive)
                 (sui-toggle-dialog-block-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-right-end (point))
      (put-text-property label-right-start (point) 'sui-section 'label-right)
      ;; (put-text-property label-right-start (point) 'sui-block-id block-id)
      (put-text-property label-right-start (point) 'help-echo block-id)
      (put-text-property label-right-start (point) 'read-only t)
      (put-text-property label-right-start (point) 'front-sticky '(read-only)))

    (when body
      (when (or label-left label-right)
        (insert "\n\n"))
      (setq body-start (point))
      ;; Removing 2 space indentation if found as it's added again below.
      (insert (string-remove-prefix "  " body))
      (setq body-end (point))
      ;; Now indent each line in place
      (save-excursion
        (goto-char body-start)
        (while (< (point) body-end)
          (unless (looking-at "^$")  ; Don't indent empty lines
            (let ((indent-start (point)))
              (insert "  ")  ; Two spaces for indentation
              (when (get-text-property 0 'face body)
                (put-text-property
                 indent-start (point)
                 'face (get-text-property 0 'face body)))
              (when (get-text-property 0 'font-lock-face body)
                (put-text-property
                 indent-start (point)
                 'font-lock-face (get-text-property 0 'font-lock-face body)))
              (setq body-end (+ body-end 2))))  ; Adjust body-end position for inserted spaces
          (forward-line 1)))

      (setq body-end (point))
      (setq block-end (point))
      (put-text-property body-start body-end 'sui-section 'body)
      (put-text-property body-start body-end 'help-echo block-id)
      (put-text-property body-start body-end 'read-only t)
      (put-text-property body-start body-end 'front-sticky '(read-only))

      (when-let ((is-collapsable collapsable)
                 (body-overlay (make-overlay (or label-right-end
                                                 label-left-end) body-end)))
        (overlay-put body-overlay 'evaporate t)
        (overlay-put body-overlay 'sui-section 'body)
        (overlay-put body-overlay 'invisible (not expanded))))
    (put-text-property
     block-start body-end
     'sui-state (list (cons :block-start block-start)
                      (cons :block-end block-end)
                      (cons :indicator-start indicator-start)
                      (cons :indicator-end indicator-end)
                      (cons :label-left-start label-left-start)
                      (cons :label-left-end label-left-end)
                      (cons :label-right-start label-right-start)
                      (cons :label-right-end label-right-end)
                      (cons :body body)
                      (cons :body-start body-start)
                      (cons :body-end body-end)
                      (cons :block-id block-id)
                      (cons :collapsed (not expanded))
                      (cons :navigatable (not no-navigation))))
    (put-text-property block-start body-end 'read-only t)
    (put-text-property block-start body-end 'front-sticky '(read-only))))

(defun sui--required-newlines (desired)
  "Return string of newlines needed to reach DESIRED (max 2) before point."
  (let ((desired (min 2 desired)))
    (make-string
     (cond
      ((looking-back "\n\n" (- (point) 2)) 0)
      ((looking-back "\n" (- (point) 1)) (max 0 (- desired 1)))
      (t desired))
     ?\n)))

(defun sui-toggle-dialog-block-at-point ()
  "Toggle visibility of dialog block body at point."
  (interactive)
  (save-excursion
    (let* ((inhibit-read-only t)
           (buffer-undo-list t)
           (state (get-text-property (point) 'sui-state))
           (body-overlay (seq-first (overlays-in (map-elt state :body-start)
                                                 (map-elt state :body-end)))))
      (when (equal (overlay-get body-overlay 'sui-section) 'body)
        (if (map-elt state :collapsed)
            ;; Expand
            (progn
              (overlay-put body-overlay 'invisible nil)
              (delete-region (map-elt state :indicator-start)
                             (map-elt state :indicator-end))
              (goto-char (map-elt state :indicator-start))
              (insert (sui-add-action-to-text
                       "▼ "
                       (lambda ()
                         (interactive)
                         (sui-toggle-dialog-block-at-point))
                       (lambda ()
                         (message "Press RET to toggle"))))
              (map-put! state :collapsed nil))
          ;; Collapse
          (progn
            (overlay-put body-overlay 'invisible t)
            (delete-region (map-elt state :indicator-start)
                           (map-elt state :indicator-end))
            (goto-char (map-elt state :indicator-start))
            (insert (sui-add-action-to-text
                     "▶ "
                     (lambda ()
                       (interactive)
                       (sui-toggle-dialog-block-at-point))
                     (lambda ()
                       (message "Press RET to toggle"))))
            (map-put! state :collapsed t)))
        (put-text-property (map-elt state :block-start)
                           (map-elt state :block-end) 'sui-state state)))))

(defun sui-collapse-dialog-block-by-id (namespace-id block-id)
  "Collapse dialog block with NAMESPACE-ID and BLOCK-ID."
  (save-excursion
    (let ((full-block-id (format "%s-%s" namespace-id block-id)))
      (goto-char (point-max))
      (when (text-property-search-backward
             'sui-state block-id
             (lambda (_ state)
               (equal (map-elt state :block-id) full-block-id))
             t)
        (sui-toggle-dialog-block-at-point)))))

(defun sui--string-or-nil (str)
  "Return STR if it is not nil and not empty, otherwise nil."
  (and str (not (string-empty-p str)) str))

(defun sui--indent-text (text &optional indent-string)
  "Indent TEXT by adding INDENT-STRING to the beginning of each non-empty line.
INDENT-STRING defaults to two spaces."
  (when text
    (let ((indent (or indent-string "  ")))
      (mapconcat (lambda (line)
                   (if (string-empty-p line)
                       line
                     (concat indent line)))
                 (split-string text "\n")
                 "\n"))))

(defun sui-forward-block ()
  "Jump to the next block."
  (interactive)
  (let* ((start-point (point))
         (state (get-text-property (point) 'sui-state)))
    ;; If in navigatable block, move past it first
    (when (map-elt state :navigatable)
      (goto-char (map-elt state :block-end)))
    ;; Now find the next navigatable block
    (if-let ((next (text-property-search-forward
                    'sui-state nil
                    (lambda (_old-val new-val)
                      (and new-val (map-elt new-val :navigatable)))
                    t)))
        (goto-char (map-elt (prop-match-value next) :block-start))
      (goto-char start-point)
      (message "No more blocks"))))

(defun sui-backward-block ()
  "Jump to the previous block."
  (interactive)
  (let* ((start-point (point))
         (state (get-text-property (point) 'sui-state)))
    ;; If in navigatable block, move to its start first
    (when (map-elt state :navigatable)
      (goto-char (map-elt state :block-start)))
    ;; Now find the previous navigatable block
    (if-let ((prev (text-property-search-backward
                    'sui-state nil
                    (lambda (_old-val new-val)
                      (and new-val (map-elt new-val :navigatable)))
                    t)))
        (goto-char (map-elt (prop-match-value prev) :block-start))
      (goto-char start-point)
      (message "No more blocks"))))

(defun sui-make-action-keymap (action)
  "Create keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun sui-add-action-to-text (text action &optional on-entered)
  "Add ACTION lambda to propertized TEXT and return modified text.
ON-ENTERED is a function to call when the cursor enters the text."
  (add-text-properties 0 (length text)
                       `(keymap ,(sui-make-action-keymap action))
                       text)
  (when on-entered
    (add-text-properties 0 (length text)
                         (list 'cursor-sensor-functions
                               (list (lambda (_window _old-pos sensor-action)
                                       (when (eq sensor-action 'entered)
                                         (funcall on-entered)))))
                         text))
  text)

(defvar sui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'sui-forward-block)
    (define-key map (kbd "<tab>") #'sui-forward-block)
    (define-key map (kbd "<backtab>") #'sui-backward-block)
    (define-key map (kbd "S-TAB") #'sui-backward-block)
    map)
  "Keymap for `sui-mode'.")

;;;###autoload
(define-minor-mode sui-mode
  "Minor mode for SUI block navigation."
  :lighter " SUI"
  :keymap sui-mode-map
  (if sui-mode
      (cursor-sensor-mode 1)
    (cursor-sensor-mode -1)))

(provide 'sui)

;;; sui.el ends here
