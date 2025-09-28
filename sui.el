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

(cl-defun sui-update-dialog-block (model &key append create-new on-post-process navigation expanded)
  "Update or add a dialog block using MODEL.

When APPEND is non-nil, append to body instead of replacing.
When CREATE-NEW is non-nil, create new block.
When ON-POST-PROCESS is non-nil, call this function after updating.
When NAVIGATION is `never', block won't be TAB navigatable.
When NAVIGATION is `auto', block is navigatable if non-empty body.
When NAVIGATION is `always', block is always TAB navigatable.
When EXPANDED is non-nil, body will be expanded by default.

For existing blocks, the current expansion state is preserved unless overridden."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (namespace-id (map-elt model :namespace-id))
           (qualified-id (format "%s-%s" namespace-id (map-elt model :block-id)))
           (new-label-left (map-elt model :label-left))
           (new-label-right (map-elt model :label-right))
           (new-body (map-elt model :body))
           (block-start nil)
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'sui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when (or new-label-left new-label-right new-body)
        (when match
          (goto-char (prop-match-beginning match)))
        (if (and match (not create-new))
            ;; Found existing block - delete and regenerate
            (let* ((existing-model (sui--read-dialog-block-at-point))
                   (state (get-text-property (point) 'sui-state))
                   (existing-body (map-elt existing-model :body))
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
              (setq block-start (prop-match-beginning match))

              ;; Safely replace existing block using narrow-to-region
              (save-excursion
                (save-restriction
                  (narrow-to-region block-start block-end)
                  (delete-region (point-min) (point-max))
                  (goto-char (point-min))
                  (sui--insert-dialog-block final-model qualified-id
                                            (not (map-elt state :collapsed))
                                            navigation))))

          ;; Not found or create-new - insert new block
          (goto-char (point-max))
          (insert (sui--required-newlines 2))
          (setq block-start (point))
          (sui--insert-dialog-block model qualified-id expanded navigation)
          (insert "\n\n")))
      (when on-post-process
        (funcall on-post-process))
      (when-let ((block-range (sui--block-range :position block-start)))
        (list (cons :block block-range)
              (cons :body (sui--nearest-range-matching-property
                           :property 'sui-section :value 'body
                           :from (map-elt block-range :start)
                           :to (map-elt block-range :end))))))))


(defun sui--read-dialog-block-at (position qualified-id)
  "Read dialog block at POSITION with QUALIFIED-ID."
  (when-let ((dialog (list (cons :block-id qualified-id)))
             (state (get-text-property position 'sui-state))
             (range (sui--block-range :position position)))
    ;; TODO: Get rid of merging block namespace and id.
    ;; Extract namespace-id from qualified-id if it contains a dash
    (when (string-match "^\\(.+\\)-\\(.+\\)$" qualified-id)
      (setf (map-elt dialog :namespace-id) (match-string 1 qualified-id))
      (setf (map-elt dialog :block-id) (match-string 2 qualified-id)))
    (save-mark-and-excursion
      (save-restriction
        (narrow-to-region (map-elt range :start)
                          (map-elt range :end))
        (goto-char (map-elt range :start))
        (setf (map-elt dialog :collapsed) (map-elt state :collapsed))
        (when-let ((label-left (sui--nearest-range-matching-property
                                :property 'sui-section :value 'label-left)))
          (setf (map-elt dialog :label-left) (buffer-substring (map-elt label-left :start)
                                                               (map-elt label-left :end))))
        (when-let ((label-right (sui--nearest-range-matching-property
                                 :property 'sui-section :value 'label-right)))
          (setf (map-elt dialog :label-right) (buffer-substring (map-elt label-right :start)
                                                                (map-elt label-right :end))))
        (if-let ((body (map-elt state :body)))
            (setf (map-elt dialog :body) body)
          (when-let ((body (sui--nearest-range-matching-property
                            :property 'sui-section :value 'body)))
            (setf (map-elt dialog :body) (buffer-substring (map-elt body :start)
                                                           (map-elt body :end)))))))
    dialog))

(cl-defun sui-delete-dialog-block (&key namespace-id block-id)
  "Delete dialog block with NAMESPACE-ID and BLOCK-ID."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (qualified-id (format "%s-%s" namespace-id block-id))
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'sui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when match
        (let ((block-start (prop-match-beginning match))
              (block-end (prop-match-end match)))
          ;; Remove vertical space that's part of the block.
          (goto-char block-end)
          (skip-chars-forward " \t\n")
          (setq block-end (point))
          (delete-region block-start block-end))))))

(defun sui--read-dialog-block-at-point ()
  "Read dialog block at point, returning model or nil if none found."
  (when-let ((state (get-text-property (point) 'sui-state))
             (range (sui--block-range :position (point))))
    (sui--read-dialog-block-at (map-elt range :start)
                               (map-elt state :qualified-id))))

(cl-defun sui--block-range (&key position)
  "Get block range at POSITION if found.  Nil otherwise.

In the form:

  ((start . 1)
   (end . 3))."
  (when-let ((qualified-id (map-elt (get-text-property (or position (point)) 'sui-state) :qualified-id)))
    (sui--nearest-range-matching-property
     :property 'sui-state
     :value qualified-id
     :predicate (lambda (qualified-id property)
                  (equal (map-elt property :qualified-id) qualified-id)))))

(cl-defun sui--nearest-range-matching-property (&key property value (predicate t) from to)
  "Return nearest range where PREDICATE is non-nil for PROPERTY and VALUE."
  (save-mark-and-excursion
    (save-restriction
      (when (and from to)
        (narrow-to-region from to))
      (let ((backward-match (or (text-property-search-backward property value predicate)
                                (progn
                                  (unless (eobp)
                                    (forward-char 1))
                                  (text-property-search-backward property value predicate))))
            (forward-match (text-property-search-forward property value predicate)))
        (when (or backward-match forward-match)
          `((:start . ,(if backward-match
                           (prop-match-beginning backward-match)
                         (prop-match-beginning forward-match)))
            (:end . ,(if forward-match
                         (prop-match-end forward-match)
                       (prop-match-end backward-match)))))))))

(defun sui--insert-dialog-block (model qualified-id &optional expanded navigation)
  "Insert dialog block from MODEL with QUALIFIED-ID text properties.
EXPANDED determines initial state (default nil for collapsed).
NAVIGATION controls navigability:

 `never' (not navigatable)
 `auto' (navigatable if body and indicator present)
 `always' (always navigatable)."
  (let ((block-start (point))
        (label-left (map-elt model :label-left))
        (label-right (map-elt model :label-right))
        (body (map-elt model :body))
        (need-space nil)
        (indicator-start)
        (indicator-end)
        (label-left-start)
        (label-left-end)
        (label-right-start)
        (label-right-end)
        (body-start)
        (body-end)
        (collapsable))

    ;; Insert collapse indicator if body exists
    (when-let ((has-labels (or label-left label-right)))
      (if body
          (progn
            (setq collapsable has-labels)
            (setq indicator-start (point))
            (insert (sui-add-action-to-text
                     (if expanded "▼ " "▶ ")
                     (lambda ()
                       (interactive)
                       (sui-toggle-dialog-block-at-point))
                     (lambda ()
                       (message "Press RET to toggle"))))
            (setq indicator-end (point))
            (add-text-properties indicator-start indicator-end
                                 `(sui-section indicator
                                               keymap ,(sui-make-action-keymap
                                                        (lambda ()
                                                          (interactive)
                                                          (sui-toggle-dialog-block-at-point)))
                                               read-only t
                                               front-sticky (read-only))))
        ;; Reserving the space for expand indicators enables
        ;; aligning columns but also avoids text jumping when
        ;; body arrives later on.
        (setq collapsable nil)
        (setq indicator-start (point))
        (insert "   ")
        (setq indicator-end (point))))

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
      (add-text-properties label-left-start label-left-end
                           `(sui-section label-left
                                         help-echo ,qualified-id
                                         read-only t
                                         front-sticky (read-only)))
      (setq need-space t))

    (when label-right
      (when need-space
        (insert " "))
      (setq label-right-start (point))
      (insert (sui-add-action-to-text
               label-right
               (lambda ()
                 (interactive)
                 (sui-toggle-dialog-block-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-right-end (point))
      (add-text-properties label-right-start label-right-end
                           `(sui-section label-right
                                         help-echo ,qualified-id
                                         read-only t
                                         front-sticky (read-only))))

    (when body
      (when (or label-left label-right)
        (insert "\n\n"))
      ;; Never leave more than two trailing newlines.
      (when (string-suffix-p "\n\n" body)
        (setq body (concat (string-trim-right body) "\n\n")))
      (setq body-start (point))
      ;; Remove existing indentation and re-apply.
      (let ((clean-body (string-remove-prefix "  " body)))
        (insert (sui--indent-text clean-body "   ")))
      (setq body-end (point))
      (add-text-properties body-start body-end
                           `(sui-section body
                                         help-echo ,qualified-id
                                         read-only t
                                         front-sticky (read-only))))
    (when-let ((is-collapsable collapsable)
               (body-overlay (make-overlay (or label-right-end
                                               label-left-end) body-end)))
      (overlay-put body-overlay 'evaporate t)
      (overlay-put body-overlay 'sui-section 'body)
      (overlay-put body-overlay 'invisible (not expanded)))
    ;; Hide trailing whitespace (don't delete) in body.
    (when body
      (save-mark-and-excursion
        (goto-char body-end)
        (when (re-search-backward "[^ \t\n]" body-start t)
          (forward-char 1)
          (when (< (point) body-end)
            (let ((ws-overlay (make-overlay (point) body-end)))
              (overlay-put ws-overlay 'invisible t)
              (overlay-put ws-overlay 'evaporate t)
              (overlay-put ws-overlay 'sui-section 'trailing-whitespace))))))
    (put-text-property
     block-start (or body-end label-right-end label-left-end)
     'sui-state (list
                 (cons :body body)
                 (cons :qualified-id qualified-id)
                 (cons :collapsed (not expanded))
                 (cons :navigatable (cond
                                     ((eq navigation 'never) nil)
                                     ((eq navigation 'always) t)
                                     ((eq navigation 'auto)
                                      (and body indicator-start))
                                     (t
                                      ;; Default to auto
                                      (and body indicator-start))))))
    (put-text-property block-start (or body-end label-right-end label-left-end) 'read-only t)
    (put-text-property block-start (or body-end label-right-end label-left-end) 'front-sticky '(read-only))))

(defun sui--required-newlines (desired)
  "Return string of newlines needed to reach DESIRED before POSITION."
  (let ((context (save-mark-and-excursion
                   (let ((end (point)))
                     (forward-line (- (+ 1 desired)))
                     (buffer-substring (point) end)))))
    (with-temp-buffer
      (insert context)
      ;; When counting visible newlines before point,
      ;; we may encounter invisible text, which may
      ;; look like newlines but gives false negatives.
      ;; In those cases, delete any 'invisible text
      ;; and try counting.
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (while (not (bobp))
          (let* ((end (point))
                 (start (previous-single-property-change end 'invisible nil (point-min))))
            (if (get-text-property (1- end) 'invisible)
                (delete-region start end))
            (goto-char start))))
      (goto-char (point-max))
      (let ((pos (point)))
        (skip-chars-backward "\n")
        (make-string (max 0 (- desired (- pos (point)))) ?\n)))))

(defun sui-toggle-dialog-block-at-point ()
  "Toggle visibility of dialog block body at point."
  (interactive)
  (save-mark-and-excursion
    (when-let* ((inhibit-read-only t)
                (buffer-undo-list t)
                (state (get-text-property (point) 'sui-state))
                (block (sui--block-range :position (point)))
                (body (sui--nearest-range-matching-property
                       :property 'sui-section :value 'body
                       :from (map-elt block :start)
                       :to (map-elt block :end)))
                (indicator (sui--nearest-range-matching-property
                            :property 'sui-section :value 'indicator
                            :from (map-elt block :start)
                            :to (map-elt block :end)))
                (body-overlay (seq-first (overlays-in (map-elt body :start)
                                                      (map-elt body :end))))
                (overlay-found (equal (overlay-get body-overlay 'sui-section) 'body)))
      (when (equal (overlay-get body-overlay 'sui-section) 'body)
        (let ((indicator-properties (text-properties-at (map-elt indicator :start))))
          (overlay-put body-overlay 'invisible (not (map-elt state :collapsed)))
          (delete-region (map-elt indicator :start)
                         (map-elt indicator :end))
          (goto-char (map-elt indicator :start))
          (insert (if (map-elt state :collapsed)
                      "▼ "
                    "▶ "))
          (add-text-properties (map-elt indicator :start)
                               (map-elt indicator :end)
                               indicator-properties)
          (map-put! state :collapsed (not (map-elt state :collapsed))))
        (put-text-property (map-elt block :start)
                           (map-elt block :end) 'sui-state state)))))

(defun sui-collapse-dialog-block-by-id (namespace-id block-id)
  "Collapse dialog block with NAMESPACE-ID and BLOCK-ID."
  (save-mark-and-excursion
    (let ((qualified-id (format "%s-%s" namespace-id block-id)))
      (goto-char (point-max))
      (when (text-property-search-backward
             'sui-state qualified-id
             (lambda (_ state)
               (equal (map-elt state :qualified-id) qualified-id))
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
  (when-let* ((start-point (point))
              (found (save-mark-and-excursion
                       ;; In navigatable block already
                       ;; move past it.
                       (when-let ((state (get-text-property (point) 'sui-state))
                                  (block (sui--block-range :position (point))))
                         (goto-char (map-elt block :end)))
                       (when-let ((next (text-property-search-forward
                                         'sui-state nil
                                         (lambda (_old-val new-val)
                                           (and new-val (map-elt new-val :navigatable)))
                                         t)))
                         (prop-match-beginning next)))))
    (when found
      (deactivate-mark)
      (goto-char found)
      found)))

(defun sui-backward-block ()
  "Jump to the previous block."
  (interactive)
  (when-let* ((start-point (point))
              (found (save-mark-and-excursion
                       ;; In navigatable block already
                       ;; move to beginning.
                       (when-let ((state (get-text-property (point) 'sui-state))
                                  (block (sui--block-range :position (point))))
                         (goto-char (map-elt block :start)))
                       (when-let ((prev (text-property-search-backward
                                         'sui-state nil
                                         (lambda (_old-val new-val)
                                           (and new-val (map-elt new-val :navigatable)))
                                         t)))
                         (prop-match-beginning prev)))))
    (when found
      (deactivate-mark)
      (goto-char found)
      found)))

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
