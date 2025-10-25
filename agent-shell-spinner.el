;;; agent-shell-spinner.el --- Spinner animation utilities -*- lexical-binding: t; -*-

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
;; Provides spinner animation functionality for agent-shell.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)

(cl-defun agent-shell-spinner-make (&key start (frames-per-second 10) on-frame-update)
  "Create a spinner animation timer.

START when non-nil, starts internal timer immediately.
FRAMES-PER-SECOND determines the timer frequency.
ON-FRAME-UPDATE is a callback function invoked on each timer tick.

Of the form:

  (lambda (frame status)
    ...
    ;; return next frame or nil if no changes to current frame
   )

STATUS is one of: `started', `busy', or `ended'.
FRAME is nil when status is `started' or `ended', otherwise current frame.

Returns an alist of the form:

  ((:spinner-timer)
   (:frames-per-second)
   (:on-frame-update)
   (:frame)
   (:status))"
  (unless on-frame-update
    (error "Missing required argument: :on-frame-update"))
  (let* ((spinner (list (cons :spinner-timer nil)
                        (cons :frames-per-second frames-per-second)
                        (cons :on-frame-update on-frame-update)
                        (cons :frame nil)
                        (cons :status nil))))
    (when start
      (agent-shell-spinner-start :spinner spinner))
    spinner))

(cl-defun agent-shell-spinner-start (&key spinner)
  "Start a spinner timer if not already started.

SPINNER is the spinner state alist.

Returns the spinner alist with the timer started."
  (unless spinner
    (error "Missing required argument: :spinner"))
  (unless (and (map-elt spinner :spinner-timer)
               (timerp (map-elt spinner :spinner-timer)))
    (map-put! spinner :status 'started)
    (map-put! spinner :frame nil)
    (map-put! spinner :spinner-timer
              (run-at-time 0 (/ 1.0 (map-elt spinner :frames-per-second))
                           (lambda ()
                             (when-let ((next-frame (funcall (map-elt spinner :on-frame-update)
                                                             (map-elt spinner :frame)
                                                             (map-elt spinner :status))))
                               (map-put! spinner :frame next-frame)
                               (map-put! spinner :status 'busy))))))
  spinner)

(cl-defun agent-shell-spinner-stop (&key spinner)
  "Stop a spinner timer.

SPINNER is the spinner state alist.

Returns the spinner with latest state."
  (unless spinner
    (error "Missing required argument: :spinner"))
  (when-let ((timer (map-elt spinner :spinner-timer)))
    (when (timerp timer)
      (cancel-timer timer))
    (map-put! spinner :spinner-timer nil))
  (map-put! spinner :status 'ended)
  (map-put! spinner :frame nil)
  (when-let ((callback (map-elt spinner :on-frame-update)))
    (funcall callback nil 'ended))
  spinner)

(provide 'agent-shell-spinner)

;;; agent-shell-spinner.el ends here
