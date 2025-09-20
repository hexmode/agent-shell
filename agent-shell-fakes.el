;;; agent-shell-fakes.el --- A fake agent shell -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

(defun agent-shell-fakes-load-session ()
  "Load and replay a traffic session from file."
  (interactive)
  (let* ((messages (acp-fakes-read-traffic-file))
         (buffer (agent-shell-fakes-start-agent messages))
         (first-prompt (progn
                         (unless buffer
                           (error "No shell buffer available"))
                         (with-current-buffer buffer
                           (seq-find (lambda (item)
                                       (and (eq (map-elt item :direction) 'outgoing)
                                            (equal (map-nested-elt item '(:object method)) "session/prompt")
                                            (let ((text (map-nested-elt item '(:object params prompt 0 text))))
                                              (and text (not (string-empty-p text))))))
                                     (map-nested-elt agent-shell--state '(:client :message-queue))))))
         (first-prompt-text (map-nested-elt first-prompt '(:object params prompt 0 text))))
    (unless first-prompt-text
      (error "No first prompt text available to kick replay off"))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (insert first-prompt-text)
        (call-interactively #'shell-maker-submit)))))

(defun agent-shell-fakes-start-agent (messages)
  "Start a fake agent with traffic MESSAGES."
  (let* ((authenticate-message (acp-fakes--get-authenticate-request :messages messages))
         (authenticate-request (when authenticate-message
                                 (list (cons :method (map-nested-elt authenticate-message '(:object method)))
                                       (cons :params (map-nested-elt authenticate-message '(:object params))))))
         (buffer (agent-shell--start
                  :new-session t
                  :mode-line-name "Fake"
                  :buffer-name "Fake"
                  :shell-prompt "Fake> "
                  :shell-prompt-regexp "Fake> "
                  :icon-name "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fi.redd.it%2Fy6ci2659qvk61.jpg"
                  :client-maker (lambda ()
                                  (acp-fakes-make-client messages))
                  :needs-authentication authenticate-request
                  :authenticate-request-maker (lambda ()
                                                authenticate-request))))
    (with-current-buffer buffer
      (map-put! agent-shell--state
                :client (acp-fakes-make-client messages)))
    buffer))

(provide 'agent-shell-fakes)

;;; agent-shell-fakes.el ends here
