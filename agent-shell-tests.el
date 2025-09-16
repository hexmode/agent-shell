;;; agent-shell-test.el --- agent-shell tests. -*- lexical-binding: t; -*-

(defun agent-shell--prompt-for-permission--test-display ()
  (interactive)
  (agent-shell--prompt-for-permission
   :model (agent-shell--make-prompt-for-permission-model
           :options '[((kind . "allow_once")
                       (name . "Approve")
                       (optionId . "opt-approve"))
                      ((kind . "reject_once")
                       (name . "Reject")
                       (optionId . "opt-reject"))
                      ((kind . "allow_always")
                       (name . "Always Approve")
                       (optionId . "opt-always"))]
           :tool-call '((:title . "The agent wants to run: git log --oneline -n 10")))
   :on-choice
   (lambda (option-id)
     (message "Selected: %s" option-id))))
