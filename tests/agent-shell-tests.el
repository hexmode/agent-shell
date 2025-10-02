;;; agent-shell-tests.el --- Tests for agent-shell -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-make-environment-variables-test ()
  "Test `agent-shell-make-environment-variables' function."
  ;; Test basic key-value pairs
  (should (equal (agent-shell-make-environment-variables
                  "PATH" "/usr/bin"
                  "HOME" "/home/user")
                 '("PATH=/usr/bin"
                   "HOME=/home/user")))

  ;; Test empty input
  (should (equal (agent-shell-make-environment-variables) '()))

  ;; Test single pair
  (should (equal (agent-shell-make-environment-variables "FOO" "bar")
                 '("FOO=bar")))

  ;; Test with keywords (should be filtered out)
  (should (equal (agent-shell-make-environment-variables
                  "VAR1" "value1"
                  :inherit-env nil
                  "VAR2" "value2")
                 '("VAR1=value1"
                   "VAR2=value2")))

  ;; Test error on incomplete pairs
  (should-error (agent-shell-make-environment-variables "PATH")
                :type 'error)

  ;; Test :inherit-env t
  (let ((process-environment '("EXISTING_VAR=existing_value"
                               "ANOTHER_VAR=another_value")))
    (should (equal (agent-shell-make-environment-variables
                    "NEW_VAR" "new_value"
                    :inherit-env t)
                   '("NEW_VAR=new_value"
                     "EXISTING_VAR=existing_value"
                     "ANOTHER_VAR=another_value")))))

(ert-deftest agent-shell--resolve-devcontainer-path-test ()
  "Test `agent-shell--resolve-devcontainer-path' function."
  ;; Mock agent-shell--get-devcontainer-workspace-path
  (cl-letf (((symbol-function 'agent-shell--get-devcontainer-workspace-path)
             (lambda (_) "/workspace")))

    ;; Need to run in an existing directory (requirement of `file-in-directory-p')
    (let ((default-directory "/tmp"))
      ;; With text file capabilities enabled
      (let ((agent-shell-text-file-capabilities t))

        ;; Resolves container paths to local filesystem paths
        (should (equal (agent-shell--resolve-devcontainer-path "/workspace/d/f.el") "/tmp/d/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/workspace/f.el") "/tmp/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/workspace") "/tmp"))

        ;; Prevents attempts to leave local working directory
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/..") :type 'error)

        ;; Resolves local filesystem paths to container paths
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/d/f.el") "/workspace/d/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/f.el") "/workspace/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp") "/workspace"))

        ;; Does not resolve unexpected paths
        (should-error (agent-shell--resolve-devcontainer-path "/unexpected") :type 'error))

      ;; With text file capabilities disabled (ie. never resolve to local filesystem)
      (let ((agent-shell-text-file-capabilities nil))

        ;; Does not resolve container paths to local filesystem paths
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/d/f.el") :type 'error)
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/f.el.") :type 'error)
        (should-error (agent-shell--resolve-devcontainer-path "/workspace") :type 'error)
        (should-error (agent-shell--resolve-devcontainer-path "/workspace/..") :type 'error)

        ;; Resolves local filesystem paths to container paths
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/d/f.el") "/workspace/d/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp/f.el") "/workspace/f.el"))
        (should (equal (agent-shell--resolve-devcontainer-path "/tmp") "/workspace"))

        ;; Does not resolve unexpected paths
        (should-error (agent-shell--resolve-devcontainer-path "/unexpected") :type 'error)))))

(defun agent-shell--prompt-for-permission--test-display ()
  "Visually inspect and test minibuffer permission prompt."
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

(provide 'agent-shell-tests)
;;; agent-shell-tests.el ends here
