;;; agent-shell-anthropic-tests.el --- Tests for agent-shell-anthropic -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-anthropic)

(ert-deftest agent-shell-anthropic-make-claude-client-test ()
  "Test agent-shell-anthropic-make-claude-client function."
  ;; Mock executable-find to always return the command path
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/claude-code-acp")))
    ;; Test with API key authentication
    (let* ((agent-shell-anthropic-authentication '(:api-key "test-api-key"))
           (agent-shell-anthropic-claude-command '("claude-code-acp" "--json"))
           (agent-shell-anthropic-claude-environment '("DEBUG=1"))
           (client (agent-shell-anthropic-make-claude-client)))
      (should (listp client))
      (should (equal (map-elt client :command) "claude-code-acp"))
      (should (equal (map-elt client :command-params) '("--json")))
      (should (member "ANTHROPIC_API_KEY=test-api-key" (map-elt client :environment-variables)))
      (should (member "DEBUG=1" (map-elt client :environment-variables))))

    ;; Test with login authentication
    (let* ((agent-shell-anthropic-authentication '(:login t))
           (agent-shell-anthropic-claude-command '("claude-code-acp" "--interactive"))
           (agent-shell-anthropic-claude-environment '("VERBOSE=true"))
           (client (agent-shell-anthropic-make-claude-client)))
      ;; Verify environment variables include empty API key for login
      (should (member "ANTHROPIC_API_KEY=" (map-elt client :environment-variables)))
      (should (member "VERBOSE=true" (map-elt client :environment-variables))))

    ;; Test with function-based API key
    (let* ((agent-shell-anthropic-authentication `(:api-key ,(lambda () "dynamic-key")))
           (agent-shell-anthropic-claude-command '("claude-code-acp"))
           (agent-shell-anthropic-claude-environment '())
           (client (agent-shell-anthropic-make-claude-client))
           (env-vars (map-elt client :environment-variables)))
      (should (member "ANTHROPIC_API_KEY=dynamic-key" env-vars)))

    ;; Test error on invalid authentication
    (let* ((agent-shell-anthropic-authentication '())
           (agent-shell-anthropic-claude-command '("claude-code-acp")))
      (should-error (agent-shell-anthropic-make-claude-client)
                    :type 'error))

    ;; Test with agent-shell-make-environment-variables and :inherit-env t
    (let* ((agent-shell-anthropic-authentication '(:api-key "test-key"))
           (agent-shell-anthropic-claude-command '("claude-code-acp"))
           (process-environment '("EXISTING_VAR=existing_value"))
           (agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables
                                                      "NEW_VAR" "new_value"
                                                      :inherit-env t))
           (client (agent-shell-anthropic-make-claude-client))
           (env-vars (map-elt client :environment-variables)))
      (should (member "ANTHROPIC_API_KEY=test-key" env-vars))
      (should (member "NEW_VAR=new_value" env-vars))
      (should (member "EXISTING_VAR=existing_value" env-vars)))))

(provide 'agent-shell-anthropic-tests)
;;; agent-shell-anthropic-tests.el ends here
