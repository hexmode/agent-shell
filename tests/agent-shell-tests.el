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
                               "MY_OTHER_VAR=another_value")))
    (should (equal (agent-shell-make-environment-variables
                    "NEW_VAR" "new_value"
                    :inherit-env t)
                   '("NEW_VAR=new_value"
                     "EXISTING_VAR=existing_value"
                     "MY_OTHER_VAR=another_value"))))

  ;; Test :load-env with single file
  (let ((env-file (let ((file (make-temp-file "test-env" nil ".env")))
                    (with-temp-file file
                      (insert "TEST_VAR=test_value\n")
                      (insert "# This is a comment\n")
                      (insert "ANOTHER_TEST=another_value\n")
                      (insert "\n")  ; empty line
                      (insert "THIRD_VAR=third_value\n"))
                    file)))
    (unwind-protect
        (should (equal (agent-shell-make-environment-variables
                        "MANUAL_VAR" "manual_value"
                        :load-env env-file)
                       '("MANUAL_VAR=manual_value"
                         "TEST_VAR=test_value"
                         "ANOTHER_TEST=another_value"
                         "THIRD_VAR=third_value")))
      (delete-file env-file)))

  ;; Test :load-env with multiple files
  (let ((env-file1 (let ((file (make-temp-file "test-env1" nil ".env")))
                     (with-temp-file file
                       (insert "FILE1_VAR=file1_value\n")
                       (insert "SHARED_VAR=from_file1\n"))
                     file))
        (env-file2 (let ((file (make-temp-file "test-env2" nil ".env")))
                     (with-temp-file file
                       (insert "FILE2_VAR=file2_value\n")
                       (insert "SHARED_VAR=from_file2\n"))
                     file)))
    (unwind-protect
        (should (equal (agent-shell-make-environment-variables
                        :load-env (list env-file1 env-file2))
                       '("FILE1_VAR=file1_value"
                         "SHARED_VAR=from_file1"
                         "FILE2_VAR=file2_value"
                         "SHARED_VAR=from_file2")))
      (delete-file env-file1)
      (delete-file env-file2)))

  ;; Test :load-env with non-existent file (should error)
  (should-error (agent-shell-make-environment-variables
                 "TEST_VAR" "test_value"
                 :load-env "/non/existent/file")
                :type 'error)

  ;; Test :load-env combined with :inherit-env
  (let ((env-file (let ((file (make-temp-file "test-env" nil ".env")))
                    (with-temp-file file
                      (insert "ENV_FILE_VAR=env_file_value\n"))
                    file))
        (process-environment '("EXISTING_VAR=existing_value")))
    (unwind-protect
        (should (equal (agent-shell-make-environment-variables
                        "MANUAL_VAR" "manual_value"
                        :load-env env-file
                        :inherit-env t)
                       '("MANUAL_VAR=manual_value"
                         "ENV_FILE_VAR=env_file_value"
                         "EXISTING_VAR=existing_value")))
      (delete-file env-file))))

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

(ert-deftest agent-shell--shorten-paths-test ()
  "Test `agent-shell--shorten-paths' function."
  ;; Mock agent-shell-cwd to return a predictable value
  (cl-letf (((symbol-function 'agent-shell-cwd)
             (lambda () "/path/to/agent-shell/")))

    ;; Test shortening full paths to project-relative format
    (should (equal (agent-shell--shorten-paths
                    "/path/to/agent-shell/README.org")
                   "README.org"))

    ;; Test with subdirectories
    (should (equal (agent-shell--shorten-paths
                    "/path/to/agent-shell/tests/agent-shell-tests.el")
                   "tests/agent-shell-tests.el"))

    ;; Test mixed text with project path
    (should (equal (agent-shell--shorten-paths
                    "Read /path/to/agent-shell/agent-shell.el (4 - 6)")
                   "Read agent-shell.el (4 - 6)"))

    ;; Test text that doesn't contain project path (should remain unchanged)
    (should (equal (agent-shell--shorten-paths
                    "Some random text without paths")
                   "Some random text without paths"))

    ;; Test text with different paths (should remain unchanged)
    (should (equal (agent-shell--shorten-paths
                    "/some/other/path/file.txt")
                   "/some/other/path/file.txt"))

    ;; Test nil input
    (should (equal (agent-shell--shorten-paths nil) nil))

    ;; Test empty string
    (should (equal (agent-shell--shorten-paths "") ""))))

(ert-deftest agent-shell--format-plan-test ()
  "Test `agent-shell--format-plan' function."
  ;; Test homogeneous statuses
  (should (equal (agent-shell--format-plan [((content . "Update state initialization")
                                             (status . "pending"))
                                            ((content . "Update session initialization")
                                             (status . "pending"))])
                 (substring-no-properties
                  " pending  Update state initialization
 pending  Update session initialization")))

  ;; Test mixed statuses
  (should (equal (substring-no-properties
                  (agent-shell--format-plan [((content . "First task")
                                              (status . "pending"))
                                             ((content . "Second task")
                                              (status . "in_progress"))
                                             ((content . "Third task")
                                              (status . "completed"))]))
                 " pending     First task
 in progress  Second task
 completed   Third task"))

  ;; Test empty entries
  (should (equal (agent-shell--format-plan []) "")))

(ert-deftest agent-shell--parse-file-mentions-test ()
  "Test agent-shell--parse-file-mentions function."
  ;; Simple @ mention
  (let ((mentions (agent-shell--parse-file-mentions "@file.txt")))
    (should (= (length mentions) 1))
    (should (equal (map-elt (car mentions) :path) "file.txt")))

  ;; @ mention with quotes
  (let ((mentions (agent-shell--parse-file-mentions "Compare @\"file with spaces.txt\" to @other.txt")))
    (should (= (length mentions) 2))
    (should (equal (map-elt (car mentions) :path) "file with spaces.txt"))
    (should (equal (map-elt (cadr mentions) :path) "other.txt")))

  ;; @ mention at start of line
  (let ((mentions (agent-shell--parse-file-mentions "@README.md is the main file")))
    (should (= (length mentions) 1))
    (should (equal (map-elt (car mentions) :path) "README.md")))

  ;; Multiple @ mentions
  (let ((mentions (agent-shell--parse-file-mentions "Compare @file1.txt with @file2.txt")))
    (should (= (length mentions) 2))
    (should (equal (map-elt (car mentions) :path) "file1.txt"))
    (should (equal (map-elt (cadr mentions) :path) "file2.txt")))

  ;; No @ mentions
  (let ((mentions (agent-shell--parse-file-mentions "No mentions here")))
    (should (= (length mentions) 0))))

(ert-deftest agent-shell--build-content-blocks-test ()
  "Test agent-shell--build-content-blocks function."
  (let* ((temp-file (make-temp-file "agent-shell-test" nil ".txt"))
         (file-content "Test file content")
         (default-directory (file-name-directory temp-file))
         (file-name (file-name-nondirectory temp-file)))

    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert file-content))

          ;; Mock agent-shell-cwd and agent-shell--state
          (cl-letf (((symbol-function 'agent-shell-cwd)
                     (lambda () default-directory)))

            ;; Test with embedded context support and small file
            (let ((agent-shell--state (list
                                       (cons :agent-supports-embedded-context t))))
              (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                (should (> (length blocks) 0))
                ;; Should have a resource block
                (let ((resource-block (seq-find
                                       (lambda (block)
                                         (equal (map-elt block 'type) "resource"))
                                       blocks)))
                  (should resource-block)
                  (should (equal (map-nested-elt resource-block '(resource text))
                                 file-content)))))

            ;; Test without embedded context support
            (let ((agent-shell--state (list
                                       (cons :agent-supports-embedded-context nil))))
              (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                (should (> (length blocks) 0))
                ;; Should have a resource_link block, not resource
                (should-not (seq-find
                             (lambda (block)
                               (equal (map-elt block 'type) "resource"))
                             blocks))
                (should (seq-find
                         (lambda (block)
                           (equal (map-elt block 'type) "resource_link"))
                         blocks))))

            ;; Test with large file (exceeds size limit)
            (let ((agent-shell--state (list
                                       (cons :agent-supports-embedded-context t)))
                  (agent-shell-embed-file-size-limit 5))  ; Very small limit
              (let ((blocks (agent-shell--build-content-blocks (format "Analyze @%s" file-name))))
                ;; Should use resource_link for large file
                (should (seq-find
                         (lambda (block)
                           (equal (map-elt block 'type) "resource_link"))
                         blocks))))

            ;; Test with no mentions
            (let ((agent-shell--state (list
                                       (cons :agent-supports-embedded-context t))))
              (let ((blocks (agent-shell--build-content-blocks "No mentions here")))
                (should (= (length blocks) 1))
                (should (equal (map-elt (car blocks) 'type) "text"))
                (should (equal (map-elt (car blocks) 'text) "No mentions here"))))))

      (delete-file temp-file))))

(ert-deftest agent-shell--collect-attached-files-test ()
  "Test agent-shell--collect-attached-files function."
  ;; Test with empty list
  (should (equal (agent-shell--collect-attached-files '()) '()))

  ;; Test with resource block
  (let ((blocks '(((type . "resource")
                   (resource . ((uri . "file:///path/to/file.txt")
                                (text . "content"))))
                  ((type . "text")
                   (text . "some text")))))
    (let ((uris (agent-shell--collect-attached-files blocks)))
      (should (= (length uris) 1))
      (should (equal (car uris) "file:///path/to/file.txt"))))

  ;; Test with resource_link block
  (let ((blocks '(((type . "resource_link")
                   (uri . "file:///path/to/file.txt")
                   (name . "file.txt"))
                  ((type . "text")
                   (text . "some text")))))
    (let ((uris (agent-shell--collect-attached-files blocks)))
      (should (= (length uris) 1))
      (should (equal (car uris) "file:///path/to/file.txt"))))

  ;; Test with multiple files
  (let ((blocks '(((type . "resource_link")
                   (uri . "file:///path/to/file1.txt"))
                  ((type . "text")
                   (text . " "))
                  ((type . "resource_link")
                   (uri . "file:///path/to/file2.txt")))))
    (let ((uris (agent-shell--collect-attached-files blocks)))
      (should (= (length uris) 2)))))

(ert-deftest agent-shell--send-command-integration-test ()
  "Integration test: verify agent-shell--send-command calls ACP correctly."
  (let ((sent-request nil)
        (agent-shell--state (list
                            (cons :client 'test-client)
                            (cons :session-id "test-session")
                            (cons :agent-supports-embedded-context t)
                            (cons :buffer (current-buffer)))))

    ;; Mock acp-send-request to capture what gets sent
    (cl-letf (((symbol-function 'acp-send-request)
               (lambda (&rest args)
                 (setq sent-request args))))

      ;; Send a simple command
      (agent-shell--send-command
       :prompt "Hello agent"
       :shell nil)

      ;; Verify request was sent
      (should sent-request)

      ;; Verify basic request structure
      (let* ((request (plist-get sent-request :request))
             (params (map-elt request 'params))
             (prompt (map-elt params 'prompt)))
        ;; Should have session-id
        (should (equal (map-elt params 'sessionId) "test-session"))
        ;; Should have prompt as a vector
        (should (vectorp prompt))
        ;; Callbacks should be functions
        (should (functionp (plist-get sent-request :on-success)))
        (should (functionp (plist-get sent-request :on-failure)))))))

(provide 'agent-shell-tests)
;;; agent-shell-tests.el ends here
