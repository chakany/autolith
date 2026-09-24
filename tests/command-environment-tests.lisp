(in-package #:autolith)

;;;; -- Command Environment Tests --

(-> test-command-environment-credentials () null)
(defun test-command-environment-credentials ()
  "Test which variables count as credentials a command must not see."
  (dolist (case '(("ANTHROPIC_API_KEY=k" t)
                  ("GITLAB_TOKEN=t" t)
                  ("github_token=t" t)
                  ("AWS_SECRET_ACCESS_KEY=s" t)
                  ("DATABASE_PASSWORD=p" t)
                  ("SSH_AUTH_SOCK=/tmp/agent" t)
                  ("AUTOLITH_GEMINI_OAUTH_CLIENT_SECRET=s" t)
                  ("PATH=/usr/bin" nil)
                  ("HOME=/home/user" nil)
                  ("TOKENIZER=sentencepiece" nil)
                  ("NIX_PATH=nixpkgs" nil)))
    (test-assert (eq (command-environment-credential-p (first case)) (second case))
                 (format nil "credential detection for ~A" (first case))))
  nil)

(-> test-command-environment-overrides () null)
(defun test-command-environment-overrides ()
  "Test command environments drop credentials and apply overrides last."
  (let ((environment (command-environment
                      :base '("PATH=/usr/bin" "HOME=/real" "OPENAI_API_KEY=k"
                              "GITLAB_TOKEN=t" "LANG=C.UTF-8")
                      :overrides '("HOME=/private"))))
    (test-assert (equal environment '("PATH=/usr/bin" "LANG=C.UTF-8" "HOME=/private"))
                 "credentials are dropped and overrides replace base values"))
  (with-test-environment (("AUTOLITH_TEST_API_KEY" "k")
                          ("AUTOLITH_TEST_VISIBLE" "v"))
    (let ((environment (command-environment)))
      (test-assert (member "AUTOLITH_TEST_VISIBLE=v" environment :test #'string=)
                   "the agent's ordinary variables reach commands")
      (test-assert (notany (lambda (binding)
                             (uiop:string-prefix-p "AUTOLITH_TEST_API_KEY=" binding))
                           environment)
                   "the agent's credentials never reach commands")))
  nil)

(-> test-shell-command-credential-environment () null)
(defun test-shell-command-credential-environment ()
  "Test a shell command cannot print the agent's credential variables."
  (let ((directory (platform-make-temporary-directory
                    *platform* (uiop:temporary-directory) "autolith-shell-test-")))
    (unwind-protect
         (with-test-environment (("AUTOLITH_TEST_API_KEY" "leaked-key")
                                 ("AUTOLITH_TEST_VISIBLE" "visible-value"))
           (let ((output (tool-result-content
                          (workspace-tool-run-shell-command
                           (test-fixture-shell-command
                            *platform* "env"
                            "Get-ChildItem Env: | ForEach-Object { $_.Value }")
                           directory (external-sandbox-policy) 60 100000))))
             (test-assert (search "visible-value" output)
                          "a shell command sees the agent's ordinary variables")
             (test-assert (not (search "leaked-key" output))
                          "a shell command never sees the agent's credentials")))
      (platform-delete-directory-tree *platform* directory
                                      :validate t :if-does-not-exist ':ignore)))
  nil)
