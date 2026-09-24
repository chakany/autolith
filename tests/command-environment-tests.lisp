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

(-> test-command-environment-private-home () null)
(defun test-command-environment-private-home ()
  "Test a private home carries the Git identity and nothing else."
  (let* ((root (platform-make-temporary-directory
                *platform* (uiop:temporary-directory) "autolith-home-test-"))
         (real-home (merge-pathnames "real/" root))
         (private-home (merge-pathnames "private/" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist private-home)
           (ensure-directories-exist (merge-pathnames ".ssh/" real-home))
           (with-open-file (stream (merge-pathnames ".gitconfig" real-home)
                                   :direction ':output)
             (format stream "[user]~%~Cname = \"Test \\\"Q\\\" User\"~%~
                             ~Cemail = test@example.com~%[credential]~%~Chelper = store~%"
                     #\Tab #\Tab #\Tab))
           (with-open-file (stream (merge-pathnames ".ssh/id_ed25519" real-home)
                                   :direction ':output)
             (write-line "secret" stream))
           (with-open-file (stream (merge-pathnames "empty-gitconfig" root)
                                   :direction ':output)
             (declare (ignore stream)))
           (let ((bindings
                   (with-test-environment
                       (("HOME" (string-right-trim "/" (uiop:native-namestring real-home)))
                        ("XDG_CONFIG_HOME" nil)
                        ("GIT_CONFIG_GLOBAL"
                         (uiop:native-namestring (merge-pathnames "empty-gitconfig" root))))
                     (command-environment-private-home private-home))))
             (test-assert
              (member (format nil "HOME=~A"
                              (string-right-trim "/" (uiop:native-namestring private-home)))
                      bindings :test #'string=)
              "the command's HOME is the private home")
             (test-assert
              (every (lambda (variable)
                       (let ((binding (find-if (lambda (binding)
                                                 (uiop:string-prefix-p
                                                  (format nil "~A=" variable) binding))
                                               bindings)))
                         (and binding
                              (uiop:directory-exists-p
                               (uiop:ensure-directory-pathname
                                (subseq binding (1+ (position #\= binding))))))))
                     '("XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_STATE_HOME"))
              "the XDG directories exist inside the private home")
             (test-assert (not (probe-file (merge-pathnames ".gitconfig" private-home)))
                          "no identity file is written without a Git identity"))
           (let ((bindings
                   (with-test-environment
                       (("HOME" (string-right-trim "/" (uiop:native-namestring real-home)))
                        ("XDG_CONFIG_HOME" nil)
                        ("GIT_CONFIG_GLOBAL"
                         (uiop:native-namestring (merge-pathnames ".gitconfig" real-home))))
                     (command-environment-private-home private-home))))
             (declare (ignore bindings))
             (let ((text (uiop:read-file-string (merge-pathnames ".gitconfig" private-home)))
                   (private-setting
                     (lambda (key)
                       (uiop:run-program
                        (list "git" "config" "--file"
                              (uiop:native-namestring
                               (merge-pathnames ".gitconfig" private-home))
                              "--get" key)
                        :output '(:string :stripped t)))))
               (test-assert (and (string= (funcall private-setting "user.name")
                                          "Test \"Q\" User")
                                 (string= (funcall private-setting "user.email")
                                          "test@example.com"))
                            "the private home carries the user's Git identity")
               (test-assert (not (search "credential" text))
                            "the private home carries no other Git settings")
               (test-assert (not (probe-file (merge-pathnames ".ssh/id_ed25519" private-home)))
                            "the private home carries no files from the real home"))))
      (platform-delete-directory-tree *platform* root
                                      :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-command-sandbox-private-home () null)
(defun test-command-sandbox-private-home ()
  "Test the POSIX command sandbox supplies a private home for one call only."
  (with-test-fixture (:posix-adapter "the POSIX command sandbox's private home")
    (test-command--check-sandbox-private-home))
  nil)

(-> test-command--check-sandbox-private-home () null)
(defun test-command--check-sandbox-private-home ()
  "Check one POSIX sandboxed call receives, and then loses, a private home."
  (let ((captured-home nil)
        (captured-environment nil)
        (workspace (platform-make-temporary-directory
                    *platform* (uiop:temporary-directory) "autolith-workspace-test-")))
    (unwind-protect
         (with-test-environment (("AUTOLITH_TEST_API_KEY" "k"))
           (platform-call-with-command-sandbox
            *platform* workspace
            (lambda (policy environment)
              (declare (ignore policy))
              (setf captured-environment environment
                    captured-home
                    (let ((binding (find-if (lambda (binding)
                                              (uiop:string-prefix-p "HOME=" binding))
                                            environment)))
                      (and binding (uiop:ensure-directory-pathname (subseq binding 5)))))
              (test-assert (and captured-home (uiop:directory-exists-p captured-home))
                           "the sandboxed command's home exists while it runs"))))
      (platform-delete-directory-tree *platform* workspace
                                      :validate t :if-does-not-exist ':ignore))
    (test-assert (and captured-home
                      (not (uiop:pathname-equal (uiop:ensure-directory-pathname
                                                 (user-homedir-pathname))
                                                captured-home)))
                 "the sandboxed command's home is not the user's home")
    (test-assert (not (uiop:directory-exists-p captured-home))
                 "the private home is removed after the command")
    (test-assert (notany (lambda (binding)
                           (uiop:string-prefix-p "AUTOLITH_TEST_API_KEY=" binding))
                         captured-environment)
                 "the sandboxed command's environment has no credentials"))
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
