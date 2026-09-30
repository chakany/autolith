(in-package #:autolith)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'agent-sandbox-wrap)
    (load (merge-pathnames "recovery/runtime.lisp"
                           (asdf:system-source-directory :autolith)))))


;;;; -- Agent Sandbox Tests --

(-> agent-sandbox-tests--write (pathname string) pathname)
(defun agent-sandbox-tests--write (pathname text)
  "Write TEXT to PATHNAME, creating its directories, and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
    (write-string text stream))
  pathname)

(-> agent-sandbox-tests--native (pathname) string)
(defun agent-sandbox-tests--native (pathname)
  "Return PATHNAME's native namestring without a trailing separator."
  (string-right-trim "/" (uiop:native-namestring pathname)))

(-> test-agent-sandbox-state () null)
(defun test-agent-sandbox-state ()
  "Test the sandbox state the launcher records, and that confined or disabled
launches pass their command through unchanged."
  (dolist (case '((nil :enabled) ("" :enabled) ("active" :active) ("off" :off)))
    (with-test-environment (("AUTOLITH_AGENT_SANDBOX" (first case)))
      (test-assert (eq (agent-sandbox-state) (second case))
                   (format nil "the sandbox state for ~S is ~S" (first case) (second case)))))
  (dolist (value '("active" "off"))
    (with-test-environment (("AUTOLITH_AGENT_SANDBOX" value))
      (test-assert (equal (agent-sandbox-wrap '("/bin/echo" "hi")
                                              :source-root #P"/nonexistent/source/"
                                              :workspace #P"/nonexistent/work/")
                          '("/bin/echo" "hi"))
                   (format nil "a launch with the sandbox ~A is not wrapped" value))))
  nil)

(-> test-agent-sandbox-refuses-home-workspace () null)
(defun test-agent-sandbox-refuses-home-workspace ()
  "Test the agent sandbox refuses a workspace containing the home directory."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((home (merge-pathnames "home/" root)))
      (ensure-directories-exist home)
      (with-test-environment (("HOME" (agent-sandbox-tests--native home))
                              ("AUTOLITH_AGENT_SANDBOX" nil))
        (dolist (workspace (list home root))
          (test-assert
           (handler-case
               (progn (agent-sandbox-policy :source-root root :workspace workspace)
                      nil)
             (agent-sandbox-unavailable (condition)
               (and (search "AUTOLITH_AGENT_SANDBOX=off" (princ-to-string condition)) t)))
           (format nil "a workspace at ~A is refused" workspace))))))
  nil)

(-> agent-sandbox-tests--socket-probe-command (pathname) string)
(defun agent-sandbox-tests--socket-probe-command (socket-pathname)
  "Return a disposable SBCL command connecting to SOCKET-PATHNAME."
  (format nil "~A --noinform --non-interactive --eval '(require :sb-bsd-sockets)' --eval ~A"
          (uiop:escape-sh-token (uiop:getenv "AUTOLITH_SBCL"))
          (uiop:escape-sh-token
           (format nil
                   "(let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))) (unwind-protect (sb-bsd-sockets:socket-connect socket ~S) (sb-bsd-sockets:socket-close socket)))"
                   (namestring socket-pathname)))))

(-> test-agent-sandbox-enforcement () null)
(defun test-agent-sandbox-enforcement ()
  "Test the agent sandbox hides the home directory, lets the agent write its
workspace and state, and keeps its images and source read-only."
  (unless (sandbox-supported-p)
    (test-withheld ':sandbox-backend "the agent sandbox")
    (return-from test-agent-sandbox-enforcement nil))
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((home (merge-pathnames "home/" root))
           (workspace (merge-pathnames "project/" home))
           (source-root (merge-pathnames "autolith/" home))
           (secret (agent-sandbox-tests--write (merge-pathnames ".ssh/id_ed25519" home)
                                               "secret"))
           (codex-auth (agent-sandbox-tests--write
                        (merge-pathnames ".codex/auth.json" home) "codex-secret"))
           (grok-auth (agent-sandbox-tests--write
                       (merge-pathnames ".grok/auth.json" home) "grok-secret"))
           (state-root (merge-pathnames ".local/state/autolith/" home))
           (data-root (merge-pathnames ".local/share/autolith/" home))
           (active (merge-pathnames ".local/share/autolith-launcher/active/" home))
           (worktrees (merge-pathnames ".local/share/autolith-launcher/recovery-worktrees/" home))
           (user-cache (merge-pathnames ".cache/common-lisp/" home))
           (trusted-cache (merge-pathnames
                           ".cache/autolith-launcher/trusted-source/" home))
           (agent-cache (merge-pathnames ".cache/autolith/" home))
           (agent-temporary
             (platform-make-temporary-directory
              *platform* #P"/tmp/" "autolith-agent.")))
      (unwind-protect
           (progn
      (agent-sandbox-tests--write (merge-pathnames "README" source-root) "source")
      (ensure-directories-exist workspace)
      (ensure-directories-exist state-root)
      (ensure-directories-exist data-root)
      (ensure-directories-exist active)
      (ensure-directories-exist worktrees)
      (ensure-directories-exist user-cache)
      (agent-sandbox-tests--write
       (merge-pathnames "trusted.fasl" trusted-cache) "trusted")
      (ensure-directories-exist agent-cache)
      (with-test-environment (("HOME" (agent-sandbox-tests--native home))
                              ("XDG_CONFIG_HOME" nil)
                              ("XDG_DATA_HOME" nil)
                              ("XDG_STATE_HOME" nil)
                              ("XDG_CACHE_HOME" nil)
                              ("AUTOLITH_AGENT_TMPDIR"
                               (agent-sandbox-tests--native agent-temporary))
                              ("AUTOLITH_AGENT_SANDBOX" nil))
        (test-assert
         (cl-exec-sandbox:sandbox-policy-isolate-processes-p
          (agent-sandbox-policy :source-root source-root
                                :workspace workspace))
         "agent policy isolates Linux processes from the broker")
        (flet ((allowed-p (script)
                 (zerop (nth-value
                         2 (uiop:run-program
                            (agent-sandbox-wrap (list "/bin/sh" "-c" script)
                                                :source-root source-root
                                                :workspace workspace)
                            :directory workspace
                            :output nil :error-output nil
                            :ignore-error-status t))))
               (output (script)
                 (uiop:run-program (agent-sandbox-wrap (list "/bin/sh" "-c" script)
                                                       :source-root source-root
                                                       :workspace workspace)
                                   :directory workspace
                                   :output ':string :error-output nil
                                   :ignore-error-status t))
               (quoted (pathname)
                 (uiop:escape-sh-token (agent-sandbox-tests--native pathname))))
          (test-assert (not (allowed-p (format nil "cat ~A" (quoted secret))))
                       "the agent cannot read the hidden home directory")
          (dolist (credential (list codex-auth grok-auth))
            (test-assert
             (not (allowed-p (format nil "cat ~A" (quoted credential))))
             "the agent cannot read a provider bootstrap login"))
          (test-assert (allowed-p (format nil "echo made > ~A"
                                          (quoted (merge-pathnames "made" workspace))))
                       "the agent writes its workspace")
          (test-assert (allowed-p (format nil "echo state > ~A"
                                          (quoted (merge-pathnames "state" state-root))))
                       "the agent writes its state root")
          (test-assert (allowed-p (format nil "echo data > ~A"
                                          (quoted (merge-pathnames "data" data-root))))
                       "the agent writes its data root")
          (test-assert (not (allowed-p (format nil "echo core > ~A"
                                               (quoted (merge-pathnames "core" active)))))
                       "the agent cannot replace the active image")
          (test-assert (allowed-p (format nil "cat ~A"
                                          (quoted (merge-pathnames "README" source-root))))
                       "the agent reads the source root")
          (test-assert (not (allowed-p (format nil "echo x > ~A"
                                               (quoted (merge-pathnames "README"
                                                                        source-root)))))
                       "the agent cannot modify a source root outside its workspace")
          (with-test-environment (("AUTOLITH_LAUNCHER_TERMINAL" "/dev/null"))
            (when (platform-agent-sandbox-terminal-rule
                   *platform* #P"/dev/null")
              (test-assert (not (allowed-p "cat /dev/null"))
                           "the agent cannot open the launcher's terminal device")))
          (test-assert
           (not (zerop (nth-value
                        2 (uiop:run-program
                           (agent-sandbox-wrap
                            (list "/bin/sh" "-c"
                                  (format nil "echo x > ~A"
                                          (quoted (merge-pathnames "README" source-root))))
                            :source-root source-root
                            :workspace source-root)
                           :directory source-root
                           :output nil :error-output nil
                           :ignore-error-status t))))
           "the agent cannot modify launcher source when it is the workspace")
        (with-test-environment
            (("AUTOLITH_TEST_SECRET" "fixture-host-secret")
             ("OPENAI_API_KEY" "fixture-provider-secret"))
          (test-assert
           (string= (output
                     "printf '%s|%s|%s' \"${AUTOLITH_TEST_SECRET:-}\" \"${OPENAI_API_KEY:-}\" \"${AUTOLITH_AGENT_SANDBOX:-}\"")
                    "||active")
           "the agent inherits its sandbox marker without host credentials"))
        (test-assert (not (allowed-p (format nil "echo x > ~A"
                                             (quoted (merge-pathnames "fasl" user-cache)))))
                     "the agent cannot write the user's ASDF cache")
        (test-assert
         (not (allowed-p
               (format nil "cat ~A"
                       (quoted (merge-pathnames "trusted.fasl"
                                                  trusted-cache)))))
         "the agent cannot read the broker's trusted compiler cache")
        (test-assert (not (allowed-p (format nil "echo x > ~A"
                                             (quoted (merge-pathnames "config" worktrees)))))
                     "the agent cannot write the checkouts recovery runs Git in")
        (dolist (relative '("installation/current" "nix/images/active" "helpers/helper"))
          (let ((target (merge-pathnames relative (merge-pathnames ".local/share/autolith-launcher/"
                                                                   home))))
            (ensure-directories-exist target)
            (test-assert (not (allowed-p (format nil "echo x > ~A"
                                                 (quoted (merge-pathnames "file" target)))))
                         (format nil "the agent cannot write the launcher's ~A" relative))))
        (let* ((broker-state-home
                 (platform-make-temporary-directory
                  *platform* #P"/tmp/" "autolith-state."))
               (broker-parent
                 (merge-pathnames
                  "autolith-launcher/broker/" broker-state-home)))
          (unwind-protect
               (progn
                 (ensure-directories-exist
                  (merge-pathnames "marker" broker-parent))
                 (with-test-environment
                     (("XDG_STATE_HOME"
                        (agent-sandbox-tests--native broker-state-home)))
                   (let* ((broker-root
                            (platform-make-temporary-directory
                             *platform* broker-parent "session."))
                          (broker-socket
                            (merge-pathnames "broker.sock" broker-root))
                          (listener
                            (make-instance 'sb-bsd-sockets:local-socket
                                           :type ':stream)))
                     (unwind-protect
                          (progn
                            (sb-bsd-sockets:socket-bind
                             listener (namestring broker-socket))
                            (sb-bsd-sockets:socket-listen listener 4)
                            (with-test-environment
                                (("AUTOLITH_BROKER_SOCKET"
                                   (agent-sandbox-tests--native
                                    broker-socket)))
                              (let ((command
                                      (agent-sandbox-tests--socket-probe-command
                                       broker-socket)))
                                (multiple-value-bind
                                      (result error-output status)
                                    (uiop:run-program
                                     (agent-sandbox-wrap
                                      (list "/bin/sh" "-c" command)
                                      :source-root source-root
                                      :workspace workspace)
                                     :directory workspace :output ':string
                                     :error-output ':string
                                     :ignore-error-status t)
                                  (test-assert
                                   (zerop status)
                                   (format nil
                                           "the agent connects to its exact broker socket: ~A ~A"
                                           result error-output))))
                              (test-assert
                               (not (allowed-p
                                     (format nil "echo replaced > ~A"
                                             (quoted broker-socket))))
                               "the agent cannot replace the broker socket")
                              (let* ((other-root
                                       (platform-make-temporary-directory
                                        *platform* broker-parent
                                        "session.other."))
                                     (other-socket
                                       (merge-pathnames
                                        "broker.sock" other-root))
                                     (other-listener
                                       (make-instance
                                        'sb-bsd-sockets:local-socket
                                        :type ':stream)))
                                (unwind-protect
                                     (progn
                                       (sb-bsd-sockets:socket-bind
                                        other-listener
                                        (namestring other-socket))
                                       (sb-bsd-sockets:socket-listen
                                        other-listener 4)
                                       (test-assert
                                        (not (allowed-p
                                              (agent-sandbox-tests--socket-probe-command
                                               other-socket)))
                                        "the agent cannot connect to another session's broker"))
                                  (ignore-errors
                                    (sb-bsd-sockets:socket-close
                                     other-listener))
                                  (platform-delete-directory-tree
                                   *platform* other-root :validate t
                                   :if-does-not-exist ':ignore)))))
                       (ignore-errors
                         (sb-bsd-sockets:socket-close listener))
                       (platform-delete-directory-tree
                        *platform* broker-root :validate t
                        :if-does-not-exist ':ignore)))))
            (platform-delete-directory-tree
             *platform* broker-state-home :validate t
             :if-does-not-exist ':ignore)))
        (let ((cache-home (output "printf %s \"$XDG_CACHE_HOME\"")))
          (test-assert (uiop:string-prefix-p
                        (agent-sandbox-tests--native
                         (cl-exec-sandbox::path--canonical agent-cache))
                        (agent-sandbox-tests--native
                         (cl-exec-sandbox::path--canonical
                          (uiop:ensure-directory-pathname cache-home))))
                       (format nil "the agent's caches live in Autolith's cache: ~A" cache-home))
          (test-assert (allowed-p (format nil "mkdir -p ~A/common-lisp && echo x > ~A/common-lisp/fasl"
                                          (uiop:escape-sh-token cache-home)
                                          (uiop:escape-sh-token cache-home)))
                       "the agent writes its ASDF cache"))
        (let ((nix-cache (merge-pathnames ".local/share/autolith-launcher/nix/asdf-cache/identity/" home)))
          (ensure-directories-exist nix-cache)
          (with-test-environment (("AUTOLITH_ASDF_CACHE" (agent-sandbox-tests--native nix-cache)))
            (let ((redirected (output "printf %s \"$AUTOLITH_ASDF_CACHE\"")))
              (test-assert (and (search "agent-sandbox" redirected)
                                (search "identity" redirected))
                           (format nil "a Nix installation's Autolith cache is the agent's own: ~A"
                                   redirected))
              (test-assert (not (allowed-p (format nil "echo x > ~A"
                                                   (quoted (merge-pathnames "fasl" nix-cache)))))
                           "the agent cannot write the cache the Nix image builder loads"))))))))
        (platform-delete-directory-tree *platform* agent-temporary
                                        :validate t
                                        :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-sandbox-host-temporary-secret () null)
(defun test-agent-sandbox-host-temporary-secret ()
  "Test that host temporary files outside Autolith are not visible to the agent."
  (unless (sandbox-supported-p)
    (test-withheld ':sandbox-backend "host temporary file isolation")
    (return-from test-agent-sandbox-host-temporary-secret nil))
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((home (merge-pathnames "home/" root))
           (workspace (merge-pathnames "project/" home))
           (source-root (merge-pathnames "source/" root))
           (host-temporary
             (platform-make-temporary-directory
              *platform* #P"/tmp/" "autolith-host-secret."))
           (agent-temporary
             (platform-make-temporary-directory
              *platform* #P"/tmp/" "autolith-agent."))
           (other-agent-temporary
             (platform-make-temporary-directory
              *platform* #P"/tmp/" "autolith-agent."))
           (linked-agent-temporary
             (platform-make-temporary-directory
              *platform* #P"/tmp/" "autolith-agent."))
           (secret (merge-pathnames "credential" host-temporary))
           (other-secret (merge-pathnames "credential" other-agent-temporary))
           (linked-secret (merge-pathnames "linked-credential" workspace)))
      (unwind-protect
           (progn
             (ensure-directories-exist workspace)
             (ensure-directories-exist source-root)
             (agent-sandbox-tests--write secret "fixture-host-credential")
             (agent-sandbox-tests--write other-secret "fixture-other-agent-credential")
             (test-fixture-make-symbolic-link
              *platform* (namestring secret) (namestring linked-secret))
             (with-test-environment
                 (("HOME" (agent-sandbox-tests--native home))
                  ("AUTOLITH_AGENT_TMPDIR"
                   (agent-sandbox-tests--native agent-temporary))
                  ("AUTOLITH_AGENT_SANDBOX" nil))
               (test-assert
                (not (zerop (nth-value
                             2 (uiop:run-program
                                (agent-sandbox-wrap
                                 (list "/bin/cat" (namestring secret))
                                 :source-root source-root
                                 :workspace workspace)
                                :directory workspace
                                :output nil :error-output nil
                                :ignore-error-status t))))
                "the agent cannot read another process's temporary credential")
               (test-assert
                (not (zerop (nth-value
                             2 (uiop:run-program
                                (agent-sandbox-wrap (list "/bin/ls" "/tmp")
                                                    :source-root source-root
                                                    :workspace workspace)
                                :directory workspace
                                :output nil :error-output nil
                                :ignore-error-status t))))
                "the agent cannot list host temporary directory entries")
               (dolist (path (list other-secret linked-secret
                                  (platform-truename *platform* secret)))
                 (test-assert
                  (not (zerop (nth-value
                               2 (uiop:run-program
                                  (agent-sandbox-wrap
                                   (list "/bin/cat" (namestring path))
                                   :source-root source-root
                                   :workspace workspace)
                                  :directory workspace
                                  :output nil :error-output nil
                                  :ignore-error-status t))))
                  (format nil "the agent cannot read a temporary credential through ~A"
                          path)))
               (test-assert
                (zerop (nth-value
                        2 (uiop:run-program
                           (agent-sandbox-wrap
                            (list "/bin/sh" "-c" "echo fixture > \"$TMPDIR/own\"")
                            :source-root source-root
                            :workspace workspace)
                           :directory workspace
                           :output nil :error-output nil
                           :ignore-error-status t)))
                "the agent can write its session temporary directory")
               (platform-delete-directory-tree
                *platform* linked-agent-temporary :validate t
                :if-does-not-exist ':ignore)
               (test-fixture-make-symbolic-link
                *platform* (namestring host-temporary)
                (agent-sandbox-tests--native linked-agent-temporary))
               (with-test-environment
                   (("AUTOLITH_AGENT_TMPDIR"
                     (agent-sandbox-tests--native linked-agent-temporary)))
                 (test-assert
                  (handler-case
                      (progn (agent-sandbox-policy
                              :source-root source-root :workspace workspace)
                             nil)
                    (agent-sandbox-unavailable () t))
                  "a symlink cannot become the agent session directory"))
               (with-test-environment
                   (("XDG_CACHE_HOME"
                     (agent-sandbox-tests--native
                      (merge-pathnames "cache/" workspace))))
                 (test-assert
                  (handler-case
                      (progn (agent-sandbox-policy
                              :source-root source-root :workspace workspace)
                             nil)
                    (agent-sandbox-unavailable () t))
                  "a missing launcher cache inside the workspace fails closed"))
               (with-test-environment
                   (("XDG_DATA_HOME"
                     (agent-sandbox-tests--native
                      (merge-pathnames "data/" workspace))))
                 (test-assert
                  (handler-case
                      (progn (agent-sandbox-policy
                              :source-root source-root :workspace workspace)
                             nil)
                    (agent-sandbox-unavailable () t))
                  "a missing launcher data root inside the workspace fails closed"))))
        (let ((status
                (platform-path-status
                 *platform*
                 (pathname (agent-sandbox-tests--native
                            linked-agent-temporary)))))
          (when (and status
                     (eq (platform-file-status-kind status) ':symbolic-link))
            (test-fixture-remove-link
             *platform* (agent-sandbox-tests--native linked-agent-temporary))))
        (platform-delete-directory-tree *platform* host-temporary
                                        :validate t
                                        :if-does-not-exist ':ignore)
        (platform-delete-directory-tree *platform* other-agent-temporary
                                        :validate t
                                        :if-does-not-exist ':ignore)
        (platform-delete-directory-tree *platform* agent-temporary
                                        :validate t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-recovery-git-ignores-repository-commands () null)
(defun test-recovery-git-ignores-repository-commands ()
  "Test that recovery's Git never runs commands a repository's configuration
names, since recovery runs outside the sandbox in checkouts the agent wrote."
  (with-test-fixture (:posix-shell "recovery Git outside the agent sandbox")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((repository (merge-pathnames "repository/" root))
             (marker (merge-pathnames "monitor-ran" root))
             (monitor (agent-sandbox-tests--write
                       (merge-pathnames "monitor" root)
                       (format nil "#!/bin/sh~%touch ~A~%"
                               (uiop:escape-sh-token (agent-sandbox-tests--native marker))))))
        (sb-posix:chmod (uiop:native-namestring monitor) #o755)
        (ensure-directories-exist repository)
        (uiop:run-program (list "git" "-C" (uiop:native-namestring repository) "init" "-q"))
        (uiop:run-program (list "git" "-C" (uiop:native-namestring repository)
                                "config" "core.fsmonitor" (uiop:native-namestring monitor)))
        (uiop:run-program (list "git" "-C" (uiop:native-namestring repository) "status")
                          :output nil :error-output nil :ignore-error-status t)
        (test-assert (probe-file marker)
                     "plain Git runs the repository's file system monitor")
        (delete-file marker)
        (recovery-git-output repository '("status" "--porcelain"))
        (test-assert (not (probe-file marker))
                     "recovery Git never runs the repository's file system monitor"))))
  nil)

(-> test-agent-sandbox-launcher-command () null)
(defun test-agent-sandbox-launcher-command ()
  "Test the command the launcher reads is NUL-separated, and that an unusable
workspace fails with an explanation instead of an unconfined command."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((home (merge-pathnames "home/" root)))
      (ensure-directories-exist home)
      (with-test-environment (("HOME" (agent-sandbox-tests--native home))
                              ("AUTOLITH_AGENT_SANDBOX" "active"))
        (let ((output (with-output-to-string (*standard-output*)
                        (test-assert (zerop (agent-sandbox-print-command
                                             root '("/bin/echo" "two words")))
                                     "the launcher command succeeds"))))
          (test-assert (string= output (format nil "/bin/echo~Ctwo words~C"
                                               (code-char 0) (code-char 0)))
                       "each argument ends with a NUL character")))
      (with-test-environment (("HOME" (agent-sandbox-tests--native home))
                              ("AUTOLITH_AGENT_SANDBOX" nil))
        (let* ((errors (make-string-output-stream))
               (output (with-output-to-string (*standard-output*)
                         (let ((*error-output* errors))
                           (uiop:with-current-directory (home)
                             (test-assert (= (agent-sandbox-print-command
                                              root '("/bin/echo"))
                                             1)
                                          "a refused workspace fails the launch"))))))
          (test-assert (string= output "")
                       "a refused workspace prints no command")
          (test-assert (search "cannot start its sandbox"
                               (get-output-stream-string errors))
                       "a refused workspace is explained")))))
  nil)

(-> test-agent-sandbox-awareness () null)
(defun test-agent-sandbox-awareness ()
  "Test that inside the agent sandbox commands default to full access and no
command sandbox is offered, while outside it nothing changes."
  (dolist (case '((nil nil :ask) ("off" nil :ask) ("active" t :full-access)))
    (with-test-environment (("AUTOLITH_AGENT_SANDBOX" (first case)))
      (test-assert (eq (agent-sandbox-active-p) (second case))
                   (format nil "the sandbox marker ~S is ~:[inactive~;active~]"
                           (first case) (second case)))
      (test-assert (eq (agent-sandbox-default-permission-mode ':ask) (third case))
                   (format nil "the default permission mode for ~S is ~S"
                           (first case) (third case)))))
  (with-test-environment (("AUTOLITH_AGENT_SANDBOX" "active"))
    (test-assert (not (application--command-sandbox-available-p))
                 "no command sandbox is offered inside the agent sandbox")
    (test-assert (search "agent sandbox" (application--command-sandbox-unavailable-message))
                 "the missing command sandbox is explained by the agent sandbox"))
  (with-test-configuration (configuration)
    (dolist (marker '(nil "off" "active"))
      (with-test-environment (("AUTOLITH_AGENT_SANDBOX" marker))
        (let ((loads 0))
          (test-call-with-function-replacements
           (list (list 'agent-sandbox-state
                       (lambda ()
                         (error "The active agent must not call recovery code.")))
                 (list 'mcp-configuration-load
                       (lambda (configuration)
                         (declare (ignore configuration))
                         (incf loads))))
           (lambda ()
             (application--load-extension-configuration configuration :pristine-p t)))
          (test-assert (= loads (if (equal marker "active") 0 1))
                       "startup loads MCP configuration only outside the agent sandbox")))))
  nil)
