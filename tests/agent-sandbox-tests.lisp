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
           (state-root (merge-pathnames ".local/state/autolith/" home))
           (active (merge-pathnames ".local/share/autolith/active/" home))
           (worktrees (merge-pathnames ".local/share/autolith/recovery-worktrees/" home)))
      (agent-sandbox-tests--write (merge-pathnames "README" source-root) "source")
      (ensure-directories-exist workspace)
      (ensure-directories-exist state-root)
      (ensure-directories-exist active)
      (ensure-directories-exist worktrees)
      (with-test-environment (("HOME" (agent-sandbox-tests--native home))
                              ("XDG_CONFIG_HOME" nil)
                              ("XDG_DATA_HOME" nil)
                              ("XDG_STATE_HOME" nil)
                              ("XDG_CACHE_HOME" nil)
                              ("AUTOLITH_AGENT_SANDBOX" nil))
        (flet ((allowed-p (script)
                 (zerop (nth-value
                         2 (uiop:run-program
                            (agent-sandbox-wrap (list "/bin/sh" "-c" script)
                                                :source-root source-root
                                                :workspace workspace)
                            :directory workspace
                            :output nil :error-output nil
                            :ignore-error-status t))))
               (quoted (pathname)
                 (uiop:escape-sh-token (agent-sandbox-tests--native pathname))))
          (test-assert (not (allowed-p (format nil "cat ~A" (quoted secret))))
                       "the agent cannot read the hidden home directory")
          (test-assert (allowed-p (format nil "echo made > ~A"
                                          (quoted (merge-pathnames "made" workspace))))
                       "the agent writes its workspace")
          (test-assert (allowed-p (format nil "echo state > ~A"
                                          (quoted (merge-pathnames "state" state-root))))
                       "the agent writes its state root")
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
        (test-assert (not (allowed-p (format nil "echo x > ~A"
                                             (quoted (merge-pathnames "config" worktrees)))))
                     "the agent cannot write the checkouts recovery runs Git in")))))
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
  nil)
