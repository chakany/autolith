(in-package #:autolith)

;;;; -- Agent Sandbox --

;;; The pristine recovery image confines every process that runs mutable
;;; Autolith code: the active image the launcher starts, and each generation
;;; or clean source it boots itself. The agent may modify itself, so the
;;; confinement lives here, in the image the agent cannot change, and in the
;;; launcher that asks for it. Commands and Lisp workers the agent starts
;;; inherit the sandbox. It hides the user's home directory, where SSH keys,
;;; cloud tokens, and other logins live, and keeps the launcher, the recovery
;;; image, and the installed runtimes read-only, so a modified agent cannot
;;; replace what starts it next time.

(defparameter *agent-sandbox-variable* "AUTOLITH_AGENT_SANDBOX"
  "The environment variable recording the agent sandbox state: \"active\"
inside the sandbox, so nested launches are not wrapped again, or \"off\" when
the user runs Autolith unconfined.")

(defparameter *agent-sandbox-read-only-data*
  '("active/" "recovery/" "runtimes/" "recovery-worktrees/" "installation/" "nix/"
    "helpers/")
  "Directories below the data root the agent may read but never replace: what
unconfined programs start or load. These are the active and recovery images,
the installed runtimes and releases, the Nix images and the compiled files
their builder loads, the sandbox helpers, and the checkouts where recovery runs
Git.")

(defparameter *agent-sandbox-home-read-paths*
  '("quicklisp/" "common-lisp/" ".codex/auth.json" ".grok/auth.json")
  "Paths below the home directory the agent reads: its Lisp dependency
stores, and the provider logins it imports until the launcher brokers them.")


;;;; -- Public Functions --

(serapeum:-> agent-sandbox-state () (member :enabled :active :off))
(defun agent-sandbox-state ()
  "Return :ACTIVE inside the agent sandbox, :OFF when the user disabled it, and
:ENABLED when a process launched now must be confined."
  (let ((value (uiop:getenv *agent-sandbox-variable*)))
    (cond
      ((equal value "active")
       ':active)
      ((equal value "off")
       ':off)
      (t
       ':enabled))))

(serapeum:-> agent-sandbox-policy
    (&key (:source-root pathname) (:workspace pathname))
    cl-exec-sandbox:sandbox-policy)
(defun agent-sandbox-policy (&key source-root workspace)
  "Return the sandbox policy for an agent working in WORKSPACE with the tracked
source at SOURCE-ROOT.

The agent may write its workspace, Autolith's own configuration, data, state,
and cache roots, and temporary directories. The source root is read-only
unless it is the workspace itself, the images and runtimes the launcher starts
are always read-only, and the rest of the home directory is hidden. The
network stays open, and processes are not isolated, so sessions started from
different terminals still find each other."
  (let* ((home (uiop:ensure-directory-pathname (user-homedir-pathname)))
         (workspace (uiop:ensure-directory-pathname workspace))
         (data-root (autolith-application-root :data)))
    (when (uiop:subpathp home workspace)
      (error 'agent-sandbox-unavailable
             :message (format nil "Autolith works in ~A, which contains the home directory ~
                                   it would hide. Start it in a project directory, or set ~
                                   ~A=off to run it without the sandbox."
                              (uiop:native-namestring workspace)
                              *agent-sandbox-variable*)))
    (flet ((rule (path access)
             (cl-exec-sandbox:make-filesystem-rule :kind ':path :path path :access access))
           (special (path access)
             (cl-exec-sandbox:make-filesystem-rule :kind ':special :path path :access access)))
      (cl-exec-sandbox:make-sandbox-policy
       :network ':enabled
       :isolate-processes-p nil
       :workspace-roots (list workspace)
       :protected-metadata-names nil
       :filesystem-rules
       (append
        (list (special ':root ':read)
              (special ':home ':deny)
              (special ':search-path ':read)
              (special ':workspace-roots ':write)
              (special ':tmpdir ':write)
              (special ':slash-tmp ':write))
        (mapcar (lambda (kind) (rule (autolith-application-root kind) ':write))
                '(:config :data :state :cache))
        (mapcar (lambda (relative) (rule (merge-pathnames relative data-root) ':read))
                *agent-sandbox-read-only-data*)
        (unless (uiop:subpathp source-root workspace)
          (list (rule source-root ':read)))
        (loop for relative in *agent-sandbox-home-read-paths*
              for path = (merge-pathnames relative home)
              when (probe-file path)
                collect (rule path ':read))
        (agent-sandbox--runtime-rules))))))

(serapeum:-> agent-sandbox-wrap
    (list &key (:source-root pathname) (:workspace pathname)
               (:working-directory pathname))
    list)
(defun agent-sandbox-wrap (command &key source-root workspace (working-directory workspace))
  "Return the argument vector that runs COMMAND, a program and its arguments,
inside the agent sandbox for WORKSPACE and SOURCE-ROOT, starting in
WORKING-DIRECTORY, which defaults to WORKSPACE.

COMMAND is returned unchanged inside the sandbox, when the user disabled it,
and on Windows, whose launcher does not confine the agent yet. Signal
AGENT-SANDBOX-UNAVAILABLE when this host has no sandbox backend."
  (if (or (not (eq (agent-sandbox-state) ':enabled))
          (uiop:os-windows-p))
      command
      (let* ((inner (agent-sandbox--with-environment command))
             (plan (handler-case
                       (cl-exec-sandbox:sandbox-build-plan
                        (first inner) (rest inner)
                        :policy (agent-sandbox-policy :source-root source-root
                                                      :workspace workspace)
                        :working-directory working-directory)
                     (cl-exec-sandbox:sandbox-unavailable (condition)
                       (error 'agent-sandbox-unavailable
                              :message (format nil "~A Set ~A=off to run Autolith without ~
                                                    the sandbox."
                                               condition *agent-sandbox-variable*))))))
        (when (cl-exec-sandbox:sandbox-plan-cleanup-paths plan)
          (error 'agent-sandbox-unavailable
                 :message "The agent sandbox would need files the launcher cannot remove."))
        (cons (uiop:native-namestring (cl-exec-sandbox:sandbox-plan-program plan))
              (cl-exec-sandbox:sandbox-plan-arguments plan)))))

(serapeum:-> agent-sandbox-cache-home () pathname)
(defun agent-sandbox-cache-home ()
  "Return the XDG cache home of processes inside the agent sandbox.

The user's own caches are hidden, because unconfined programs, recovery among
them, load compiled files from the ASDF cache there: a file the agent compiled
into it could run outside the sandbox. Inside, ASDF's default cache and
Autolith's cache root both follow this directory, below Autolith's own cache,
whatever output translations the runtime installs."
  (merge-pathnames "agent-sandbox/" (autolith-application-root :cache)))

(serapeum:-> agent-sandbox-print-command (pathname list) integer)
(defun agent-sandbox-print-command (source-root command)
  "Write the sandboxed argument vector for COMMAND to standard output, each
argument followed by a NUL character, for the launcher to run. Return the
process status: 0, or 1 after explaining why the sandbox is unavailable."
  (handler-case
      (let ((wrapped (agent-sandbox-wrap command
                                         :source-root source-root
                                         :workspace (uiop:getcwd))))
        (dolist (argument wrapped)
          (write-string argument)
          (write-char (code-char 0)))
        (finish-output)
        0)
    (agent-sandbox-unavailable (condition)
      (format *error-output* "Autolith cannot start its sandbox: ~A~%" condition)
      1)))


;;;; -- Private Functions --

(serapeum:-> agent-sandbox--with-environment (list) list)
(defun agent-sandbox--with-environment (command)
  "Return COMMAND run through env with every cache inside the agent sandbox.

A Nix installation compiles Autolith's source into AUTOLITH_ASDF_CACHE, which
its unconfined image builder also loads, so the agent gets its own there too."
  (let* ((cache-home (agent-sandbox-cache-home))
         (nix-cache (uiop:getenv "AUTOLITH_ASDF_CACHE"))
         (nix-identity (and nix-cache
                            (plusp (length nix-cache))
                            (first (last (pathname-directory
                                          (uiop:ensure-directory-pathname nix-cache))))))
         (bindings
           (list (format nil "XDG_CACHE_HOME=~A"
                         (string-right-trim "/" (uiop:native-namestring cache-home))))))
    (when nix-identity
      (setf bindings
            (append bindings
                    (list (format nil "AUTOLITH_ASDF_CACHE=~A"
                                  (uiop:native-namestring
                                   (merge-pathnames (format nil "nix-asdf/~A/" nix-identity)
                                                    cache-home)))))))
    (append (list "/usr/bin/env") bindings command)))

(serapeum:-> agent-sandbox--runtime-rules () list)
(defun agent-sandbox--runtime-rules ()
  "Return read rules for the SBCL runtime and dependency setup the environment
names, which may live below the hidden home directory."
  (loop for (variable . directory-p) in '(("AUTOLITH_SBCL" . nil)
                                          ("SBCL_HOME" . t)
                                          ("AUTOLITH_PROJECT_SETUP" . nil))
        for value = (uiop:getenv variable)
        for path = (and value
                        (uiop:absolute-pathname-p (pathname value))
                        (if directory-p
                            (uiop:ensure-directory-pathname value)
                            (uiop:pathname-directory-pathname value)))
        when (and path (probe-file path))
          collect (cl-exec-sandbox:make-filesystem-rule :kind ':path
                                                        :path path
                                                        :access ':read)))


;;;; -- Conditions --

(define-condition agent-sandbox-unavailable (error)
  ((message
    :initarg :message
    :reader agent-sandbox-unavailable-message
    :documentation "Why the agent cannot be started inside its sandbox."))
  (:documentation "Signaled when the agent sandbox cannot confine a launch.")
  (:report (lambda (condition stream)
             (write-string (agent-sandbox-unavailable-message condition) stream))))
