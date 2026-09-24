(in-package #:autolith)

;;;; -- Command Environment --

;;; Shell commands the agent runs receive the agent's own environment, which
;;; can carry provider keys and other credentials. Every command instead
;;; receives a copy without them, and sandboxed commands also receive a
;;; private home directory, because the sandbox hides the user's home, where
;;; credentials usually live.

(defparameter *command-credential-variables*
  '("ANTHROPIC_API_KEY" "OPENROUTER_API_KEY" "MISTRAL_API_KEY"
    "FIREWORKS_API_KEY" "OPENCODE_API_KEY" "OPENAI_API_KEY"
    "AUTOLITH_GEMINI_OAUTH_CLIENT_ID" "AUTOLITH_GEMINI_OAUTH_CLIENT_SECRET"
    "AUTOLITH_MANAGEMENT_REPL_TOKEN_FILE" "SSH_AUTH_SOCK")
  "Variables removed from every command environment by exact name: the
credentials Autolith itself reads and the SSH agent socket, which signs with
the user's keys.")

(defparameter *command-credential-suffixes*
  '("_API_KEY" "_TOKEN" "_SECRET" "_SECRET_KEY" "_SECRET_ACCESS_KEY" "_PASSWORD")
  "Name suffixes that mark a variable as a credential, such as GITLAB_TOKEN or
AWS_SECRET_ACCESS_KEY, removed from every command environment.")

(defparameter *command-home-variables*
  '(("HOME" . "")
    ("XDG_CONFIG_HOME" . ".config/")
    ("XDG_CACHE_HOME" . ".cache/")
    ("XDG_DATA_HOME" . ".local/share/")
    ("XDG_STATE_HOME" . ".local/state/"))
  "Variables pointing into a sandboxed command's private home, each with its
directory relative to that home.")


;;;; -- Public Functions --

(-> command-environment-credential-p (string) boolean)
(defun command-environment-credential-p (binding)
  "Return whether NAME=VALUE BINDING holds a credential a command must not see."
  (let ((name (string-upcase (subseq binding 0 (or (position #\= binding)
                                                    (length binding))))))
    (and (or (member name *command-credential-variables* :test #'string=)
             (some (lambda (suffix) (uiop:string-suffix-p name suffix))
                   *command-credential-suffixes*))
         t)))

(-> command-environment (&key (:base list) (:overrides list)) list)
(defun command-environment (&key (base (sb-ext:posix-environ)) overrides)
  "Return the NAME=VALUE bindings a command runs with: BASE without credentials
or the names OVERRIDES sets, followed by OVERRIDES."
  (let ((names (mapcar #'command-environment--name overrides)))
    (append (remove-if (lambda (binding)
                         (or (command-environment-credential-p binding)
                             (member (command-environment--name binding) names
                                     :test #'string-equal)))
                       base)
            overrides)))

(-> command-environment-private-home (pathname) list)
(defun command-environment-private-home (home)
  "Prepare HOME as a sandboxed command's private home directory and return the
bindings that point the command at it.

HOME receives the user's Git name and email, and nothing else from the real
home directory, so commits keep their author while credentials stay hidden."
  (dolist (entry *command-home-variables*)
    (ensure-directories-exist (merge-pathnames (rest entry) home)))
  (command-environment--write-git-identity home)
  (mapcar (lambda (entry)
            (format nil "~A=~A" (first entry)
                    (string-right-trim "/" (uiop:native-namestring
                                            (merge-pathnames (rest entry) home)))))
          *command-home-variables*))


;;;; -- Private Functions --

(-> command-environment--name (string) string)
(defun command-environment--name (binding)
  "Return the variable name of NAME=VALUE BINDING."
  (subseq binding 0 (or (position #\= binding) (length binding))))

(-> command-environment--git-setting (string) (option string))
(defun command-environment--git-setting (key)
  "Return the user's global Git setting KEY, or NIL when Git or KEY is absent."
  (handler-case
      (multiple-value-bind (output error-output status)
          (uiop:run-program (list "git" "config" "--global" "--get" key)
                            :output '(:string :stripped t)
                            :error-output nil
                            :ignore-error-status t)
        (declare (ignore error-output))
        (and (zerop status) (non-empty-string-p output) output))
    (error ()
      nil)))

(-> command-environment--write-git-identity (pathname) null)
(defun command-environment--write-git-identity (home)
  "Write the user's Git name and email, when set, to HOME's .gitconfig."
  (let ((settings (loop for (key field) in '(("user.name" "name") ("user.email" "email"))
                        for value = (command-environment--git-setting key)
                        when (and value (not (find #\Newline value)))
                          collect (list field value))))
    (when settings
      (with-open-file (stream (merge-pathnames ".gitconfig" home)
                              :direction ':output
                              :if-exists ':supersede
                              :external-format ':utf-8)
        (format stream "[user]~%")
        (loop for (field value) in settings
              do (format stream "~C~A = ~A~%" #\Tab field
                         (command-environment--git-quoted value))))))
  nil)

(-> command-environment--git-quoted (string) string)
(defun command-environment--git-quoted (value)
  "Return VALUE as a quoted Git configuration value."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across value
          do (when (member character '(#\" #\\))
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))
