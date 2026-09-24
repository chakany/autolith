(in-package #:autolith)

;;;; -- Command Environment --

;;; Shell commands the agent runs receive the agent's own environment, which
;;; can carry provider keys and other credentials. Every command instead
;;; receives a copy without them.

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


;;;; -- Private Functions --

(-> command-environment--name (string) string)
(defun command-environment--name (binding)
  "Return the variable name of NAME=VALUE BINDING."
  (subseq binding 0 (or (position #\= binding) (length binding))))
