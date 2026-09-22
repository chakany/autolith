(require :asdf)

;; Run with a dependency-equipped SBCL; use --host-only to omit Docker tests.
(let* ((directory (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-parent-directory-pathname directory)))
       (arguments (uiop:command-line-arguments)))
  (unless (or (null arguments) (equal arguments '("--host-only")))
    (format *error-output* "Usage: sbcl --script check-artifacts.lisp [--host-only]~%")
    (uiop:quit 2))
  (asdf:clear-system :autolith)
  (asdf:clear-system :autolith/tests)
  (push root asdf:*central-registry*)
  (load (merge-pathnames "autolith.asd" root))
  (asdf:load-system :autolith/tests)
  (dolist (name '("artifact-broker.lisp" "artifact-runner.lisp" "artifact-tests.lisp"))
    (load (merge-pathnames name directory)))
  (format t "~&Source: ~A~%" (asdf:system-source-directory :autolith))
  (uiop:quit
   (if (uiop:symbol-call '#:autolith '#:run-tests
                        :suites (if arguments
                                    '("rumprun-artifact-broker")
                                    '("rumprun-artifact-broker" "rumprun-artifact-guest")))
       0
       1)))
