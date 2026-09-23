;;;; Guest checks of programs the broker runs on the host. The host exports
;;;; its workspace, which the guest mounts at the same path, so both sides
;;;; name the same files; the host checks what the guest built afterward.
(require :asdf)

(defvar *checks* 0
  "The number of checks that passed.")

(defparameter *workspace* (uiop:ensure-directory-pathname (uiop:getenv "CHECK_WORKSPACE"))
  "The shared workspace.")

(defun check (value label)
  "Fail the guest on an incorrect result, reporting a completed check otherwise."
  (unless value (error "Guest process check failed: ~A" label))
  (incf *checks*)
  (format t "PASS ~A~%" label)
  (finish-output))

(defun run (arguments &rest options)
  "Run ARGUMENTS in the workspace and return UIOP's output, error, and status."
  (apply #'uiop:run-program arguments :directory *workspace* :ignore-error-status t options))

(multiple-value-bind (output error status)
    (run '("/bin/sh" "-c" "printf out; printf err >&2; exit 3") :output :string :error-output :string)
  (check (and (string= output "out") (string= error "err") (= status 3))
         "standard output, error, and exit status"))
(check (string= (run '("cat") :input (make-string-input-stream "piped input") :output :string)
                "piped input")
       "standard input to a program found in the host's search path")
(check (string= (run '("pwd") :output '(:string :stripped t)) (string-right-trim "/" (namestring *workspace*)))
       "the program starts in the shared workspace")
(let ((process (uiop:launch-program '("sleep" "30") :directory *workspace*)))
  (sleep 0.5)
  (check (uiop:process-alive-p process) "a started program is alive")
  (uiop:terminate-process process)
  (check (/= 0 (uiop:wait-process process)) "a terminated program ends"))
(defun failure-text (function)
  "Return the report of the error FUNCTION signals, or a note that it signals none."
  (handler-case (progn (funcall function) "no error")
    (error (condition) (princ-to-string condition))))

(let ((text (failure-text (lambda () (run '("/no/such/program"))))))
  (check (search "No such file" text) (format nil "a missing program is reported as missing: ~A" text)))
(let ((text (failure-text (lambda () (uiop:run-program '("true") :directory "/core/")))))
  (check (search "Permission denied" text)
         (format nil "a program outside the workspace is refused: ~A" text)))
;; Compiling on the host into the shared workspace.
(with-open-file (out (merge-pathnames "answer.c" *workspace*) :direction :output :if-exists :supersede)
  (write-line "int main(void) { return 42; }" out))
(multiple-value-bind (output error status) (run '("cc" "-o" "answer" "answer.c")
                                                :output :string :error-output :string)
  (declare (ignore output))
  (check (zerop status) (format nil "the host compiler builds in the workspace~@[: ~A~]"
                                   (and (plusp (length error)) error))))
(check (probe-file (merge-pathnames "answer" *workspace*)) "the guest sees the host's build output")
(check (= 42 (nth-value 2 (run '("./answer")))) "the built program runs on the host")
(format t "PROCESS-OK ~D checks~%" *checks*)
(sb-ext:exit :code 0)
