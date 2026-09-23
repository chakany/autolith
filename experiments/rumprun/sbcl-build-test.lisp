;;;; Run with sbcl --script experiments/rumprun/sbcl-build-test.lisp.
(require :asdf)
(let ((*features* (cons :sbcl-build-library *features*)))
  (load (merge-pathnames "sbcl-build.lisp" *load-truename*)))
(in-package #:autolith)

(defun sbcl-build-test-error-p (function)
  "Return whether FUNCTION signals an error."
  (handler-case (progn (funcall function) nil)
    (error () t)))

(defun sbcl-build-test-main ()
  "Exercise process statuses, guest evidence, and fail-closed grovel extraction."
  (let* ((directory (uiop:ensure-directory-pathname
                     (string-trim '(#\Newline #\Return)
                                  (uiop:run-program '("mktemp" "-d" ".sbcl-driver-test.XXXXXX")
                                                    :output ':string))))
         (*sbcl-log-directory* directory)
         (log (merge-pathnames "input.log" directory))
         (output (merge-pathnames "generated.lisp" directory))
         (checks 0))
    (unwind-protect
         (labels ((check (value)
                    (assert value)
                    (incf checks))

                  (write-log (text)
                    (with-open-file (stream log :direction ':output :if-exists ':supersede)
                      (write-string text stream)))

                  (grovel (body &optional (header t) (marker t))
                    (format nil "boot noise~%~A~%~A~%~A~%"
                            (if header
                                ";;;; This is an automatically generated file, please do not hand-edit it."
                                "")
                            body
                            (if marker "=== main() of \"grovel\" returned 0 ===" ""))))
           (dolist (status '(0 1 7 124))
             (check (= status (sbcl-build-command
                               (list "sh" "-c" (format nil "exit ~D" status))
                               directory "status.log" :accepted-statuses (list status)))))
           (check (sbcl-build-test-error-p
                   (lambda () (sbcl-build-command '("sh" "-c" "exit 7") directory "status.log"))))
           (write-log (format nil "MACHINE-OK: checked~C~%=== main() of \"machine-test\" returned 0 ===~C~%"
                              #\Return #\Return))
           (check (not (sbcl-build-validate-guest
                        log '((:prefix "MACHINE-OK:")
                              "=== main() of \"machine-test\" returned 0 ==="))))
           (dolist (text '("MACHINE-OK: checked" "=== main() of \"machine-test\" returned 1 ==="
                           "prefix === main() of \"machine-test\" returned 0 ==="))
             (write-log text)
             (check (sbcl-build-test-error-p
                     (lambda () (sbcl-build-validate-guest
                                 log '((:prefix "MACHINE-OK:")
                                       "=== main() of \"machine-test\" returned 0 ==="))))))
           (let ((body (format nil "(in-package \"SB-ALIEN\")~%~{~A~%~}(define-alien-type os-vm-size-t (unsigned 64))"
                               (make-list 131 :initial-element "(defconstant x 1)"))))
             (write-log (grovel body))
             (sbcl-build-extract-grovel log output)
             (check (= 133 (length (sbcl-build-read-forms (uiop:read-file-string output)))))
             (let ((original (uiop:read-file-string output)))
               (dolist (text (list (grovel body nil) (grovel body t nil)
                                  (grovel (concatenate 'string "(" body))
                                  (grovel (concatenate 'string "#.(error \"evaluated\")" body))
                                  (grovel (concatenate 'string body " :eof ("))
                                  (grovel (concatenate 'string body " (extra)"))
                                  (grovel "(in-package \"SB-ALIEN\")")))
                 (write-log text)
                 (check (sbcl-build-test-error-p
                         (lambda () (sbcl-build-extract-grovel log output))))
                 (check (string= original (uiop:read-file-string output))))))
           (format t "SBCL-BUILD-TEST-OK ~D checks~%" checks))
      (dolist (file (uiop:directory-files directory))
        (delete-file file))
      (uiop:run-program (list "rmdir" (namestring directory))))))

(sbcl-build-test-main)
