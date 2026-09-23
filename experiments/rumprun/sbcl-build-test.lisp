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
  "Exercise process statuses, guest evidence, and fail-closed grovel and
export extraction."
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
           (let ((capture (merge-pathnames "export.bin" directory))
                 (unpacked (merge-pathnames "unpacked/" directory)))
             (labels ((fnv (octets)
                        (let ((hash #xcbf29ce484222325))
                          (loop for octet across octets
                                do (setf hash (ldb (byte 64 0)
                                                   (* (logxor hash octet) #x100000001b3))))
                          hash))

                      (octets (text)
                        (map '(vector (unsigned-byte 8)) #'char-code text))

                      (write-capture (records &key (header "RUMPRUN-EXPORT 1")
                                                   (trailer (format nil "END~%"))
                                                   corrupt)
                        ;; Each record is (PATH CONTENT); CORRUPT damages the first hash.
                        (with-open-file (stream capture :direction ':output :if-exists ':supersede
                                                        :element-type '(unsigned-byte 8))
                          (flet ((emit (text) (write-sequence (octets text) stream)))
                            (emit (format nil "~A~%" header))
                            (loop for (path content) in records
                                  for first = t then nil
                                  do (let ((data (octets content)))
                                       (emit (format nil "FILE ~A ~D ~(~16,'0x~)~%" path (length data)
                                                     (logxor (fnv data) (if (and corrupt first) 1 0))))
                                       (write-sequence data stream)
                                       (emit (string #\Newline))))
                            (when trailer (emit trailer)))))

                      (rejected-p (records &rest options)
                        (apply #'write-capture records options)
                        (sbcl-build-test-error-p
                         (lambda () (sbcl-build-extract-exports capture unpacked)))))
               (write-capture '(("sbcl.core" "core bytes") ("obj/from-self/a.fasl" "")))
               (check (equal (sbcl-build-extract-exports capture unpacked)
                             '("sbcl.core" "obj/from-self/a.fasl")))
               (check (string= (uiop:read-file-string (merge-pathnames "sbcl.core" unpacked))
                               "core bytes"))
               (check (zerop (with-open-file (stream (merge-pathnames "obj/from-self/a.fasl" unpacked))
                               (file-length stream))))
               (check (rejected-p '(("sbcl.core" "core")) :corrupt t))
               (check (rejected-p '(("../escape" "x"))))
               (check (rejected-p '((".hidden" "x"))))
               (check (rejected-p '(("a//b" "x"))))
               (check (rejected-p '(("same" "x") ("same" "y"))))
               (check (rejected-p '(("sbcl.core" "x")) :header "RUMPRUN-EXPORT 2"))
               (check (rejected-p '(("sbcl.core" "x")) :trailer nil))
               (check (rejected-p '(("sbcl.core" "x")) :trailer (format nil "END~%extra")))
               (write-capture '(("sbcl.core" "core bytes")))
               (let ((text (uiop:read-file-string capture)))
                 ;; Truncate inside the file data.
                 (with-open-file (stream capture :direction ':output :if-exists ':supersede)
                   (write-string (subseq text 0 (- (length text) 8)) stream)))
               (check (sbcl-build-test-error-p
                       (lambda () (sbcl-build-extract-exports capture unpacked))))))
           (format t "SBCL-BUILD-TEST-OK ~D checks~%" checks))
      (uiop:run-program (list "rm" "-rf" (namestring directory))))))

(sbcl-build-test-main)
