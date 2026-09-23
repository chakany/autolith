;;;; Build the SBCL contribs Autolith needs inside the rumprun guest, as
;;;; contrib/make-contrib.lisp does natively. Contribs that grovel C constants
;;;; take two guests. Without constants from the host, this script prints each
;;;; grovel C program into /core/export/grovel/; the host compiles and runs
;;;; those programs as guests of their own. With the constants embedded below
;;;; /core/rumprun-grovel/, it builds every contrib into
;;;; /core/export/sbcl-home/contrib/, the layout SBCL_HOME expects.
(defpackage #:autolith (:use #:cl))
(in-package #:autolith)

(defparameter *contribs*
  '("sb-posix" "sb-bsd-sockets" "sb-rotate-byte" "sb-cltl2" "sb-introspect")
  "Contribs to build, in dependency order.")

(defparameter *grovel-constants* #p"/core/rumprun-grovel/"
  "Host-produced grovel output, one <contrib>.lisp file per groveling contrib.")

(defparameter *contrib-home* #p"/core/export/sbcl-home/contrib/"
  "Directory receiving the joined contrib fasls and their ASDF definitions.")

(setf (logical-pathname-translations "SYS")
      '(("SYS:CONTRIB;**;*.*.*" "/core/contrib/**/*.*")
        ("SYS:OBJ;**;*.*.*"     "/core/obj/**/*.*")
        ("SYS:SRC;**;*.*.*"     "/core/src/**/*.*")))

(defun contrib-definition (system)
  "Return the DEFSYSTEM form of SYSTEM, skipping the ERROR form that stops
ordinary ASDF loads of contrib definitions."
  (with-open-file (stream (format nil "/core/contrib/~A/~A.asd" system system))
    (let ((*package*   (find-package "CL-USER"))
          (*read-eval* nil))
      (let ((form (read stream)))
        (assert (eq (first form) 'error))
        (read stream)))))

(defun contrib-sources (definition)
  "Return SYSTEM's component files as (GENERATED-P STEM) entries in build
order, and its grovel specifications as (SPECFILE . PACKAGE) pairs, as two
values. Grovel output compiles in place of the first specification."
  (let ((sources nil)
        (specifications nil))
    (labels ((flatten (prefix components)
               (dolist (component components)
                 (ecase (first component)
                   (:module
                    (let* ((subdirectory (second component))
                           (pathname     (getf component :pathname subdirectory)))
                      (flatten (if (string= pathname "")
                                   prefix
                                   (concatenate 'string prefix subdirectory "/"))
                               (getf component :components))))
                   (:file
                    (let ((if-feature (getf component :if-feature)))
                      (when (or (not if-feature) (sb-int:featurep if-feature))
                        (push (list nil (concatenate 'string prefix (second component)))
                              sources))))
                   (:sb-grovel-constants-file
                    (destructuring-bind (specfile &key package if-feature &allow-other-keys)
                        (rest component)
                      (assert package)
                      (when (or (not if-feature) (sb-int:featurep if-feature))
                        (unless specifications
                          (push (list t "generated-constants") sources))
                        (push (cons specfile package) specifications))))))))
      (flatten "" (getf definition :components)))
    (values (nreverse sources) (nreverse specifications))))

(defun contrib-features ()
  "Return the features in effect while building a contrib, as
make-contrib.lisp binds them."
  (append '(:sb-building-contrib) *features* sb-impl:+internal-features+))

(defun contrib-apply-evaluation (definition)
  "Evaluate DEFINITION's :EVAL form, which may select features."
  (let ((form (getf definition :eval)))
    (when form
      (eval form))))

(defun load-grovel (&rest names)
  "Load the named sb-grovel source files, compiling as make-contrib.lisp does."
  (load "/core/contrib/sb-grovel/defpackage.lisp")
  (let ((sb-ext:*evaluator-mode* ':compile))
    (dolist (name names)
      (load (format nil "/core/contrib/sb-grovel/~A.lisp" name)))))

(defun grovel-inputs (system specifications)
  "Combine SYSTEM's grovel SPECIFICATIONS into headers, definitions, and the
single package they share, as three values."
  (let ((headers nil) (definitions nil) (package nil))
    (dolist (specification specifications)
      (if package
          (assert (eq (rest specification) package))
          (setf package (rest specification)))
      (with-open-file (stream (format nil "/core/contrib/~A/~A.lisp" system (first specification)))
        (let ((*read-eval* nil))
          ;; Order can matter for headers.
          (setf headers     (append headers (read stream))
                definitions (append definitions (read stream))))))
    (values headers definitions package)))

(defun print-grovel-programs ()
  "Write the grovel C program of each groveling contrib to /core/export/grovel/."
  (load-grovel "def-to-lisp")
  (dolist (system *contribs*)
    (let ((definition (contrib-definition system)))
      (contrib-apply-evaluation definition)
      (multiple-value-bind (sources specifications)
          (let ((*features* (contrib-features))) (contrib-sources definition))
        (declare (ignore sources))
        (when specifications
          (multiple-value-bind (headers definitions package)
              (grovel-inputs system specifications)
            (let ((path (format nil "/core/export/grovel/~A.c" system)))
              (ensure-directories-exist path)
              (with-open-file (stream path :direction ':output :if-exists ':supersede)
                (funcall (find-symbol "PRINT-C-SOURCE" "SB-GROVEL")
                         stream headers definitions package))
              (format t "CONTRIB-GROVEL-PROGRAM ~A~%" system))))))))

(defun logicalize (system stem generated-p)
  "Return the logical source pathname of STEM, a component of SYSTEM."
  (pathname (format nil (if generated-p
                            "SYS:OBJ;FROM-SELF;CONTRIB;~:@(~A~);~:@(~A~).LISP"
                            "SYS:CONTRIB;~:@(~A~);~:@(~A~).LISP")
                    system (substitute #\; #\/ stem))))

(defun copy-bytes (input output)
  "Copy every byte of the open INPUT stream to OUTPUT."
  (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop for count = (read-sequence buffer input)
          while (plusp count)
          do (write-sequence buffer output :end count))))

(defun join-files (inputs output)
  "Concatenate the INPUTS files byte for byte into OUTPUT."
  (with-open-file (out output :direction ':output :if-exists ':supersede
                              :element-type '(unsigned-byte 8))
    (dolist (input inputs)
      (with-open-file (in input :element-type '(unsigned-byte 8))
        (copy-bytes in out)))))

(defun build-contrib (system)
  "Compile and load SYSTEM's components serially, then join them into one
fasl with a PROVIDE form and write its ASDF definition, as make-contrib.lisp
does. Fail on compiler warnings."
  (let* ((definition (contrib-definition system))
         (objdir     (format nil "/core/obj/from-self/contrib/~A/" system)))
    (contrib-apply-evaluation definition)
    (ensure-directories-exist objdir)
    (let ((*compile-verbose* nil)
          (*features* (contrib-features))
          (bindings   (getf definition :bind))
          (fasls      nil))
      (progv (mapcar #'first bindings) (mapcar #'second bindings)
        (multiple-value-bind (sources specifications) (contrib-sources definition)
          (when specifications
            (load-grovel "def-to-lisp")
            (join-files (list (merge-pathnames (format nil "~A.lisp" system) *grovel-constants*))
                        (format nil "~Agenerated-constants.lisp" objdir))
            ;; foreign-glue holds the macros the generated file uses.
            (load-grovel "foreign-glue"))
          (let ((warnings nil))
            (handler-bind (((and warning (not style-warning))
                             (lambda (condition) (push condition warnings))))
              (with-compilation-unit ()
                (loop for (generated-p stem) in sources
                      do (multiple-value-bind (output warnings-p failure-p)
                             (compile-file (logicalize system stem generated-p)
                                           :output-file (format nil "~A~A.fasl" objdir stem))
                           (when (or warnings-p failure-p (null output))
                             (error "Compiling ~A for ~A failed." stem system))
                           (push output fasls)
                           (load output)))))
            (when warnings
              (error "Building ~A signaled warnings: ~{~A~^; ~}" system warnings)))))
      (push (sb-c:compile-form-to-file `(provide ,(string-upcase system))
                                       (format nil "~Amodule-provide" objdir))
            fasls)
      (ensure-directories-exist *contrib-home*)
      (join-files (reverse fasls) (merge-pathnames (format nil "~A.fasl" system) *contrib-home*))
      (with-open-file (asd (merge-pathnames (format nil "~A.asd" system) *contrib-home*)
                           :direction ':output :if-exists ':supersede)
        (format asd "(defsystem :~A :class require-system)~%" system))
      (format t "CONTRIB-BUILT ~A~%" system))))

(defun build-asdf ()
  "Compile and load UIOP and then ASDF into the contrib directory, as
contrib/asdf does."
  (ensure-directories-exist *contrib-home*)
  (let ((*readtable* (copy-readtable)))
    (setf (sb-ext:readtable-base-char-preference *readtable*) ':both)
    (dolist (name '("UIOP" "ASDF"))
      (let ((fasl (compile-file (format nil "SYS:CONTRIB;ASDF;~A.LISP" name)
                                :print nil
                                :output-file (merge-pathnames (format nil "~(~A~).fasl" name)
                                                              *contrib-home*))))
        (unless fasl
          (error "Compiling ~A failed." name))
        ;; Compiling ASDF requires UIOP, which provides its own module.
        (load fasl))
      (format t "CONTRIB-BUILT ~(~A~)~%" name))))

(if (probe-file *grovel-constants*)
    (progn
      (build-asdf)
      (mapc #'build-contrib *contribs*)
      (format t "CONTRIBS-OK ~D~%" (+ 2 (length *contribs*))))
    (print-grovel-programs))
