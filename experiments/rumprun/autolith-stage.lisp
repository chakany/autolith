;;;; Stage Autolith and the sources of every system it depends on for the
;;;; rumprun Autolith guests. Run with an SBCL whose ASDF registry holds
;;;; Autolith's dependencies, such as the Nix sbcl-with-packages:
;;;;
;;;;   sbcl --script experiments/rumprun/autolith-stage.lisp OUTPUT
;;;;
;;;; OUTPUT receives autolith/ (the definition, sources, tests, release
;;;; server, and documentation) and deps/ (one source tree per dependency,
;;;; named after its registry directory). Compiled and native artifacts are
;;;; left out: guests compile everything they load.
(require :asdf)
(unless (find-package '#:autolith) (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

(defparameter *autolith-stage-systems*
  '("autolith" "autolith/tests" "autolith/release-server")
  "The Autolith systems whose dependencies the guests load.")

(defparameter *autolith-stage-excluded-types*
  '("fasl" "so" "dylib" "dll" "o" "a" "c" "h" "core" "png" "jpg" "gif" "pdf")
  "File types never copied into the stage.")

(defparameter *autolith-stage-repository-entries*
  '("autolith.asd" "src/" "tests/" "server/" "recovery/" "bin/" "script/" "docs/")
  "Repository entries staged below autolith/.")

(defun autolith-stage-dependency-names (specification)
  "Return the system names SPECIFICATION names. Feature-conditional
dependencies are included whatever the staging host's features are,
because the guest's features differ."
  (cond ((or (stringp specification) (symbolp specification))
         (list (asdf:coerce-name specification)))
        ((member (first specification) '(:feature))
         (autolith-stage-dependency-names (third specification)))
        ((member (first specification) '(:version))
         (autolith-stage-dependency-names (second specification)))
        ((member (first specification) '(:require))
         nil)
        (t
         (error "Unknown dependency specification ~S." specification))))

(defun autolith-stage-system-closure (names)
  "Return every system reachable from NAMES through ordinary, weak, and
definition-time dependencies."
  (let ((seen (make-hash-table :test #'equal))
        (pending (copy-list names)))
    (loop while pending
          do (let ((name (pop pending)))
               (unless (gethash name seen)
                 (let ((system (asdf:find-system name nil)))
                   (setf (gethash name seen) system)
                   (when system
                     (dolist (specification (append (asdf:system-depends-on system)
                                                    (asdf:system-weakly-depends-on system)
                                                    (asdf:system-defsystem-depends-on system)))
                       (dolist (dependency (autolith-stage-dependency-names specification))
                         (push dependency pending))))))))
    (let ((systems nil))
      (maphash (lambda (name system)
                 (declare (ignore name))
                 (when system (push system systems)))
               seen)
      systems)))

(defun autolith-stage-registry-root (system)
  "Return the top directory of SYSTEM's source below a Nix store, or NIL
for systems SBCL provides and for Autolith itself, which the guest has
from its own contribs and the repository."
  (let* ((directory (asdf:system-source-directory system))
         (parts     (and directory (pathname-directory directory))))
    (when (and parts
               (not (equal (asdf:primary-system-name system) "autolith"))
               (not (uiop:subpathp directory (truename (sb-int:sbcl-homedir-pathname))))
               (equal (subseq parts 0 (min 3 (length parts))) '(:absolute "nix" "store"))
               (>= (length parts) 4))
      (make-pathname :directory (subseq parts 0 4) :defaults directory
                     :name nil :type nil :version nil))))

(defun autolith-stage-copy-tree (source destination)
  "Copy SOURCE's regular files below DESTINATION, skipping excluded types.
Return the number of files copied."
  (let ((count 0))
    (labels ((walk (directory target)
               (dolist (file (uiop:directory-files directory))
                 (unless (member (pathname-type file) *autolith-stage-excluded-types*
                                 :test #'equalp)
                   (let ((output (merge-pathnames (file-namestring file) target)))
                     (ensure-directories-exist output)
                     (uiop:copy-file file output)
                     (incf count))))
               (dolist (child (uiop:subdirectories directory))
                 (walk child (merge-pathnames
                              (make-pathname :directory
                                             (list :relative
                                                   (first (last (pathname-directory child)))))
                              target)))))
      (walk source destination))
    count))

(defun autolith-stage-main (arguments)
  "Stage Autolith and its dependencies below the directory in ARGUMENTS."
  (destructuring-bind (output) arguments
    (let* ((output     (uiop:ensure-directory-pathname (uiop:parse-native-namestring output)))
           (repository (uiop:pathname-parent-directory-pathname
                        (uiop:pathname-parent-directory-pathname
                         (uiop:pathname-directory-pathname *load-truename*))))
           (roots      nil))
      (when (uiop:directory-exists-p output)
        (error "Stage directory ~A already exists." output))
      (asdf:load-asd (merge-pathnames "autolith.asd" repository))
      (dolist (system (autolith-stage-system-closure *autolith-stage-systems*))
        (let ((root (autolith-stage-registry-root system)))
          (when root
            (pushnew root roots :test #'equal))))
      (dolist (root roots)
        (autolith-stage-copy-tree
         root (merge-pathnames (make-pathname :directory
                                              (list :relative "deps"
                                                    (first (last (pathname-directory root)))))
                               output)))
      (dolist (entry *autolith-stage-repository-entries*)
        (let ((source (merge-pathnames entry repository))
              (target (merge-pathnames entry (merge-pathnames "autolith/" output))))
          (if (uiop:directory-pathname-p source)
              (autolith-stage-copy-tree source target)
              (progn (ensure-directories-exist target)
                     (uiop:copy-file source target)))))
      (format t "Staged Autolith and ~D dependency trees in ~A.~%" (length roots) output))))

(autolith-stage-main (uiop:command-line-arguments))
