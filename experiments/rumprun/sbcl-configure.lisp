;;;; Cross configuration for the pinned SBCL 2.6.6 rumprun experiment.
;;;; Run inside the build container: sbcl --script sbcl-configure.lisp SOURCE

(require :asdf)
(unless (find-package '#:autolith)
  (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

(defun rump-sbcl-write (root name content)
  "Write one generated build input beneath ROOT."
  (let ((path (merge-pathnames name root)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction ':output :if-exists ':supersede)
      (write-string content stream))))

(defun rump-sbcl-configure (root)
  "Prepare a fresh SBCL 2.6.6 tree without executing target probes or Git."
  (let* ((root (uiop:ensure-directory-pathname root))
         (backend (uiop:read-file-string
                   (merge-pathnames "crossbuild-runner/backends/x86-64/features" root)))
         (version (uiop:read-file-string (merge-pathnames "version.lisp-expr" root))))
    (assert (search "2.6.6" version))
    (rump-sbcl-write
     root "local-target-features.lisp-expr"
     (format nil "(lambda (features)~%  (set-difference~%   (union features '(:x86-64 :unix :bsd :netbsd :elf :little-endian~%                     :rumprun :os-provides-blksize-t :os-provides-suseconds-t~%                     :os-provides-clock-gettime~%                     :os-provides-dlopen :sb-simd-pack :sb-simd-pack-256 :avx2~%                     :sb-thread :sb-safepoint~%                     ~A))~%   '(:sb-futex :immobile-space :immobile-code :sb-core-compression)))~%" backend))
    (rump-sbcl-write root "output/build-config"
                     (format nil "GNUMAKE=make; export GNUMAKE~%SBCL_XC_HOST='sbcl --noinform --no-sysinit --no-userinit --disable-debugger'; export SBCL_XC_HOST~%android=false; export android~%"))
    (rump-sbcl-write root "output/prefix.def" "SBCL_PREFIX=/opt/sbcl-rumprun")
    (rump-sbcl-write root "output/dynamic-space-size.txt" "128")
    (rump-sbcl-write root "output/build-id.inc" "\"sbcl-2.6.6-rumprun-experiment\"")
    (dolist (pair '(("x86-64-arch.h" "target-arch.h")
                    ("x86-64-lispregs.h" "target-lispregs.h")
                    ("x86-64-bsd-os.h" "target-arch-os.h")
                    ("bsd-os.h" "target-os.h")))
      (uiop:run-program (list "ln" "-sf" (first pair) (second pair))
                        :directory (merge-pathnames "src/runtime/" root)))
    (uiop:run-program (list "rm" "-f" "Config")
                      :directory (merge-pathnames "src/runtime/" root))
    (rump-sbcl-write
     root "src/runtime/Config"
     (format nil "include Config.x86-64-bsd~%OS_SRC = bsd-os.c x86-64-bsd-os.c rumprun-os.c~%OS_LIBS =~%CC = /opt/rumprun/bin/x86_64-rumprun-netbsd-gcc~%LINKFLAGS =~%"))
    (format t "Configured ~A for threaded x86-64 NetBSD rumprun with safepoints.~%" root)))

(rump-sbcl-configure (or (first (uiop:command-line-arguments))
                         (error "Supply the disposable SBCL source directory.")))
