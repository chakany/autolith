;;;; Build and check Autolith inside rumprun guests on top of the SBCL port.
;;;; Run in the image Dockerfile.sbcl builds, with the tree autolith-stage.lisp
;;;; staged at /build/autolith-stage/ (autolith/ and deps/).
(require :asdf)
(unless (find-package '#:autolith)
  (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

(let ((*features* (cons :sbcl-build-library *features*)))
  (load (merge-pathnames "sbcl-build.lisp" *load-truename*)))

(defparameter *autolith-openssl-url*
  "https://github.com/openssl/openssl/releases/download/openssl-3.6.4/openssl-3.6.4.tar.gz"
  "The pinned OpenSSL release that cl+ssl reaches through the static runtime.")

(defparameter *autolith-openssl-sha256*
  "9bffaa1ad1e07b354c21bd3324ec02fa15579f45a7d0494b3e74bc449b7333ef"
  "The SHA-256 OpenSSL publishes for *AUTOLITH-OPENSSL-URL*.")

(defparameter *autolith-openssl-directory* "/build/openssl-3.6.4/"
  "Where the OpenSSL source is extracted and its static libraries built.")

(defparameter *autolith-openssl-options*
  '("BSD-x86_64" "no-shared" "no-asm" "no-async" "no-dso" "no-module" "no-tests"
    "no-docs" "no-afalgeng" "no-devcryptoeng" "no-dgram" "no-dtls" "no-quic"
    "-D__STDC_NO_ATOMICS__" "--openssldir=/core/openssl")
  "OpenSSL configuration for the guest. The sysroot has no stdatomic.h, so
OpenSSL uses its compiler-builtin atomics. Asynchronous jobs need ucontext,
and the datagram BIO needs ipi_spec_dst, neither of which this NetBSD
provides; Autolith uses neither DTLS nor QUIC.")

(defparameter *autolith-stage-directory* "/build/autolith-stage/"
  "The staged Autolith and dependency sources the guests embed.")

(defparameter *autolith-guest-command* "sbcl 2048"
  "The guest command line: a 2 GiB dynamic space, backed only where used.")

(defun autolith-build-openssl ()
  "Download, verify, and build OpenSSL's static libraries for the guest.
Return the archives in link order."
  (let ((archive "/build/openssl-3.6.4.tar.gz")
        (directory *autolith-openssl-directory*))
    (sbcl-build-command (list "curl" "-fL" "--retry" "5" *autolith-openssl-url* "-o" archive)
                        "/build/" "autolith-01-openssl-download.log")
    (sbcl-build-command
     (list "sh" "-c" (format nil "printf '%s  %s\\n' '~A' '~A' | sha256sum -c -"
                             *autolith-openssl-sha256* archive))
     "/build/" "autolith-02-openssl-verify.log")
    (sbcl-build-command (list "rm" "-rf" directory) "/build/" "autolith-03-openssl-clean.log")
    (sbcl-build-command (list "tar" "-xzf" archive "-C" "/build/") "/build/"
                        "autolith-04-openssl-extract.log")
    (sbcl-build-command
     (append (list "env"
                   "CC=/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc"
                   "AR=/opt/rumprun/bin/x86_64-rumprun-netbsd-ar"
                   "RANLIB=/opt/rumprun/bin/x86_64-rumprun-netbsd-ranlib"
                   "./Configure")
             *autolith-openssl-options*)
     directory "autolith-05-openssl-configure.log")
    (sbcl-build-command (list "make" "-j8" "build_libs") directory
                        "autolith-06-openssl-make.log")
    (list (namestring (merge-pathnames "libssl.a" directory))
          (namestring (merge-pathnames "libcrypto.a" directory)))))

(defun autolith-build-stage-contribs (sbcl-home)
  "Place the guest-built contribs from SBCL-HOME in the stage."
  (let ((target (merge-pathnames "sbcl-home/" *autolith-stage-directory*)))
    (sbcl-build-command (list "rm" "-rf" (namestring target)) "/build/"
                        "autolith-07-stage-clean.log")
    (sbcl-build-command (list "cp" "-R" (string-right-trim "/" sbcl-home)
                              (namestring *autolith-stage-directory*))
                        "/build/" "autolith-08-stage-contribs.log")))

(defun autolith-build-load (libraries warm-core lookups)
  "Load Autolith in a guest booted from WARM-CORE and save the loaded core.
Return the saved core and its lookup manifest."
  (let* ((image    (sbcl-build-sbcl-guest :name "autolith-load" :core warm-core :lookups lookups
                                          :script "/probe/sbcl-autolith.lisp"
                                          :sources "tree" :tree *autolith-stage-directory*
                                          :libraries libraries))
         (exported (sbcl-build-boot-exporting :name "autolith-load" :image image
                                              :destination "/build/autolith-export/"
                                              :command *autolith-guest-command*
                                              :timeout 3600)))
    (unless (equal (sort (copy-list exported) #'string<)
                   '("autolith.core" "foreign-lookups.txt"))
      (error "The Autolith load guest exported ~S." exported))
    (values "/build/autolith-export/autolith.core"
            "/build/autolith-export/foreign-lookups.txt")))

(defun autolith-build-tests (libraries core lookups)
  "Run Autolith's suites in a guest booted from the loaded CORE and return
the summary line the guest printed."
  (let ((image (sbcl-build-sbcl-guest :name "autolith-tests" :core core :lookups lookups
                                      :script "/probe/sbcl-autolith-tests.lisp"
                                      :sources "tree" :tree *autolith-stage-directory*
                                      :libraries libraries)))
    (multiple-value-bind (status log-path)
        (sbcl-build-command
         (list "timeout" "14400" "qemu-system-x86_64"
               "-machine" "pc,accel=tcg" "-cpu" "qemu64" "-m" "3072"
               "-net" "none" "-vga" "none" "-display" "none" "-serial" "stdio"
               "-monitor" "none" "-no-reboot" "-kernel" image
               "-append" (format nil "{\"cmdline\":\"~A\"}" *autolith-guest-command*)
               "-device" "isa-debug-exit,iobase=0xf4,iosize=0x04")
         "/build/" "autolith-tests-boot.log" :accepted-statuses '(1))
      (declare (ignore status))
      (sbcl-build-validate-guest log-path '((:prefix "AUTOLITH-TESTS-DONE ")
                                            "SBCL-GUEST-EXIT 0"))
      (let ((text (uiop:read-file-string log-path)))
        (subseq text (search "AUTOLITH-TESTS-DONE " text)
                (position #\Newline text :start (search "AUTOLITH-TESTS-DONE " text)))))))

(defun autolith-build-main ()
  "Build OpenSSL, load and save Autolith in a guest, and run its suites."
  (let ((libraries (autolith-build-openssl)))
    (autolith-build-stage-contribs "/build/contrib-export/sbcl-home/")
    (multiple-value-bind (core lookups)
        (autolith-build-load libraries "/build/warm-export/sbcl.core"
                             "/build/contrib-export/foreign-lookups.txt")
      (format t "~&~A~%" (autolith-build-tests libraries core lookups)))))

(autolith-build-main)
