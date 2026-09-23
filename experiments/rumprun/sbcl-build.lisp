;;;; Repeatable SBCL 2.6.6 rumprun build driver from pinned source inputs.

(require :asdf)

(unless (find-package '#:autolith)
  (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

(defparameter *sbcl-source-url*
  "https://downloads.sourceforge.net/project/sbcl/sbcl/2.6.6/sbcl-2.6.6-source.tar.bz2")
(defparameter *sbcl-source-sha256*
  "a65a7a30812aaf54925d1192b9b9e810f527c79911c6000b7548105aef7da34b")
(defparameter *sbcl-source-directory* "/build/sbcl-2.6.6/")
(defparameter *sbcl-log-directory* "/build/sbcl-logs/")

(defun sbcl-build-command (arguments directory log-name
                            &key (accepted-statuses '(0)))
  "Run ARGUMENTS in DIRECTORY, logging output and accepting ACCEPTED-STATUSES."
  (let ((log-path (merge-pathnames log-name
                                   (uiop:ensure-directory-pathname
                                    *sbcl-log-directory*))))
    (ensure-directories-exist log-path)
    (format t "~&[sbcl-build] ~{~A~^ ~}~%" arguments)
    (with-open-file (log log-path :direction ':output :if-exists ':supersede)
      (multiple-value-bind (ignored-output ignored-error status)
          (uiop:run-program arguments
                             :directory directory
                             :output log
                             :error-output :output
                             :ignore-error-status t)
        (declare (ignore ignored-output ignored-error))
        (unless (member status accepted-statuses :test #'eql)
          (error "Command failed with status ~A; see ~A" status log-path))
        (values status log-path)))))

(defun sbcl-build-download-source (archive)
  "Download and verify the pinned SBCL source archive."
  (sbcl-build-command
   (list "curl" "-fL" "--retry" "5"
         *sbcl-source-url* "-o" (namestring archive))
   "/build/" "01-download.log")
  (sbcl-build-command
   (list "sh" "-c"
         (format nil "printf '%s  %s\\n' '~A' '~A' | sha256sum -c -"
                 *sbcl-source-sha256* (namestring archive)))
   "/build/" "02-verify-source.log"))

(defun sbcl-build-extract-source (archive)
  "Extract the verified archive into the disposable SBCL source directory."
  (sbcl-build-command
   (list "rm" "-rf" (namestring (uiop:ensure-directory-pathname
                                  *sbcl-source-directory*)))
   "/build/" "03-clean-source.log")
  (sbcl-build-command
   (list "mkdir" "-p" (namestring (uiop:ensure-directory-pathname
                                     *sbcl-source-directory*)))
   "/build/" "04-mkdir-source.log")
  (sbcl-build-command
   (list "tar" "-xjf" (namestring archive) "--strip-components=1"
         "-C" (namestring (uiop:ensure-directory-pathname
                            *sbcl-source-directory*)))
   "/build/" "05-extract-source.log"))

(defun sbcl-build-run-script (name)
  "Run one standalone build adaptation script against the source tree."
  (sbcl-build-command
   (list "sbcl" "--noinform" "--no-userinit" "--no-sysinit"
         "--disable-debugger" "--script"
         (namestring (merge-pathnames name "/probe/"))
         (namestring (uiop:ensure-directory-pathname
                      *sbcl-source-directory*)))
   "/build/" (format nil "~A.log" name)))

(defun sbcl-build-read-forms (text)
  "Read all generated forms in TEXT without evaluating them."
  (let ((*read-eval* nil)
        (*package* (find-package '#:autolith))
        (stream (make-string-input-stream text))
        (end (gensym "END"))
        (forms nil))
    (handler-case
        (loop for form = (read stream nil end)
              until (eq form end)
              do (push form forms))
      (error (condition)
        (error "Generated grovel Lisp is unreadable: ~A" condition)))
    (nreverse forms)))

(defun sbcl-build-generated-lisp (text end)
  "Extract the complete generated region and validate every readable form."
  (let* ((header ";;;; This is an automatically generated file, please do not hand-edit it.")
         (start (or (search header text :end2 end)
                    (error "Grovel header missing.")))
         (candidate (remove #\Return (subseq text start end)))
         (forms (sbcl-build-read-forms candidate)))
    (unless (and (= (length forms) 133)
                 (equal (first forms) '(in-package "SB-ALIEN"))
                 (equal (first (last forms))
                        '(define-alien-type os-vm-size-t (unsigned 64))))
      (error "Unexpected target header definitions."))
    candidate))

(defun sbcl-build-extract-grovel (log-path output-path)
  "Extract and validate grovel's generated Lisp from LOG-PATH."
  (let* ((text (uiop:read-file-string log-path))
         (marker "=== main() of \"grovel\" returned 0 ===")
         (end (or (search marker text)
                  (error "Grovel success marker missing from ~A" log-path)))
         (generated (sbcl-build-generated-lisp text end)))
    (with-open-file (stream output-path :direction ':output :if-exists ':supersede)
      (write-string generated stream))
    (format t "Extracted ~D bytes of grovel Lisp to ~A.~%"
            (length generated) output-path)))

(defun sbcl-build-grovel ()
  "Cross-build, boot, and capture the header-grovel executable."
  (let ((root (uiop:ensure-directory-pathname *sbcl-source-directory*)))
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed"
           "/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc"
           "-I" "src/runtime" "tools-for-build/grovel-headers.c"
           "-o" "/build/grovel") root "07-grovel-compile.log")
    (sbcl-build-command
     (list "/opt/rumprun/bin/rumprun-bake" "hw_generic"
           "/build/grovel.bin" "/build/grovel") root "08-grovel-bake.log")
    (sbcl-build-command
     (list "timeout" "10" "qemu-system-x86_64" "-machine" "pc,accel=tcg"
           "-cpu" "qemu64" "-m" "256" "-net" "none" "-vga" "none"
           "-display" "none" "-serial" "stdio" "-monitor" "none"
           "-no-reboot" "-kernel" "/build/grovel.bin"
           "-append" "{\"cmdline\":\"grovel\"}")
     "/build/" "09-grovel-boot.log"
     :accepted-statuses '(0 124))
    (sbcl-build-extract-grovel
     "/build/sbcl-logs/09-grovel-boot.log"
     (merge-pathnames "output/stuff-groveled-from-headers.lisp" root))))

(defun sbcl-build-validate-guest (log-path markers)
  "Require complete output lines or explicitly marked prefixes in LOG-PATH."
  (let ((lines (uiop:split-string (remove #\Return (uiop:read-file-string log-path))
                                  :separator '(#\Newline))))
    (dolist (marker markers)
      (unless (some (lambda (line)
                      (if (consp marker)
                          (uiop:string-prefix-p (second marker) line)
                          (string= marker line)))
                    lines)
        (error "Guest marker ~S missing from ~A" marker log-path)))))

(defun sbcl-build-boot (&key image command log-name (memory 256) (timeout 25)
                            debug-exit markers)
  "Boot a guest and validate both QEMU status and guest completion evidence."
  (multiple-value-bind (status log-path)
      (sbcl-build-command
       (append (list "timeout" (write-to-string timeout) "qemu-system-x86_64"
                     "-machine" "pc,accel=tcg" "-cpu" "qemu64"
                     "-m" (write-to-string memory) "-net" "none" "-vga" "none"
                     "-display" "none" "-serial" "stdio" "-monitor" "none"
                     "-no-reboot" "-kernel" image
                     "-append" (format nil "{\"cmdline\":\"~A\"}" command))
               (when debug-exit
                 '("-device" "isa-debug-exit,iobase=0xf4,iosize=0x04")))
       "/build/" log-name :accepted-statuses (if debug-exit '(1) '(0 124)))
    (declare (ignore status))
    (sbcl-build-validate-guest
     log-path
     (append markers
             (unless debug-exit
               (list (format nil "=== main() of ~S returned 0 ===" command)))))))

(defun sbcl-build-clock-fixtures ()
  "Compare native and wrapped guest clocks, asserting monotonic wrapped time."
  (dolist (wrapped '(nil t))
    (let* ((name (if wrapped "clock-fixed" "clock-native"))
           (executable (concatenate 'string "/probe/" name))
           (image (concatenate 'string executable ".bin")))
      (sbcl-build-command
       (append '("env" "RUMPRUN_STUBLINK=succeed"
                 "/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc"
                 "-Wall" "-Wextra" "-Werror" "-I/build/rumprun/include"
                 "/probe/sbcl-clock-test.c")
               (when wrapped
                 '("-DREQUIRE_MONOTONIC" "/probe/sbcl-clock.c"
                   "-Wl,--wrap=__clock_gettime50,--wrap=__gettimeofday50"))
               (list "-o" executable))
       "/build/" (format nil "~A-compile.log" name))
      (sbcl-build-command
       (list "/opt/rumprun/bin/rumprun-bake" "hw_generic" image executable)
       "/build/" (format nil "~A-bake.log" name))
      (sbcl-build-boot :image image :command name
                       :log-name (format nil "~A-boot.log" name)
                       :markers '((:prefix "CLOCK-PROBE: "))))))

(defun sbcl-build-main ()
  "Build and boot the disposable SBCL port, machine fixture, and clock probes."
  (let ((archive "/build/sbcl-2.6.6-source.tar.bz2"))
    (sbcl-build-download-source archive)
    (sbcl-build-extract-source archive)
    (sbcl-build-run-script "sbcl-configure.lisp")
    (sbcl-build-run-script "sbcl-runtime.lisp")
    (sbcl-build-command
     (list "sh" "make-host-1.sh") *sbcl-source-directory* "06-make-host-1.log")
    (sbcl-build-grovel)
    (sbcl-build-command
     (list "sh" "make-host-2.sh") *sbcl-source-directory* "10-make-host-2.log")
    (sbcl-build-run-script "sbcl-link.lisp")
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed" "make" "-j4" "-C"
           "src/runtime" "sbcl") *sbcl-source-directory* "11-runtime-link.log")
    (sbcl-build-command
     (list "/opt/rumprun/bin/rumprun-bake" "hw_generic"
           "/probe/sbcl.bin" "src/runtime/sbcl") *sbcl-source-directory*
     "12-sbcl-bake.log")
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed"
           "/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc" "-O2" "-Wall"
           "-Wextra" "-Werror" "-I" "src/runtime"
           "-Wl,--wrap=__sigaction14,--wrap=__sigprocmask14,--wrap=__sigaltstack14,--wrap=posix_memalign,--wrap=free"
           "/probe/sbcl-machine-test.c" "src/runtime/sbcl-machine.c"
           "src/runtime/sbcl-traps.S" "-o" "/probe/sbcl-machine-test")
     *sbcl-source-directory* "13-machine-test-compile.log")
    (sbcl-build-command
     (list "/opt/rumprun/bin/rumprun-bake" "hw_generic"
           "/probe/sbcl-machine-test.bin" "/probe/sbcl-machine-test")
     *sbcl-source-directory* "14-machine-test-bake.log")
    (sbcl-build-boot :image "/probe/sbcl-machine-test.bin" :command "machine-test"
                     :log-name "15-machine-test-boot.log"
                     :markers '((:prefix "MACHINE-OK:")))
    (sbcl-build-clock-fixtures)
    (sbcl-build-boot :image "/probe/sbcl.bin" :command "sbcl"
                     :log-name "16-sbcl-smoke-boot.log" :memory 3072 :timeout 600
                     :debug-exit t
                     :markers '("LISP-SMOKE-OK 11 checks" "SBCL-GUEST-EXIT 0"))
    (format t "SBCL rumprun build and guest checks completed.~%")))

(unless (member :sbcl-build-library *features*)
  (sbcl-build-main))
