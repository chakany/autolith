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
    ;; Signal only after the log is closed: unwinding out of WITH-OPEN-FILE
    ;; aborts the stream, which deletes a newly written log.
    (let ((status (with-open-file (log log-path :direction ':output :if-exists ':supersede)
                    (nth-value 2 (uiop:run-program arguments
                                                   :directory           directory
                                                   :output              log
                                                   :error-output        ':output
                                                   :ignore-error-status t)))))
      (unless (member status accepted-statuses :test #'eql)
        (sbcl-build-print-log-tail log-path)
        (error "Command failed with status ~A; see ~A" status log-path))
      (values status log-path))))

(defparameter *sbcl-log-tail-lines* 40
  "How many final lines of a failed command's log to print, so that a failure
inside an image build, whose logs are discarded with the layer, can be
diagnosed.")

(defun sbcl-build-print-log-tail (log-path)
  "Print the last *SBCL-LOG-TAIL-LINES* lines of LOG-PATH."
  (let ((lines (uiop:split-string (uiop:read-file-string log-path :external-format ':latin-1)
                                  :separator '(#\Newline))))
    (format t "~&[sbcl-build] End of ~A:~%~{~A~%~}" log-path
            (last lines *sbcl-log-tail-lines*))
    (finish-output)))

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

(defun sbcl-build-run-script (name &key arguments (log-name (format nil "~A.log" name)))
  "Run one standalone build script against the source tree with ARGUMENTS."
  (sbcl-build-command
   (append (list "sbcl" "--noinform" "--no-userinit" "--no-sysinit"
                 "--disable-debugger" "--script"
                 (namestring (merge-pathnames name "/probe/"))
                 (namestring (uiop:ensure-directory-pathname
                              *sbcl-source-directory*)))
           arguments)
   "/build/" log-name))

(defparameter *sbcl-final-wraps* '("___lwp_park60" "__fork" "__vfork14" "kill" "_sys___wait450")
  "Symbols wrapped in the final unikernel link, where rumprun's libraries
reference or define them. Other wraps apply earlier, to the runtime's own
objects.")

(defparameter *sbcl-bake-specs* "/opt/rumprun/rumprun-x86_64/lib/rumprun-hw/specs-bake"
  "The GCC specs rumprun-bake links unikernel images with.")

(defparameter *sbcl-bake-linker-script* "/opt/rumprun/rumprun-x86_64/lib/hw.ldscript"
  "The linker script those specs name.")

(defparameter *sbcl-tls-alignment* 16
  "The TLS alignment bmk's allocator guarantees for a thread's TLS area.")

(defparameter *sbcl-tls-sections*
  '(("	.tdata : {
		_tdata_start = . ;
		*(.tdata)
		_tdata_end = . ;
	}" . "	.tdata : ALIGN(16) {
		_tdata_start = . ;
		*(.tdata .tdata.*)
		. = ALIGN(16);
		_tdata_end = . ;
	}")
    ("	.tbss : {
		_tbss_start = . ;
		*(.tbss)
		_tbss_end = . ;
	}" . "	.tbss : ALIGN(16) {
		_tbss_start = . ;
		*(.tbss .tbss.*)
		. = ALIGN(16);
		_tbss_end = . ;
	}"))
  "Rumprun's TLS sections and their replacements. bmk sizes each thread's
TLS area as the distance between these bounds and places it directly below
the thread control block. The linker addresses TLS below that block by the
segment's size rounded to its alignment, so any padding outside the bounds,
such as between .tdata and a more aligned .tbss, shifts every thread-local
variable and lets the lowest one overwrite the area's allocation header.
Aligning both sections and bounds makes the measured size the segment's.")

(defun sbcl-build-replace-exactly (text before after name)
  "Return TEXT with its single occurrence of BEFORE replaced by AFTER."
  (let ((position (search before text)))
    (unless (and position (not (search before text :start2 (1+ position))))
      (error "Expected exactly one ~A fragment to replace." name))
    (concatenate 'string (subseq text 0 position) after
                 (subseq text (+ position (length before))))))

(defun sbcl-build-tls-specs ()
  "Write the corrected linker script, and specs naming it, to
/build/rumprun-link/ and return the specs path."
  (let* ((directory (uiop:ensure-directory-pathname "/build/rumprun-link/"))
         (script    (merge-pathnames "hw.ldscript" directory))
         (specs     (merge-pathnames "specs-bake" directory))
         (text      (uiop:read-file-string *sbcl-bake-linker-script*)))
    (ensure-directories-exist directory)
    (loop for (before . after) in *sbcl-tls-sections*
          do (setf text (sbcl-build-replace-exactly text before after "TLS section")))
    (with-open-file (out script :direction ':output :if-exists ':supersede)
      (write-string text out))
    (with-open-file (out specs :direction ':output :if-exists ':supersede)
      (write-string (sbcl-build-replace-exactly
                     (uiop:read-file-string *sbcl-bake-specs*)
                     (format nil "-T ~A" *sbcl-bake-linker-script*)
                     (format nil "-T ~A" (namestring script))
                     "linker script")
                    out))
    (namestring specs)))

(defun sbcl-build-symbol-address (image name)
  "Return the address of symbol NAME in IMAGE."
  (let ((line (find-if (lambda (line) (uiop:string-suffix-p line (concatenate 'string " " name)))
                       (uiop:split-string
                        (uiop:run-program (list "/opt/rumprun/bin/x86_64-rumprun-netbsd-nm" image)
                                          :output ':string)
                        :separator '(#\Newline)))))
    (unless line
      (error "~A defines no ~A." image name))
    (parse-integer line :end (position #\Space line) :radix 16)))

(defun sbcl-build-check-tls (image)
  "Fail unless bmk's TLS bounds in IMAGE span its TLS segment exactly, with
no gap between .tdata and .tbss, and the segment's size is a multiple of an
alignment bmk's TLS areas provide, since the linker rounds it up to that."
  (let* ((headers (uiop:run-program (list "/opt/rumprun/bin/x86_64-rumprun-netbsd-readelf"
                                          "-lW" image)
                                    :output ':string))
         (line    (find-if (lambda (line) (uiop:string-prefix-p "TLS" (string-left-trim " " line)))
                           (uiop:split-string headers :separator '(#\Newline))))
         (fields  (and line (remove "" (uiop:split-string line :separator '(#\Space))
                                    :test #'string=)))
         (memory  (and fields (parse-integer (sixth fields) :start 2 :radix 16)))
         (align   (and fields (parse-integer (first (last fields)) :start 2 :radix 16)))
         (start   (sbcl-build-symbol-address image "_tdata_start"))
         (end     (sbcl-build-symbol-address image "_tbss_end")))
    (unless (and memory align (<= align *sbcl-tls-alignment*)
                 (zerop (mod memory align))
                 (= memory (- end start))
                 (= (sbcl-build-symbol-address image "_tdata_end")
                    (sbcl-build-symbol-address image "_tbss_start")))
      (error "~A has a TLS segment of ~A bytes at alignment ~A that bmk's TLS bounds, ~A bytes from _tdata_start to _tbss_end, do not lay out exactly."
             image memory align (- end start)))
    image))

(defun sbcl-build-bake (image binary log-name)
  "Bake BINARY into the unikernel IMAGE as rumprun-bake does, adding
*SBCL-FINAL-WRAPS* to its final link, which rumprun-bake cannot extend, and
linking with the corrected TLS layout, which the result is checked for."
  (let* ((plan     (uiop:run-program (list "/opt/rumprun/bin/rumprun-bake" "-n" "hw_generic"
                                           image binary)
                                     :output ':string :error-output nil))
         (commands (remove-if-not (lambda (line) (uiop:string-prefix-p "/opt/rumprun/" line))
                                  (uiop:split-string plan :separator '(#\Newline))))
         (object   (let* ((copy (first commands))
                          (end  (length copy)))
                     (subseq copy (1+ (position #\Space copy :from-end t :end end))))))
    (unless (and (= (length commands) 2) (search "--redefine-sym main=" (first commands)))
      (error "Unexpected rumprun-bake plan for ~A:~%~A" binary plan))
    (sbcl-build-command (list "mkdir" "-p" (directory-namestring object)) "/build/"
                        (format nil "~A-tmp.log" log-name))
    (sbcl-build-command (list "sh" "-c" (first commands)) "/build/"
                        (format nil "~A-copy.log" log-name))
    (sbcl-build-command (list "sh" "-c"
                              (format nil "~A~{ -Wl,--wrap=~A~}"
                                      (sbcl-build-replace-exactly
                                       (second commands)
                                       (format nil "-specs=~A" *sbcl-bake-specs*)
                                       (format nil "-specs=~A" (sbcl-build-tls-specs))
                                       "bake specs")
                                      *sbcl-final-wraps*))
                        "/build/" log-name)
    (sbcl-build-check-tls image)
    (sbcl-build-command (list "rm" "-rf" (directory-namestring object)) "/build/"
                        (format nil "~A-cleanup.log" log-name))
    image))

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
     (merge-pathnames "09-grovel-boot.log" (uiop:ensure-directory-pathname *sbcl-log-directory*))
     (merge-pathnames "output/stuff-groveled-from-headers.lisp" root))))

(defun sbcl-build-extract-grovel-program (log-path output-path)
  "Write the Lisp printed between the single SBCL-GROVEL-BEGIN and
SBCL-GROVEL-END lines of a successful grovel guest's LOG-PATH to OUTPUT-PATH."
  (let* ((text  (remove #\Return (uiop:read-file-string log-path)))
         (begin (format nil "~%SBCL-GROVEL-BEGIN~%"))
         (end   (format nil "~%SBCL-GROVEL-END~%"))
         (start (search begin text))
         (stop  (and start (search end text :start2 (+ start (length begin))))))
    (unless (and start stop
                 (not (search begin text :start2 (1+ start)))
                 (not (search end text :start2 (1+ stop)))
                 (search "=== main() of \"grovel\" returned 0 ===" text :start2 stop))
      (error "Grovel guest output in ~A is incomplete or ambiguous." log-path))
    (with-open-file (stream output-path :direction ':output :if-exists ':supersede)
      (write-string (subseq text (+ start (length begin)) (1+ stop)) stream))
    (format t "Extracted grovel constants to ~A.~%" output-path)))

(defun sbcl-build-read-export-line (stream)
  "Read one newline-terminated ASCII line of at most 1100 bytes from STREAM."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte)
                    (error "Guest export ended inside a header line."))
                   ((= byte 10)
                    (return (map 'string #'code-char bytes)))
                   ((or (< byte 32) (> byte 126) (>= (length bytes) 1100))
                    (error "Guest export header line is malformed."))
                   (t
                    (vector-push-extend byte bytes))))))

(defun sbcl-build-export-path-p (path)
  "Return T when PATH is a relative path of components made of letters,
digits, '.', '-', and '_', none of which begins with a dot."
  (let ((components (uiop:split-string path :separator '(#\/))))
    (if (every (lambda (component)
                 (and (plusp (length component))
                      (char/= (char component 0) #\.)
                      (every (lambda (character)
                               (or (alphanumericp character) (find character ".-_")))
                             component)))
               components)
        t
        nil)))

(defun sbcl-build-copy-export-file (stream output length)
  "Copy LENGTH bytes from STREAM to OUTPUT and return their FNV-1a 64-bit hash."
  (declare (type (unsigned-byte 62) length) (optimize (speed 3)))
  (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8)))
        (hash #xcbf29ce484222325))
    (declare (type (unsigned-byte 64) hash))
    (loop while (plusp length)
          do (let* ((wanted (min length (length buffer)))
                    (count  (read-sequence buffer stream :end wanted)))
               (declare (type fixnum wanted count))
               (unless (= count wanted)
                 (error "Guest export ended inside a file."))
               (dotimes (index count)
                 (setf hash (ldb (byte 64 0)
                                 (* (logxor hash (aref buffer index)) #x100000001b3))))
               (write-sequence buffer output :end count)
               (decf length count)))
    hash))

(defun sbcl-build-extract-exports (capture destination)
  "Unpack the guest export stream in CAPTURE below DESTINATION, verifying each
file's framing, length, and FNV-1a hash. Return the exported relative paths."
  (let ((paths nil)
        (root  (uiop:ensure-directory-pathname destination)))
    (with-open-file (stream capture :element-type '(unsigned-byte 8))
      (unless (string= (sbcl-build-read-export-line stream) "RUMPRUN-EXPORT 1")
        (error "Guest export header missing from ~A." capture))
      (loop
        (let* ((line   (sbcl-build-read-export-line stream))
               (fields (uiop:split-string line :separator '(#\Space))))
          (when (string= line "END")
            (unless (null (read-byte stream nil nil))
              (error "Guest export has data after END."))
            (return))
          (destructuring-bind (tag path length hash) fields
            (let ((length (parse-integer length))
                  (hash   (parse-integer hash :radix 16)))
              (unless (and (string= tag "FILE")
                           (sbcl-build-export-path-p path)
                           (not (member path paths :test #'string=))
                           (<= 0 length (1- (expt 2 62))))
                (error "Guest export record is malformed: ~A" line))
              (let ((output-path (merge-pathnames path root)))
                (ensure-directories-exist output-path)
                (with-open-file (output output-path :direction ':output
                                                    :if-exists ':supersede
                                                    :element-type '(unsigned-byte 8))
                  (unless (= (sbcl-build-copy-export-file stream output length) hash)
                    (error "Guest export hash mismatch for ~A." path))))
              (unless (eql (read-byte stream nil nil) 10)
                (error "Guest export record for ~A is unterminated." path))
              (push path paths))))))
    (format t "Extracted ~D guest files from ~A.~%" (length paths) capture)
    (nreverse paths)))

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
                            debug-exit export markers)
  "Boot a guest and validate both QEMU status and guest completion evidence.
EXPORT names the host file that receives the guest's export stream."
  (multiple-value-bind (status log-path)
      (sbcl-build-command
       (append (list "timeout" (write-to-string timeout) "qemu-system-x86_64"
                     "-machine" "pc,accel=tcg" "-cpu" "qemu64"
                     "-m" (write-to-string memory) "-net" "none" "-vga" "none"
                     "-display" "none" "-serial" "stdio" "-monitor" "none"
                     "-no-reboot" "-kernel" image
                     "-append" (format nil "{\"cmdline\":\"~A\"}" command))
               (when debug-exit
                 '("-device" "isa-debug-exit,iobase=0xf4,iosize=0x04"))
               (when export
                 (list "-debugcon" (format nil "file:~A" export))))
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

(defun sbcl-build-sbcl-guest (&key name core script sources tree lookups libraries)
  "Link, compile, and bake the SBCL guest NAME embedding CORE, SCRIPT, and
SOURCES (\"warm\", \"contrib\", \"tree\" with TREE, or \"none\"). LOOKUPS
is the foreign lookup manifest of the guest that saved or last booted CORE,
if any. LIBRARIES are static archives the runtime links, in order. Return
the guest image path."
  (let ((image (format nil "/probe/~A.bin" name)))
    (sbcl-build-run-script "sbcl-link.lisp"
                           :arguments (append (list "--core" core "--script" script
                                                    "--sources" sources)
                                              (and tree (list "--tree" tree))
                                              (and lookups (list "--lookups" lookups))
                                              (loop for library in libraries
                                                    append (list "--library" library)))
                           :log-name (format nil "~A-link.log" name))
    (sbcl-build-command
     (list "rm" "-f" "src/runtime/sbcl") *sbcl-source-directory*
     (format nil "~A-clean.log" name))
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed" "make" "-j4" "-C"
           "src/runtime" "sbcl") *sbcl-source-directory*
     (format nil "~A-runtime.log" name))
    (sbcl-build-bake image (namestring (merge-pathnames "src/runtime/sbcl" *sbcl-source-directory*))
                     (format nil "~A-bake.log" name))))

(defun sbcl-build-boot-exporting (&key name image destination (command "sbcl") (timeout 900))
  "Boot the SBCL guest IMAGE with guest COMMAND, allowing TIMEOUT seconds,
require success, and unpack its exports below DESTINATION. Return the
exported relative paths."
  (let ((capture (format nil "/build/~A-export.bin" name)))
    (uiop:delete-file-if-exists capture)
    (sbcl-build-boot :image image :command command
                     :log-name (format nil "~A-boot.log" name)
                     :memory 3072 :timeout timeout :debug-exit t :export capture
                     :markers '("SBCL-GUEST-EXIT 0"))
    (sbcl-build-extract-exports capture destination)))

(defun sbcl-build-warm-core ()
  "Build SBCL's warm core inside guests, as make-target-2.sh does natively:
compile the warm sources in one guest, then load those fasls into a fresh
cold core in a second guest and save it. Return the warm core path and the
foreign lookup manifest of the guest that saved it as two values."
  (let* ((root   *sbcl-source-directory*)
         (cold   (namestring (merge-pathnames "output/cold-sbcl.core" root)))
         (fasls  (sbcl-build-boot-exporting
                  :name "warm-compile"
                  :image (sbcl-build-sbcl-guest :name "warm-compile" :core cold
                                                :script "/probe/sbcl-warm-compile.lisp"
                                                :sources "warm")
                  :destination "/build/warm-compile-export/")))
    (unless (and (member "foreign-lookups.txt" fasls :test #'string=)
                 (every (lambda (path)
                          (or (string= path "foreign-lookups.txt")
                              (uiop:string-prefix-p "obj/from-self/" path)))
                        fasls)
                 (> (length fasls) 1))
      (error "The warm compilation guest exported unexpected files: ~S" fasls))
    (sbcl-build-command (list "rm" "-rf" "obj/from-self") root "warm-fasls-clean.log")
    (sbcl-build-command (list "cp" "-R" "/build/warm-compile-export/obj" ".") root
                        "warm-fasls-install.log")
    (let ((exports (sbcl-build-boot-exporting
                    :name "warm-load"
                    :image (sbcl-build-sbcl-guest :name "warm-load" :core cold
                                                  :script "/probe/sbcl-warm.lisp"
                                                  :sources "warm")
                    :destination "/build/warm-export/")))
      (unless (equal (sort (copy-list exports) #'string<)
                     '("foreign-lookups.txt" "sbcl.core"))
        (error "The warm load guest exported unexpected files: ~S" exports))
      (values "/build/warm-export/sbcl.core" "/build/warm-export/foreign-lookups.txt"))))

(defparameter *sbcl-contribs*
  '("sb-posix" "sb-bsd-sockets" "sb-rotate-byte" "sb-cltl2" "sb-introspect")
  "Contribs sbcl-contribs.lisp builds; the first two grovel C constants.")

(defun sbcl-build-grovel-contrib (system program)
  "Compile the grovel C PROGRAM of SYSTEM, run it as a guest, and install its
constants where the contrib build guest embeds them."
  (let* ((root   (uiop:ensure-directory-pathname *sbcl-source-directory*))
         (object (format nil "/build/~A-grovel.o" system))
         (binary (format nil "/build/~A-grovel" system))
         (image  (format nil "/build/~A-grovel.bin" system)))
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed" "/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc"
           "-Dmain=sbcl_grovel_main" "-c" program "-o" object)
     "/build/" (format nil "~A-grovel-compile.log" system))
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed" "/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc"
           "-Wall" "-Wextra" "-Werror" "/probe/sbcl-grovel-main.c" object "-o" binary)
     "/build/" (format nil "~A-grovel-link.log" system))
    (sbcl-build-command
     (list "/opt/rumprun/bin/rumprun-bake" "hw_generic" image binary)
     "/build/" (format nil "~A-grovel-bake.log" system))
    (sbcl-build-boot :image image :command "grovel"
                     :log-name (format nil "~A-grovel-boot.log" system)
                     :markers '("SBCL-GROVEL-END"))
    (let ((output (merge-pathnames (format nil "rumprun-grovel/~A.lisp" system) root)))
      (ensure-directories-exist output)
      (sbcl-build-extract-grovel-program
       (merge-pathnames (format nil "~A-grovel-boot.log" system)
                        (uiop:ensure-directory-pathname *sbcl-log-directory*))
       output))))

(defun sbcl-build-contribs (warm-core lookups)
  "Build the contribs inside guests booted from WARM-CORE, whose saving guest
recorded LOOKUPS: one guest prints the grovel programs, the host runs them as
guests, and another guest compiles the contribs against their constants.
Return the exported SBCL_HOME and the building guest's lookup manifest as
two values."
  (let ((root (uiop:ensure-directory-pathname *sbcl-source-directory*)))
    (flet ((guest (name)
             (sbcl-build-sbcl-guest :name name :core warm-core :lookups lookups
                                    :script "/probe/sbcl-contribs.lisp"
                                    :sources "contrib")))
      (sbcl-build-command (list "rm" "-rf" "rumprun-grovel") root "grovel-clean.log")
      (let ((programs (sbcl-build-boot-exporting :name "contrib-c" :image (guest "contrib-c")
                                                 :destination "/build/contrib-c-export/")))
        (unless (equal (sort (copy-list programs) #'string<)
                       '("foreign-lookups.txt" "grovel/sb-bsd-sockets.c" "grovel/sb-posix.c"))
          (error "The contrib grovel guest exported unexpected files: ~S" programs))
        (dolist (system '("sb-posix" "sb-bsd-sockets"))
          (sbcl-build-grovel-contrib
           system (format nil "/build/contrib-c-export/grovel/~A.c" system))))
      (let ((built    (sbcl-build-boot-exporting :name "contrib-build"
                                                 :image (guest "contrib-build")
                                                 :destination "/build/contrib-export/"))
            (expected (append '("foreign-lookups.txt" "sbcl-home/contrib/asdf.fasl"
                                "sbcl-home/contrib/uiop.fasl")
                              (loop for system in *sbcl-contribs*
                                    append (list (format nil "sbcl-home/contrib/~A.asd" system)
                                                 (format nil "sbcl-home/contrib/~A.fasl" system))))))
        (unless (equal (sort (copy-list built) #'string<) (sort expected #'string<))
          (error "The contrib build guest exported unexpected files: ~S" built))
        (values "/build/contrib-export/sbcl-home/" "/build/contrib-export/foreign-lookups.txt")))))

(defun sbcl-build-defined-symbols (path)
  "Return a hash set of the global symbols the object or archive PATH defines."
  (let ((symbols (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string (uiop:run-program
                                      (list "/opt/rumprun/bin/x86_64-rumprun-netbsd-nm"
                                            "-g" "--defined-only" path)
                                      :output ':string)
                                     :separator '(#\Newline)))
      (let ((fields (remove "" (uiop:split-string line :separator '(#\Space)) :test #'string=)))
        (when (= (length fields) 3)
          (setf (gethash (third fields) symbols) t))))
    symbols))

(defun sbcl-build-check-missing-symbols (log-path image)
  "Fail when the guest in LOG-PATH looked up a C symbol that IMAGE or
rumprun's libc defines: the static table should have provided it. Return the
missing names, all of which neither defines."
  (let ((image-symbols (sbcl-build-defined-symbols image))
        (libc-symbols  (sbcl-build-defined-symbols "/opt/rumprun/rumprun-x86_64/lib/libc.a"))
        (prefix        "rumprun: missing static foreign symbol ")
        (missing       nil))
    (dolist (line (uiop:split-string (remove #\Return (uiop:read-file-string log-path))
                                     :separator '(#\Newline)))
      (when (uiop:string-prefix-p prefix line)
        (pushnew (subseq line (length prefix)) missing :test #'string=)))
    (dolist (name missing)
      (when (or (gethash name image-symbols) (gethash name libc-symbols))
        (error "Guest ~A missed ~A, which the image could provide." log-path name)))
    (format t "Guest symbols unavailable on rumprun: ~{~A~^ ~}~%" missing)
    missing))

(defun sbcl-build-main ()
  "Build the disposable SBCL port, its warm core, and its contribs inside
guests, then boot the machine fixture, clock probes, and SBCL smoke guest."
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
    ;; Build the runtime objects once, so each guest link can expose every
    ;; function the runtime defines.
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed" "make" "-j4" "-C" "src/runtime" "sbcl")
     *sbcl-source-directory* "11-runtime-objects.log")
    (multiple-value-bind (warm-core warm-lookups) (sbcl-build-warm-core)
      (multiple-value-bind (sbcl-home lookups) (sbcl-build-contribs warm-core warm-lookups)
        (sbcl-build-command (list "rm" "-rf" "/build/smoke-stage") "/build/" "smoke-clean.log")
        (sbcl-build-command (list "mkdir" "-p" "/build/smoke-stage") "/build/" "smoke-stage.log")
        (sbcl-build-command (list "cp" "-R" (string-right-trim "/" sbcl-home) "/build/smoke-stage/")
                            "/build/" "smoke-contribs.log")
        (sbcl-build-sbcl-guest :name "sbcl" :core warm-core :lookups lookups
                               :script "/probe/sbcl-smoke.lisp"
                               :sources "tree" :tree "/build/smoke-stage/")))
    (sbcl-build-command
     (list "env" "RUMPRUN_STUBLINK=succeed"
           "/opt/rumprun/bin/x86_64-rumprun-netbsd-gcc" "-O2" "-Wall"
           "-Wextra" "-Werror" "-I" "src/runtime" "-I/build/rumprun/include"
           "-Wl,--wrap=__sigaction14,--wrap=__sigprocmask14,--wrap=pthread_sigmask,--wrap=__libc_thr_sigsetmask,--wrap=pthread_create,--wrap=pthread_kill,--wrap=sigwait,--wrap=__sigaltstack14,--wrap=bmk_pgalloc,--wrap=bmk_pgfree"
           "/probe/sbcl-machine-test.c" "src/runtime/sbcl-machine.c" "src/runtime/sbcl-process.c"
           "src/runtime/sbcl-traps.S" "-lpthread" "-o" "/probe/sbcl-machine-test")
     *sbcl-source-directory* "13-machine-test-compile.log")
    (sbcl-build-bake "/probe/sbcl-machine-test.bin" "/probe/sbcl-machine-test"
                     "14-machine-test-bake.log")
    (sbcl-build-boot :image "/probe/sbcl-machine-test.bin" :command "machine-test"
                     :log-name "15-machine-test-boot.log"
                     :markers '((:prefix "MACHINE-OK:")))
    (sbcl-build-clock-fixtures)
    (sbcl-build-boot :image "/probe/sbcl.bin" :command "sbcl"
                     :log-name "16-sbcl-smoke-boot.log" :memory 3072 :timeout 300
                     :debug-exit t
                     :markers '("LISP-SMOKE-OK 24 checks" "SBCL-GUEST-EXIT 0"))
    (sbcl-build-check-missing-symbols "/build/sbcl-logs/16-sbcl-smoke-boot.log" "/probe/sbcl.bin")
    (format t "SBCL rumprun build and guest checks completed.~%")))

(unless (member :sbcl-build-library *features*)
  (sbcl-build-main))
