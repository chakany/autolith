;;;; Generate the guest's static foreign-symbol table and embedded data.
(require :asdf)
(unless (find-package '#:autolith) (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

(defun rump-sbcl-source-archive (root)
  "Write and return an archive of warm-init sources as length-prefixed byte
records in stable path order."
  (let ((files nil))
    (labels ((walk (directory all)
               (dolist (file (uiop:directory-files directory))
                 (when (or all (member (pathname-type file) '("lisp" "lisp-expr")
                                      :test #'equal))
                   (push file files)))
               (dolist (child (uiop:subdirectories directory)) (walk child all))))
      (walk (merge-pathnames "src/" root) nil)
      (walk (merge-pathnames "output/ucd/" root) t)
      ;; The load phase reads fasls a previous guest compiled and exported.
      (when (uiop:directory-exists-p (merge-pathnames "obj/from-self/" root))
        (walk (merge-pathnames "obj/from-self/" root) t)))
    ;; Warm compilation also reads build-generated data outside src/.
    (dolist (file (uiop:directory-files (merge-pathnames "tools-for-build/" root)))
      (when (equal (pathname-type file) "lisp-expr")
        (push file files)))
    (dolist (file (uiop:directory-files (merge-pathnames "output/" root)))
      (when (or (member (pathname-type file) '("lisp-expr" "lisp" "txt" "inc" "def")
                        :test #'equal)
                (equal (file-namestring file) "build-config"))
        (push file files)))
    (dolist (name '("version.lisp-expr" "local-target-features.lisp-expr"
                    "make-target-2-load.lisp" "contrib/asdf/asdf.lisp"))
      (push (merge-pathnames name root) files))
    (rump-sbcl-write-archive root files (merge-pathnames "output/warm-sources.bin" root))))

(defun rump-sbcl-write-archive (root files output)
  "Write FILES, named relative to ROOT, to the archive OUTPUT as length-prefixed
byte records in stable path order, and return OUTPUT."
  (with-open-file (out output :direction ':output :if-exists ':supersede
                              :element-type '(unsigned-byte 8))
    (flet ((u32 (value)
             (assert (typep value '(unsigned-byte 32)))
             (loop for shift from 24 downto 0 by 8
                   do (write-byte (ldb (byte 8 shift) value) out))))
      (dolist (file (sort (copy-list files) #'string< :key #'namestring))
        (let ((name (enough-namestring file root)))
          (with-open-file (input file :element-type '(unsigned-byte 8))
            (u32 (length name)) (u32 (file-length input))
            (map nil (lambda (character) (write-byte (char-code character) out)) name)
            (uiop:copy-stream-to-stream input out :element-type '(unsigned-byte 8)))))
      (u32 0) (u32 0)))
  output)

(defun rump-sbcl-contrib-archive (root)
  "Write and return an archive of contrib Lisp sources and definitions, and
the host-produced grovel constants in rumprun-grovel/ when present."
  (let ((files nil))
    (labels ((walk (directory types)
               (dolist (file (uiop:directory-files directory))
                 (when (or (null types) (member (pathname-type file) types :test #'equal))
                   (push file files)))
               (dolist (child (uiop:subdirectories directory))
                 (walk child types))))
      (walk (merge-pathnames "contrib/" root) '("lisp" "asd"))
      (when (uiop:directory-exists-p (merge-pathnames "rumprun-grovel/" root))
        (walk (merge-pathnames "rumprun-grovel/" root) nil)))
    (rump-sbcl-write-archive root files (merge-pathnames "output/contrib-sources.bin" root))))

(defun rump-sbcl-tree-archive (root tree)
  "Write and return an archive of every regular file below TREE, named
relative to TREE so it unpacks below the guest's /core/."
  (let ((tree (uiop:ensure-directory-pathname tree))
        (files nil))
    (labels ((walk (directory)
               (dolist (file (uiop:directory-files directory))
                 (push file files))
               (dolist (child (uiop:subdirectories directory))
                 (walk child))))
      (walk tree))
    (rump-sbcl-write-archive tree files (merge-pathnames "output/tree-sources.bin" root))))

(defun rump-sbcl-empty-archive (root)
  "Write and return an archive holding only the terminating record."
  (rump-sbcl-write-archive root nil (merge-pathnames "output/no-sources.bin" root)))

(defparameter *rump-sbcl-foreign-operators*
  '("extern-alien" "define-alien-routine" "define-alien-variable"
    "foreign-symbol-address" "find-foreign-symbol-address"
    "find-dynamic-foreign-symbol-address"
    "syscall" "syscall*" "int-syscall" "void-syscall" "with-restarted-syscall")
  "Lisp operators whose first string argument names a C symbol.")

(defparameter *rump-sbcl-symbol-named-operators*
  '("extern-alien" "define-alien-routine" "define-alien-variable")
  "Operators that also accept a Lisp symbol, naming the C symbol SBCL derives
by downcasing it and replacing hyphens with underscores.")

(defun rump-sbcl-c-identifier-p (name)
  "Return T when NAME is a nonempty C identifier."
  (if (and (plusp (length name))
           (not (digit-char-p (char name 0)))
           (every (lambda (character)
                    (or (alphanumericp character) (char= character #\_)))
                  name))
      t
      nil))

(defun rump-sbcl-symbol->c-name (token)
  "Convert an unqualified Lisp symbol TOKEN to SBCL's default C name, or NIL."
  (let ((name (substitute #\_ #\- (string-downcase token))))
    (and (rump-sbcl-c-identifier-p name) name)))

(defun rump-sbcl-foreign-name-at (text start &key symbols)
  "Return the C name given at START, after optional whitespace and one
optional opening parenthesis, or NIL. A string literal names the symbol
directly; with SYMBOLS, an unparenthesized Lisp symbol names it through
SBCL's conversion."
  (flet ((skip-space (position)
           (or (position-if-not (lambda (character)
                                  (member character '(#\Space #\Tab #\Newline #\Return)))
                                text :start position)
               (length text))))
    (let ((position      (skip-space start))
          (parenthesized nil))
      (when (and (< position (length text)) (char= (char text position) #\())
        (setf position      (skip-space (1+ position))
              parenthesized t))
      (cond ((>= position (length text))
             nil)
            ((char= (char text position) #\")
             (let* ((name-start (1+ position))
                    (name-end   (position #\" text :start name-start))
                    (name       (and name-end (subseq text name-start name-end))))
               (and name (rump-sbcl-c-identifier-p name) name)))
            ((and symbols (not parenthesized))
             (let* ((token-end (or (position-if (lambda (character)
                                                  (member character '(#\Space #\Tab #\Newline
                                                                      #\Return #\( #\))))
                                                text :start position)
                                   (length text)))
                    (token     (subseq text position token-end)))
               (and (not (find #\: token)) (rump-sbcl-symbol->c-name token))))
            (t
             nil)))))

(defun rump-sbcl-operator-position-p (text start)
  "Return T when the symbol at START directly follows an opening parenthesis,
optionally qualified by a package prefix such as SB-ALIEN: or SB-SYS::."
  (let ((position (1- start)))
    (when (and (>= position 0) (char= (char text position) #\:))
      (loop while (and (>= position 0) (char= (char text position) #\:))
            do (decf position))
      (loop while (and (>= position 0)
                       (let ((character (char text position)))
                         (or (alphanumericp character) (char= character #\-))))
            do (decf position)))
    (if (and (>= position 0) (char= (char text position) #\())
        t
        nil)))

(defun rump-sbcl-warm-foreign-names (root script)
  "Collect C symbols that warm-loaded sources, contrib sources, and SCRIPT
name in foreign operator forms. The scan is textual, so it also sees names under other
platforms' feature conditionals; those stay unresolved weak references."
  (let ((names nil))
    (dolist (file (append (list script)
                          (uiop:directory-files (merge-pathnames "src/**/" root) "*.lisp")
                          (uiop:directory-files (merge-pathnames "contrib/**/" root) "*.lisp")
                          (uiop:directory-files (merge-pathnames "contrib/**/" root) "*.asd")))
      (let ((text (uiop:read-file-string file :external-format ':latin-1)))
        (dolist (operator *rump-sbcl-foreign-operators*)
          (let ((needle operator))
            (loop for found = (search needle text) then (search needle text :start2 (1+ found))
                  while found
                  do (let ((after (+ found (length needle))))
                       (when (and (< after (length text))
                                  (rump-sbcl-operator-position-p text found)
                                  (member (char text after) '(#\Space #\Tab #\Newline #\Return)))
                         (let ((name (rump-sbcl-foreign-name-at
                                      text after
                                      :symbols (member operator *rump-sbcl-symbol-named-operators*
                                                       :test #'string=))))
                           (when name
                             (pushnew name names :test #'string=))))))))
        ;; WITH-ALIEN names C symbols through its :EXTERN option.
        (loop for found = (search ":extern" text) then (search ":extern" text :start2 (1+ found))
              while found
              do (let ((name (or (rump-sbcl-foreign-name-at text (+ found (length ":extern")))
                                 (rump-sbcl-binding-c-name text found))))
                   (when name
                     (pushnew name names :test #'string=))))))
    names))

(defparameter *rump-sbcl-libc-archive* "/opt/rumprun/rumprun-x86_64/lib/libc.a"
  "The rumprun libc archive that the final bake links.")

(defparameter *rump-sbcl-nm* "/opt/rumprun/bin/x86_64-rumprun-netbsd-nm"
  "The rumprun toolchain's symbol lister.")

(defun rump-sbcl-archive-symbols (archive)
  "Return a hash set of the global symbols ARCHIVE defines."
  (let ((symbols (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string (uiop:run-program
                                      (list *rump-sbcl-nm* "-g" "--defined-only" archive)
                                      :output ':string)
                                     :separator '(#\Newline)))
      (let ((fields (remove "" (uiop:split-string line :separator '(#\Space)) :test #'string=)))
        (when (= (length fields) 3)
          (setf (gethash (third fields) symbols) t))))
    (assert (gethash "memcpy" symbols) () "No libc symbols read from ~A." archive)
    symbols))

(defun rump-sbcl-enclosing-open (text position)
  "Return the index of the unmatched opening parenthesis before POSITION, or NIL."
  (let ((depth 0))
    (loop for index downfrom (1- position) to 0
          for character = (char text index)
          do (case character
               (#\)
                (incf depth))
               (#\(
                (if (zerop depth)
                    (return index)
                    (decf depth)))))))

(defun rump-sbcl-binding-c-name (text extern)
  "Return the C name of the WITH-ALIEN binding whose bare :EXTERN option
starts at EXTERN, derived from the binding's variable symbol, or NIL when
the keyword is not an option of such a binding."
  (let* ((binding (rump-sbcl-enclosing-open text extern))
         (list    (and binding (rump-sbcl-enclosing-open text binding)))
         (before  (and list (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                               (subseq text 0 list)))))
    (when (and before (uiop:string-suffix-p before "with-alien"))
      (rump-sbcl-foreign-name-at text (1+ binding) :symbols t))))

(defun rump-sbcl-wrapped-symbols (root)
  "Return the symbols the runtime link wraps, from its Config's --wrap flags."
  (let ((text  (uiop:read-file-string (merge-pathnames "src/runtime/Config" root)))
        (names nil))
    (loop for start = (search "--wrap=" text) then (search "--wrap=" text :start2 end)
          for end = (and start (or (position-if (lambda (character)
                                                  (member character '(#\, #\Space #\Newline)))
                                                text :start (+ start 7))
                                   (length text)))
          while start
          do (pushnew (subseq text (+ start 7) end) names :test #'string=))
    (assert (member "__sigaction14" names :test #'string=))
    names))

(defun rump-sbcl-symbol-target (name wrapped)
  "Return the link-time symbol a Lisp lookup of NAME must reach. Lisp calls
into WRAPPED symbols reach the same wrappers as C callers, and the classic
signal set operations resolve to NetBSD's current ABI."
  (cond ((member name wrapped :test #'string=) (concatenate 'string "__wrap_" name))
        ((string= name "sigaddset") "__sigaddset14")
        ((string= name "sigdelset") "__sigdelset14")
        (t name)))

(defun rump-sbcl-write-symbol-table (root names weak)
  "Write the static dlsym table for NAMES, declaring members of WEAK as weak."
  (let ((wrapped (rump-sbcl-wrapped-symbols root)))
    (with-open-file (out (merge-pathnames "src/runtime/rumprun-symbols.c" root)
                         :direction ':output :if-exists ':supersede)
      (format out "#include <stddef.h>~%#include <string.h>~%")
      (loop for name in names for index from 0
            do (format out "extern char foreign_~D[] __asm__(~S)~:[~; __attribute__((weak))~];~%"
                       index (rump-sbcl-symbol-target name wrapped)
                       (member name weak :test #'string=)))
      (format out "static const struct { const char *name; void *address; } symbols[] = {~%")
      (loop for name in names for index from 0
            do (format out "  {~S, foreign_~D},~%" name index))
      (format out "};~%void *rumprun_symbol(const char *name) {~%  for (size_t i=0; i<sizeof(symbols)/sizeof(symbols[0]); ++i)~%    if (!strcmp(name, symbols[i].name)) return symbols[i].address;~%  return NULL;~%}~%"))
    (format t "Declared ~D warm-only symbols weak.~%" (length weak))))

(defun rump-sbcl-read-lookups (path)
  "Return the C names in the guest foreign lookup manifest at PATH, whose
lines are \"found NAME\" or \"missing NAME\"."
  (with-open-file (stream path)
    (loop for line = (read-line stream nil) while line
          collect (let* ((space (position #\Space line))
                         (state (and space (subseq line 0 space)))
                         (name  (and space (subseq line (1+ space)))))
                    (unless (and (member state '("found" "missing") :test #'string=)
                                 (rump-sbcl-c-identifier-p name))
                      (error "Malformed foreign lookup line in ~A: ~S" path line))
                    name))))

(defun rump-sbcl-link-inputs (root &key core script sources tree lookups)
  "Generate the static symbol table and embed CORE, SCRIPT, and SOURCES,
which is :WARM for SBCL's warm-initialization sources, :CONTRIB for contrib
sources, :TREE for every file below TREE, or :NONE. LOOKUPS
names the foreign lookup manifest of the guest that saved CORE, if any. Use
genesis's explicit alien linkage map, never host symbol addresses."
  (let ((names nil) (inside nil))
    (with-open-file (stream (merge-pathnames "output/cold-sbcl.map" root))
      (loop for line = (read-line stream nil) while line
            do (cond ((search "IX. alien linkage table:" line) (setf inside t))
                     (inside
                      (let ((position (search " = " line)))
                        (when position
                          (let ((name (string-trim '(#\Space #\Tab #\Return)
                                                   (subseq line (+ position 3)))))
                            (assert (every (lambda (character)
                                             (or (alphanumericp character)
                                                 (find character "_"))) name))
                            (pushnew name names :test #'string=))))))))
    (assert (> (length names) 100))
    ;; Genesis names must link, and so must warm-only names that libc defines,
    ;; because a weak reference never extracts an archive member. Other
    ;; warm-only names are weak: absent symbols resolve to NULL and report
    ;; through the dlsym wrapper when looked up.
    (let* ((libc (rump-sbcl-archive-symbols *rump-sbcl-libc-archive*))
           (warm (set-difference (remove-duplicates
                                  (append (rump-sbcl-warm-foreign-names root script)
                                          (and lookups (rump-sbcl-read-lookups lookups)))
                                  :test #'string=)
                                 names :test #'string=))
           (weak (remove-if (lambda (name) (gethash name libc)) warm)))
      (setf names (append names (remove-if-not (lambda (name) (gethash name libc)) warm)))
      (setf names (sort (append names (copy-list weak)) #'string<))
      (rump-sbcl-write-symbol-table root names weak))
    (let ((archive (ecase sources
                     (:warm    (rump-sbcl-source-archive root))
                     (:contrib (rump-sbcl-contrib-archive root))
                     (:tree    (rump-sbcl-tree-archive root tree))
                     (:none    (rump-sbcl-empty-archive root)))))
      (with-open-file (out (merge-pathnames "src/runtime/rumprun-core.S" root)
                           :direction ':output :if-exists ':supersede)
        (format out ".section .rodata~%.balign 4096~%.globl rumprun_core_start, rumprun_core_end~%rumprun_core_start:~%.incbin ~S~%rumprun_core_end:~%"
                (namestring core))
        (format out ".globl rumprun_script_start, rumprun_script_end~%rumprun_script_start:~%.incbin ~S~%rumprun_script_end:~%"
                (namestring script))
        (format out ".globl rumprun_sources_start, rumprun_sources_end~%rumprun_sources_start:~%.incbin ~S~%rumprun_sources_end:~%.section .note.GNU-stack,~S,@progbits~%"
                (namestring archive) "")))
    (let ((config (merge-pathnames "src/runtime/Config" root)))
      (unless (search "rumprun-symbols.c" (uiop:read-file-string config))
        (with-open-file (out config :direction ':output :if-exists ':append)
          (format out "~%OS_SRC += rumprun-symbols.c~%ASSEM_SRC += rumprun-core.S~%"))))
    (format t "Generated ~D static symbol entries.~%" (length names))))

(defun rump-sbcl-link-main (arguments)
  "Parse ROOT --core FILE --script FILE --sources warm|contrib|tree|none
[--tree DIRECTORY] [--lookups FILE]
and link."
  (destructuring-bind (root &rest options) arguments
    (flet ((option (name &optional (required t))
             (let ((tail (member name options :test #'string=)))
               (or (second tail) (and required (error "Missing ~A." name))))))
      (rump-sbcl-link-inputs (uiop:ensure-directory-pathname root)
                             :core    (uiop:parse-native-namestring (option "--core"))
                             :script  (uiop:parse-native-namestring (option "--script"))
                             :sources (let ((sources (option "--sources")))
                                        (cond ((string= sources "warm") ':warm)
                                              ((string= sources "contrib") ':contrib)
                                              ((string= sources "tree") ':tree)
                                              ((string= sources "none") ':none)
                                              (t (error "Unknown --sources ~A." sources))))
                             :tree    (let ((tree (option "--tree" nil)))
                                        (and tree (uiop:parse-native-namestring tree)))
                             :lookups (let ((lookups (option "--lookups" nil)))
                                        (and lookups (uiop:parse-native-namestring lookups)))))))

(rump-sbcl-link-main (uiop:command-line-arguments))
