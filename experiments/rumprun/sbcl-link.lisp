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

(defparameter *rump-sbcl-system-archives*
  '("/opt/rumprun/rumprun-x86_64/lib/libc.a"
    "/opt/rumprun/rumprun-x86_64/lib/rumprun-hw/librump.a"
    "/opt/rumprun/rumprun-x86_64/lib/libpthread.a")
  "The archives the final bake links for the C library interface: libc, the
rump kernel's system call entry points such as link, and libpthread.")

(defparameter *rump-sbcl-nm* "/opt/rumprun/bin/x86_64-rumprun-netbsd-nm"
  "The rumprun toolchain's symbol lister.")

(defun rump-sbcl-archive-symbols (archive &key (types nil) (required "memcpy"))
  "Return a hash set of the global symbols ARCHIVE defines, limited to the
nm type letters in TYPES when given. Fail unless REQUIRED is among them,
or, when REQUIRED is NIL, unless nm read any symbol."
  (let ((symbols (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string (uiop:run-program
                                      (list *rump-sbcl-nm* "-g" "--defined-only" archive)
                                      :output ':string)
                                     :separator '(#\Newline)))
      (let ((fields (remove "" (uiop:split-string line :separator '(#\Space)) :test #'string=)))
        (when (and (= (length fields) 3)
                   (or (null types) (find (char (second fields) 0) types)))
          (setf (gethash (third fields) symbols) t))))
    (assert (if required (gethash required symbols) (plusp (hash-table-count symbols)))
            () "No symbol ~:[~;~:*~A ~]read from ~A." required archive)
    symbols))

(defun rump-sbcl-library-functions (library)
  "Return the sorted names of the global functions static LIBRARY defines,
including weak definitions. Every one is linked, because Lisp may look up
any of them by name."
  (let ((names nil))
    (maphash (lambda (name value)
               (declare (ignore value))
               (when (rump-sbcl-c-identifier-p name)
                 (push name names)))
             (rump-sbcl-archive-symbols (namestring library) :types "TW" :required nil))
    (sort names #'string<)))

(defun rump-sbcl-system-symbols ()
  "Return a hash set of every global symbol the system archives define."
  (let ((symbols (make-hash-table :test #'equal)))
    (dolist (archive *rump-sbcl-system-archives*)
      (maphash (lambda (name value)
                 (setf (gethash name symbols) value))
               (rump-sbcl-archive-symbols archive :required nil)))
    (assert (and (gethash "memcpy" symbols) (gethash "link" symbols)) ()
            "System archive symbols unread.")
    symbols))

(defun rump-sbcl-system-functions ()
  "Return a hash set of the functions the system archives define outside
libc's compat members, whose old ABIs conflict with the current definitions."
  (let ((functions (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string (uiop:run-program
                                      (list* *rump-sbcl-nm* "-A" "-g" "--defined-only"
                                             *rump-sbcl-system-archives*)
                                      :output ':string)
                                     :separator '(#\Newline)))
      (let* ((fields (remove "" (uiop:split-string line :separator '(#\Space)) :test #'string=))
             (member (and (= (length fields) 3)
                          (second (uiop:split-string (first fields) :separator '(#\:))))))
        (when (and member
                   (find (char (second fields) 0) "TW")
                   (not (uiop:string-prefix-p "compat" member)))
          (setf (gethash (third fields) functions) t))))
    (assert (and (gethash "mkdtemp" functions) (gethash "link" functions)) ()
            "No system functions read.")
    functions))

(defun rump-sbcl-quoted-identifiers (files)
  "Return the C identifiers that FILES contain as whole string literals.
Foreign interfaces often name C functions this way outside the operators
the foreign-name scan knows, as sb-posix does when it asks at compile time
whether mkdtemp exists. Matching only a quote, an identifier, and a quote
keeps a stray quote in a comment or a #\\\" character from hiding names."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (file files)
      (let ((text (uiop:read-file-string file :external-format ':latin-1)))
        (loop for start = (position #\" text) then (position #\" text :start end)
              for end = (and start (or (position-if-not (lambda (character)
                                                          (or (alphanumericp character)
                                                              (char= character #\_)))
                                                        text :start (1+ start))
                                       (length text)))
              while start
              do (when (and (< end (length text)) (char= (char text end) #\"))
                   (let ((name (subseq text (1+ start) end)))
                     (when (rump-sbcl-c-identifier-p name)
                       (setf (gethash name names) t)))))))
    names))

(defun rump-sbcl-runtime-functions (root)
  "Return the sorted names of the global functions SBCL's own runtime
objects define, such as sb-posix's s_isdir. They are always linked, so the
table exposes them all at no cost."
  (let* ((generated '("rumprun-symbols.o" "rumprun-core.o"))
         (objects   (remove-if (lambda (object)
                                 (member (file-namestring object) generated :test #'string=))
                               (uiop:directory-files (merge-pathnames "src/runtime/" root) "*.o")))
         (names     nil))
    (assert objects () "No runtime objects below ~A." root)
    (dolist (line (uiop:split-string (uiop:run-program
                                      (list* *rump-sbcl-nm* "-g" "--defined-only"
                                             (mapcar #'namestring objects))
                                      :output ':string)
                                     :separator '(#\Newline)))
      (let ((fields (remove "" (uiop:split-string line :separator '(#\Space)) :test #'string=)))
        (when (and (= (length fields) 3)
                   (find (char (second fields) 0) "TW")
                   (rump-sbcl-c-identifier-p (third fields)))
          (pushnew (third fields) names :test #'string=))))
    (sort names #'string<)))

(defparameter *rump-sbcl-include-directory* "/opt/rumprun/rumprun-x86_64/include/"
  "The rumprun sysroot headers that C code in the guest compiles against.")

(defparameter *rump-sbcl-declaration-words*
  '("void" "int" "char" "short" "long" "signed" "unsigned" "float" "double"
    "const" "volatile" "struct" "union" "enum")
  "C words that can directly precede a parenthesis in a declaration without
being the declared function's name.")

(defun rump-sbcl-identifier-before (text end)
  "Return the C identifier that ends at END, ignoring whitespace, or NIL."
  (let* ((last  (position-if-not (lambda (character)
                                   (member character '(#\Space #\Tab #\Newline #\Return)))
                                 text :end end :from-end t))
         (start (and last (position-if-not (lambda (character)
                                             (or (alphanumericp character) (char= character #\_)))
                                           text :end (1+ last) :from-end t)))
         (name  (and last (subseq text (if start (1+ start) 0) (1+ last)))))
    (and name (rump-sbcl-c-identifier-p name) name)))

(defun rump-sbcl-header-renames ()
  "Return a hash table from each function name the sysroot headers rename
with __RENAME to the symbol C callers actually reach, as NetBSD does to move
a function to a new ABI, such as unsetenv to __unsetenv13."
  (let ((renames (make-hash-table :test #'equal)))
    (dolist (file (uiop:directory-files (merge-pathnames "**/" *rump-sbcl-include-directory*)
                                        "*.h"))
      ;; Skip dangling links, which no C code can include either.
      (let ((text (handler-case (uiop:read-file-string file :external-format ':latin-1)
                    (file-error ()
                      ""))))
        (loop for found = (search "__RENAME(" text) then (search "__RENAME(" text :start2 (1+ found))
              while found
              do (let* ((target-start (+ found (length "__RENAME(")))
                        (target-end   (position #\) text :start target-start))
                        (target       (and target-end (string-trim " " (subseq text target-start
                                                                          target-end))))
                        (start        (1+ (or (position-if (lambda (character)
                                                             (member character '(#\; #\{ #\} #\#)))
                                                           text :end found :from-end t)
                                              -1)))
                        (name         (loop for open = (position #\( text :start start :end found)
                                              then (position #\( text :start (1+ open) :end found)
                                            while open
                                            do (let ((candidate (rump-sbcl-identifier-before
                                                                 text open)))
                                                 (when (and candidate
                                                            (not (member candidate
                                                                         *rump-sbcl-declaration-words*
                                                                         :test #'string=)))
                                                   (return candidate))))))
                   (when (and name target (rump-sbcl-c-identifier-p target)
                              (string/= name target))
                     (setf (gethash name renames) target))))))
    (assert (equal (gethash "unsetenv" renames) "__unsetenv13") () "Header renames unread.")
    renames))

(defun rump-sbcl-quoted-system-functions (files renames)
  "Return the names FILES contain as whole string literals that reach a
system function, directly or through the header RENAMES."
  (let ((system (rump-sbcl-system-functions))
        (names nil))
    (maphash (lambda (name value)
               (declare (ignore value))
               (when (gethash (gethash name renames name) system)
                 (push name names)))
             (rump-sbcl-quoted-identifiers files))
    (sort names #'string<)))

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

(defun rump-sbcl-symbol-target (name wrapped renames)
  "Return the link-time symbol a Lisp lookup of NAME must reach: the symbol
a C caller reaches through the header RENAMES, such as __sigaddset14 for
sigaddset, and for WRAPPED symbols the same wrapper C callers reach."
  (let ((target (gethash name renames name)))
    (cond ((member name wrapped :test #'string=)
           (concatenate 'string "__wrap_" name))
          ((member target wrapped :test #'string=)
           (concatenate 'string "__wrap_" target))
          (t
           target))))

(defun rump-sbcl-write-symbol-table (root names weak renames)
  "Write the static dlsym table for NAMES, declaring members of WEAK as weak
and resolving names through the header RENAMES."
  (let ((wrapped (rump-sbcl-wrapped-symbols root)))
    (with-open-file (out (merge-pathnames "src/runtime/rumprun-symbols.c" root)
                         :direction ':output :if-exists ':supersede)
      (format out "#include <stddef.h>~%#include <string.h>~%")
      (loop for name in names for index from 0
            do (format out "extern char foreign_~D[] __asm__(~S)~:[~; __attribute__((weak))~];~%"
                       index (rump-sbcl-symbol-target name wrapped renames)
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

(defun rump-sbcl-link-inputs (root &key core script sources tree lookups libraries)
  "Generate the static symbol table and embed CORE, SCRIPT, and SOURCES,
which is :WARM for SBCL's warm-initialization sources, :CONTRIB for contrib
sources, :TREE for every file below TREE, or :NONE. LOOKUPS
names the foreign lookup manifest of the guest that saved CORE, if any.
LIBRARIES are static archives linked into the runtime, in link order, whose
every function the table exposes. Use genesis's explicit alien linkage map,
never host symbol addresses."
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
    ;; Genesis names must link, and so must every function of the runtime and
    ;; of a linked library, which Lisp may look up by any name. Warm-only names
    ;; that the system archives define are strong too, because a weak reference
    ;; never extracts an archive member. Other warm-only names are weak: absent
    ;; symbols resolve to NULL and report through the dlsym wrapper when looked
    ;; up.
    (setf names (union names (rump-sbcl-runtime-functions root) :test #'string=))
    (dolist (library libraries)
      (setf names (union names (rump-sbcl-library-functions library) :test #'string=)))
    (let* ((system   (rump-sbcl-system-symbols))
           (renames  (rump-sbcl-header-renames))
           (quoted   (rump-sbcl-quoted-system-functions
                      (append (list script)
                              (uiop:directory-files (merge-pathnames "src/**/" root) "*.lisp")
                              (uiop:directory-files (merge-pathnames "contrib/**/" root) "*.lisp")
                              (and tree (uiop:directory-files
                                         (merge-pathnames "**/" (uiop:ensure-directory-pathname tree))
                                         "*.lisp")))
                      renames))
           (warm     (set-difference (remove-duplicates
                                      (append (rump-sbcl-warm-foreign-names root script)
                                              quoted
                                              (and lookups (rump-sbcl-read-lookups lookups)))
                                      :test #'string=)
                                     names :test #'string=))
           (system-p (lambda (name) (gethash (gethash name renames name) system)))
           (weak     (remove-if system-p warm)))
      (setf names (append names (remove-if-not system-p warm)))
      (setf names (sort (append names (copy-list weak)) #'string<))
      (rump-sbcl-write-symbol-table root names weak renames))
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
          (format out "~%OS_SRC += rumprun-symbols.c~%ASSEM_SRC += rumprun-core.S~%")))
      (unless (search "rumprun-libraries.mk" (uiop:read-file-string config))
        (with-open-file (out config :direction ':output :if-exists ':append)
          (format out "-include rumprun-libraries.mk~%"))))
    ;; Each link names its own libraries, so a later guest does not inherit them.
    (with-open-file (out (merge-pathnames "src/runtime/rumprun-libraries.mk" root)
                         :direction ':output :if-exists ':supersede)
      (dolist (library libraries)
        (format out "OS_LIBS += ~A~%" (namestring library))))
    (format t "Generated ~D static symbol entries.~%" (length names))))

(defun rump-sbcl-link-main (arguments)
  "Parse ROOT --core FILE --script FILE --sources warm|contrib|tree|none
[--tree DIRECTORY] [--lookups FILE] [--library ARCHIVE]...
and link. Repeated --library options keep their order."
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
                                        (and lookups (uiop:parse-native-namestring lookups)))
                             :libraries (loop for (name value) on options
                                              when (string= name "--library")
                                                collect (uiop:parse-native-namestring
                                                         (or value (error "Missing --library archive."))))))))

(rump-sbcl-link-main (uiop:command-line-arguments))
