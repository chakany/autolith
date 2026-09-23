;;;; NFSv3 (RFC 1813) and MOUNT v3 service exporting host directories to
;;;; rumprun guests, whose rump kernel NFS client mounts them.
;;;;
;;;; Guests are untrusted. A guest reaches only the exported directories: file
;;;; handles name paths the server itself built from an export root and
;;;; validated names, and every operation rechecks that each directory from
;;;; the export root to the target is a real directory, not a symbolic link,
;;;; under one server-wide lock. The server never follows a symbolic link on
;;;; the host; READLINK returns a link's text for the guest's own client to
;;;; resolve inside its mount. Files appear owned by the calling guest user,
;;;; since the server acts for it as the host user running the broker.
(require :sb-posix)
(require :sb-bsd-sockets)
(unless (find-package '#:autolith) (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

;;;; -- Types and Conditions --

(defclass nfs-export ()
  ((name
    :initarg :name
    :reader nfs-export-name
    :type string
    :documentation "The path a guest mounts, such as /workspace.")
   (root
    :initarg :root
    :reader nfs-export-root
    :type string
    :documentation "The exported host directory as a native namestring
without a trailing slash.")
   (case-insensitive
    :initarg :case-insensitive
    :reader nfs-export-case-insensitive-p
    :documentation "Whether the host file system folds case in the export,
as macOS volumes do by default."))
  (:documentation "One host directory a guest may mount."))

(defclass nfs-server ()
  ((exports
    :initarg :exports
    :reader nfs-server-exports
    :documentation "The NFS-EXPORT instances a guest may mount.")
   (instance
    :initarg :instance
    :reader nfs-server-instance
    :documentation "Eight random octets prefixing every file handle, so a
handle from an earlier server is stale rather than misdirected.")
   (verifier
    :initarg :verifier
    :reader nfs-server-verifier
    :documentation "The write verifier, which changes when the server restarts.")
   (handles
    :initform (make-hash-table :test #'equalp)
    :reader nfs-server-handles
    :documentation "Each issued file handle's host path.")
   (paths
    :initform (make-hash-table :test #'equal)
    :reader nfs-server-paths
    :documentation "Each registered host path's file handle.")
   (counter
    :initform 0
    :accessor nfs-server-counter
    :documentation "The number of file handles issued.")
   (stamps
    :initform (make-hash-table :test #'equal)
    :reader nfs-server-stamps
    :documentation "Each host path's change stamp; see NFS--STAMP.")
   (lock
    :initform (sb-thread:make-mutex :name "NFS server")
    :reader nfs-server-lock
    :documentation "Serializes every operation, so a guest cannot race its
own path checks."))
  (:documentation "An NFSv3 and MOUNT v3 service over exported host directories."))

(define-condition nfs-status (error)
  ((status
    :initarg :status
    :reader nfs-status-code
    :documentation "The nfsstat3 value to reply with.")
   (path
    :initarg :path
    :initform nil
    :reader nfs-status-path
    :documentation "The host path the failure concerns, if any."))
  (:report (lambda (condition stream)
             (format stream "NFS status ~D~@[ for ~A~]"
                     (nfs-status-code condition) (nfs-status-path condition))))
  (:documentation "An NFS operation fails with a protocol status."))


;;;; -- Protocol Constants --

(defparameter *nfs-program* 100003 "The NFS program number.")
(defparameter *nfs-mount-program* 100005 "The MOUNT program number.")
(defparameter *nfs-version* 3 "The NFS and MOUNT version served.")
(defparameter *nfs-handle-size* 16 "The size of every issued file handle.")
(defparameter *nfs-maximum-handle* 64 "The largest NFSv3 file handle.")
(defparameter *nfs-maximum-name* 255 "The longest file name component.")
(defparameter *nfs-maximum-path* 1024 "The longest mount path or link text.")
(defparameter *nfs-transfer-size* (* 256 1024)
  "The largest READ or WRITE the server offers, within *RPC-MAXIMUM-RECORD*.")

(defparameter *nfs-ok* 0 "NFS3_OK.")
(defparameter *nfs-error-permission* 1 "NFS3ERR_PERM.")
(defparameter *nfs-error-missing* 2 "NFS3ERR_NOENT.")
(defparameter *nfs-error-io* 5 "NFS3ERR_IO.")
(defparameter *nfs-error-access* 13 "NFS3ERR_ACCES.")
(defparameter *nfs-error-exists* 17 "NFS3ERR_EXIST.")
(defparameter *nfs-error-cross-device* 18 "NFS3ERR_XDEV.")
(defparameter *nfs-error-not-directory* 20 "NFS3ERR_NOTDIR.")
(defparameter *nfs-error-directory* 21 "NFS3ERR_ISDIR.")
(defparameter *nfs-error-invalid* 22 "NFS3ERR_INVAL.")
(defparameter *nfs-error-too-big* 27 "NFS3ERR_FBIG.")
(defparameter *nfs-error-no-space* 28 "NFS3ERR_NOSPC.")
(defparameter *nfs-error-read-only* 30 "NFS3ERR_ROFS.")
(defparameter *nfs-error-links* 31 "NFS3ERR_MLINK.")
(defparameter *nfs-error-name-too-long* 63 "NFS3ERR_NAMETOOLONG.")
(defparameter *nfs-error-not-empty* 66 "NFS3ERR_NOTEMPTY.")
(defparameter *nfs-error-quota* 69 "NFS3ERR_DQUOT.")
(defparameter *nfs-error-stale* 70 "NFS3ERR_STALE.")
(defparameter *nfs-error-bad-handle* 10001 "NFS3ERR_BADHANDLE.")
(defparameter *nfs-error-not-synchronized* 10002 "NFS3ERR_NOT_SYNC.")
(defparameter *nfs-error-bad-cookie* 10003 "NFS3ERR_BAD_COOKIE.")
(defparameter *nfs-error-unsupported* 10004 "NFS3ERR_NOTSUPP.")
(defparameter *nfs-error-server* 10006 "NFS3ERR_SERVERFAULT.")

(defparameter *nfs-access-bits* '(:read 1 :lookup 2 :modify 4 :extend 8 :delete 16 :execute 32)
  "The ACCESS3 permission bits.")


;;;; -- Errors --

(defun nfs--fail (status &optional path)
  "Signal NFS-STATUS with STATUS for host PATH."
  (error 'nfs-status :status status :path path))

(defun nfs--errno-status (errno)
  "Return the nfsstat3 for host ERRNO."
  (cond ((= errno sb-posix:eperm) *nfs-error-permission*)
        ((= errno sb-posix:enoent) *nfs-error-missing*)
        ((= errno sb-posix:eacces) *nfs-error-access*)
        ((= errno sb-posix:eexist) *nfs-error-exists*)
        ((= errno sb-posix:exdev) *nfs-error-cross-device*)
        ((= errno sb-posix:enotdir) *nfs-error-not-directory*)
        ((= errno sb-posix:eisdir) *nfs-error-directory*)
        ((= errno sb-posix:einval) *nfs-error-invalid*)
        ((= errno sb-posix:efbig) *nfs-error-too-big*)
        ((= errno sb-posix:enospc) *nfs-error-no-space*)
        ((= errno sb-posix:erofs) *nfs-error-read-only*)
        ((= errno sb-posix:emlink) *nfs-error-links*)
        ((= errno sb-posix:enametoolong) *nfs-error-name-too-long*)
        ((= errno sb-posix:enotempty) *nfs-error-not-empty*)
        ((= errno sb-posix:edquot) *nfs-error-quota*)
        ((= errno sb-posix:eloop) *nfs-error-invalid*)
        (t *nfs-error-io*)))

(defmacro with-nfs-host-call ((path) &body body)
  "Evaluate BODY, translating a failed host system call on PATH into
NFS-STATUS. PATH is evaluated only when a call fails."
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (sb-posix:syscall-error (,condition)
         (nfs--fail (nfs--errno-status (sb-posix:syscall-errno ,condition)) ,path)))))


;;;; -- Paths and Handles --

(defun nfs-server-create (exports)
  "Return a server for EXPORTS, a list of (NAME ROOT) pairs naming the mount
path a guest uses and the host directory it reaches. Each ROOT must be an
existing directory."
  (flet ((random-octets (count)
           (let ((octets (make-array count :element-type '(unsigned-byte 8))))
             (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
               (read-sequence octets in))
             octets)))
    (make-instance
     'nfs-server
     :instance (random-octets 8)
     :verifier (random-octets 8)
     :exports (loop for (name root) in exports
                    collect (let ((native (string-right-trim "/" (uiop:native-namestring
                                                                  (uiop:ensure-directory-pathname root)))))
                              (unless (and (plusp (length native))
                                           (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:lstat native))))
                                (error "Export root ~A is not a directory." root))
                              (make-instance 'nfs-export :name name :root native
                                                          :case-insensitive (nfs--case-insensitive-p native)))))))

(defun nfs--case-insensitive-p (root)
  "Return true when host directory ROOT folds the case of file names, by
creating a probe file and looking it up in another case."
  (let* ((suffix (format nil "~36R" (random (ash 1 64) (make-random-state t))))
         (upper  (nfs--child root (concatenate 'string ".NFS-CASE-" suffix)))
         (lower  (nfs--child root (string-downcase (concatenate 'string ".NFS-CASE-" suffix)))))
    (sb-posix:close (sb-posix:open upper (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl) #o600))
    (unwind-protect (and (nfs--status lower) t)
      (sb-posix:unlink upper))))

(defun nfs--name-valid-p (name)
  "Return true when NAME is one path component other than . and .."
  (and (plusp (length name))
       (<= (length (sb-ext:string-to-octets name :external-format ':utf-8)) *nfs-maximum-name*)
       (not (member name '("." "..") :test #'string=))
       (not (find #\/ name))
       (not (find (code-char 0) name))
       t))

(defun nfs--export-of (server path)
  "Return the export whose root contains host PATH, or NIL."
  (find-if (lambda (export)
             (let ((root (nfs-export-root export)))
               (or (string= path root)
                   (and (> (length path) (length root))
                        (string= root path :end2 (length root))
                        (char= (char path (length root)) #\/)))))
           (nfs-server-exports server)))

(defun nfs--check-ancestors (server path)
  "Signal NFS-STATUS unless PATH lies in an export and its export root and
every directory between that root and PATH are real directories. A guest
could otherwise turn a directory it owns into a symbolic link and reach
outside the export through paths the server built before."
  (let* ((export (or (nfs--export-of server path) (nfs--fail *nfs-error-stale* path)))
         (root   (nfs-export-root export)))
    (loop for directory = root
            then (subseq path 0 slash)
          for slash = (position #\/ path :start (min (1+ (length root)) (length path)))
            then (and slash (position #\/ path :start (1+ slash)))
          do (let ((status (nfs--status directory)))
               (unless (and status (sb-posix:s-isdir (sb-posix:stat-mode status)))
                 (nfs--fail *nfs-error-stale* path)))
          while slash)))

(defun nfs--child (directory name)
  "Return host path NAME within host DIRECTORY."
  (concatenate 'string directory "/" name))

(defun nfs--handle (server path)
  "Return the file handle for host PATH, issuing one when needed."
  (or (gethash path (nfs-server-paths server))
      (let ((handle (make-array *nfs-handle-size* :element-type '(unsigned-byte 8))))
        (replace handle (nfs-server-instance server))
        (let ((counter (incf (nfs-server-counter server))))
          (loop for index from 8 below 16
                do (setf (aref handle index) (ldb (byte 8 (* 8 (- 15 index))) counter))))
        (setf (gethash handle (nfs-server-handles server)) path
              (gethash path (nfs-server-paths server)) handle))))

(defun nfs--forget (server path)
  "Forget the handles of host PATH and everything below it."
  (let ((prefix (concatenate 'string path "/")))
    (loop for known being the hash-keys of (nfs-server-paths server) using (hash-value handle)
          when (or (string= known path)
                   (and (> (length known) (length prefix))
                        (string= prefix known :end2 (length prefix))))
            do (remhash known (nfs-server-paths server))
               (remhash handle (nfs-server-handles server)))))

(defun nfs--move (server from to)
  "Move the handles of host path FROM and everything below it to TO."
  (nfs--forget server to)
  (let ((prefix (concatenate 'string from "/"))
        (moves  nil))
    (loop for known being the hash-keys of (nfs-server-paths server) using (hash-value handle)
          when (string= known from)
            do (push (cons handle to) moves)
          when (and (> (length known) (length prefix))
                    (string= prefix known :end2 (length prefix)))
            do (push (cons handle (concatenate 'string to (subseq known (length from)))) moves))
    (loop for (handle . new) in moves
          do (remhash (gethash handle (nfs-server-handles server)) (nfs-server-paths server)))
    (loop for (handle . new) in moves
          do (setf (gethash handle (nfs-server-handles server)) new
                   (gethash new (nfs-server-paths server)) handle))))

(defun nfs--resolve (server reader)
  "Read a file handle from READER and return its host path, after checking
the path's ancestors."
  (let ((handle (xdr-read-opaque reader *nfs-maximum-handle*)))
    (unless (= (length handle) *nfs-handle-size*)
      (nfs--fail *nfs-error-bad-handle*))
    (let ((path (or (gethash handle (nfs-server-handles server))
                    (nfs--fail *nfs-error-stale*))))
      (nfs--check-ancestors server path)
      path)))

(defun nfs--root-p (server path)
  "Return true when host PATH is an export root."
  (and (find path (nfs-server-exports server) :key #'nfs-export-root :test #'string=) t))


;;;; -- Attributes --

(defun nfs--type (mode)
  "Return the ftype3 for host file MODE."
  (cond ((sb-posix:s-isreg mode) 1)
        ((sb-posix:s-isdir mode) 2)
        ((sb-posix:s-isblk mode) 3)
        ((sb-posix:s-ischr mode) 4)
        ((sb-posix:s-islnk mode) 5)
        ((sb-posix:s-issock mode) 6)
        (t 7)))

(defun nfs--status (path)
  "Return host PATH's lstat, or NIL when it does not exist."
  (handler-case (sb-posix:lstat path)
    (sb-posix:syscall-error ()
      nil)))

(defvar *nfs-server* nil
  "The server performing the current operation, bound during dispatch.")

(defun nfs--stamp (path)
  "Return host PATH's change stamp: how many times the current server has
changed it, which the server reports as the sub-second part of its
modification and change times. Host times have one-second resolution here,
so without the stamp a guest could not tell two changes within one second
apart, and NFS clients would stop trusting the attributes around their own
operations."
  (gethash path (nfs-server-stamps *nfs-server*) 0))

(defun nfs--touch (&rest paths)
  "Record a change the current server made to each host path in PATHS."
  (dolist (path paths)
    (setf (gethash path (nfs-server-stamps *nfs-server*))
          (mod (1+ (nfs--stamp path)) 1000000000))))

(defun nfs--write-attributes (writer path status call)
  "Append the fattr3 of host PATH, whose lstat is STATUS, owned by CALL's
user."
  (let ((mode  (sb-posix:stat-mode status))
        (stamp (nfs--stamp path)))
    (xdr-write-unsigned writer (nfs--type mode))
    (xdr-write-unsigned writer (logand mode #o7777))
    (xdr-write-unsigned writer (sb-posix:stat-nlink status))
    (xdr-write-unsigned writer (rpc-call-uid call))
    (xdr-write-unsigned writer (rpc-call-gid call))
    (xdr-write-hyper writer (sb-posix:stat-size status))
    ;; sb-posix does not expose st_blocks; report the size in whole blocks.
    (xdr-write-hyper writer (* 512 (ceiling (sb-posix:stat-size status) 512)))
    (xdr-write-unsigned writer 0)
    (xdr-write-unsigned writer 0)
    (xdr-write-hyper writer (ldb (byte 64 0) (sb-posix:stat-dev status)))
    (xdr-write-hyper writer (ldb (byte 64 0) (sb-posix:stat-ino status)))
    (loop for seconds in (list (sb-posix:stat-atime status) (sb-posix:stat-mtime status)
                               (sb-posix:stat-ctime status))
          for nanoseconds in (list 0 stamp stamp)
          do (xdr-write-unsigned writer (ldb (byte 32 0) seconds))
             (xdr-write-unsigned writer nanoseconds))))

(defun nfs--write-post-attributes (writer path call)
  "Append post_op_attr for host PATH, absent when PATH cannot be read."
  (let ((status (and path (nfs--status path))))
    (xdr-write-boolean writer status)
    (when status
      (nfs--write-attributes writer path status call))))

(defun nfs--pre-attributes (path)
  "Return host PATH's status and change stamp before an operation, for
wcc_data, or NIL when PATH cannot be read."
  (let ((status (and path (nfs--status path))))
    (and status (cons status (nfs--stamp path)))))

(defun nfs--write-wcc (writer before path call)
  "Append wcc_data from BEFORE, a result of NFS--PRE-ATTRIBUTES, and host
PATH's current attributes."
  (xdr-write-boolean writer before)
  (when before
    (destructuring-bind (status . stamp) before
      (xdr-write-hyper writer (sb-posix:stat-size status))
      (xdr-write-unsigned writer (ldb (byte 32 0) (sb-posix:stat-mtime status)))
      (xdr-write-unsigned writer stamp)
      (xdr-write-unsigned writer (ldb (byte 32 0) (sb-posix:stat-ctime status)))
      (xdr-write-unsigned writer stamp)))
  (nfs--write-post-attributes writer path call))

(defun nfs--write-new-handle (writer server path call)
  "Append post_op_fh3 and post_op_attr for newly created host PATH."
  (xdr-write-boolean writer t)
  (xdr-write-opaque writer (nfs--handle server path))
  (nfs--write-post-attributes writer path call))

(defun nfs--read-time (reader)
  "Read a set_atime or set_mtime and return :SERVER, seconds, or NIL."
  (ecase (xdr-read-unsigned reader)
    (0 nil)
    (1 :server)
    (2 (prog1 (xdr-read-unsigned reader) (xdr-read-unsigned reader)))))

(defun nfs--read-settable (reader)
  "Read a sattr3 and return a plist of the attributes it sets."
  (let ((mode (and (xdr-read-boolean reader) (xdr-read-unsigned reader))))
    (when (xdr-read-boolean reader) (xdr-read-unsigned reader))
    (when (xdr-read-boolean reader) (xdr-read-unsigned reader))
    (let ((size  (and (xdr-read-boolean reader) (xdr-read-hyper reader)))
          (atime (nfs--read-time reader))
          (mtime (nfs--read-time reader)))
      (list :mode mode :size size :atime atime :mtime mtime))))

(defun nfs--apply-settable (path settable)
  "Apply the SETTABLE attributes to host PATH. Ownership changes are
accepted and ignored: files always appear owned by the caller."
  (let ((mode  (getf settable :mode))
        (size  (getf settable :size))
        (atime (getf settable :atime))
        (mtime (getf settable :mtime)))
    (when (or mode size atime mtime)
      (let ((status (with-nfs-host-call (path) (sb-posix:lstat path))))
        (when (sb-posix:s-islnk (sb-posix:stat-mode status))
          (nfs--fail *nfs-error-invalid* path))
        (when mode
          (with-nfs-host-call (path) (sb-posix:chmod path (logand mode #o7777))))
        (when size
          (with-nfs-host-call (path) (sb-posix:truncate path size)))
        (when (or atime mtime)
          (let ((now (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0))))
            (with-nfs-host-call (path)
              (sb-posix:utimes path
                               (cond ((eq atime :server) now)
                                     (atime atime)
                                     (t (sb-posix:stat-atime status)))
                               (cond ((eq mtime :server) now)
                                     (mtime mtime)
                                     (t (sb-posix:stat-mtime status)))))))))))


;;;; -- File Data --

(defun nfs--with-descriptor (path flags function &optional (mode #o644))
  "Call FUNCTION with a descriptor for host PATH opened with FLAGS and
O_NOFOLLOW, closing it afterward."
  (let ((descriptor (with-nfs-host-call (path)
                      (sb-posix:open path (logior flags sb-posix:o-nofollow) mode))))
    (unwind-protect (funcall function descriptor)
      (sb-posix:close descriptor))))

(defun nfs--read-data (path offset count)
  "Return up to COUNT octets of host PATH at OFFSET and whether they reach
the end of the file."
  (nfs--with-descriptor
   path sb-posix:o-rdonly
   (lambda (descriptor)
     (let ((buffer (make-array count :element-type '(unsigned-byte 8)))
           (done   0))
       (with-nfs-host-call (path) (sb-posix:lseek descriptor offset sb-posix:seek-set))
       (sb-sys:with-pinned-objects (buffer)
         (loop while (< done count)
               do (let ((read (with-nfs-host-call (path)
                                (sb-posix:read descriptor
                                               (sb-sys:sap+ (sb-sys:vector-sap buffer) done)
                                               (- count done)))))
                    (when (zerop read) (return))
                    (incf done read))))
       (values (subseq buffer 0 done)
               (>= (+ offset done) (sb-posix:stat-size (sb-posix:fstat descriptor))))))))

(defun nfs--write-data (path offset octets)
  "Write OCTETS to host PATH at OFFSET."
  (nfs--with-descriptor
   path sb-posix:o-wronly
   (lambda (descriptor)
     (with-nfs-host-call (path) (sb-posix:lseek descriptor offset sb-posix:seek-set))
     (sb-sys:with-pinned-objects (octets)
       (let ((done 0))
         (loop while (< done (length octets))
               do (incf done (with-nfs-host-call (path)
                               (sb-posix:write descriptor
                                               (sb-sys:sap+ (sb-sys:vector-sap octets) done)
                                               (- (length octets) done))))))))))

(defun nfs--directory-entries (path)
  "Return host directory PATH's entry names other than . and .., sorted so
that cookies stay stable while the directory does not change. Replies list
. and .. first, resolved within the export."
  (let ((directory (with-nfs-host-call (path) (sb-posix:opendir path)))
        (names     nil))
    (unwind-protect
         (loop for entry = (sb-posix:readdir directory)
               until (sb-alien:null-alien entry)
               do (let ((name (sb-posix:dirent-name entry)))
                    (unless (member name '("." "..") :test #'string=)
                      (push name names))))
      (sb-posix:closedir directory))
    (sort names #'string<)))

(defun nfs--directory-verifier (path)
  "Return the cookie verifier of host directory PATH: its modification time
and change stamp, which differ whenever the server changes the directory."
  (let ((writer (xdr-writer-create)))
    (xdr-write-unsigned writer (ldb (byte 32 0) (sb-posix:stat-mtime (sb-posix:lstat path))))
    (xdr-write-unsigned writer (nfs--stamp path))
    (xdr-writer->octets writer)))


;;;; -- NFS Procedures --

(defun nfs--require-directory (path)
  "Signal NFS-STATUS unless host PATH is a directory."
  (unless (sb-posix:s-isdir (sb-posix:stat-mode (with-nfs-host-call (path) (sb-posix:lstat path))))
    (nfs--fail *nfs-error-not-directory* path)))

(defun nfs--lookup-name (server directory name)
  "Return the host path NAME denotes within host DIRECTORY, including . and
.., which never leave the export."
  (cond ((string= name ".") directory)
        ((string= name "..")
         (if (nfs--root-p server directory)
             directory
             (subseq directory 0 (position #\/ directory :from-end t))))
        ((nfs--name-valid-p name) (nfs--child directory name))
        ((> (length name) *nfs-maximum-name*) (nfs--fail *nfs-error-name-too-long*))
        (t (nfs--fail *nfs-error-invalid*))))

(defun nfs--new-name (reader)
  "Read a name for a new directory entry, which must be one valid component."
  (let ((name (xdr-read-string reader *nfs-maximum-path*)))
    (unless (nfs--name-valid-p name)
      (nfs--fail (if (> (length name) *nfs-maximum-name*) *nfs-error-name-too-long* *nfs-error-invalid*)))
    name))

(defun nfs-procedure (server call writer)
  "Perform NFS CALL's procedure, appending its successful results to WRITER
after the NFS3_OK status. Signal NFS-STATUS to fail with a status, and
return :UNAVAILABLE for an unknown procedure."
  (let ((reader (rpc-call-arguments call)))
    (flet ((ok () (xdr-write-unsigned writer *nfs-ok*)))
      (case (rpc-call-procedure call)
        (0
         nil)
        (1
         (let ((path (nfs--resolve server reader)))
           (let ((status (with-nfs-host-call (path) (sb-posix:lstat path))))
             (ok)
             (nfs--write-attributes writer path status call))))
        (2
         (let* ((path     (nfs--resolve server reader))
                (settable (nfs--read-settable reader))
                (before   (nfs--pre-attributes path)))
           (when (xdr-read-boolean reader)
             (let ((seconds (xdr-read-unsigned reader)))
               (xdr-read-unsigned reader)
               (unless (and before (= seconds (ldb (byte 32 0) (sb-posix:stat-ctime before))))
                 (nfs--fail *nfs-error-not-synchronized* path))))
           (nfs--apply-settable path settable)
           (nfs--touch path)
           (ok)
           (nfs--write-wcc writer before path call)))
        (3
         (let* ((directory (nfs--resolve server reader))
                (name      (xdr-read-string reader *nfs-maximum-path*)))
           (nfs--require-directory directory)
           (let* ((path   (nfs--lookup-name server directory name))
                  (status (with-nfs-host-call (path) (sb-posix:lstat path))))
             (ok)
             (xdr-write-opaque writer (nfs--handle server path))
             (xdr-write-boolean writer t)
             (nfs--write-attributes writer path status call)
             (nfs--write-post-attributes writer directory call))))
        (4
         (let* ((path    (nfs--resolve server reader))
                (wanted  (xdr-read-unsigned reader))
                (status  (with-nfs-host-call (path) (sb-posix:lstat path)))
                (mode    (sb-posix:stat-mode status))
                (execute (getf *nfs-access-bits* :execute)))
           (ok)
           (xdr-write-boolean writer t)
           (nfs--write-attributes writer path status call)
           (xdr-write-unsigned writer (if (and (sb-posix:s-isreg mode) (zerop (logand mode #o111)))
                                          (logandc2 wanted execute)
                                          wanted))))
        (5
         (let ((path (nfs--resolve server reader)))
           (unless (sb-posix:s-islnk (sb-posix:stat-mode (with-nfs-host-call (path) (sb-posix:lstat path))))
             (nfs--fail *nfs-error-invalid* path))
           (let ((text (with-nfs-host-call (path) (sb-posix:readlink path))))
             (ok)
             (nfs--write-post-attributes writer path call)
             (xdr-write-string writer text))))
        (6
         (let* ((path   (nfs--resolve server reader))
                (offset (xdr-read-hyper reader))
                (count  (min (xdr-read-unsigned reader) *nfs-transfer-size*)))
           (multiple-value-bind (octets eof) (nfs--read-data path offset count)
             (ok)
             (nfs--write-post-attributes writer path call)
             (xdr-write-unsigned writer (length octets))
             (xdr-write-boolean writer eof)
             (xdr-write-opaque writer octets))))
        (7
         (let* ((path   (nfs--resolve server reader))
                (offset (xdr-read-hyper reader))
                (count  (xdr-read-unsigned reader)))
           (xdr-read-unsigned reader)
           (let ((octets (xdr-read-opaque reader *nfs-transfer-size*))
                 (before (nfs--pre-attributes path)))
             (unless (= count (length octets))
               (nfs--fail *nfs-error-invalid* path))
             (nfs--write-data path offset octets)
             (nfs--touch path)
             (ok)
             (nfs--write-wcc writer before path call)
             (xdr-write-unsigned writer (length octets))
             (xdr-write-unsigned writer 0)
             (xdr-write-fixed-opaque writer (nfs-server-verifier server)))))
        (8
         (let* ((directory (nfs--resolve server reader))
                (name      (nfs--new-name reader))
                (how       (xdr-read-unsigned reader))
                (settable  (if (= how 2)
                               (progn (xdr-read-fixed-opaque reader 8) nil)
                               (nfs--read-settable reader)))
                (path      (nfs--child directory name))
                (before    (nfs--pre-attributes directory)))
           (nfs--require-directory directory)
           (nfs--with-descriptor path (logior sb-posix:o-wronly sb-posix:o-creat
                                              (if (zerop how) 0 sb-posix:o-excl))
                                 #'identity
                                 (or (getf settable :mode) #o644))
           (nfs--apply-settable path (list :size (getf settable :size)
                                           :atime (getf settable :atime)
                                           :mtime (getf settable :mtime)))
           (nfs--touch path directory)
           (ok)
           (nfs--write-new-handle writer server path call)
           (nfs--write-wcc writer before directory call)))
        (9
         (let* ((directory (nfs--resolve server reader))
                (name      (nfs--new-name reader))
                (settable  (nfs--read-settable reader))
                (path      (nfs--child directory name))
                (before    (nfs--pre-attributes directory)))
           (nfs--require-directory directory)
           (with-nfs-host-call (path) (sb-posix:mkdir path (or (getf settable :mode) #o755)))
           (nfs--touch path directory)
           (ok)
           (nfs--write-new-handle writer server path call)
           (nfs--write-wcc writer before directory call)))
        (10
         (let* ((directory (nfs--resolve server reader))
                (name      (nfs--new-name reader)))
           (nfs--read-settable reader)
           (let ((text   (xdr-read-string reader *nfs-maximum-path*))
                 (path   (nfs--child directory name))
                 (before (nfs--pre-attributes directory)))
             (nfs--require-directory directory)
             (with-nfs-host-call (path) (sb-posix:symlink text path))
             (nfs--touch path directory)
             (ok)
             (nfs--write-new-handle writer server path call)
             (nfs--write-wcc writer before directory call))))
        (11
         (nfs--fail *nfs-error-unsupported*))
        ((12 13)
         (let* ((directory (nfs--resolve server reader))
                (name      (nfs--new-name reader))
                (path      (nfs--child directory name))
                (before    (nfs--pre-attributes directory)))
           (nfs--require-directory directory)
           (with-nfs-host-call (path)
             (if (= (rpc-call-procedure call) 12)
                 (sb-posix:unlink path)
                 (sb-posix:rmdir path)))
           (nfs--forget server path)
           (nfs--touch directory)
           (ok)
           (nfs--write-wcc writer before directory call)))
        (14
         (let* ((from-directory (nfs--resolve server reader))
                (from-name      (nfs--new-name reader))
                (to-directory   (nfs--resolve server reader))
                (to-name        (nfs--new-name reader))
                (from           (nfs--child from-directory from-name))
                (to             (nfs--child to-directory to-name))
                (from-before    (nfs--pre-attributes from-directory))
                (to-before      (nfs--pre-attributes to-directory)))
           (nfs--require-directory from-directory)
           (nfs--require-directory to-directory)
           (unless (eq (nfs--export-of server from) (nfs--export-of server to))
             (nfs--fail *nfs-error-cross-device* to))
           (when (and (> (length to) (length from))
                      (string= (concatenate 'string from "/") to :end2 (1+ (length from))))
             (nfs--fail *nfs-error-invalid* to))
           (with-nfs-host-call (from) (sb-posix:rename from to))
           (nfs--move server from to)
           (nfs--touch from-directory to-directory to)
           (ok)
           (nfs--write-wcc writer from-before from-directory call)
           (nfs--write-wcc writer to-before to-directory call)))
        (15
         (let* ((file      (nfs--resolve server reader))
                (directory (nfs--resolve server reader))
                (name      (nfs--new-name reader))
                (path      (nfs--child directory name))
                (before    (nfs--pre-attributes directory)))
           (nfs--require-directory directory)
           (unless (eq (nfs--export-of server file) (nfs--export-of server path))
             (nfs--fail *nfs-error-cross-device* path))
           ;; Some hosts, such as macOS, link to a symbolic link's target,
           ;; which could lie outside every export.
           (when (sb-posix:s-islnk (sb-posix:stat-mode (with-nfs-host-call (file) (sb-posix:lstat file))))
             (nfs--fail *nfs-error-invalid* file))
           (with-nfs-host-call (path) (sb-posix:link file path))
           (nfs--touch file directory)
           (ok)
           (nfs--write-post-attributes writer file call)
           (nfs--write-wcc writer before directory call)))
        ((16 17)
         (nfs--directory-reply server call reader writer (= (rpc-call-procedure call) 17)))
        (18
         (let ((path (nfs--resolve server reader)))
           (ok)
           (nfs--write-post-attributes writer path call)
           (dotimes (index 6)
             (xdr-write-hyper writer (ash 1 40)))
           (xdr-write-unsigned writer 0)))
        (19
         (let ((path (nfs--resolve server reader)))
           (ok)
           (nfs--write-post-attributes writer path call)
           ;; rtmax, rtpref, rtmult, wtmax, wtpref, wtmult, and dtpref.
           (dolist (size (list *nfs-transfer-size* *nfs-transfer-size* 4096
                               *nfs-transfer-size* *nfs-transfer-size* 4096
                               *nfs-transfer-size*))
             (xdr-write-unsigned writer size))
           (xdr-write-hyper writer (1- (ash 1 63)))
           ;; Times have one-second resolution: FSF3_LINK, FSF3_SYMLINK,
           ;; FSF3_HOMOGENEOUS, and FSF3_CANSETTIME.
           (xdr-write-unsigned writer 1)
           (xdr-write-unsigned writer 0)
           (xdr-write-unsigned writer (logior 1 2 8 16))))
        (20
         (let ((path (nfs--resolve server reader)))
           (ok)
           (nfs--write-post-attributes writer path call)
           (xdr-write-unsigned writer 32767)
           (xdr-write-unsigned writer *nfs-maximum-name*)
           (xdr-write-boolean writer t)
           (xdr-write-boolean writer t)
           (xdr-write-boolean writer (nfs-export-case-insensitive-p (nfs--export-of server path)))
           (xdr-write-boolean writer t)))
        (21
         (let ((path (nfs--resolve server reader)))
           (xdr-read-hyper reader)
           (xdr-read-unsigned reader)
           (let ((before (nfs--pre-attributes path)))
             (unless (sb-posix:s-isdir (sb-posix:stat-mode (car before)))
               (nfs--with-descriptor path sb-posix:o-rdonly
                                     (lambda (descriptor)
                                       (with-nfs-host-call (path) (sb-posix:fsync descriptor)))))
             (ok)
             (nfs--write-wcc writer before path call)
             (xdr-write-fixed-opaque writer (nfs-server-verifier server)))))
        (otherwise
         :unavailable)))))

(defun nfs--directory-reply (server call reader writer plus)
  "Append a READDIR, or with PLUS a READDIRPLUS, reply for the directory
the arguments in READER name."
  (let* ((directory (nfs--resolve server reader))
         (cookie    (xdr-read-hyper reader))
         (verifier  (xdr-read-fixed-opaque reader 8))
         (limit     (progn (when plus (xdr-read-unsigned reader))
                           (min (xdr-read-unsigned reader) *nfs-transfer-size*))))
    (nfs--require-directory directory)
    (let ((current (nfs--directory-verifier directory))
          (names   (list* "." ".." (nfs--directory-entries directory))))
      (unless (or (zerop cookie) (equalp verifier current))
        (nfs--fail *nfs-error-bad-cookie* directory))
      (when (> cookie (length names))
        (nfs--fail *nfs-error-bad-cookie* directory))
      (xdr-write-unsigned writer *nfs-ok*)
      (nfs--write-post-attributes writer directory call)
      (xdr-write-fixed-opaque writer current)
      (let ((used (length (xdr-writer-octets writer)))
            (eof  t))
        (loop for name in (nthcdr cookie names)
              for index from (1+ cookie)
              do (let* ((path   (nfs--lookup-name server directory name))
                        (status (nfs--status path))
                        (entry  (xdr-writer-create)))
                   (when status
                     (xdr-write-boolean entry t)
                     (xdr-write-hyper entry (ldb (byte 64 0) (sb-posix:stat-ino status)))
                     (xdr-write-string entry name)
                     (xdr-write-hyper entry index)
                     (when plus
                       (xdr-write-boolean entry t)
                       (nfs--write-attributes entry path status call)
                       (xdr-write-boolean entry t)
                       (xdr-write-opaque entry (nfs--handle server path)))
                     (when (> (+ used (length (xdr-writer-octets entry)) 8) limit)
                       (setf eof nil)
                       (return))
                     (incf used (length (xdr-writer-octets entry)))
                     (xdr-write-fixed-opaque writer (xdr-writer-octets entry)))))
        (xdr-write-boolean writer nil)
        (xdr-write-boolean writer eof)))))


;;;; -- MOUNT Procedures --

(defun nfs-mount-procedure (server call writer)
  "Perform MOUNT CALL's procedure, appending its results to WRITER. Return
:UNAVAILABLE for an unknown procedure."
  (let ((reader (rpc-call-arguments call)))
    (case (rpc-call-procedure call)
      ((0 3 4)
       (when (= (rpc-call-procedure call) 3)
         (xdr-read-string reader *nfs-maximum-path*))
       nil)
      (1
       (let* ((name   (xdr-read-string reader *nfs-maximum-path*))
              (export (find name (nfs-server-exports server) :key #'nfs-export-name :test #'string=)))
         (cond (export
                (xdr-write-unsigned writer *nfs-ok*)
                (xdr-write-opaque writer (nfs--handle server (nfs-export-root export)))
                (xdr-write-unsigned writer 1)
                (xdr-write-unsigned writer *rpc-authentication-unix*))
               (t
                (xdr-write-unsigned writer *nfs-error-access*)))))
      (2
       (xdr-write-boolean writer nil))
      (5
       (dolist (export (nfs-server-exports server))
         (xdr-write-boolean writer t)
         (xdr-write-string writer (nfs-export-name export))
         (xdr-write-boolean writer nil))
       (xdr-write-boolean writer nil))
      (otherwise
       :unavailable))))


;;;; -- Dispatch and Connections --

(defparameter *nfs-failure-attributes*
  '((1 . 0) (2 . 2) (3 . 1) (4 . 1) (5 . 1) (6 . 1) (7 . 2) (8 . 2) (9 . 2) (10 . 2)
    (11 . 2) (12 . 2) (13 . 2) (14 . 4) (15 . 3) (16 . 1) (17 . 1) (18 . 1) (19 . 1)
    (20 . 1) (21 . 2))
  "For each NFS procedure, how many optional attribute items follow the
status in its failure reply: post_op_attr counts one and wcc_data two.")

(defun nfs--failure-results (call writer status)
  "Append the results of NFS CALL failing with STATUS: the status and absent
attributes, which clients refresh afterward."
  (xdr-write-unsigned writer status)
  (loop repeat (or (cdr (assoc (rpc-call-procedure call) *nfs-failure-attributes*)) 0)
        do (xdr-write-boolean writer nil)))

(defun nfs-server-dispatch (server octets)
  "Return the reply record for the call record OCTETS. Signal XDR-ERROR when
OCTETS is not a well-formed call, which ends the connection."
  (let* ((call    (rpc-call-decode octets))
         (xid     (rpc-call-xid call))
         (program (rpc-call-program call)))
    (cond ((not (member program (list *nfs-program* *nfs-mount-program*)))
           (xdr-writer->octets (rpc-reply-header xid *rpc-accept-program-unavailable*)))
          ((/= (rpc-call-version call) *nfs-version*)
           (let ((writer (rpc-reply-header xid *rpc-accept-program-mismatch*)))
             (xdr-write-unsigned writer *nfs-version*)
             (xdr-write-unsigned writer *nfs-version*)
             (xdr-writer->octets writer)))
          (t
           (let* ((writer (rpc-reply-header xid *rpc-accept-success*))
                  (start  (length (xdr-writer-octets writer))))
             (flet ((fail (status)
                      (setf (fill-pointer (xdr-writer-octets writer)) start)
                      (nfs--failure-results call writer status)
                      (xdr-writer->octets writer)))
               (handler-case
                   (if (eq (sb-thread:with-mutex ((nfs-server-lock server))
                             (let ((*nfs-server* server))
                               (if (= program *nfs-program*)
                                   (nfs-procedure server call writer)
                                   (nfs-mount-procedure server call writer))))
                           :unavailable)
                       (xdr-writer->octets (rpc-reply-header xid *rpc-accept-procedure-unavailable*))
                       (xdr-writer->octets writer))
                 (xdr-error ()
                   (xdr-writer->octets (rpc-reply-header xid *rpc-accept-garbage-arguments*)))
                 (nfs-status (condition)
                   (fail (nfs-status-code condition)))
                 (error (condition)
                   (format *error-output* "~&NFS procedure ~D failed: ~A~%"
                           (rpc-call-procedure call) condition)
                   (fail *nfs-error-server*)))))))))

(defun nfs-server-serve-connection (server stream)
  "Answer call records from octet STREAM until the peer closes it or sends
malformed data."
  (handler-case
      (loop for record = (rpc-read-record stream)
            while record
            do (rpc-write-record stream (nfs-server-dispatch server record)))
    (xdr-error ()
      nil)
    (stream-error ()
      nil)))

(defun nfs-server-listen (server &key (address "127.0.0.1") (port 0))
  "Serve SERVER on TCP ADDRESS and PORT, zero choosing a free port, one
thread per connection. Return the listening socket and the port it bound."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener (sb-bsd-sockets:make-inet-address address) port)
    (sb-bsd-sockets:socket-listen listener 16)
    (sb-thread:make-thread
     (lambda ()
       (handler-case
           (loop
             (let ((connection (sb-bsd-sockets:socket-accept listener)))
               (sb-thread:make-thread
                (lambda ()
                  (unwind-protect
                       (nfs-server-serve-connection
                        server
                        (sb-bsd-sockets:socket-make-stream connection :input t :output t
                                                                      :element-type '(unsigned-byte 8)
                                                                      :buffering ':full))
                    (sb-bsd-sockets:socket-close connection)))
                :name "NFS connection")))
         (sb-bsd-sockets:socket-error ()
           nil)))
     :name "NFS listener")
    (values listener (nth-value 1 (sb-bsd-sockets:socket-name listener)))))
