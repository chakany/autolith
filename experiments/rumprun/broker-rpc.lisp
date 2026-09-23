;;;; ONC RPC (RFC 5531) and XDR (RFC 4506) over TCP for the host broker.
;;;; Guests are untrusted: every length is bounded before allocation, and
;;;; malformed input signals XDR-ERROR rather than reaching the services.
(unless (find-package '#:autolith) (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

;;;; -- Types and Conditions --

(defclass xdr-reader ()
  ((octets
    :initarg :octets
    :reader xdr-reader-octets
    :type (simple-array (unsigned-byte 8) (*))
    :documentation "The encoded data.")
   (position
    :initarg :position
    :initform 0
    :accessor xdr-reader-position
    :type (integer 0)
    :documentation "The offset of the next unread octet."))
  (:documentation "A cursor decoding XDR data from an octet vector."))

(defclass xdr-writer ()
  ((octets
    :initform (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
    :reader xdr-writer-octets
    :documentation "The encoded data so far."))
  (:documentation "An accumulator encoding XDR data."))

(define-condition xdr-error (error)
  ((message
    :initarg :message
    :reader xdr-error-message
    :documentation "Why the data could not be decoded."))
  (:report (lambda (condition stream)
             (format stream "Malformed XDR data: ~A" (xdr-error-message condition))))
  (:documentation "Encoded data from a peer is malformed or exceeds a bound."))

(defclass rpc-call ()
  ((xid
    :initarg :xid
    :reader rpc-call-xid
    :documentation "The transaction identifier the reply must echo.")
   (program
    :initarg :program
    :reader rpc-call-program
    :documentation "The RPC program number.")
   (version
    :initarg :version
    :reader rpc-call-version
    :documentation "The program version.")
   (procedure
    :initarg :procedure
    :reader rpc-call-procedure
    :documentation "The procedure number.")
   (credential-flavor
    :initarg :credential-flavor
    :reader rpc-call-credential-flavor
    :documentation "The authentication flavor of the caller's credential.")
   (uid
    :initarg :uid
    :initform 0
    :reader rpc-call-uid
    :documentation "The caller's user ID from an AUTH_SYS credential, else 0.")
   (gid
    :initarg :gid
    :initform 0
    :reader rpc-call-gid
    :documentation "The caller's group ID from an AUTH_SYS credential, else 0.")
   (arguments
    :initarg :arguments
    :reader rpc-call-arguments
    :type xdr-reader
    :documentation "A reader positioned at the procedure arguments."))
  (:documentation "One decoded RPC call message."))


;;;; -- Constants --

(defparameter *rpc-maximum-record* (* 2 1024 1024)
  "The largest RPC record the broker accepts, which bounds guest memory use.")

(defparameter *rpc-message-call* 0 "The RPC message type of a call.")
(defparameter *rpc-message-reply* 1 "The RPC message type of a reply.")
(defparameter *rpc-reply-accepted* 0 "A reply to an accepted call.")
(defparameter *rpc-reply-denied* 1 "A reply to a denied call.")
(defparameter *rpc-accept-success* 0 "The call succeeded.")
(defparameter *rpc-accept-program-unavailable* 1 "The program is not served.")
(defparameter *rpc-accept-program-mismatch* 2 "The program version is not served.")
(defparameter *rpc-accept-procedure-unavailable* 3 "The procedure does not exist.")
(defparameter *rpc-accept-garbage-arguments* 4 "The arguments could not be decoded.")
(defparameter *rpc-reject-mismatch* 0 "The RPC protocol version is not 2.")
(defparameter *rpc-authentication-none* 0 "The AUTH_NONE flavor.")
(defparameter *rpc-authentication-unix* 1 "The AUTH_SYS flavor.")
(defparameter *rpc-maximum-authentication* 400 "The largest credential body RFC 5531 allows.")


;;;; -- Decoding --

(defun xdr-reader-create (octets)
  "Return a reader at the start of OCTETS."
  (make-instance 'xdr-reader :octets octets))

(defun xdr--take (reader count)
  "Return the offset of COUNT octets at READER's position and advance past
them, or signal XDR-ERROR when fewer remain."
  (let ((start (xdr-reader-position reader)))
    (when (> (+ start count) (length (xdr-reader-octets reader)))
      (error 'xdr-error :message "The data ends inside an item."))
    (setf (xdr-reader-position reader) (+ start count))
    start))

(defun xdr-read-unsigned (reader)
  "Read a 32-bit unsigned integer."
  (let ((start  (xdr--take reader 4))
        (octets (xdr-reader-octets reader)))
    (logior (ash (aref octets start) 24) (ash (aref octets (+ start 1)) 16)
            (ash (aref octets (+ start 2)) 8) (aref octets (+ start 3)))))

(defun xdr-read-signed (reader)
  "Read a 32-bit signed integer."
  (let ((value (xdr-read-unsigned reader)))
    (if (logbitp 31 value) (- value (ash 1 32)) value)))

(defun xdr-read-hyper (reader)
  "Read a 64-bit unsigned integer."
  (logior (ash (xdr-read-unsigned reader) 32) (xdr-read-unsigned reader)))

(defun xdr-read-boolean (reader)
  "Read a boolean, which must be encoded as zero or one."
  (let ((value (xdr-read-unsigned reader)))
    (case value
      (0
       nil)
      (1
       t)
      (otherwise
       (error 'xdr-error :message (format nil "Boolean encoded as ~D." value))))))

(defun xdr-read-fixed-opaque (reader count)
  "Read COUNT opaque octets and their padding."
  (let ((start (xdr--take reader count)))
    (xdr--take reader (mod (- count) 4))
    (subseq (xdr-reader-octets reader) start (+ start count))))

(defun xdr-read-opaque (reader maximum)
  "Read variable-length opaque data of at most MAXIMUM octets."
  (let ((count (xdr-read-unsigned reader)))
    (when (> count maximum)
      (error 'xdr-error :message (format nil "Opaque data of ~D octets exceeds ~D." count maximum)))
    (xdr-read-fixed-opaque reader count)))

(defun xdr-read-string (reader maximum)
  "Read a UTF-8 string of at most MAXIMUM octets."
  (let ((octets (xdr-read-opaque reader maximum)))
    (handler-case
        (sb-ext:octets-to-string octets :external-format ':utf-8)
      (error ()
        (error 'xdr-error :message "A string is not valid UTF-8.")))))

(defun xdr-reader-finished-p (reader)
  "Return true when READER has consumed all of its data."
  (= (xdr-reader-position reader) (length (xdr-reader-octets reader))))


;;;; -- Encoding --

(defun xdr-writer-create ()
  "Return an empty writer."
  (make-instance 'xdr-writer))

(defun xdr--push (writer octet)
  "Append OCTET to WRITER."
  (vector-push-extend octet (xdr-writer-octets writer)))

(defun xdr-write-unsigned (writer value)
  "Append a 32-bit unsigned integer."
  (loop for shift from 24 downto 0 by 8
        do (xdr--push writer (ldb (byte 8 shift) value)))
  writer)

(defun xdr-write-signed (writer value)
  "Append a 32-bit signed integer."
  (xdr-write-unsigned writer (ldb (byte 32 0) value)))

(defun xdr-write-hyper (writer value)
  "Append a 64-bit unsigned integer."
  (xdr-write-unsigned writer (ldb (byte 32 32) value))
  (xdr-write-unsigned writer (ldb (byte 32 0) value)))

(defun xdr-write-boolean (writer value)
  "Append VALUE as an XDR boolean."
  (xdr-write-unsigned writer (if value 1 0)))

(defun xdr-write-fixed-opaque (writer octets)
  "Append OCTETS and their padding."
  (loop for octet across octets
        do (xdr--push writer octet))
  (loop repeat (mod (- (length octets)) 4)
        do (xdr--push writer 0))
  writer)

(defun xdr-write-opaque (writer octets)
  "Append variable-length opaque data."
  (xdr-write-unsigned writer (length octets))
  (xdr-write-fixed-opaque writer octets))

(defun xdr-write-string (writer string)
  "Append STRING encoded as UTF-8."
  (xdr-write-opaque writer (sb-ext:string-to-octets string :external-format ':utf-8)))

(defun xdr-writer->octets (writer)
  "Return a simple copy of WRITER's data."
  (coerce (xdr-writer-octets writer) '(simple-array (unsigned-byte 8) (*))))


;;;; -- Record Marking --

(defun rpc-read-record (stream)
  "Read one record-marked RPC message from octet STREAM, or return NIL at a
clean end of stream. Signal XDR-ERROR for an oversized or truncated record."
  (let ((record (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (header (make-array 4 :element-type '(unsigned-byte 8))))
    (loop
      (let ((count (read-sequence header stream)))
        (cond ((and (zerop count) (zerop (length record)))
               (return nil))
              ((< count 4)
               (error 'xdr-error :message "The stream ends inside a record header."))))
      (let* ((mark   (logior (ash (aref header 0) 24) (ash (aref header 1) 16)
                             (ash (aref header 2) 8) (aref header 3)))
             (length (ldb (byte 31 0) mark))
             (start  (length record)))
        (when (> (+ start length) *rpc-maximum-record*)
          (error 'xdr-error :message "A record exceeds the broker's size limit."))
        (adjust-array record (+ start length) :fill-pointer (+ start length))
        (unless (= (read-sequence record stream :start start) (+ start length))
          (error 'xdr-error :message "The stream ends inside a record."))
        (when (logbitp 31 mark)
          (return (coerce record '(simple-array (unsigned-byte 8) (*)))))))))

(defun rpc-write-record (stream octets)
  "Write OCTETS to STREAM as one record-marked message and flush it."
  (let ((mark (logior (ash 1 31) (length octets))))
    (write-sequence (vector (ldb (byte 8 24) mark) (ldb (byte 8 16) mark)
                            (ldb (byte 8 8) mark) (ldb (byte 8 0) mark))
                    stream))
  (write-sequence octets stream)
  (finish-output stream)
  nil)


;;;; -- Calls and Replies --

(defun rpc--decode-credential (flavor body)
  "Return the user and group IDs an AUTH_SYS credential BODY names, as two
values, or 0 and 0 for any other FLAVOR."
  (if (= flavor *rpc-authentication-unix*)
      (let ((reader (xdr-reader-create body)))
        (xdr-read-unsigned reader)
        (xdr-read-string reader 255)
        (let ((uid (xdr-read-unsigned reader))
              (gid (xdr-read-unsigned reader)))
          (let ((count (xdr-read-unsigned reader)))
            (when (> count 16)
              (error 'xdr-error :message "An AUTH_SYS credential lists over 16 groups."))
            (loop repeat count do (xdr-read-unsigned reader)))
          (values uid gid)))
      (values 0 0)))

(defun rpc-call-decode (octets)
  "Decode a call message from OCTETS. Signal XDR-ERROR when it is not a
well-formed version 2 call."
  (let* ((reader (xdr-reader-create octets))
         (xid    (xdr-read-unsigned reader)))
    (unless (= (xdr-read-unsigned reader) *rpc-message-call*)
      (error 'xdr-error :message "The message is not a call."))
    (let* ((rpc-version (xdr-read-unsigned reader))
           (program     (xdr-read-unsigned reader))
           (version     (xdr-read-unsigned reader))
           (procedure   (xdr-read-unsigned reader))
           (flavor      (xdr-read-unsigned reader)))
      (multiple-value-bind (uid gid)
          (rpc--decode-credential flavor (xdr-read-opaque reader *rpc-maximum-authentication*))
        (xdr-read-unsigned reader)
        (xdr-read-opaque reader *rpc-maximum-authentication*)
        (unless (= rpc-version 2)
          (error 'xdr-error :message (format nil "RPC version ~D is not 2." rpc-version)))
        (make-instance 'rpc-call :xid xid :program program :version version
                                 :procedure procedure :credential-flavor flavor
                                 :uid uid :gid gid :arguments reader)))))

(defun rpc-reply-header (xid accept-status)
  "Return a writer holding an accepted reply to XID with ACCEPT-STATUS and an
AUTH_NONE verifier; the caller appends any results."
  (let ((writer (xdr-writer-create)))
    (xdr-write-unsigned writer xid)
    (xdr-write-unsigned writer *rpc-message-reply*)
    (xdr-write-unsigned writer *rpc-reply-accepted*)
    (xdr-write-unsigned writer *rpc-authentication-none*)
    (xdr-write-unsigned writer 0)
    (xdr-write-unsigned writer accept-status)
    writer))
