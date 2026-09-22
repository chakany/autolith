(in-package #:autolith)

;;;; -- Bounded Artifact Channel --

(deftype rump-artifact-bytes ()
  "An owned vector of binary artifact or protocol bytes."
  '(simple-array (unsigned-byte 8) (*)))

(defclass rump-artifact-grant ()
  ((slot :initarg :slot :reader rump-artifact-slot
         :documentation "Session-local identifier chosen by the host.")
   (read-p :initarg :read-p :reader rump-artifact-read-p
           :documentation "Whether the peer may read this snapshot.")
   (write-p :initarg :write-p :reader rump-artifact-write-p
            :documentation "Whether the peer may stage replacements.")
   (limit :initarg :limit :reader rump-artifact-limit
          :documentation "Maximum staged artifact size, at most 4096 bytes.")
   (bytes :initarg :bytes :accessor rump-artifact-data
          :documentation "Owned input snapshot or staged output.")
   (revision :initarg :revision :accessor rump-artifact-revision
             :documentation "Monotonic session revision for compare-and-swap writes.")
   (dirty-p :initform nil :accessor rump-artifact-dirty-p
            :documentation "Whether an authorized write has been staged."))
  (:documentation "A host grant; no guest-controlled pathname is represented."))

(defclass rump-artifact-session ()
  ((grants :initarg :grants :reader rump-artifact-grants
           :documentation "Private copies of host grants indexed by slot.")
   (requests :initform 0 :accessor rump-artifact-requests
             :documentation "Request frames charged before receiving their payloads.")
   (traffic :initform 0 :accessor rump-artifact-traffic
            :documentation "Total framed request and response bytes charged.")
   (request-limit :initarg :request-limit :reader rump-artifact-request-limit
                  :documentation "Maximum request count, including FINISH.")
   (byte-limit :initarg :byte-limit :reader rump-artifact-byte-limit
               :documentation "Maximum aggregate framed traffic in both directions.")
   (statuses :initform (make-array 5 :initial-element 0)
             :reader rump-artifact-statuses
             :documentation "Bounded counters for response codes zero through four.")
   (state :initform ':new :accessor rump-artifact-state
          :documentation "Lifecycle: NEW, SERVING, COMPLETE, or ABORTED."))
  (:documentation "One single-use, single-threaded artifact transaction."))

(define-condition rump-artifact-channel-error (error)
  ((reason :initarg :reason :reader rump-artifact-error-reason
           :documentation "Host-selected failure category, never peer text."))
  (:report (lambda (condition stream)
             (format stream "Artifact channel stopped: ~A."
                     (rump-artifact-error-reason condition))))
  (:documentation "A terminal framing, budget, transport, or lifecycle failure."))

(-> rump-artifact--fail (keyword) nil)
(defun rump-artifact--fail (reason)
  "End the transaction with a typed terminal failure."
  (error 'rump-artifact-channel-error :reason reason))

(-> rump-artifact-grant (&key (:slot (integer 1 65535)) (:read-p boolean)
                              (:write-p boolean) (:limit (integer 0 4096))
                              (:bytes rump-artifact-bytes)
                              (:revision (unsigned-byte 32))) rump-artifact-grant)
(defun rump-artifact-grant (&key slot read-p write-p (limit 4096)
                                (bytes (make-array 0 :element-type '(unsigned-byte 8)))
                                (revision 0))
  "Create a grant from trusted host policy, copying BYTES."
  (check-type slot (integer 1 65535))
  (check-type read-p boolean)
  (check-type write-p boolean)
  (check-type limit (integer 0 4096))
  (check-type bytes rump-artifact-bytes)
  (check-type revision (unsigned-byte 32))
  (unless (<= (length bytes) limit)
    (rump-artifact--fail ':invalid-grant))
  (make-instance 'rump-artifact-grant :slot slot :read-p read-p :write-p write-p
                 :limit limit :bytes (copy-seq bytes) :revision revision))

(-> rump-artifact-session (list &key (:request-limit (integer 1 1024))
                                   (:byte-limit (integer 1 1048576))) rump-artifact-session)
(defun rump-artifact-session (grants &key (request-limit 16) (byte-limit 16384))
  "Snapshot GRANTS into an independent transaction; reject duplicate slots."
  (check-type request-limit (integer 1 1024))
  (check-type byte-limit (integer 1 1048576))
  (let ((table (make-hash-table)))
    (dolist (grant grants)
      (check-type grant rump-artifact-grant)
      (when (gethash (rump-artifact-slot grant) table)
        (rump-artifact--fail ':duplicate-grant))
      (setf (gethash (rump-artifact-slot grant) table)
            (rump-artifact-grant :slot (rump-artifact-slot grant)
                                 :read-p (rump-artifact-read-p grant)
                                 :write-p (rump-artifact-write-p grant)
                                 :limit (rump-artifact-limit grant)
                                 :bytes (rump-artifact-data grant)
                                 :revision (rump-artifact-revision grant))))
    (make-instance 'rump-artifact-session :grants table
                   :request-limit request-limit :byte-limit byte-limit)))

(-> rump-artifact--uint (rump-artifact-bytes (integer 0 *) (integer 1 4))
                      (unsigned-byte 32))
(defun rump-artifact--uint (bytes offset width)
  "Decode an unsigned big-endian integer from a validated buffer window."
  (loop for index from offset below (+ offset width)
        for value = (aref bytes index) then (+ (ash value 8) (aref bytes index))
        finally (return value)))

(-> rump-artifact--put-uint (&key (:bytes rump-artifact-bytes) (:offset (integer 0 *))
                                (:width (integer 1 4)) (:value (unsigned-byte 32))) null)
(defun rump-artifact--put-uint (&key bytes offset width value)
  "Encode an unsigned big-endian integer into a validated buffer window."
  (dotimes (index width)
    (setf (aref bytes (+ offset index))
          (ldb (byte 8 (* 8 (- width index 1))) value)))
  nil)

(-> rump-artifact-packet (&key (:code (unsigned-byte 8)) (:slot (unsigned-byte 16))
                             (:revision (unsigned-byte 32)) (:data rump-artifact-bytes))
                         rump-artifact-bytes)
(defun rump-artifact-packet (&key (code 0) (slot 0) (revision 0)
                                 (data (make-array 0 :element-type '(unsigned-byte 8))))
  "Encode a version-one request or response payload, without length prefix."
  (let ((bytes (make-array (+ 8 (length data)) :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
    (setf (aref bytes 0) 1 (aref bytes 1) code)
    (rump-artifact--put-uint :bytes bytes :offset 2 :width 2 :value slot)
    (rump-artifact--put-uint :bytes bytes :offset 4 :width 4 :value revision)
    (replace bytes data :start1 8)
    bytes))

(-> rump-artifact--dispatch (rump-artifact-session rump-artifact-bytes)
                          (values rump-artifact-bytes boolean))
(defun rump-artifact--dispatch (session packet)
  "Authorize one bounded request. Return response and whether FINISH was valid.
Codes: 0 success, 1 denied, 2 stale, 3 capacity exhausted, 4 malformed."
  (let* ((version  (aref packet 0))
         (opcode   (aref packet 1))
         (slot     (rump-artifact--uint packet 2 2))
         (revision (rump-artifact--uint packet 4 4))
         (size     (- (length packet) 8))
         (grant    (gethash slot (rump-artifact-grants session))))
    (flet ((reply (code &optional (revision 0))
             (values (rump-artifact-packet :code code :slot slot :revision revision) nil)))
      (cond
        ((or (/= version 1) (not (member opcode '(1 2 3)))
             (and (= opcode 1) (or (plusp size) (plusp revision)))
             (and (= opcode 3) (or (plusp slot) (plusp size) (plusp revision))))
         (reply 4))
        ((= opcode 3)
         (values (rump-artifact-packet) t))
        ((or (null grant)
             (and (= opcode 1) (not (rump-artifact-read-p grant)))
             (and (= opcode 2) (not (rump-artifact-write-p grant))))
         (reply 1))
        ((= opcode 1)
         (values (rump-artifact-packet :slot slot :revision (rump-artifact-revision grant)
                                      :data (rump-artifact-data grant)) nil))
        ((/= revision (rump-artifact-revision grant))
         (reply 2 (rump-artifact-revision grant)))
        ((or (> size (rump-artifact-limit grant)) (= revision #xffffffff))
         (reply 3 (rump-artifact-revision grant)))
        (t
         (setf (rump-artifact-data grant) (subseq packet 8)
               (rump-artifact-dirty-p grant) t)
         (incf (rump-artifact-revision grant))
         (reply 0 (rump-artifact-revision grant)))))))

(-> rump-artifact--read-exact (stream (integer 0 4104)) rump-artifact-bytes)
(defun rump-artifact--read-exact (input length)
  "Read a bounded binary window; EOF at any position is terminal."
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length bytes)
      (let ((byte (read-byte input nil nil)))
        (unless byte
          (rump-artifact--fail ':truncated))
        (setf (aref bytes index) byte)))))

(-> rump-artifact--read-frame (rump-artifact-session stream) rump-artifact-bytes)
(defun rump-artifact--read-frame (session input)
  "Check declared length and remaining traffic before allocating its payload."
  (when (>= (rump-artifact-requests session) (rump-artifact-request-limit session))
    (rump-artifact--fail ':request-limit))
  (when (> (+ 4 (rump-artifact-traffic session)) (rump-artifact-byte-limit session))
    (rump-artifact--fail ':byte-limit))
  (let ((length (rump-artifact--uint (rump-artifact--read-exact input 4) 0 4)))
    (unless (<= 8 length 4104)
      (rump-artifact--fail ':frame-size))
    (when (> (+ 4 length (rump-artifact-traffic session))
             (rump-artifact-byte-limit session))
      (rump-artifact--fail ':byte-limit))
    (incf (rump-artifact-traffic session) (+ 4 length))
    (incf (rump-artifact-requests session))
    (rump-artifact--read-exact input length)))

(-> rump-artifact--write-frame (rump-artifact-session stream rump-artifact-bytes) null)
(defun rump-artifact--write-frame (session output packet)
  "Charge the bounded response before sending any of it."
  (when (> (+ 4 (length packet) (rump-artifact-traffic session))
           (rump-artifact-byte-limit session))
    (rump-artifact--fail ':byte-limit))
  (let ((prefix (make-array 4 :element-type '(unsigned-byte 8))))
    (rump-artifact--put-uint :bytes prefix :offset 0 :width 4 :value (length packet))
    (incf (rump-artifact-traffic session) (+ 4 (length packet)))
    (write-sequence prefix output)
    (write-sequence packet output)
    (finish-output output))
  nil)

(-> rump-artifact-serve (rump-artifact-session stream stream) rump-artifact-session)
(defun rump-artifact-serve (session input output)
  "Serve a single transaction. The transport owner must enforce a wall deadline.
Only FINISH followed by a successfully flushed response commits the session.
Every abnormal exit aborts it and removes staged writable data."
  (unless (eq (rump-artifact-state session) ':new)
    (rump-artifact--fail ':reused-session))
  (setf (rump-artifact-state session) ':serving)
  (unwind-protect
       (loop
         (multiple-value-bind (response finish-p)
             (rump-artifact--dispatch session (rump-artifact--read-frame session input))
           (incf (aref (rump-artifact-statuses session) (aref response 1)))
           (rump-artifact--write-frame session output response)
           (when finish-p
             (setf (rump-artifact-state session) ':complete)
             (return session))))
    (unless (eq (rump-artifact-state session) ':complete)
      (setf (rump-artifact-state session) ':aborted)
      (maphash (lambda (slot grant)
                 (declare (ignore slot))
                 (when (rump-artifact-write-p grant)
                   (setf (rump-artifact-data grant)
                         (make-array 0 :element-type '(unsigned-byte 8))
                         (rump-artifact-dirty-p grant) nil)))
               (rump-artifact-grants session)))))

(-> rump-artifact-outputs (rump-artifact-session) list)
(defun rump-artifact-outputs (session)
  "Return copied (SLOT REVISION BYTES) outputs only from a completed session.
The trusted caller decides whether and where to publish these untrusted bytes."
  (unless (eq (rump-artifact-state session) ':complete)
    (rump-artifact--fail ':uncommitted))
  (let ((outputs nil))
    (maphash (lambda (slot grant)
               (when (rump-artifact-dirty-p grant)
                 (push (list slot (rump-artifact-revision grant)
                             (copy-seq (rump-artifact-data grant))) outputs)))
             (rump-artifact-grants session))
    (sort outputs #'< :key #'first)))
