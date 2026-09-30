(in-package #:autolith)

;;;; -- Broker Response Stream --

(-> broker-response--read-frame (stream) t)
(defun broker-response--read-frame (stream)
  "Read a bounded frame, preserving sanitized broker failure categories."
  (let ((frame (management-repl-read-frame stream *broker-maximum-frame-size*)))
    (when (or (equal frame '(:broker-result :status :failed))
              (and (listp frame)
                   (eql (ignore-errors (list-length frame)) 5)
                   (equal (subseq frame 0 4)
                          '(:broker-result :status :failed :reason))
                   (member (fifth frame) '(:target :payload :authentication :handler))))
      (let ((reason (if (= (length frame) 5) (fifth frame) ':handler)))
        (error 'broker-unavailable
               :reason reason
               :message
               (case reason
                 (:target
                  "The requested provider or model is unavailable in the broker's trusted configuration. Private agent mutations do not update the broker.")
                 (:payload
                  "The broker rejected the provider request payload.")
                 (:authentication
                  "The broker could not authenticate with the provider. Run autolith auth in a terminal.")
                 (otherwise
                  "The broker could not complete the provider request.")))))
    frame))

(defclass broker-response-stream (sb-gray:fundamental-character-input-stream)
  ((source
    :initarg :source
    :reader broker-response-stream-source
    :type stream
    :documentation "The bounded binary frame stream from the broker.")
   (chunk
    :initform ""
    :accessor broker-response-stream-chunk
    :type string
    :documentation "The current validated text chunk.")
   (position
    :initform 0
    :accessor broker-response-stream-position
    :type (integer 0)
    :documentation "The next character in the current chunk.")
   (ended-p
    :initform nil
    :accessor broker-response-stream-ended-p
    :type boolean
    :documentation "Whether the broker sent its terminal frame."))
  (:documentation "A character stream over bounded broker response frames."))

(-> broker-response-open (stream) (values broker-response-stream integer))
(defun broker-response-open (stream)
  "Validate the broker's opening response and return a streaming body."
  (let ((frame (broker-response--read-frame stream)))
    (unless (and (listp frame)
                 (eql (ignore-errors (list-length frame)) 5)
                 (eq (first frame) ':broker-result)
                 (eq (second frame) ':status)
                 (eq (third frame) ':open)
                 (eq (fourth frame) ':code)
                 (integerp (fifth frame))
                 (<= 100 (fifth frame) 599))
      (error 'broker-protocol-error
             :message "The broker returned an invalid opening frame."
             :reason ':response))
    (values (make-instance 'broker-response-stream :source stream)
            (fifth frame))))

(-> broker-response-stream--next-chunk (broker-response-stream) null)
(defun broker-response-stream--next-chunk (response)
  "Advance RESPONSE to its next validated text chunk or terminal frame."
  (loop
    for frame = (broker-response--read-frame
                 (broker-response-stream-source response))
    do (cond
         ((equal frame '(:broker-end))
          (setf (broker-response-stream-ended-p response) t)
          (return))
         ((and (listp frame)
               (eql (ignore-errors (list-length frame)) 3)
               (eq (first frame) ':broker-chunk)
               (eq (second frame) ':text)
               (stringp (third frame))
               (plusp (length (third frame))))
          (setf (broker-response-stream-chunk response) (third frame)
                (broker-response-stream-position response) 0)
          (return))
         (t
          (error 'broker-protocol-error
                 :message "The broker returned an invalid streaming frame."
                 :reason ':response))))
  nil)

(defmethod sb-gray:stream-read-char ((response broker-response-stream))
  "Read one character from RESPONSE, fetching only validated frames."
  (loop
    (when (broker-response-stream-ended-p response)
      (return :eof))
    (let ((chunk (broker-response-stream-chunk response))
          (position (broker-response-stream-position response)))
      (when (< position (length chunk))
        (incf (broker-response-stream-position response))
        (return (char chunk position))))
    (broker-response-stream--next-chunk response)))
