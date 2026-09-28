(in-package #:autolith)

;;;; -- Base Conditions --

(define-condition autolith-control-condition (condition)
  ((message
    :initarg :message
    :reader autolith-control-condition-message
    :type string
    :documentation "A concise explanation of the requested control transfer."))
  (:documentation "The base condition for non-error Autolith control transfers.")
  (:report (lambda (condition stream)
             (write-string (autolith-control-condition-message condition) stream))))

(define-condition autolith-error (error)
  ((message
    :initarg :message
    :reader autolith-error-message
    :type string
    :documentation "A concise explanation suitable for the terminal."))
  (:documentation "The base condition for expected Autolith failures.")
  (:report (lambda (condition stream)
             (write-string (autolith-error-message condition) stream))))
