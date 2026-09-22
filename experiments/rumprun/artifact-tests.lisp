(in-package #:autolith)

;;;; -- Broker Contract Tests --

(-> rump-artifact-test-wire (list) rump-artifact-bytes)
(defun rump-artifact-test-wire (packets)
  "Frame test packets into one owned byte vector."
  (let* ((length (loop for packet in packets sum (+ 4 (length packet))))
         (wire (make-array length :element-type '(unsigned-byte 8)))
         (offset 0))
    (dolist (packet packets wire)
      (rump-artifact--put-uint :bytes wire :offset offset :width 4 :value (length packet))
      (incf offset 4)
      (replace wire packet :start1 offset)
      (incf offset (length packet)))))

(-> rump-artifact-test-exchange (rump-artifact-bytes rump-artifact-session)
                              (values rump-artifact-session list))
(defun rump-artifact-test-exchange (wire session)
  "Exercise real framing through binary streams and return decoded replies."
  (uiop:with-temporary-file (:stream input :direction ':io
                            :element-type '(unsigned-byte 8))
    (uiop:with-temporary-file (:stream output :direction ':io
                              :element-type '(unsigned-byte 8))
      (write-sequence wire input)
      (file-position input 0)
      (rump-artifact-serve session input output)
      (file-position output 0)
      (let ((responses nil))
        (loop while (< (file-position output) (file-length output))
              for length = (rump-artifact--uint (rump-artifact--read-exact output 4) 0 4)
              do (push (rump-artifact--read-exact output length) responses))
        (values session (nreverse responses))))))

(-> rump-artifact-test-error (keyword function) null)
(defun rump-artifact-test-error (reason function)
  "Require a specific typed channel failure from FUNCTION."
  (handler-case
      (progn (funcall function) (test-assert nil "Expected a channel failure"))
    (rump-artifact-channel-error (condition)
      (test-assert (eq reason (rump-artifact-error-reason condition))
                   (format nil "Expected terminal category ~A" reason)))))

(-> test-rump-artifact-authorization () null)
(defun test-rump-artifact-authorization ()
  "Verify grant enforcement, revision checks, capacity checks, and wire replies."
  (test-assert (equal '(1 2 18 52 18 52 86 120 65)
                       (coerce (rump-artifact-packet :code 2 :slot #x1234
                                                    :revision #x12345678
                                                    :data (rump-artifact-ascii "A")) 'list))
               "Wire integers use big-endian layout")
  (let* ((output (rump-artifact-ascii "approved output"))
         (packets
           (list (rump-artifact-packet :code 1 :slot 1)
                 (rump-artifact-packet :code 2 :slot 1 :data output)
                 (rump-artifact-packet :code 1 :slot 65535)
                 (rump-artifact-packet :code 2 :slot 65535 :data output)
                 (rump-artifact-packet :code 1 :slot 2)
                 (rump-artifact-packet :code 99 :slot 1)
                 (rump-artifact-packet :code 2 :slot 2
                                      :data (make-array 33 :element-type '(unsigned-byte 8)
                                                          :initial-element 0))
                 (rump-artifact-packet :code 2 :slot 2 :data output)
                 (rump-artifact-packet :code 2 :slot 2 :data output)
                 (rump-artifact-packet :code 3))))
    (multiple-value-bind (session replies)
        (rump-artifact-test-exchange (rump-artifact-test-wire packets)
                                     (rump-artifact-demo-session))
      (test-assert (= 10 (length replies)) "Every complete request has one response")
      (test-assert (equalp #(0 1 1 1 1 4 3 0 2 0)
                           (map 'vector (lambda (reply) (aref reply 1)) replies))
                   "Authorization and CAS outcomes match host policy")
      (test-assert (equalp (subseq (first replies) 8) (rump-artifact-ascii "input snapshot"))
                   "Authorized read returns the snapshot")
      (loop for reply in (subseq replies 1 5)
            do (test-assert (and (= 8 (length reply))
                                  (zerop (rump-artifact--uint reply 4 4)))
                             "Denied requests reveal neither content nor revision"))
      (test-assert (= 1 (rump-artifact--uint (nth 8 replies) 4 4))
                   "Stale writer receives current authorized revision")
      (test-assert (equalp (rump-artifact-outputs session) (list (list 2 1 output)))
                   "Only authorized output is committed")
      (setf (aref (third (first (rump-artifact-outputs session))) 0) 0)
      (test-assert (equalp (rump-artifact-outputs session) (list (list 2 1 output)))
                   "Returned bytes do not alias internal storage")
      (rump-artifact-test-error ':reused-session
                               (lambda ()
                                 (rump-artifact-test-exchange
                                  (rump-artifact-test-wire (list (rump-artifact-packet :code 3)))
                                  session)))))
  nil)

(-> test-rump-artifact-data () null)
(defun test-rump-artifact-data ()
  "Verify snapshot ownership and uninterpreted binary output."
  (let* ((source (rump-artifact-ascii "snapshot"))
         (grant (rump-artifact-grant :slot 1 :read-p t :bytes source))
         (session (rump-artifact-session
                   (list grant (rump-artifact-grant :slot 2 :write-p t))))
         (binary (concatenate '(simple-array (unsigned-byte 8) (*))
                              (rump-artifact-ascii "#.(error \"never evaluate\")/../../file")
                              (make-array 256 :element-type '(unsigned-byte 8)
                                              :initial-contents (loop for n below 256 collect n)))))
    (setf (aref source 0) 0
          (aref (rump-artifact-data grant) 1) 0)
    (multiple-value-bind (session replies)
        (rump-artifact-test-exchange
         (rump-artifact-test-wire (list (rump-artifact-packet :code 1 :slot 1)
                                       (rump-artifact-packet :code 2 :slot 2 :data binary)
                                       (rump-artifact-packet :code 3))) session)
      (test-assert (equalp (subseq (first replies) 8) (rump-artifact-ascii "snapshot"))
                   "Session snapshots do not alias trusted caller buffers")
      (test-assert (equalp (third (first (rump-artifact-outputs session))) binary)
                   "Reader syntax, path syntax, and arbitrary bytes remain data")))
  (let ((grant (rump-artifact-grant :slot 2 :write-p t :revision #xffffffff)))
    (multiple-value-bind (session replies)
        (rump-artifact-test-exchange
         (rump-artifact-test-wire (list (rump-artifact-packet :code 2 :slot 2
                                                            :revision #xffffffff)
                                       (rump-artifact-packet :code 3)))
         (rump-artifact-session (list grant)))
      (test-assert (= 3 (aref (first replies) 1)) "Revision exhaustion cannot wrap")
      (test-assert (null (rump-artifact-outputs session)) "Exhausted writes produce no output")))
  nil)

(-> test-rump-artifact-failures () null)
(defun test-rump-artifact-failures ()
  "Reject malformed framing, enforce aggregate bounds, and discard aborted writes."
  (dolist (case '((:truncated ()) (:truncated (0 0))
                  (:frame-size (0 0 0 7)) (:frame-size (0 0 16 9))
                  (:frame-size (255 255 255 255)) (:truncated (0 0 0 8 1 1 0))))
    (let ((session (rump-artifact-demo-session)))
      (rump-artifact-test-error
       (first case)
       (lambda ()
         (rump-artifact-test-exchange
          (make-array (length (second case)) :element-type '(unsigned-byte 8)
                                            :initial-contents (second case)) session)))
      (test-assert (eq ':aborted (rump-artifact-state session)) "Framing failure aborts transaction")
      (rump-artifact-test-error ':uncommitted (lambda () (rump-artifact-outputs session)))))
  (dolist (case '((:request-limit 1 16384) (:byte-limit 16 23) (:byte-limit 16 3)))
    (let ((session (rump-artifact-session nil :request-limit (second case)
                                            :byte-limit (third case))))
      (rump-artifact-test-error
       (first case)
       (lambda ()
         (rump-artifact-test-exchange
          (rump-artifact-test-wire (list (rump-artifact-packet :code 1 :slot 9)
                                        (rump-artifact-packet :code 3))) session)))))
  (let* ((grant (rump-artifact-grant :slot 2 :write-p t))
         (session (rump-artifact-session (list grant))))
    (rump-artifact-test-error
     ':truncated
     (lambda ()
       (rump-artifact-test-exchange
        (rump-artifact-test-wire (list (rump-artifact-packet :code 2 :slot 2
                                         :data (rump-artifact-ascii "uncommitted")))) session)))
    (test-assert (zerop (length (rump-artifact-data grant))) "Abort never mutates original grant")
    (test-assert (zerop (length (rump-artifact-data (gethash 2 (rump-artifact-grants session)))))
                 "Aborted writable data is discarded")
    (rump-artifact-test-error ':uncommitted (lambda () (rump-artifact-outputs session))))
  (let ((grant (rump-artifact-grant :slot 1)))
    (rump-artifact-test-error ':duplicate-grant
                             (lambda () (rump-artifact-session (list grant grant)))))
  nil)

(-> test-rump-artifact-boundaries () null)
(defun test-rump-artifact-boundaries ()
  "Exercise semantic malformed requests and the largest legal binary artifact."
  (let* ((bad-version (rump-artifact-packet :code 2 :slot 2
                                         :data (rump-artifact-ascii "bad version")))
         (malformed (list bad-version
                          (rump-artifact-packet :code 1 :slot 1 :revision 1)
                          (rump-artifact-packet :code 1 :slot 1 :data (rump-artifact-ascii "x"))
                          (rump-artifact-packet :code 3 :slot 1)
                          (rump-artifact-packet :code 3 :revision 1)
                          (rump-artifact-packet :code 3 :data (rump-artifact-ascii "x")))))
    (setf (aref bad-version 0) 0)
    (multiple-value-bind (session replies)
        (rump-artifact-test-exchange
         (rump-artifact-test-wire (append malformed (list (rump-artifact-packet :code 3))))
         (rump-artifact-demo-session))
      (test-assert (every (lambda (reply) (= 4 (aref reply 1))) (butlast replies))
                   "Malformed requests neither read, write, nor finish")
      (test-assert (null (rump-artifact-outputs session)) "Bad-version write has no effect")))
  (let ((bytes (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 255)))
    (multiple-value-bind (session replies)
        (rump-artifact-test-exchange
         (rump-artifact-test-wire (list (rump-artifact-packet :code 2 :slot 2 :data bytes)
                                       (rump-artifact-packet :code 2 :slot 2 :revision 1 :data bytes)
                                       (rump-artifact-packet :code 3)))
         (rump-artifact-session (list (rump-artifact-grant :slot 2 :write-p t))))
      (test-assert (every (lambda (reply) (zerop (aref reply 1))) replies)
                   "Maximum-size writes and advancing revisions are accepted")
      (test-assert (equalp (rump-artifact-outputs session) (list (list 2 2 bytes)))
                   "Maximum-size artifact survives framing without truncation")))
  (let ((session (rump-artifact-demo-session)))
    (uiop:with-temporary-file (:stream input :direction ':io :element-type '(unsigned-byte 8))
      (uiop:with-temporary-file (:stream output :direction ':io :element-type '(unsigned-byte 8))
        (write-sequence (rump-artifact-test-wire
                         (list (rump-artifact-packet :code 2 :slot 2
                                                    :data (rump-artifact-ascii "lost reply")))) input)
        (file-position input 0)
        (close output)
        (let ((failed nil))
          (handler-case (rump-artifact-serve session input output)
            (error () (setf failed t)))
          (test-assert failed "Reply transport failed"))))
    (test-assert (eq ':aborted (rump-artifact-state session)) "Failed reply aborts transaction")
    (rump-artifact-test-error ':uncommitted (lambda () (rump-artifact-outputs session))))
  nil)

(-> test-rump-artifact-guest () null)
(defun test-rump-artifact-guest ()
  "Exercise a real guest and hostile modes through the external broker."
  (let ((session (rump-artifact-run-guest)))
    (test-assert (= 10 (rump-artifact-requests session)) "Guest completed the ten-step exchange")
    (test-assert (equalp #(3 4 1 1 1) (rump-artifact-statuses session))
                 "Host independently observed allowed and rejected operations")
    (test-assert (equalp (rump-artifact-outputs session)
                         (list (list 2 1 (rump-artifact-ascii "approved output"))))
                 "Real guest output matches the authorized artifact"))
  (dolist (case '(("oversize" :frame-size) ("flood" :request-limit)
                  ("truncated" :deadline) ("stall" :deadline) ("rollback" :deadline)))
    (let ((session (rump-artifact-demo-session)))
      (rump-artifact-test-error
       (second case)
       (lambda () (rump-artifact-run-guest :mode (first case) :session session :seconds 3)))
      (test-assert (eq ':aborted (rump-artifact-state session)) "Hostile guest session is aborted")
      (rump-artifact-test-error ':uncommitted (lambda () (rump-artifact-outputs session)))
      (test-assert (zerop (length (rump-artifact-data (gethash 2 (rump-artifact-grants session)))))
                   "No staged write survives guest failure")))
  nil)

(define-test-suite "rumprun-artifact-broker"
  test-rump-artifact-authorization test-rump-artifact-data test-rump-artifact-failures
  test-rump-artifact-boundaries)

(define-test-suite "rumprun-artifact-guest" test-rump-artifact-guest)
