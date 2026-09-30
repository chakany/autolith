(in-package #:autolith)

;;;; -- Credential Broker Socket --

(defclass broker-server ()
  ((pathname
    :initarg :pathname
    :reader broker-server-pathname
    :type pathname
    :documentation "The launch-specific Unix socket pathname.")
   (handler
    :initarg :handler
    :reader broker-server-handler
    :type function
    :documentation "Trusted function called with a validated request and frame writer.")
   (capability
    :initarg :capability
    :initform nil
    :reader broker-server-capability
    :type (or null string)
    :documentation "Per-launch request capability, or NIL for an internal server.")
   (listener
    :initform nil
    :accessor broker-server-listener
    :documentation "The listening socket, or NIL after shutdown.")
   (identity
    :initform nil
    :accessor broker-server-identity
    :documentation "The filesystem identity of the bound socket.")
   (clients
    :initform nil
    :accessor broker-server-clients
    :type list
    :documentation "Accepted sockets currently owned by the server.")
   (threads
    :initform nil
    :accessor broker-server-threads
    :type list
    :documentation "Client threads currently owned by the server.")
   (lock
    :initform (make-lock "Credential broker clients")
    :reader broker-server-lock
    :documentation "Protects lifecycle state and client ownership.")
   (stopping-p
    :initform nil
    :accessor broker-server-stopping-p
    :type boolean
    :documentation "Whether the listener is stopping."))
  (:documentation "A bounded Unix socket endpoint in the trusted broker process."))

(defparameter *broker-maximum-clients* 8
  "The maximum concurrent clients accepted by one broker process.")

(define-condition broker-server-error (autolith-error)
  ((reason
    :initarg :reason
    :reader broker-server-error-reason
    :type keyword
    :documentation "The failed broker socket lifecycle stage."))
  (:documentation "The trusted broker endpoint could not be started safely."))

(-> broker-server-create (pathname function &key (:capability (or null string))) broker-server)
(defun broker-server-create (pathname handler &key capability)
  "Create a server for a launch-specific socket and trusted HANDLER."
  (make-instance 'broker-server :pathname pathname :handler handler
                 :capability capability))

(-> broker-server--authorized-p (broker-server list) boolean)
(defun broker-server--authorized-p (server request)
  "Compare the request capability without ending on the first mismatch."
  (let ((expected (broker-server-capability server))
        (provided (getf (rest request) ':capability)))
    (if (null expected)
        t
        (let ((difference (logxor (length expected) (length provided))))
          (loop for index below (max (length expected) (length provided))
                do (setf difference
                         (logior difference
                                 (logxor (if (< index (length expected))
                                             (char-code (char expected index)) 0)
                                         (if (< index (length provided))
                                             (char-code (char provided index)) 0)))))
          (zerop difference)))))

(-> broker-server--prepare-path (pathname) null)
(defun broker-server--prepare-path (pathname)
  "Require an absent socket under an owned private directory without links."
  (let* ((directory (uiop:pathname-directory-pathname pathname))
         (status (platform-path-status *platform* directory)))
    (unless (and status
                 (eq (platform-file-status-kind status) ':directory)
                 (platform-file-status-owned-p status)
                 (platform-file-status-private-p status)
                 (null (platform-path-status *platform* pathname)))
      (error 'broker-server-error
             :message "The broker socket path is occupied or its directory is not private."
             :reason ':path)))
  nil)

(-> broker-server-start (broker-server) broker-server)
(defun broker-server-start (server)
  "Bind SERVER's Unix socket after verifying its private launch directory."
  (unless (platform-supports-p *platform* ':local-sockets)
    (error 'broker-server-error
           :message "This host cannot start a credential broker Unix socket."
           :reason ':unsupported))
  (broker-server--prepare-path (broker-server-pathname server))
  (let ((listener (platform-local-listener
                   *platform* (broker-server-pathname server)
                   :backlog *broker-maximum-clients*)))
    (handler-case
        (let ((identity (platform-path-status
                         *platform* (broker-server-pathname server))))
          (unless (and identity
                       (eq (platform-file-status-kind identity) ':socket)
                       (platform-file-status-owned-p identity)
                       (platform-file-status-private-p identity))
            (error 'broker-server-error
                   :message "The broker socket was not created privately."
                   :reason ':bind))
          (setf (broker-server-listener server) listener
                (broker-server-identity server) identity)
          server)
      (error (condition)
        (ignore-errors (sb-bsd-sockets:socket-close listener))
        (error condition)))))

(-> broker-server--write-failure (stream) null)
(defun broker-server--write-failure (stream)
  "Report failure without exposing a trusted handler condition or credential."
  (ignore-errors
    (broker-write-frame stream '(:broker-result :status :failed)))
  nil)

(-> broker-server--serve-client (broker-server sb-bsd-sockets:socket) null)
(defun broker-server--serve-client (server socket)
  "Read one request and let SERVER's trusted handler emit bounded frames."
  (unwind-protect
       (let ((stream (sb-bsd-sockets:socket-make-stream
                      socket :input t :output t
                      :element-type '(unsigned-byte 8)
                      :buffering ':none :timeout 10)))
         (unwind-protect
              (handler-case
                  (let ((request (broker-read-request stream)))
                    (when request
                      (unless (broker-server--authorized-p server request)
                        (error 'broker-protocol-error
                               :message "The broker request is not authorized."
                               :reason ':capability))
                      (funcall
                       (broker-server-handler server)
                       request
                       (lambda (frame)
                         (broker-write-frame stream frame)))))
                (error ()
                  (broker-server--write-failure stream)))
           (ignore-errors (close stream))))
    (with-lock-held ((broker-server-lock server))
      (setf (broker-server-clients server)
            (delete socket (broker-server-clients server))
            (broker-server-threads server)
            (delete (current-thread) (broker-server-threads server))))
    (ignore-errors (sb-bsd-sockets:socket-close socket)))
  nil)

(-> broker-server-serve (broker-server) null)
(defun broker-server-serve (server)
  "Accept bounded clients until SERVER is closed, then release its socket."
  (unless (broker-server-listener server)
    (broker-server-start server))
  (unwind-protect
       (loop
         (let ((socket
                 (handler-case
                     (sb-bsd-sockets:socket-accept
                      (broker-server-listener server))
                   (error ()
                     (return)))))
           (with-lock-held ((broker-server-lock server))
             (if (or (broker-server-stopping-p server)
                     (>= (length (broker-server-clients server))
                         *broker-maximum-clients*))
                 (ignore-errors (sb-bsd-sockets:socket-close socket))
                 (handler-case
                     (let ((thread
                             (make-thread
                              (lambda ()
                                (broker-server--serve-client server socket))
                              :name "Credential broker client")))
                       (push socket (broker-server-clients server))
                       (push thread (broker-server-threads server)))
                   (error ()
                     (ignore-errors (sb-bsd-sockets:socket-close socket))))))))
    (broker-server-close server))
  nil)

(-> broker-server-close (broker-server) null)
(defun broker-server-close (server)
  "Stop SERVER and remove only the exact socket it bound."
  (with-lock-held ((broker-server-lock server))
    (setf (broker-server-stopping-p server) t)
    (when (broker-server-listener server)
      (ignore-errors
        (sb-bsd-sockets:socket-close (broker-server-listener server)))
      (setf (broker-server-listener server) nil))
    (dolist (socket (broker-server-clients server))
      (ignore-errors (sb-bsd-sockets:socket-close socket))))
  (dolist (thread (broker-server-threads server))
    (unless (eq thread (current-thread))
      (ignore-errors (join-thread thread))))
  (with-lock-held ((broker-server-lock server))
    (let ((current (platform-path-status
                    *platform* (broker-server-pathname server))))
      (when (and current
                 (broker-server-identity server)
                 (eq (platform-file-status-kind current) ':socket)
                 (platform-file-status-same-object-p
                  current (broker-server-identity server)))
        (delete-file (broker-server-pathname server))
        (setf (broker-server-identity server) nil))))
  nil)
