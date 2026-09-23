;;;; Remote process service for rumprun guests. A guest cannot create
;;;; processes, so its runtime asks the broker to run programs for it over one
;;;; TCP connection per process; see sbcl-process.c for the frame format. The
;;;; broker's policy decides whether and how each request runs, and the
;;;; broker relays the process's standard streams, signals, and exit.
;;;;
;;;; Guests are untrusted. A request must carry the broker's token, compared
;;;; in constant time; every frame is bounded before allocation; and a
;;;; process whose guest disconnects is killed.
(require :sb-posix)
(require :sb-bsd-sockets)
(unless (find-package '#:autolith) (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

;;;; -- Types and Conditions --

(defclass broker-process-request ()
  ((program
    :initarg :program
    :reader broker-process-request-program
    :documentation "The program the guest names, a path or a name to search for.")
   (arguments
    :initarg :arguments
    :reader broker-process-request-arguments
    :documentation "The full argument vector, starting with the program's name.")
   (environment
    :initarg :environment
    :reader broker-process-request-environment
    :documentation "The guest's NAME=VALUE environment strings for the process.")
   (search
    :initarg :search
    :reader broker-process-request-search-p
    :documentation "Whether PROGRAM is looked up in the search path.")
   (directory
    :initarg :directory
    :reader broker-process-request-directory
    :documentation "The working directory the guest asks for, or NIL."))
  (:documentation "A guest's request to run one program."))

(defclass broker-process-plan ()
  ((program
    :initarg :program
    :reader broker-process-plan-program
    :documentation "The host program to run.")
   (arguments
    :initarg :arguments
    :reader broker-process-plan-arguments
    :documentation "Its arguments, without the program name.")
   (environment
    :initarg :environment
    :reader broker-process-plan-environment
    :documentation "Its complete NAME=VALUE environment.")
   (search
    :initarg :search
    :reader broker-process-plan-search-p
    :documentation "Whether PROGRAM is looked up in the search path.")
   (directory
    :initarg :directory
    :reader broker-process-plan-directory
    :documentation "Its host working directory."))
  (:documentation "How the broker runs an authorized request on the host."))

(defclass broker-process-service ()
  ((token
    :initarg :token
    :reader broker-process-service-token
    :documentation "The secret every request must present.")
   (policy
    :initarg :policy
    :reader broker-process-service-policy
    :documentation "The policy that authorizes and plans requests."))
  (:documentation "The broker's remote process service for one guest."))

(defclass broker-refusing-policy ()
  ()
  (:documentation "A policy that runs nothing, which a service uses unless
another is configured."))

(defclass broker-host-policy ()
  ((directories
    :initarg :directories
    :reader broker-host-policy-directories
    :documentation "Host directories a process may start in, as native
namestrings without trailing slashes: the directories exported to the guest,
which it sees at the same paths.")
   (environment
    :initarg :environment
    :reader broker-host-policy-environment
    :documentation "The host NAME=VALUE environment processes start from.")
   (protected
    :initarg :protected
    :initform '("PATH" "HOME" "USER" "LOGNAME" "SHELL" "TMPDIR" "SBCL_HOME")
    :reader broker-host-policy-protected
    :documentation "Variables the guest may not override, since they locate
the host's own tools and files.")
   (sandbox
    :initarg :sandbox
    :reader broker-host-policy-sandbox
    :documentation "A function from a plan to the plan that runs it inside
the host sandbox, or :NONE to run plans directly."))
  (:documentation "A policy that runs programs on the host, starting in the
exported directories."))

(define-condition broker-refusal (error)
  ((reason
    :initarg :reason
    :reader broker-refusal-reason
    :documentation ":REFUSED when policy forbids the request, :MISSING when
the program cannot be found.")
   (message
    :initarg :message
    :reader broker-refusal-message
    :documentation "Why the broker ran nothing."))
  (:report (lambda (condition stream)
             (write-string (broker-refusal-message condition) stream)))
  (:documentation "The broker declines to run a guest's request."))


;;;; -- Protocol --

(defparameter *broker-frame-types*
  '(:spawn 1 :started 2 :failed 3 :stdin 4 :stdin-eof 5 :stdout 6 :stderr 7
    :stdout-eof 8 :stderr-eof 9 :signal 10 :exit 11)
  "The frame type octets, as sbcl-process.c defines them.")

(defparameter *broker-frame-limit* (ash 1 20)
  "The largest frame the broker accepts from a guest.")

(defparameter *broker-data-limit* 65536
  "The most standard stream data the broker sends in one frame.")

(defparameter *broker-netbsd-signals*
  '((1 . :hup) (2 . :int) (3 . :quit) (4 . :ill) (5 . :trap) (6 . :abrt) (8 . :fpe)
    (9 . :kill) (10 . :bus) (11 . :segv) (12 . :sys) (13 . :pipe) (14 . :alrm)
    (15 . :term) (16 . :urg) (17 . :stop) (18 . :tstp) (19 . :cont) (20 . :chld)
    (21 . :ttin) (22 . :ttou) (23 . :io) (24 . :xcpu) (25 . :xfsz) (26 . :vtalrm)
    (27 . :prof) (28 . :winch) (30 . :usr1) (31 . :usr2))
  "Guest signal numbers, in NetBSD's numbering, and their names.")

(defgeneric broker-launch-plan (policy request)
  (:documentation "Return the BROKER-PROCESS-PLAN that runs REQUEST under
POLICY, or signal BROKER-REFUSAL."))

(defun broker--frame-type (name)
  "Return the type octet of frame NAME."
  (or (getf *broker-frame-types* name) (error "Unknown frame ~S." name)))

(defun broker--host-signal (name)
  "Return the host's number for signal NAME, or NIL."
  (let ((symbol (find-symbol (format nil "SIG~A" name) '#:sb-posix)))
    (and symbol (boundp symbol) (symbol-value symbol))))

(defun broker--guest-signal (host-number)
  "Return the guest's number for HOST-NUMBER, or NIL when it has none."
  (car (find host-number *broker-netbsd-signals*
             :key (lambda (entry) (broker--host-signal (cdr entry))))))

(defun broker--read-frame (stream)
  "Read one frame from octet STREAM and return its type octet and payload,
or NIL at a clean end of stream. Signal XDR-ERROR for a malformed frame."
  (let ((header (make-array 4 :element-type '(unsigned-byte 8))))
    (let ((count (read-sequence header stream)))
      (cond ((zerop count)
             (return-from broker--read-frame nil))
            ((< count 4)
             (error 'xdr-error :message "The stream ends inside a frame header."))))
    (let ((length (logior (ash (aref header 0) 24) (ash (aref header 1) 16)
                          (ash (aref header 2) 8) (aref header 3))))
      (when (or (zerop length) (> length *broker-frame-limit*))
        (error 'xdr-error :message (format nil "A frame of ~D octets is out of bounds." length)))
      (let ((body (make-array length :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence body stream) length)
          (error 'xdr-error :message "The stream ends inside a frame."))
        (values (aref body 0) (subseq body 1))))))

(defun broker--write-frame (stream lock name &optional (payload #()))
  "Write frame NAME with PAYLOAD octets to STREAM under LOCK."
  (let ((length (1+ (length payload))))
    (sb-thread:with-mutex (lock)
      (write-sequence (vector (ldb (byte 8 24) length) (ldb (byte 8 16) length)
                              (ldb (byte 8 8) length) (ldb (byte 8 0) length)
                              (broker--frame-type name))
                      stream)
      (write-sequence payload stream)
      (finish-output stream))))

(defun broker--words (&rest values)
  "Return VALUES encoded as XDR 32-bit words."
  (let ((writer (xdr-writer-create)))
    (dolist (value values)
      (xdr-write-unsigned writer value))
    (xdr-writer->octets writer)))

(defun broker--read-strings (reader limit)
  "Read a counted XDR array of at most LIMIT strings."
  (let ((count (xdr-read-unsigned reader)))
    (when (> count limit)
      (error 'xdr-error :message (format nil "~D strings exceed ~D." count limit)))
    (loop repeat count collect (xdr-read-string reader 65536))))

(defun broker--tokens-equal-p (given expected)
  "Compare token strings without leaking where they differ."
  (let ((difference (logxor (length given) (length expected))))
    (dotimes (index (length expected))
      (setf difference (logior difference
                               (logxor (char-code (char expected index))
                                       (if (< index (length given))
                                           (char-code (char given index))
                                           0)))))
    (zerop difference)))

(defun broker--decode-spawn (service payload)
  "Decode a SPAWN PAYLOAD into a request after checking its token. Signal
BROKER-REFUSAL for a wrong token and XDR-ERROR for malformed data."
  (let* ((reader (xdr-reader-create payload))
         (token  (xdr-read-string reader 256)))
    (unless (broker--tokens-equal-p token (broker-process-service-token service))
      (error 'broker-refusal :reason :refused :message "The request's broker token is wrong."))
    (let* ((program     (xdr-read-string reader 4096))
           (arguments   (broker--read-strings reader 65536))
           (environment (broker--read-strings reader 4096))
           (search      (xdr-read-boolean reader))
           (directory   (xdr-read-string reader 4096)))
      (make-instance 'broker-process-request
                     :program program :arguments arguments :environment environment
                     :search search :directory (and (plusp (length directory)) directory)))))


;;;; -- Policies --

(defmethod broker-launch-plan ((policy broker-refusing-policy) request)
  "Refuse every request."
  (declare (ignore request))
  (error 'broker-refusal :reason :refused :message "This broker runs no programs."))

(defun broker--within-p (path directories)
  "Return true when host PATH is one of DIRECTORIES or lies below one."
  (let ((path (string-right-trim "/" path)))
    (some (lambda (directory)
            (or (string= path directory)
                (and (> (length path) (length directory))
                     (string= directory path :end2 (length directory))
                     (char= (char path (length directory)) #\/))))
          directories)))

(defun broker--variable-name (entry)
  "Return the name of NAME=VALUE ENTRY."
  (subseq entry 0 (or (position #\= entry) (length entry))))

(defmethod broker-launch-plan ((policy broker-host-policy) request)
  "Run REQUEST on the host in its working directory, which must lie in an
exported directory, with the guest's variables over the host environment
except for protected ones."
  (let ((directory (broker-process-request-directory request))
        (roots     (broker-host-policy-directories policy)))
    (unless directory
      (error 'broker-refusal :reason :refused
                             :message "A process must name its working directory."))
    (unless (and (not (search "/../" (concatenate 'string directory "/")))
                 (broker--within-p directory roots))
      (error 'broker-refusal :reason :refused
                             :message (format nil "~A is outside the exported directories."
                                              directory)))
    (let* ((protected (broker-host-policy-protected policy))
           (guest     (remove-if (lambda (entry)
                                   (or (not (position #\= entry))
                                       (member (broker--variable-name entry) protected
                                               :test #'string=)))
                                 (broker-process-request-environment request)))
           (names     (mapcar #'broker--variable-name guest))
           (host      (remove-if (lambda (entry)
                                   (member (broker--variable-name entry) names :test #'string=))
                                 (broker-host-policy-environment policy)))
           (plan      (make-instance 'broker-process-plan
                                     :program (broker-process-request-program request)
                                     :arguments (rest (broker-process-request-arguments request))
                                     :environment (append host guest)
                                     :search (broker-process-request-search-p request)
                                     :directory directory))
           (sandbox   (broker-host-policy-sandbox policy)))
      (if (eq sandbox :none)
          plan
          (funcall sandbox plan)))))


;;;; -- Serving --

(defun broker--pump (descriptor stream lock data-frame eof-frame)
  "Send DESCRIPTOR's data as DATA-FRAME frames until its end, then EOF-FRAME.
Stop quietly when the guest connection is gone."
  (let ((buffer (make-array *broker-data-limit* :element-type '(unsigned-byte 8))))
    (handler-case
        (progn
          (loop
            (let ((count (handler-case
                             (sb-sys:with-pinned-objects (buffer)
                               (sb-posix:read descriptor (sb-sys:vector-sap buffer) (length buffer)))
                           (sb-posix:syscall-error ()
                             0))))
              (when (zerop count)
                (return))
              (broker--write-frame stream lock data-frame (subseq buffer 0 count))))
          (broker--write-frame stream lock eof-frame))
      (error ()
        nil))))

(defun broker--exit-payload (process)
  "Return the EXIT payload for finished PROCESS: how it ended and its exit
code or guest signal number."
  (if (eq (sb-ext:process-status process) :signaled)
      (broker--words 1 (or (broker--guest-signal (sb-ext:process-exit-code process)) 9))
      (broker--words 0 (ldb (byte 8 0) (sb-ext:process-exit-code process)))))

(defun broker--run (plan)
  "Start PLAN's program with piped standard streams. Signal BROKER-REFUSAL
when it cannot start."
  (handler-case
      (sb-ext:run-program (broker-process-plan-program plan)
                          (broker-process-plan-arguments plan)
                          :search (broker-process-plan-search-p plan)
                          :environment (broker-process-plan-environment plan)
                          :directory (broker-process-plan-directory plan)
                          :input :stream :output :stream :error :stream :wait nil)
    (error (condition)
      (error 'broker-refusal
             :reason (if (search "No such file" (princ-to-string condition)) :missing :refused)
             :message (princ-to-string condition)))))

(defun broker-process-serve-connection (service stream)
  "Serve one guest process connection on octet STREAM."
  (let ((lock    (sb-thread:make-mutex :name "Broker process connection"))
        (process nil)
        (threads nil))
    (unwind-protect
         (handler-case
             (multiple-value-bind (type payload) (broker--read-frame stream)
               (unless (eql type (broker--frame-type :spawn))
                 (return-from broker-process-serve-connection nil))
               (handler-case
                   (setf process (broker--run (broker-launch-plan (broker-process-service-policy service)
                                                                  (broker--decode-spawn service payload))))
                 (broker-refusal (refusal)
                   (let ((writer (xdr-writer-create)))
                     (xdr-write-unsigned writer (if (eq (broker-refusal-reason refusal) :missing) 2 1))
                     (xdr-write-string writer (broker-refusal-message refusal))
                     (broker--write-frame stream lock :failed (xdr-writer->octets writer)))
                   (return-from broker-process-serve-connection nil)))
               (broker--write-frame stream lock :started (broker--words (sb-ext:process-pid process)))
               (let* ((input  (sb-sys:fd-stream-fd (sb-ext:process-input process)))
                      (pumps  (list (sb-thread:make-thread
                                      #'broker--pump :name "Broker stdout"
                                      :arguments (list (sb-sys:fd-stream-fd (sb-ext:process-output process))
                                                       stream lock :stdout :stdout-eof))
                                     (sb-thread:make-thread
                                      #'broker--pump :name "Broker stderr"
                                      :arguments (list (sb-sys:fd-stream-fd (sb-ext:process-error process))
                                                       stream lock :stderr :stderr-eof))))
                      (waiter (sb-thread:make-thread
                               (lambda ()
                                 (sb-ext:process-wait process)
                                 (mapc #'sb-thread:join-thread pumps)
                                 (ignore-errors
                                  (broker--write-frame stream lock :exit (broker--exit-payload process))))
                               :name "Broker exit")))
                 (setf threads (cons waiter pumps))
                 (loop
                   (multiple-value-bind (type payload) (broker--read-frame stream)
                     (cond ((null type)
                            (return))
                           ((eql type (broker--frame-type :stdin))
                            (handler-case
                                (sb-sys:with-pinned-objects (payload)
                                  (let ((done 0))
                                    (loop while (< done (length payload))
                                          do (incf done (sb-posix:write input
                                                                        (sb-sys:sap+ (sb-sys:vector-sap payload) done)
                                                                        (- (length payload) done))))))
                              (sb-posix:syscall-error ()
                                nil)))
                           ((eql type (broker--frame-type :stdin-eof))
                            (close (sb-ext:process-input process)))
                           ((eql type (broker--frame-type :signal))
                            (let* ((reader (xdr-reader-create payload))
                                   (number (xdr-read-unsigned reader))
                                   (group  (xdr-read-unsigned reader))
                                   (name   (cdr (assoc number *broker-netbsd-signals*)))
                                   (host   (and name (broker--host-signal name))))
                              (when host
                                (ignore-errors
                                 (sb-ext:process-kill process host (if (= group 1) :process-group :pid))))))
                           (t
                            (return)))))
                 (sb-thread:join-thread waiter :default nil :timeout 5)))
           (xdr-error ()
             nil)
           (stream-error ()
             nil))
      ;; A guest that disconnects leaves nothing running, and the connection
      ;; closes only after the relays have stopped using it.
      (when (and process (sb-ext:process-alive-p process))
        (ignore-errors (sb-ext:process-kill process sb-posix:sigkill :process-group))
        (ignore-errors (sb-ext:process-kill process sb-posix:sigkill)))
      (dolist (thread threads)
        (sb-thread:join-thread thread :default nil :timeout 10))
      (when process
        (ignore-errors (sb-ext:process-close process))))))

(defun broker-process-listen (service &key (address "127.0.0.1") (port 0))
  "Serve SERVICE on TCP ADDRESS and PORT, zero choosing a free port, one
thread per connection. Return the listening socket and its port."
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
                       (broker-process-serve-connection
                        service
                        (sb-bsd-sockets:socket-make-stream connection :input t :output t
                                                                      :element-type '(unsigned-byte 8)
                                                                      :buffering ':full))
                    ;; Serving is over; unsent output has no reader left.
                    (sb-bsd-sockets:socket-close connection :abort t)))
                :name "Broker process connection")))
         (sb-bsd-sockets:socket-error ()
           nil)))
     :name "Broker process listener")
    (values listener (nth-value 1 (sb-bsd-sockets:socket-name listener)))))
