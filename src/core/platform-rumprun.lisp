(in-package #:autolith)

;;;; -- Rumprun Platform Adapter --

;;; Autolith running as a rumprun unikernel guest: one process on NetBSD's
;;; libc and a rump kernel. Files, links, permissions, the environment, and
;;; local sockets behave as on other POSIX hosts, so this adapter inherits
;;; the POSIX one. The guest cannot fork: it cannot fork a saver or detach
;;; from a session, and it reports those capabilities as withheld rather
;;; than letting the POSIX paths fail. Programs it starts run on the host
;;; through the host broker, which offers no workspace command sandbox.

(defclass rumprun-platform (posix-platform)
  ()
  (:documentation "The adapter for a rumprun unikernel guest built on NetBSD's libc."))


;;;; -- Capabilities and Processes --

(-> rumprun--unavailable (keyword string) nil)
(defun rumprun--unavailable (capability message)
  "Signal that CAPABILITY is withheld in the guest, explained by MESSAGE."
  (error 'platform-capability-unavailable
         :message message
         :capability capability))

(defmethod platform-supports-p ((platform rumprun-platform) capability)
  "Report the one optional capability a single-process guest provides."
  (declare (ignore platform))
  (not (null (member capability '(:local-sockets)))))

(defmethod platform-detach-session ((platform rumprun-platform))
  "Refuse, since the guest has no controlling session to leave."
  (declare (ignore platform))
  (rumprun--unavailable
   ':detached-sessions
   "A rumprun guest is a single process without sessions to detach from."))

(defmethod platform-session-launch-command ((platform rumprun-platform) source-root)
  "Refuse, since the guest cannot start the stable launcher."
  (declare (ignore platform source-root))
  (rumprun--unavailable
   ':detached-sessions
   "A rumprun guest cannot start another process to run the stable launcher."))

(defmethod platform-launch-detached-process
    ((platform rumprun-platform) arguments
     &key directory output launcher-pid-pathname gate-pathname supervisor-script)
  "Refuse, since the guest cannot create processes."
  (declare (ignore platform arguments directory output launcher-pid-pathname
                   gate-pathname supervisor-script))
  (rumprun--unavailable
   ':detached-sessions
   "A rumprun guest cannot create a detached session process."))

(defmethod platform-run-image-saver ((platform rumprun-platform) child-function)
  "Refuse, since the guest cannot fork a saver that shares this heap."
  (declare (ignore platform child-function))
  (rumprun--unavailable
   ':forked-image-saver
   "A rumprun guest cannot fork a process that shares this image's heap."))

(defmethod platform-command-sandbox-unavailable-message ((platform rumprun-platform))
  "Explain that guest commands run on the host through the broker."
  (declare (ignore platform))
  "A rumprun guest has no workspace command sandbox. Commands run on the host through the host broker with the broker user's privileges, so sandbox mode is disabled and command approval choices run with full access.")

(-> rumprun--random-octets ((integer 1 256)) (simple-array (unsigned-byte 8) (*)))
(defun rumprun--random-octets (count)
  "Return COUNT octets from the rump kernel's random number generator, which
libc's arc4random_buf reads."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (octets)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "arc4random_buf"
                              (function sb-alien:void sb-sys:system-area-pointer
                                        sb-alien:unsigned-long))
       (sb-sys:vector-sap octets)
       count))
    octets))

(defmethod platform-unique-identifier ((platform rumprun-platform))
  "Return a random version 4 UUID string drawn from kernel randomness."
  (declare (ignore platform))
  (platform-random-octets->uuid (rumprun--random-octets 16)))


(setf *platform* (make-instance 'rumprun-platform))
