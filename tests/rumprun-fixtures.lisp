(in-package #:autolith)

;;;; -- Rumprun Guest Fixtures --

;;; A rumprun guest provides the POSIX file, environment, and socket
;;; fixtures through NetBSD's libc and the rump kernel, so it reuses the
;;; POSIX implementations. It is a single process, so it withholds the
;;; fixtures that create one: forked children, pseudo-terminals, and the
;;; POSIX shell.

(defmethod test-fixture-available-p ((platform rumprun-platform) fixture)
  "Withhold the fixtures that need another process."
  (declare (ignore platform))
  (check-type fixture test-fixture-kind)
  (not (member fixture '(:fork :pseudo-terminals :posix-shell))))
