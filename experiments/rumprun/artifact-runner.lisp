(in-package #:autolith)

;;;; -- Disposable Guest Transport --

(-> rump-artifact-ascii (string) rump-artifact-bytes)
(defun rump-artifact-ascii (text)
  "Encode trusted ASCII fixture text without invoking a Lisp reader."
  (let ((bytes (make-array (length text) :element-type '(unsigned-byte 8))))
    (loop for character across text
          for index from 0
          for code = (char-code character)
          do (assert (< code 128))
             (setf (aref bytes index) code))
    bytes))

(-> rump-artifact-demo-session () rump-artifact-session)
(defun rump-artifact-demo-session ()
  "Grant one immutable input and one bounded output to the demonstration guest."
  (rump-artifact-session
   (list (rump-artifact-grant :slot 1 :read-p t :revision 1
                              :bytes (rump-artifact-ascii "input snapshot"))
         (rump-artifact-grant :slot 2 :write-p t :limit 32))))

(-> rump-artifact-run-guest (&key (:mode string) (:image string)
                                (:seconds (integer 1 20))
                                (:session rump-artifact-session)) rump-artifact-session)
(defun rump-artifact-run-guest (&key (mode "normal")
                                    (image "autolith-rumprun-probe:local")
                                    (seconds 5)
                                    (session (rump-artifact-demo-session)))
  "Serve the real guest on COM2 with bounded resources and a host wall deadline.
Discard the guest console. Always remove the created container, even if the
peer stalls or the broker aborts. Successful output is returned as staged
bytes through RUMP-ARTIFACT-OUTPUTS, never written to a guest-selected path."
  (unless (member mode '("normal" "oversize" "truncated" "flood" "stall" "rollback")
                  :test #'string=)
    (rump-artifact--fail ':invalid-mode))
  (check-type seconds (integer 1 20))
  (let* ((command
           (list "docker" "create" "-i" "--network" "none" "--read-only"
                 "--cap-drop" "ALL" "--security-opt" "no-new-privileges"
                 "--memory" "512m" "--cpus" "2" "--pids-limit" "32"
                 "--log-driver" "none" "--entrypoint" "timeout" image "30"
                 "qemu-system-x86_64" "-machine" "pc,accel=tcg" "-cpu" "qemu64"
                 "-m" "256" "-net" "none" "-vga" "none" "-display" "none"
                 "-serial" "null" "-chardev" "stdio,id=artifacts,signal=off"
                 "-serial" "chardev:artifacts" "-monitor" "none" "-no-reboot"
                 "-kernel" "/probe/artifact-guest.bin" "-append"
                 (format nil "{\"cmdline\":\"artifacts ~A\"}" mode)))
         (container nil)
         (process nil))
    (multiple-value-bind (output error-output status)
        (uiop:run-program command :output ':string :error-output nil
                                  :ignore-error-status t)
      (declare (ignore error-output))
      (unless (zerop status)
        (rump-artifact--fail ':container-create))
      (setf container (string-trim '(#\Space #\Tab #\Return #\Newline) output))
      (unless (and (= (length container) 64)
                   (every (lambda (character) (digit-char-p character 16)) container))
        (rump-artifact--fail ':container-identity)))
    (unwind-protect
         (progn
           (setf process
                 (uiop:launch-program (list "docker" "start" "-ai" container)
                                      :input ':stream :output ':stream :error-output nil
                                      :element-type '(unsigned-byte 8)))
           (handler-case
               (sb-ext:with-timeout seconds
                 (rump-artifact-serve session (uiop:process-info-output process)
                                             (uiop:process-info-input process)))
             (sb-ext:timeout ()
               (rump-artifact--fail ':deadline))
             (stream-error ()
               (rump-artifact--fail ':transport))))
      ;; Removing by the daemon-issued ID cannot target a pre-existing container.
      (multiple-value-bind (output error-output status)
          (uiop:run-program (list "docker" "rm" "--force" container)
                            :output nil :error-output nil :ignore-error-status t)
        (declare (ignore output error-output))
        (when process
          (ignore-errors (close (uiop:process-info-input process)))
          (ignore-errors (close (uiop:process-info-output process)))
          (uiop:wait-process process))
        (unless (zerop status)
          (rump-artifact--fail ':container-cleanup))))))
