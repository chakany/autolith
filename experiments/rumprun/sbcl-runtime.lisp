;;;; Apply narrowly checked runtime adapters to a disposable SBCL 2.6.6 tree.
(require :asdf)
(unless (find-package '#:autolith) (defpackage #:autolith (:use #:cl)))
(in-package #:autolith)

(defun rump-sbcl-replace (root file &key before after)
  "Replace one exact source fragment, accepting an already-applied change."
  (let* ((path (merge-pathnames file root))
         (text (uiop:read-file-string path))
         (position (search before text)))
    (cond ((search after text))
          (position
           (assert (not (search before text :start2 (+ position (length before)))))
           (with-open-file (stream path :direction ':output :if-exists ':supersede)
             (write-string text stream :end position)
             (write-string after stream)
             (write-string text stream :start (+ position (length before)))))
          (t (error "Source mismatch in ~A" path)))))

(defun rump-sbcl-adapt-runtime (root)
  "Install the experiment's C/assembly files and explicit runtime integration."
  (let ((here (uiop:pathname-directory-pathname *load-truename*)))
    (dolist (name '("rumprun-os.c" "rumprun-mounts.c" "rumprun-mounts.h" "sbcl-clock.c"
                    "sbcl-machine.c" "sbcl-machine.h" "sbcl-process.c" "sbcl-traps.S"))
      (uiop:copy-file (merge-pathnames name here)
                      (merge-pathnames (concatenate 'string "src/runtime/" name) root))))
  (rump-sbcl-replace root "src/runtime/bsd-os.c"
                    :before "    netbsd_init();" :after "    /* rumprun has no process data rlimit. */")
  (rump-sbcl-replace root "src/runtime/bsd-os.c"
                    :before "#include \"bsd-os.inc\" // for os_alloc_gc_space"
                    :after "/* os_alloc_gc_space is supplied by rumprun-os.c. */")
  (rump-sbcl-replace root "src/runtime/os-common.c"
                    :before "#include \"sys_mmap.inc\""
                    :after "#include \"sys_mmap.inc\"
#include \"sbcl-machine.h\"
extern void *rumprun_load_core(int, os_vm_offset_t, os_vm_address_t, os_vm_size_t);")
  (rump-sbcl-replace root "src/runtime/os-common.c"
                    :before "if (sbcl_munmap(addr, len) == -1) perror(\"munmap\");"
                    :after "if (rumprun_vm_unmap(addr, len) == -1) lose(\"rumprun unmap failed\");")
  (rump-sbcl-replace root "src/runtime/os-common.c"
                    :before "    int fail = 0;
    os_vm_address_t actual;"
                    :after "#ifdef LISP_FEATURE_RUMPRUN
    (void)is_readonly_space;
    return rumprun_load_core(fd, offset, addr, len);
#else
    int fail = 0;
    os_vm_address_t actual;")
  (rump-sbcl-replace root "src/runtime/os-common.c"
                    :before "    return (void*)actual;"
                    :after "    return (void*)actual;
#endif /* LISP_FEATURE_RUMPRUN */")
  (rump-sbcl-replace root "src/runtime/os-common.c"
                    :before "if (sbcl_mprotect(address, length, prot) < 0)"
                    :after "if (rumprun_vm_protect(address, length, prot) < 0)")
  ;; Before Lisp can handle errors, the runtime loses; report where the
  ;; error arose, since the guest has no debugger to inspect it afterward.
  (rump-sbcl-replace root "src/runtime/interrupt.c"
                    :before "        describe_internal_error(context);"
                    :after "        describe_internal_error(context);
        extern void lisp_backtrace(int);
        lisp_backtrace(30);")
  (rump-sbcl-replace root "src/code/common-os.lisp"
                    :before "(native-pathname (sb-alien:extern-alien \"sbcl_runtime\" sb-alien:c-string))"
                    :after "(let ((runtime (sb-alien:extern-alien \"sbcl_runtime\" sb-alien:c-string)))
     (and runtime (native-pathname runtime)))")
  ;; A failed spawn never reaches wait-for-exec, which closes the exec status
  ;; pipe, so leave it to the error cleanup, which runs after the failure is
  ;; reported: closing it here would reset errno before the report reads it.
  ;; Every spawn fails in the guest, which cannot create processes.
  (rump-sbcl-replace root "src/code/run-program.lisp"
                    :before "                                        (unless (minusp child)
                                          (setf child (wait-for-exec child channel))))))))))"
                    :after "                                        (if (minusp child)
                                            (progn (push (deref channel 0) *close-fds-on-error*)
                                                   (push (deref channel 1) *close-fds-on-error*))
                                            (setf child (wait-for-exec child channel))))))))))")
  ;; Safepoints share their POSIX runtime path with Darwin and Linux.
  (rump-sbcl-replace root "src/cold/shared.lisp"
                     :before "(and sb-safepoint (not (and (or arm64 x86 x86-64) (or darwin linux win32))))"
                     :after "(and sb-safepoint (not (and (or arm64 x86 x86-64) (or darwin linux win32 rumprun))))")
  ;; Retain getrusage for CPU accounting independently of elapsed time.
  (dolist (prefix '("#+" "#-"))
    (rump-sbcl-replace root "src/code/unix.lisp"
                      :before (concatenate 'string prefix "(and os-provides-clock-gettime (not sunos))")
                      :after (concatenate 'string prefix "(and os-provides-clock-gettime (not (or sunos rumprun)))")))
  (let ((path (merge-pathnames "src/runtime/Config" root)))
    (unless (search "sbcl-machine.c" (uiop:read-file-string path))
      (with-open-file (stream path :direction ':output :if-exists ':append)
        (format stream "~%OS_SRC += sbcl-machine.c sbcl-clock.c sbcl-process.c rumprun-mounts.c~%ASSEM_SRC += sbcl-traps.S~%CPPFLAGS += -I/build/rumprun/include~%LINKFLAGS += -Wl,--wrap=__sigaction14,--wrap=__sigprocmask14,--wrap=pthread_sigmask,--wrap=__libc_thr_sigsetmask,--wrap=pthread_create,--wrap=pthread_kill,--wrap=sigwait,--wrap=__sigaltstack14,--wrap=dlsym,--wrap=dlopen,--wrap=dlerror,--wrap=exit,--wrap=_exit,--wrap=__clock_gettime50,--wrap=__gettimeofday50~%")))))

(rump-sbcl-adapt-runtime
 (uiop:ensure-directory-pathname
  (or (first (uiop:command-line-arguments)) (error "Supply disposable source directory."))))
