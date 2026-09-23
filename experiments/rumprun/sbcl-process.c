/* Process calls for the single-process rumprun guest.
 * Rumprun's stubs for these return ENOTSUP as a positive result, which its
 * libc_stubs.c itself marks as an incorrect return value: fork appears to
 * create child 86, kill appears to reach every process, and waitpid appears
 * to reap child 86. The final unikernel link wraps each stub, so every
 * caller, C or Lisp, sees the failures POSIX specifies instead. */
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <unistd.h>

/* The guest cannot create processes. */
pid_t __wrap___fork(void)
{
    errno = ENOTSUP;
    return -1;
}

pid_t __wrap___vfork14(void)
{
    errno = ENOTSUP;
    return -1;
}

/* The guest is the only process: it exists and may be probed with signal
 * zero, and every other process does not exist. Process-directed signals
 * are not delivered; pthread_kill reaches threads. */
int __wrap_kill(pid_t pid, int signal)
{
    if (signal < 0 || signal >= NSIG) {
        errno = EINVAL;
        return -1;
    }
    if (pid != getpid() && pid != 0) {
        errno = ESRCH;
        return -1;
    }
    if (signal == 0) return 0;
    errno = ENOTSUP;
    return -1;
}

/* With no children there is nothing to wait for. */
pid_t __wrap__sys___wait450(pid_t pid, int *status, int options, struct rusage *usage)
{
    (void)pid; (void)status; (void)options; (void)usage;
    errno = ECHILD;
    return -1;
}
