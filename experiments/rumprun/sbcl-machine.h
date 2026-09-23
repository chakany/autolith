/* Experimental single-vCPU rumprun machine and signal interface. */
#ifndef SBCL_RUMPRUN_MACHINE_H
#define SBCL_RUMPRUN_MACHINE_H
#include <stddef.h>
/* Map reserves nonzero lengths rounded to pages (at most 16 GiB), backing each
 * page with zeroed memory on first access; running out of physical pages
 * then stops the guest. Unmap and protect round lengths as munmap and
 * mprotect do. Only owned low-canonical
 * [4 GiB, 128 TiB) mappings are changed. One vCPU, whose cooperative
 * scheduler never mutates the tables concurrently.
 * Synchronous exceptions and signals raised by pthread_kill run their
 * handlers on the interrupted stack (red zone preserved), or the thread's
 * registered alternate stack for SA_ONSTACK, below a signal frame holding
 * the context and siginfo. Masks and alternate stacks are per thread;
 * actions are shared. Handlers may block, nest, or be unwound. A signal sent
 * to a thread runs when that thread's park or mask change next allows it.
 * FXSAVE covers x87/SSE, not AVX/YMM. Initialize once before installing
 * handlers, from the main thread. */
void *rumprun_vm_map(void *, size_t);
int rumprun_vm_unmap(void *, size_t);
int rumprun_vm_protect(void *, size_t, int);
void rumprun_machine_init(void);
/* Deliver a signal sent to the whole guest process, as kill does. */
int rumprun_raise_process_signal(int);
#endif
