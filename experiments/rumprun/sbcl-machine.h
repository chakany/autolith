/* Experimental single-vCPU rumprun machine interface. */
#ifndef SBCL_RUMPRUN_MACHINE_H
#define SBCL_RUMPRUN_MACHINE_H
#include <stddef.h>
/* Map rounds nonzero lengths to pages (at most 1 GiB). Unmap/protect require
 * nonzero page-multiple lengths. Only owned low-canonical [4 GiB, 128 TiB)
 * mappings are changed. Single vCPU only; no concurrent table mutation.
 * Synchronous handlers run on the interrupted stack (red zone preserved), or
 * the registered alternate stack for SA_ONSTACK, below a signal frame holding
 * the context and siginfo. Three live nested exceptions fit the private IST
 * dispatch partitions; a fourth aborts. Handlers may be unwound instead of
 * returning. FXSAVE covers x87/SSE, not AVX/YMM. Initialize once before
 * installing handlers. */
void *rumprun_vm_map(void *, size_t);
int rumprun_vm_unmap(void *, size_t);
int rumprun_vm_protect(void *, size_t, int);
void rumprun_machine_init(void);
#endif
