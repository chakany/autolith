/* Exercise real page permissions and resumable CPU exceptions before Lisp. */
#include "sbcl-machine.h"
#include <assert.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ucontext.h>
#include <sys/mman.h>
#include <stdlib.h>

static volatile unsigned nested, fp_faults;
static int nesting;
static int expected_fp;
static uintptr_t fp_resume;
static unsigned char alternate_memory[131072] __attribute__((aligned(16)));
static int expect_alternate;

/* Test-only linker allocation injection: count only tracked aligned pages. */
static int allocation_budget = -1;
static void *tracked[32];
extern int __real_posix_memalign(void **, size_t, size_t);
extern void __real_free(void *);
int __wrap_posix_memalign(void **out, size_t alignment, size_t size)
{
    if (allocation_budget == 0) return 12;
    int result = __real_posix_memalign(out, alignment, size);
    if (!result && allocation_budget > 0) {
        --allocation_budget;
        unsigned i;
        for (i = 0; i < 32 && tracked[i]; ++i) {}
        assert(i < 32);
        tracked[i] = *out;
    }
    return result;
}
void __wrap_free(void *pointer)
{
    for (unsigned i = 0; i < 32; ++i)
        if (tracked[i] == pointer) tracked[i] = NULL;
    __real_free(pointer);
}

static void nested_exception(int signal, siginfo_t *info, void *raw)
{
    (void)info;
    ucontext_t *context = raw;
    sigset_t mask;
    assert(!sigprocmask(SIG_SETMASK, NULL, &mask));
    assert(sigismember(&mask, SIGUSR1));
    assert(!sigismember(&mask, signal));
    volatile unsigned char canary[24576];
    uintptr_t here = (uintptr_t)&canary[0];
    uintptr_t interrupted = context->uc_mcontext.__gregs[_REG_RSP];
    uintptr_t frame = (uintptr_t)context;
    /* The signal frame sits between the handler and the interrupted red zone,
     * where a conservative scan of the handler's stack finds its registers. */
    assert(here < frame && (uintptr_t)info < frame);
    if (expect_alternate) {
        assert(here >= (uintptr_t)alternate_memory);
        assert(here < (uintptr_t)alternate_memory + sizeof(alternate_memory));
        assert(frame + sizeof(*context) <= (uintptr_t)alternate_memory + sizeof(alternate_memory));
        stack_t current;
        assert(!sigaltstack(NULL, &current) && (current.ss_flags & SS_ONSTACK));
    } else {
        assert(here < interrupted && interrupted - here < 32768);
        assert(frame + sizeof(*context) <= interrupted - 128);
    }
    for (unsigned i = 0; i < sizeof(canary); ++i) canary[i] = (unsigned char)i;
    ++nested;
    if (!nesting) {
        nesting = 1;
        __asm__ volatile("int3" ::: "memory");
        nesting = 0;
    }
    for (unsigned i = 0; i < sizeof(canary); ++i)
        assert(canary[i] == (unsigned char)i);
    assert(!sigismember(&context->uc_sigmask, signal));
}

static void floating_exception(int signal, siginfo_t *info, void *raw)
{
    assert(signal == SIGFPE && info->si_code == expected_fp);
    uint16_t control, status;
    uint32_t mxcsr;
    __asm__ volatile("fnstcw %0; fnstsw %1; stmxcsr %2"
                     : "=m"(control), "=m"(status), "=m"(mxcsr));
    assert(control == 0x37f && !(status & 0x3f) && mxcsr == 0x1f80);
    ucontext_t *context = raw;
    unsigned char *fp = (void *)context->uc_mcontext.__fpregs;
    control = 0x37f; status = 0; mxcsr = 0x1f80;
    memcpy(fp, &control, 2); memcpy(fp + 2, &status, 2);
    memcpy(fp + 24, &mxcsr, 4);
    context->uc_mcontext.__gregs[_REG_RIP] = fp_resume;
    ++fp_faults;
}

static void test_floating(void)
{
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = floating_exception; action.sa_flags = SA_SIGINFO;
    assert(!sigaction(SIGFPE, &action, NULL));
    uint32_t mxcsr = 0x1d80; /* unmask divide by zero */
    double one = 1.0, zero = 0.0;
    expected_fp = FPE_FLTDIV;
    __asm__ volatile("leaq 1f(%%rip),%%rax; movq %%rax,%0; ldmxcsr %1;"
                     "movsd %2,%%xmm0; divsd %3,%%xmm0; 1:"
                     : "=m"(fp_resume) : "m"(mxcsr), "m"(one), "m"(zero)
                     : "rax", "xmm0", "memory");
    if (!fp_faults) {
        /* QEMU 2.11 TCG does not raise #XM for this arithmetic operation.
         * Exercise the hardware vector separately with an explicit saved status. */
        puts("MACHINE-SKIP: arithmetic SSE #XM absent; testing injected vector 19");
        mxcsr = 0x1d84;
        __asm__ volatile("leaq 1f(%%rip),%%rax; movq %%rax,%0;"
                         "ldmxcsr %1; int $19; 1:"
                         : "=m"(fp_resume) : "m"(mxcsr) : "rax", "memory");
    }
    assert(fp_faults == 1);
    uint16_t control = 0x37b;
    __asm__ volatile("leaq 1f(%%rip),%%rax; movq %%rax,%0; fldcw %1;"
                     "fldl %2; fdivl %3; fwait; 1: fninit"
                     : "=m"(fp_resume) : "m"(control), "m"(one), "m"(zero)
                     : "rax", "memory");
    assert(fp_faults == 2);
    const int codes[] = { FPE_FLTINV, FPE_FLTUND, FPE_FLTDIV,
                          FPE_FLTOVF, FPE_FLTUND, FPE_FLTRES };
    for (unsigned bit = 0; bit < 6; ++bit) {
        expected_fp = codes[bit];
        mxcsr = (0x1f80 & ~(1U << (bit + 7))) | (1U << bit);
        __asm__ volatile("leaq 1f(%%rip),%%rax; movq %%rax,%0;"
                         "ldmxcsr %1; int $19; 1:"
                         : "=m"(fp_resume) : "m"(mxcsr) : "rax", "memory");
        assert(fp_faults == bit + 3);
    }
}

static void test_ranges(void *base)
{
    uintptr_t invalid[] = { (uintptr_t)base + (1UL << 48),
                            UINTPTR_MAX - 4095, 1UL << 47, 4096 };
    for (unsigned i = 0; i < sizeof(invalid)/sizeof(invalid[0]); ++i) {
        assert(!rumprun_vm_map((void *)invalid[i], 4096));
        assert(rumprun_vm_protect((void *)invalid[i], 4096, PROT_NONE) == -1);
        assert(rumprun_vm_unmap((void *)invalid[i], 4096) == -1);
    }
    assert(rumprun_vm_unmap(base, SIZE_MAX - 4095) == -1);
    assert(rumprun_vm_protect(base, SIZE_MAX - 4095, PROT_NONE) == -1);
    assert(rumprun_vm_unmap(base, 1) == -1);
    assert(*(volatile uint64_t *)base == 2);
    for (int budget = 0; budget < 7; ++budget) {
        allocation_budget = budget;
        assert(!rumprun_vm_map((void *)(1UL << 40), 4*4096));
        allocation_budget = -1;
        for (unsigned i = 0; i < 32; ++i) assert(!tracked[i]);
    }
    allocation_budget = 7;
    void *mapped = rumprun_vm_map((void *)(1UL << 40), 4*4096);
    allocation_budget = -1;
    assert(mapped);
    assert(!rumprun_vm_unmap(mapped, 4*4096));
    for (unsigned i = 0; i < 32; ++i) assert(!tracked[i]);
}

static volatile unsigned faults, breaks;
static void *memory;
static void exception(int signal, siginfo_t *info, void *raw)
{
    ucontext_t *context = raw;
    if (signal == SIGTRAP) {
        ++breaks;
        sigset_t mask;
        assert(!sigprocmask(SIG_SETMASK, NULL, &mask));
        assert(sigismember(&mask, SIGTRAP));
        context->uc_mcontext.__gregs[_REG_RAX] = 43;
    } else {
        assert(info->si_addr == memory);
        ++faults;
        assert(!rumprun_vm_protect(memory, 4096, PROT_READ|PROT_WRITE|PROT_EXEC));
    }
}

static jmp_buf unwind_target;
static volatile unsigned unwinds;
static void unwinding_exception(int signal, siginfo_t *info, void *raw)
{
    (void)signal; (void)info; (void)raw;
    ++unwinds;
    _longjmp(unwind_target, 1);
}

/* Lisp non-local exits leave handlers without returning to the dispatcher.
 * Far more unwinds than IST partitions must still leave traps resumable. */
static void test_unwinding(const struct sigaction *resuming)
{
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = unwinding_exception;
    action.sa_flags = SA_SIGINFO | SA_NODEFER;
    assert(!sigaction(SIGTRAP, &action, NULL));
    for (unsigned round = 0; round < 16; ++round) {
        if (!_setjmp(unwind_target)) {
            __asm__ volatile("int3" ::: "memory");
            assert(!"unwinding handler returned");
        }
        assert(unwinds == round + 1);
    }
    assert(!sigaction(SIGTRAP, resuming, NULL));
    unsigned before = breaks;
    uint64_t answer;
    __asm__ volatile("mov $42,%%rax; int3" : "=a"(answer) :: "memory");
    assert(answer == 43 && breaks == before + 1);
}

int main(void)
{
    rumprun_machine_init();
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = exception;
    action.sa_flags = SA_SIGINFO;
    assert(!sigaction(SIGSEGV, &action, NULL));
    assert(!sigaction(SIGTRAP, &action, NULL));
    memory = rumprun_vm_map(NULL, 4096);
    assert(memory);
    *(volatile uint64_t *)memory = 1;
    assert(!rumprun_vm_protect(memory, 4096, PROT_READ));
    *(volatile uint64_t *)memory = 2;
    assert(faults == 1 && *(volatile uint64_t *)memory == 2);
    test_ranges(memory);
    uint64_t answer;
    __asm__ volatile("mov $42,%%rax; int3" : "=a"(answer) :: "memory");
    assert(answer == 43 && breaks == 1);
    unsigned char redzone_ok;
    __asm__ volatile("movq $123,-8(%%rsp); movq $456,-128(%%rsp); int3;"
                     "cmpq $123,-8(%%rsp); sete %%al;"
                     "cmpq $456,-128(%%rsp); sete %%dl; andb %%dl,%%al"
                     : "=a"(redzone_ok) :: "rdx", "cc", "memory");
    assert(redzone_ok);
    /* An ordinary trap preserves caller x87/SSE state, not handler temporaries. */
    uint64_t input = 0x123456789abcdef0UL, output;
    double fp_input = 3.25, fp_output;
    uint32_t rounding = 0x5f80, restored;
    __asm__ volatile("ldmxcsr %5; movq %3,%%xmm15; fldl %4; int3;"
                     "movq %%xmm15,%0; fstpl %1; stmxcsr %2"
                     : "=m"(output), "=m"(fp_output), "=m"(restored)
                     : "m"(input), "m"(fp_input), "m"(rounding)
                     : "rax", "xmm15", "memory");
    assert(output == input && fp_output == fp_input && restored == rounding);
    /* Generated code must fault under NX, then resume after the handler repairs it. */
    unsigned char code[] = { 0xb8, 42, 0, 0, 0, 0xc3 };
    memcpy(memory, code, sizeof(code));
    assert(!rumprun_vm_protect(memory, 4096, PROT_READ|PROT_WRITE));
    assert(((int (*)(void))memory)() == 42 && faults == 2);
    assert(!rumprun_vm_unmap(memory, 4096));
    assert(rumprun_vm_map(memory, 4096) == memory);
    assert(*(uint64_t *)memory == 0);
    /* Aliased query/update arguments must install the original input. */
    struct sigaction alias = action, current;
    alias.sa_flags |= SA_NODEFER;
    alias.sa_sigaction = nested_exception;
    sigaddset(&alias.sa_mask, SIGUSR1);
    assert(!sigaction(SIGTRAP, &alias, &alias));
    assert(!sigaction(SIGTRAP, NULL, &current));
    assert(current.sa_sigaction == nested_exception);
    __asm__ volatile("int3" ::: "memory");
    assert(nested == 2);
    sigset_t mask;
    sigemptyset(&mask); sigaddset(&mask, SIGUSR2);
    assert(!sigprocmask(SIG_SETMASK, &mask, &mask));
    assert(!sigismember(&mask, SIGUSR2));
    assert(!sigprocmask(SIG_SETMASK, NULL, &mask));
    assert(sigismember(&mask, SIGUSR2) && !sigismember(&mask, SIGUSR1));
    stack_t stack, observed;
    memset(&stack, 0, sizeof(stack));
    stack.ss_sp = alternate_memory; stack.ss_size = sizeof(alternate_memory);
    assert(!sigaltstack(&stack, &stack));
    assert(!sigaltstack(NULL, &observed));
    assert(observed.ss_sp == alternate_memory && observed.ss_size == sizeof(alternate_memory));
    current.sa_flags |= SA_ONSTACK;
    assert(!sigaction(SIGTRAP, &current, NULL));
    expect_alternate = 1;
    __asm__ volatile("int3" ::: "memory");
    assert(nested == 4);
    test_floating();
    test_unwinding(&action);
    puts("MACHINE-OK: VM ranges/rollback, masks, nested 24KiB stacks, FP preservation/x87/XM vector, unwound handlers");
    return 0;
}
