/* Exercise real page permissions and resumable CPU exceptions before Lisp. */
#include "sbcl-machine.h"
#include <assert.h>
#include <setjmp.h>
#include <pthread.h>
#include <sched.h>
#include <errno.h>
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

/* Test-only linker allocation injection: count only tracked machine pages. */
static int allocation_budget = -1;
static void *tracked[32];
extern void *__real_bmk_pgalloc(int);
extern void __real_bmk_pgfree(void *, int);
void *__wrap_bmk_pgalloc(int order)
{
    assert(order == 0);
    if (allocation_budget == 0) return NULL;
    void *page = __real_bmk_pgalloc(order);
    if (page && allocation_budget > 0) {
        --allocation_budget;
        unsigned i;
        for (i = 0; i < 32 && tracked[i]; ++i) {}
        assert(i < 32);
        tracked[i] = page;
    }
    return page;
}
void __wrap_bmk_pgfree(void *page, int order)
{
    assert(order == 0);
    for (unsigned i = 0; i < 32; ++i)
        if (tracked[i] == page) tracked[i] = NULL;
    __real_bmk_pgfree(page, order);
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
    assert(*(volatile uint64_t *)base == 2);
    /* Unmap rounds its length up to whole pages, as munmap does. */
    unsigned char *pair = rumprun_vm_map((void *)(1UL << 41), 2*4096);
    assert(pair && !rumprun_vm_unmap(pair, 4096 + 1));
    assert(rumprun_vm_map(pair, 2*4096) == pair && !rumprun_vm_unmap(pair, 2*4096));
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

static volatile unsigned forced_traps;

static void forcing_trap(int signal, siginfo_t *info, void *raw)
{
    (void)signal; (void)info; (void)raw;
    /* Without SA_NODEFER this handler runs with SIGTRAP blocked, yet a
     * breakpoint here must still be delivered. */
    if (++forced_traps == 1) __asm__ volatile("int3" ::: "memory");
}

/* A CPU exception whose signal is blocked is delivered anyway, unblocking
 * the signal, as Linux's force_sig_info does. */
static void test_forced_traps(const struct sigaction *resuming)
{
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = forcing_trap;
    action.sa_flags = SA_SIGINFO;
    assert(!sigaction(SIGTRAP, &action, NULL));
    __asm__ volatile("int3" ::: "memory");
    assert(forced_traps == 2);
    assert(!sigaction(SIGTRAP, resuming, NULL));
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

static volatile unsigned thread_traps;
static volatile int joining;

static void thread_trap(int signal, siginfo_t *info, void *raw)
{
    (void)info; (void)raw;
    sigset_t mask;
    assert(!sigprocmask(SIG_SETMASK, NULL, &mask));
    /* This thread blocked SIGUSR2 itself; the main thread did not. */
    assert(sigismember(&mask, SIGUSR2) && sigismember(&mask, signal));
    ++thread_traps;
}

static void *trapping_thread(void *argument)
{
    (void)argument;
    sigset_t mask;
    /* The creator runs in a SIGTRAP handler, and threads inherit its mask. */
    assert(!sigprocmask(SIG_SETMASK, NULL, &mask) && sigismember(&mask, SIGTRAP));
    sigemptyset(&mask); sigaddset(&mask, SIGTRAP);
    assert(!sigprocmask(SIG_UNBLOCK, &mask, NULL));
    sigemptyset(&mask); sigaddset(&mask, SIGUSR2);
    assert(!sigprocmask(SIG_BLOCK, &mask, NULL));
    __asm__ volatile("int3" ::: "memory");
    return NULL;
}

static void joining_trap(int signal, siginfo_t *info, void *raw)
{
    (void)signal; (void)info; (void)raw;
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = thread_trap;
    action.sa_flags = SA_SIGINFO;
    assert(!sigaction(SIGTRAP, &action, NULL));
    /* Block inside this handler while another thread takes its own trap. */
    pthread_t thread;
    assert(!pthread_create(&thread, NULL, trapping_thread, NULL));
    assert(!pthread_join(thread, NULL));
    sigset_t mask;
    assert(!sigprocmask(SIG_SETMASK, NULL, &mask));
    assert(!sigismember(&mask, SIGUSR2));
    joining = 1;
}

/* Signal masks are per thread, and a thread blocked in a handler leaves the
 * trap stack free for other threads' traps. */
static void test_threads(const struct sigaction *resuming)
{
    sigset_t empty, previous;
    sigemptyset(&empty);
    assert(!sigprocmask(SIG_SETMASK, &empty, &previous));
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = joining_trap;
    action.sa_flags = SA_SIGINFO;
    assert(!sigaction(SIGTRAP, &action, NULL));
    __asm__ volatile("int3" ::: "memory");
    assert(joining && thread_traps == 1);
    assert(!sigaction(SIGTRAP, resuming, NULL));
    assert(!sigprocmask(SIG_SETMASK, &previous, NULL));
}

static pthread_mutex_t wake_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t wake_condition = PTHREAD_COND_INITIALIZER;
static volatile int waiting, released, wait_returns;
static volatile unsigned signal_deliveries;
static pthread_t signal_thread;

static void user_signal(int signal, siginfo_t *info, void *raw)
{
    (void)raw;
    assert(signal == SIGUSR1 && info->si_code == SI_LWP);
    signal_thread = pthread_self();
    ++signal_deliveries;
}

static void *waiting_thread(void *argument)
{
    (void)argument;
    assert(!pthread_mutex_lock(&wake_lock));
    waiting = 1;
    while (!released) {
        assert(!pthread_cond_wait(&wake_condition, &wake_lock));
        ++wait_returns;
    }
    assert(!pthread_mutex_unlock(&wake_lock));
    return NULL;
}

static volatile int sigwaiting, waited_signal;

static void *sigwaiting_thread(void *argument)
{
    (void)argument;
    sigset_t wanted;
    sigemptyset(&wanted); sigaddset(&wanted, SIGUSR2);
    assert(!pthread_sigmask(SIG_BLOCK, &wanted, NULL));
    sigwaiting = 1;
    int signal = 0;
    assert(!sigwait(&wanted, &signal));
    waited_signal = signal;
    return NULL;
}

/* pthread_kill runs the handler in the target thread while it waits, and
 * the wait resumes afterwards, as with a kernel's signal delivery. */
static void test_thread_signals(void)
{
    struct sigaction action, previous;
    memset(&action, 0, sizeof(action));
    action.sa_sigaction = user_signal;
    action.sa_flags = SA_SIGINFO;
    assert(!sigaction(SIGUSR1, &action, &previous));
    sigset_t empty, mask;
    sigemptyset(&empty);
    assert(!sigprocmask(SIG_SETMASK, &empty, &mask));

    pthread_t thread;
    assert(!pthread_create(&thread, NULL, waiting_thread, NULL));
    while (!waiting) sched_yield();
    assert(pthread_kill(thread, 0) == 0);
    assert(pthread_kill(thread, SIGURG) == 0); /* Default action: ignored. */
    assert(pthread_kill(thread, SIGUSR1) == 0);
    while (!signal_deliveries) sched_yield();
    assert(signal_deliveries == 1 && pthread_equal(signal_thread, thread));
    assert(waiting && !released);
    assert(!pthread_mutex_lock(&wake_lock));
    released = 1;
    assert(!pthread_cond_signal(&wake_condition));
    assert(!pthread_mutex_unlock(&wake_lock));
    assert(!pthread_join(thread, NULL));
    assert(pthread_kill(thread, 0) == ESRCH);

    /* A blocked signal to oneself waits for the unblocking call. */
    sigset_t user;
    sigemptyset(&user); sigaddset(&user, SIGUSR1);
    assert(!sigprocmask(SIG_BLOCK, &user, NULL));
    assert(pthread_kill(pthread_self(), SIGUSR1) == 0);
    assert(signal_deliveries == 1);
    assert(!sigprocmask(SIG_UNBLOCK, &user, NULL));
    assert(signal_deliveries == 2 && pthread_equal(signal_thread, pthread_self()));

    assert(!sigaction(SIGUSR1, &previous, NULL));
    assert(!sigprocmask(SIG_SETMASK, &mask, NULL));

    /* sigwait receives a blocked signal sent to its thread, even one sent
     * before the thread first runs. */
    sigset_t wanted;
    sigemptyset(&wanted); sigaddset(&wanted, SIGUSR2);
    assert(!sigprocmask(SIG_BLOCK, &wanted, NULL));
    assert(!pthread_create(&thread, NULL, sigwaiting_thread, NULL));
    assert(pthread_kill(thread, SIGUSR2) == 0);
    assert(!pthread_join(thread, NULL));
    assert(sigwaiting && waited_signal == SIGUSR2);
    assert(!sigprocmask(SIG_SETMASK, &mask, NULL));
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
    test_forced_traps(&action);
    test_threads(&action);
    test_thread_signals();
    puts("MACHINE-OK: VM ranges/rollback, masks, nested 24KiB stacks, FP preservation/x87/XM vector, unwound handlers, forced traps, threads, thread signals");
    return 0;
}
