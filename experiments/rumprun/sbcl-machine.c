/* Single-vCPU machine support for the pinned rumprun guest.
 * Page tables and physical pages belong to this guest, not the host broker. */
#include "sbcl-machine.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <ucontext.h>
#include <sys/mman.h>
#include <pthread.h>
#include <lwp.h>
#include <bmk-core/pgalloc.h>

#define PAGE 4096UL
#define PHYSICAL_MASK 0x000ffffffffff000UL
#define NX (1UL << 63)
#define OWNED (1UL << 9)
/* An owned leaf with LAZY has no page yet; the first access backs it. Its RW
 * and NX bits hold the protection, and ACCESSIBLE stands in for present. */
#define LAZY (1UL << 10)
#define ACCESSIBLE (1UL << 11)
#define MAP_LIMIT (1UL << 34)
#define VM_START (1UL << 32)
#define VM_END (1UL << 47)
static uintptr_t next_virtual = 0x2000000000UL;
static struct sigaction actions[NSIG];
/* Signal masks and alternate stacks are per thread; actions are shared. */
static __thread sigset_t blocked;
static __thread stack_t alternate = { .ss_flags = SS_DISABLE };
static unsigned char trap_stack[65536] __attribute__((aligned(16)));
static unsigned char task_state[104] __attribute__((aligned(16)));
static unsigned char emergency_stacks[2][8192] __attribute__((aligned(16)));
static uint64_t *exception_ist;
struct descriptor_pointer { uint16_t limit; uintptr_t base; } __attribute__((packed));
struct interrupt_gate {
    uint16_t low, selector;
    uint8_t ist, attributes;
    uint16_t middle;
    uint32_t high, reserved;
} __attribute__((packed));
static void install_gate(unsigned vector, void (*handler)(void), unsigned ist)
{
    struct descriptor_pointer descriptor;
    __asm__ volatile("sidt %0" : "=m"(descriptor));
    struct interrupt_gate *gate = (void *)(descriptor.base + vector * 16);
    uintptr_t address = (uintptr_t)handler;
    gate->low = address; gate->middle = address >> 16; gate->high = address >> 32;
    gate->selector = 8; gate->ist = ist; gate->attributes = 0x8e; gate->reserved = 0;
}

static uintptr_t gate_handler(unsigned vector)
{
    struct descriptor_pointer descriptor;
    __asm__ volatile("sidt %0" : "=m"(descriptor));
    struct interrupt_gate *gate = (void *)(descriptor.base + vector * 16);
    return gate->low | ((uintptr_t)gate->middle << 16) | ((uintptr_t)gate->high << 32);
}

extern void rumprun_trap_0(void), rumprun_trap_3(void), rumprun_trap_6(void);
extern void rumprun_trap_13(void), rumprun_trap_14(void), rumprun_trap_16(void);
extern void rumprun_trap_19(void), rumprun_trap_130(void);

/* Hardware interrupts on the PIC's 16 lines, vectors 32..47. */
#define TRAP_IST 4
#define IRQ_IST 5
#define IRQ_LINES 16
extern void rumprun_irq_0(void), rumprun_irq_1(void), rumprun_irq_2(void);
extern void rumprun_irq_3(void), rumprun_irq_4(void), rumprun_irq_5(void);
extern void rumprun_irq_6(void), rumprun_irq_7(void), rumprun_irq_8(void);
extern void rumprun_irq_9(void), rumprun_irq_10(void), rumprun_irq_11(void);
extern void rumprun_irq_12(void), rumprun_irq_13(void), rumprun_irq_14(void);
extern void rumprun_irq_15(void);
static void (*const irq_entries[IRQ_LINES])(void) = {
    rumprun_irq_0, rumprun_irq_1, rumprun_irq_2, rumprun_irq_3,
    rumprun_irq_4, rumprun_irq_5, rumprun_irq_6, rumprun_irq_7,
    rumprun_irq_8, rumprun_irq_9, rumprun_irq_10, rumprun_irq_11,
    rumprun_irq_12, rumprun_irq_13, rumprun_irq_14, rumprun_irq_15
};
/* Rumprun's handler for each line, which our IST entry calls. */
uintptr_t rumprun_irq_original[IRQ_LINES];
static unsigned char irq_stack[16384] __attribute__((aligned(16)));
static int irq_ready; /* Set once the IRQ IST exists. */
static void irq_refresh(void);

/* Pages and page tables come whole from the bmk page allocator: an aligned
 * malloc of one page would take a two-page bucket for its header. */
static void *page_allocate(void)
{
    void *page = bmk_pgalloc_one();
    if (page) memset(page, 0, PAGE);
    return page;
}

static void page_free(void *page)
{
    bmk_pgfree_one(page);
}

/* Protection bits of a leaf entry: RW and NX, plus P when backed or
 * ACCESSIBLE when lazy. */
static uint64_t protection_bits(int protection, uint64_t accessible)
{
    return (protection ? accessible : 0)
         | ((protection & PROT_WRITE) ? 2 : 0)
         | ((protection & PROT_EXEC) ? 0 : NX);
}

static uint64_t *page_entry(uintptr_t address, int create)
{
    uintptr_t root;
    __asm__ volatile("mov %%cr3,%0" : "=r"(root));
    uint64_t *table = (void *)(root & PHYSICAL_MASK);
    for (int shift = 39; shift > 12; shift -= 9) {
        uint64_t *entry = &table[(address >> shift) & 511];
        if (!(*entry & 1)) {
            if (!create) return NULL;
            void *page = page_allocate();
            if (!page) return NULL;
            *entry = (uintptr_t)page | 3 | OWNED;
        }
        /* Never split or change the boot kernel's identity mappings. */
        if (*entry & 128) return NULL;
        table = (void *)(*entry & PHYSICAL_MASK);
    }
    return &table[(address >> 12) & 511];
}

static void invalidate(uintptr_t address)
{
    __asm__ volatile("invlpg (%0)" :: "r"(address) : "memory");
}

/* Back the lazy page containing ADDRESS with a zeroed page when its
 * protection allows access. Return 1 when it was backed, 0 when there is
 * nothing to back, and -1 when no physical page is left. */
static int page_back(uintptr_t address)
{
    if (address < VM_START || address >= VM_END) return 0;
    uint64_t *entry = page_entry(address, 0);
    if (!entry || (*entry & (OWNED|LAZY|ACCESSIBLE)) != (OWNED|LAZY|ACCESSIBLE)) return 0;
    void *page = page_allocate();
    if (!page) return -1;
    *entry = (uintptr_t)page | 1 | (*entry & (2|NX)) | OWNED;
    invalidate(address);
    return 1;
}

/* Only free tables we installed, never a boot-provided ancestor. */
static void prune_tables(uintptr_t address)
{
    uintptr_t root;
    uint64_t *parents[3], *tables[3];
    unsigned depth = 0;
    __asm__ volatile("mov %%cr3,%0" : "=r"(root));
    uint64_t *table = (void *)(root & PHYSICAL_MASK);
    for (int shift = 39; shift > 12; shift -= 9) {
        uint64_t *entry = &table[(address >> shift) & 511];
        if (!(*entry & 1) || (*entry & 128)) break;
        parents[depth] = entry;
        table = (void *)(*entry & PHYSICAL_MASK);
        tables[depth++] = table;
    }
    while (depth) {
        --depth;
        if (!(*parents[depth] & OWNED)) break;
        unsigned i;
        for (i = 0; i < 512 && !tables[depth][i]; ++i) {}
        if (i != 512) break;
        *parents[depth] = 0;
        invalidate(address);
        page_free(tables[depth]);
    }
}

/* Round LENGTH up to whole pages as munmap and mprotect do, or return 0 when
 * it cannot be represented. */
static size_t page_round(size_t length)
{
    return length > SIZE_MAX - (PAGE-1) ? 0 : (length + PAGE-1) & ~(PAGE-1);
}

static int valid_range(uintptr_t address, size_t length)
{
    return address >= VM_START && address < VM_END
        && !(address & (PAGE-1)) && length && !(length & (PAGE-1))
        && length <= VM_END - address;
}

/* Reserve LENGTH bytes of lazily backed, readable, writable, and executable
 * memory. Page tables are built now; each page is backed on first access. */
void *rumprun_vm_map(void *requested, size_t length)
{
    uintptr_t address = (uintptr_t)requested;
    if (!length || length > MAP_LIMIT || (address & (PAGE-1))) return NULL;
    length = (length + PAGE-1) & ~(PAGE-1);
    if (!address) address = (next_virtual + 32767) & ~32767UL;
    if (!valid_range(address, length)) return NULL;
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (entry && *entry) return NULL;
    }
    size_t offset;
    for (offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 1);
        if (!entry) {
            prune_tables(address + offset);
            if (offset) rumprun_vm_unmap((void *)address, offset);
            return NULL;
        }
        *entry = OWNED | LAZY | protection_bits(PROT_READ|PROT_WRITE|PROT_EXEC, ACCESSIBLE);
    }
    if (!requested) next_virtual = address + length;
    return (void *)address;
}

int rumprun_vm_unmap(void *base, size_t length)
{
    uintptr_t address = (uintptr_t)base;
    length = page_round(length);
    if (!valid_range(address, length)) return -1;
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (entry && *entry && !(*entry & OWNED)) return -1;
    }
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (entry && *entry) {
            uint64_t old = *entry;
            *entry = 0;
            invalidate(address + offset);
            if (!(old & LAZY)) page_free((void *)(old & PHYSICAL_MASK));
        }
    }
    for (size_t offset = 0; offset < length; offset += PAGE)
        prune_tables(address + offset);
    return 0;
}

int rumprun_vm_protect(void *base, size_t length, int protection)
{
    uintptr_t address = (uintptr_t)base;
    length = page_round(length);
    if (!valid_range(address, length)
        || (protection & ~(PROT_READ|PROT_WRITE|PROT_EXEC))) return -1;
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (!entry || !(*entry & OWNED)) return -1;
    }
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        *entry = (*entry & LAZY)
               ? OWNED | LAZY | protection_bits(protection, ACCESSIBLE)
               : (*entry & PHYSICAL_MASK) | OWNED | protection_bits(protection, 1);
        invalidate(address + offset);
    }
    return 0;
}

/* Only synchronous CPU exceptions are delivered in this milestone. */
int __wrap___sigaction14(int signal, const struct sigaction *action,
                        struct sigaction *old)
{
    if (signal <= 0 || signal >= NSIG) { errno = EINVAL; return -1; }
    struct sigaction copy;
    if (action) copy = *action;
    if (old) *old = actions[signal];
    if (action) actions[signal] = copy;
    return 0;
}

static int deliver_pending(void);
static void publish_mask(void);

int __wrap___sigprocmask14(int how, const sigset_t *set, sigset_t *old)
{
    sigset_t copy;
    if (set) {
        if (how != SIG_SETMASK && how != SIG_BLOCK && how != SIG_UNBLOCK) {
            errno = EINVAL; return -1;
        }
        copy = *set;
        set = &copy;
    }
    if (old) *old = blocked;
    if (!set) return 0;
    if (how == SIG_SETMASK) blocked = *set;
    else if (how == SIG_BLOCK || how == SIG_UNBLOCK) {
        for (int signal = 1; signal < NSIG; ++signal)
            if (sigismember(set, signal)) {
                if (how == SIG_BLOCK) sigaddset(&blocked, signal);
                else sigdelset(&blocked, signal);
            }
    } else { errno = EINVAL; return -1; }
    publish_mask();
    /* Unblocking delivers pending signals before returning, as POSIX requires. */
    deliver_pending();
    return 0;
}

/* NetBSD's libpthread implements this with the raw system call, which
 * rumprun ignores; route it to the same per-thread emulation. Its headers
 * rename C references to __libc_thr_sigsetmask, while Lisp looks up the
 * standard name, so both reach this function. */
int __wrap___libc_thr_sigsetmask(int how, const sigset_t *set, sigset_t *old)
{
    return __wrap___sigprocmask14(how, set, old) ? errno : 0;
}

int __wrap_pthread_sigmask(int how, const sigset_t *set, sigset_t *old)
{
    return __wrap___libc_thr_sigsetmask(how, set, old);
}

/* Threads are registered with their LWP and pending signals so other
 * threads can signal them. Rumprun's scheduler is cooperative, so this table
 * changes only between the scheduling points of the threads it names. */
#define THREAD_LIMIT 256
#define SIGNAL_VECTOR 0x82
static struct registered_thread {
    int used;
    pthread_t thread;
    lwpid_t lwp;                /* 0 until the thread first runs */
    sigset_t pending;
    sigset_t mask;              /* the thread's mask, for process-directed signals */
    const sigset_t *waiting;    /* the set it awaits in sigwait, or NULL */
} threads[THREAD_LIMIT];
static __thread int delivering_signal;

static struct registered_thread *thread_record(pthread_t thread)
{
    for (int i = 0; i < THREAD_LIMIT; ++i)
        if (threads[i].used && pthread_equal(threads[i].thread, thread))
            return &threads[i];
    return NULL;
}

/* Record THREAD, whichever of its creator and itself runs first. A new
 * thread does not run until its creator yields, but it may already be
 * signalled. */
static struct registered_thread *thread_register(pthread_t thread)
{
    struct registered_thread *record = thread_record(thread);
    if (record) return record;
    for (int i = 0; i < THREAD_LIMIT; ++i)
        if (!threads[i].used) {
            threads[i].used = 1;
            threads[i].thread = thread;
            threads[i].lwp = 0;
            sigemptyset(&threads[i].pending);
            sigemptyset(&threads[i].mask);
            threads[i].waiting = NULL;
            return &threads[i];
        }
    fprintf(stderr, "rumprun: more than %d threads\n", THREAD_LIMIT);
    abort();
}

/* Publish this thread's mask, which only the thread itself changes, so that
 * a process-directed signal can find a thread that accepts it. */
static void publish_mask(void)
{
    struct registered_thread *self = thread_record(pthread_self());
    if (self) self->mask = blocked;
}

/* Called by each thread when it starts running. */
static void thread_started(void)
{
    thread_register(pthread_self())->lwp = _lwp_self();
}

static void thread_unregister(void)
{
    struct registered_thread *self = thread_record(pthread_self());
    if (self) self->used = 0;
}

/* Signals whose default action is to ignore them. */
static int default_ignored_p(int signal)
{
    return signal == SIGURG || signal == SIGCHLD || signal == SIGWINCH || signal == SIGINFO;
}

/* Deliver this thread's pending, unblocked signals through the trap path,
 * so each handler receives a real context of the interrupted code. Return
 * how many handlers ran. */
static int deliver_pending(void)
{
    struct registered_thread *self = thread_record(pthread_self());
    int delivered = 0;
    for (int signal = 1; self && signal < NSIG; ++signal) {
        if (!sigismember(&self->pending, signal) || sigismember(&blocked, signal)) continue;
        sigdelset(&self->pending, signal);
        struct sigaction action = actions[signal];
        if (action.sa_handler == SIG_IGN
            || (action.sa_handler == SIG_DFL && default_ignored_p(signal)))
            continue;
        if (action.sa_handler == SIG_DFL) {
            fprintf(stderr, "RUMPRUN fatal signal %d with its default action\n", signal);
            abort();
        }
        delivering_signal = signal;
        __asm__ volatile("int %0" :: "i"(SIGNAL_VECTOR) : "memory");
        ++delivered;
        signal = 0; /* A handler may change the mask or raise more signals. */
    }
    return delivered;
}

struct thread_start { void *(*function)(void *); void *argument; sigset_t mask; };

static void *thread_trampoline(void *raw)
{
    struct thread_start start = *(struct thread_start *)raw;
    free(raw);
    blocked = start.mask; /* A new thread inherits its creator's mask. */
    thread_started();
    publish_mask();
    void *result = start.function(start.argument);
    thread_unregister();
    return result;
}

extern int __real_pthread_create(pthread_t *, const pthread_attr_t *,
                                 void *(*)(void *), void *);
int __wrap_pthread_create(pthread_t *thread, const pthread_attr_t *attributes,
                          void *(*function)(void *), void *argument)
{
    struct thread_start *start = malloc(sizeof(*start));
    if (!start) return EAGAIN;
    *start = (struct thread_start){ function, argument, blocked };
    int status = __real_pthread_create(thread, attributes, thread_trampoline, start);
    if (status)
        free(start);
    else
        thread_register(*thread);
    return status;
}

/* Rumprun never delivers signals, and its _lwp_kill aborts. Mark the signal
 * pending and wake the target from any park; it runs the handler when its
 * park returns, as a kernel delivers to a sleeping thread. A thread that has
 * not run yet receives it at its first park or mask change. */
int __wrap_pthread_kill(pthread_t thread, int signal)
{
    struct registered_thread *target = thread_record(thread);
    if (!target) return ESRCH;
    if (signal == 0) return 0;
    if (signal < 0 || signal >= NSIG) return EINVAL;
    sigaddset(&target->pending, signal);
    if (pthread_equal(thread, pthread_self()))
        deliver_pending();
    else if (target->lwp)
        _lwp_unpark(target->lwp, NULL); /* Otherwise it has yet to run. */
    return 0;
}

/* Deliver process-directed SIGNAL as a kernel would: to a thread waiting for
 * it in sigwait, else to a thread that does not block it, else pending on the
 * initial thread, whose next unblocking or sigwait receives it. */
int rumprun_raise_process_signal(int signal)
{
    if (signal < 0 || signal >= NSIG) { errno = EINVAL; return -1; }
    if (signal == 0) return 0;
    struct registered_thread *target = NULL;
    for (int i = 0; !target && i < THREAD_LIMIT; ++i)
        if (threads[i].used && threads[i].waiting && sigismember(threads[i].waiting, signal))
            target = &threads[i];
    for (int i = 0; !target && i < THREAD_LIMIT; ++i)
        if (threads[i].used && !sigismember(&threads[i].mask, signal))
            target = &threads[i];
    for (int i = 0; !target && i < THREAD_LIMIT; ++i)
        if (threads[i].used)
            target = &threads[i];
    if (!target) { errno = ESRCH; return -1; }
    int status = __wrap_pthread_kill(target->thread, signal);
    if (status) { errno = status; return -1; }
    return 0;
}

/* libpthread blocks in ___lwp_park60, NetBSD's versioned _lwp_park. Deliver
 * pending signals before and after parking; a delivery interrupts the park,
 * and libpthread resumes its wait. */
extern int __real____lwp_park60(clockid_t, int, const struct timespec *, lwpid_t,
                                const void *, const void *);
int __wrap____lwp_park60(clockid_t clock, int flags, const struct timespec *timeout,
                         lwpid_t unpark, const void *hint, const void *unpark_hint)
{
    irq_refresh();
    if (deliver_pending()) {
        if (unpark) _lwp_unpark(unpark, unpark_hint);
        errno = EINTR;
        return -1;
    }
    int result = __real____lwp_park60(clock, flags, timeout, unpark, hint, unpark_hint);
    if (deliver_pending() && result == 0) {
        errno = EINTR;
        result = -1;
    }
    return result;
}

/* Wait for a signal in SET pending for this thread. The caller blocks SET,
 * so ordinary delivery leaves those signals for this call. Nothing between
 * the last pending check and the park can reschedule, so pthread_kill's
 * wakeup cannot be lost on this cooperative scheduler. */
int __wrap_sigwait(const sigset_t *set, int *signal)
{
    struct registered_thread *self = thread_record(pthread_self());
    if (!self) return EINVAL;
    self->waiting = set;
    for (;;) {
        deliver_pending();
        for (int number = 1; number < NSIG; ++number)
            if (sigismember(set, number) && sigismember(&self->pending, number)) {
                sigdelset(&self->pending, number);
                self->waiting = NULL;
                *signal = number;
                return 0;
            }
        __real____lwp_park60(CLOCK_MONOTONIC, 0, NULL, 0, NULL, NULL);
    }
}

static int on_alternate(uintptr_t pointer)
{
    uintptr_t base = (uintptr_t)alternate.ss_sp;
    return !(alternate.ss_flags & SS_DISABLE) && pointer >= base
        && pointer - base < alternate.ss_size;
}

int __wrap___sigaltstack14(const stack_t *stack, stack_t *old)
{
    stack_t copy;
    uintptr_t pointer = (uintptr_t)&copy;
    int active = on_alternate(pointer);
    if (stack) {
        copy = *stack;
        if (active) { errno = EPERM; return -1; }
        if (copy.ss_flags != 0 && copy.ss_flags != SS_DISABLE) {
            errno = EINVAL; return -1;
        }
        if (!(copy.ss_flags & SS_DISABLE)
            && (copy.ss_size < MINSIGSTKSZ || !copy.ss_sp
                || copy.ss_size > UINTPTR_MAX - (uintptr_t)copy.ss_sp)) {
            errno = ENOMEM; return -1;
        }
    }
    if (old) {
        *old = alternate;
        if (active) old->ss_flags |= SS_ONSTACK;
    }
    if (stack) alternate = copy;
    return 0;
}

static int floating_code(int vector, const void *floating)
{
    const unsigned char *bytes = floating;
    uint16_t control, status;
    uint32_t mxcsr;
    memcpy(&control, bytes, 2);
    memcpy(&status, bytes + 2, 2);
    memcpy(&mxcsr, bytes + 24, 4);
    unsigned pending = vector == 16 ? status & ~control
                                   : mxcsr & ~(mxcsr >> 7);
    if (pending & 1) return FPE_FLTINV;
    if (pending & 4) return FPE_FLTDIV;
    if (pending & 8) return FPE_FLTOVF;
    if (pending & 18) return FPE_FLTUND;
    if (pending & 32) return FPE_FLTRES;
    return FPE_FLTINV;
}
/* The trap stub saves the interrupted state on the IST, then prepare lays a
 * delivery record on the target stack, as a kernel builds a signal frame.
 * The stub moves to that stack at once, so the IST is free again for the
 * next trap from any thread, and handlers may block, nest, or be unwound.
 * Afterwards the stub restores the resume block and IRETQs from there. */
struct resume_block {
    unsigned char floating[512] __attribute__((aligned(16)));
    uint64_t registers[15];     /* r15..rax, the stub's pop order */
    uint64_t iret[5];           /* RIP, CS, RFLAGS, RSP, SS */
};

struct trap_delivery {
    struct resume_block resume; /* first: the stub restores from here */
    void (*handler)(int, siginfo_t *, void *);
    int signal;
    int interrupts;             /* interrupted RFLAGS.IF */
    siginfo_t info;
    ucontext_t context;
} __attribute__((aligned(64)));

static const int trap_registers[] = {
    _REG_R15, _REG_R14, _REG_R13, _REG_R12, _REG_R11, _REG_R10,
    _REG_R9, _REG_R8, _REG_RDI, _REG_RSI, _REG_RBP, _REG_RBX,
    _REG_RDX, _REG_RCX, _REG_RAX
};

static int trap_stack_p(uintptr_t pointer)
{
    return pointer >= (uintptr_t)trap_stack
        && pointer - (uintptr_t)trap_stack <= sizeof(trap_stack);
}

/* Stop the guest when a lazy page cannot be backed, as an out-of-memory
 * kill would. */
static void page_exhausted(uintptr_t address, uintptr_t rip)
{
    fprintf(stderr, "RUMPRUN fatal: no physical page left to back %lx "
            "in lwp %d: rip=%lx\n", address, _lwp_self(), rip);
    abort();
}

/* Runs on the IST. FRAME lists r15..rax, vector, error, RIP, CS, RFLAGS,
 * RSP, SS; FLOATING is the FXSAVE area. Return the delivery record, or NULL
 * when the trap was a first access to a lazy page, now backed, and the
 * interrupted instruction should simply run again. */
struct trap_delivery *rumprun_trap_prepare(uint64_t *frame, void *floating)
{
    int vector = frame[15];
    if (vector == 14 && !(frame[16] & 1)) {
        uintptr_t missing;
        __asm__ volatile("mov %%cr2,%0" : "=r"(missing));
        int backed = page_back(missing);
        if (backed < 0) page_exhausted(missing, frame[17]);
        if (backed) return NULL;
    }
    int signal = vector == SIGNAL_VECTOR ? delivering_signal
               : vector == 3 ? SIGTRAP : vector == 6 ? SIGILL
               : (vector == 0 || vector == 16 || vector == 19) ? SIGFPE : SIGSEGV;
    uintptr_t address = vector == SIGNAL_VECTOR ? 0 : frame[17];
    if (vector == 14) __asm__ volatile("mov %%cr2,%0" : "=r"(address));
    struct sigaction action = actions[signal];
    /* A CPU exception cannot wait for its signal to be unblocked. As Linux's
     * force_sig_info does, and as SBCL's safepoint code expects, unblock it
     * in this thread's mask and deliver. Signals from pthread_kill wait. */
    if (vector != SIGNAL_VECTOR && action.sa_handler != SIG_DFL
        && action.sa_handler != SIG_IGN)
        sigdelset(&blocked, signal);
    const char *fatal = trap_stack_p(frame[20]) ? "during trap delivery"
                      : action.sa_handler == SIG_DFL ? "with the default action"
                      : action.sa_handler == SIG_IGN ? "while ignored"
                      : sigismember(&blocked, signal) ? "while blocked" : NULL;
    if (fatal) {
        fprintf(stderr, "RUMPRUN fatal trap %d (signal %d) %s in lwp %d: "
                "rip=%lx address=%lx error=%lx\n", vector, signal, fatal, _lwp_self(),
                frame[17], address, frame[16]);
        abort();
    }
    uintptr_t top = frame[20] - 128; /* Preserve the interrupted red zone. */
    if ((action.sa_flags & SA_ONSTACK) && !(alternate.ss_flags & SS_DISABLE)
        && !on_alternate(frame[20]))
        top = (uintptr_t)alternate.ss_sp + alternate.ss_size;
    struct trap_delivery *delivery =
        (void *)((top - sizeof(struct trap_delivery)) & ~(uintptr_t)63);
    /* A fault while writing the record would reenter this IST and overwrite
     * the frame being delivered, so back its lazy pages first and refuse a
     * target stack that is not writable. */
    for (uintptr_t page = (uintptr_t)delivery & ~(PAGE-1); page < top; page += PAGE) {
        if (page_back(page) < 0) page_exhausted(page, frame[17]);
        uint64_t *entry = page >= VM_START && page < VM_END ? page_entry(page, 0) : NULL;
        if (entry && (*entry & OWNED) && (*entry & 3) != 3) {
            fprintf(stderr, "RUMPRUN fatal trap %d (signal %d): signal stack %lx is "
                    "not writable in lwp %d: rip=%lx\n", vector, signal, page,
                    _lwp_self(), frame[17]);
            abort();
        }
    }
    memset(delivery, 0, sizeof(*delivery));
    memcpy(delivery->resume.floating, floating, 512);
    memcpy(delivery->resume.registers, frame, sizeof(delivery->resume.registers));
    delivery->resume.iret[0] = frame[17];
    delivery->resume.iret[1] = frame[18];
    delivery->resume.iret[2] = frame[19];
    delivery->resume.iret[3] = frame[20];
    delivery->resume.iret[4] = frame[21];
    delivery->handler = action.sa_sigaction;
    delivery->signal = signal;
    delivery->interrupts = (frame[19] & 512) != 0;

    ucontext_t *context = &delivery->context;
    uint64_t *gregs = context->uc_mcontext.__gregs;
    for (int i = 0; i < 15; ++i) gregs[trap_registers[i]] = frame[i];
    gregs[_REG_RIP] = frame[17]; gregs[_REG_CS] = frame[18];
    gregs[_REG_RFLAGS] = frame[19]; gregs[_REG_RSP] = frame[20];
    gregs[_REG_SS] = frame[21]; gregs[_REG_ERR] = frame[16];
    memcpy(context->uc_mcontext.__fpregs, floating, 512);
    context->uc_sigmask = blocked;
    context->uc_stack = alternate;
    if (on_alternate(frame[20])) context->uc_stack.ss_flags |= SS_ONSTACK;

    siginfo_t *info = &delivery->info;
    info->si_signo = signal;
    info->si_code = vector == SIGNAL_VECTOR ? SI_LWP
                  : vector == 14 ? ((frame[16] & 1) ? SEGV_ACCERR : SEGV_MAPERR)
                  : vector == 3 ? TRAP_BRKPT : vector == 0 ? FPE_INTDIV
                  : vector == 6 ? ILL_ILLOPC : vector == 13 ? SEGV_ACCERR
                  : floating_code(vector, floating);
    info->si_addr = (void *)address;

    for (int number = 1; number < NSIG; ++number)
        if (sigismember(&action.sa_mask, number)) sigaddset(&blocked, number);
    if (!(action.sa_flags & SA_NODEFER)) sigaddset(&blocked, signal);
    publish_mask();
    return delivery;
}

/* Runs on the target stack below DELIVERY. Call the handler, then refresh
 * the resume block from the context it may have changed. */
struct resume_block *rumprun_trap_deliver(struct trap_delivery *delivery)
{
    if (delivery->interrupts) __asm__ volatile("sti" ::: "memory");
    delivery->handler(delivery->signal, &delivery->info, &delivery->context);
    __asm__ volatile("cli" ::: "memory");
    ucontext_t *context = &delivery->context;
    uint64_t *gregs = context->uc_mcontext.__gregs;
    blocked = context->uc_sigmask;
    publish_mask();
    for (int i = 0; i < 15; ++i) delivery->resume.registers[i] = gregs[trap_registers[i]];
    delivery->resume.iret[0] = gregs[_REG_RIP];
    delivery->resume.iret[2] = gregs[_REG_RFLAGS];
    delivery->resume.iret[3] = gregs[_REG_RSP];
    memcpy(delivery->resume.floating, context->uc_mcontext.__fpregs, 512);
    return &delivery->resume;
}

static uint64_t interrupts_disable(void)
{
    uint64_t flags;
    __asm__ volatile("pushfq; popq %0; cli" : "=r"(flags) :: "memory");
    return flags;
}

static void interrupts_restore(uint64_t flags)
{
    __asm__ volatile("pushq %0; popfq" :: "r"(flags) : "memory", "cc");
}

/* Put our IST entry on every PIC vector, recording rumprun's handler for
 * the line. Rumprun installs a device's handler when the device registers
 * its line, which happens while the rump kernel attaches devices before
 * main; a later registration is recorded at the next park. */
static void irq_refresh(void)
{
    if (!irq_ready) return;
    uint64_t flags = interrupts_disable();
    for (unsigned line = 0; line < IRQ_LINES; ++line) {
        uintptr_t current = gate_handler(32 + line);
        if (current != (uintptr_t)irq_entries[line]) {
            rumprun_irq_original[line] = current;
            install_gate(32 + line, irq_entries[line], IRQ_IST);
        }
    }
    interrupts_restore(flags);
}

void rumprun_machine_init(void)
{
    /* QEMU's selected x86-64 CPU supports NX. Enable it for our page leaves. */
    unsigned low, high;
    __asm__ volatile("rdmsr" : "=a"(low), "=d"(high) : "c"(0xc0000080));
    low |= 1U << 11;
    __asm__ volatile("wrmsr" :: "a"(low), "d"(high), "c"(0xc0000080));
    struct descriptor_pointer descriptor;
    uint16_t selector;
    __asm__ volatile("sgdt %0" : "=m"(descriptor));
    __asm__ volatile("str %0" : "=r"(selector));
    uint64_t *entry = (void *)(descriptor.base + (selector & ~7));
    uintptr_t tss = (uintptr_t)task_state;
    *(uint64_t *)(task_state + 36 + 1*8) = (uintptr_t)(emergency_stacks[0] + 8192);
    *(uint64_t *)(task_state + 36 + 2*8) = (uintptr_t)(emergency_stacks[1] + 8192);
    *(uint16_t *)(task_state + 102) = sizeof(task_state);
    entry[0] = 103 | ((tss & 0xffffff) << 16) | (0x89UL << 40)
             | (((tss >> 24) & 255) << 56);
    entry[1] = tss >> 32;
    __asm__ volatile("ltr %0" :: "r"(selector) : "memory");
    exception_ist = (void *)(tss + 36 + (TRAP_IST-1)*8);
    *exception_ist = (uintptr_t)(trap_stack + sizeof(trap_stack));
    *(uint64_t *)(task_state + 36 + (IRQ_IST-1)*8) = (uintptr_t)(irq_stack + sizeof(irq_stack));
    install_gate(0, rumprun_trap_0, TRAP_IST);
    install_gate(3, rumprun_trap_3, TRAP_IST);
    install_gate(6, rumprun_trap_6, TRAP_IST);
    install_gate(13, rumprun_trap_13, TRAP_IST);
    install_gate(14, rumprun_trap_14, TRAP_IST);
    install_gate(16, rumprun_trap_16, TRAP_IST);
    install_gate(19, rumprun_trap_19, TRAP_IST);
    install_gate(SIGNAL_VECTOR, rumprun_trap_130, TRAP_IST);
    irq_ready = 1;
    irq_refresh();
    /* Route x87 errors to #MF rather than the legacy external IRQ13. */
    uintptr_t control;
    __asm__ volatile("mov %%cr0,%0" : "=r"(control));
    control |= 1UL << 5;
    __asm__ volatile("mov %0,%%cr0" :: "r"(control) : "memory");
    __asm__ volatile("mov %%cr4,%0" : "=r"(control));
    control |= (1UL << 9) | (1UL << 10);
    __asm__ volatile("mov %0,%%cr4" :: "r"(control) : "memory");
    sigemptyset(&blocked);
    thread_started();
    publish_mask();
}
