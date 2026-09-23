/* Single-vCPU, single-Lisp-thread machine support for the pinned rumprun guest.
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

#define PAGE 4096UL
#define PHYSICAL_MASK 0x000ffffffffff000UL
#define NX (1UL << 63)
#define OWNED (1UL << 9)
#define VM_START (1UL << 32)
#define VM_END (1UL << 47)
#define TRAP_PARTITION 65536UL
static uintptr_t next_virtual = 0x2000000000UL;
static struct sigaction actions[NSIG];
static sigset_t blocked;
static stack_t alternate = { .ss_flags = SS_DISABLE };
static unsigned char trap_stack[262144] __attribute__((aligned(16)));
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
static void install_gate(unsigned vector, void (*handler)(void))
{
    struct descriptor_pointer descriptor;
    __asm__ volatile("sidt %0" : "=m"(descriptor));
    struct interrupt_gate *gate = (void *)(descriptor.base + vector * 16);
    uintptr_t address = (uintptr_t)handler;
    gate->low = address; gate->middle = address >> 16; gate->high = address >> 32;
    gate->selector = 8; gate->ist = 4; gate->attributes = 0x8e; gate->reserved = 0;
}
extern void rumprun_trap_0(void), rumprun_trap_3(void), rumprun_trap_6(void);
extern void rumprun_trap_13(void), rumprun_trap_14(void), rumprun_trap_16(void);
extern void rumprun_trap_19(void);

static void *page_allocate(void)
{
    void *page;
    if (posix_memalign(&page, PAGE, PAGE)) return NULL;
    memset(page, 0, PAGE);
    return page;
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
        free(tables[depth]);
    }
}

static int valid_range(uintptr_t address, size_t length)
{
    return address >= VM_START && address < VM_END
        && !(address & (PAGE-1)) && length && !(length & (PAGE-1))
        && length <= VM_END - address;
}
void *rumprun_vm_map(void *requested, size_t length)
{
    uintptr_t address = (uintptr_t)requested;
    if (!length || length > (1UL << 30) || (address & (PAGE-1))) return NULL;
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
        void *page = entry ? page_allocate() : NULL;
        if (!page) {
            prune_tables(address + offset);
            if (offset) rumprun_vm_unmap((void *)address, offset);
            return NULL;
        }
        *entry = (uintptr_t)page | 3 | OWNED;
        invalidate(address + offset);
    }
    if (!requested) next_virtual = address + length;
    return (void *)address;
}

int rumprun_vm_unmap(void *base, size_t length)
{
    uintptr_t address = (uintptr_t)base;
    if (!valid_range(address, length)) return -1;
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (entry && *entry && !(*entry & OWNED)) return -1;
    }
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (entry && *entry) {
            void *page = (void *)(*entry & PHYSICAL_MASK);
            *entry = 0;
            invalidate(address + offset);
            free(page);
        }
    }
    for (size_t offset = 0; offset < length; offset += PAGE)
        prune_tables(address + offset);
    return 0;
}

int rumprun_vm_protect(void *base, size_t length, int protection)
{
    uintptr_t address = (uintptr_t)base;
    if (!valid_range(address, length)
        || (protection & ~(PROT_READ|PROT_WRITE|PROT_EXEC))) return -1;
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        if (!entry || !(*entry & OWNED)) return -1;
    }
    for (size_t offset = 0; offset < length; offset += PAGE) {
        uint64_t *entry = page_entry(address + offset, 0);
        *entry = (*entry & PHYSICAL_MASK) | OWNED
               | (protection ? 1 : 0)
               | ((protection & PROT_WRITE) ? 2 : 0)
               | ((protection & PROT_EXEC) ? 0 : NX);
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
    return 0;
}

static int on_alternate(uintptr_t pointer)
{
    uintptr_t base = (uintptr_t)alternate.ss_sp;
    return !(alternate.ss_flags & SS_DISABLE) && pointer >= base
        && pointer - base < alternate.ss_size;
}

/* The assembly bridge preserves the IST continuation on its original stack. */
extern void rumprun_call_handler(uintptr_t, void (*)(int), int, siginfo_t *, ucontext_t *);
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
#define TRAP_PARTITIONS (sizeof(trap_stack) / TRAP_PARTITION)
/* A dispatch is live until its handler returns. Lisp routinely unwinds out
 * of handlers, so liveness is also judged from where the next trap arrives. */
struct dispatch { unsigned partition; uintptr_t handler_top; int alternate; };
static struct dispatch dispatches[TRAP_PARTITIONS];
static unsigned live_dispatches;

static uintptr_t partition_top(unsigned partition)
{
    return (uintptr_t)(trap_stack + sizeof(trap_stack)) - partition * TRAP_PARTITION;
}

static unsigned partition_of(const void *address)
{
    return (partition_top(0) - 1 - (uintptr_t)address) / TRAP_PARTITION;
}

/* A trap interrupting code outside a handler's stack region proves that the
 * handler, and every dispatch nested in it, was unwound. */
static void discard_unwound_dispatches(uintptr_t interrupted)
{
    while (live_dispatches) {
        const struct dispatch *last = &dispatches[live_dispatches - 1];
        int inside = last->alternate ? on_alternate(interrupted)
                   : on_alternate(interrupted) || interrupted < last->handler_top;
        if (inside) return;
        --live_dispatches;
    }
}

/* Point the IST at the first partition no live dispatch occupies. The final
 * partition stays free for reporting excessive nesting. */
static void select_free_partition(void)
{
    for (unsigned partition = 0; partition + 1 < TRAP_PARTITIONS; ++partition) {
        unsigned i = 0;
        while (i < live_dispatches && dispatches[i].partition != partition) ++i;
        if (i == live_dispatches) {
            *exception_ist = partition_top(partition);
            return;
        }
    }
    *exception_ist = partition_top(TRAP_PARTITIONS - 1);
}

/* The assembly frame lists r15..rax, vector, error, RIP, CS, flags, RSP, SS. */
void rumprun_trap_dispatch(uint64_t *frame, void *floating)
{
    static const int registers[] = {
        _REG_R15, _REG_R14, _REG_R13, _REG_R12, _REG_R11, _REG_R10,
        _REG_R9, _REG_R8, _REG_RDI, _REG_RSI, _REG_RBP, _REG_RBX,
        _REG_RDX, _REG_RCX, _REG_RAX
    };
    int vector = frame[15];
    int signal = vector == 3 ? SIGTRAP : vector == 6 ? SIGILL
               : (vector == 0 || vector == 16 || vector == 19) ? SIGFPE : SIGSEGV;
    uintptr_t address = frame[17];
    if (vector == 14) __asm__ volatile("mov %%cr2,%0" : "=r"(address));
    struct sigaction action = actions[signal];
    if (action.sa_handler == SIG_DFL || action.sa_handler == SIG_IGN
        || sigismember(&blocked, signal)) {
        fprintf(stderr, "RUMPRUN fatal trap %d rip=%lx address=%lx error=%lx\n",
                vector, frame[17], address, frame[16]);
        abort();
    }
    /* Each live dispatch owns one 64 KiB IST partition for its raw frame.
     * Handlers use the control stack or registered alternate stack. */
    discard_unwound_dispatches(frame[20]);
    if (live_dispatches + 1 >= TRAP_PARTITIONS) {
        fprintf(stderr, "RUMPRUN fatal trap %d: %u nested dispatches\n",
                vector, live_dispatches + 1);
        abort();
    }
    uintptr_t handler_stack = frame[20] - 128; /* Preserve interrupted red zone. */
    int handler_alternate = 0;
    if ((action.sa_flags & SA_ONSTACK) && !(alternate.ss_flags & SS_DISABLE)
        && !on_alternate(frame[20])) {
        handler_stack = (uintptr_t)alternate.ss_sp + alternate.ss_size;
        handler_alternate = 1;
    }
    /* Like a kernel signal frame, the context lives on the handler's stack.
     * SBCL without threads finds interrupted register roots only by
     * conservatively scanning the control stack, which then covers it. */
    ucontext_t *context = (void *)((handler_stack - sizeof(ucontext_t)) & ~(uintptr_t)63);
    siginfo_t *info = (void *)(((uintptr_t)context - sizeof(siginfo_t)) & ~(uintptr_t)15);
    memset(context, 0, sizeof(*context));
    memset(info, 0, sizeof(*info));
    uint64_t *gregs = context->uc_mcontext.__gregs;
    for (int i = 0; i < 15; ++i) gregs[registers[i]] = frame[i];
    gregs[_REG_RIP] = frame[17]; gregs[_REG_CS] = frame[18];
    gregs[_REG_RFLAGS] = frame[19]; gregs[_REG_RSP] = frame[20];
    gregs[_REG_SS] = frame[21]; gregs[_REG_ERR] = frame[16];
    memcpy(context->uc_mcontext.__fpregs, floating, 512);
    context->uc_sigmask = blocked;
    context->uc_stack = alternate;
    if (on_alternate(frame[20])) context->uc_stack.ss_flags |= SS_ONSTACK;
    info->si_signo = signal;
    info->si_code = vector == 14 ? ((frame[16] & 1) ? SEGV_ACCERR : SEGV_MAPERR)
                  : vector == 3 ? TRAP_BRKPT : vector == 0 ? FPE_INTDIV
                  : vector == 6 ? ILL_ILLOPC : vector == 13 ? SEGV_ACCERR
                  : floating_code(vector, floating);
    info->si_addr = (void *)address;
    for (int number = 1; number < NSIG; ++number)
        if (sigismember(&action.sa_mask, number)) sigaddset(&blocked, number);
    if (!(action.sa_flags & SA_NODEFER)) sigaddset(&blocked, signal);
    unsigned self = live_dispatches++;
    dispatches[self].partition = partition_of(frame);
    dispatches[self].handler_top = handler_stack;
    dispatches[self].alternate = handler_alternate;
    select_free_partition();
    if (frame[19] & 512) __asm__ volatile("sti" ::: "memory");
    rumprun_call_handler((uintptr_t)info, action.sa_handler, signal, info, context);
    __asm__ volatile("cli" ::: "memory");
    /* Also drops nested dispatches whose handlers unwound into this one. */
    live_dispatches = self;
    select_free_partition();
    blocked = context->uc_sigmask;
    for (int i = 0; i < 15; ++i) frame[i] = gregs[registers[i]];
    frame[17] = gregs[_REG_RIP]; frame[19] = gregs[_REG_RFLAGS];
    frame[20] = gregs[_REG_RSP];
    memcpy(floating, context->uc_mcontext.__fpregs, 512);
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
    exception_ist = (void *)(tss + 36 + 3*8);
    *exception_ist = (uintptr_t)(trap_stack + sizeof(trap_stack));
    install_gate(0, rumprun_trap_0);
    install_gate(3, rumprun_trap_3);
    install_gate(6, rumprun_trap_6);
    install_gate(13, rumprun_trap_13);
    install_gate(14, rumprun_trap_14);
    install_gate(16, rumprun_trap_16);
    install_gate(19, rumprun_trap_19);
    /* Route x87 errors to #MF rather than the legacy external IRQ13. */
    uintptr_t control;
    __asm__ volatile("mov %%cr0,%0" : "=r"(control));
    control |= 1UL << 5;
    __asm__ volatile("mov %0,%%cr0" :: "r"(control) : "memory");
    __asm__ volatile("mov %%cr4,%0" : "=r"(control));
    control |= (1UL << 9) | (1UL << 10);
    __asm__ volatile("mov %0,%%cr4" :: "r"(control) : "memory");
    sigemptyset(&blocked);
}
