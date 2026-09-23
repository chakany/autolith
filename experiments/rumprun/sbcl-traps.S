/* Trap entry for the rumprun guest. The CPU enters on the IST, so its frame
 * never lands on the interrupted Lisp stack or red zone. The stub saves the
 * interrupted state there only long enough for rumprun_trap_prepare to copy
 * it into a delivery record on the target stack, then continues on that
 * stack, freeing the IST for the next trap. */
.text
.macro TRAP number, error=0
.globl rumprun_trap_\number
rumprun_trap_\number:
.if !\error
    pushq $0
.endif
    pushq $\number
    jmp rumprun_trap_common
.endm
TRAP 0
TRAP 3
TRAP 6
TRAP 13, 1
TRAP 14, 1
TRAP 16
TRAP 19
/* Software interrupt that delivers a pending signal (SIGNAL_VECTOR). */
TRAP 130
rumprun_trap_common:
    pushq %rax
    pushq %rcx
    pushq %rdx
    pushq %rbx
    pushq %rbp
    pushq %rsi
    pushq %rdi
    pushq %r8
    pushq %r9
    pushq %r10
    pushq %r11
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rsp, %rdi
    movq %rsp, %rbx /* Saved above, so free to hold the frame. */
    andq $-16, %rsp
    subq $512, %rsp
    fxsave64 (%rsp)
    /* No pending/unmasked FP exceptions may leak into C. */
    fninit
    subq $16, %rsp
    movl $0x1f80, (%rsp)
    ldmxcsr (%rsp)
    addq $16, %rsp
    cld
    movq %rsp, %rsi
    call rumprun_trap_prepare
    testq %rax, %rax
    jnz 1f
    /* A lazy page was backed: rerun the faulting instruction from the IST. */
    fxrstor64 (%rsp)
    movq %rbx, %rsp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %r11
    popq %r10
    popq %r9
    popq %r8
    popq %rdi
    popq %rsi
    popq %rbp
    popq %rbx
    popq %rdx
    popq %rcx
    popq %rax
    addq $16, %rsp /* Vector and error code. */
    iretq
1:
    /* Continue on the target stack, below the delivery record. */
    movq %rax, %rsp
    movq %rax, %rdi
    call rumprun_trap_deliver
    /* RAX is the resume block: FXSAVE area, r15..rax, then the IRET frame. */
    movq %rax, %rsp
    fxrstor64 (%rsp)
    addq $512, %rsp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %r11
    popq %r10
    popq %r9
    popq %r8
    popq %rdi
    popq %rsi
    popq %rbp
    popq %rbx
    popq %rdx
    popq %rcx
    popq %rax
    iretq
/* Hardware interrupts on the PIC vectors 32..47 enter on their own IST, so
 * they never write into an interrupted red zone. The entry saves every
 * general register and the FXSAVE state, since interrupted Lisp code keeps
 * live values in caller-saved and SSE registers, then enters rumprun's own
 * handler for the line through a synthesized interrupt frame on the IST.
 * That handler's iretq returns here to restore the interrupted state. */
.macro IRQ number
.globl rumprun_irq_\number
rumprun_irq_\number:
    pushq $\number
    jmp rumprun_irq_common
.endm
IRQ 0
IRQ 1
IRQ 2
IRQ 3
IRQ 4
IRQ 5
IRQ 6
IRQ 7
IRQ 8
IRQ 9
IRQ 10
IRQ 11
IRQ 12
IRQ 13
IRQ 14
IRQ 15
rumprun_irq_common:
    pushq %rax
    pushq %rcx
    pushq %rdx
    pushq %rbx
    pushq %rbp
    pushq %rsi
    pushq %rdi
    pushq %r8
    pushq %r9
    pushq %r10
    pushq %r11
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rsp, %rbx
    andq $-16, %rsp
    subq $512, %rsp
    fxsave64 (%rsp)
    fninit
    subq $16, %rsp
    movl $0x1f80, (%rsp)
    ldmxcsr (%rsp)
    addq $16, %rsp
    cld
    movq 120(%rbx), %rax /* The line number pushed by the entry. */
    leaq rumprun_irq_original(%rip), %rcx
    movq (%rcx,%rax,8), %rax
    movq %rsp, %rcx
    movq %ss, %rdx
    pushq %rdx
    pushq %rcx
    pushfq
    movq %cs, %rdx
    pushq %rdx
    leaq 2f(%rip), %rdx
    pushq %rdx
    jmp *%rax
2:
    fxrstor64 (%rsp)
    movq %rbx, %rsp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %r11
    popq %r10
    popq %r9
    popq %r8
    popq %rdi
    popq %rsi
    popq %rbp
    popq %rbx
    popq %rdx
    popq %rcx
    popq %rax
    addq $8, %rsp
    iretq
.section .note.GNU-stack,"",@progbits
