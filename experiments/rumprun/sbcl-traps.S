/* IST prevents overwriting the interrupted Lisp stack and its red zone. */
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
    movq %rsp, %r12
    andq $-16, %rsp
    subq $512, %rsp
    fxsave64 (%rsp)
    /* No pending/unmasked FP exceptions may leak into a C handler. */
    fninit
    subq $16, %rsp
    movl $0x1f80, (%rsp)
    ldmxcsr (%rsp)
    addq $16, %rsp
    cld
    movq %r12, %rdi
    movq %rsp, %rsi
    call rumprun_trap_dispatch
    fxrstor64 (%rsp)
    movq %r12, %rsp
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
    addq $16, %rsp
    iretq

/* RDI=stack top, RSI=handler, EDX=signal, RCX=info, R8=context.
 * Extra arguments are harmless for the one-argument AMD64 signal ABI. */
.globl rumprun_call_handler
rumprun_call_handler:
    pushq %r12
    movq %rsp, %r12
    movq %rsi, %rax
    movq %rdi, %rsp
    andq $-16, %rsp
    movl %edx, %edi
    movq %rcx, %rsi
    movq %r8, %rdx
    call *%rax
    movq %r12, %rsp
    popq %r12
    ret
.section .note.GNU-stack,"",@progbits
