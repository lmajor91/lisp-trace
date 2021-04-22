;;;; This file is for me to look through the decompiled assembly code to better understand the program.

	.file	"prog.c"
	.text
	.globl	flag
	.data
	.align 4
	.type	flag, @object
	.size	flag, 4
flag:
	.long	43981
	.globl	x
	.bss
	.align 8
	.type	x, @object
	.size	x, 8
x:
	.zero	8
	.section	.rodata
	.align 8
.LC0:
	.string	"Modified flag variable! Flag now: %x"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	jmp	.L2
.L3:
	movq	x(%rip), %rax
	addq	$1, %rax
	movq	%rax, x(%rip)
.L2:
	movl	flag(%rip), %eax
	cmpl	$43981, %eax
	je	.L3
	movl	flag(%rip), %eax
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 10.2.0"
	.section	.note.GNU-stack,"",@progbits
