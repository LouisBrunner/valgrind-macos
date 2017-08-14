	.file	"fpgames.c"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%02x "
.LC1:
	.string	"\n"
.text
	.align 4
.globl show
	.type	 show,@function
show:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$4, %esp
	movl	$0, %ebx
	.p2align 2
.L21:
	subl	$8, %esp
	movzbl	st(%ebx), %eax
	pushl	%eax
	pushl	$.LC0
	call	printf
	addl	$16, %esp
	testl	%ebx, %ebx
	jle	.L20
	movl	%ebx, %eax
	andl	$3, %eax
	cmpl	$3, %eax
	jne	.L20
	subl	$12, %esp
	pushl	$.LC1
	call	printf
	addl	$16, %esp
.L20:
	incl	%ebx
	cmpl	$27, %ebx
	jle	.L21
	movl	$0, %ebx
	.p2align 2
.L27:
	subl	$8, %esp
	movzbl	st+28(%ebx), %eax
	pushl	%eax
	pushl	$.LC0
	call	printf
	addl	$16, %esp
	testl	%ebx, %ebx
	jle	.L26
	movl	$10, %edx
	movl	%ebx, %eax
	movl	%edx, %ecx
	cltd
	idivl	%ecx
	cmpl	$9, %edx
	jne	.L26
	subl	$12, %esp
	pushl	$.LC1
	call	printf
	addl	$16, %esp
.L26:
	incl	%ebx
	cmpl	$79, %ebx
	jle	.L27
	subl	$12, %esp
	pushl	$.LC1
	call	printf
	movl	-4(%ebp), %ebx
	leave
	ret
.Lfe1:
	.size	 show,.Lfe1-show
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC2:
	.string	"\n\n"
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
#APP
	finit ; fnsave st
#NO_APP
	call	show
	subl	$12, %esp
	pushl	$.LC2
	call	printf
#APP
	fld1 ; fnsave st
#NO_APP
	call	show
	movl	$0, %eax
	leave
	ret
.Lfe2:
	.size	 main,.Lfe2-main
	.comm	st,108,32
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.3 2.96-110)"
