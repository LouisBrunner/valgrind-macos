	.file	"tronical.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl set
	.type	 set,@function
set:
	pushl	%ebp
	movl	foo, %eax
	orb	$128, (%eax)
	movl	%esp, %ebp
	popl	%ebp
	ret
.Lfe1:
	.size	 set,.Lfe1-set
	.section	.rodata.str1.1,"ams",@progbits,1
.LC0:
	.string	"blieb\n"
.text
	.align 4
.globl get
	.type	 get,@function
get:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	foo, %eax
	cmpb	$0, (%eax)
	js	.L4
	subl	$12, %esp
	pushl	$.LC0
	call	printf
	addl	$16, %esp
.L4:
	leave
	ret
.Lfe2:
	.size	 get,.Lfe2-get
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$20, %esp
	pushl	$4
	call	malloc
	movl	%eax, foo
	call	set
	call	get
	xorl	%eax, %eax
	leave
	ret
.Lfe3:
	.size	 main,.Lfe3-main
	.comm	foo,4,4
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
