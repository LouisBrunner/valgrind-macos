	.file	"fp1.c"
	.version	"01.01"
gcc2_compiled.:
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC2:
	.string	"result = %f\n"
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC0:
	.long	0xeb851eb8,0x40263851
.text
	.align 4
.globl main
	.type	 main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$0, %eax
	movl	$a, %edx
	fldl	.LC0
	.p2align 2
.L21:
	fld	%st(0)
	pushl	%eax
	fimull	(%esp)
	popl	%eax
	fstpl	(%edx,%eax,8)
	incl	%eax
	cmpl	$9, %eax
	jle	.L21
	fstp	%st(0)
	fldz
	movl	$0, %eax
	movl	$a, %edx
	.p2align 2
.L26:
	faddl	(%edx,%eax,8)
	incl	%eax
	cmpl	$9, %eax
	jle	.L26
	subl	$12, %esp
	fstpl	(%esp)
	pushl	$.LC2
	call	printf
	movl	$0, %eax
	leave
	ret
.Lfe1:
	.size	 main,.Lfe1-main
	.comm	a,80,32
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.3 2.96-110)"
