	.file	"twoparams.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl fooble
	.type	 fooble,@function
fooble:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	subl	12(%ebp), %eax
	popl	%ebp
	ret
.Lfe1:
	.size	 fooble,.Lfe1-fooble
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
