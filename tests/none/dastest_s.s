
/* general simple function to use as a template for assembly hacks */

	.file	"oneparam.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl dastest
	.type	 dastest,@function
dastest:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
        das
	daa
	popl	%ebp
	ret
.Lfe1:
	.size	 dastest,.Lfe1-dastest
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
