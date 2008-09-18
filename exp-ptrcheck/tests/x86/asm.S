	.file	"asm.S"
	.text
.globl main
	.type	main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	andl	$-16, %esp
	movl	$0, %eax
	subl	%eax, %esp

        /* ----------------------------------------------------------------- */
        /* Setup                                                             */
        /* ----------------------------------------------------------------- */	
	pushl	%esi
	pushl	%edi
	pushl	%ebx

	subl	$12, %esp
	pushl	$1365
	call	malloc
	addl	$16, %esp
	movl	%eax, %esi
	movl	%eax, %edi
	addl	$4,   %edi

	rdtsc
	cpuid

	bsf	%eax, %ebx
	bsf	%esi, %ebx	// error
	bsr	%eax, %ebx
	bsr	%esi, %ebx	// error

	sahf
	lahf

	movl	%esi, %edx
	shldl	$1, %eax, %ebx
	shldl	$1, %edx, %ebx	// error
	shldl	$1, %eax, %edx	// error

	movl	%esi, %edx
	shrdl	$1, %eax, %ebx
	shrdl	$1, %edx, %ebx	// error
	shrdl	$1, %eax, %edx	// error

	movw	%si, %dx
	shldw	$1, %ax, %bx
	shldw	$1, %ax, %si
	shldw	$1, %si, %bx

	movw	%si, %dx
	shrdw	$1, %ax, %bx
	shrdw	$1, %ax, %si
	shrdw	$1, %si, %bx

        fstsw	%ax
	fstsw	(%esp)		// ok (I get seg faults if I use %esi...)

	movl	$1, %ecx
//        movsd			// for get_dirflag

        /* ----------------------------------------------------------------- */
        /* End                                                               */
        /* ----------------------------------------------------------------- */
	popl	%ebx
	popl	%edi
	popl	%esi

	movl	$0, %eax
	xorw	%ax, %ax
	leave
	ret
.Lfe1:
	.size	main,.Lfe1-main
        .comm   foo,24,4
	.ident	"GCC: (GNU) 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"


