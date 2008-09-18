	.file	"lea.S"
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

	subl	$12, %esp
	pushl	$1365
	call	malloc
	addl	$16, %esp
	movl	%eax, %esi

	movl    $foo, %ecx              /* %ecx = foo */

        xorl    %edx, %edx              /* %edx = 0 */

        /* ----------------------------------------------------------------- */
        /* Start                                                             */
        /* ----------------------------------------------------------------- */
        /* At one point, for LEA1 result I was just using t1.vseg.  This
         * is too simplistic, because sometimes the offset can be a static data
         * pointer, so if t1 is a known non-ptr, the result should not be a
         * non-ptr, but unknown.  Before I fixed it, the following instruction
         * would have caused this warning:
         *
         *   ==5234== Invalid read
         *   ==5234==    at 0x8048306: main (lea.S:21)
         *   ==5234==    by 0x8048264: ??? (start.S:81)
         *   ==5234== Address 0x80494CC is not derived from any heap block
         */
        movl    foo(%edx), %eax         /* ?(n) case */

        /* Do a similar check with LEA2 */
        movl    foo(%edx,%edx,1), %eax

        /* ok -------------------------------------------------------------- */
        movl    0(%ecx,%edx,1), %eax	// ok
        movl    0(%edx,%ecx,1), %eax	// ok
        movl    0(%esi,%edx,1), %eax	// ok
        movl    0(%edx,%esi,1), %eax	// ok

        movl    0(%ecx,%edx,2), %eax	// ok
        movl    0(%esi,%edx,2), %eax	// ok

        /* bad ------------------------------------------------------------- */
        movl    -1(%ecx,%edx,1), %eax	// undet
        movl    -1(%edx,%ecx,1), %eax	// undet
        movl    -1(%esi,%edx,1), %eax	// det
        movl    -1(%edx,%esi,1), %eax	// det

        movl    -1(%ecx,%edx,2), %eax	// undet
        movl    -1(%esi,%edx,2), %eax	// det

	/* bad (type error) ------------------------------------------------ */
	leal    0(%esi,%esi,1),	%eax	// adding two pointers

        /* ----------------------------------------------------------------- */
        /* Scale known pointer by 2.  */
        leal    0(%edx,%esi,2), %eax  	/* result is pointer */

        /* ----------------------------------------------------------------- */
        /* End                                                               */
        /* ----------------------------------------------------------------- */
	popl	%esi

	movl	$0, %eax
	leave
	ret
.Lfe1:
	.size	main,.Lfe1-main
        .comm   foo,24,4
	.ident	"GCC: (GNU) 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"


