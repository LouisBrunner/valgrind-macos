/*

2006-05-21: vex r1619 finally causes the x86->IR front end to state
exactly the %eflags dataflow surrounding 'cmpb $0, ... ;  js ..'
and so memcheck no longer gives a false positive on this test.
	
-----------	

(original comments)
Assembly derived from the following program compiled with -O2.
This fools Valgrind, causing it to give a false error.

#include <stdio.h>

struct Foo
{
    int a1 : 1;
    int a2 : 1;
    int a3 : 1;
    int a4 : 1;
    int a5 : 1;
    int a6 : 1;
    int a7 : 1;
    int bleh : 1;
};

struct Foo* foo;

void set()
{
    foo->bleh = 1;
}

void get()
{
    if ( foo->bleh == 0 )
        printf( "blieb\n" );
}

int main()
{
  foo = malloc(sizeof(struct Foo));
    set();

    get();

    return 0;
}

*/

#include "tests/asm.h"

	.file	"tronical.c"
#if defined(VGO_linux) || defined(VGO_solaris)
	.version	"01.01"
#endif
gcc2_compiled.:
.text
	.align 4
.globl set
set:
	pushl	%ebp
	movl	foo, %eax
	orb	$128, (%eax)
	movl	%esp, %ebp
	popl	%ebp
	ret
.Lfe1:
.LC0:
	.ascii	"blieb\n"
.text
	.align 4
.globl get
get:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	foo, %eax
	cmpb	$0, (%eax)
	js	.L4
	subl	$12, %esp
	pushl	$.LC0
	call	VG_SYM_ASM(printf)
	addl	$16, %esp
.L4:
	leave
	ret
.Lfe2:
	.align 4
.globl VG_SYM_ASM(main)
VG_SYM_ASM(main):
	pushl	%ebp
	movl	%esp, %ebp
	subl	$20, %esp
	pushl	$4
	call	VG_SYM_ASM(malloc)
	movl	%eax, foo
	call	set
	call	get
	xorl	%eax, %eax
	leave
	ret
.Lfe3:
	.comm	foo,4,4
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
