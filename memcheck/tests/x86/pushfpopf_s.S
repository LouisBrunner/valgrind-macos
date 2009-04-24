#include "tests/asm.h"

	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl VG_SYM_ASM(fooble)
#if defined(VGO_linux)
	.type	VG_SYM_ASM(fooble),@function
#endif
VG_SYM_ASM(fooble):
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	
	subl	12(%ebp), %eax
	# flags are now undef if either operand is
	# save possibly undef flags on stack
	pushfl
	
	movl	$0, %eax
	addl	$0, %eax
	# flags are now definitely defined

	popfl
	# resulting flag definedness depends on outcome of sub above
	# should override that created by 0 + 0 above
	# because Vex does an emulation-warning check on the popfl,
	# an error should be reported for the popfl
	
	# now use the condition codes to generate a value
	# in a way which will cause undefinedness to get reported
	# (a second time)
	jz	labelz
	movl	$22, %eax
	jmp	theend
labelz:
	movl	$33, %eax
theend:	
	popl	%ebp
	ret
.Lfe1:
#if defined(VGO_linux)
	.size	VG_SYM_ASM(fooble),.Lfe1-VG_SYM_ASM(fooble)
#endif
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
