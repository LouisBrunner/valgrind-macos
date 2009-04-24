#include "tests/asm.h"
	
	.file	"oneparam.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4

.globl VG_SYM_ASM(get_cpuid0)
VG_SYM_ASM(get_cpuid0):
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax

	pushl	%edi
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx

	movl	%eax, %edi
	movl	$0, %eax
	cpuid	
	movl	%eax, (%edi)
	movl	%ebx, 4(%edi)
	movl	%ecx, 8(%edi)
	movl	%edx, 12(%edi)

	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	popl	%edi
	
	popl	%ebp
	ret


.globl VG_SYM_ASM(get_cpuid1)
VG_SYM_ASM(get_cpuid1):
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax

	pushl	%edi
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx

	movl	%eax, %edi
	movl	$1, %eax
	cpuid	
	movl	%eax, (%edi)
	movl	%ebx, 4(%edi)
	movl	%ecx, 8(%edi)
	movl	%edx, 12(%edi)

	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	popl	%edi
	
	popl	%ebp
	ret




	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
