
	
	.file	"oneparam.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4

.globl get_cpuid0
	.type	 get_cpuid0,@function
get_cpuid0:
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
.Lfe1:
	.size	 get_cpuid0,.Lfe1-get_cpuid0


.globl get_cpuid1
	.type	 get_cpuid1,@function
get_cpuid1:
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
.Lfe2:
	.size	 get_cpuid1,.Lfe2-get_cpuid1




	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.1 2.96-98)"
