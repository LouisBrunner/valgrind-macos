	.text
	.globl _ume_entry

	/* Record the very initial value of %esp before starting the
	   rest of the executable */
_ume_entry:
	movl	%esp, ume_exec_esp
	jmp	_start

	.data
	.globl ume_exec_esp
ume_exec_esp:	.long 0
