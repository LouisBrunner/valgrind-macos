	.file	"coolo_strlen.c"
	.version	"01.01"
gcc2_compiled.:
.section	.rodata
.LC0:
	.string	"HALLO"
.globl memset
.LC1:
	.string	"THis is a very long strings"
.text
	.align 4
.globl main
	.type	 main,@function
main:
	movl .LC0,%eax
	pushl %ebp
	movl %esp,%ebp
	subl $216,%esp
	movl %eax,-200(%ebp)
	movw .LC0+4,%ax
	movw %ax,-196(%ebp)
	leal -194(%ebp),%eax
	addl $-4,%esp
	pushl $194
	pushl $0
	pushl %eax
	call memset
	addl $16,%esp
	addl $-12,%esp
	addl $-8,%esp
	pushl $.LC1
	leal -200(%ebp),%eax
	pushl %eax
	call strcat
	addl $16,%esp
	pushl %eax
	call __strdup
	movl %eax,%edx
	movl %edx,%ecx
	andl $3,%ecx
	je .L105
	jp .L110
	cmpl $2,%ecx
	je .L111
	cmpb %ch,(%eax)
	je .L109
	incl %eax
.L111:
	cmpb %ch,(%eax)
	je .L109
	incl %eax
.L110:
	cmpb %ch,(%eax)
	je .L109
	incl %eax
.L105:
	movl (%eax),%ecx
	testb %ch,%cl
	jne .L106
	testb %cl,%cl
	je .L109
	testb %ch,%ch
	je .L108
.L106:
	testl $16711680,%ecx
	je .L107
	addl $4,%eax
	testl $-16777216,%ecx
	jne .L105
	subl $3,%eax
.L107:
	incl %eax
.L108:
	incl %eax
.L109:
	subl %edx,%eax
	cmpl $11,%eax
	jle .L102
	movl $1,%eax
	jmp .L104
	.p2align 4,,7
.L102:
	xorl %eax,%eax
.L104:
	movl %ebp,%esp
	popl %ebp
	ret
.Lfe1:
	.size	 main,.Lfe1-main
	.ident	"GCC: (GNU) 2.95.3 20010315 (release)"
