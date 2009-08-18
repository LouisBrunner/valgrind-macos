
.globl _start

_start:
        # This code tests for the fldcw "load floating point command word"
	#   instruction.  On most x86 processors the retired_instruction
	#   performance counter counts this as one instruction.  However,
	#   on Pentium 4 systems it counts as two.  Therefore this can
	#   affect BBV results on such a system.
	# fldcw is most often used to set the rouding mode when doing
	#   floating point to integer conversions
	
	# It is encoded as "d9 /5" which means
	#   1101 1001 xx10 1yyy
	# Where xx is the "mod" which will be 00, 01, or 10 indicating offset
	#   and yyy is the register field

        # these are instructions with similar encodings to fldcw
	# that can cause false positives if the test isn't explicit enough
similar:	
        fld1   	   	       		# d9 e8
	fldl2t				# d9 e9
	fldl2e				# d9 ea
	fldpi				# d9 eb
	fldlg2				# d9 ec
	fldln2				# d9 ed
	fldz				# d9 ee

	# check some varied ways of calling fldcw

	# offset on stack
stack:	
	sub	$8,%rsp			# allocate space on stack
	fnstcw	2(%rsp)		
	fldcw	2(%rsp)		
	add	$8,%rsp			# restore stack

	# 64-bit register
sixtyfour_reg:	
	fnstcw	cw
	mov	$cw,%rax
	fldcw	0(%rax)			# rax
	mov	$cw,%rbx
	fldcw	0(%rbx)			# rbx
	mov	$cw,%rcx	
	fldcw	0(%rcx)			# rcx
	mov	$cw,%rdx		 
	fldcw	0(%rdx)			# rdx

	# 32-bit register
	
	# Note!  The assembler that comes with SuSE 9.1
	#        cannot assemble 32-bit fldcw on 64-bit systems
	#        Hence the need to hand-code them
	
	
thirtytwo_reg:	
	fnstcw	cw
	mov	$cw,%eax
	
#	fldcw	0(%eax)			# eax
	.byte	0x67,0xd9,0x28

	mov	$cw,%ebx
	
#	fldcw	0(%ebx)			# ebx
	.byte	0x67,0xd9,0x2b

	mov	$cw,%ecx	
	
#	fldcw	0(%ecx)			# ecx
	.byte	0x67,0xd9,0x29

	mov	$cw,%edx
	
#	fldcw	0(%edx)			# edx
	.byte	0x67,0xd9,0x2a

	# register + 8-bit offset
eight_bit:	
	mov	$cw,%eax
	sub	$32,%eax
	
#	fldcw	32(%eax)		# eax + 8 bit offset
	.byte 0x67,0xd9,0x68,0x20

	mov	%eax,%ebx
#	fldcw	32(%ebx)		# ebx + 8 bit offset	
	.byte	0x67,0xd9,0x6b,0x20

	mov	%eax,%ecx
	
#	fldcw	32(%ecx)		# ecx + 8 bit offset		
	.byte	0x67,0xd9,0x69,0x20

	mov	%eax,%edx
	
#	fldcw	32(%edx)		# edx + 8 bit offset
	.byte	0x67,0xd9,0x6a,0x20
	
	
	# register + 32-bit offset
thirtytwo_bit:	
	mov	$cw,%eax
	sub	$30000,%eax
	
#	fldcw	30000(%eax)		# eax + 16 bit offset
	.byte	0x67,0xd9,0xa8,0x30,0x75,0x00,0x00
	
	mov	%eax,%ebx
	
#	fldcw	30000(%ebx)		# ebx + 16 bit offset
	.byte	0x67,0xd9,0xab,0x30,0x75,0x00,0x00
	
	mov	%eax,%ecx
	
#	fldcw	30000(%ecx)		# ecx + 16 bit offset
	.byte	0x67,0xd9,0xa9,0x30,0x75,0x00,0x00
	
	mov	%eax,%edx
	
#	fldcw	30000(%edx)		# edx + 16 bit offset
	.byte	0x67,0xd9,0xaa,0x30,0x75,0x00,0x00
	
	# check an fp/integer conversion
	# in a loop to give a bigger count

	mov	$1024,%rcx
big_loop:

	fldl	three			# load value onto fp stack
	fnstcw	saved_cw		# store control word to mem
	movzwl	saved_cw, %eax		# load cw from mem, zero extending
	movb	$12, %ah		# set cw for "round to zero"
	movw	%rax, cw		# store back to memory
	fldcw	cw   			# save new rounding mode
	fistpl	result			# save stack value as integer to mem
	fldcw	saved_cw		# restore old cw
	
	loop	big_loop		# loop to make the count more obvious

	movl	result, %ebx		# sanity check to see if the
	cmp	$3,%rbx			# result is the expected one
	je	exit
	
print_error:
	mov 	$1,%rax			# write syscall
	mov	$1,%rdi			# stdout
	mov	$error,%rsi		# string	
	mov 	$22,%rdx		# length of string
	syscall
	
exit:	
	xor	%rdi, %rdi		# return 0
	mov	$60, %rax		# SYSCALL_EXIT
	syscall
	


.data
saved_cw:	.long 0
cw:  	.long	0
result: .long	0
three:	.long	0			# a floating point 3.0
	.long	1074266112
error:	.asciz  "Error!  Wrong result!\n"
