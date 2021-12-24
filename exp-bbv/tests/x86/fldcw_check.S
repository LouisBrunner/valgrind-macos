
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
	sub	$4,%esp			# allocate space on stack
	fnstcw	2(%esp)		
	fldcw	2(%esp)		
	add	$4,%esp			# restore stack
	
	# 32-bit register
	
	fnstcw	cw
	mov	$cw,%eax
	fldcw	0(%eax)			# eax
	mov	$cw,%ebx
	fldcw	0(%ebx)			# ebx
	mov	$cw,%ecx	
	fldcw	0(%ecx)			# ecx
	mov	$cw,%edx		 
	fldcw	0(%edx)			# edx
	
	# register + 8-bit offset
eight_bit:	
	mov	$cw,%eax
	sub	$32,%eax
	
	fldcw	32(%eax)		# eax + 8 bit offset
	mov	%eax,%ebx
	fldcw	32(%ebx)		# ebx + 8 bit offset	
	mov	%eax,%ecx
	fldcw	32(%ecx)		# ecx + 8 bit offset		
	mov	%eax,%edx
	fldcw	32(%edx)		# edx + 8 bit offset
	
	# register + 32-bit offset
thirtytwo_bit:	
	mov	$cw,%eax
	sub	$30000,%eax
	
	fldcw	30000(%eax)		# eax + 16 bit offset
	mov	%eax,%ebx
	fldcw	30000(%ebx)		# ebx + 16 bit offset	
	mov	%eax,%ecx
	fldcw	30000(%ecx)		# ecx + 16 bit offset		
	mov	%eax,%edx
	fldcw	30000(%edx)		# edx + 16 bit offset			

	# check an fp/integer conversion
	# in a loop to give a bigger count

	mov	$1024,%ecx
big_loop:

	fldl	three			# load value onto fp stack
	fnstcw	saved_cw		# store control word to mem
	movzwl	saved_cw, %eax		# load cw from mem, zero extending
	movb	$12, %ah		# set cw for "round to zero"
	movw	%ax, cw			# store back to memory
	fldcw	cw   			# save new rounding mode
	fistpl	result			# save stack value as integer to mem
	fldcw	saved_cw		# restore old cw
	
	loop	big_loop		# loop to make the count more obvious

	movl	result, %ebx		# sanity check to see if the
	cmp	$3,%ebx			# result is the expected one
	je	exit
	
print_error:
	mov 	$4,%eax			# write syscall
#if defined(VGO_darwin) || defined(VGO_freebsd)
	pushl	$22
	pushl	$error
	pushl	$1
	int 	$0x80
#elif defined(VGO_linux)
	mov	$1,%ebx			# stdout
	mov	$error,%ecx		# string	
	mov 	$22,%edx		# length of string
	int 	$0x80
#elif defined(VGO_solaris)
	pushl	$22
	pushl	$error
	pushl	$1
	int 	$0x91
#else
#  error "Unknown OS"
#endif

exit:
	movl	$1,	%eax		# SYSCALL_EXIT
#if defined(VGO_darwin) || defined(VGO_freebsd)
	pushl	result
	int	$0x80
#elif defined(VGO_linux)
	movl	result, %ebx		# load converted value
	int	$0x80
#elif defined(VGO_solaris)
	pushl	result
	int	$0x91
#else
#  error "Unknown OS"
#endif


.data
saved_cw:	.long 0
cw:  	.long	0
result: .long	0
three:	.long	0			# a floating point 3.0
	.long	1074266112
error:	.ascii  "Error!  Wrong result!\n\0"
