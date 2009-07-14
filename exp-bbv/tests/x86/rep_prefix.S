#
# rep, repe (repz) and repne (repnz) prefixed string instructions
#   only count as one instruction, even though they repeat many times
# This test makes sure the bbv plugin counts these instructions properly
# The answer is validated to hw perf counters.
#

	.globl _start	
_start:	
	cld				# we want these to happen forward

	#===================================
	# Check varied order of the size prefix
	#   with the rep prefix.  Older binutils
	#   did this one way, newer binutils the other
	#===================================
	
size_prefix:
	# test 16-bit load
	
	mov	$8192, %ecx
	mov	$buffer1, %esi		# set source
	.byte 0x66, 0xf3, 0xad		# lodsw
	
	mov	$8192, %ecx
	mov	$buffer1, %esi		# set source
	.byte 0xf3, 0x66, 0xad		# lodsw	
	
	
	

	#===================================
	# Load and Store Instructions
	#===================================
loadstore:
	xor	%eax, %eax
	mov	$0xd, %al		# set eax to d
	
	# test 8-bit store
	
	mov	$16384, %ecx
	mov	$buffer1, %edi		# set destination
	rep	stosb	    		# store d 16384 times, auto-increment
	
	# test 8-bit load
	
	mov	$16384, %ecx
	mov	$buffer1, %esi		# set source
	rep	lodsb	    		# load byte 16384 times, auto-increment

	cmp	$0xd,%al		# if we loaded wrong value
	jne	print_error		# print an error

	# test 16-bit store
	
	mov    	$0x020d,%ax		# store 0x020d
	
	mov	$8192, %ecx
	mov	$buffer1, %edi		# set destination
	rep	stosw	    		# store 8192 times, auto-increment
	
	# test 16-bit load
	
	mov	$8192, %ecx
	mov	$buffer1, %esi		# set source
	rep	lodsw	    		# load 8192 times, auto-increment

	cmp	$0x020d,%ax		# if we loaded wrong value
	jne	print_error		# print an error
	
	# test 32-bit store
	
	mov    	$0x0feb1378,%eax	# store 0x0feb1378
	
	mov	$4096, %ecx
	mov	$buffer1, %edi		# set destination
	rep	stosl	    		# store 4096 times, auto-increment
	
	# test 32-bit load
	
	mov	$4096, %ecx
	mov	$buffer1, %esi		# set source
	rep	lodsl	    		# load 4096 times, auto-increment

	cmp	$0x0feb1378,%eax	# if we loaded wrong value
	jne	print_error		# print an error

	#=============================
	# Move instructions
	#=============================
moves:
	# test 8-bit move
	
	mov    $16384, %ecx
	mov    $buffer1, %esi
	mov    $buffer2, %edi
	rep    movsb
	
	# test 16-bit move
	
	mov    $8192, %ecx
	mov    $buffer2, %esi
	mov    $buffer1, %edi
	rep    movsw
	
	# test 32-bit move
	
	mov    $4096, %ecx
	mov    $buffer1, %esi
	mov    $buffer2, %edi
	rep    movsl	
	
	#==================================
	# Compare equal instructions
	#==================================
compare_equal:	
	# first set up the areas to compare
	
	mov	$0xa5a5a5a5,%eax
	mov	$buffer1, %edi
	mov	$4096, %ecx
	rep	stosl
	
	mov	$0xa5a5a5a5,%eax
	mov	$buffer2, %edi
	mov	$4096, %ecx
	rep	stosl
	
	# test 8-bit
	
	mov	$buffer1,%esi
	mov	$buffer2,%edi
	mov	$16384, %ecx
	repe	cmpsb
	jnz	print_error
	
	# test 16-bit
	
	mov	$buffer1,%esi
	mov	$buffer2,%edi
	mov	$8192, %ecx
	repe	cmpsw
	jnz	print_error	
	
	# test 32-bit
	
	mov	$buffer1,%esi
	mov	$buffer2,%edi
	mov	$4096, %ecx
	repe	cmpsl
	jnz	print_error		
	
	#==================================
	# Compare not equal instructions
	#==================================
compare_noteq:	
	# change second buffer
	
	mov	$0x5a5a5a5a,%eax
	mov	$buffer2, %edi
	mov	$4096, %ecx
	rep	stosl
	
	# test 8-bit
	
	mov	$buffer1,%esi
	mov	$buffer2,%edi
	mov	$16384, %ecx
	repne	cmpsb
	je	print_error
	
	# test 16-bit
	
	mov	$buffer1,%esi
	mov	$buffer2,%edi
	mov	$8192, %ecx
	repne	cmpsw
	je	print_error	
	
	# test 32-bit
	
	mov	$buffer1,%esi
	mov	$buffer2,%edi
	mov	$4096, %ecx
	repne	cmpsl
	je	print_error			
	
	#====================================
	# Check scan equal instruction
	#====================================

	# test 8-bit

	mov     $0xa5,%al
	mov	$buffer1,%edi
	mov	$16384, %ecx
	repe	scasb
	jnz	print_error
	
	# test 16-bit
	
	mov     $0xa5a5,%ax
	mov	$buffer1,%edi
	mov	$8192, %ecx
	repe	scasw
	jnz	print_error	
	
	# test 32-bit
	
	mov	$0xa5a5a5a5,%eax
	mov	$buffer1,%edi
	mov	$4096, %ecx
	repe	scasl
	jnz	print_error		

	#====================================
	# Check scan not-equal instruction
	#====================================

	# test 8-bit

	mov     $0xa5,%al
	mov	$buffer2,%edi
	mov	$16384, %ecx
	repne	scasb
	jz	print_error
	
	# test 16-bit
	
	mov     $0xa5a5,%ax
	mov	$buffer2,%edi
	mov	$8192, %ecx
	repne	scasw
	jz	print_error	
	
	# test 32-bit
	
	mov	$0xa5a5a5a5,%eax
	mov	$buffer2,%edi
	mov	$4096, %ecx
	repne	scasl
	jz	print_error		

	jmp	exit			# no error, skip to exit
	
print_error:
	    
	mov 	$4, %eax		# Write syscall
#ifdef VGO_darwin
	pushl	$1
	pushl	$error_string
	pushl	$16
#else	
	mov	$1, %ebx		# print to stdout
	mov	$error_string, %ecx	# string to print
	mov	$16, %edx      	   	# strlen
#endif	
	int	$0x80	 		# call syscall

	#================================
	# Exit
	#================================
exit:
#ifdef VGO_darwin	
	xor     %ebx,%ebx		# we return 0
#else
	pushl	$0			# we return 0
#endif	
	xor	%eax,%eax
	inc	%eax	 		# put exit syscall number (1) in eax
	int     $0x80             	# and exit


.data
error_string:	.asciz "Error detected!\n"

#.bss

.lcomm	buffer1,	16384
.lcomm	buffer2,	16384
