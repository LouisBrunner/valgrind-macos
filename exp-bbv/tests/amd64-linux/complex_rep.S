# When trying (and failing) to instrument at the basic block level
# I thought up a lot of corner-cases in the rep code.  This tries
# to catch some of them

# Performance counters give us 8207 insns
#    11 + 8*1024 + 3 = 8206

	.globl _start	
_start:	
	cld				# we want these to happen forward

	mov    $0xfeb131978,%rax	# value to store

	# test back-to-back rep/stosb's

	mov	$1024,%rcx
	mov	$buffer1, %rdi		# set destination
	rep	stosb	    		# store 1024 times
	rep	stosb	    		# should store 0 times	
	rep	stosb			# should store 0 times

	
	# test stosb where cx is 0
	
	xor    %rcx,%rcx
	mov    $buffer1, %rdi		# set destination
	rep    stosb	  		# should not load at all
	
	# test rep inside of a loop
	
	mov    $1024, %rbx
rep_loop:	

	mov    $1024,%rcx
	mov    $buffer1, %rdi		# set destination
	rep    stosb
	
	mov    $1024,%rcx
	mov    $buffer1, %rdi		# set destination
	rep    stosb

	dec    %rbx
	jnz    rep_loop
	
	
	#================================
	# Exit
	#================================
exit:
     	mov	$60,%rax
	xor     %rdi,%rdi		# we return 0
	syscall             		# and exit


.bss

.lcomm	buffer1,	16384

