# When trying (and failing) to instrument at the basic block level
# I thought up a lot of corner-cases in the rep code.  This tries
# to catch some of them

# Performance counters give us 8207 insns
#    11 + 8*1024 + 3 = 8206

	.globl _start	
_start:	
	cld				# we want these to happen forward

	mov    $0xfeb1378,%eax		# value to store

	# test back-to-back rep/stosb's

	mov	$1024,%ecx
	mov	$buffer1, %edi		# set destination
	rep	stosb	    		# store 1024 times
	rep	stosb	    		# should store 0 times	
	rep	stosb			# should store 0 times

	
	# test stosb where cx is 0
	
	xor    %ecx,%ecx
	mov    $buffer1, %edi		# set destination
	rep    stosb	  		# should not load at all
	
	# test rep inside of a loop
	
	mov    $1024, %ebx
rep_loop:	

	mov    $1024,%ecx
	mov    $buffer1, %edi		# set destination
	rep    stosb
	
	mov    $1024,%ecx
	mov    $buffer1, %edi		# set destination
	rep    stosb

	dec    %ebx
	jnz    rep_loop
	
	
	#================================
	# Exit
	#================================
exit:
     	mov	$1,%eax
#if defined(VGO_darwin) || defined(VGO_freebsd)
	pushl   $0			# we return 0
	int     $0x80          		# and exit
#elif defined(VGO_linux)
	xor     %ebx,%ebx		# we return 0
	int	$0x80          		# and exit
#elif defined(VGO_solaris)
	pushl   $0			# we return 0
	int     $0x91          		# and exit
#else
#  error "Unknown OS"
#endif


#.bss

.lcomm	buffer1,	16384

