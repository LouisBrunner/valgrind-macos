		# many thanks to David Fang
		# for providing an OSX 10.5 machine to test on

     	     # count for 1 million instructions
	     #   total is 1 + 1 + 499997*2 + 4

	.globl _start	
_start:
	xor	%ecx,%ecx		# not needed, pads total to 1M
	mov	$499997,%ecx		# load counter
test_loop:	
	dec	%ecx			# repeat count times
	jnz	test_loop

	#================================
	# Exit
	#================================

	# syscall numbers in /usr/include/sys/syscall.h on OSX
	#                 in arc/x86/include/asm/unistd_32.h on Linux
	# disassemble on OSX otool -tV
exit:
#ifdef VGO_darwin
	pushl   $0			# we return 0
	xor	%eax,%eax
	inc	%eax	 		# put exit syscall number (1) in eax
	int     $0x80             	# and exit
#else	
	xor     %ebx,%ebx		# we return 0
	xor	%eax,%eax
	inc	%eax	 		# put exit syscall number (1) in eax
	int     $0x80             	# and exit
#endif
