
     	     # count for 1 million instructions
	     #   total is 3 + 499997*2 + 3
	     
	.globl _start	
_start:	
	nop				# to give us an even million
	lis	15,499997@ha		# load high 16-bits of counter
	addi	15,15,499997@l		# load low 16-bits of counter
test_loop:	
	addic.	15,15,-1		# decrement counter		
	bne	0,test_loop		# loop until zero

	#================================
	# Exit
	#================================

exit:
        li      3,0             # 0 exit value
	li      0,1             # put the exit syscall number (1) in r0
	sc                      # and exit
			

