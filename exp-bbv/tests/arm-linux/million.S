
     	     # count for 1 million instructions
	     #   total is 1 + 333332*3 + 2


# Sycscalls
.equ SYSCALL_EXIT,      1

	.globl _start	
_start:	

       ldr     r2,count                        @ set count
       
big_loop:
       add     r2,r2,#-1
       cmp     r2,#0
       bne     big_loop       	       	       @ repeat till zero

        @================================
	@ Exit
	@================================
exit:
	mov     r0,#0				@ result is zero
	mov	r7,#SYSCALL_EXIT
        swi     0x0            			@ and exit

count:       .word 333332
