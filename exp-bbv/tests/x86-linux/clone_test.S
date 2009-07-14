     	     # count for ~1 million instructions thread 1
	     # count for ~2 million instructions thread 2
	     # count for additional 500 million each before exit
	     
	.globl _start	
_start:	

	#################################################
        # 1000 cycles in initial thread                 #
	#################################################
	
	xor	%eax,%eax
	mov	$499,%ecx		# load counter
initial_loop:	
	dec	%ecx			# repeat count times
	jnz	initial_loop


	#####################################################
	# Spawn a thread!                                   #
	#####################################################
clone:
	mov    $120,%eax		# clone syscall
	
	# Note, clone syscall is different than the glibc implementation
	
# 	int clone (flags, stack_pointer,parent_tidptr,child_tidptr,tls)


	       				# Flags in 
	       				#/usr/include/bits/sched.h
					# CLONE_THREAD 0x10000
					# CLONE_SIGHAND 0x800
					# CLONE_VM      0x100
					# above must be called together
					# Below required for Valgrind
					# CLONE_FS	 0x200
					# CLONE_FILES	 0x400

	mov    $0x10f00,%ebx
	

	mov    $(new_stack+4096),%ecx	 	 	# new stack

	

	mov    $0,%edx		# args (none)

	int    $0x80
	
	cmp   $0,%eax		# are we in new thread?
	jz    thread2		# if so, jump to thrad2


	###############################################
	# thread1                                     #
	###############################################

thread1:

	mov	$499997,%ecx		# load counter
thread1_loop:	
	dec	%ecx			# repeat count times
	jnz	thread1_loop

	xor     %ebx,%ebx		# we return 0
	jmp    exit
	
thread2:	
	mov	$999997,%ecx		# load counter
thread2_loop:	
	dec	%ecx			# repeat count times
	jnz	thread2_loop	
	
	mov    $5,%ebx			# we return 5
	
	
	#================================
	# Exit
	#================================
exit:

     	# count an additional 500 million

	mov	$250000,%ecx		# load counter
exit_loop:	
	dec	%ecx			# repeat count times
	jnz	exit_loop	

actual_exit:
	mov	$1,%eax		# put exit syscall number (60) in rax
	int	$0x80

.bss
.lcomm	new_stack,4096
