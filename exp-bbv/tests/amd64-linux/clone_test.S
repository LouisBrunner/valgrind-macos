     	     # count for ~1 million instructions thread 1
	     # count for ~2 million instructions thread 2
	     # count for additional 500 million each before exit
	     
	.globl _start	
_start:	

	#################################################
        # 1000 cycles in initial thread                 #
	#################################################
	
	xor	%rax,%rax
	mov	$499,%rcx		# load counter
initial_loop:	
	dec	%rcx			# repeat count times
	jnz	initial_loop


	#####################################################
	# Spawn a thread!                                   #
	#####################################################
clone:
	mov    $56,%rax			# clone syscall
	
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

	mov    $0x10f00,%rdi		
	

	mov    $(new_stack+4096),%rsi	 	 	# new stack

	

	mov    $0,%rdx		# args (none)

	syscall
	
	cmp   $0,%rax		# are we in new thread?
	jz    thread2		# if so, jump to thrad2


	###############################################
	# thread1                                     #
	###############################################

thread1:

	mov	$499997,%rcx		# load counter
thread1_loop:	
	dec	%rcx			# repeat count times
	jnz	thread1_loop

	xor     %rdi,%rdi		# we return 0
	jmp    exit
	
thread2:	
	mov	$999997,%rcx		# load counter
thread2_loop:	
	dec	%rcx			# repeat count times
	jnz	thread2_loop	
	
	mov    $5,%rdi			# we return 5
	
	
	#================================
	# Exit
	#================================
exit:

     	# count an additional 500 million

	mov	$250000,%rcx		# load counter
exit_loop:	
	dec	%rcx			# repeat count times
	jnz	exit_loop	

actual_exit:
	mov	$60,%rax		# put exit syscall number (60) in rax
	syscall

.bss
.lcomm	new_stack,4096
