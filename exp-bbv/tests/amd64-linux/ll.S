#
#  linux_logo in x86_64 assembly language
#    based on the code from ll_asm-0.36
#
#  By Vince Weaver <vince _at_ deater.net>
#
# Modified to remove non-deterministic system calls
# And to avoid reading from /proc
#

		
.include "../logo.include"

# offsets into the results returned by the uname syscall
.equ U_SYSNAME,0
.equ U_NODENAME,65
.equ U_RELEASE,65*2
.equ U_VERSION,(65*3)
.equ U_MACHINE,(65*4)
.equ U_DOMAINNAME,65*5

# offset into the results returned by the sysinfo syscall
.equ S_TOTALRAM,32

# Sycscalls
.equ SYSCALL_EXIT,    60
.equ SYSCALL_READ,     0
.equ SYSCALL_WRITE,    1
.equ SYSCALL_OPEN,     2
.equ SYSCALL_CLOSE,    3
.equ SYSCALL_SYSINFO, 99
.equ SYSCALL_UNAME,   63

#
.equ STDIN,0
.equ STDOUT,1
.equ STDERR,2

	.globl _start	
_start:	
	#=========================
	# PRINT LOGO
	#=========================

# LZSS decompression algorithm implementation
# by Stephan Walter 2002, based on LZSS.C by Haruhiko Okumura 1989
# optimized some more by Vince Weaver

	# we used to fill the buffer with FREQUENT_CHAR
	# but, that only gains us one byte of space in the lzss image.
	# the lzss algorithm does automatic RLE... pretty clever
	# so we compress with NUL as FREQUENT_CHAR and it is pre-done for us

	mov     $(N-F), %ebp   	     	# R

	mov  	$logo, %esi		# %esi points to logo (for lodsb)

	mov	$out_buffer, %edi	# point to out_buffer
	push	%rdi	     		# save this value for later

	xor	%ecx, %ecx

decompression_loop:	
	lodsb			# load in a byte

	mov 	$0xff, %bh	# re-load top as a hackish 8-bit counter
	mov 	%al, %bl	# move in the flags

test_flags:
	cmp	$logo_end, %esi # have we reached the end?
	je	done_logo  	# ! if so, exit

	shr 	$1, %ebx	# shift bottom bit into carry flag
	jc	discrete_char	# ! if set, we jump to discrete char

offset_length:
	lodsw                   # get match_length and match_position
	mov %eax,%edx		# copy to edx
	    			# no need to mask dx, as we do it
				# by default in output_loop
	
	shr $(P_BITS),%eax	
	add $(THRESHOLD+1),%al
	mov %al,%cl             # cl = (ax >> P_BITS) + THRESHOLD + 1
				  #                       (=match_length)
		
output_loop:
	and 	$POSITION_MASK,%dh  	# mask it
	mov 	text_buf(%rdx), %al	# load byte from text_buf[]
	inc 	%edx	    		# advance pointer in text_buf
store_byte:	
	stosb				# store it
	
	mov     %al, text_buf(%rbp)	# store also to text_buf[r]
	inc 	%ebp 			# r++
	and 	$(N-1), %bp		# mask r

	loop 	output_loop		# repeat until k>j
	
	or	%bh,%bh			# ! if 0 we shifted through 8 and must
	jnz	test_flags		# re-load flags
	
	jmp 	decompression_loop

discrete_char:
	lodsb				# load a byte
	inc	%ecx			# we set ecx to one so byte
					# will be output once
					# (how do we know ecx is zero?)
					
	jmp     store_byte              # and cleverly store it


# end of LZSS code

done_logo:

	pop 	%rbp			# get out_buffer and keep in bp
	mov	%ebp,%ecx		# move out_buffer to ecx

	call	write_stdout		# print the logo

	#
	#  Setup
	#
setup:
	mov	$strcat,%edx		# use rdx as call pointer (smaller op)

	
	#==========================
	# PRINT VERSION
	#==========================
	
#	push 	$SYSCALL_UNAME		# uname syscall
#	pop	%rax			# in 3 bytes	
	mov	$uname_info,%edi	# uname struct (0 extend address)
#	syscall				# do syscall

	mov	%ebp,%edi		# point %edi to out_buffer
		
	mov	$(uname_info+U_SYSNAME),%esi	# os-name from uname "Linux"
	call	*%rdx			# call strcat

	mov	$ver_string,%esi		# source is " Version "
	call 	*%rdx			        # call strcat
	push	%rsi  				# save our .txt pointer
	
	mov	$(uname_info+U_RELEASE),%esi    # version from uname "2.4.1"
	call 	*%rdx				# call strcat
	
	pop	%rsi  			# restore .txt pointer
					# source is ", Compiled "
	call 	*%rdx			# call strcat
	push	%rsi  			# store for later

	mov	$(uname_info+U_VERSION),%esi	# compiled date
	call 	*%rdx			# call strcat

	mov	%ebp,%ecx		# move out_buffer to ecx

	mov	$0xa,%ax		# store linefeed on end
	stosw				# and zero			  

	call	*%rdx			# call strcat
	
	call	center_and_print	# center and print

	#===============================
	# Middle-Line
	#===============================
middle_line:		
	#=========
	# Load /proc/cpuinfo into buffer
	#=========

	push	%rdx			# save call pointer

#	push	$SYSCALL_OPEN		# load 5 [ open() ]
#	pop	%rax			# in 3 bytes
	
#	mov	$cpuinfo,%edi		# '/proc/cpuinfo'
#	xor	%esi,%esi		# 0 = O_RDONLY <bits/fcntl.h>
#	cdq				# clear edx in clever way
#	syscall				# syscall.  fd in eax.  
					# we should check that eax>=0
					
#	mov	%eax,%edi		# save our fd
	
#	xor	%eax,%eax		# SYSCALL_READ make== 0

	mov	$disk_buffer,%esi

#	mov	$16,%dh		 	# 4096 is maximum size of proc file #)
					# we load sneakily by knowing
					# 16<<8 = 4096. be sure edx clear

#	syscall

#	push	$SYSCALL_CLOSE		# close (to be correct)
#	pop	%rax
#	syscall			

	#=============
	# Number of CPUs
	#=============
number_of_cpus:

	xor	%ebx,%ebx		# chip count
	
					# $disk_buffer still in %rsi
bogo_loop:	
	mov	(%rsi), %eax		# load 4 bytes into eax
	inc	%esi			# increment pointer
	
	cmp	$0,%al			# check for end of file
	je	done_bogo
	
	# Grrr, due to a bug in binutils 2.18.50.0.9
        #   (which unfortunately shipped with Fedora 10)
	#   http://sourceware.org/bugzilla/show_bug.cgi?id=6878
	#   We can't use the apostrophe character

#       cmp     $('o'<<24+'g'<<16+'o'<<8+'b'),%eax
        cmp     $(0x6f<<24+0x67<<16+0x6f<<8+0x62),%eax
				        # "bogo" in little-endian
					
	jne	bogo_loop		# ! if not equal, keep going
	add	$2,%ebx			# otherwise, we have a bogo
					# 2 times too for future magic
	jmp	bogo_loop

done_bogo:
	lea	one-6(%rbx,%rbx,2), %esi	
				    	# Load into esi
					# [one]+(num_cpus*6)
					#
					# the above multiplies by three
					# esi = (ebx+(ebx*2))
	 				# and we double-incremented ebx 
					# earlier
	 
	mov	%ebp,%edi		# move output buffer to edi

	pop	%rdx			# restore call pointer
	call	*%rdx			# copy it (call strcat)

#	mov	$' ',%al		# print a space
	mov	$0x20,%al		# print a space
	
	stosb

	push %rbx
	push %rdx			# store strcat pointer

	#=========
	# MHz
	#=========
print_mhz:
#       mov     $('z'<<24+'H'<<16+'M'<<8+' '),%ebx
        mov     $(0x7a<<24+0x48<<16+0x4d<<8+0x20),%ebx
			   		# find ' MHz' and grab up to .
	                                # we are little endian
#	mov	$'.',%ah
	mov	$0x2e,%ah

	# below is same as "sub $(strcat-find_string),%edx
	# gas won't let us force the one-byte constant
	.byte 0x83,0xEA,strcat-find_string   
	
	call	*%rdx			# call find string

	mov	%ebx,%eax  		# clever way to get MHz in, sadly
	ror	$8,%eax			# not any smaller than a mov
	stosl	    			

	#=========
	# Chip Name
	#=========
chip_name:
#       mov     $('e'<<24+'m'<<16+'a'<<8+'n'),%ebx
        mov     $(0x65<<24+0x6d<<16+0x61<<8+0x6e),%ebx
					# find 'name\t: ' and grab up to \n
					# we are little endian
#	mov	$' ',%ah
	mov	$0x20,%ah
	call	*%rdx	   		# call find_string
	stosb
	call 	skip_spaces
	
	pop     %rdx
	pop     %rbx                    # restore chip count
	pop     %rsi
				
	call    *%rdx                   # ' Processor'
	cmpb    $2,%bl
	jne     print_s
	inc     %rsi   			# ! if singular, skip the s
print_s:
        call    *%rdx                   # 's, '
	
        push    %rsi                    # restore the values
	push    %rdx
			
	#========
	# RAM
	#========

#	push	%rdi	
#	push    $SYSCALL_SYSINFO	# sysinfo() syscall
#	pop	%rax	
#	mov	$sysinfo_buff,%edi
#	syscall
#	pop	%rdi

	# The following has to be a 64 bit load, to support
	# Ram > 4GB
	mov	(sysinfo_buff+S_TOTALRAM),%rax	# size in bytes of RAM
	shr	$20,%rax		# divide by 1024*1024 to get M
	adc	$0, %eax		# round 

	call num_to_ascii
	
	pop  %rdx	 		# restore strcat pointer
	
	pop     %rsi	 		# print 'M RAM, '
	call	*%rdx			# call strcat

	push	%rsi
	
	#========
	# Bogomips
	#========
	
#       mov     $('s'<<24+'p'<<16+'i'<<8+'m'),%ebx
        mov     $(0x73<<24+0x70<<16+0x69<<8+0x6d),%ebx
					# find 'mips\t: ' and grab up to \n
	mov	$0xa,%ah
	call	find_string

	pop	%rsi	   		# bogo total follows RAM 

	call 	*%rdx			# call strcat

	push	%rsi

	mov	%ebp,%ecx		# point ecx to out_buffer

	push	%rcx
	call	center_and_print	# center and print

	#=================================
	# Print Host Name
	#=================================
last_line:
	mov     %ebp,%edi		# point to output_buffer
	
	mov	$(uname_info+U_NODENAME),%esi	# host name from uname()
	call    *%rdx			# call strcat

	pop	%rcx	      		# ecx is unchanged
	call	center_and_print	# center and print
	
	pop	%rcx			# (.txt) pointer to default_colors
	
	call	write_stdout

	#================================
	# Exit
	#================================
exit:
	push	$SYSCALL_EXIT		# Put exit syscall in rax
	pop	%rax

	xor	%edi,%edi		# Make return value $0
	syscall


	#=================================
	# FIND_STRING 
	#=================================
	#   ah is char to end at
	#   ebx is 4-char ascii string to look for
	#   edi points at output buffer

find_string:
					
	mov	$disk_buffer-1,%esi	# look in cpuinfo buffer
find_loop:
	inc	%esi
	cmpb	$0, (%rsi)		# are we at EOF?
	je	done			# ! if so, done

	cmp	(%rsi), %ebx		# do the strings match?
	jne	find_loop		# ! if not, loop
	
					# ! if we get this far, we matched

find_colon:
	lodsb				# repeat till we find colon
	cmp	$0,%al
	je	done
#	cmp	$':',%al
	cmp	$0x3a,%al	
	jne	find_colon

skip_spaces:		
	lodsb				# skip spaces
	cmp	$0x20,%al		# Loser new intel chips have lots??
	je	skip_spaces
	
store_loop:	 
	cmp	$0,%al
	je	done
	cmp	%ah,%al			# is it end string?
	je 	almost_done		# ! if so, finish
#	cmp	$'\n',%al
	cmp	$0xa,%al
	je	almost_done
	stosb				# ! if not store and continue
	lodsb
	
	jmp	store_loop
	 
almost_done:	 
	movb	 $0, (%rdi)	        # replace last value with NUL 
done:
	ret


	#================================
	# strcat
	#================================

strcat:
	lodsb				# load a byte from [ds:esi]
	stosb				# store a byte to [es:edi]
	cmp	$0,%al			# is it zero?
	jne	strcat			# ! if not loop
	dec	%edi			# point to one less than null
	ret				# return

	#==============================
	# center_and_print
	#==============================
	# string to center in ecx

center_and_print:
	push    %rdx			# save strcat pointer
	push	%rcx			# save the string pointer
	inc	%edi			# move to a clear buffer
	push	%rdi			# save for later

#	mov	$('['<<8+27),%ax	# we want to output ^[[
	mov	$(0x5b<<8+27),%ax	# we want to output ^[[	
	stosw

	cdq	      			# clear dx
	
str_loop2:				# find end of string	
	inc	%edx
	cmpb	$0,(%rcx,%rdx)		# repeat till we find zero
	jne	str_loop2
	
	push	$81	 		# one added to cheat, we don't
					# count the trailing '\n'
	pop	%rax
	
	cmp	%eax,%edx		# see if we are >=80
	jl	not_too_big		# ! if so, don't center
	push	$80
	pop	%rdx
	
not_too_big:			
	sub	%edx,%eax		# subtract size from 80
	
	shr	%eax			# then divide by 2
	
	call	num_to_ascii		# print number of spaces
#	mov	$'C',%al		# tack a 'C' on the end
	mov	$0x43,%al		# tack a 'C' on the end	
					# ah is zero from num_to_ascii
	stosw				# store C and a NULL
	pop  %rcx			# pop the pointer to ^[[xC
	
	call write_stdout		# write to the screen
	
done_center:
	pop  %rcx			# restore string pointer
	     				# and trickily print the real string

	pop %rdx			# restore strcat pointer

	#================================
	# WRITE_STDOUT
	#================================
	# ecx has string
	# eax,ebx,ecx,edx trashed
write_stdout:
	push    %rdx
	push	$SYSCALL_WRITE		# put 4 in eax (write syscall)
	pop     %rax     		# in 3 bytes of code
	
	cdq   	      			# clear edx
	
	lea	1(%rdx),%edi		# put 1 in ebx (stdout)
					# in 3 bytes of code

	mov	%ecx,%esi
	
str_loop1:
	inc	%edx
	cmpb	$0,(%rcx,%rdx)		# repeat till zero
	jne	str_loop1

	syscall  			# run the syscall
	pop	%rdx
	ret

	##############################
	# num_to_ascii
	##############################
	# ax = value to print
	# edi points to where we want it
	
num_to_ascii:
	push    $10
	pop     %rbx
	xor     %ecx,%ecx       # clear ecx
div_by_10:
	cdq                     # clear edx
	div     %ebx            # divide
	push    %rdx            # save for later
	inc     %ecx            # add to length counter
	or      %eax,%eax       # was Q zero?
	jnz     div_by_10       # ! if not divide again
	
write_out:
	pop     %rax            # restore in reverse order
	add     $0x30, %al      # convert to ASCII
	stosb                   # save digit
	loop    write_out       # loop till done
	ret

#===========================================================================
#	section .data
#===========================================================================
.data

ver_string:	.ascii	" Version \0"
compiled_string:	.ascii	", Compiled \0"
processor:		.ascii  " Processor\0"
s_comma:		.ascii  "s, \0"
ram_comma:	.ascii	"M RAM, \0"
bogo_total:	.ascii	" Bogomips Total\n\0"

default_colors:	.ascii "\033[0m\n\n\0"

cpuinfo:	.ascii	"/proc/cpuinfo\0"


one:	.ascii	"One\0\0\0"
two:	.ascii	"Two\0\0\0"
three:	.ascii	"Three\0"
four:	.ascii	"Four\0"

.include	"../logo.lzss_new"

disk_buffer:
.ascii "processor	: 0\n"
.ascii "vendor_id	: GenuineIntel\n"
.ascii "cpu family	: 15\n"
.ascii "model		: 6\n"
.ascii "model name	: Intel(R) Xeon(TM) CPU 3.46GHz\n"
.ascii "stepping	: 4\n"
.ascii "cpu MHz		: 3200.000\n"
.ascii "cache size	: 2048 KB\n"
.ascii "physical id	: 0\n"
.ascii "siblings	: 2\n"
.ascii "core id		: 0\n"
.ascii "cpu cores	: 2\n"
.ascii "apicid		: 0\n"
.ascii "initial apicid	: 0\n"
.ascii "fpu		: yes\n"
.ascii "fpu_exception	: yes\n"
.ascii "cpuid level	: 6\n"
.ascii "wp		: yes\n"
.ascii "flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx lm constant_tsc pebs bts pni dtes64 monitor ds_cpl vmx est cid cx16 xtpr pdcm lahf_lm tpr_shadow\n"
.ascii "bogomips	: 6934.38\n"
.ascii "clflush size	: 64\n"
.ascii "cache_alignment	: 128\n"
.ascii "address sizes	: 36 bits physical, 48 bits virtual\n"
.ascii "power management:\n"
.ascii "\n"
.ascii "processor	: 1\n"
.ascii "vendor_id	: GenuineIntel\n"
.ascii "cpu family	: 15\n"
.ascii "model		: 6\n"
.ascii "model name	: Intel(R) Xeon(TM) CPU 3.46GHz\n"
.ascii "stepping	: 4\n"
.ascii "cpu MHz		: 3200.000\n"
.ascii "cache size	: 2048 KB\n"
.ascii "physical id	: 1\n"
.ascii "siblings	: 2\n"
.ascii "core id		: 0\n"
.ascii "cpu cores	: 2\n"
.ascii "apicid		: 4\n"
.ascii "initial apicid	: 4\n"
.ascii "fpu		: yes\n"
.ascii "fpu_exception	: yes\n"
.ascii "cpuid level	: 6\n"
.ascii "wp		: yes\n"
.ascii "flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx lm constant_tsc pebs bts pni dtes64 monitor ds_cpl vmx est cid cx16 xtpr pdcm lahf_lm tpr_shadow\n"
.ascii "bogomips	: 6934.13\n"
.ascii "clflush size	: 64\n"
.ascii "cache_alignment	: 128\n"
.ascii "address sizes	: 36 bits physical, 48 bits virtual\n"
.ascii "power management:\n\0"

uname_info:
.ascii "Linux\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

.ascii "domori\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

.ascii "2.6.29\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

.ascii "#1 SMP Mon May 4 09:51:54 EDT 2009\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

sysinfo_buff:
.long 0,0,0,0,0,0,0,0,2048*1024*1024,0,0,0,0,0,0,0


#============================================================================
#	section .bss
#============================================================================
.bss

.lcomm  text_buf, (N+F-1)
.lcomm	out_buffer,16384
