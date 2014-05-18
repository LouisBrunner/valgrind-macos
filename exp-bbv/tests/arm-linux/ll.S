#
#  linux_logo in ARM assembly language
#    based on the code from ll_asm-0.41
#
#  By Vince Weaver <vince _at_ deater.net>
#
# Modified to remove non-deterministic system calls
# And to avoid reading from /proc

.include "logo.include"

# offsets into the results returned by the uname syscall
.equ U_SYSNAME,0
.equ U_NODENAME,65
.equ U_RELEASE,65*2
.equ U_VERSION,(65*3)
.equ U_MACHINE,(65*4)
.equ U_DOMAINNAME,65*5

# offset into the results returned by the sysinfo syscall
.equ S_TOTALRAM,16

# Sycscalls
.equ SYSCALL_EXIT,	1
.equ SYSCALL_WRITE,	4

#
.equ STDIN,0
.equ STDOUT,1
.equ STDERR,2

	.globl _start	
_start:
	ldr	r11,data_addr
	ldr	r12,bss_addr

	#=========================
	# PRINT LOGO
	#=========================

# LZSS decompression algorithm implementation
# by Stephan Walter 2002, based on LZSS.C by Haruhiko Okumura 1989
# optimized some more by Vince Weaver

	ldr	r1,out_addr		@ buffer we are printing to

	mov     r2,#(N-F)		@ R

	add	r3,r11,#(logo-data_begin)
					@ r3 points to logo data
	ldr	r8,logo_end_addr
					@ r8 points to logo end
	ldr	r9,text_addr		@ r9 points to text buf

decompression_loop:
	ldrb	r4,[r3],#+1		@ load a byte, increment pointer 	

	mov	r5,#0xff		@ load top as a hackish 8-bit counter
	orr 	r5,r4,r5,LSL #8		@ shift 0xff left by 8 and or in the byte we loaded

test_flags:
	cmp	r3,r8		@ have we reached the end?
	bge	done_logo  	@ if so, exit

	lsrs 	r5,#1		@ shift bottom bit into carry flag
	bcs	discrete_char	@ if set, we jump to discrete char

offset_length:
	ldrb	r0,[r3],#+1	@ load a byte, increment pointer
	ldrb	r4,[r3],#+1	@ load a byte, increment pointer	
				@ we can't load halfword as no unaligned loads on arm

	orr	r4,r0,r4,LSL #8	@ merge back into 16 bits
				@ this has match_length and match_position

	mov	r7,r4		@ copy r4 to r7
				@ no need to mask r7, as we do it
				@ by default in output_loop

	mov	r0,#(THRESHOLD+1)
	add	r6,r0,r4,LSR #(P_BITS)
				@ r6 = (r4 >> P_BITS) + THRESHOLD + 1
				@                       (=match_length)

output_loop:
	ldr	r0,pos_mask		@ urgh, can't handle simple constants
	and	r7,r7,r0		@ mask it
	ldrb 	r4,[r9,r7]		@ load byte from text_buf[]
	add	r7,r7,#1		@ advance pointer in text_buf

store_byte:
	strb	r4,[r1],#+1		@ store a byte, increment pointer
	strb	r4,[r9,r2]		@ store a byte to text_buf[r]
	add 	r2,r2,#1		@ r++
	mov	r0,#(N)
	sub	r0,r0,#1		@ grrr no way to get this easier
	and 	r2,r2,r0		@ mask r

	subs	r6,r6,#1		@ decement count
	bne 	output_loop		@ repeat until k>j

	tst	r5,#0xff00		@ are the top bits 0?
	bne	test_flags		@ if not, re-load flags

	b	decompression_loop

discrete_char:
	ldrb	r4,[r3],#+1		@ load a byte, increment pointer 		
	mov	r6,#1			@ we set r6 to one so byte
					@ will be output once

	b	store_byte		@ and store it


# end of LZSS code

done_logo:
	ldr	r1,out_addr		@ buffer we are printing to

	bl	write_stdout		@ print the logo

	#==========================
	# PRINT VERSION
	#==========================
first_line:

	mov	r0,#0
	add	r1,r11,#(uname_info-data_begin)
						@ os-name from uname "Linux"

	ldr	r10,out_addr			@ point r10 to out_buffer

	bl	strcat				@ call strcat

	
	add	r1,r11,#(ver_string-data_begin) @ source is " Version "
	bl 	strcat			        @ call strcat

	add	r1,r11,#((uname_info-data_begin)+U_RELEASE)
						@ version from uname, ie "2.6.20"
	bl	strcat				@ call strcat
	
	add	r1,r11,#(compiled_string-data_begin)
						@ source is ", Compiled "
	bl	strcat				@  call strcat

	add	r1,r11,#((uname_info-data_begin)+U_VERSION)
						@ compiled date
	bl	strcat				@ call strcat

	mov	r3,#0xa	
	strb	r3,[r10],#+1		@ store a linefeed, increment pointer
	strb	r0,[r10],#+1		@ NUL terminate, increment pointer
	
	bl	center_and_print	@ center and print

	@===============================
	@ Middle-Line
	@===============================
middle_line:		
	@=========
	@ Load /proc/cpuinfo into buffer
	@=========

	ldr	r10,out_addr		@ point r10 to out_buffer
	
	@=============
	@ Number of CPUs
	@=============
number_of_cpus:

	add	r1,r11,#(one-data_begin)
					# cheat.  Who has an SMP arm?
	bl	strcat

	@=========
	@ MHz
	@=========
print_mhz:
	
	@ the arm system I have does not report MHz

	@=========
	@ Chip Name
	@=========
chip_name:	
	mov	r0,#'s'
	mov	r1,#'o'
	mov	r2,#'r'
	mov	r3,#' '
	bl	find_string
					@ find 'sor\t: ' and grab up to ' '

	add	r1,r11,#(processor-data_begin)
					@ print " Processor, "
	bl	strcat	
	
	@========
	@ RAM
	@========
	
	
	ldr	r3,[r11,#((sysinfo_buff-data_begin)+S_TOTALRAM)]
					@ size in bytes of RAM
	movs	r3,r3,lsr #20		@ divide by 1024*1024 to get M
	adc	r3,r3,#0		@ round

	mov	r0,#1
	bl num_to_ascii
	
	add	r1,r11,#(ram_comma-data_begin)
					@ print 'M RAM, '
	bl	strcat			@ call strcat
	

	@========
	@ Bogomips
	@========

	mov	r0,#'I'
	mov	r1,#'P'
	mov	r2,#'S'
	mov	r3,#'\n'
	bl	find_string

	add	r1,r11,#(bogo_total-data_begin)
	bl	strcat			@ print bogomips total
	
	bl	center_and_print	@ center and print

	#=================================
	# Print Host Name
	#=================================
last_line:
	ldr	r10,out_addr		@ point r10 to out_buffer	
	
	add	r1,r11,#((uname_info-data_begin)+U_NODENAME)
					@ host name from uname()
	bl	strcat			@ call strcat
	
	bl	center_and_print	@ center and print

	add	r1,r11,#(default_colors-data_begin)
					@ restore colors, print a few linefeeds
	bl	write_stdout
	

	@================================
	@ Exit
	@================================
exit:
	mov	r0,#0				@ result is zero
	mov	r7,#SYSCALL_EXIT
	swi	0x0				@ and exit


	@=================================
	@ FIND_STRING 
	@=================================
	@ r0,r1,r2 = string to find
	@ r3 = char to end at
	@ r5 trashed
find_string:
	ldr	r7,disk_addr		@ look in cpuinfo buffer
find_loop:
	ldrb	r5,[r7],#+1		@ load a byte, increment pointer	
	cmp	r5,r0			@ compare against first byte
	ldrb	r5,[r7]			@ load next byte
	cmpeq	r5,r1			@ if first byte matched, comp this one
	ldrb	r5,[r7,#+1]		@ load next byte 
	cmpeq	r5,r2			@ if first two matched, comp this one
	beq	find_colon		@ if all 3 matched, we are found
	
	cmp	r5,#0			@ are we at EOF?
	beq	done			@ if so, done

	b	find_loop
	
find_colon:
	ldrb	r5,[r7],#+1		@ load a byte, increment pointer
	cmp	r5,#':'
	bne	find_colon		@ repeat till we find colon

	add	r7,r7,#1		@ skip the space
		
store_loop:
	ldrb	r5,[r7],#+1		@ load a byte, increment pointer
	strb	r5,[r10],#+1		@ store a byte, increment pointer	
	cmp	r5,r3
	bne	store_loop
	
almost_done:
	mov	r0,#0
	strb	r0,[r10],#-1		@ replace last value with NUL

done:
	bx	r14			@ return

	#================================
	# strcat
	#================================
	# value to cat in r1
	# output buffer in r10
	# r3 trashed
strcat:
	ldrb	r3,[r1],#+1		@ load a byte, increment pointer 
	strb	r3,[r10],#+1		@ store a byte, increment pointer
	cmp	r3,#0			@ is it zero?
	bne	strcat			@ if not loop
	sub	r10,r10,#1		@ point to one less than null
	bx	r14			@ return
	

	#==============================
	# center_and_print
	#==============================
	# string to center in at output_buffer

center_and_print:

	stmfd	SP!,{LR}		@ store return address on stack

	add	r1,r11,#(escape-data_begin)
					@ we want to output ^[[
	bl	write_stdout
		
str_loop2:				
	ldr	r2,out_addr		@ point r2 to out_buffer
	sub	r2,r10,r2		@ get length by subtracting

	rsb	r2,r2,#81		@ reverse subtract!  r2=81-r2
					@ we use 81 to not count ending \n

	bne	done_center		@ if result negative, don't center
	
	lsrs	r3,r2,#1		@ divide by 2
	adc	r3,r3,#0		@ round?

	mov	r0,#0			@ print to stdout
	bl	num_to_ascii		@ print number of spaces

	add	r1,r11,#(C-data_begin)
					@ we want to output C
	bl	write_stdout

done_center:
	ldr	r1,out_addr		@ point r1 to out_buffer
	ldmfd	SP!,{LR}		@ restore return address from stack

	#================================
	# WRITE_STDOUT
	#================================
	# r1 has string
	# r0,r2,r3 trashed
write_stdout:
	mov	r2,#0				@ clear count

str_loop1:
	add	r2,r2,#1
	ldrb	r3,[r1,r2]
	cmp	r3,#0
	bne	str_loop1			@ repeat till zero

write_stdout_we_know_size:	
	mov	r0,#STDOUT			@ print to stdout
	mov	r7,#SYSCALL_WRITE
	swi	0x0		 		@ run the syscall
	bx	r14				@ return

	
	@#############################
	@ num_to_ascii
	@#############################
	@ r3 = value to print
	@ r0 = 0=stdout, 1=strcat
	
num_to_ascii:
	stmfd	SP!,{r10,LR}		@ store return address on stack
	add	r10,r12,#((ascii_buffer-bss_begin))
	add	r10,r10,#10
					@ point to end of our buffer

	mov	r4,#10		@ we'll be dividing by 10
div_by_10:
	bl	divide		@ Q=r7,$0, R=r8,$1
	add	r8,r8,#0x30	@ convert to ascii
	strb	r8,[r10],#-1	@ store a byte, decrement pointer	
	adds	r3,r7,#0	@ move Q in for next divide, update flags
	bne	div_by_10	@ if Q not zero, loop
	
write_out:
	add	r1,r10,#1	@ adjust pointer
	ldmfd	SP!,{r10,LR}	@ restore return address from stack
	
	cmp	r0,#0
	bne	strcat		@ if 1, strcat
	
	b write_stdout		@ else, fallthrough to stdout

	
	@===================================================
	@ Divide - because ARM has no hardware int divide
	@ yes this is an awful algorithm, but simple
	@  and uses few registers
	@==================================================
	@ r3=numerator   r4=denominator
	@ r7=quotient    r8=remainder
	@ r5=trashed
divide:

	mov	r7,#0		@ zero out quotient
divide_loop:
	mul	r5,r7,r4	@ multiply Q by denominator
	add	r7,r7,#1	@ increment quotient
	cmp	r5,r3		@ is it greater than numerator?
	ble	divide_loop	@ if not, loop
	sub	r7,r7,#2	@ otherwise went too far, decrement
				@ and done
	
	mul	r5,r7,r4	@ calculate remainder
	sub	r8,r3,r5	@ R=N-(Q*D)
	bx	r14		@ return

	
bss_addr:	.word bss_begin
data_addr:	.word data_begin
out_addr:	.word out_buffer
disk_addr:	.word disk_buffer
logo_end_addr:	.word logo_end
pos_mask:	.word ((POSITION_MASK<<8)+0xff)
text_addr:	.word text_buf
							
#===========================================================================
#	section .data
#===========================================================================
.data
data_begin:
ver_string:	.ascii	" Version \0"
compiled_string:	.ascii	", Compiled \0"
processor:	.ascii	" Processor, \0"
ram_comma:	.ascii	"M RAM, \0"
bogo_total:	.ascii	" Bogomips Total\n\0"

default_colors:	.ascii "\033[0m\n\n\0"
escape:		.ascii "\033[\0"
C:		.ascii "C\0"

one:	.ascii	"One \0"

uname_info:
.ascii "Linux\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "lindt\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "2.6.32\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "#1 Wed May 13 15:51:54 UTC 2009\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"


disk_buffer:
.ascii "Processor   : Feroceon 88FR131 rev 1 (v5l)\n"
.ascii "BogoMIPS    : 1192.75\n"
.ascii "Features    : swp half thumb fastmult edsp \n"
.ascii "CPU implementer	  : 0x56\n"
.ascii "CPU architecture: 5TE\n"
.ascii "CPU variant	  : 0x2\n"
.ascii "CPU part	  : 0x131\n"
.ascii "CPU revision	  : 1\n"
.ascii "\n"
.ascii "Hardware	  : Marvell SheevaPlug Reference Board\n"
.ascii "Revision	  : 0000\n"
.ascii "Serial		    : 0000000000000000\n\0"


sysinfo_buff:
.long 0,0,0,0,512*1024*1024,0,0,0

.include	"logo.lzss_new"


#============================================================================
#	section .bss
#============================================================================
.bss
bss_begin:
.lcomm ascii_buffer,10
.lcomm  text_buf, (N+F-1)
.lcomm	out_buffer,16384
