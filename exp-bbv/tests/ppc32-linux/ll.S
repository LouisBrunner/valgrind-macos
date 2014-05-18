#
#  linux_logo in ppc assembly language
#    based on the code from ll_asm-0.36
#
#  By Vince Weaver <vince _at_ deater.net>
#
# Modified to remove non-deterministic system calls
# And to avoid reading from /proc
#

# offsets into the results returned by the uname syscall
.equ U_SYSNAME,0
.equ U_NODENAME,65
.equ U_RELEASE,65*2
.equ U_VERSION,(65*3)
.equ U_MACHINE,(65*4)
.equ U_DOMAINNAME,65*5

# offset into the SYSCALL_SYSINFO buffer
.equ S_TOTALRAM,16

# Sycscalls
.equ SYSCALL_EXIT,     1
#.equ SYSCALL_READ,     3
.equ SYSCALL_WRITE,    4
#.equ SYSCALL_OPEN,     5
#.equ SYSCALL_CLOSE,    6
#.equ SYSCALL_SYSINFO,116
#.equ SYSCALL_UNAME,  122

#
.equ STDIN, 0
.equ STDOUT,1
.equ STDERR,2

.equ BSS_BEGIN,25
.equ DATA_BEGIN,26

.include "logo.include"

	.globl _start	
_start:	

        #========================
	# Initialization
	#========================
	

#	eieio				# coolest opcode of all time ;)
					# not needed, but I had to put it here
  	# the hack loading BSS_BEGIN and DATA_BEGIN
	# saves one instruction on any future load from memory
	# as we can just do an addi rather than an lis;addi

	lis	25,bss_begin@ha
	addi	25,25,bss_begin@l
	
	lis	26,data_begin@ha
	addi	26,26,data_begin@l

	addi	14,BSS_BEGIN,(out_buffer-bss_begin)
					# the output buffer

	addi	21,BSS_BEGIN,(text_buf-bss_begin)
 	     	

	mr	17,14		    	# store out-buffer for later

        #=========================
	# PRINT LOGO
	#=========================

# LZSS decompression algorithm implementation
# by Stephan Walter 2002, based on LZSS.C by Haruhiko Okumura 1989
# optimized some more by Vince Weaver


	li	8,(N-F)			# grab "R"

	addi	9,DATA_BEGIN,(logo-data_begin)-1
					# logo_pointer

	addi	12,DATA_BEGIN,(logo_end-data_begin)-1
					# end of the logo


	mr      16,17

decompression_loop:
	lbzu 	10,1(9)			# load in a byte
					# auto-update
	mr	11,10			# copy to 11
	ori	11,11,0xff00		# re-load top as a hackish 
					# 8-bit counter

test_flags:
	cmpw	0,12,9			# have we reached the end?
	ble	done_logo		# ! if so exit

	andi.	13,11,0x1
	srawi   11,11,1
	
	bne	0,discrete_char

offset_length:
	lbzu  	10,1(9)
	lbzu	24,1(9)
	slwi	24,24,8
	or	24,24,10
	
	mr	10,24

	srawi  15,10,P_BITS
	addi   15,15,THRESHOLD+1 # cl = ax >> (P_BITS)+THRESH+1
	       			 # = match length
	       			 
output_loop:
	andi.  24,24,(POSITION_MASK<<8+0xff)	# mask it
	lbzx   10,21,24				
	addi   24,24,1
	
store_byte:
	stbu   10,1(16)
	
	stbx    10,21,8
	addi	8,8,1
	andi.	8,8,(N-1)

	addic.	15,15,-1
	bne	0,output_loop
	
	andi.	13,11,0xff00
	bne	test_flags
	
	b	decompression_loop

discrete_char:

	lbzu    10,1(9)
	li	15,1

	b       store_byte

done_logo:

	addi	4,17,1		# restore (plus one because r17 is decremented)
	bl	write_stdout	# and print the logo
	

        #==========================
	# First Line
	#==========================

	
	#==========================
	# PRINT VERSION
	#==========================
	
#	li	0,SYSCALL_UNAME		# uname syscall
#	addi	3,BSS_BEGIN,(uname_info-bss_begin)		
					# uname struct
#	sc				# do syscall


	addi	16,DATA_BEGIN,(uname_info-data_begin)+U_SYSNAME@l-1	
					# os-name from uname "Linux"
	bl	strcat
	
	addi	16,DATA_BEGIN,(ver_string-data_begin)-1
					# source is " Version "
	bl 	strcat
	
	addi	16,DATA_BEGIN,(uname_info-data_begin)+U_RELEASE@l-1
					# version from uname "2.4.1"
	bl 	strcat
	
	addi	16,DATA_BEGIN,(compiled_string-data_begin)-1
					# source is ", Compiled "
	bl 	strcat

	addi	16,DATA_BEGIN,(uname_info-data_begin)+U_VERSION-1
      					# compiled date
	bl 	strcat
	
	bl	center_and_print	# write it to screen
	

	#===============================
	# Middle-Line
	#===============================
	
	#=========
	# Load /proc/cpuinfo into buffer
	#=========

#	li	0,SYSCALL_OPEN		# open()
#	addi	3,DATA_BEGIN,(cpuinfo-data_begin)		
					# '/proc/cpuinfo'
#	li	4,0			# O_RDONLY <bits/fcntl.h>
#	sc				# syscall.  fd in r0.  
					# we should check that r0>=0
					
#	mr	13,3			# save fd in r13
	
#	li	0,SYSCALL_READ		# read
#	addi	4,BSS_BEGIN,(disk_buffer-bss_begin)
#	li	5,4096		 	# 4096 is maximum size of proc file ;)
#	sc	

#	mr	3,13			# restore fd
#	li	0,6			# close
#	sc

	#=============
	# Number of CPUs
	#=============
	
	mr	14,17 			# point output to out_buf

	# Assume 1 CPU for now
	# my iBook's /proc/cpuinfo does not have a "processor" line ???
	
	addi	16,DATA_BEGIN,(one-data_begin)-1
	bl	strcat
	
	#=========
	# MHz
	#=========
	
    	lis	20,('l'<<8)+'o'		# find 'lock ' and grab up to M
	addi	20,20,('c'<<8)+'k'
	li	23,'M'			
   	bl	find_string
   
	addi	16,DATA_BEGIN,(megahertz-data_begin)-1
					# print 'MHz '
	bl	strcat
   
  
	#=========
	# Chip Name
	#=========
	
   	lis     20,('c'<<8)+'p'     	# find 'cpu\t: ' and grab up to \n
	addi	20,20,('u'<<8)+'\t'
	li	23,'\n'
	bl	find_string
	
	addi	16,DATA_BEGIN,(comma-data_begin)-1
					# print ', '
	bl	strcat
	
	#========
	# RAM
	#========
	
#	li	0,SYSCALL_SYSINFO	# sysinfo() syscall
#	addi	3,BSS_BEGIN,(sysinfo_buff-bss_begin)
					# sysinfo_buffer

#	sc

	lwz	4,(sysinfo_buff+S_TOTALRAM-data_begin)(DATA_BEGIN)
					# load bytes of RAM into r4

	srawi	4,4,20		# divide by 2^20 to get MB
	li	5,0

	bl	num_to_ascii

	addi	16,DATA_BEGIN,(ram_comma-data_begin)-1
					# print 'M RAM, '

	bl	strcat
	
	#========
	# Bogomips
	#========
	
	lis	20,('m'<<8)+'i'		# find 'mips' and grab up to \n
	addi	20,20,('p'<<8)+'s'
	li	23,'\n'
	bl	find_string
      
	addi	16,DATA_BEGIN,(bogo_total-data_begin)-1
					# print "Bogomips Total"
	bl	strcat

	bl	center_and_print	# center it


	#=================================
	# Print Host Name
	#=================================
	
	mr	14,17			# restore out buffer
	
	addi	16,DATA_BEGIN,((uname_info-data_begin)+U_NODENAME)-1
					# hostname		      
					
	bl	strcat				
	
	bl	center_and_print

	#================================
	# Exit
	#================================
exit:	
        li      3,0		# 0 exit value
	li      0,SYSCALL_EXIT  # put the exit syscall number in eax
	sc	             	# and exit




	#=================================
	# FIND_STRING 
	#=================================
	#   r23 is char to end at
	#   r20 is the 4-char ascii string to look for
	#   r14 points at output buffer
	#   r16,r21

find_string:
		
	addi	16,DATA_BEGIN,(disk_buffer-data_begin)-1	
					# look in cpuinfo buffer
					# -1 so we can use lbzu
	
find_loop:
	lwzu	13,1(16)		# load in 32 bits, incrementing 8bits
	cmpwi	13,0			# ! if null, we are done
	beq	done
	cmpw	13,20			# compare with out 4 char string
	bne	find_loop		# ! if no match, keep looping

	
					# ! if we get this far, we matched
					
	li	21,':'
find_colon:
	lbzu	13,1(16)		# repeat till we find colon
	cmpwi	13,0
	beq	done
	cmpw	13,21
	bne	find_colon

	addi	16,16,1			# skip a char [should be space]
	
store_loop:	 
	 lbzu	13,1(16)
	 cmpwi	13,0
	 beq	done
    	 cmpw	13,23			# is it end string?
	 beq 	almost_done		# ! if so, finish
	 stbu	13,1(14)		# ! if not store and continue
	 b	store_loop
	 
almost_done:	 
	li	13,0			# replace last value with null
	stb	13,1(14)

done:
	blr

	#================================
	# strcat
	#================================
	# r13 = "temp"
	# r16 = "source"
       	# r14 = "destination"
strcat:
	lbzu	13,1(16)		# load a byte from [r16]
	stbu	13,1(14)		# store a byte to [r14]
	cmpwi	13,0			# is it zero?
	bne	strcat			# ! if not loop
	subi	14,14,1			# point to one less than null
	blr				# return

	#==============================
	# center_and_print
	#==============================
	# r14 is end of buffer
	# r17 is start of buffer
	# r29 = saved link register
	# r4-r10, r19-r22, r30 trashed
	
center_and_print:

	mflr 	29			# back up return address

	subf	5,17,14			# see how long the output
					# buffer is
					
	cmpwi	5,80			# see if we are >80
        bgt	done_center		# ! if so, bail

	li	4,80			# 80 column screen
	subf	4,5,4			# subtract strlen
	srawi	23,4,1			# divide by two

	lis	4,escape@ha
	addi	4,4,escape@l
	bl	write_stdout

	mr	4,23
	li	5,1			# print to stdout
	bl	num_to_ascii		# print number
	
	lis	4,c@ha
	addi	4,4,c@l
	bl	write_stdout


done_center:	

	addi	4,17,1			# move string to output+1
	bl	write_stdout		# call write stdout

	lis	4,linefeed@ha
	addi	4,4,linefeed@l

	mtlr	29	      		# restore link register
					# and let write_stdout
					# return for us



	#================================
	# WRITE_STDOUT
	#================================
	# r4 has string
	# r0,r3,r4,r5,r6 trashed
		
write_stdout:
	li	0,SYSCALL_WRITE		# write syscall
	li	3,STDOUT		# stdout	
	
	li	5,0			# string length counter
strlen_loop:
	lbzx 	6,4,5			# get byte from (r4+r5)
       	addi	5,5,1			# increment counter
	cmpi	0,6,0			# is it zero?
	bne	strlen_loop		# ! if not keep counting
	addi	5,5,-1
	sc				# syscall
	
	blr				# return


	##############################
	# Num to Ascii
	##############################
	# num is in r4
	# r5 =0 then strcat, otherwise stdout
	# r5-r10,r19,r20,r21,r22,r30 trashed	

num_to_ascii:

	mflr    30			# save the link register

	addi	16,BSS_BEGIN,(num_to_ascii_end-bss_begin)
					# the end of a backwards growing
					# 10 byte long buffer.  
					
	li	20,10			# we will divide by 10
	mr	19,4			# load in the value passed
	
div_by_10:
	divw	21,19,20		# divide r19 by r20 put into r21 
	
	mullw	22,21,20		# find remainder.  1st q*dividend
	subf	22,22,19		# then subtract from original = R
	addi	22,22,0x30		# convert remainder to ascii
    	
	stbu	22,-1(16)		# Store to backwards buffer
	
	mr	19,21			# move Quotient as new dividend
	cmpwi	19,0			# was quotient zero?
	bne    	div_by_10		# ! if not keep dividing
	
write_out:
	cmpwi	5,0			# ! if r5 is 0 then skip ahead
	bne 	stdout_num		

	addi	16,16,-1		# point to the beginning
	bl	strcat			# and strcat it

	mtlr	30			# restore link register

	blr				# return
	
stdout_num:
        mr	4,16			# point to our buffer
	mtlr	30			# restore link register
	b	write_stdout		# stdout will return for us


#===========================================================================
.data
#===========================================================================


data_begin:

.include "logo.lzss_new"

ver_string:	.ascii	" Version \0"
compiled_string:	.ascii	", Compiled \0"
megahertz:	.ascii	"MHz PPC \0"
.equ space, ram_comma+6
.equ comma, ram_comma+5
linefeed:   	.ascii  "\n\0"
escape:		.ascii	"\033[\0"
c:		.ascii  "C\0"
ram_comma:	.ascii	"M RAM, \0"

bogo_total:	.ascii	" Bogomips Total\0"

default_colors:	.ascii	"\033[0m\n\n\0"

cpuinfo:	.ascii	"/proc/cpuinfo\0"

one:	.ascii	"One \0"

disk_buffer:
.ascii "processor	: 0\n"
.ascii "cpu		: 745/755\n"
.ascii "temperature 	: 22-24 C (uncalibrated)\n"
.ascii "clock		: 600.000000MHz\n"
.ascii "revision	: 51.17 (pvr 0008 3311)\n"
.ascii "bogomips	: 49.79\n"
.ascii "timebase	: 24960000\n"
.ascii "platform	: PowerMac\n"
.ascii "model		: PowerBook4,1\n"
.ascii "machine		: PowerBook4,1\n"
.ascii "motherboard	: PowerBook4,1 MacRISC2 MacRISC Power Macintosh\n"
.ascii "detected as	: 257 (iBook 2)\n"
.ascii "pmac flags	: 0000001b\n"
.ascii "L2 cache	: 256K unified\n"
.ascii "pmac-generation	: NewWorld\n\0"

uname_info:
.ascii "Linux\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "henparma\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "2.6.29\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "#1 Wed May 13 15:51:54 UTC 2009\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
.ascii "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

	
sysinfo_buff:
.long 0,0,0,0,512*1024*1024,0,0,0

#============================================================================
#.bss
#============================================================================

.lcomm bss_begin,0
.lcomm	num_to_ascii_buff,10
.lcomm num_to_ascii_end,1
.lcomm  text_buf, (N+F-1)	# These buffers must follow each other
.lcomm	out_buffer,16384










