
##--------------------------------------------------------------------##
##--- Support routines for the JITter output.                      ---##
##---                                                 vg_helpers.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an extensible x86 protected-mode
  emulator for monitoring program execution on x86-Unixes.

  Copyright (C) 2000-2003 Julian Seward 
     jseward@acm.org

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  02111-1307, USA.

  The GNU General Public License is contained in the file COPYING.
*/

#include "vg_constants.h"

/* ------------------ SIMULATED CPU HELPERS ------------------ */
/* A stubs for a return which we want to catch: a signal return.
   returns and pthread returns.  In the latter case, the thread's
   return value is in %EAX, so we pass this as the first argument
   to the request.  In both cases we use the user request mechanism.
   You need to to read the definition of VALGRIND_MAGIC_SEQUENCE
   in valgrind.h to make sense of this.
*/
.global VG_(signalreturn_bogusRA)
VG_(signalreturn_bogusRA):
	subl	$20, %esp	# allocate arg block
	movl	%esp, %edx	# %edx == &_zzq_args[0]
	movl	$VG_USERREQ__SIGNAL_RETURNS, 0(%edx)	# request
	movl	$0, 4(%edx)	# arg1
	movl	$0, 8(%edx)	# arg2
	movl	$0, 12(%edx)	# arg3
	movl	$0, 16(%edx)	# arg4
	movl	%edx, %eax
	# and now the magic sequence itself:
	roll $29, %eax
	roll $3, %eax
	rorl $27, %eax
	rorl $5, %eax
	roll $13, %eax
	roll $19, %eax
	# should never get here
	pushl	$signalreturn_bogusRA_panic_msg
	call	VG_(core_panic)
	
.data
signalreturn_bogusRA_panic_msg:
.ascii	"vg_signalreturn_bogusRA: VG_USERREQ__SIGNAL_RETURNS was missed"
.byte	0
.text	
	


	
/* ------------------ REAL CPU HELPERS ------------------ */
/* The rest of this lot run on the real CPU. */
	
/* Various helper routines, for instructions which are just too
   darn tedious for the JITter to output code in-line:
	
	* integer division
	* integer multiplication
        * setting and getting obscure eflags
	* double-length shifts
	
   All routines use a standard calling convention designed for
   calling from translations, in which the incoming args are
   underneath the return address, the callee saves _all_ registers,
   and the incoming parameters can be modified, to return results.
*/

/* Fetch the time-stamp-ctr reg.
   On entry:
	dummy, replaced by %EAX value
	dummy, replaced by %EDX value
	RA   <- %esp
*/
.global VG_(helper_RDTSC)
VG_(helper_RDTSC):
	pushl	%eax
	pushl	%edx
	rdtsc
	movl	%edx, 12(%esp)
	movl	%eax, 16(%esp)
	popl	%edx
	popl	%eax
	ret


/* Do the CPUID instruction.
   On entry:
	dummy, replaced by %EAX value
	dummy, replaced by %EBX value
	dummy, replaced by %ECX value
	dummy, replaced by %EDX value
	RA   <- %esp

   As emulating a real CPUID is kinda hard, as it
   has to return different values depending on EAX, 
   we just pretend to not support CPUID at all until
   it becomes a problem. This will for sure disable
   all MMX / 3dnow checks so they don't bother us
   with code we don't understand. (Dirk <dirk@kde.org>)
   
   http://www.sandpile.org/ia32/cpuid.htm

   (Later: we instead pretend to be like Werner's P54C P133, that is
    an original pre-MMX Pentium).
   <werner> cpuid words (0): 0x1 0x756e6547 0x6c65746e 0x49656e69
   <werner> cpuid words (1): 0x52b 0x0 0x0 0x1bf
*/
.global VG_(helper_CPUID)
VG_(helper_CPUID):
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	movl	32(%esp), %eax
/*
	cpuid
*/
/*
        xor     %eax,%eax
        xor     %ebx,%ebx
        xor     %ecx,%ecx
        xor     %edx,%edx
*/
	cmpl	$0, %eax
	jz	cpuid__0
	movl	$0x52b, %eax
	movl	$0x0,   %ebx
	movl	$0x0,   %ecx
	movl	$0x008001bf, %edx
	jmp	cpuid__99
cpuid__0:
	movl	$0x1,        %eax
	movl	$0x756e6547, %ebx
	movl	$0x6c65746e, %ecx
	movl	$0x49656e69, %edx
cpuid__99:
		
	movl	%edx, 20(%esp)
	movl	%ecx, 24(%esp)
	movl	%ebx, 28(%esp)
	movl	%eax, 32(%esp)
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret


/* Fetch the FPU status register.
   On entry:
	dummy, replaced by result
	RA   <- %esp
*/
.global VG_(helper_fstsw_AX)
VG_(helper_fstsw_AX):
	pushl	%eax
	pushl	%esi
	movl	VGOFF_(m_ssestate), %esi

	pushfl
	cmpb	$0, VG_(have_ssestate)
	jz	aa1nosse
	fxrstor	(%ebp, %esi, 4)
	jmp	aa1merge
aa1nosse:
	frstor	(%ebp, %esi, 4)
aa1merge:
	popfl

	fstsw	%ax
	popl	%esi
	movw	%ax, 8(%esp)
	popl	%eax
	ret


/* Copy %ah into %eflags.
   On entry:
	value of %eax
	RA   <- %esp
*/
.global VG_(helper_SAHF)
VG_(helper_SAHF):
	pushl	%eax
	movl	8(%esp), %eax
	sahf
	popl	%eax
	ret

/* Copy %eflags into %ah.
   On entry:
	value of %eax
	RA   <- %esp
*/
.global VG_(helper_LAHF)
VG_(helper_LAHF):
	pushl	%eax
	movl	8(%esp), %eax
	lahf
	movl	%eax, 8(%esp)
	popl	%eax
	ret


/* Do %al = DAS(%al).  Note that the passed param has %AL as the least
   significant 8 bits, since it was generated with GETB %AL,
   some-temp.  Fortunately %al is the least significant 8 bits of
   %eax anyway, which is why it's safe to work with %eax as a
   whole. 
 
   On entry:
	value of %eax
	RA   <- %esp
*/
.global VG_(helper_DAS)
VG_(helper_DAS):
	pushl	%eax
	movl	8(%esp), %eax
	das
 	movl	%eax, 8(%esp)
	popl	%eax
	ret


/* Similarly, do %al = DAA(%al). */
.global VG_(helper_DAA)
VG_(helper_DAA):
       pushl   %eax
       movl    8(%esp), %eax
       daa
       movl    %eax, 8(%esp)
       popl    %eax
       ret
	

/* Bit scan forwards/reverse.  Sets flags (??).
   On entry:
	value, replaced by result
	RA   <- %esp
*/
.global VG_(helper_bsr)
VG_(helper_bsr):
	pushl	%eax
	movl	12(%esp), %eax
	bsrl	8(%esp), %eax
	movl	%eax, 12(%esp)
	popl	%eax
	ret

.global VG_(helper_bsf)
VG_(helper_bsf):
	pushl	%eax
	movl	12(%esp), %eax
	bsfl	8(%esp), %eax
	movl	%eax, 12(%esp)
	popl	%eax
	ret


/* 32-bit double-length shift left/right.
   On entry:
	amount
	src
	dst
	RA   <- %esp
*/
.global VG_(helper_shldl)
VG_(helper_shldl):
	pushl	%eax
	pushl	%ebx
	pushl	%ecx

	movb	24(%esp), %cl
	movl	20(%esp), %ebx
	movl	16(%esp), %eax
	shldl	%cl, %ebx, %eax
	movl	%eax, 16(%esp)
	
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret

.global VG_(helper_shldw)
VG_(helper_shldw):
	pushl	%eax
	pushl	%ebx
	pushl	%ecx

	movb	24(%esp), %cl
	movw	20(%esp), %bx
	movw	16(%esp), %ax
	shldw	%cl, %bx, %ax
	movw	%ax, 16(%esp)
	
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret

.global VG_(helper_shrdl)
VG_(helper_shrdl):
	pushl	%eax
	pushl	%ebx
	pushl	%ecx

	movb	24(%esp), %cl
	movl	20(%esp), %ebx
	movl	16(%esp), %eax
	shrdl	%cl, %ebx, %eax
	movl	%eax, 16(%esp)
	
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret

.global VG_(helper_shrdw)
VG_(helper_shrdw):
	pushl	%eax
	pushl	%ebx
	pushl	%ecx

	movb	24(%esp), %cl
	movw	20(%esp), %bx
	movw	16(%esp), %ax
	shrdw	%cl, %bx, %ax
	movw	%ax, 16(%esp)
	
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret

	
/* Get the direction flag, and return either 1 or -1. */
.global VG_(helper_get_dirflag)
VG_(helper_get_dirflag):
	pushl	%eax

	movl	VGOFF_(m_dflag), %eax
	movl	(%ebp, %eax, 4), %eax
	movl	%eax, 8(%esp)

	popl	%eax
	ret

/* Clear/set the direction flag. */
.global VG_(helper_CLD)
VG_(helper_CLD):
	pushl	%eax

	movl	VGOFF_(m_dflag), %eax
	movl	$1, (%ebp, %eax, 4)

	popl	%eax
	ret

.global VG_(helper_STD)
VG_(helper_STD):
	pushl	%eax

	movl	VGOFF_(m_dflag), %eax
	movl	$-1, (%ebp, %eax, 4)

	popl	%eax
	ret

/* Clear/set the carry flag. */
.global VG_(helper_CLC)
VG_(helper_CLC):
        clc
        ret

.global VG_(helper_STC)  
VG_(helper_STC):
        stc
        ret

/* Signed 32-to-64 multiply. */
.globl VG_(helper_imul_32_64)
VG_(helper_imul_32_64):
	pushl	%eax
	pushl	%edx
	movl	16(%esp), %eax
	imull	12(%esp)
	movl	%eax, 16(%esp)
	movl	%edx, 12(%esp)
	popl	%edx
	popl	%eax
	ret
	
/* Signed 16-to-32 multiply. */
.globl VG_(helper_imul_16_32)
VG_(helper_imul_16_32):
	pushl	%eax
	pushl	%edx
	movw	16(%esp), %ax
	imulw	12(%esp)
	movw	%ax, 16(%esp)
	movw	%dx, 12(%esp)
	popl	%edx
	popl	%eax
	ret
	
/* Signed 8-to-16 multiply. */
.globl VG_(helper_imul_8_16)
VG_(helper_imul_8_16):
	pushl	%eax
	pushl	%edx
	movb	16(%esp), %al
	imulb	12(%esp)
	movw	%ax, 16(%esp)
	popl	%edx
	popl	%eax
	ret
	

	
	
	
	
/* Unsigned 32-to-64 multiply. */
.globl VG_(helper_mul_32_64)
VG_(helper_mul_32_64):
	pushl	%eax
	pushl	%edx
	movl	16(%esp), %eax
	mull	12(%esp)
	movl	%eax, 16(%esp)
	movl	%edx, 12(%esp)
	popl	%edx
	popl	%eax
	ret
	
/* Unsigned 16-to-32 multiply. */
.globl VG_(helper_mul_16_32)
VG_(helper_mul_16_32):
	pushl	%eax
	pushl	%edx
	movw	16(%esp), %ax
	mulw	12(%esp)
	movw	%ax, 16(%esp)
	movw	%dx, 12(%esp)
	popl	%edx
	popl	%eax
	ret
	
/* Unsigned 8-to-16 multiply. */
.globl VG_(helper_mul_8_16)
VG_(helper_mul_8_16):
	pushl	%eax
	pushl	%edx
	movb	16(%esp), %al
	mulb	12(%esp)
	movw	%ax, 16(%esp)
	popl	%edx
	popl	%eax
	ret
	
	
	
		
/* Unsigned 64-into-32 divide. */
.globl	VG_(helper_div_64_32)
VG_(helper_div_64_32):
	pushl	%eax
	pushl	%edx
	movl	16(%esp),%eax
	movl	12(%esp),%edx
	divl	20(%esp)
	movl	%eax,16(%esp)
	movl	%edx,12(%esp)
	popl	%edx
	popl	%eax
	ret

/* Signed 64-into-32 divide. */
.globl	VG_(helper_idiv_64_32)
VG_(helper_idiv_64_32):
	pushl	%eax
	pushl	%edx
	movl	16(%esp),%eax
	movl	12(%esp),%edx
	idivl	20(%esp)
	movl	%eax,16(%esp)
	movl	%edx,12(%esp)
	popl	%edx
	popl	%eax
	ret

/* Unsigned 32-into-16 divide. */
.globl	VG_(helper_div_32_16)
VG_(helper_div_32_16):
	pushl	%eax
	pushl	%edx
	movw	16(%esp),%ax
	movw	12(%esp),%dx
	divw	20(%esp)
	movw	%ax,16(%esp)
	movw	%dx,12(%esp)
	popl	%edx
	popl	%eax
	ret

/* Signed 32-into-16 divide. */
.globl	VG_(helper_idiv_32_16)
VG_(helper_idiv_32_16):
	pushl	%eax
	pushl	%edx
	movw	16(%esp),%ax
	movw	12(%esp),%dx
	idivw	20(%esp)
	movw	%ax,16(%esp)
	movw	%dx,12(%esp)
	popl	%edx
	popl	%eax
	ret

/* Unsigned 16-into-8 divide. */
.globl	VG_(helper_div_16_8)
VG_(helper_div_16_8):
	pushl	%eax
	movw	12(%esp),%ax
	divb	16(%esp)
	movb	%ah,12(%esp)
	movb	%al,8(%esp)
	popl	%eax
	ret

/* Signed 16-into-8 divide. */
.globl	VG_(helper_idiv_16_8)
VG_(helper_idiv_16_8):
	pushl	%eax
	movw	12(%esp),%ax
	idivb	16(%esp)
	movb	%ah,12(%esp)
	movb	%al,8(%esp)
	popl	%eax
	ret


/* Undefined instruction (generates SIGILL) */
.globl VG_(helper_undefined_instruction)
VG_(helper_undefined_instruction):
1:	ud2
	jmp	1b
			
##--------------------------------------------------------------------##
##--- end                                             vg_helpers.S ---##
##--------------------------------------------------------------------##
