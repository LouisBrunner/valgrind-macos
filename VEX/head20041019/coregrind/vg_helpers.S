##--------------------------------------------------------------------##
##--- Support routines for the JITter output.                      ---##
##---                                                 vg_helpers.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an extensible x86 protected-mode
  emulator for monitoring program execution on x86-Unixes.

  Copyright (C) 2000-2004 Julian Seward 
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

#include "core_asm.h"

/* ------------------ SIMULATED CPU HELPERS ------------------ */
/* A stubs for a return which we want to catch: a signal return.
   returns and pthread returns.  In the latter case, the thread's
   return value is in %EAX, so we pass this as the first argument
   to the request.  In both cases we use the user request mechanism.
   You need to to read the definition of VALGRIND_MAGIC_SEQUENCE
   in valgrind.h to make sense of this.

   This isn't used in-place.  It is copied into the client address space
   at an arbitary address.  Therefore, this code must be completely
   position-independent.
*/
.global VG_(trampoline_code_start)
.global VG_(trampoline_code_length)
.global VG_(tramp_sigreturn_offset)
.global VG_(tramp_syscall_offset)
	
VG_(trampoline_code_start):
sigreturn_start:	
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
	ud2

	# We can point our sysinfo stuff here
	.align 16
syscall_start:	
	int	$0x80
	ret
tramp_code_end:
			
.data
VG_(trampoline_code_length):
	.long tramp_code_end - VG_(trampoline_code_start)
VG_(tramp_sigreturn_offset):
	.long sigreturn_start - VG_(trampoline_code_start)
VG_(tramp_syscall_offset):
	.long syscall_start - VG_(trampoline_code_start)
.text

	
/* ------------------ REAL CPU HELPERS ------------------ */
/* The rest of this lot run on the real CPU. */
	
/* Various helper routines, for instructions which are just too
   darn tedious for the JITter to output code in-line:
	
	* integer division
	* integer multiplication
        * setting and getting obscure eflags
	* double-length shifts
        * eight byte compare and exchange
	
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

/* 
   Fetch a byte/word/dword from given port
   On entry:
         size     1, 2 or 4
         port, replaced by result
         RA
*/
.global VG_(helper_IN)
VG_(helper_IN):
	pushl	%eax
	pushl   %edx
	movl	16(%esp), %eax
	movl	12(%esp), %edx

	pushfl
	cmpl	$4, %eax
	je	in_dword
	cmpl	$2, %eax
	je	in_word
in_byte:
	inb	(%dx), %al
	jmp	in_done
in_word:
	in	(%dx), %ax
	jmp	in_done
in_dword:
	inl	(%dx),%eax
in_done:
	popfl
	movl	%eax,12(%esp)
	popl	%edx
	popl	%eax
	ret

/* 
   Write a byte/word/dword to given port
   On entry:
         size     1, 2 or 4
         port
         value
         RA
*/
.global VG_(helper_OUT)
VG_(helper_OUT):
	pushl	%eax
	pushl   %edx
	movl	16(%esp), %edx
	movl	12(%esp), %eax

	pushfl
	cmpl	$4, 20(%esp)
	je	out_dword
	cmpl	$2, 20(%esp)
	je	out_word
out_byte:
	outb	%al,(%dx)
	jmp	out_done
out_word:
	out	%ax,(%dx)
	jmp	out_done
out_dword:
	outl	%eax,(%dx)
out_done:
	popfl
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

   We save registers and package up the args so we can call a C helper
   for all this.
*/
.global VG_(helper_CPUID)
VG_(helper_CPUID):
	pushl	%ebp
	movl	%esp,%ebp
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushf
	
	lea	2*4(%ebp),%eax	/* &edx */
	pushl	%eax
	addl	$4,%eax		/* &ecx */
	pushl	%eax
	addl	$4,%eax		/* &ebx */
	pushl	%eax
	addl	$4,%eax		/* &eax */
	pushl	%eax
	pushl	(%eax)		/* eax */

	call	VG_(helperc_CPUID)
	addl	$20,%esp

	popf
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	popl	%ebp
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
	

/* Similarly, do %ax = AAS(%ax). */
.global VG_(helper_AAS)
VG_(helper_AAS):
       pushl   %eax
       movl    8(%esp), %eax
       aas
       movl    %eax, 8(%esp)
       popl    %eax
       ret


/* Similarly, do %ax = AAA(%ax). */
.global VG_(helper_AAA)
VG_(helper_AAA):
       pushl   %eax
       movl    8(%esp), %eax
       aaa
       movl    %eax, 8(%esp)
       popl    %eax
       ret


/* Similarly, do %ax = AAD(%ax). */
.global VG_(helper_AAD)
VG_(helper_AAD):
       pushl   %eax
       movl    8(%esp), %eax
       aad
       movl    %eax, 8(%esp)
       popl    %eax
       ret


/* Similarly, do %ax = AAM(%ax). */
.global VG_(helper_AAM)
VG_(helper_AAM):
       pushl   %eax
       movl    8(%esp), %eax
       aam
       movl    %eax, 8(%esp)
       popl    %eax
       ret
	


/* Eight byte compare and exchange. */
.globl VG_(helper_cmpxchg8b)
VG_(helper_cmpxchg8b):
        pushl %eax
        pushl %ebx
        pushl %ecx
        pushl %edx
        movl 20(%esp), %eax
        movl 24(%esp), %edx
        movl 28(%esp), %ebx
        movl 32(%esp), %ecx
        cmpxchg8b 36(%esp)
        movl %eax, 20(%esp)
        movl %edx, 24(%esp)
        movl %ebx, 28(%esp)
        movl %ecx, 32(%esp)
        popl %edx
        popl %ecx
        popl %ebx
        popl %eax
        ret


/* Undefined instruction (generates SIGILL) */
.globl VG_(helper_undefined_instruction)
VG_(helper_undefined_instruction):
1:	ud2
	jmp	1b
	        
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits
		
##--------------------------------------------------------------------##
##--- end                                             vg_helpers.S ---##
##--------------------------------------------------------------------##
