##--------------------------------------------------------------------##
##--- Support routines for the JITter output.      amd64/helpers.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

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

#if 0
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


/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits
#endif
		
##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
