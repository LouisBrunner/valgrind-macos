
##--------------------------------------------------------------------##
##--- Support for doing system calls.                              ---##
##---                                                 vg_syscall.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an extensible x86 protected-mode
  emulator for monitoring program execution on x86-Unixes.

  Copyright (C) 2000-2002 Julian Seward 
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


.globl	VG_(do_syscall)

# NOTE that this routine expects the simulated machines state
# to be in m_state_static.  Therefore it needs to be wrapped by
# code which copies from baseBlock before the call, into
# m_state_static, and back afterwards.
	
VG_(do_syscall):
	# Save all the int registers of the real machines state on the
	# simulators stack.
	pushal

	# and save the real FPU state too
	fwait
	fnsave	VG_(real_fpu_state_saved_over_syscall)
	frstor	VG_(real_fpu_state_saved_over_syscall)

	# remember what the simulators stack pointer is
	movl	%esp, VG_(esp_saved_over_syscall)
	
	# Now copy the simulated machines state into the real one
	# esp still refers to the simulators stack
	frstor	VG_(m_state_static)+64
	movl	VG_(m_state_static)+56, %eax
	pushl	%eax
	popfl
#if 0
	/* don't bother to save/restore seg regs across the kernel iface.  
	   Once we have our hands on them, our simulation of it is 
	   completely internal, and the kernel sees nothing.  
	   What's more, loading new values in to %cs seems 
	   to be impossible anyway. */
	movw	VG_(m_state_static)+0, %cs
	movw	VG_(m_state_static)+4, %ss
	movw	VG_(m_state_static)+8, %ds
	movw	VG_(m_state_static)+12, %es
	movw	VG_(m_state_static)+16, %fs
	movw	VG_(m_state_static)+20, %gs
#endif
	movl	VG_(m_state_static)+24, %eax
	movl	VG_(m_state_static)+28, %ecx
	movl	VG_(m_state_static)+32, %edx
	movl	VG_(m_state_static)+36, %ebx
	movl	VG_(m_state_static)+40, %esp
	movl	VG_(m_state_static)+44, %ebp
	movl	VG_(m_state_static)+48, %esi
	movl	VG_(m_state_static)+52, %edi

	# esp now refers to the simulatees stack
	# Do the actual system call
	int	$0x80

	# restore stack as soon as possible
	# esp refers to simulatees stack
	movl	%esp, VG_(m_state_static)+40
	movl	VG_(esp_saved_over_syscall), %esp
	# esp refers to simulators stack

	# ... and undo everything else.  
	# Copy real state back to simulated state.
#if 0
	movw	%cs, VG_(m_state_static)+0
	movw	%ss, VG_(m_state_static)+4
	movw	%ds, VG_(m_state_static)+8
	movw	%es, VG_(m_state_static)+12
	movw	%fs, VG_(m_state_static)+16
	movw	%gs, VG_(m_state_static)+20
#endif
	movl	%eax, VG_(m_state_static)+24
	movl	%ecx, VG_(m_state_static)+28
	movl	%edx, VG_(m_state_static)+32
	movl	%ebx, VG_(m_state_static)+36
	movl	%ebp, VG_(m_state_static)+44
	movl	%esi, VG_(m_state_static)+48
	movl	%edi, VG_(m_state_static)+52
	pushfl
	popl	%eax
	movl	%eax, VG_(m_state_static)+56
	fwait
	fnsave	VG_(m_state_static)+64
	frstor	VG_(m_state_static)+64

	# Restore the state of the simulator
	frstor	VG_(real_fpu_state_saved_over_syscall)
	popal

	ret

##--------------------------------------------------------------------##
##--- end                                             vg_syscall.S ---##
##--------------------------------------------------------------------##
