
##--------------------------------------------------------------------##
##--- Support for doing system calls.                              ---##
##---                                                 vg_syscall.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an x86 protected-mode emulator 
  designed for debugging and profiling binaries on x86-Unixes.

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
	frstor	VG_(m_state_static)+40
	movl	VG_(m_state_static)+32, %eax
	pushl	%eax
	popfl
	movl	VG_(m_state_static)+0, %eax
	movl	VG_(m_state_static)+4, %ecx
	movl	VG_(m_state_static)+8, %edx
	movl	VG_(m_state_static)+12, %ebx
	movl	VG_(m_state_static)+16, %esp
	movl	VG_(m_state_static)+20, %ebp
	movl	VG_(m_state_static)+24, %esi
	movl	VG_(m_state_static)+28, %edi

	# esp now refers to the simulatees stack
	# Do the actual system call
	int	$0x80

	# restore stack as soon as possible
	# esp refers to simulatees stack
	movl	%esp, VG_(m_state_static)+16
	movl	VG_(esp_saved_over_syscall), %esp
	# esp refers to simulators stack

	# ... and undo everything else.  
	# Copy real state back to simulated state.	
	movl	%eax, VG_(m_state_static)+0
	movl	%ecx, VG_(m_state_static)+4
	movl	%edx, VG_(m_state_static)+8
	movl	%ebx, VG_(m_state_static)+12
	movl	%ebp, VG_(m_state_static)+20
	movl	%esi, VG_(m_state_static)+24
	movl	%edi, VG_(m_state_static)+28
	pushfl
	popl	%eax
	movl	%eax, VG_(m_state_static)+32
	fwait
	fnsave	VG_(m_state_static)+40
	frstor	VG_(m_state_static)+40

	# Restore the state of the simulator
	frstor	VG_(real_fpu_state_saved_over_syscall)
	popal

	ret

##--------------------------------------------------------------------##
##--- end                                             vg_syscall.S ---##
##--------------------------------------------------------------------##
