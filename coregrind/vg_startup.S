
##--------------------------------------------------------------------##
##--- Startup and shutdown code for Valgrind.                      ---##
##---                                                 vg_startup.S ---##
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

#include "vg_constants.h"
#include "config.h"

.section .text
	

	
.global	VG_(switch_to_real_CPU)
VG_(switch_to_real_CPU):
	# Once Valgrind has decided it needs to exit,
	# because the specified number of insns have been completed
	# during a debugging run, it jumps here, which copies the
	# simulators state into the real machine state.  Execution
	# of the rest of the program continues on the real CPU,
	# and there is no way for the simulator to regain control
	# after this point.

	pushfl
	cmpb	$0, VG_(have_ssestate)
	jz	qq4nosse
	andl	$0x0000FFBF, VG_(m_state_static)+64+24
	fxrstor	VG_(m_state_static)+64
	jmp	qq4merge
qq4nosse:
	frstor	VG_(m_state_static)+64
qq4merge:
	popfl
	
	movl	VG_(m_state_static)+56, %eax
	pushl	%eax
	popfl
	/* some of these are apparently illegal */
	/* movw	VG_(m_state_static)+0, %cs */
	movw	VG_(m_state_static)+4, %ss
	movw	VG_(m_state_static)+8, %ds
	movw	VG_(m_state_static)+12, %es
	movw	VG_(m_state_static)+16, %fs
	movw	VG_(m_state_static)+20, %gs
	movl	VG_(m_state_static)+24, %eax
	movl	VG_(m_state_static)+28, %ecx
	movl	VG_(m_state_static)+32, %edx
	movl	VG_(m_state_static)+36, %ebx
	movl	VG_(m_state_static)+40, %esp
	movl	VG_(m_state_static)+44, %ebp
	movl	VG_(m_state_static)+48, %esi
	movl	VG_(m_state_static)+52, %edi

	jmp	*VG_(m_state_static)+60



##--------------------------------------------------------------------##
##--- end                                             vg_startup.S ---##
##--------------------------------------------------------------------##
