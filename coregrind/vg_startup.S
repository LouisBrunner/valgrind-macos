
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



/*------------------------------------------------------------*/
/*--- A function to temporarily copy %ESP/%EBP into        ---*/
/*--- %esp/%ebp and then start up GDB.                     ---*/
/*------------------------------------------------------------*/

/*
extern void VG_(swizzle_esp_then_start_GDB) ( Addr m_eip_at_error,
                                              Addr m_esp_at_error,
                                              Addr m_ebp_at_error );
*/

/*--- This is clearly not re-entrant! ---*/
.data
vg_ebp_saved_over_GDB_start:
	.long	0
vg_esp_saved_over_GDB_start:
	.long	0
.text
	
.type VG_(swizzle_esp_then_start_GDB),@function
.global VG_(swizzle_esp_then_start_GDB)	
VG_(swizzle_esp_then_start_GDB):
#ifdef HAVE_GAS_CFI
	.cfi_startproc
#endif
	pushal

	# remember the simulators current stack/frame pointers
	movl	%ebp, vg_ebp_saved_over_GDB_start
	movl	%esp, vg_esp_saved_over_GDB_start

	# get args into regs
	movl	44(%esp), %eax		# client %EBP
	movl	40(%esp), %ebx		# client %ESP
	movl	36(%esp), %ecx		# client %EIP

	# Now that we dont need to refer to simulators stack any more,
	# put %ESP into %esp
	movl	%ebx, %esp

	### %esp now refers to clients stack
	### mess with the clients stack to make it look as if it
	### called this procedure, since otherwise it will look to gdb
	### as if the top (currently executing) stack frame of the
	### client is missing.
	
	# push %EIP.  This is a faked-up return address.
	pushl	%ecx

	# push %EBP.  This is a faked %ebp-chain pointer.
	pushl	%eax
#ifdef HAVE_GAS_CFI
	.cfi_adjust_cfa_offset 0x4
#endif

	movl	%esp, %ebp
#ifdef HAVE_GAS_CFI
	.cfi_def_cfa_register ebp
#endif
	
	call	VG_(start_GDB_whilst_on_client_stack)

	# restore the simulators stack/frame pointer
	movl	vg_ebp_saved_over_GDB_start, %ebp
	movl	vg_esp_saved_over_GDB_start, %esp
#ifdef HAVE_GAS_CFI
	.cfi_adjust_cfa_offset -0x4
#endif
	
	popal
	ret
#ifdef HAVE_GAS_CFI
	.cfi_endproc
#endif

# gcc puts this construction at the end of every function.  I think it
# allows the linker to figure out the size of the function.  So we do
# the same, in the vague hope that it might help GDBs navigation.
.Lend_of_swizzle:
	.size	VG_(swizzle_esp_then_start_GDB), .Lend_of_swizzle-VG_(swizzle_esp_then_start_GDB)


##--------------------------------------------------------------------##
##--- end                                             vg_startup.S ---##
##--------------------------------------------------------------------##
