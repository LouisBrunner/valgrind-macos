
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                                vg_dispatch.S ---##
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

  The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_constants.h"


/*------------------------------------------------------------*/
/*--- The normal-case dispatch machinery.                  ---*/
/*------------------------------------------------------------*/
	
/* To transfer to an (original) code address, load it into %eax and
   jump to vg_dispatch.  This fragment of code tries to find the
   address of the corresponding translation by searching the translation
   table.   If it fails, a new translation is made, added to the
   translation table, and then jumped to.  Almost all the hard
   work is done by C routines; this code simply handles the
   common case fast -- when the translation address is found in
   the translation cache.

   At entry, %eax is the only live (real-machine) register; the
   entire simulated state is tidily saved in vg_m_state.  
*/

	
/* The C world needs a way to get started simulating.  So we provide
   a function void vg_run_innerloop ( void ), which starts running
   from vg_m_eip, and exits when the counter reaches zero.  This loop
   can also exit if vg_oursignalhandler() catches a non-resumable
   signal, for example SIGSEGV.  It then longjmp()s back past here.
*/
	
.globl VG_(run_innerloop)
VG_(run_innerloop):
	#OYNK(1000)

	# ----- entry point to VG_(run_innerloop) -----
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp

	# Set up the baseBlock pointer
	movl	$VG_(baseBlock), %ebp

	# fetch m_eip into %eax
	movl	VGOFF_(m_eip), %esi
	movl	(%ebp, %esi, 4), %eax
	
	# Start off dispatching paranoically, since we no longer have
	# any indication whether or not this might be a special call/ret
	# transfer.
	jmp	dispatch_stkadj
	
	
dispatch_main:
	# Jump here to do a new dispatch.
	# %eax holds destination (original) address.
	# %ebp indicates further details of the control transfer
	# requested to the address in %eax.
	#
	# If ebp == & VG_(baseBlock), just jump next to %eax.
	# 
	# If ebp == VG_EBP_JMP_SYSCALL, do a system call before 
	# continuing at eax.
	#
	# If ebp == VG_EBP_JMP_CLIENTREQ, do a client request before 
	# continuing at eax.
	#
	# If %ebp has any other value, we panic.
	
	cmpl	$VG_(baseBlock), %ebp
	jnz	dispatch_exceptional

dispatch_boring:
	# save the jump address at VG_(baseBlock)[VGOFF_(m_eip)],
	movl	VGOFF_(m_eip), %esi
	movl	%eax, (%ebp, %esi, 4)
	
	# do a timeslice check.
	# are we out of timeslice?  If yes, defer to scheduler.
	#OYNK(1001)
	decl	VG_(dispatch_ctr)
	jz	counter_is_zero

	#OYNK(1002)
	# try a fast lookup in the translation cache
	movl	%eax, %ebx
	andl	$VG_TT_FAST_MASK, %ebx	
	# ebx = tt_fast index
	movl	VG_(tt_fast)(,%ebx,4), %ebx	
	# ebx points at a tt entry
	# now compare target with the tte.orig_addr field (+0)
	cmpl	%eax, (%ebx)
	jnz	fast_lookup_failed

	# Found a match.  Set the tte.mru_epoch field (+8)
	# and call the tte.trans_addr field (+4)
	movl	VG_(current_epoch), %ecx
	movl	%ecx, 8(%ebx)
	call	*4(%ebx)
	jmp	dispatch_main
	
fast_lookup_failed:
	# %EIP is up to date here since dispatch_boring dominates
	movl	$VG_TRC_INNER_FASTMISS, %eax
	jmp	run_innerloop_exit

counter_is_zero:
	# %EIP is up to date here since dispatch_boring dominates
	movl	$VG_TRC_INNER_COUNTERZERO, %eax
	jmp	run_innerloop_exit
	
run_innerloop_exit:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	ret	



/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	# this is jumped to only, not fallen-through from above
	cmpl	$VG_TRC_EBP_JMP_STKADJ, %ebp
	jz	dispatch_stkadj
	cmpl	$VG_TRC_EBP_JMP_SYSCALL, %ebp
	jz	dispatch_syscall
	cmpl	$VG_TRC_EBP_JMP_CLIENTREQ, %ebp
	jz	dispatch_clientreq

	# ebp has an invalid value ... crap out.
	pushl	$panic_msg_ebp
	call	VG_(panic)
	#	(never returns)

dispatch_syscall:
	# save %eax in %EIP and defer to sched
	movl	$VG_(baseBlock), %ebp
	movl	VGOFF_(m_eip), %esi
	movl	%eax, (%ebp, %esi, 4)
	movl	$VG_TRC_EBP_JMP_SYSCALL, %eax
	jmp	run_innerloop_exit
	
dispatch_clientreq:
	# save %eax in %EIP and defer to sched
	movl	$VG_(baseBlock), %ebp
	movl	VGOFF_(m_eip), %esi
	movl	%eax, (%ebp, %esi, 4)
	movl	$VG_TRC_EBP_JMP_CLIENTREQ, %eax
	jmp	run_innerloop_exit

dispatch_stkadj:
	# save %eax in %EIP
	movl	$VG_(baseBlock), %ebp
	movl	VGOFF_(m_eip), %esi
	movl	%eax, (%ebp, %esi, 4)

	# see if we need to mess with stack blocks
	pushl	%eax
	call	VG_(delete_client_stack_blocks_following_ESP_change)
	popl	%eax
	movl	$VG_(baseBlock), %ebp
		
	# ok, its not interesting.  Handle the normal way.
	jmp	dispatch_boring


.data
panic_msg_ebp:
.ascii	"vg_dispatch: %ebp has invalid value!"
.byte	0
.text	


##--------------------------------------------------------------------##
##--- end                                            vg_dispatch.S ---##
##--------------------------------------------------------------------##
