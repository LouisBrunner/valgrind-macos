
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                                vg_dispatch.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an x86 protected-mode emulator 
  designed for debugging and profiling binaries on x86-Unixes.

  Copyright (C) 2000-2002 Julian Seward 
     jseward@acm.org
     Julian_Seward@muraroa.demon.co.uk

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
	pushal
	# Set up the baseBlock pointer
	movl	$VG_(baseBlock), %ebp

	# fetch m_eip into %eax
	movl	VGOFF_(m_eip), %esi
	movl	(%ebp, %esi, 4), %eax
	
	# fall thru to vg_dispatch
	
.globl VG_(dispatch)
VG_(dispatch):
	# %eax holds destination (original) address
	# To signal any kind of interruption, set vg_dispatch_ctr
	# to 1, and vg_interrupt_reason to the appropriate value
	# before jumping here.

	# %ebp indicates further details of the control transfer
	# requested to the address in %eax.  The idea is that we 
	# want to check all jump targets to see if they are either
	# VG_(signalreturn_bogusRA) or VG_(trap_here), both of which
	# require special treatment.  However, testing all branch
	# targets is expensive, and anyway in most cases JITter knows
	# that a jump cannot be to either of these two.  We therefore
	# adopt the following trick.
	#
	# If ebp == & VG_(baseBlock), which is what it started out as,
	# this is a jump for which the JITter knows no check need be
	# made.
	# 
	# If it is ebp == VG_EBP_DISPATCH_CHECKED, we had better make
	# the check. 
	#
	# If %ebp has any other value, we panic.
	#
	# What the JITter assumes is that VG_(signalreturn_bogusRA) can
	# only be arrived at from an x86 ret insn, and dually that
	# VG_(trap_here) can only be arrived at from an x86 call insn.
	# The net effect is that all call and return targets are checked
	# but straightforward jumps are not.
	#
	# Thinks ... is this safe if the client happens to tailcall
	# VG_(trap_here)  ?  I dont think that can happen -- if it did
	# it would be a problem.
	#
	cmpl	$VG_(baseBlock), %ebp
	jnz	dispatch_checked_maybe

dispatch_unchecked:
	# save the jump address at VG_(baseBlock)[VGOFF_(m_eip)],
	# so that if this block takes a fault, we later know where we were.
	movl	VGOFF_(m_eip), %esi
	movl	%eax, (%ebp, %esi, 4)
	
	# do we require attention?
	# this check has to be after the call/ret transfer checks, because
	# we have to ensure that any control transfer following a syscall
	# return is an ordinary transfer.  By the time we get here, we have
	# established that the next transfer, which might get delayed till
	# after a syscall return, is an ordinary one.
	# All a bit subtle ...
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
	jnz	full_search
	# Found a match.  Set the tte.mru_epoch field (+8)
	# and call the tte.trans_addr field (+4)
	movl	VG_(current_epoch), %ecx
	movl	%ecx, 8(%ebx)
	call	*4(%ebx)
	jmp	VG_(dispatch)
	
full_search:
	#no luck?  try the full table search	
	pushl	%eax
	call	VG_(search_transtab)
	addl	$4, %esp

	# %eax has trans addr or zero
	cmpl	$0, %eax
	jz	need_translation
	# full table search also zeroes the tte.last_use field,
	# so we dont have to do so here.
	call	*%eax
	jmp	VG_(dispatch)

need_translation:
	OYNK(1003)
	movl	$VG_Y_TRANSLATE, VG_(interrupt_reason)
counter_is_zero:
	OYNK(1004)
	popal
	# ----- (the only) exit point from VG_(run_innerloop) -----
	# ----- unless of course vg_oursignalhandler longjmp()s
	# ----- back through it, due to an unmanagable signal
	ret


/* The normal way to get back to the translation loop is to put
   the address of the next (original) address and return.
   However, simulation of a RET insn requires a check as to whether 
   the next address is vg_signalreturn_bogusRA.  If so, a signal 
   handler is returning, so we need to invoke our own mechanism to 
   deal with that, by calling vg_signal_returns().  This restores 
   the simulated machine state from the VgSigContext structure on 
   the stack, including the (simulated, of course) %eip saved when 
   the signal was delivered.  We then arrange to jump to the 
   restored %eip.
*/ 
dispatch_checked_maybe:
	# Possibly a checked dispatch.  Sanity check ...
	cmpl	$VG_EBP_DISPATCH_CHECKED, %ebp
	jz	dispatch_checked
	# ebp has an invalid value ... crap out.
	pushl	$panic_msg_ebp
	call	VG_(panic)
	#	(never returns)

dispatch_checked:
	OYNK(2000)
	# first off, restore %ebp -- since it is currently wrong
	movl	$VG_(baseBlock), %ebp

	# see if we need to mess with stack blocks
	pushl	%ebp
	pushl	%eax
	call	VG_(delete_client_stack_blocks_following_ESP_change)
	popl	%eax
	popl	%ebp
	
	# is this a signal return?
	cmpl	$VG_(signalreturn_bogusRA), %eax
	jz	dispatch_to_signalreturn_bogusRA
	# should we intercept this call?
	cmpl	$VG_(trap_here), %eax
	jz	dispatch_to_trap_here
	# ok, its not interesting.  Handle the normal way.
	jmp	dispatch_unchecked

dispatch_to_signalreturn_bogusRA:
	OYNK(2001)
	pushal
	call	VG_(signal_returns)
	popal
	# %EIP will now point to the insn which should have followed
	# the signal delivery.  Jump to it.  Since we no longer have any
	# hint from the JITter about whether or not it is checkable,
	# go via the conservative route.
	movl	VGOFF_(m_eip), %esi
	movl	(%ebp, %esi, 4), %eax
	jmp	dispatch_checked

	
/* Similarly, check CALL targets to see if it is the ultra-magical
   vg_trap_here(), and, if so, act accordingly.  See vg_clientmalloc.c.
   Be careful not to get the real and simulated CPUs, 
   stacks and regs mixed up ...
*/
dispatch_to_trap_here:
	OYNK(111)
	/* Considering the params to vg_trap_here(), we should have:
	   12(%ESP) is what_to_do
	    8(%ESP) is arg2
	    4(%ESP) is arg1
	    0(%ESP) is return address
	*/
	movl	VGOFF_(m_esp), %esi
	movl	(%ebp, %esi, 4), %ebx
	# %ebx now holds simulated %ESP
	cmpl	$0x4000, 12(%ebx)
	jz	handle_malloc
	cmpl	$0x4001, 12(%ebx)
	jz	handle_malloc
	cmpl	$0x4002, 12(%ebx)
	jz	handle_malloc
	cmpl	$0x5000, 12(%ebx)
	jz	handle_free
	cmpl	$0x5001, 12(%ebx)
	jz	handle_free
	cmpl	$0x5002, 12(%ebx)
	jz	handle_free
	cmpl	$6666, 12(%ebx)
	jz	handle_calloc
	cmpl	$7777, 12(%ebx)
	jz	handle_realloc
	cmpl	$8888, 12(%ebx)
	jz	handle_memalign
	push	$panic_msg_trap
	call	VG_(panic)
	# vg_panic never returns

handle_malloc:
	# %ESP is in %ebx
	pushl     12(%ebx)
	pushl	8(%ebx)
	call	VG_(client_malloc)
	addl	$8, %esp
	# returned value is in %eax
	jmp	save_eax_and_simulate_RET
	
handle_free:
	# %ESP is in %ebx
	pushl	12(%ebx)
	pushl	8(%ebx)
	call	VG_(client_free)
	addl	$8, %esp
	jmp	simulate_RET
	
handle_calloc:
	# %ESP is in %ebx
	pushl	8(%ebx)
	pushl	4(%ebx)
	call	VG_(client_calloc)
	addl	$8, %esp
	# returned value is in %eax
	jmp	save_eax_and_simulate_RET

handle_realloc:
	# %ESP is in %ebx
	pushl	8(%ebx)
	pushl	4(%ebx)
	call	VG_(client_realloc)
	addl	$8, %esp
	# returned value is in %eax
	jmp	save_eax_and_simulate_RET

handle_memalign:
	# %ESP is in %ebx
	pushl	8(%ebx)
	pushl	4(%ebx)
	call	VG_(client_memalign)
	addl	$8, %esp
	# returned value is in %eax
	jmp	save_eax_and_simulate_RET

save_eax_and_simulate_RET:
	movl	VGOFF_(m_eax), %esi
	movl	%eax, (%ebp, %esi, 4)	# %eax -> %EAX
	# set %EAX bits to VALID
	movl	VGOFF_(sh_eax), %esi
	movl	$0x0 /* All 32 bits VALID */, (%ebp, %esi, 4)
	# fall thru ...
simulate_RET:
	# standard return
	movl	VGOFF_(m_esp), %esi
	movl	(%ebp, %esi, 4), %ebx	# %ESP -> %ebx
	movl	0(%ebx), %eax		# RA -> %eax
	addl	$4, %ebx		# %ESP += 4
	movl	%ebx, (%ebp, %esi, 4)	# %ebx -> %ESP
	jmp	dispatch_checked	# jump to %eax

.data
panic_msg_trap:
.ascii	"dispatch_to_trap_here: unknown what_to_do"
.byte	0
panic_msg_ebp:
.ascii	"vg_dispatch: %ebp has invalid value!"
.byte	0
.text	

	
/*------------------------------------------------------------*/
/*--- A helper for delivering signals when the client is   ---*/
/*--- (presumably) blocked in a system call.               ---*/
/*------------------------------------------------------------*/

/* Returns, in %eax, the next orig_addr to run.
   The caller needs to decide whether the returned orig_addr
   requires special handling.
 
   extern Addr VG_(run_singleton_translation) ( Addr trans_addr )
*/
	
/* should we take care to save the FPU state here? */
	
.globl VG_(run_singleton_translation)
VG_(run_singleton_translation):
	movl    4(%esp), %eax		# eax = trans_addr
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp

	# set up ebp correctly for translations
	movl	$VG_(baseBlock), %ebp

	# run the translation
	call	*%eax

	# next orig_addr is correctly in %eax already
	
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	
        ret

##--------------------------------------------------------------------##
##--- end                                            vg_dispatch.S ---##
##--------------------------------------------------------------------##
