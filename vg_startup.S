
##--------------------------------------------------------------------##
##--- Startup and shutdown code for Valgrind.                      ---##
##---                                                 vg_startup.S ---##
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


#---------------------------------------------------------------------
#
# Startup and shutdown code for Valgrind.  Particularly hairy.
#
# The dynamic linker, ld.so, will run the contents of the .init
# section, once it has located, mmap-d and and linked the shared
# libraries needed by the program.  Valgrind is itself a shared
# library.  ld.so then runs code in the .init sections of each
# library in turn, in order to give them a chance to initialise
# themselves.  We hijack this mechanism.  Our startup routine
# does return -- and execution continues -- except on the
# synthetic CPU, not the real one.  But ld.so, and the program
# it is starting, cant tell the difference.
#
# The management apologise for the lack of apostrophes in these
# comments.  GNU as seems to object to them, for some reason.


.section .init
	call VG_(startup)
.section .fini
	call VG_(shutdown)
.section .text
	


VG_(startup):
        # Record %esp as it was when we got here.  This is because argv/c
	# and envp[] are passed as args to this function, and we need to see
	# envp so we can get at the env var VG_ARGS without help from libc.
	# The stack layout at this point depends on the version of glibc in
	# use.  See process_cmd_line_options() in vg_main.c for details.
        movl    %esp, VG_(esp_at_startup)
        
	# We have control!  Save the state of the machine in
	# the simulators state, and switch stacks.
	# Except ... we cant copy the machines registers into their
	# final places in vg_baseBlock, because the offsets to them
	# have not yet been set up.  Instead, they are copied to a
	# temporary place (m_state_static).  In vg_main.c, once the
	# baseBlock offsets are set up, values are copied into baseBlock.
	movl	%eax, VG_(m_state_static)+0
	movl	%ecx, VG_(m_state_static)+4
	movl	%edx, VG_(m_state_static)+8
	movl	%ebx, VG_(m_state_static)+12
	movl	%esp, VG_(m_state_static)+16
	movl	%ebp, VG_(m_state_static)+20
	movl	%esi, VG_(m_state_static)+24
	movl	%edi, VG_(m_state_static)+28
	pushfl
	popl	%eax
	movl	%eax, VG_(m_state_static)+32
	fwait
	fnsave	VG_(m_state_static)+40
	frstor	VG_(m_state_static)+40

	# keep the first and last 10 words free to check for overruns	
	movl	$VG_(stack)+39996 -40, %esp

	# Now some real magic.  We need this procedure to return,
	# since thats what ld.so expects, but running on the
	# simulator.  So vg_main starts the simulator running at
	# the insn labelled first_insn_to_simulate.

	movl	$first_insn_to_simulate, VG_(m_state_static)+36
	jmp	VG_(main)
first_insn_to_simulate:
	# Nothing else to do -- just return in the "normal" way.
	ret



.global VG_(shutdown)	
VG_(shutdown):
	# ld.so will call here after execution of the program proper
	# is complete, to allow libraries to close down cleanly.
	# Note that we will enter here on the synthetic CPU, not
	# the real one!  So the interpreter must notice when this
	# procedure is called, and use that as its cue to switch
	# back to the real CPU.  That means the code placed here is
	# utterly irrelevant, since it will never get run, but I
	# place a RET here anyway, since it is the traditional way
	# to return from a subroutine :-)
	ret



.global	VG_(switch_to_real_CPU)
VG_(switch_to_real_CPU):
	# Once Valgrind has decided it needs to exit, either
	# because it has detected a call to vg_shutdown, or
	# because the specified number of insns have been completed
	# during a debugging run, it jumps here, which copies the
	# simulators state into the real machine state.  Execution
	# of the rest of the program continues on the real CPU,
	# and there is no way for the simulator to regain control
	# after this point.
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

	pushal
	pushfl
	# We hope that vg_sigshutdown_actions does not alter
	# the FPU state.
	call	 VG_(sigshutdown_actions)
	popfl
	popal
	# re-restore the FPU state anyway ...
	frstor	VG_(m_state_static)+40	
	jmp	*VG_(m_state_static)+36



/*------------------------------------------------------------*/
/*--- A function to temporarily copy %ESP/%EBP into        ---*/
/*--- %esp/%ebp and then start up GDB.                     ---*/
/*------------------------------------------------------------*/

/*--- This is clearly not re-entrant! ---*/
.data
vg_ebp_saved_over_GDB_start:
	.word	0
vg_esp_saved_over_GDB_start:
	.word	0
.text
	
.global VG_(swizzle_esp_then_start_GDB)	
VG_(swizzle_esp_then_start_GDB):
	pushal

	# remember the simulators current stack/frame pointers
	movl	%ebp, vg_ebp_saved_over_GDB_start
	movl	%esp, vg_esp_saved_over_GDB_start
	
	movl	$VG_(baseBlock), %ebx

	# fetch %ESP into %esp
	movl	VGOFF_(m_esp), %esi
	movl	(%ebx, %esi, 4), %esp

	### %esp now refers to clients stack
	### mess with the clients stack to make it look as if it
	### called this procedure, since otherwise it will look to gdb
	### as if the top (currently executing) stack frame of the
	### client is missing.
	
	# push %EIP, via %eax.  This is a faked-up return address.
	movl	VGOFF_(m_eip), %esi
	movl	(%ebx, %esi, 4), %eax
	pushl	%eax

	# push %EBP, via %eax.  This is a faked %ebp-chain pointer.
	movl	VGOFF_(m_ebp), %esi
	movl	(%ebx, %esi, 4), %eax
	pushl	%eax

	movl	%esp, %ebp
	
	call	VG_(start_GDB_whilst_on_client_stack)

	# restore the simulators stack/frame pointer
	movl	vg_ebp_saved_over_GDB_start, %ebp
	movl	vg_esp_saved_over_GDB_start, %esp
	
	popal
	ret

# gcc puts this construction at the end of every function.  I think it
# allows the linker to figure out the size of the function.  So we do
# the same, in the vague hope that it might help GDBs navigation.
.Lend_of_swizzle:
	.size	VG_(swizzle_esp_then_start_GDB), .Lend_of_swizzle-VG_(swizzle_esp_then_start_GDB)

##--------------------------------------------------------------------##
##--- end                                             vg_startup.S ---##
##--------------------------------------------------------------------##
