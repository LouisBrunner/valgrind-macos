/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                    dispatch-nanomips-linux.S ---*/
/*--------------------------------------------------------------------*/

#   This file is part of Valgrind, a dynamic binary instrumentation
#   framework.

#   Copyright (C) 2017-2018 RT-RK

#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of the
#   License, or (at your option) any later version.

#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
#    02111-1307, USA.

#   The GNU General Public License is contained in the file COPYING.

#include "pub_core_basics_asm.h"

#if defined(VGP_nanomips_linux)

#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"   /* for OFFSET_mips_PC */

# Signature:
# void VG_(disp_run_translations)( UWord* two_words,
#                                  void*  guest_state,
#                                  Addr   host_addr);

# The dispatch loop.  VG_(disp_run_translations) is used to run all
# translations, including no-redir ones.

.text
.globl VG_(disp_run_translations)
VG_(disp_run_translations):
# a0 holds two_words
# a1 holds guest_state
# a2 holds host_addr
   save 32, $s0-$s7
   save 32, $fp-$ra
   sw $gp, 20($sp)
   sw $a0, 16($sp)

# Load address of guest_state into guest state register ($s7)
   move $s7, $a1

# And jump into the code cache.  Chained translations in
# the code cache run, until for whatever reason, they can't
# continue.  When that happens, the translation in question
# will jump (or call) to one of the continuation points
# VG_(cp_...) below.

   jrc $a2

# * Postamble and exit:
   postamble:
# At this point, $t4 and $t5 contain two
# words to be returned to the caller.  $t4
# holds a TRC value, and $t5 optionally may
# hold another word (for CHAIN_ME exits, the
# address of the place to patch.)

#  Restore $a0 from stack; holds address of two_words
   lw $a0, 16($sp)
   sw $t4, 0($a0)      # Store $t4 to two_words[0]
   sw $t5, 4($a0)      # Store $t5 to two_words[1]

   lw $gp, 20($sp)
   restore 32, $fp-$ra
   restore 32, $s0-$s7

   jrc $ra

# * Continuation points:

.global VG_(disp_cp_chain_me_to_slowEP)
VG_(disp_cp_chain_me_to_slowEP):
# We got called.  The return address indicates
# where the patching needs to happen. Collect
# the return address and, exit back to C land,
# handing the caller the pair (Chain_me_S, RA) */
   li $t4, VG_TRC_CHAIN_ME_TO_SLOW_EP
#  8 = mkLoadImm32_EXACTLY2
#  4 = jalrc $9
   addiu  $t5, $ra, -12
   bc postamble

.global VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
# We got called. The return address indicates
# where the patching needs to happen. Collect
# the return address and, exit back to C land,
# handing the caller the pair (Chain_me_S, RA) */
   li $t4, VG_TRC_CHAIN_ME_TO_FAST_EP
#  8 = mkLoadImm32_EXACTLY2
#  4 = jalrc $9
   addiu  $t5, $ra, -12
   bc postamble

.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
# /* Where are we going? */
   lw $a7, OFFSET_mips32_PC($s7)
   lw $t1, VG_(stats__n_xIndirs_32)
   addiu $t1, $t1, 1
   sw $t1, VG_(stats__n_xIndirs_32)

# try a fast lookup in the translation cache
# t2 = VG_TT_FAST_HASH(addr) * sizeof(ULong*)
#    = (t2 >> 2 & VG_TT_FAST_MASK)  << 3
   move $t2, $a7
   li $t0, VG_TT_FAST_MASK
   srl $t2, $t2, 2
   and $t2, $t2, $t0
   sll $t2, $t2, 3

# t1 = (addr of VG_(tt_fast)) + t2
   la $t1, VG_(tt_fast)
   addu $t1, $t1, $t2

# t9 = VG_(tt_fast)[hash] :: ULong*
   lw $t0, 0($t1)
   addiu $t1, $t1, 4
   lw $t9, 0($t1)

# little-endian, so comparing 1st 32bit word
   bnec $t0, $a7, fast_lookup_failed
   jrc $t9

   fast_lookup_failed:
#  %PC is up to date */
#  back out decrement of the dispatch counter */
#  hold dispatch_ctr in t0 (r8) */
   lw $t1, VG_(stats__n_xIndirs_32)
   addiu $t1, $t1, 0x1
   sw $t1, VG_(stats__n_xIndirs_32)
   li $t4, VG_TRC_INNER_FASTMISS
   move $t5, $zero
   bc postamble

.global VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
# guest-state-pointer contains the TRC. Put the value into the
# return register
   move $t4, $s7
   move $t5, $zero
   bc postamble

.global VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
   li $t4, VG_TRC_INNER_COUNTERZERO
   move $t5, $zero
   bc postamble

.size VG_(disp_run_translations), . - VG_(disp_run_translations)

#endif

# Let the linker know we don't need an executable stack
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                               dispatch-nanomips-linux.S  ---*/
/*--------------------------------------------------------------------*/
