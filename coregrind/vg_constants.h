
/*--------------------------------------------------------------------*/
/*--- A header file containing constants (for assembly code).      ---*/
/*---                                               vg_constants.h ---*/
/*--------------------------------------------------------------------*/

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

#ifndef __VG_CONSTANTS_H
#define __VG_CONSTANTS_H


/* This file is included in all Valgrind source files, including
   assembly ones. */

/* All symbols externally visible from valgrind.so are prefixed
   as specified here.  The prefix can be changed, so as to avoid
   namespace conflict problems.
*/
#define VGAPPEND(str1,str2) str1##str2

/* These macros should add different prefixes so the same base
   name can safely be used across different macros. */
#define VG_(str)    VGAPPEND(vgPlain_,str)
#define VGM_(str)   VGAPPEND(vgMem_,str)
#define VGP_(str)   VGAPPEND(vgProf_,str)
#define VGOFF_(str) VGAPPEND(vgOff_,str)


/* Magic values that %ebp might be set to when returning to the
   dispatcher.  The only other legitimate value is to point to the
   start of VG_(baseBlock).  These also are return values from
   VG_(run_innerloop) to the scheduler.

   EBP means %ebp can legitimately have this value when a basic block
   returns to the dispatch loop.  TRC means that this value is a valid
   thread return code, which the dispatch loop may return to the
   scheduler.  */
#define VG_TRC_EBP_JMP_STKADJ     17 /* EBP only; handled by dispatcher */
#define VG_TRC_EBP_JMP_SYSCALL    19 /* EBP and TRC */
#define VG_TRC_EBP_JMP_CLIENTREQ  23 /* EBP and TRC */

#define VG_TRC_INNER_COUNTERZERO  29  /* TRC only; means bb ctr == 0 */
#define VG_TRC_INNER_FASTMISS     31  /* TRC only; means fast-cache miss. */
#define VG_TRC_UNRESUMABLE_SIGNAL 37  /* TRC only; got sigsegv/sigbus */


/* Debugging hack for assembly code ... sigh. */
#if 0
#define OYNK(nnn) pushal;  pushl $nnn; call VG_(oynk) ; addl $4,%esp; popal
#else
#define OYNK(nnn)
#endif

#if 0
#define OYNNK(nnn) pushal;  pushl $nnn; call VG_(oynk) ; addl $4,%esp; popal
#else
#define OYNNK(nnn)
#endif


/* Constants for the fast translation lookup cache. */
#define VG_TT_FAST_BITS 15
#define VG_TT_FAST_SIZE (1 << VG_TT_FAST_BITS)
#define VG_TT_FAST_MASK ((VG_TT_FAST_SIZE) - 1)

/* Constants for the fast original-code-write check cache. */


/* Usually you want this to be zero. */
#define VG_SMC_FASTCHECK_IN_C 0

#define VG_SMC_CACHE_BITS  19
#define VG_SMC_CACHE_SIZE  (1 << VG_SMC_CACHE_BITS)
#define VG_SMC_CACHE_MASK  ((VG_SMC_CACHE_SIZE) - 1)

#define VG_SMC_CACHE_SHIFT 6


/* Assembly code stubs make these requests ... */
#define VG_USERREQ__SIGNAL_RETURNS          0x4001
#define VG_USERREQ__PTHREAD_RETURNS         0x4002
#define VG_USERREQ__SHUTDOWN_VALGRIND       0x4003

#endif /* ndef __VG_INCLUDE_H */

/*--------------------------------------------------------------------*/
/*--- end                                           vg_constants.h ---*/
/*--------------------------------------------------------------------*/
