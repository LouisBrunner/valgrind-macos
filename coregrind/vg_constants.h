
/*--------------------------------------------------------------------*/
/*--- A header file containing constants (for assembly code).      ---*/
/*---                                               vg_constants.h ---*/
/*--------------------------------------------------------------------*/

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

#ifndef __VG_CONSTANTS_H
#define __VG_CONSTANTS_H

#include "vg_constants_skin.h"

/* This file is included in all Valgrind source files, including
   assembly ones. */

/* Magic values that %ebp might be set to when returning to the
   dispatcher.  The only other legitimate value is to point to the
   start of VG_(baseBlock).  These also are return values from
   VG_(run_innerloop) to the scheduler.

   EBP means %ebp can legitimately have this value when a basic block
   returns to the dispatch loop.  TRC means that this value is a valid
   thread return code, which the dispatch loop may return to the
   scheduler.  */
#define VG_TRC_EBP_JMP_SYSCALL    19 /* EBP and TRC */
#define VG_TRC_EBP_JMP_CLIENTREQ  23 /* EBP and TRC */

#define VG_TRC_INNER_FASTMISS     31 /* TRC only; means fast-cache miss. */
#define VG_TRC_INNER_COUNTERZERO  29 /* TRC only; means bb ctr == 0 */
#define VG_TRC_UNRESUMABLE_SIGNAL 37 /* TRC only; got sigsegv/sigbus */


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


/* Assembly code stubs make this request */
#define VG_USERREQ__SIGNAL_RETURNS          0x4001

#endif /* ndef __VG_CONSTANTS_H */

/*--------------------------------------------------------------------*/
/*--- end                                           vg_constants.h ---*/
/*--------------------------------------------------------------------*/
