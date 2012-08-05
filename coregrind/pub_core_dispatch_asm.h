
/*--------------------------------------------------------------------*/
/*--- Asm-only dispatcher stuff.           pub_core_dispatch_asm.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward
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

#ifndef __PUB_CORE_DISPATCH_ASM_H
#define __PUB_CORE_DISPATCH_ASM_H

/* Magic values that the guest state might be set to when returning to the
   dispatcher.  The only other legitimate value is to point to the
   start of the thread's VEX guest state.  These also are return values from
   from VG_(run_innerloop) to the scheduler.
*/
/* Defines values for JMP_EMWARN, JMP_SYSCALL, JMP_CLIENTREQ and
   JMP_YIELD */
#include "libvex_trc_values.h"

/* And some more of our own.  These must not have the same values as
   those from libvex_trc_values.h.  (viz, 60 or below is safe).

   (The following comment is no longer relevant, but is retained
   for historical purposes.)
   These values *must* be odd (have bit 0 set) because the dispatchers
   (coregrind/m_dispatch/dispatch-*-*.S) use this fact to distinguish
   a TRC value from the unchanged baseblock pointer -- which has 0 as
   its lowest bit.
*/
#define VG_TRC_BORING              29 /* no event; just keep going */
#define VG_TRC_INNER_FASTMISS      37 /* TRC only; means fast-cache miss. */
#define VG_TRC_INNER_COUNTERZERO   41 /* TRC only; means bb ctr == 0 */
#define VG_TRC_FAULT_SIGNAL        43 /* TRC only; got sigsegv/sigbus */
#define VG_TRC_INVARIANT_FAILED    47 /* TRC only; invariant violation */
#define VG_TRC_CHAIN_ME_TO_SLOW_EP 49 /* TRC only; chain to slow EP */
#define VG_TRC_CHAIN_ME_TO_FAST_EP 51 /* TRC only; chain to fast EP */

#endif   // __PUB_CORE_DISPATCH_ASM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
