
/*--------------------------------------------------------------------*/
/*--- The dispatcher.                          pub_core_dispatch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __PUB_CORE_DISPATCH_H
#define __PUB_CORE_DISPATCH_H

//--------------------------------------------------------------------
// PURPOSE: This module contains the inner loop of the execution
// mechanism, which is: find next basic block, execute it, repeat until
// the next bb isn't found in the fast-cache; or if the current bb
// exited with a request for some special action before continuing; or
// if the current thread has used up its scheduling quantum.
//--------------------------------------------------------------------

/* This subroutine is called from the C world.  It is passed
   a pointer to the VEX guest state (arch.vex).  It must run code
   from the instruction pointer in the guest state, and exit when
   VG_(dispatch_ctr) reaches zero, or we need to defer to the scheduler.
   The return value must indicate why it returned back to the scheduler.
   It can also be exited if the executing code throws a non-resumable
   signal, for example SIGSEGV, in which case control longjmp()s back past
   here.

   This code simply handles the common case fast -- when the translation
   address is found in the translation cache.  For anything else, the
   scheduler does the work.
*/
extern UWord VG_(run_innerloop) ( void* guest_state );

/* Magic values that the guest state might be set to when returning to the
   dispatcher.  The only other legitimate value is to point to the
   start of the thread's VEX guest state.  These also are return values from
   from VG_(run_innerloop) to the scheduler.
*/
/* Defines values for JMP_EMWARN, JMP_SYSCALL, JMP_CLIENTREQ and
   JMP_YIELD */
#include "libvex_trc_values.h"

/* And some more of our own.  These must not have the same values as
   those from libvex_trc_values.h. */
#define VG_TRC_INNER_FASTMISS     37 /* TRC only; means fast-cache miss. */
#define VG_TRC_INNER_COUNTERZERO  41 /* TRC only; means bb ctr == 0 */
#define VG_TRC_FAULT_SIGNAL       43 /* TRC only; got sigsegv/sigbus */
#define VG_TRC_INVARIANT_FAILED   47 /* TRC only; invariant violation */

#endif   // __PUB_CORE_DISPATCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
