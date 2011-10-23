
/*--------------------------------------------------------------------*/
/*--- The dispatcher.                          pub_core_dispatch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Julian Seward
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

#include "pub_core_dispatch_asm.h"

/* This subroutine is called from the C world.  It is passed
   a pointer to the VEX guest state (arch.vex).  It must run code
   from the instruction pointer in the guest state, and exit when
   VG_(dispatch_ctr) reaches zero, or we need to defer to the scheduler.
   The return value must indicate why it returned back to the scheduler.
   It can also be exited if the executing code throws a non-resumable
   signal, for example SIGSEGV, in which case control longjmp()s back past
   here.

   If do_profiling is nonzero, the profile counters arrays should be
   updated for each translation run.

   This code simply handles the common case fast -- when the translation
   address is found in the translation cache.  For anything else, the
   scheduler does the work.

   NOTE, VG_(run_innerloop) MUST NOT BE USED for noredir translations.
   Instead use VG_(run_a_noredir_translation).
*/
extern 
UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
#if defined(VGA_x86) || defined(VGA_amd64)
/* We need to locate a couple of labels inside VG_(run_innerloop), so
   that Vex can add branches to them from generated code.  Hence the
   following somewhat bogus decls.  At least on x86 and amd64.  ppc32
   and ppc64 use straightforward bl-blr to get from dispatcher to
   translation and back and so do not need these labels. */
extern Addr VG_(run_innerloop__dispatch_unassisted_unprofiled);
extern Addr VG_(run_innerloop__dispatch_assisted_unprofiled);
extern Addr VG_(run_innerloop__dispatch_unassisted_profiled);
extern Addr VG_(run_innerloop__dispatch_assisted_profiled);
#endif


/* Run a no-redir translation.  argblock points to 4 UWords, 2 to carry args
   and 2 to carry results:
      0: input:  ptr to translation
      1: input:  ptr to guest state
      2: output: next guest PC
      3: output: guest state pointer afterwards (== thread return code)
   MUST NOT BE USED for non-noredir (normal) translations.
*/
extern void VG_(run_a_noredir_translation) ( volatile UWord* argblock );
#if defined(VGA_x86) || defined(VGA_amd64)
/* We need to a label inside VG_(run_a_noredir_translation), so that
   Vex can add branches to them from generated code.  Hence the
   following somewhat bogus decl. */
extern Addr VG_(run_a_noredir_translation__return_point);
#endif


#endif   // __PUB_CORE_DISPATCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
