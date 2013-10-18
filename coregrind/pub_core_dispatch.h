
/*--------------------------------------------------------------------*/
/*--- The dispatcher.                          pub_core_dispatch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
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
#include "pub_core_basics.h"        // Addr

/* Run translations, with the given guest state, and starting by
   running the host code at 'host_addr'.  It is almost always the case
   that host_addr is the translation for guest_state.guest_IP, that
   is, host_addr is what it would be if we looked up the address of
   the translation corresponding to guest_state.guest_IP.

   The only case where this isn't true is where we're running a
   no-redir translation.  In this case host_addr is the address of the
   alternative (non-redirected) translation for guest_state.guest_IP.

   The return value must indicate why it returned back to the scheduler.
   It can also be exited if the executing code throws a non-resumable
   signal, for example SIGSEGV, in which case control longjmp()s back past
   here.

   two_words holds the return values (two words).  First is
   a TRC value.  Second is generally unused, except in the case
   where we have to return a chain-me request.
*/
void VG_(disp_run_translations)( HWord* two_words,
                                 void*  guest_state, 
                                 Addr   host_addr );

/* We need to know addresses of the continuation-point (cp_) labels so
   we can tell VEX what they are.  They will get baked into the code
   VEX generates.  The type is entirely mythical, but we need to
   state _some_ type, so as to keep gcc happy. */
void VG_(disp_cp_chain_me_to_slowEP)(void);
void VG_(disp_cp_chain_me_to_fastEP)(void);
void VG_(disp_cp_xindir)(void);
void VG_(disp_cp_xassisted)(void);
void VG_(disp_cp_evcheck_fail)(void);

#endif   // __PUB_CORE_DISPATCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
