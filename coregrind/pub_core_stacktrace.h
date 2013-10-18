/*--------------------------------------------------------------------*/
/*--- Stack traces: getting, traversing, printing.                 ---*/
/*---                                        pub_core_stacktrace.h ---*/
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

#ifndef __PUB_CORE_STACKTRACE_H
#define __PUB_CORE_STACKTRACE_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with stack traces:  getting them, 
// traversing them, and printing them.
//--------------------------------------------------------------------

#include "pub_tool_stacktrace.h"
#include "pub_core_basics.h"         // UnwindStartRegs

// Variant that gives a little more control over the stack-walking
// (this is the "worker" function that actually does the walking).
// If you know what the thread ID for this stack is, send that
// as the first parameter, else send zero.  This helps generate
// better stack traces on ppc64-linux and has no effect on other
// platforms.
//
// The acquired IP values are placed in
// ips[0 .. min(n_ips,return_value)].  If sps and fps are non-NULL,
// the corresponding frame-pointer and stack-pointer values for each
// frame are stored there.

UInt VG_(get_StackTrace_wrk) ( ThreadId tid_if_known,
                               /*OUT*/Addr* ips, UInt n_ips,
                               /*OUT*/Addr* sps, /*OUT*/Addr* fps,
                               UnwindStartRegs* startRegs,
                               Addr fp_max_orig );

#endif   // __PUB_CORE_STACKTRACE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
