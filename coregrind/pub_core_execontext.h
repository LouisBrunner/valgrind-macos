
/*--------------------------------------------------------------------*/
/*--- ExeContexts: long-lived, non-dup'd stack traces.             ---*/
/*---                                        pub_core_execontext.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_EXECONTEXT_H
#define __PUB_CORE_EXECONTEXT_H

//--------------------------------------------------------------------
// PURPOSE: This module provides an abstract data type, ExeContext,
// which is a stack trace stored in such a way that duplicates are
// avoided.  This also facilitates fast comparisons if necessary.
//--------------------------------------------------------------------

#include "pub_tool_execontext.h"

/* The maximum number of calls we're prepared to save in an ExeContext. */
#define VG_DEEPEST_BACKTRACE 500

// Print stats (informational only).
// If with_stacktraces, outputs all the recorded stacktraces.
extern void VG_(print_ExeContext_stats) ( Bool with_stacktraces );

// All ExeContext that are valid in the current epoch and have one or more
// ips in the given range are archived, i.e. their epoch is frozen to
// the given last_epoch.
extern void VG_(archive_ExeContext_in_range) (DiEpoch last_epoch,
                                              Addr text_avma, SizeT length );

// Extract the StackTrace from an ExeContext.
// (Minor hack: we use Addr* as the return type instead of StackTrace so
// that modules #including this file don't also have to #include
// pub_core_stacktrace.h also.)
extern
/*StackTrace*/Addr* VG_(get_ExeContext_StackTrace) ( ExeContext* e );


#endif   // __PUB_CORE_EXECONTEXT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
