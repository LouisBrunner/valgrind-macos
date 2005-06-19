/*--------------------------------------------------------------------*/
/*--- Stack traces: getting, traversing, printing.                 ---*/
/*---                                            tool_stacktrace.h ---*/
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

#ifndef __PUB_TOOL_STACKTRACE_H
#define __PUB_TOOL_STACKTRACE_H

// The basic stack trace type:  just an array of code addresses.
typedef Addr* StackTrace;

// Walks the stack to get instruction pointers from the top stack frames for
// thread 'tid'.  Maximum of 'n_ips' addresses put into 'ips';  0 is the top
// of the stack, 1 is its caller, etc.
extern UInt VG_(get_StackTrace) ( ThreadId tid, StackTrace ips, UInt n_ips );

// Apply a function to every element in the StackTrace.  The parameter 'n'
// gives the index of the passed ip.  Doesn't go below main() unless
// --show-below-main=yes is set.
extern void VG_(apply_StackTrace)( void(*action)(UInt n, Addr ip),
                                   StackTrace ips, UInt n_ips );

// Print a StackTrace.
extern void VG_(pp_StackTrace) ( StackTrace ips, UInt n_ips );

// Gets and immediately prints a StackTrace.  Just a bit simpler than
// calling VG_(get_StackTrace)() then VG_(pp_StackTrace)().
extern void VG_(get_and_pp_StackTrace) ( ThreadId tid, UInt n_ips );

#endif   // __PUB_TOOL_STACKTRACE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
