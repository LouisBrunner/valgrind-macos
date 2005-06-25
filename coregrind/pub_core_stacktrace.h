/*--------------------------------------------------------------------*/
/*--- Stack traces: getting, traversing, printing.                 ---*/
/*---                                        pub_core_stacktrace.h ---*/
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

#ifndef __PUB_CORE_STACKTRACE_H
#define __PUB_CORE_STACKTRACE_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with stack traces:  getting them, 
// traversing them, and printing them.
//--------------------------------------------------------------------

#include "pub_tool_stacktrace.h"

//zz // Variant that gives a little more control over the stack-walking.
//zz extern UInt VG_(get_StackTrace2) ( StackTrace ips, UInt n_ips, 
//zz                                    Addr ip, Addr sp, Addr fp, 
//zz                                    Addr fp_min, Addr fp_max );

#endif   // __PUB_CORE_STACKTRACE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
