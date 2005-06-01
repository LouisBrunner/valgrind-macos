
/*--------------------------------------------------------------------*/
/*--- The built-in profiler.                    pub_core_profile.h ---*/
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

#ifndef __PUB_CORE_PROFILE_H
#define __PUB_CORE_PROFILE_H

//--------------------------------------------------------------------
// PURPOSE: This module implements Valgrind's internal tick-and-stack-based
// profiler.  To use it, define VG_DO_PROFILING and use --profile=yes.
// Unfortunately, it's currently broken (and has been for some time)
// because it doesn't interact well with signal handling.
//--------------------------------------------------------------------

#include "pub_tool_profile.h"

extern void VG_(init_profiling) ( void );
extern void VG_(done_profiling) ( void );

#endif   // __PUB_CORE_PROFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
