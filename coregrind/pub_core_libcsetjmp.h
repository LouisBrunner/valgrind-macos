
/*--------------------------------------------------------------------*/
/*--- A minimal setjmp/longjmp facility.     pub_core_libcsetjmp.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2017 Mozilla Foundation

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Julian Seward <jseward@acm.org> */

#ifndef __PUB_CORE_LIBCSETJMP_H
#define __PUB_CORE_LIBCSETJMP_H

//--------------------------------------------------------------------
// PURPOSE: Provides a minimal setjmp/longjmp facility, that saves/
// restores integer registers, but not necessarily anything more.
//--------------------------------------------------------------------

// No core-only exports; everything in this module is visible to both
// the core and tools.

#include "pub_tool_libcsetjmp.h"

#endif   // __PUB_CORE_LIBCSETJMP_H

/*--------------------------------------------------------------------*/
/*--- end                                    pub_core_libcsetjmp.h ---*/
/*--------------------------------------------------------------------*/
