
/*--------------------------------------------------------------------*/
/*--- A mapping where the keys exactly cover the address space.    ---*/
/*---                                          pub_core_rangemap.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2017 Mozilla Foundation

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

/* Contributed by Julian Seward <jseward@acm.org> */

#ifndef __PUB_CORE_RANGEMAP_H
#define __PUB_CORE_RANGEMAP_H

//--------------------------------------------------------------------
// PURPOSE: a mapping from the host machine word (UWord) ranges to
// arbitrary other UWord values.  The set of ranges exactly covers all
// possible UWord values.
// --------------------------------------------------------------------

// No core-only exports; everything in this module is visible to both
// the core and tools.

#include "pub_tool_rangemap.h"

#endif   // __PUB_CORE_RANGEMAP_H

/*--------------------------------------------------------------------*/
/*--- end                                      pub_core_rangemap.h ---*/
/*--------------------------------------------------------------------*/
