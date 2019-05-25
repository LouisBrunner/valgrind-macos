
/*--------------------------------------------------------------------*/
/*--- An ordered set implementation.               pub_core_oset.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2017 Nicholas Nethercote
      njn@valgrind.org

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

#ifndef __PUB_CORE_OSET_H
#define __PUB_CORE_OSET_H

//--------------------------------------------------------------------
// PURPOSE:  A generic data structure with fast (eg. amortised log(n) or
// better) insertion, lookup and deletion of elements.  It does not allow
// duplicates.
//--------------------------------------------------------------------

#include "pub_tool_oset.h"

// No core-only exports;  everything in this module is visible to both
// the core and tools.

#endif   // __PUB_CORE_OSET_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
