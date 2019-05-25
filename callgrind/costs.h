/*--------------------------------------------------------------------*/
/*--- Callgrind cost array interface.                      costs.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 Josef Weidendorfer
      josef.weidendorfer@gmx.de

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


#ifndef CLG_COSTS
#define CLG_COSTS

#include "pub_tool_basics.h"

#define CLG_(str) VGAPPEND(vgCallgrind_,str)

extern UInt CLG_(costarray_entries);
extern UInt CLG_(costarray_chunks);

/* Array of 64bit costs. This is separated from other structs
 * to support a dynamic number of costs for a cost item.
 * Chunks are allocated on demand.
 */
typedef struct _CostChunk CostChunk;
struct _CostChunk {
  Int size;
  Int used;
  CostChunk *next, *prev;
  ULong data[0];
};

/* Allocate a number of 64bit cost values.
 * Typically used from ct_events.c */
ULong* CLG_(get_costarray)(Int size);

#endif /* CLG_COSTS */
