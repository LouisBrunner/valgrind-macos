/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                   ct_costs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2013, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

#include "global.h"

#include "pub_tool_mallocfree.h"

#define COSTCHUNK_SIZE 100000

UInt CLG_(costarray_entries) = 0;
UInt CLG_(costarray_chunks) = 0;
static CostChunk* cost_chunk_base = 0;
static CostChunk* cost_chunk_current = 0;

ULong* CLG_(get_costarray)(Int size)
{
  ULong* ptr;

  if (!cost_chunk_current ||
      (cost_chunk_current->size - cost_chunk_current->used < size)) {
    CostChunk* cc  = (CostChunk*) CLG_MALLOC("cl.costs.gc.1",
                                              sizeof(CostChunk) +
					      COSTCHUNK_SIZE * sizeof(ULong));
    cc->size = COSTCHUNK_SIZE;
    cc->used = 0;
    cc->next = 0;

    if (cost_chunk_current)
      cost_chunk_current->next = cc;
    cost_chunk_current = cc;

    if (!cost_chunk_base) cost_chunk_base = cc;

    CLG_(costarray_chunks)++;
  }
  
  ptr = &(cost_chunk_current->data[cost_chunk_current->used]);
  cost_chunk_current->used += size;

  CLG_(costarray_entries) += size;

  return ptr;
}

void CLG_(free_costarrays)()
{
  CostChunk* cc = cost_chunk_base, *cc_next;
  while(cc) {
    cc_next = cc->next;
    VG_(free)(cc);
    cc = cc_next;
  }
  cost_chunk_base = 0;
  cost_chunk_current = 0;
}
