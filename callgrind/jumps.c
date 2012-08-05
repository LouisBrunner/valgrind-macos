/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                   ct_jumps.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2012, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

#define N_JCC_INITIAL_ENTRIES  4437

/*------------------------------------------------------------*/
/*--- Jump Cost Center (JCC) operations, including Calls   ---*/
/*------------------------------------------------------------*/

#define N_JCC_INITIAL_ENTRIES  4437

jcc_hash current_jccs;

void CLG_(init_jcc_hash)(jcc_hash* jccs)
{
   Int i;

   CLG_ASSERT(jccs != 0);

   jccs->size    = N_JCC_INITIAL_ENTRIES;
   jccs->entries = 0;
   jccs->table = (jCC**) CLG_MALLOC("cl.jumps.ijh.1",
                                    jccs->size * sizeof(jCC*));
   jccs->spontaneous = 0;

   for (i = 0; i < jccs->size; i++)
     jccs->table[i] = 0;
}


void CLG_(copy_current_jcc_hash)(jcc_hash* dst)
{
  CLG_ASSERT(dst != 0);

  dst->size        = current_jccs.size;
  dst->entries     = current_jccs.entries;
  dst->table       = current_jccs.table;
  dst->spontaneous = current_jccs.spontaneous;
}

void CLG_(set_current_jcc_hash)(jcc_hash* h)
{
  CLG_ASSERT(h != 0);

  current_jccs.size        = h->size;
  current_jccs.entries     = h->entries;
  current_jccs.table       = h->table;
  current_jccs.spontaneous = h->spontaneous;
}

__inline__
static UInt jcc_hash_idx(BBCC* from, UInt jmp, BBCC* to, UInt size)
{
  return (UInt) ( (UWord)from + 7* (UWord)to + 13*jmp) % size;
} 

/* double size of jcc table  */
static void resize_jcc_table(void)
{
    Int i, new_size, conflicts1 = 0, conflicts2 = 0;
    jCC** new_table;
    UInt new_idx;
    jCC *curr_jcc, *next_jcc;

    new_size  = 2* current_jccs.size +3;
    new_table = (jCC**) CLG_MALLOC("cl.jumps.rjt.1",
                                   new_size * sizeof(jCC*));
 
    if (!new_table) return;
 
    for (i = 0; i < new_size; i++)
      new_table[i] = NULL;
 
    for (i = 0; i < current_jccs.size; i++) {
	if (current_jccs.table[i] == NULL) continue;
 
	curr_jcc = current_jccs.table[i];
	while (NULL != curr_jcc) {
	    next_jcc = curr_jcc->next_hash;

	    new_idx = jcc_hash_idx(curr_jcc->from, curr_jcc->jmp,
				    curr_jcc->to, new_size);

	    curr_jcc->next_hash = new_table[new_idx];
	    new_table[new_idx] = curr_jcc;
	    if (curr_jcc->next_hash) {
		conflicts1++;
		if (curr_jcc->next_hash->next_hash)
		    conflicts2++;
	    }

	    curr_jcc = next_jcc;
	}
    }

    VG_(free)(current_jccs.table);


    CLG_DEBUG(0, "Resize JCC Hash: %d => %d (entries %d, conflicts %d/%d)\n",
	     current_jccs.size, new_size,
	     current_jccs.entries, conflicts1, conflicts2);

    current_jccs.size  = new_size;
    current_jccs.table = new_table;
    CLG_(stat).jcc_hash_resizes++;
}



/* new jCC structure: a call was done to a BB of a BBCC 
 * for a spontaneous call, from is 0 (i.e. caller unknown)
 */
static jCC* new_jcc(BBCC* from, UInt jmp, BBCC* to)
{
   jCC* jcc;
   UInt new_idx;

   /* check fill degree of jcc hash table and resize if needed (>80%) */
   current_jccs.entries++;
   if (10 * current_jccs.entries / current_jccs.size > 8)
       resize_jcc_table();

   jcc = (jCC*) CLG_MALLOC("cl.jumps.nj.1", sizeof(jCC));

   jcc->from      = from;
   jcc->jmp       = jmp;
   jcc->to        = to;
   jcc->jmpkind   = jk_Call;
   jcc->call_counter = 0;
   jcc->cost = 0;

   /* insert into JCC chain of calling BBCC.
    * This list is only used at dumping time */

   if (from) {
       /* Prohibit corruption by array overrun */
       CLG_ASSERT((0 <= jmp) && (jmp <= from->bb->cjmp_count));
       jcc->next_from = from->jmp[jmp].jcc_list;
       from->jmp[jmp].jcc_list = jcc;
   }
   else {
       jcc->next_from = current_jccs.spontaneous;
       current_jccs.spontaneous = jcc;
   }

   /* insert into JCC hash table */
   new_idx = jcc_hash_idx(from, jmp, to, current_jccs.size);
   jcc->next_hash = current_jccs.table[new_idx];
   current_jccs.table[new_idx] = jcc;

   CLG_(stat).distinct_jccs++;

   CLG_DEBUGIF(3) {
     VG_(printf)("  new_jcc (now %d): %p\n",
		 CLG_(stat).distinct_jccs, jcc);
   }

   return jcc;
}


/* get the jCC for a call arc (BBCC->BBCC) */
jCC* CLG_(get_jcc)(BBCC* from, UInt jmp, BBCC* to)
{
    jCC* jcc;
    UInt idx;

    CLG_DEBUG(5, "+ get_jcc(bbcc %p/%d => bbcc %p)\n",
		from, jmp, to);

    /* first check last recently used JCC */
    jcc = to->lru_to_jcc;
    if (jcc && (jcc->from == from) && (jcc->jmp == jmp)) {
	CLG_ASSERT(to == jcc->to);
	CLG_DEBUG(5,"- get_jcc: [LRU to] jcc %p\n", jcc);
	return jcc;
    }

    jcc = from->lru_from_jcc;
    if (jcc && (jcc->to == to) && (jcc->jmp == jmp)) {
	CLG_ASSERT(from == jcc->from);
	CLG_DEBUG(5, "- get_jcc: [LRU from] jcc %p\n", jcc);
	return jcc;
    }

    CLG_(stat).jcc_lru_misses++;

    idx = jcc_hash_idx(from, jmp, to, current_jccs.size);
    jcc = current_jccs.table[idx];

    while(jcc) {
	if ((jcc->from == from) &&
	    (jcc->jmp == jmp) &&
	    (jcc->to == to)) break;
	jcc = jcc->next_hash;
    }

    if (!jcc)
	jcc = new_jcc(from, jmp, to);

    /* set LRU */
    from->lru_from_jcc = jcc;
    to->lru_to_jcc = jcc;

    CLG_DEBUG(5, "- get_jcc(bbcc %p => bbcc %p)\n",
		from, to);

    return jcc;
}

