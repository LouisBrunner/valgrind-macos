/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                         bb.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2007, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

/*------------------------------------------------------------*/
/*--- Basic block (BB) operations                          ---*/
/*------------------------------------------------------------*/

/* BB hash, resizable */
bb_hash bbs;

void CLG_(init_bb_hash)()
{
   Int i;

   bbs.size    = 8437;
   bbs.entries = 0;
   bbs.table = (BB**) CLG_MALLOC(bbs.size * sizeof(BB*));

   for (i = 0; i < bbs.size; i++) bbs.table[i] = NULL;
}

bb_hash* CLG_(get_bb_hash)()
{
  return &bbs;
}

/* The hash stores BBs according to
 * - ELF object (is 0 for code in anonymous mapping)
 * - BB base as object file offset
 */
static __inline__
UInt bb_hash_idx(obj_node* obj, OffT offset, UInt size)
{
  return (((Addr)obj) + offset) % size;
}

/* double size of bb table  */
static
void resize_bb_table(void)
{
    Int i, new_size, conflicts1 = 0, conflicts2 = 0;
    BB **new_table, *curr, *next;
    UInt new_idx;

    new_size  = 2* bbs.size +3;
    new_table = (BB**) CLG_MALLOC(new_size * sizeof(BB*));
 
    if (!new_table) return;
 
    for (i = 0; i < new_size; i++)
      new_table[i] = NULL;
 
    for (i = 0; i < bbs.size; i++) {
	if (bbs.table[i] == NULL) continue;
 
	curr = bbs.table[i];
	while (NULL != curr) {
	    next = curr->next;

	    new_idx = bb_hash_idx(curr->obj, curr->offset, new_size);

	    curr->next = new_table[new_idx];
	    new_table[new_idx] = curr;
	    if (curr->next) {
		conflicts1++;
		if (curr->next->next)
		    conflicts2++;
	    }

	    curr = next;
	}
    }

    VG_(free)(bbs.table);


    CLG_DEBUG(0, "Resize BB Hash: %d => %d (entries %d, conflicts %d/%d)\n",
	     bbs.size, new_size,
	     bbs.entries, conflicts1, conflicts2);

    bbs.size  = new_size;
    bbs.table = new_table;
    CLG_(stat).bb_hash_resizes++;
}


/**
 * Allocate new BB structure (including space for event type list)
 * Not initialized:
 * - instr_len, cost_count, instr[]
 */
static BB* new_bb(obj_node* obj, OffT offset,
		  UInt instr_count, UInt cjmp_count, Bool cjmp_inverted)
{
   BB* new;
   UInt new_idx, size;

   /* check fill degree of bb hash table and resize if needed (>80%) */
   bbs.entries++;
   if (10 * bbs.entries / bbs.size > 8)
       resize_bb_table();

   size = sizeof(BB) + instr_count * sizeof(InstrInfo)
                     + (cjmp_count+1) * sizeof(CJmpInfo);
   new = (BB*) CLG_MALLOC(size);
   VG_(memset)(new, 0, size);

   new->obj        = obj;
   new->offset     = offset;
   
   new->instr_count = instr_count;
   new->cjmp_count  = cjmp_count;
   new->cjmp_inverted = cjmp_inverted;
   new->jmp         = (CJmpInfo*) &(new->instr[instr_count]);
   new->instr_len   = 0;
   new->cost_count  = 0;
   new->sect_kind   = VG_(seginfo_sect_kind)(offset + obj->offset);
   new->fn          = 0;
   new->line        = 0;
   new->is_entry    = 0;
   new->bbcc_list   = 0;
   new->last_bbcc   = 0;

   /* insert into BB hash table */
   new_idx = bb_hash_idx(obj, offset, bbs.size);
   new->next = bbs.table[new_idx];
   bbs.table[new_idx] = new;

   CLG_(stat).distinct_bbs++;

#if CLG_ENABLE_DEBUG
   CLG_DEBUGIF(3) {
     VG_(printf)("  new_bb (instr %d, jmps %d, inv %s) [now %d]: ",
		 instr_count, cjmp_count,
		 cjmp_inverted ? "yes":"no",
		 CLG_(stat).distinct_bbs);
      CLG_(print_bb)(0, new);
      VG_(printf)("\n");
   }
#endif

   CLG_(get_fn_node)(new);

   return new;
}


/* get the BB structure for a BB start address */
static __inline__
BB* lookup_bb(obj_node* obj, OffT offset)
{
    BB* bb;
    Int idx;

    idx = bb_hash_idx(obj, offset, bbs.size);
    bb = bbs.table[idx];

    while(bb) {
      if ((bb->obj == obj) && (bb->offset == offset)) break;
      bb = bb->next;
    }

    CLG_DEBUG(5, "  lookup_bb (Obj %s, off %p): %p\n",
	     obj->name, offset, bb);
    return bb;
}

static __inline__
obj_node* obj_of_address(Addr addr)
{
  obj_node* obj;
  SegInfo* si;
  OffT offset;

  si = VG_(find_seginfo)(addr);
  obj = CLG_(get_obj_node)( si );

  /* Update symbol offset in object if remapped */
  offset = si ? VG_(seginfo_sym_offset)(si):0;
  if (obj->offset != offset) {
      Addr start = si ? VG_(seginfo_start)(si) : 0;

      CLG_DEBUG(0, "Mapping changed for '%s': %p -> %p\n",
		obj->name, obj->start, start);

      /* Size should be the same, and offset diff == start diff */
      CLG_ASSERT( obj->size == (si ? VG_(seginfo_size)(si) : 0) );
      CLG_ASSERT( obj->start - start == obj->offset - offset );
      obj->offset = offset;
      obj->start = start;
  }

  return obj;
}

/* Get the BB structure for a BB start address.
 * If the BB has to be created, the IRBB is needed to
 * compute the event type list for costs, and seen_before is
 * set to False. Otherwise, seen_before is set to True.
 *
 * BBs are never discarded. There are 2 cases where this function
 * is called from CLG_(instrument)() and a BB already exists:
 * - The instrumented version was removed from Valgrinds TT cache
 * - The ELF object of the BB was unmapped and mapped again.
 *   This involves a possibly different address, but is handled by
 *   looking up a BB keyed by (obj_node, file offset).
 *
 * bbIn==0 is possible for artifical BB without real code.
 * Such a BB is created when returning to an unknown function.
 */
BB* CLG_(get_bb)(Addr addr, IRSB* bbIn, /*OUT*/ Bool *seen_before)
{
  BB*   bb;
  obj_node* obj;
  UInt n_instrs, n_jmps;
  Bool cjmp_inverted = False;

  CLG_DEBUG(5, "+ get_bb(BB %p)\n", addr);

  obj = obj_of_address(addr);
  bb = lookup_bb(obj, addr - obj->offset);

  n_instrs = 0;
  n_jmps = 0;
  CLG_(collectBlockInfo)(bbIn, &n_instrs, &n_jmps, &cjmp_inverted);

  *seen_before = bb ? True : False;
  if (*seen_before) {
    if (bb->instr_count != n_instrs) {
      VG_(message)(Vg_DebugMsg, 
		   "ERROR: BB Retranslation Mismatch at BB %p", addr);
      VG_(message)(Vg_DebugMsg,
		   "  new: Obj %s, Off %p, BBOff %p, Instrs %u",
		   obj->name, obj->offset,
		   addr - obj->offset, n_instrs);
      VG_(message)(Vg_DebugMsg,
		   "  old: Obj %s, Off %p, BBOff %p, Instrs %u",
		   bb->obj->name, bb->obj->offset,
		   bb->offset, bb->instr_count);
      CLG_ASSERT(bb->instr_count == n_instrs );
    }
    CLG_ASSERT(bb->cjmp_count == n_jmps );
    CLG_(stat).bb_retranslations++;

    CLG_DEBUG(5, "- get_bb(BB %p): seen before.\n", addr);
    return bb;
  }

  bb = new_bb(obj, addr - obj->offset, n_instrs, n_jmps, cjmp_inverted);

  CLG_DEBUG(5, "- get_bb(BB %p)\n", addr);

  return bb;
}

/* Delete the BB info for the bb with unredirected entry-point
   address 'addr'. */
void CLG_(delete_bb)(Addr addr)
{
    BB  *bb, *bp;
    Int idx, size;

    obj_node* obj = obj_of_address(addr);
    OffT offset = addr - obj->offset;

    idx = bb_hash_idx(obj, offset, bbs.size);
    bb = bbs.table[idx];

    /* bb points at the current bb under consideration, and bp is the
       one before. */
    bp = NULL;
    while(bb) {
      if ((bb->obj == obj) && (bb->offset == offset)) break;
      bp = bb;
      bb = bb->next;
    }

    if (bb == NULL) {
	CLG_DEBUG(3, "  delete_bb (Obj %s, off %p): NOT FOUND\n",
		  obj->name, offset);

	/* we didn't find it.
	 * this happens when callgrinds instrumentation mode
	 * was off at BB translation time, ie. no BB was created.
	 */
	return;
    }

    /* unlink it from hash table */

    if (bp == NULL) {
       /* we found the first one in the list. */
       tl_assert(bb == bbs.table[idx]);
       bbs.table[idx] = bb->next;
    } else {
       tl_assert(bb != bbs.table[idx]);
       bp->next = bb->next;
    }

    CLG_DEBUG(3, "  delete_bb (Obj %s, off %p): %p, BBCC head: %p\n",
	      obj->name, offset, bb, bb->bbcc_list);

    if (bb->bbcc_list == 0) {
	/* can be safely deleted */

	/* Fill the block up with junk and then free it, so we will
	   hopefully get a segfault if it is used again by mistake. */
	size = sizeof(BB)
	    + bb->instr_count * sizeof(InstrInfo)
	    + (bb->cjmp_count+1) * sizeof(CJmpInfo);
	VG_(memset)( bb, 0xAA, size );
	CLG_FREE(bb);
	return;
    }
    CLG_DEBUG(3, "  delete_bb: BB in use, can not free!\n");
}
