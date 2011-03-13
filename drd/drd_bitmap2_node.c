/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2011 Bart Van Assche <bvanassche@acm.org>.

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

/*
 * Block allocator for second-level bitmap nodes. Each node consists of
 * an OSetGen node and a struct bitmap2. The code below allocates
 * NODES_PER_CHUNK nodes at a time.
 */


#include "drd_basics.h"           /* DRD_() */
#include "drd_bitmap.h"           /* struct bitmap2 */
#include "pub_drd_bitmap.h"
#include "pub_tool_basics.h"      /* Addr, SizeT */
#include "pub_tool_libcassert.h"  /* tl_assert() */
#include "pub_tool_libcbase.h"    /* VG_ROUNDUP() */
#include "pub_tool_libcprint.h"   /* VG_(message)() */
#include "pub_tool_mallocfree.h"  /* VG_(malloc), VG_(free) */


#define NODES_PER_CHUNCK 512


/* Local type definitions. */

struct block_allocator_chunk {
   struct block_allocator_chunk* next;
   struct block_allocator_chunk* prev;
   int   nallocated;
   void* data;
   void* data_end;
   void* first_free;
};


/* Local variables. */

static SizeT s_bm2_node_size;
static struct block_allocator_chunk* s_first;


/* Function definitions. */

/**
 * Allocate a new chunk and insert it at the start of the doubly-linked list
 * s_first.
 */
static struct block_allocator_chunk* allocate_new_chunk(void)
{
   struct block_allocator_chunk* p;
   int i;

   tl_assert(s_bm2_node_size > 0);

   p = VG_(malloc)("drd.bitmap.bac",
                   sizeof(*p) + NODES_PER_CHUNCK * s_bm2_node_size);
   tl_assert(p);
   p->next = s_first;
   if (s_first)
      p->next->prev = p;
   s_first = p;
   p->prev = 0;
   p->nallocated = 0;
   p->data = (char*)p + sizeof(*p);
   tl_assert(p->data);
   p->data_end = (char*)(p->data) + NODES_PER_CHUNCK * s_bm2_node_size;
   p->first_free = p->data;
   for (i = 0; i < NODES_PER_CHUNCK - 1; i++)
   {
      *(void**)((char*)(p->data) + i * s_bm2_node_size)
         = (char*)(p->data) + (i + 1) * s_bm2_node_size;
   }
   tl_assert(i == NODES_PER_CHUNCK - 1);
   *(void**)((char*)(p->data) + i * s_bm2_node_size) = NULL;

   return p;
}

/** Free a chunk and remove it from the list of chunks. */
static void free_chunk(struct block_allocator_chunk* const p)
{
   tl_assert(p);
   tl_assert(p->nallocated == 0);

   if (p == s_first)
      s_first = p->next;
   else if (p->prev)
      p->prev->next = p->next;
   if (p->next)
      p->next->prev = p->prev;
   VG_(free)(p);
}

/** Allocate a node. */
void* DRD_(bm2_alloc_node)(HChar* const ec, const SizeT szB)
{
   /*
    * If szB < sizeof(struct bitmap2) then this function has been called to
    * allocate an AVL tree root node. Otherwise it has been called to allocate
    * an AVL tree branch or leaf node.
    */
   if (szB < sizeof(struct bitmap2))
      return VG_(malloc)(ec, szB);

   while (True)
   {
      struct block_allocator_chunk* p;

      if (s_bm2_node_size == 0)
         s_bm2_node_size = szB;
      else
         tl_assert(s_bm2_node_size == szB);

      for (p = s_first; p; p = p->next)
      {
         if (p->first_free)
         {
            void* result;

            p->nallocated++;
            result = p->first_free;
            p->first_free = *(void**)(p->first_free);
            return result;
         }
      }

      allocate_new_chunk();
   }
}

/** Free a node. */
void  DRD_(bm2_free_node)(void* const bm2)
{
   struct block_allocator_chunk* p;

   tl_assert(bm2);

   if (s_bm2_node_size > 0) {
      for (p = s_first; p; p = p->next) {
	 if (p->data <= bm2 && bm2 < p->data_end) {
	    /* Free a non-root AVL tree node. */
	    tl_assert(((char*)bm2 - (char*)(p->data)) % s_bm2_node_size == 0);
	    *(void**)bm2 = p->first_free;
	    p->first_free = bm2;
	    tl_assert(p->nallocated >= 1);
	    if (--(p->nallocated) == 0)
	       free_chunk(p);
	    return;
	 }
      }
   }
   /* Free the memory that was allocated for an AVL tree root node. */
   VG_(free)(bm2);
}
