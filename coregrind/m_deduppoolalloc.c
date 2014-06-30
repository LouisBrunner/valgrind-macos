/*--------------------------------------------------------------------*/
/*--- A pool (memory) allocator that avoids duplicated copies.     ---*/
/*---                                           m_deduppoolalloc.c ---*/
/*--------------------------------------------------------------------*/
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2014 Philippe Waroquiers philippe.waroquiers@skynet.be

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

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcassert.h"
#include "pub_core_xarray.h"
#include "pub_core_deduppoolalloc.h" /* self */
#include "pub_core_hashtable.h"
#include "pub_core_poolalloc.h"
#include "pub_core_options.h"
#include "pub_core_mallocfree.h"
#include "pub_core_debuglog.h"

struct _DedupPoolAlloc {
   SizeT  poolSzB; /* Minimum size of a pool. */
   SizeT  eltAlign;
   void*   (*alloc)(const HChar*, SizeT); /* pool allocator */
   const HChar*  cc; /* pool allocator's cc */
   void    (*free)(void*); /* pool allocator's free-er */
   /* XArray of void* (pointers to pools).  The pools themselves.
      Each element is a pointer to a block of size at least PoolSzB bytes. */
   XArray *pools;

   /* hash table of pool elements, used to dedup.
      If NULL, it means the DedupPoolAlloc is frozen. */
   VgHashTable ht_elements;

   /* Hash table nodes of pool_elements are allocated with a pool, to
      decrease memory overhead during insertion in the DedupPoolAlloc. */
   PoolAlloc *ht_node_pa;

   UChar *curpool_free;  /* Pos in current pool to allocate next elt. */
   UChar *curpool_limit; /* Last pos in current pool. */

   /* Total nr of alloc calls, resulting in (we hope) a lot less
      real (dedup) elements. */
    ULong nr_alloc_calls;
};

typedef
   struct _ht_node {
      struct _ht_node *next; // Read/Write by hashtable (pub_tool_hashtable.h)
      UWord   key;           // Read by hashtable (pub_tool_hashtable.h)
      SizeT   eltSzB;
      void    *elt;
   }
   ht_node;

extern DedupPoolAlloc* VG_(newDedupPA) ( SizeT  poolSzB,
                                         SizeT  eltAlign,
                                         void*  (*alloc)(const HChar*, SizeT),
                                         const  HChar* cc,
                                         void   (*free_fn)(void*) ) 
{
   DedupPoolAlloc* ddpa;
   vg_assert(poolSzB >= eltAlign);
   vg_assert(poolSzB >= 100); /* let's say */
   vg_assert(poolSzB >= 10*eltAlign); /* let's say */
   vg_assert(alloc);
   vg_assert(cc);
   vg_assert(free_fn);
   ddpa = alloc(cc, sizeof(*ddpa));
   vg_assert(ddpa);
   VG_(memset)(ddpa, 0, sizeof(*ddpa));
   ddpa->poolSzB  = poolSzB;
   ddpa->eltAlign = eltAlign;
   ddpa->alloc    = alloc;
   ddpa->cc       = cc;
   ddpa->free     = free_fn;
   ddpa->pools    = VG_(newXA)( alloc, cc, free_fn, sizeof(void*) );

   ddpa->ht_elements = VG_(HT_construct) (cc);
   ddpa->ht_node_pa = VG_(newPA) ( sizeof(ht_node),
                                   1000,
                                   alloc,
                                   cc,
                                   free_fn);

   ddpa->curpool_limit = NULL;
   ddpa->curpool_free = ddpa->curpool_limit + 1;
   vg_assert(ddpa->pools);
   return ddpa;
}

void VG_(deleteDedupPA) ( DedupPoolAlloc* ddpa)
{
   Word i;
   if (ddpa->ht_elements)
      VG_(freezeDedupPA) (ddpa, NULL); // Free data structures used for insertion.
   for (i = 0; i < VG_(sizeXA) (ddpa->pools); i++)
      ddpa->free (*(UWord **)VG_(indexXA) ( ddpa->pools, i ));
   VG_(deleteXA) (ddpa->pools);
   ddpa->free (ddpa);
}

static __inline__
void ddpa_align_curpool_free ( DedupPoolAlloc* ddpa )
{
   ddpa->curpool_free = (UChar*)VG_ROUNDUP(ddpa->curpool_free, ddpa->eltAlign);
}

/* No space.  Allocate a new pool. */
__attribute__((noinline))
static void ddpa_add_new_pool ( DedupPoolAlloc* ddpa ) 
{
   vg_assert(ddpa);
   ddpa->curpool_free = ddpa->alloc( ddpa->cc, ddpa->poolSzB);
   vg_assert(ddpa->curpool_free);
   ddpa->curpool_limit = ddpa->curpool_free + ddpa->poolSzB - 1;
   /* add to our collection of pools */
   VG_(addToXA)( ddpa->pools, &ddpa->curpool_free );
   ddpa_align_curpool_free (ddpa);
}

static Word cmp_pool_elt (const void* node1, const void* node2 )
{
   const ht_node* hnode1 = node1;
   const ht_node* hnode2 = node2;

   if (hnode1->key < hnode2->key)
      return -1;
   else if (hnode1->key > hnode2->key)
      return 1;
   else if (hnode1->eltSzB == hnode2->eltSzB)
      return VG_(memcmp) (hnode1->elt, hnode2->elt, hnode1->eltSzB);
   else if (hnode1->eltSzB < hnode2->eltSzB)
      return -1;
   else
      return 1;
}

/* Print some stats. */
static void print_stats (DedupPoolAlloc *ddpa)
{
   VG_(message)(Vg_DebugMsg,
                "dedupPA:%s %ld allocs (%d uniq)" 
                " %ld pools (%ld bytes free in last pool)\n",
                ddpa->cc,
                (long int) ddpa->nr_alloc_calls,
                VG_(HT_count_nodes)(ddpa->ht_elements),
                VG_(sizeXA)(ddpa->pools),
                (long int) (ddpa->curpool_limit - ddpa->curpool_free + 1));
   VG_(HT_print_stats) (ddpa->ht_elements, cmp_pool_elt);
}

/* Dummy free, as the ht elements are allocated in a pool, and
   we will destroy the pool in one single operation. */
static void htelem_dummyfree(void* ht_elem)
{
}

void VG_(freezeDedupPA) (DedupPoolAlloc *ddpa,
                         void (*shrink_block)(void*, SizeT))
{
   if (VG_(clo_stats) 
       && (VG_(clo_verbosity) > 2 || VG_(debugLog_getLevel) () >= 2)) {
      print_stats(ddpa);
   }
   if (shrink_block && ddpa->curpool_limit > ddpa->curpool_free) {
      UChar *last_added_pool = 
         (*(UChar **)VG_(indexXA) ( ddpa->pools, 
                                    VG_(sizeXA)(ddpa->pools) - 1));
      (*shrink_block)(last_added_pool, ddpa->curpool_free - last_added_pool);
   }
   VG_(HT_destruct) ( ddpa->ht_elements, htelem_dummyfree);
   ddpa->ht_elements = NULL;
   VG_(deletePA) (ddpa->ht_node_pa);
   ddpa->ht_node_pa = NULL;
}

void* VG_(allocEltDedupPA) (DedupPoolAlloc *ddpa, SizeT eltSzB, const void *elt)
{
   ht_node ht_elt;
   void* elt_ins;
   ht_node *ht_ins;
   vg_assert(ddpa);
   vg_assert(ddpa->ht_elements);
   vg_assert (eltSzB <= ddpa->poolSzB);

   ddpa->nr_alloc_calls++;

   // Currently using adler32 as hash function.
   // Many references tells adler32 is bad as a hash function.
   // And effectively, some tests on dwarf debug string shows
   // a lot of collisions (at least for short elements).
   // (A lot can be 10% of the elements colliding, even on
   // small nr of elements such as 10_000).
   ht_elt.key = VG_(adler32) (0, NULL, 0);
   ht_elt.key = VG_(adler32) (ht_elt.key, (UChar*)elt, eltSzB);

   ht_elt.eltSzB = eltSzB;
   ht_elt.elt = (UChar*) elt;

   ht_ins = VG_(HT_gen_lookup) (ddpa->ht_elements, &ht_elt, cmp_pool_elt);
   if (ht_ins)
      return ht_ins->elt;

   /* Not found -> we need to allocate a new element from the pool
      and insert it in the hash table of inserted elements. */

   // Add a new pool if not enough space in the current pool
   if (UNLIKELY(ddpa->curpool_free + eltSzB - 1 > ddpa->curpool_limit)) {
      ddpa_add_new_pool(ddpa);
   }

   elt_ins = ddpa->curpool_free;
   VG_(memcpy)(elt_ins, elt, eltSzB);
   ddpa->curpool_free = ddpa->curpool_free + eltSzB;
   ddpa_align_curpool_free (ddpa);

   ht_ins = VG_(allocEltPA) (ddpa->ht_node_pa);
   ht_ins->key = ht_elt.key;
   ht_ins->eltSzB = eltSzB;
   ht_ins->elt = elt_ins;
   VG_(HT_add_node)(ddpa->ht_elements, ht_ins);
   return elt_ins;
}
