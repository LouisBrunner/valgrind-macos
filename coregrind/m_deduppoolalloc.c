/*--------------------------------------------------------------------*/
/*--- A pool (memory) allocator that avoids duplicated copies.     ---*/
/*---                                           m_deduppoolalloc.c ---*/
/*--------------------------------------------------------------------*/
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2017 Philippe Waroquiers philippe.waroquiers@skynet.be

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
   SizeT  fixedSzb; /* If using VG_(allocFixedEltDedupPA), size of elements */
   Bool   strPA;    /* True if this is a string dedup pool */
   SizeT  eltAlign;
   Alloc_Fn_t alloc_fn; /* pool allocator */
   const HChar*  cc; /* pool allocator's cost centre */
   Free_Fn_t free_fn; /* pool allocator's deallocation function */
   /* XArray of void* (pointers to pools).  The pools themselves.
      Each element is a pointer to a block of size at least PoolSzB bytes.
      The last block might be smaller due to a call to shrink_block. */
   XArray *pools;

   /* hash table of pool elements, used to dedup.
      If NULL, it means the DedupPoolAlloc is frozen. */
   VgHashTable *ht_elements;

   /* Hash table nodes of pool_elements are allocated with a pool, to
      decrease memory overhead during insertion in the DedupPoolAlloc. */
   PoolAlloc *ht_node_pa;

   UChar *curpool;       /* last allocated pool. */
   UChar *curpool_free;  /* Pos in current pool to allocate next elt.
                            always aligned on eltAlign. */
   UChar *curpool_limit; /* Last pos in current pool. */
   /* Note that for a fixed size pool, we only have a single pool to allow
      simple/fast indexing. This single pool is grown, which might change
      the address of the already allocated elements. */

   /* Total nr of alloc calls, resulting in (we hope) a lot less
      real (dedup) elements. */
   ULong nr_alloc_calls;
};

typedef
   struct _ht_node {
      struct _ht_node *next; // Read/Write by hashtable (pub_tool_hashtable.h)
      UWord   key;           // Read by hashtable (pub_tool_hashtable.h)
      SizeT   eltSzBorStrNr; // for a normal pool, elt size 
                             // for a string pool, the unique str number
      const void *elt;
   }
   ht_node;

DedupPoolAlloc* VG_(newDedupPA) ( SizeT  poolSzB,
                                  SizeT  eltAlign,
                                  Alloc_Fn_t alloc_fn,
                                  const  HChar* cc,
                                  Free_Fn_t free_fn )
{
   DedupPoolAlloc* ddpa;
   vg_assert(poolSzB >= eltAlign);
   vg_assert(poolSzB >= 100); /* let's say */
   vg_assert(poolSzB >= 10*eltAlign); /* let's say */
   vg_assert(alloc_fn);
   vg_assert(cc);
   vg_assert(free_fn);
   ddpa = alloc_fn(cc, sizeof(*ddpa));
   VG_(memset)(ddpa, 0, sizeof(*ddpa));
   ddpa->poolSzB  = poolSzB;
   ddpa->fixedSzb = 0;
   ddpa->strPA = False;
   ddpa->eltAlign = eltAlign;
   ddpa->alloc_fn = alloc_fn;
   ddpa->cc       = cc;
   ddpa->free_fn  = free_fn;
   ddpa->pools    = VG_(newXA)( alloc_fn, cc, free_fn, sizeof(void*) );

   ddpa->ht_elements = VG_(HT_construct) (cc);
   ddpa->ht_node_pa = VG_(newPA) ( sizeof(ht_node),
                                   1000,
                                   alloc_fn,
                                   cc,
                                   free_fn);
   ddpa->curpool = NULL;
   ddpa->curpool_limit = NULL;
   ddpa->curpool_free = NULL;

   return ddpa;
}

void VG_(deleteDedupPA) ( DedupPoolAlloc* ddpa)
{
   Word i;
   if (ddpa->ht_elements)
      // Free data structures used for insertion.
      VG_(freezeDedupPA) (ddpa, NULL);
   for (i = 0; i < VG_(sizeXA) (ddpa->pools); i++)
      ddpa->free_fn (*(UWord **)VG_(indexXA) ( ddpa->pools, i ));
   VG_(deleteXA) (ddpa->pools);
   ddpa->free_fn (ddpa);
}

static __inline__
UChar* ddpa_align ( DedupPoolAlloc* ddpa, UChar *c )
{
   return (UChar*)VG_ROUNDUP(c, ddpa->eltAlign);
}

/* Allocate a new pool or grow the (only) pool for a fixed size ddpa. */
__attribute__((noinline))
static void ddpa_add_new_pool_or_grow ( DedupPoolAlloc* ddpa )
{
   vg_assert(ddpa);

   if (ddpa->fixedSzb > 0 && ddpa->curpool != NULL) {
      // Grow (* 2) the current (fixed elt) pool
      UChar *curpool_align = ddpa_align(ddpa, ddpa->curpool);
      SizeT curpool_used = ddpa->curpool_free - curpool_align;
      SizeT curpool_size = ddpa->curpool_limit - ddpa->curpool + 1;
      UChar *newpool = ddpa->alloc_fn (ddpa->cc, 2 * curpool_size);
      UChar *newpool_free = ddpa_align (ddpa, newpool);
      UChar *newpool_limit = newpool + 2 * curpool_size - 1;
      Word reloc_offset = (Addr)newpool_free - (Addr)curpool_align;
      ht_node *n;

      VG_(memcpy) (newpool_free, curpool_align, curpool_used);
      /* We have reallocated the (only) pool. We need to relocate the pointers
         in the hash table nodes. */
      VG_(HT_ResetIter) (ddpa->ht_elements);
      while ((n = VG_(HT_Next) (ddpa->ht_elements))) {
        n->elt = (void*)((Addr)n->elt + reloc_offset);
      }
      newpool_free += curpool_used;

      VG_(dropHeadXA) (ddpa->pools, 1);
      ddpa->free_fn (ddpa->curpool);
      ddpa->curpool = newpool;
      ddpa->curpool_free = newpool_free;
      ddpa->curpool_limit = newpool_limit;
      VG_(addToXA)( ddpa->pools, &ddpa->curpool);
   } else {
      /* Allocate a new pool, or allocate the first/only pool for a
         fixed size ddpa. */
      ddpa->curpool = ddpa->alloc_fn( ddpa->cc, ddpa->poolSzB);
      ddpa->curpool_limit = ddpa->curpool + ddpa->poolSzB - 1;
      ddpa->curpool_free = ddpa_align (ddpa, ddpa->curpool);
      /* add to our collection of pools */
      VG_(addToXA)( ddpa->pools, &ddpa->curpool );
   }
}

/* Compare function for 'gen' hash table. No need to compare the key
   in this function, as the hash table already does it for us,
   and that in any case, if the data is equal, the keys must also be
   equal. */
static Word cmp_pool_elt (const void* node1, const void* node2 )
{
   const ht_node* hnode1 = node1;
   const ht_node* hnode2 = node2;

   /* As this function is called by hashtable, that has already checked
      for key equality, it is likely that it is the 'good' element.
      So, we handle the equal case first. */
   if (hnode1->eltSzBorStrNr == hnode2->eltSzBorStrNr)
      return VG_(memcmp) (hnode1->elt, hnode2->elt, hnode1->eltSzBorStrNr);
   else if (hnode1->eltSzBorStrNr < hnode2->eltSzBorStrNr)
      return -1;
   else
      return 1;
}

/* String compare function for 'gen' hash table.
   Similarly to cmp_pool_elt, no need to compare the key. */
static Word cmp_pool_str (const void* node1, const void* node2 )
{
   const ht_node* hnode1 = node1;
   const ht_node* hnode2 = node2;

   return VG_(strcmp)(hnode1->elt, hnode2->elt);
}

/* Print some stats. */
static void print_stats (DedupPoolAlloc *ddpa)
{
   VG_(message)(Vg_DebugMsg,
                "dedupPA:%s %ld allocs (%u uniq)"
                " %ld pools (%ld bytes free in last pool)\n",
                ddpa->cc,
                (long int) ddpa->nr_alloc_calls,
                VG_(HT_count_nodes)(ddpa->ht_elements),
                VG_(sizeXA)(ddpa->pools),
                ddpa->curpool ?
                (long int) (ddpa->curpool_limit - ddpa->curpool_free + 1) : 0);
   if (ddpa->strPA)
      VG_(HT_print_stats) (ddpa->ht_elements, cmp_pool_str);
   else
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
   vg_assert (!ddpa->fixedSzb || VG_(sizeXA) (ddpa->pools) == 1);
   if (shrink_block && ddpa->curpool_limit > ddpa->curpool_free)
      (*shrink_block)(ddpa->curpool, ddpa->curpool_free - ddpa->curpool);
   VG_(HT_destruct) ( ddpa->ht_elements, htelem_dummyfree);
   ddpa->ht_elements = NULL;
   VG_(deletePA) (ddpa->ht_node_pa);
   ddpa->ht_node_pa = NULL;
}


// hash function used by gawk and SDBM.
static UInt sdbm_hash (const UChar* buf, UInt len )
{
  UInt h;
  UInt i;

  h = 0;
  for (i = 0; i < len; i++)
    h = *buf++ + (h<<6) + (h<<16) - h;
  return h;
}

static ht_node* allocEltDedupPA (DedupPoolAlloc *ddpa, SizeT eltSzB,
                                 const void *elt)
{
   ht_node ht_elt;
   void* elt_ins;
   ht_node *ht_ins;
   vg_assert(ddpa);
   vg_assert(ddpa->ht_elements);

   ddpa->nr_alloc_calls++;

   ht_elt.key = sdbm_hash (elt, eltSzB);

   ht_elt.elt = elt;

   if (ddpa->strPA)
      ht_ins = VG_(HT_gen_lookup) (ddpa->ht_elements, &ht_elt, cmp_pool_str);
   else {
      ht_elt.eltSzBorStrNr = eltSzB;
      ht_ins = VG_(HT_gen_lookup) (ddpa->ht_elements, &ht_elt, cmp_pool_elt);
   }
   if (ht_ins)
      return ht_ins;

   /* Not found -> we need to allocate a new element from the pool
      and insert it in the hash table of inserted elements. */

   // Add a new pool or grow pool if not enough space in the current pool
   if (eltSzB + ddpa->eltAlign > ddpa->poolSzB) {
      // Element (+eltAlign for worst case) bigger than the pool size
      // => allocate a specific pool just for this element
      UChar *newpool = ddpa->alloc_fn (ddpa->cc, eltSzB + ddpa->eltAlign);
      /* add to our collection of pools */
      VG_(addToXA)( ddpa->pools, &newpool );
      elt_ins = ddpa_align (ddpa, newpool);
   } else {
      if (UNLIKELY(ddpa->curpool_free == NULL
                   || ddpa->curpool_free + eltSzB - 1 > ddpa->curpool_limit)) {
         ddpa_add_new_pool_or_grow (ddpa);
      }
      elt_ins = ddpa->curpool_free;
      ddpa->curpool_free = ddpa_align(ddpa, ddpa->curpool_free + eltSzB);
   }


   VG_(memcpy)(elt_ins, elt, eltSzB);
   ht_ins = VG_(allocEltPA) (ddpa->ht_node_pa);
   ht_ins->key = ht_elt.key;
   if (ddpa->strPA)
      ht_ins->eltSzBorStrNr = VG_(HT_count_nodes)(ddpa->ht_elements) + 1;
   else
      ht_ins->eltSzBorStrNr = eltSzB;
   ht_ins->elt = elt_ins;
   VG_(HT_add_node)(ddpa->ht_elements, ht_ins);
   return ht_ins;
}

const void* VG_(allocEltDedupPA) (DedupPoolAlloc *ddpa, SizeT eltSzB,
                                  const void *elt)
{
   return allocEltDedupPA(ddpa, eltSzB, elt)->elt;
}

UInt VG_(allocStrDedupPA) (DedupPoolAlloc *ddpa,
                           const HChar* str,
                           Bool* newStr)
{
   if (!ddpa->strPA) {
      // First insertion in this ddpa
      vg_assert (ddpa->nr_alloc_calls == 0);
      vg_assert (ddpa->fixedSzb == 0);
      ddpa->strPA = True;
   }

   const UInt nr_str = VG_(HT_count_nodes)(ddpa->ht_elements);
   const ht_node* ht_ins = allocEltDedupPA(ddpa, VG_(strlen)(str)+1, str);

   *newStr = nr_str < VG_(HT_count_nodes)(ddpa->ht_elements);
   return ht_ins->eltSzBorStrNr;
}

static __inline__
UInt elt2nr (DedupPoolAlloc *ddpa, const void *dedup_elt)
{
   vg_assert (dedup_elt >= (const void *)ddpa->curpool
              && dedup_elt < (const void *)ddpa->curpool_free);
   return 1 + ((const UChar*)dedup_elt - (const UChar *)ddpa->curpool)
      / VG_ROUNDUP(ddpa->fixedSzb, ddpa->eltAlign);
}

UInt VG_(allocFixedEltDedupPA) (DedupPoolAlloc *ddpa,
                                SizeT eltSzB, const void *elt)
{
   if (ddpa->fixedSzb == 0) {
      // First insertion in this ddpa
      vg_assert (!ddpa->strPA);
      vg_assert (ddpa->nr_alloc_calls == 0);
      vg_assert (eltSzB > 0);
      ddpa->fixedSzb = eltSzB;
   }
   vg_assert (ddpa->fixedSzb == eltSzB);
   const void *dedup_elt = VG_(allocEltDedupPA) (ddpa, eltSzB, elt);
   return elt2nr (ddpa, dedup_elt);
}

void* VG_(indexEltNumber) (DedupPoolAlloc *ddpa,
                           UInt eltNr)
{
   void *dedup_elt;

   dedup_elt = ddpa->curpool
      + (eltNr - 1) * VG_ROUNDUP(ddpa->fixedSzb, ddpa->eltAlign);

   vg_assert ((UChar*)dedup_elt >= ddpa->curpool
              && (UChar*)dedup_elt < ddpa->curpool_free);

   return dedup_elt;
}

UInt VG_(sizeDedupPA) (DedupPoolAlloc *ddpa)
{
   if (ddpa->curpool == NULL)
      return 0;

   vg_assert (ddpa->fixedSzb);
   return (ddpa->curpool_free - ddpa_align(ddpa, ddpa->curpool))
      / VG_ROUNDUP(ddpa->fixedSzb, ddpa->eltAlign);
}
