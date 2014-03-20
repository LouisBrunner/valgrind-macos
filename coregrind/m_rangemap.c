
/*--------------------------------------------------------------------*/
/*--- A mapping where the keys exactly cover the address space.    ---*/
/*---                                                 m_rangemap.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2014 Mozilla Foundation

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

/* Contributed by Julian Seward <jseward@acm.org> */

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "pub_core_rangemap.h"    /* self */


/* See pub_tool_rangemap.h for details of what this is all about. */

#define UWORD_MIN ((UWord)0)
#define UWORD_MAX (~(UWord)0)

typedef
   struct { UWord key_min; UWord key_max; UWord val; }
   Range;


struct _RangeMap {
   void* (*alloc) ( const HChar*, SizeT ); /* alloc fn (nofail) */
   const HChar* cc;                 /* cost centre for alloc */
   void  (*free) ( void* );         /* free fn */
   XArray* ranges;
};


/* fwds */
static void preen (/*MOD*/RangeMap* rm);
static Word find ( RangeMap* rm, UWord key );
static void split_at ( /*MOD*/RangeMap* rm, UWord key );
static void show ( RangeMap* rm );


RangeMap* VG_(newRangeMap) ( void*(*alloc_fn)(const HChar*,SizeT), 
                             const HChar* cc,
                             void(*free_fn)(void*),
                             UWord initialVal )
{
   /* check user-supplied info .. */
   vg_assert(alloc_fn);
   vg_assert(free_fn);
   RangeMap* rm = alloc_fn(cc, sizeof(RangeMap));
   vg_assert(rm);
   rm->alloc  = alloc_fn;
   rm->cc     = cc;
   rm->free   = free_fn;
   rm->ranges = VG_(newXA)( alloc_fn, cc, free_fn, sizeof(Range) );
   vg_assert(rm->ranges);
   /* Add the initial range */
   Range r;
   r.key_min = UWORD_MIN;
   r.key_max = UWORD_MAX;
   r.val     = initialVal;
   Word i = VG_(addToXA)(rm->ranges, &r);
   vg_assert(i == 0);
   vg_assert(VG_(sizeXA)(rm->ranges) == 1);
   /* */
   return rm;
}

void VG_(deleteRangeMap) ( RangeMap* rm )
{
   vg_assert(rm);
   vg_assert(rm->free);
   vg_assert(rm->ranges);
   VG_(deleteXA)(rm->ranges);
   rm->free(rm);
}

void VG_(bindRangeMap) ( RangeMap* rm,
                         UWord key_min, UWord key_max, UWord val )
{
   vg_assert(key_min <= key_max);
   split_at(rm, key_min);
   if (key_max < UWORD_MAX)
      split_at(rm, key_max + 1);
   Word iMin, iMax, i;
   iMin = find(rm, key_min);
   iMax = find(rm, key_max);
   for (i = iMin; i <= iMax; i++) {
      Range* rng = VG_(indexXA)(rm->ranges, i);
      rng->val = val;
   }
   preen(rm);
}

void VG_(lookupRangeMap) ( /*OUT*/UWord* key_min, /*OUT*/UWord* key_max,
                           /*OUT*/UWord* val, RangeMap* rm, UWord key )
{
   Word   i   = find(rm, key);
   Range* rng = (Range*)VG_(indexXA)(rm->ranges, i);
   *key_min = rng->key_min;
   *key_max = rng->key_max;
   *val     = rng->val;
}

Word VG_(sizeRangeMap) ( RangeMap* rm )
{
   vg_assert(rm && rm->ranges);
   return VG_(sizeXA)(rm->ranges);
}

void VG_(indexRangeMap) ( /*OUT*/UWord* key_min, /*OUT*/UWord* key_max,
                          /*OUT*/UWord* val, RangeMap* rm, Word ix )
{
   vg_assert(rm && rm->ranges);
   Range* rng = (Range*)VG_(indexXA)(rm->ranges, ix);
   *key_min = rng->key_min;
   *key_max = rng->key_max;
   *val     = rng->val;
}

/* Helper functions, not externally visible. */

static void preen (/*MOD*/RangeMap* rm)
{
   Word    i;
   XArray* ranges = rm->ranges;
   for (i = 0; i < VG_(sizeXA)(ranges) - 1; i++) {
      Range* rng0 = VG_(indexXA)(ranges, i+0);
      Range* rng1 = VG_(indexXA)(ranges, i+1);
      if (rng0->val != rng1->val)
         continue;
      rng0->key_max = rng1->key_max;
      VG_(removeIndexXA)(ranges, i+1);
      /* Back up one, so as not to miss an opportunity to merge with
         the entry after this one. */
      i--;
   }
}

static Word find ( RangeMap* rm, UWord key )
{
   XArray* ranges = rm->ranges;
   Word    lo     = 0;
   Word    hi     = VG_(sizeXA)(ranges);
   while (True) {
      /* The unsearched space is lo .. hi inclusive */
      if (lo > hi) {
         /* Not found.  This can't happen. */
         VG_(core_panic)("RangeMap::find: not found");
         /*NOTREACHED*/
         return -1;
      }
      Word   mid         = (lo + hi) / 2;
      Range* mid_rng     = (Range*)VG_(indexXA)(ranges, mid);
      UWord  key_mid_min = mid_rng->key_min;
      UWord  key_mid_max = mid_rng->key_max;
      if (key < key_mid_min) { hi = mid-1; continue; }
      if (key > key_mid_max) { lo = mid+1; continue; }
      return mid;
   }
}

static void split_at ( /*MOD*/RangeMap* rm, UWord key )
{
   XArray* ranges = rm->ranges;
   Word    i      = find(rm, key);
   Range   rng_i0 = *(Range*)VG_(indexXA)( ranges, i+0 );
   if (rng_i0.key_min == key)
      return;
   VG_(insertIndexXA)( ranges, i+1, &rng_i0 );
   /* The insert could have relocated the payload, hence the
      re-indexing of i+0 here. */
   Range* rng_i0p = (Range*)VG_(indexXA)( ranges, i+0 );
   Range* rng_i1p = (Range*)VG_(indexXA)( ranges, i+1 );
   rng_i0p->key_max = key-1;
   rng_i1p->key_min = key;
}

__attribute__((unused))
static void show ( RangeMap* rm )
{
   Word i;
   VG_(printf)("<< %ld entries:\n", VG_(sizeXA)(rm->ranges) );
   for (i = 0; i < VG_(sizeXA)(rm->ranges); i++) {
      Range* rng = (Range*)VG_(indexXA)(rm->ranges, i);
      VG_(printf)("  %016llx  %016llx  --> 0x%llx\n",
                  (ULong)rng->key_min, (ULong)rng->key_max, (ULong)rng->val);
   }
   VG_(printf)(">>\n");
}


/*--------------------------------------------------------------------*/
/*--- end                                             m_rangemap.c ---*/
/*--------------------------------------------------------------------*/
