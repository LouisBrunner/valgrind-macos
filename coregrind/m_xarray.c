
/*--------------------------------------------------------------------*/
/*--- An expandable array implementation.               m_xarray.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007-2011 OpenWorks LLP
      info@open-works.co.uk

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
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"    /* self */


/* See pub_tool_xarray.h for details of what this is all about. */

struct _XArray {
   void* (*alloc) ( HChar*, SizeT ); /* alloc fn (nofail) */
   HChar* cc;                       /* cost centre for alloc */
   void  (*free) ( void* );         /* free fn */
   Int   (*cmpFn) ( void*, void* ); /* cmp fn (may be NULL) */
   Word  elemSzB;   /* element size in bytes */
   void* arr;       /* pointer to elements */
   Word  usedsizeE; /* # used elements in arr */
   Word  totsizeE;  /* max size of arr, in elements */
   Bool  sorted;    /* is it sorted? */
};


XArray* VG_(newXA) ( void*(*alloc_fn)(HChar*,SizeT), 
                     HChar* cc,
                     void(*free_fn)(void*),
                     Word elemSzB )
{
   struct _XArray* xa;
   /* Implementation relies on Word being signed and (possibly)
      on SizeT being unsigned. */
   vg_assert( sizeof(Word) == sizeof(void*) );
   vg_assert( ((Word)(-1)) < ((Word)(0)) );
   vg_assert( ((SizeT)(-1)) > ((SizeT)(0)) );
   /* check user-supplied info .. */
   vg_assert(alloc_fn);
   vg_assert(free_fn);
   vg_assert(elemSzB > 0);
   xa = alloc_fn( cc, sizeof(struct _XArray) );
   vg_assert(xa);
   xa->alloc     = alloc_fn;
   xa->cc        = cc;
   xa->free      = free_fn;
   xa->cmpFn     = NULL;
   xa->elemSzB   = elemSzB;
   xa->usedsizeE = 0;
   xa->totsizeE  = 0;
   xa->sorted    = False;
   xa->arr       = NULL;
   return xa;
}

XArray* VG_(cloneXA)( HChar* cc, XArray* xao )
{
   struct _XArray* xa = (struct _XArray*)xao;
   struct _XArray* nyu;
   HChar* nyu_cc;
   vg_assert(xa);
   vg_assert(xa->alloc);
   vg_assert(xa->free);
   vg_assert(xa->elemSzB >= 1);
   nyu_cc = cc ? cc : xa->cc;
   nyu = xa->alloc( nyu_cc, sizeof(struct _XArray) );
   if (!nyu)
      return NULL;
   /* Copy everything verbatim ... */
   *nyu = *xa;
   nyu->cc = nyu_cc;
   /* ... except we have to clone the contents-array */
   if (nyu->arr) {
      /* Restrict the total size of the new array to its current
         actual size.  That means we don't waste space copying the
         unused tail of the original.  The tradeoff is that it
         guarantees we will have to resize the child if even one more
         element is later added to it, unfortunately. */
      nyu->totsizeE = nyu->usedsizeE;
      /* and allocate .. */
      nyu->arr = nyu->alloc( nyu->cc, nyu->totsizeE * nyu->elemSzB );
      if (!nyu->arr) {
         nyu->free(nyu);
         return NULL;
      }
      VG_(memcpy)( nyu->arr, xa->arr, nyu->totsizeE * nyu->elemSzB );
   }
   /* We're done! */
   return nyu;
}

void VG_(deleteXA) ( XArray* xao )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(xa->free);
   if (xa->arr)
      xa->free(xa->arr);
   xa->free(xa);
}

void VG_(setCmpFnXA) ( XArray* xao, Int (*compar)(void*,void*) )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(compar);
   xa->cmpFn  = compar;
   xa->sorted = False;
}

inline void* VG_(indexXA) ( XArray* xao, Word n )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n < xa->usedsizeE);
   return ((char*)xa->arr) + n * xa->elemSzB;
}

static inline void ensureSpaceXA ( struct _XArray* xa )
{
   if (xa->usedsizeE == xa->totsizeE) {
      void* tmp;
      Word  newsz;
      if (xa->totsizeE == 0)
         vg_assert(!xa->arr);
      if (xa->totsizeE > 0)
         vg_assert(xa->arr);
      if (xa->totsizeE == 0) {
         /* No point in having tiny (eg) 2-byte allocations for the
            element array, since all allocs are rounded up to 8 anyway.
            Hence increase the initial array size for tiny elements in
            an attempt to avoid reallocations of size 2, 4, 8 if the
            array does start to fill up. */
         if (xa->elemSzB == 1) newsz = 8;
         else if (xa->elemSzB == 2) newsz = 4;
         else newsz = 2;
      } else {
         newsz = 2 + (3 * xa->totsizeE) / 2;  /* 2 * xa->totsizeE; */
      }
      if (0 && xa->totsizeE >= 10000) 
         VG_(printf)("addToXA: increasing from %ld to %ld\n", 
                     xa->totsizeE, newsz);
      tmp = xa->alloc(xa->cc, newsz * xa->elemSzB);
      vg_assert(tmp);
      if (xa->usedsizeE > 0) 
         VG_(memcpy)(tmp, xa->arr, xa->usedsizeE * xa->elemSzB);
      if (xa->arr)
         xa->free(xa->arr);
      xa->arr = tmp;
      xa->totsizeE = newsz;
   }
}

Word VG_(addToXA) ( XArray* xao, void* elem )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(elem);
   vg_assert(xa->totsizeE >= 0);
   vg_assert(xa->usedsizeE >= 0 && xa->usedsizeE <= xa->totsizeE);
   ensureSpaceXA( xa );
   vg_assert(xa->usedsizeE < xa->totsizeE);
   vg_assert(xa->arr);
   VG_(memcpy)( ((UChar*)xa->arr) + xa->usedsizeE * xa->elemSzB,
                elem, xa->elemSzB );
   xa->usedsizeE++;
   xa->sorted = False;
   return xa->usedsizeE-1;
}

Word VG_(addBytesToXA) ( XArray* xao, void* bytesV, Word nbytes )
{
   Word r, i;
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(xa->elemSzB == 1);
   vg_assert(nbytes >= 0);
   vg_assert(xa->totsizeE >= 0);
   vg_assert(xa->usedsizeE >= 0 && xa->usedsizeE <= xa->totsizeE);
   r = xa->usedsizeE;
   for (i = 0; i < nbytes; i++) {
      ensureSpaceXA( xa );
      vg_assert(xa->usedsizeE < xa->totsizeE);
      vg_assert(xa->arr);
      * (((UChar*)xa->arr) + xa->usedsizeE) = ((UChar*)bytesV)[i];
      xa->usedsizeE++;
   }
   xa->sorted = False;
   return r;
}

void VG_(sortXA) ( XArray* xao )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(xa->cmpFn);
   VG_(ssort)( xa->arr, xa->usedsizeE, xa->elemSzB, xa->cmpFn );
   xa->sorted = True;
}

Bool VG_(lookupXA_UNSAFE) ( XArray* xao, void* key,
                            /*OUT*/Word* first, /*OUT*/Word* last,
                            Int(*cmpFn)(void*,void*) )
{
   Word  lo, mid, hi, cres;
   void* midv;
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   lo = 0;
   hi = xa->usedsizeE-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return False; /* not found */
      mid  = (lo + hi) / 2;
      midv = VG_(indexXA)( xa, mid );
      cres = cmpFn( key, midv );
      if (cres < 0)  { hi = mid-1; continue; }
      if (cres > 0)  { lo = mid+1; continue; }
      /* Found it, at mid.  See how far we can expand this. */
      vg_assert(cmpFn( key, VG_(indexXA)(xa, lo) ) >= 0);
      vg_assert(cmpFn( key, VG_(indexXA)(xa, hi) ) <= 0);
      if (first) {
         *first = mid;
         while (*first > 0 
                && 0 == cmpFn( key, VG_(indexXA)(xa, (*first)-1))) {
            (*first)--;
         }
      }
      if (last) {
         *last = mid;
         while (*last < xa->usedsizeE-1
                && 0 == cmpFn( key, VG_(indexXA)(xa, (*last)+1))) {
            (*last)++;
         }
      }
      return True;
   }
}

Bool VG_(lookupXA) ( XArray* xao, void* key,
                     /*OUT*/Word* first, /*OUT*/Word* last )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(xa->cmpFn);
   vg_assert(xa->sorted);
   return VG_(lookupXA_UNSAFE)(xao, key, first, last, xa->cmpFn);
}

Word VG_(sizeXA) ( XArray* xao )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   return xa->usedsizeE;
}

void VG_(dropTailXA) ( XArray* xao, Word n )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n <= xa->usedsizeE);
   xa->usedsizeE -= n;
}

void VG_(dropHeadXA) ( XArray* xao, Word n )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n <= xa->usedsizeE);
   if (n == 0) {
      return;
   }
   if (n == xa->usedsizeE) {
      xa->usedsizeE = 0;
      return;
   }
   vg_assert(n > 0);
   vg_assert(xa->usedsizeE - n > 0);
   VG_(memcpy)( (char*)xa->arr,
                ((char*)xa->arr) + n * xa->elemSzB, 
                (xa->usedsizeE - n) * xa->elemSzB );
   xa->usedsizeE -= n;
}

void VG_(getContentsXA_UNSAFE)( XArray* xao,
                                /*OUT*/void** ctsP,
                                /*OUT*/Word* usedP )
{
   struct _XArray* xa = (struct _XArray*)xao;
   vg_assert(xa);
   *ctsP  = (void*)xa->arr;
   *usedP = xa->usedsizeE;
}

/* --------- Printeffery --------- */

static void add_char_to_XA ( HChar c, void* opaque )
{
   XArray* dst = (XArray*)opaque;
   (void) VG_(addBytesToXA)( dst, &c, 1 );
}

void VG_(xaprintf)( XArray* dst, const HChar* format, ... )
{
   va_list vargs;
   va_start(vargs, format);
   VG_(vcbprintf)( add_char_to_XA, (void*)dst, format, vargs );
   va_end(vargs);
}


/*--------------------------------------------------------------------*/
/*--- end                                               m_xarray.c ---*/
/*--------------------------------------------------------------------*/
