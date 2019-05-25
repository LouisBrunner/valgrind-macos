
/*--------------------------------------------------------------------*/
/*--- An expandable array implementation.               m_xarray.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007-2017 OpenWorks LLP
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"    /* self */


/* See pub_tool_xarray.h for details of what this is all about. */

struct _XArray {
   Alloc_Fn_t alloc_fn;                /* alloc fn (nofail) */
   const HChar* cc;                    /* cost centre for alloc */
   Free_Fn_t free_fn;                  /* free fn */
   Int   (*cmpFn) ( const void*, const void* ); /* cmp fn (may be NULL) */
   Word  elemSzB;   /* element size in bytes */
   void* arr;       /* pointer to elements */
   Word  usedsizeE; /* # used elements in arr */
   Word  totsizeE;  /* max size of arr, in elements */
   Bool  sorted;    /* is it sorted? */
};


XArray* VG_(newXA) ( Alloc_Fn_t alloc_fn,
                     const HChar* cc,
                     Free_Fn_t free_fn,
                     Word elemSzB )
{
   XArray* xa;
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
   xa->alloc_fn  = alloc_fn;
   xa->cc        = cc;
   xa->free_fn   = free_fn;
   xa->cmpFn     = NULL;
   xa->elemSzB   = elemSzB;
   xa->usedsizeE = 0;
   xa->totsizeE  = 0;
   xa->sorted    = False;
   xa->arr       = NULL;
   return xa;
}

XArray* VG_(cloneXA)( const HChar* cc, const XArray* xa )
{
   XArray* nyu;
   const HChar* nyu_cc;
   vg_assert(xa);
   vg_assert(xa->alloc_fn);
   vg_assert(xa->free_fn);
   vg_assert(xa->elemSzB >= 1);
   nyu_cc = cc ? cc : xa->cc;
   nyu = xa->alloc_fn( nyu_cc, sizeof(struct _XArray) );

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
      nyu->arr = nyu->alloc_fn( nyu->cc, nyu->totsizeE * nyu->elemSzB );
      VG_(memcpy)( nyu->arr, xa->arr, nyu->totsizeE * nyu->elemSzB );
   }
   /* We're done! */
   return nyu;
}

void VG_(deleteXA) ( XArray* xa )
{
   vg_assert(xa);
   vg_assert(xa->free_fn);
   if (xa->arr)
      xa->free_fn(xa->arr);
   xa->free_fn(xa);
}

void VG_(setCmpFnXA) ( XArray* xa, XACmpFn_t compar )
{
   vg_assert(xa);
   vg_assert(compar);
   xa->cmpFn  = compar;
   xa->sorted = False;
}

inline void* VG_(indexXA) ( const XArray* xa, Word n )
{
   vg_assert(xa);
   /* vg_assert(n >= 0); If n negative, the UWord conversion will make
      it bigger than usedsizeE, which is verified to be non negative when
      xa is modified. */
   vg_assert((UWord)n < (UWord)xa->usedsizeE);
   return ((char*)xa->arr) + n * xa->elemSzB;
}

void VG_(hintSizeXA) ( XArray* xa, Word n)
{
   /* Currently, we support giving a size hint only just after the
      call to VG_(newXA). So, we could instead have done
      a function VG_(newXA_with_SizeHint). The separate VG_(hintSizeXA)
      function is however chosen as we might one day accept to
      give a size hint after having added elements. That could be useful
      for reducing the size of an xarray to just the size currently needed
      or to give a size hint when it is known that a lot more elements
      are needed or when the final nr of elements is known. */
   vg_assert(xa);
   vg_assert(xa->usedsizeE == 0);
   vg_assert(xa->totsizeE == 0);
   vg_assert(!xa->arr);
   if (n > 0) {
      xa->arr = xa->alloc_fn(xa->cc, n * xa->elemSzB);
      xa->totsizeE = n;
   }
}

static inline void ensureSpaceXA ( XArray* xa )
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
      tmp = xa->alloc_fn(xa->cc, newsz * xa->elemSzB);
      if (xa->usedsizeE > 0) 
         VG_(memcpy)(tmp, xa->arr, xa->usedsizeE * xa->elemSzB);
      if (xa->arr)
         xa->free_fn(xa->arr);
      xa->arr = tmp;
      xa->totsizeE = newsz;
   }
}

Word VG_(addToXA) ( XArray* xa, const void* elem )
{
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

Word VG_(addBytesToXA) ( XArray* xa, const void* bytesV, Word nbytes )
{
   Word r, i;
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
      * (((UChar*)xa->arr) + xa->usedsizeE) = ((const UChar*)bytesV)[i];
      xa->usedsizeE++;
   }
   xa->sorted = False;
   return r;
}

void VG_(sortXA) ( XArray* xa )
{
   vg_assert(xa);
   vg_assert(xa->cmpFn);
   VG_(ssort)( xa->arr, xa->usedsizeE, xa->elemSzB, xa->cmpFn );
   xa->sorted = True;
}

Bool VG_(lookupXA_UNSAFE) ( const XArray* xa, const void* key,
                            /*OUT*/Word* first, /*OUT*/Word* last,
                            Int(*cmpFn)(const void*, const void*) )
{
   Word  lo, mid, hi, cres;
   void* midv;
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

Bool VG_(lookupXA) ( const XArray* xa, const void* key,
                     /*OUT*/Word* first, /*OUT*/Word* last )
{
   vg_assert(xa);
   vg_assert(xa->cmpFn);
   vg_assert(xa->sorted);
   return VG_(lookupXA_UNSAFE)(xa, key, first, last, xa->cmpFn);
}

/* FIXME: This function should return an unsigned value because the number
   of elements cannot be negative. Unfortunately, making the change causes
   a lot of ripple. */
Word VG_(sizeXA) ( const XArray* xa )
{
   vg_assert(xa);
   return xa->usedsizeE;
}

void VG_(dropTailXA) ( XArray* xa, Word n )
{
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n <= xa->usedsizeE);
   xa->usedsizeE -= n;
}

void VG_(dropHeadXA) ( XArray* xa, Word n )
{
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

void VG_(removeIndexXA)( XArray* xa, Word n )
{
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n < xa->usedsizeE);
   if (n+1 < xa->usedsizeE) {
      VG_(memmove)( ((char*)xa->arr) + (n+0) * xa->elemSzB,
                    ((char*)xa->arr) + (n+1) * xa->elemSzB,
                    (xa->usedsizeE - n - 1) * xa->elemSzB );
   }
   xa->usedsizeE--;
}

void VG_(insertIndexXA)( XArray* xa, Word n, const void* elem )
{
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n <= xa->usedsizeE);
   vg_assert(xa->usedsizeE >= 0 && xa->usedsizeE <= xa->totsizeE);
   ensureSpaceXA( xa );
   vg_assert(xa->usedsizeE < xa->totsizeE);
   vg_assert(xa->arr);
   if (n < xa->usedsizeE) {
      VG_(memmove) ( ((char*)xa->arr) + (n+1) * xa->elemSzB,
                     ((char*)xa->arr) + (n+0) * xa->elemSzB,
                     (xa->usedsizeE - n) * xa->elemSzB );
   }
   VG_(memcpy)( ((UChar*)xa->arr) + n * xa->elemSzB,
                elem, xa->elemSzB );
   xa->usedsizeE++;
   xa->sorted = False;
}

void VG_(replaceIndexXA)( XArray* xa, Word n, const void* elem )
{
   vg_assert(xa);
   vg_assert(n >= 0);
   vg_assert(n < xa->usedsizeE);
   vg_assert(xa->usedsizeE >= 0 && xa->usedsizeE <= xa->totsizeE);
   VG_(memcpy)( ((UChar*)xa->arr) + n * xa->elemSzB,
                elem, xa->elemSzB );
   xa->sorted = False;
}

void VG_(getContentsXA_UNSAFE)( XArray* xa,
                                /*OUT*/void** ctsP,
                                /*OUT*/Word* usedP )
{
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

Bool VG_(strIsMemberXA)(const XArray* xa, const HChar* str )
{
   Word i;
   HChar** members = (HChar**)xa->arr;

   for (i = 0; i < xa->usedsizeE; i++)
      if (VG_(strcmp)(str, members[i]) == 0)
         return True;
   return False;
}

/*--------------------------------------------------------------------*/
/*--- end                                               m_xarray.c ---*/
/*--------------------------------------------------------------------*/
