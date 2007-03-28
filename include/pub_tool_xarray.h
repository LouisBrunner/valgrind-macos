
/*--------------------------------------------------------------------*/
/*--- An expandable array implementation.        pub_tool_xarray.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007-2007 OpenWorks LLP
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

#ifndef __PUB_TOOL_XARRAY_H
#define __PUB_TOOL_XARRAY_H

//--------------------------------------------------------------------
// PURPOSE: Provides a simple but useful structure, which is an array
// in which elements can be added at the end.  The array is expanded
// as needed by multiplying its size by a constant factor (usually 2).
// This gives amortised O(1) insertion cost, and, following sorting,
// the usual O(log N) binary search cost.  Arbitrary element sizes
// are allowed; the comparison function for sort/lookup can be changed
// at any time, and duplicates (modulo the comparison function) are
// allowed.
//--------------------------------------------------------------------


/* It's an abstract type.  Bwaha. */
typedef  void  XArray;

/* Create new XArray, using given allocation and free function, and
   for elements of the specified size.  Alloc fn must not fail (that
   is, if it returns it must have succeeded.) */
extern XArray* VG_(newXA) ( void*(*alloc_fn)(SizeT), 
                            void(*free_fn)(void*),
                            Word elemSzB );

/* Free all memory associated with an XArray. */
extern void VG_(deleteXA) ( XArray* );

/* Set the comparison function for this XArray.  This clears an
   internal 'array is sorted' flag, which means you must call sortXA
   before making further queries with lookupXA. */
extern void VG_(setCmpFnXA) ( XArray*, Int (*compar)(void*,void*) );

/* Add an element to an XArray.  Element is copied into the XArray.
   Index at which it was added is returned.  Note this will be
   invalidated if the array is later sortXA'd. */
extern Int VG_(addToXA) ( XArray*, void* elem );

/* Sort an XArray using its comparison function, if set; else bomb.
   Probably not a stable sort w.r.t. equal elements module cmpFn. */
extern void VG_(sortXA) ( XArray* );

/* Lookup (by binary search) 'key' in the array.  Set *first to be the
   index of the first, and *last to be the index of the last matching
   value found.  If any values are found, return True, else return
   False, and don't change *first or *last.  Bomb if the array is not
   sorted. */
extern Bool VG_(lookupXA) ( XArray*, void* key, 
                            /*OUT*/Word* first, /*OUT*/Word* last );

/* How elements are there in this XArray now? */
extern Word VG_(sizeXA) ( XArray* );

/* Index into the XArray.  Checks bounds and bombs if the index is
   invalid.  What this returns is the address of the specified element
   in the array, not (of course) the element itself.  Note that the
   element may get moved by subsequent addToXAs/sortXAs, so you should
   copy it out immediately and not regard its address as unchanging.
   Note also that indexXA will of course not return NULL if it
   succeeds. */
extern void* VG_(indexXA) ( XArray*, Word );

/* Drop the last n elements of an XArray.  Bombs if there are less
   than n elements in the array. */
extern void VG_(dropTailXA) ( XArray*, Word );


#endif   // __PUB_TOOL_XARRAY_H

/*--------------------------------------------------------------------*/
/*--- end                                        pub_tool_xarray.h ---*/
/*--------------------------------------------------------------------*/
