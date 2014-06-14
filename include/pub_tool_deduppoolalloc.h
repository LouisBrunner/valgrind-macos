
/*--------------------------------------------------------------------*/
/*--- A pool (memory) allocator that avoids duplicated copies.     ---*/
/*---                                    pub_tool_deduppoolalloc.h ---*/
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

#ifndef __PUB_TOOL_DEDUPPOOLALLOC_H
#define __PUB_TOOL_DEDUPPOOLALLOC_H

#include "pub_tool_basics.h"   // UWord

//-----------------------------------------------------------------------------
// PURPOSE: Provides a pool allocator for elements, storing only once identical
//  elements. In other words, this can be considered a "dictionary" of elements.
//
// This pool allocator manages elements allocation by allocating "pools" of
// many elements from a lower level allocator (typically pub_tool_mallocfree.h).
// Single elements are allocated from these pools.
// Currently, elements can only be allocated, elements cannot be freed
// individually.
// Once allocated, an element must not be modified anymore.
// 
// A dedup pool allocator has significantly less memory overhead than
// calling directly pub_tool_mallocfree.h if the deduplication factor
// is big. However, allocating an element incurs a cost for searching
// if an identical element is already in the pool.
//
// Note: the elements of the pool cannot be freed (at least currently).
// The only way to free the elements is to delete the pool allocator.
//--------------------------------------------------------------------


typedef  struct _DedupPoolAlloc  DedupPoolAlloc;

/* Create new DedupPoolAlloc, using given allocation and free function.
   Alloc fn must not fail (that is, if it returns it must have succeeded.)
   poolSzB is the (minimum) size in bytes of the pool of elements allocated
   with alloc. 
   eltAlign is the minimum required alignement for the elements allocated
   from the DedupPoolAlloc. */
extern DedupPoolAlloc* VG_(newDedupPA) ( SizeT  poolSzB,
                                       SizeT  eltAlign,
                                       void*  (*alloc)(const HChar*, SizeT),
                                       const  HChar* cc,
                                       void   (*free_fn)(void*) );

/* Allocates a new element from ddpa with eltSzB bytes to store elt. */
extern void* VG_(allocEltDedupPA) (DedupPoolAlloc *ddpa,
                                   SizeT eltSzB, const void *elt);


/* The Dedup Pool Allocator must maintain a data structure to avoid
   duplicates as long as new elements can be allocated from the pool.
   Once no new elements will be allocated, this dedup data structure
   can be released using VG_(freezeDedupPA). Once ddpa has been frozen,
   it is an error to call VG_(allocEltDedupPA). */
extern void VG_(freezeDedupPA) (DedupPoolAlloc *ddpa);

/* Free all memory associated with a DedupPoolAlloc. */
extern void VG_(deleteDedupPA) ( DedupPoolAlloc *ddpa);

#endif   // __PUB_TOOL_DEDUPPOOLALLOC_

/*--------------------------------------------------------------------*/
/*--- end                               pub_tool_deduppoolalloc.h  ---*/
/*--------------------------------------------------------------------*/
