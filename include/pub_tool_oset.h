
/*--------------------------------------------------------------------*/
/*--- OSet: a fast data structure with no dups.    pub_tool_oset.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
      jseward@acm.org

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

#ifndef __PUB_TOOL_OSET_H
#define __PUB_TOOL_OSET_H

// This module implements an ordered set, a data structure with fast
// (eg. amortised log(n) or better) insertion, lookup and deletion of
// elements.  It does not allow duplicates, and will assert if you insert a
// duplicate to an OSet.
//
// The structure is totally generic.  The user provides the allocation and
// deallocation functions.  Also, each element has a key, which the lookup
// is done with.  The key may be the whole element (eg. in an OSet of
// integers, each integer serves both as an element and a key), or it may be
// only part of it (eg. if the key is a single field in a struct).  The user
// can provide a function that compares an element with a key;  this is very
// flexible, and with the right comparison function even a (non-overlapping)
// interval list can be created.  But the cost of calling a function for
// every comparison can be high during lookup.  If no comparison function is
// provided, we assume that keys are (signed or unsigned) words, and that
// the key is the first word in each element.  This fast comparison is
// suitable for an OSet of Ints, or an OSet containing structs where the
// first element is an Addr, for example.
//
// Each OSet also has an iterator, which makes it simple to traverse all the
// nodes in order.  Note that the iterator maintains state and so is
// non-reentrant.
//
// Note that once you insert an element into an OSet, if you modify any part
// of it looked at by your cmp() function, this may cause incorrect
// behaviour as the sorted order maintained will be wrong.

/*--------------------------------------------------------------------*/
/*--- Types                                                        ---*/
/*--------------------------------------------------------------------*/

typedef struct _OSet     OSet;
typedef struct _OSetNode OSetNode;

typedef Int   (*OSetCmp_t)   ( void* key, void* elem );
typedef void* (*OSetAlloc_t) ( SizeT szB );
typedef void  (*OSetFree_t)  ( void* p );

/*--------------------------------------------------------------------*/
/*--- Creating and destroying OSets and OSet members               ---*/
/*--------------------------------------------------------------------*/

// * Create: allocates an initialises the OSet.  Arguments:
//   - keyOff    The offset of the key within the element.
//   - elemSize  The size of the element.
//   - cmp       The comparison function between keys and elements, or NULL
//               if the OSet should use fast comparisons.
//   - alloc     The allocation function used for allocating the OSet itself;
//               it's also called for each invocation of VG_(OSet_AllocNode)().
//   - free      The deallocation function used by VG_(OSet_FreeNode)() and
//               VG_(OSet_Destroy)().
//
//   If cmp is NULL, keyOff must be zero.  This is checked.
//
// * Destroy: frees all nodes in the table, plus the memory used by
//   the table itself.
//
// * AllocNode: Allocate and zero memory for a node to go into the OSet.
//   Uses the alloc function given to VG_(OSet_Create)() to allocated a node
//   which is big enough for both an element and the OSet metadata.
//   Not all elements in one OSet have to be the same size.
//
//   Note that the element allocated will be at most word-aligned, which may
//   be less aligned than the element type would normally be.
//
// * FreeNode: Deallocate a node allocated with OSet_AllocNode().  Using
//   a deallocation function (such as VG_(free)()) directly will likely
//   lead to assertions in Valgrind's allocator.

extern OSet* VG_(OSet_Create)    ( OffT keyOff, OSetCmp_t cmp,
                                   OSetAlloc_t alloc, OSetFree_t free );
extern void  VG_(OSet_Destroy)   ( OSet* os );
extern void* VG_(OSet_AllocNode) ( OSet* os, SizeT elemSize );
extern void  VG_(OSet_FreeNode)  ( OSet* os, void* elem );

/*--------------------------------------------------------------------*/
/*--- Operations on OSets                                          ---*/
/*--------------------------------------------------------------------*/

// * Size: The number of elements in the set.
//
// * Contains: Determines if any element in the OSet matches the key.
//
// * Lookup: Returns a pointer to the element matching the key, if there is
//   one, otherwise returns NULL.
//
// * Insert: Inserts a new element into the list.  Note that 'elem' must
//   have been allocated using VG_(OSet_AllocNode)(), otherwise you will get
//   assertion failures about "bad magic".  Duplicates are forbidden, and
//   will also cause assertion failures.
//
// * Remove: Removes the element matching the key, if there is one.  Returns
//   NULL if no element matches the key.
//
// * ResetIter: Each OSet has an iterator.  This resets it to point to the
//   first element in the OSet.
// 
// * Next: Returns a pointer to the element pointed to by the OSet's
//   iterator, and advances the iterator by one;  the elements are visited
//   in order.  Or, returns NULL if the iterator has reached the OSet's end.
//   
//   You can thus iterate in order through an OSet like this:
//
//     VG_(OSet_ResetIter)(oset);
//     while ( (elem = VG_(OSet_Next)(oset)) ) {
//        ... do stuff with 'elem' ...
//     }
//
//   Note that iterators are cleared any time an element is inserted or
//   removed from the OSet, to avoid possible mayhem caused by the iterator
//   getting out of sync with the OSet's contents.  "Cleared" means that
//   they will return NULL if VG_(OSet_Next)() is called without an
//   intervening call to VG_(OSet_ResetIter)().

extern Int   VG_(OSet_Size)      ( OSet* os );
extern void  VG_(OSet_Insert)    ( OSet* os, void* elem );
extern Bool  VG_(OSet_Contains)  ( OSet* os, void* key  );
extern void* VG_(OSet_Lookup)    ( OSet* os, void* key  );
extern void* VG_(OSet_Remove)    ( OSet* os, void* key  );
extern void  VG_(OSet_ResetIter) ( OSet* os );
extern void* VG_(OSet_Next)      ( OSet* os );

#endif   // __PUB_TOOL_OSET_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
