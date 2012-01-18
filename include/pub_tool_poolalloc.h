
/*--------------------------------------------------------------------*/
/*--- A simple pool (memory) allocator.     pub_tool_poolalloc.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2012 OpenWorks LLP info@open-works.co.uk,
                           Philippe Waroquiers philippe.waroquiers@skynet.be

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

#ifndef __PUB_TOOL_GROUPALLOC_H
#define __PUB_TOOL_GROUPALLOC_H

//--------------------------------------------------------------------
// PURPOSE: Provides efficient allocation and free of elements of
// the same size.
// This pool allocator manages elements alloc/free by allocating
// "pools" of many elements from a lower level allocator (typically
// pub_tool_mallocfree.h).
// Single elements can then be allocated and released from these pools.
// A pool allocator is faster and has less memory overhead than
// calling directly pub_tool_mallocfree.h
// Note: the pools of elements are not freed, even if all the
// single elements have been freed. The only way to free the underlying
// pools of elements is to delete the pool allocator.
//--------------------------------------------------------------------


typedef  struct _PoolAlloc  PoolAlloc;

/* Create new PoolAlloc, using given allocation and free function, and
   for elements of the specified size.  Alloc fn must not fail (that
   is, if it returns it must have succeeded.) */
PoolAlloc* VG_(newPA) ( UWord  elemSzB,
                        UWord  nPerPool,
                        void*  (*alloc)(HChar*, SizeT),
                        HChar* cc,
                        void   (*free_fn)(void*) );


/* Free all memory associated with a PoolAlloc. */
extern void VG_(deletePA) ( PoolAlloc* pa);

/* Allocates an element from pa. */
extern void* VG_(allocEltPA) ( PoolAlloc* pa);

/* Free element of pa. */
extern void VG_(freeEltPA) ( PoolAlloc* pa, void* p);

/* A pool allocator can be shared between multiple data structures.
   For example, multiple OSet* can allocate/free nodes from the same
   pool allocator.
   The Pool Allocator provides support to use a ref counter
   to detect a pool allocator is not needed anymore.
   It is the caller responsibility to delete the PA if the ref counter
   drops to 0. In other words, this just helps the caller to manage
   the PA memory destruction but it does not fully manage it.
   Note that the usage of pool reference counting is optional. */

// VG_(addRefPA) indicates there is a new reference to pa.
extern void VG_(addRefPA) ( PoolAlloc* pa);

// VG_(releasePA) decrements the pa reference count and deletes the pa if that
// reference count has dropped to zero. Returns the new value of the reference
// count.
extern UWord VG_(releasePA) ( PoolAlloc* pa);

#endif   // __PUB_TOOL_POOLALLOC_

/*--------------------------------------------------------------------*/
/*--- end                                    pub_tool_poolalloc.h  ---*/
/*--------------------------------------------------------------------*/
