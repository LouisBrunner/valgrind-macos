
/*--------------------------------------------------------------------*/
/*--- A hash table implementation.            pub_tool_hashtable.h ---*/
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

#ifndef __PUB_TOOL_HASHTABLE_H
#define __PUB_TOOL_HASHTABLE_H

/* Generic type for a separately-chained hash table.  Via a kind of dodgy
   C-as-C++ style inheritance, tools can extend the VgHashNode type, so long
   as the first two fields match the sizes of these two fields.  Requires
   a bit of casting by the tool. */

// Problems with this data structure:
// - Separate chaining gives bad cache behaviour.  Hash tables with linear
//   probing give better cache behaviour.
// - It's not very abstract, eg. deleting nodes exposes more internals than
//   I'd like.

typedef
   struct _VgHashNode {
      struct _VgHashNode * next;
      UWord              key;
   }
   VgHashNode;

typedef struct _VgHashTable * VgHashTable;

/* Make a new table.  Allocates the memory with VG_(calloc)(), so can be freed
   with VG_(free)().  n_chains should be prime. */
extern VgHashTable VG_(HT_construct) ( UInt n_chains );

/* Count the number of nodes in a table. */
extern Int VG_(HT_count_nodes) ( VgHashTable table );

/* Add a node to the table. */
extern void VG_(HT_add_node) ( VgHashTable t, void* node );

/* Looks up a node in the hash table.  Also returns the address of the
   previous node's `next' pointer which allows it to be removed from the
   list later without having to look it up again.  */
extern void* VG_(HT_get_node) ( VgHashTable t, UWord key,
                                    /*OUT*/VgHashNode*** next_ptr );

/* Looks up a VgHashNode in the table.  Returns NULL if not found. */
extern void* VG_(HT_lookup) ( VgHashTable table, UWord key );

/* Removes a VgHashNode from the table.  Returns NULL if not found. */
extern void* VG_(HT_remove) ( VgHashTable table, UWord key );

/* Allocates an array of pointers to all the shadow chunks of malloc'd
   blocks.  Must be freed with VG_(free)(). */
extern VgHashNode** VG_(HT_to_array) ( VgHashTable t, /*OUT*/ UInt* n_shadows );

/* Returns first node that matches predicate `p', or NULL if none do.
   Extra arguments can be implicitly passed to `p' using `d' which is an
   opaque pointer passed to `p' each time it is called. */
extern void* VG_(HT_first_match) ( VgHashTable t,
                                   Bool (*p)(VgHashNode*, void*),
                                   void* d );

/* Applies a function f() once to each node.  Again, `d' can be used
   to pass extra information to the function. */
extern void VG_(HT_apply_to_all_nodes)( VgHashTable t,
                                        void (*f)(VgHashNode*, void*),
                                        void* d );

/* Reset the table's iterator to point to the first element. */
extern void VG_(HT_ResetIter) ( VgHashTable table );

/* Return the element pointed to by the iterator and move on to the next
   one.  Returns NULL if the last one has been passed, or if HT_ResetIter()
   has not been called previously. */
extern void* VG_(HT_Next) ( VgHashTable table );

/* Destroy a table. */
extern void VG_(HT_destruct) ( VgHashTable t );


#endif   // __PUB_TOOL_HASHTABLE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
