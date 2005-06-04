
/*--------------------------------------------------------------------*/
/*--- A skiplist implementation.                      m_skiplist.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2002-2005 Jeremy Fitzhardinge
      jeremy@goop.org

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

/* 
   This file implements a generic skip-list type.

   Skip-lists are described in William Pugh, Skip Lists: A
   Probabilistic Alternative to Balanced Trees, CACM, 33(6):668-676,
   June 1990. 
   http://www.cs.unc.edu/~baruah/Teaching/2002f/HandOuts/skiplist-CACM.pdf

   Skip-lists are a randomized linked-list, where a node in the list
   has at least one "next" pointer, but may have more.  When
   traversing the list, the "higher" next pointers allow you to skip
   multiple list entries, allowing you to find the right place
   quickly.  

   On average, 1/2 the entries have one next pointer, 1/4 have 2, 1/8
   have 3, etc.  This means that the skiplist has the same search
   complexity as a balanced binary tree, but there is no need to
   rebalance or do any other structural changes.  The randomness also
   means that it is invulnerable to pathalogical workloads.

   Because the each node can be a different size, this implementation
   puts the SkipNode structure at the end of the allocation, with the
   node data starting at the beginning.

   low address ->+---------------+  ^
		 | list data     |  |
     key offset->:       key     : structure size
		 |               |  |
		 +---------------+  V
		 | struct        |
		 |   SkipNode    |
		 +- - - - - - - -+
		 | next pointers |
		 :               :


   When you declare the list with VG_SKIPLIST_INIT, you specify the
   structure for each list node, structure member you want to use as a
   key, and the function for comparing keys.

   Each node must be allocated with SkipNode_Alloc, which allocates
   enough space for the client data, the SkipNode structure, and the
   next pointers for this node.

   The helper functions data_of_node and node_of_data do the correct
   pointer arithmetic to sort all this out.  The SkipNode also has a
   magic number which is checked after each operation to make sure
   that we're really operating on a SkipNode.

   The first node of the list, the head node, is special.  It contains
   the maximum number of next pointers (SK_MAXHEIGHT).  It has no data
   associated with it, and it always compares less-than to all other
   nodes.  Because it has no data attached to it, it is always an
   error to try and use data_of_node on it.  To detect this problem,
   it has a different magic number from all other SkipNodes so that it
   won't be accidentally used.
 */

#include "core.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_skiplist.h"

#include <stdlib.h>

#define SKIPLIST_DEBUG	0

#define SK_MAXHEIGHT	20	/* 2^20 elements */
#define SKIPLIST_MAGIC		0x5b1ff872
#define SKIPLIST_HEAD_MAGIC	(~SKIPLIST_MAGIC)


#if SKIPLIST_DEBUG
#define inline
#endif /* SKIPLIST_DEBUG */

struct _SkipNode {
   UInt		magic;
   UShort	level;		/* level is the max level (level == 0 means 1 next pointer) */
   SkipNode	*next[0];
};


/* 
   Compute the height of a new node.  1/2 will be 1, 1/4 2, 1/8 3,
   etc.
 */
static inline Int get_height(void)
{
   UInt ret = 0;

   while((ret < SK_MAXHEIGHT - 1) && (random() & 1))
      ret++;

   return ret;
}

/* 
   Given a pointer to the node's data, return a pointer to the key.
 */
static inline void *key_of_data(const SkipList *l, void *d)
{
   return (void *)((Char *)d + l->keyoff);
}

/* 
   Given a pointer to the node's data, return the pointer to the SkipNode
   structure.  If the node has a bad magic number, it will die with an
   assertion failure.
 */
static inline SkipNode *node_of_data(const SkipList *l, const void *d)
{
   SkipNode *n = (SkipNode *)((Char *)d + l->size);

   if (SKIPLIST_DEBUG && n->magic != SKIPLIST_MAGIC)
      VG_(printf)("bad magic on node %p = %x (not %x)\n",
		  n, n->magic, SKIPLIST_MAGIC);

   vg_assert(n->magic == SKIPLIST_MAGIC);

   return n; 
}

/* 
   Given a SkipNode structure, return the pointer to the node's data.
 */
static inline void *data_of_node(const SkipList *l, const SkipNode *n)
{
   if (SKIPLIST_DEBUG && n->magic != SKIPLIST_MAGIC)
      VG_(printf)("bad magic on node %p = %x (not %x)\n",
		  n, n->magic, SKIPLIST_MAGIC);

   vg_assert(n->magic == SKIPLIST_MAGIC);
   return (void *)((Char *)n - l->size);
}

/* 
   Given a SkipNode structure, return the pointer to the node's key.
 */
static inline void *key_of_node(const SkipList *l, const SkipNode *n)
{
   return key_of_data(l, data_of_node(l, n));
}

static inline void validate_skiplist(const SkipList *l, const Char *where)
{
#if SKIPLIST_DEBUG
   const SkipNode *prev[SK_MAXHEIGHT];
   Int i;
   const SkipNode *n, *next;
   
   VG_(printf)("---------------- %s ----------------\n", where);

   if (l->head == NULL)
      return;

   for(i = 0; i <= l->head->level; i++) {
      VG_(printf)("l->head->next[%d]=%p\n",
		  i, l->head->next[i]);
      prev[i] = l->head->next[i];
   }

   for(n = l->head->next[0]; n != NULL; n = next) {
      next = n->next[0];

      VG_(printf)("n=%p next=%p, n->level=%d k=%s\n",
		  n, next, n->level, (*l->strkey)(key_of_node(l, n)));
      for(i = 0; i <= n->level; i++) {
	 VG_(printf)("  n->next[%d] = %p\n",
		     i, n->next[i]);
	 VG_(printf)("  prev[%d] = %p\n",
		     i, prev[i]);
      }
      
      vg_assert(l->head->level >= n->level);

      for(i = 0; i <= n->level; i++)
	 vg_assert(prev[i] == n);

      for(i = 0; i <= n->level; i++)
	 prev[i] = n->next[i];

      vg_assert(next == NULL || (l->cmp)(key_of_node(l, n), key_of_node(l, next)) < 0);
   }
#endif	/* SKIPLIST_DEBUG */
}

void *VG_(SkipNode_Alloc)(const SkipList *l)
{
   UInt size;
   Int h;
   SkipNode *n;
   Char *ret;

   h = get_height();

   size = l->size;
   size += sizeof(SkipNode) + (h+1)*sizeof(SkipNode *);

   if (l->arena == -1)
      *(Short *)&l->arena = VG_AR_TOOL;

   ret = VG_(arena_malloc)(l->arena, size);

   if (ret == NULL)
      return NULL;

   n = (SkipNode *)(ret + l->size);
   n->level = h;
   n->magic = SKIPLIST_MAGIC;

   VG_(memset)(n->next, 0, sizeof(n->next[0]) * (h+1));

   return ret;
}

void VG_(SkipNode_Free)(const SkipList *l, void *p)
{
   if (SKIPLIST_DEBUG) {
      SkipNode *n = node_of_data(l, p);

      VG_(printf)("SkipNode_Free: freeing %p (node %p)\n",
		  p, n);
      n->magic = 0x55ffaabb;
   }
   VG_(arena_free)(l->arena, p);
}

void *VG_(SkipNode_First)(const SkipList *l)
{
   SkipNode *n = l->head ? l->head->next[0] : NULL;

   if (n == NULL)
      return NULL;
   else
      return data_of_node(l, n);
}

void *VG_(SkipNode_Next)(const SkipList *l, void *data)
{
   SkipNode *n = node_of_data(l, data);
   
   n = n->next[0];

   if (n == NULL)
      return NULL;

   return data_of_node(l, n);
}



static Int cmp(const SkipList *l, SkipNode *n, void *k2)
{
   void *k1 = key_of_node(l, n);

   if (k1 == k2)
      return 0;

   if (l->head == n)
      return -1;

   return (l->cmp)(k1, k2);
}

/* Search the list for k; it either returns the k if it exists, or the
   one before if not.  */
static SkipNode *SkipList__Find(const SkipList *l, void *k, SkipNode **prevs)
{
   SkipNode *n;
   Int lvl;

   if (SKIPLIST_DEBUG)
      VG_(printf)("SkipList__Find: finding %s\n", (*l->strkey)(k));

   validate_skiplist(l, "SkipList__Find");

   if (l->head == NULL)
      return NULL;

   for(lvl = l->head->level, n = l->head; lvl >= 0; lvl--) {
      while(n->next[lvl] != NULL && cmp(l, n->next[lvl], k) < 0) {
	 if (SKIPLIST_DEBUG)
	    VG_(printf)("SkipList__Find: n=%p n->next[%d]=%p\n",
			n, lvl, n->next[lvl]);
	 n = n->next[lvl];
      }
      if (prevs)
	 prevs[lvl] = n;
   }

   /* XXX Is there a cleaner way of getting this? 
      
      If we get an exact match, return it.
      If we get the head, return NULL.
      Otherwise return the one before where the hit would be.
    */
   if (n->next[0] != NULL && cmp(l, n->next[0], k) == 0)
      n =  n->next[0];
   if (n == l->head)
      n = NULL;

   if (SKIPLIST_DEBUG) {

      VG_(printf)("SkipList__Find returning node %p\n", n);

      if (n == NULL) {
	 SkipNode *nn;

	 for(nn = l->head->next[0]; nn != NULL; nn = nn->next[0])
	    vg_assert(cmp(l, nn, k) != 0);
      } else
	 vg_assert(cmp(l, n, k) <= 0);
   }

   return n;
}

/* Return list element which is <= k, or NULL if there is none. */
void *VG_(SkipList_Find_Before)(const SkipList *l, void *k)
{
   SkipNode *n = SkipList__Find(l, k, NULL);

   if (n != NULL)
      return data_of_node(l, n);
   return NULL;
}

/* Return the list element which == k, or NULL if none */
void *VG_(SkipList_Find_Exact)(const SkipList *l, void *k)
{
   SkipNode *n = SkipList__Find(l, k, NULL);

   if (n != NULL && (l->cmp)(key_of_node(l, n), k) == 0)
      return data_of_node(l, n);
   return NULL;
}

/* Return the list element which is >= k, or NULL if none */
void *VG_(SkipList_Find_After)(const SkipList *l, void *k)
{
   SkipNode *n = SkipList__Find(l, k, NULL);

   if (n != NULL && (l->cmp)(key_of_node(l, n), k) < 0)
      n = n->next[0];

   if (n != NULL)
      return data_of_node(l, n);

   return NULL;
}

void VG_(SkipList_Insert)(SkipList *l, void *data)
{
   SkipNode *update[SK_MAXHEIGHT];
   SkipNode *n, *nn;
   void *k = key_of_data(l, data);
   Int i;

   if (SKIPLIST_DEBUG)
      VG_(printf)("inserting node %p, key %s, height %d\n",
		  data, (*l->strkey)(key_of_data(l, data)), node_of_data(l, data)->level);

   validate_skiplist(l, "SkipList_Insert before");

   if (l->head == NULL) {
      Int size = sizeof(SkipNode) + sizeof(SkipNode *) * SK_MAXHEIGHT;

      if (l->arena == -1)
	 *(Short *)&l->arena = VG_AR_TOOL;
      
      l->head = VG_(arena_malloc)(l->arena, size);
      VG_(memset)(l->head, 0, size);

      l->head->magic = SKIPLIST_HEAD_MAGIC;
      l->head->level = 0;
   }

   n = node_of_data(l, data);

   /* update size of head's next vector to fit this new node */
   vg_assert(l->head != NULL);
   if (l->head->level < n->level) {
      for(i = l->head->level+1; i <= n->level; i++)
	 l->head->next[i] = NULL;
      l->head->level = n->level;
   }

   /* Look for the node, but we're mostly interested in setting
      "update", which is the set of previous nodes with next pointers
      we need to fix up. */
   nn = SkipList__Find(l, k, update);
   
   /* check the new entry is unique */
   vg_assert(nn == NULL || (l->cmp)(key_of_node(l, nn), k) != 0);

   /* update the previous node's next pointers */
   for(i = 0; i <= n->level; i++) {
      n->next[i] = update[i]->next[i];
      update[i]->next[i] = n;
   }

   validate_skiplist(l, "SkipList_Insert after");
}

void *VG_(SkipList_Remove)(SkipList *l, void *k)
{
   SkipNode *update[SK_MAXHEIGHT];
   SkipNode *n;
   Int i;
   
   validate_skiplist(l, "SkipList_Remove before");

   n = SkipList__Find(l, k, update);
   if (n == NULL)
      return NULL;

   vg_assert((l->cmp)(k, key_of_node(l, n)) == 0);

   for(i = 0; i <= n->level; i++) {
      update[i]->next[i] = n->next[i];
      n->next[i] = NULL;
   }

   validate_skiplist(l, "SkipList_Remove after");

   return data_of_node(l, n);
}


/* --------------------------------------------------
   Comparison functions
   -------------------------------------------------- */
Int VG_(cmp_Int)(const void *v1, const void *v2)
{
   Int a = *(const Int *)v1;
   Int b = *(const Int *)v2;

   if (a < b)
      return -1;
   if (a == b)
      return 0;
   return 1;
}

Int VG_(cmp_UInt)(const void *v1, const void *v2)
{
   UInt a = *(const UInt *)v1;
   UInt b = *(const UInt *)v2;

   if (a < b)
      return -1;
   if (a == b)
      return 0;
   return 1;
}

Int VG_(cmp_Addr)(const void *v1, const void *v2)
{
   Addr a = *(const Addr *)v1;
   Addr b = *(const Addr *)v2;

   if (a < b)
      return -1;
   if (a == b)
      return 0;
   return 1;
}

Int VG_(cmp_string)(const void *v1, const void *v2)
{
   const Char *a = *(const Char **)v1;
   const Char *b = *(const Char **)v2;

   return VG_(strcmp)(a, b);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

