
/*--------------------------------------------------------------------*/
/*--- An ordered set implemented using an AVL tree.       m_oset.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2013 Nicholas Nethercote
      njn@valgrind.org

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

//----------------------------------------------------------------------
// This file is based on:
//
//   ANSI C Library for maintainance of AVL Balanced Trees
//   (C) 2000 Daniel Nagy, Budapest University of Technology and Economics
//   Released under GNU General Public License (GPL) version 2
//----------------------------------------------------------------------

// This file implements a generic ordered set using an AVL tree.
//
// Each node in the tree has two parts.  
// - First is the AVL metadata, which is three words: a left pointer, a
//   right pointer, and a word containing balancing information and a
//   "magic" value which provides some checking that the user has not
//   corrupted the metadata.  So the overhead is 12 bytes on 32-bit
//   platforms and 24 bytes on 64-bit platforms.
// - Second is the user's data.  This can be anything.  Note that because it
//   comes after the metadata, it will only be word-aligned, even if the
//   user data is a struct that would normally be doubleword-aligned.
//
// AvlNode* node -> +---------------+  V
//                  | struct        |
//                  |   AvlNode     |
// void* element -> +---------------+  ^
//                  | element       |  |
//      keyOff ->   | key           | elemSize
//                  +---------------+  v
//
// Users have to allocate AvlNodes with OSetGen_AllocNode(), which allocates
// space for the metadata.
//
// The terminology used throughout this file:
// - a "node", usually called "n", is a pointer to the metadata.
// - an "element", usually called "e", is a pointer to the user data.
// - a "key", usually called "k", is a pointer to a key.
//
// The helper functions elem_of_node and node_of_elem do the pointer
// arithmetic to switch between the node and the element.  The node magic is
// checked after each operation to make sure that we're really operating on
// an AvlNode.
//
// Each tree also has an iterator.  Note that we cannot use the iterator
// internally within this file (eg. we could implement OSetGen_Size() by
// stepping through with the iterator and counting nodes) because it's
// non-reentrant -- the user might be using it themselves, and the
// concurrent uses would screw things up.

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_oset.h"
#include "pub_core_poolalloc.h"

/*--------------------------------------------------------------------*/
/*--- Types and constants                                          ---*/
/*--------------------------------------------------------------------*/

typedef struct _OSetNode OSetNode;

// Internal names for the OSet types.
typedef OSet     AvlTree;
typedef OSetNode AvlNode;

// The padding ensures that magic is right at the end of the node,
// regardless of the machine's word size, so that any overwrites will be
// detected earlier.
struct _OSetNode {
   AvlNode* left;
   AvlNode* right;
   Char     balance;
   Char     padding[sizeof(void*)-sizeof(Char)-sizeof(Short)];
   Short    magic;
};

#define STACK_MAX    32    // At most 2**32 entries can be iterated over
#define OSET_MAGIC   0x5b1f

// An OSet (AVL tree).  If cmp is NULL, the key must be a UWord, and must
// be the first word in the element.  If cmp is set, arbitrary keys in
// arbitrary positions can be used.
struct _OSet {
   SizeT       keyOff;     // key offset
   OSetCmp_t   cmp;        // compare a key and an element, or NULL
   OSetAlloc_t alloc;      // allocator
   const HChar* cc;        // cc for allocator
   OSetFree_t  free;       // deallocator
   PoolAlloc*  node_pa;    // (optional) pool allocator for nodes.
   SizeT       maxEltSize; // for node_pa, must be > 0. Otherwise unused.
   Word        nElems;     // number of elements in the tree
   AvlNode*    root;       // root node

   AvlNode*    nodeStack[STACK_MAX];   // Iterator node stack
   Int          numStack[STACK_MAX];   // Iterator num stack
   Int         stackTop;               // Iterator stack pointer, one past end
};

/*--------------------------------------------------------------------*/
/*--- Helper operations                                            ---*/
/*--------------------------------------------------------------------*/

// Given a pointer to the node's element, return the pointer to the AvlNode
// structure.  If the node has a bad magic number, it will die with an
// assertion failure.
static inline
AvlNode* node_of_elem(const void *elem)
{
   AvlNode* n = (AvlNode*)((Addr)elem - sizeof(AvlNode));
   vg_assert2(n->magic == OSET_MAGIC,
              "bad magic on node %p = %x (expected %x)\n"
              "possible causes:\n"
              " - node not allocated with VG_(OSetGen_AllocNode)()?\n"
              " - node metadata corrupted by underwriting start of element?\n",
              n, n->magic, OSET_MAGIC);
   return n;
}

// Given an AvlNode, return the pointer to the element.
static inline
void* elem_of_node(const AvlNode *n)
{
   vg_assert2(n->magic == OSET_MAGIC,
              "bad magic on node %p = %x (expected %x)\n"
              "possible causes:\n"
              " - node metadata corrupted by overwriting end of element?\n",
              n, n->magic, OSET_MAGIC);
   return (void*)((Addr)n + sizeof(AvlNode));
}

// Like elem_of_node, but no magic checking.
static inline
void* elem_of_node_no_check(const AvlNode *n)
{
   return (void*)((Addr)n + sizeof(AvlNode));
}

static inline
void* slow_key_of_node(AvlTree* t, AvlNode* n)
{
   return (void*)((Addr)elem_of_node(n) + t->keyOff);
}

static inline
void* fast_key_of_node(AvlNode* n)
{
   return elem_of_node(n);
}

// Compare the first word of each element.  Inlining is *crucial*.
static inline Word fast_cmp(const void* k, const AvlNode* n)
{
   UWord w1 = *(const UWord*)k;
   UWord w2 = *(const UWord*)elem_of_node(n);
   // In previous versions, we tried to do this faster by doing
   // "return w1 - w2".  But it didn't work reliably, because the
   // complete result of subtracting two N-bit numbers is an N+1-bit
   // number, and what the caller is interested in is the sign of
   // the complete N+1-bit result.  The branching version is slightly
   // slower, but safer and easier to understand.
   if (w1 > w2) return  1;
   if (w1 < w2) return -1;
   return 0;
}

// Compare a key and an element.  Inlining is *crucial*.
static 
inline Word slow_cmp(const AvlTree* t, const void* k, const AvlNode* n)
{
   return t->cmp(k, elem_of_node(n));
}


// Swing to the left.   Warning: no balance maintainance.
static void avl_swl ( AvlNode** root )
{
   AvlNode* a = *root;
   AvlNode* b = a->right;
   *root    = b;
   a->right = b->left;
   b->left  = a;
}

// Swing to the right.  Warning: no balance maintainance.
static void avl_swr ( AvlNode** root )
{
   AvlNode* a = *root;
   AvlNode* b = a->left;
   *root    = b;
   a->left  = b->right;
   b->right = a;
}

// Balance maintainance after especially nasty swings.
static void avl_nasty ( AvlNode* root )
{
   switch (root->balance) {
   case -1:
      root->left->balance  = 0;
      root->right->balance = 1;
      break;
   case 1:
      root->left->balance  =-1;
      root->right->balance = 0;
      break;
   case 0:
      root->left->balance  = 0;
      root->right->balance = 0;
   }
   root->balance = 0;
}


// Clear the iterator stack.
static void stackClear(AvlTree* t)
{
   Int i;
   vg_assert(t);
   for (i = 0; i < STACK_MAX; i++) {
      t->nodeStack[i] = NULL;
      t->numStack[i]  = 0;
   }
   t->stackTop = 0;
}

// Push onto the iterator stack.
static inline void stackPush(AvlTree* t, AvlNode* n, Int i)
{
   vg_assert(t->stackTop < STACK_MAX);
   vg_assert(1 <= i && i <= 3);
   t->nodeStack[t->stackTop] = n;
   t-> numStack[t->stackTop] = i;
   t->stackTop++;
}

// Pop from the iterator stack.
static inline Bool stackPop(AvlTree* t, AvlNode** n, Int* i)
{
   vg_assert(t->stackTop <= STACK_MAX);

   if (t->stackTop > 0) {
      t->stackTop--;
      *n = t->nodeStack[t->stackTop];
      *i = t-> numStack[t->stackTop];
      vg_assert(1 <= *i && *i <= 3);
      t->nodeStack[t->stackTop] = NULL;
      t-> numStack[t->stackTop] = 0;
      return True;
   } else {
      return False;
   }
}

/*--------------------------------------------------------------------*/
/*--- Creating and destroying AvlTrees and AvlNodes                ---*/
/*--------------------------------------------------------------------*/

// The underscores avoid GCC complaints about overshadowing global names.
AvlTree* VG_(OSetGen_Create)(PtrdiffT _keyOff, OSetCmp_t _cmp,
                             OSetAlloc_t _alloc, const HChar* _cc,
                             OSetFree_t _free)
{
   AvlTree* t;

   // Check the padding is right and the AvlNode is the expected size.
   vg_assert(sizeof(AvlNode) == 3*sizeof(void*));

   // Sanity check args
   vg_assert(_alloc);
   vg_assert(_free);
   if (!_cmp) vg_assert(0 == _keyOff);    // If no cmp, offset must be zero

   t           = _alloc(_cc, sizeof(AvlTree));
   t->keyOff   = _keyOff;
   t->cmp      = _cmp;
   t->alloc    = _alloc;
   t->cc       = _cc;
   t->free     = _free;
   t->node_pa  = NULL;
   t->maxEltSize = 0; // Just in case it would be wrongly used.
   t->nElems   = 0;
   t->root     = NULL;
   stackClear(t);

   return t;
}

AvlTree* VG_(OSetGen_Create_With_Pool)(PtrdiffT _keyOff, OSetCmp_t _cmp,
                                       OSetAlloc_t _alloc, const HChar* _cc,
                                       OSetFree_t _free,
                                       SizeT _poolSize,
                                       SizeT _maxEltSize)
{
   AvlTree* t;

   t = VG_(OSetGen_Create) (_keyOff, _cmp,
                            _alloc, _cc,
                            _free);

   vg_assert (_poolSize > 0);
   vg_assert (_maxEltSize > 0);
   t->maxEltSize = _maxEltSize;
   t->node_pa = VG_(newPA)(sizeof(AvlNode) 
                           + VG_ROUNDUP(_maxEltSize, sizeof(void*)),
                           _poolSize,
                           t->alloc,
                           _cc,
                           t->free);
   VG_(addRefPA) (t->node_pa);

   return t;
}

AvlTree* VG_(OSetGen_EmptyClone) (AvlTree* os)
{
   AvlTree* t;

   vg_assert(os);

   t           = os->alloc(os->cc, sizeof(AvlTree));
   t->keyOff   = os->keyOff;
   t->cmp      = os->cmp;
   t->alloc    = os->alloc;
   t->cc       = os->cc;
   t->free     = os->free;
   t->node_pa  = os->node_pa;
   if (t->node_pa)
      VG_(addRefPA) (t->node_pa);
   t->maxEltSize = os->maxEltSize;
   t->nElems   = 0;
   t->root     = NULL;
   stackClear(t);

   return t;
}

AvlTree* VG_(OSetWord_Create)(OSetAlloc_t _alloc, const HChar* _cc, 
                              OSetFree_t _free)
{
   return VG_(OSetGen_Create)(/*keyOff*/0, /*cmp*/NULL, _alloc, _cc, _free);
}

// Destructor, frees up all memory held by remaining nodes.
void VG_(OSetGen_Destroy)(AvlTree* t)
{
   Bool has_node_pa;
   vg_assert(t);

   has_node_pa = t->node_pa != NULL;

   /*
    * If we are the only remaining user of this pool allocator, release all
    * the elements by deleting the pool allocator. That's more efficient than
    * deleting tree nodes one by one.
    */
   if (!has_node_pa || VG_(releasePA)(t->node_pa) > 0) {
      AvlNode* n = NULL;
      Int i = 0;
      Word sz = 0;
   
      stackClear(t);
      if (t->root)
         stackPush(t, t->root, 1);

      /* Free all the AvlNodes.  This is a post-order traversal, because we */
      /* must free all children of a node before the node itself. */
      while (stackPop(t, &n, &i)) {
         switch (i) {
         case 1: 
            stackPush(t, n, 2);
            if (n->left)  stackPush(t, n->left, 1);
            break;
         case 2: 
            stackPush(t, n, 3);
            if (n->right) stackPush(t, n->right, 1);
            break;
         case 3:
            if (has_node_pa)
               VG_(freeEltPA) (t->node_pa, n);
            else
               t->free(n);
            sz++;
            break;
         }
      }
      vg_assert(sz == t->nElems);
   }

   /* Free the AvlTree itself. */
   t->free(t);
}

void VG_(OSetWord_Destroy)(AvlTree* t)
{
   VG_(OSetGen_Destroy)(t);
}

// Allocate and initialise a new node.
void* VG_(OSetGen_AllocNode)(AvlTree* t, SizeT elemSize)
{
   AvlNode* n;
   Int nodeSize = sizeof(AvlNode) + elemSize;
   vg_assert(elemSize > 0);
   if (t->node_pa) {
      vg_assert(elemSize <= t->maxEltSize);
      n = VG_(allocEltPA) (t->node_pa);
   } else {
      n = t->alloc( t->cc, nodeSize );
   }
   VG_(memset)(n, 0, nodeSize);
   n->magic = OSET_MAGIC;
   return elem_of_node(n);
}

void VG_(OSetGen_FreeNode)(AvlTree* t, void* e)
{
   if (t->node_pa)
      VG_(freeEltPA) (t->node_pa, node_of_elem (e));
   else
      t->free( node_of_elem(e) );
}

/*--------------------------------------------------------------------*/
/*--- Insertion                                                    ---*/
/*--------------------------------------------------------------------*/

static inline Word cmp_key_root(AvlTree* t, AvlNode* n)
{
   return t->cmp
          ? slow_cmp(t, slow_key_of_node(t, n), t->root)
          : fast_cmp(   fast_key_of_node(   n), t->root);
}

// Insert element e into the non-empty AVL tree t.
// Returns True if the depth of the tree has grown.
static Bool avl_insert(AvlTree* t, AvlNode* n)
{
   Word cmpres = cmp_key_root(t, n);

   if (cmpres < 0) {
      // Insert into the left subtree.
      if (t->root->left) {
         // Only need to set the used fields in the subtree.
         AvlTree left_subtree;
         left_subtree.root   = t->root->left;
         left_subtree.cmp    = t->cmp;
         left_subtree.keyOff = t->keyOff;
         if (avl_insert(&left_subtree, n)) {
             switch (t->root->balance--) {
             case 1: return False;
             case 0: return True;
             }
             if (t->root->left->balance < 0) {
                avl_swr(&(t->root));
                t->root->balance = 0;
                t->root->right->balance = 0;
             } else {
                avl_swl(&(t->root->left));
                avl_swr(&(t->root));
                avl_nasty(t->root);
             }
         } else {
            t->root->left=left_subtree.root;
         }
         return False;
      } else {
         t->root->left = n;
         if (t->root->balance--) return False;
         return True;
      }

   } else if (cmpres > 0) {
      // Insert into the right subtree
      if (t->root->right) {
         // Only need to set the used fields in the subtree.
         AvlTree right_subtree;
         right_subtree.root   = t->root->right;
         right_subtree.cmp    = t->cmp;
         right_subtree.keyOff = t->keyOff;
         if (avl_insert(&right_subtree, n)) {
            switch (t->root->balance++) {
            case -1: return False;
            case  0: return True;
            }
            if (t->root->right->balance > 0) {
               avl_swl(&(t->root));
               t->root->balance = 0;
               t->root->left->balance = 0;
            } else {
               avl_swr(&(t->root->right));
               avl_swl(&(t->root));
               avl_nasty(t->root);
            }
         } else {
            t->root->right=right_subtree.root;
         }
         return False;
      } else {
         t->root->right = n;
         if (t->root->balance++) return False;
         return True;
      }

   } else {
      vg_assert2(0, "OSet{Word,Gen}_Insert: duplicate element added");
   }
}

// Insert element e into the AVL tree t.  This is just a wrapper for
// avl_insert() which doesn't return a Bool.
void VG_(OSetGen_Insert)(AvlTree* t, void* e)
{
   AvlNode* n;

   vg_assert(t);

   // Initialise.  Even though OSetGen_AllocNode zeroes these fields, 
   // we should do it again in case a node is removed and then 
   // re-added to the tree.
   n          = node_of_elem(e);
   n->left    = 0;
   n->right   = 0;
   n->balance = 0;

   // Insert into an empty tree
   if (!t->root) {
      t->root = n;
   } else {
      avl_insert(t, n);
   }

   t->nElems++;
   t->stackTop = 0;  // So the iterator can't get out of sync
}

void VG_(OSetWord_Insert)(AvlTree* t, UWord val)
{
   Word* node = VG_(OSetGen_AllocNode)(t, sizeof(UWord));
   *node = val;
   VG_(OSetGen_Insert)(t, node);
}

/*--------------------------------------------------------------------*/
/*--- Lookup                                                       ---*/
/*--------------------------------------------------------------------*/

// Find the *node* in t matching k, or NULL if not found.
static AvlNode* avl_lookup(const AvlTree* t, const void* k)
{
   Word     cmpres;
   AvlNode* curr = t->root;

   if (t->cmp) {
      // General case
      while (True) {
         if (curr == NULL) return NULL;
         cmpres = slow_cmp(t, k, curr);
         if      (cmpres < 0) curr = curr->left;
         else if (cmpres > 0) curr = curr->right;
         else return curr;
      }
   } else {
      // Fast-track special case.  We use the no-check version of
      // elem_of_node because it saves about 10% on lookup time.  This
      // shouldn't be very dangerous because each node will have been
      // checked on insertion.
      UWord w1 = *(const UWord*)k;
      UWord w2;
      while (True) {
         if (curr == NULL) return NULL;
         w2 = *(UWord*)elem_of_node_no_check(curr);
         if      (w1 < w2) curr = curr->left;
         else if (w1 > w2) curr = curr->right;
         else return curr;
      }
   }
}

// Find the *element* in t matching k, or NULL if not found.
void* VG_(OSetGen_Lookup)(const AvlTree* t, const void* k)
{
   AvlNode* n;
   vg_assert(t);
   n = avl_lookup(t, k);
   return ( n ? elem_of_node(n) : NULL );
}

// Find the *element* in t matching k, or NULL if not found;  use the given
// comparison function rather than the standard one.
void* VG_(OSetGen_LookupWithCmp)(AvlTree* t, const void* k, OSetCmp_t cmp)
{
   // Save the normal one to the side, then restore once we're done.
   void* e;
   OSetCmp_t tmpcmp;
   vg_assert(t);
   tmpcmp = t->cmp;
   t->cmp = cmp;
   e = VG_(OSetGen_Lookup)(t, k);
   t->cmp = tmpcmp;
   return e;
}

// Is there an element matching k?
Bool VG_(OSetGen_Contains)(const AvlTree* t, const void* k)
{
   return (NULL != VG_(OSetGen_Lookup)(t, k));
}

Bool VG_(OSetWord_Contains)(AvlTree* t, UWord val)
{
   return (NULL != VG_(OSetGen_Lookup)(t, &val));
}

/*--------------------------------------------------------------------*/
/*--- Deletion                                                     ---*/
/*--------------------------------------------------------------------*/

static Bool avl_removeroot(AvlTree* t);

// Remove an already-selected node n from the AVL tree t.
// Returns True if the depth of the tree has shrunk.
static Bool avl_remove(AvlTree* t, AvlNode* n)
{
   Bool ch;
   Word cmpres = cmp_key_root(t, n);

   if (cmpres < 0) {
      AvlTree left_subtree;
      // Remove from the left subtree
      vg_assert(t->root->left);
      // Only need to set the used fields in the subtree.
      left_subtree.root   = t->root->left;
      left_subtree.cmp    = t->cmp;
      left_subtree.keyOff = t->keyOff;
      ch = avl_remove(&left_subtree, n);
      t->root->left = left_subtree.root;
      if (ch) {
         switch (t->root->balance++) {
         case -1: return True;
         case  0: return False;
         }
         switch (t->root->right->balance) {
         case 0:
            avl_swl(&(t->root));
            t->root->balance = -1;
            t->root->left->balance = 1;
            return False;
         case 1:
            avl_swl(&(t->root));
            t->root->balance = 0;
            t->root->left->balance = 0;
            return True;
         }
         avl_swr(&(t->root->right));
         avl_swl(&(t->root));
         avl_nasty(t->root);
         return True;
      } else {
         return False;
      }
   
   } else if (cmpres > 0) {
      // Remove from the right subtree
      AvlTree right_subtree;
      vg_assert(t->root->right);
      // Only need to set the used fields in the subtree.
      right_subtree.root   = t->root->right;
      right_subtree.cmp    = t->cmp;
      right_subtree.keyOff = t->keyOff;
      ch = avl_remove(&right_subtree, n);
      t->root->right = right_subtree.root;
      if (ch) {
         switch (t->root->balance--) {
         case 1: return True;
         case 0: return False;
         }
         switch (t->root->left->balance) {
         case 0:
            avl_swr(&(t->root));
            t->root->balance = 1;
            t->root->right->balance = -1;
            return False;
         case -1:
            avl_swr(&(t->root));
            t->root->balance = 0;
            t->root->right->balance = 0;
            return True;
         }
         avl_swl(&(t->root->left));
         avl_swr(&(t->root));
         avl_nasty(t->root);
         return True;
      } else {
         return False;
      }

   } else {
      // Found the node to be removed.
      vg_assert(t->root == n);
      return avl_removeroot(t);
   }
}

// Remove the root of the AVL tree t.
// Returns True if the depth of the tree has shrunk.
static Bool avl_removeroot(AvlTree* t)
{
   Bool     ch;
   AvlNode* n;

   if (!t->root->left) {
      if (!t->root->right) {
         t->root = NULL;
         return True;
      }
      t->root = t->root->right;
      return True;
   }
   if (!t->root->right) {
      t->root = t->root->left;
      return True;
   }
   if (t->root->balance < 0) {
      // Remove from the left subtree
      n = t->root->left;
      while (n->right) n = n->right;
   } else {
      // Remove from the right subtree
      n = t->root->right;
      while (n->left) n = n->left;
   }
   ch = avl_remove(t, n);
   n->left    = t->root->left;
   n->right   = t->root->right;
   n->balance = t->root->balance;
   t->root    = n;
   if (n->balance == 0) return ch;
   return False;
}

// Remove and return the element matching the key 'k', or NULL 
// if not present.
void* VG_(OSetGen_Remove)(AvlTree* t, const void* k)
{
   // Have to find the node first, then remove it.
   AvlNode* n = avl_lookup(t, k);
   if (n) {
      avl_remove(t, n);
      t->nElems--;
      t->stackTop = 0;     // So the iterator can't get out of sync
      return elem_of_node(n);
   } else {
      return NULL;
   }
}

Bool VG_(OSetWord_Remove)(AvlTree* t, UWord val)
{
   void* n = VG_(OSetGen_Remove)(t, &val);
   if (n) {
      VG_(OSetGen_FreeNode)(t, n);
      return True;
   } else {
      return False;
   }
}

/*--------------------------------------------------------------------*/
/*--- Iterator                                                     ---*/
/*--------------------------------------------------------------------*/

// The iterator is implemented using in-order traversal with an explicit
// stack, which lets us do the traversal one step at a time and remember
// where we are between each call to OSetGen_Next().

void VG_(OSetGen_ResetIter)(AvlTree* t)
{
   vg_assert(t);
   stackClear(t);
   if (t->root)
      stackPush(t, t->root, 1);
}

void VG_(OSetWord_ResetIter)(AvlTree* t)
{
   VG_(OSetGen_ResetIter)(t);
}

void* VG_(OSetGen_Next)(AvlTree* t)
{
   Int i = 0;
   OSetNode* n = NULL;
   
   vg_assert(t);

   // This in-order traversal requires each node to be pushed and popped
   // three times.  These could be avoided by updating nodes in-situ on the
   // top of the stack, but the push/pop cost is so small that it's worth
   // keeping this loop in this simpler form.
   while (stackPop(t, &n, &i)) {
      switch (i) {
      case 1: case_1:
         stackPush(t, n, 2);
         /* if (n->left)  stackPush(t, n->left, 1); */
         if (n->left) { n = n->left; goto case_1; }
         break;
      case 2: 
         stackPush(t, n, 3);
         return elem_of_node(n);
      case 3:
         /* if (n->right) stackPush(t, n->right, 1); */
         if (n->right) { n = n->right; goto case_1; }
         break;
      }
   }

   // Stack empty, iterator is exhausted, return NULL
   return NULL;
}

Bool VG_(OSetWord_Next)(AvlTree* t, UWord* val)
{
   UWord* n = VG_(OSetGen_Next)(t);
   if (n) {
      *val = *n;
      return True;
   } else {
      return False;
   }
}

// set up 'oset' for iteration so that the first key subsequently
// produced VG_(OSetGen_Next) is the smallest key in the map 
// >= start_at.  Naturally ">=" is defined by the comparison 
// function supplied to VG_(OSetGen_Create).
void VG_(OSetGen_ResetIterAt)(AvlTree* oset, const void* k)
{
   AvlNode *t;
   Word    cmpresS; /* signed */
   UWord   cmpresU; /* unsigned */

   vg_assert(oset);
   stackClear(oset);

   if (!oset->root)
      return;

   // We need to do regular search and fill in the stack.
   t = oset->root;

   while (True) {
      if (t == NULL) return;

      if (oset->cmp) {
         cmpresS = (Word)slow_cmp(oset, k, t);
      } else {
         cmpresS = fast_cmp(k, t);
      }

      /* Switch the sense of the comparison, since the comparison
         order of args (k vs t) above is opposite to that of the
         corresponding code in hg_wordfm.c. */
      if (cmpresS < 0) { cmpresS = 1; } 
      else if (cmpresS > 0) { cmpresS = -1; }

      if (cmpresS == 0) {
         // We found the exact key -- we are done.
         // The iteration should start with this node.
         stackPush(oset, t, 2);
         // The stack now looks like {2, 2, ... ,2, 2}
         return;
      }
      cmpresU = (UWord)cmpresS;
      cmpresU >>=/*unsigned*/ (8 * sizeof(cmpresU) - 1);
      vg_assert(cmpresU == 0 || cmpresU == 1);
      if (!cmpresU) {
         // Push this node only if we go to the left child.
         stackPush(oset, t, 2);
      }
      t = cmpresU==0 ? t->left : t->right;
   }
}

/*--------------------------------------------------------------------*/
/*--- Miscellaneous operations                                     ---*/
/*--------------------------------------------------------------------*/

Word VG_(OSetGen_Size)(const AvlTree* t)
{
   vg_assert(t);
   return t->nElems;
}

Word VG_(OSetWord_Size)(AvlTree* t)
{
   return VG_(OSetGen_Size)(t);
}

static void OSet_Print2( AvlTree* t, AvlNode* n,
                         HChar*(*strElem)(void *), Int p )
{
   // This is a recursive in-order traversal.
   Int q = p;
   if (NULL == n) return;
   if (n->right) OSet_Print2(t, n->right, strElem, p+1);
   while (q--) VG_(printf)(".. ");
   VG_(printf)("%s\n", strElem(elem_of_node(n)));
   if (n->left) OSet_Print2(t, n->left, strElem, p+1);
}

__attribute__((unused))
static void OSet_Print( AvlTree* t, const HChar *where,
                        HChar*(*strElem)(void *) )
{
   VG_(printf)("-- start %s ----------------\n", where);
   OSet_Print2(t, t->root, strElem, 0);
   VG_(printf)("-- end   %s ----------------\n", where);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
