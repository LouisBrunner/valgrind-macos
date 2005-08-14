
/*--------------------------------------------------------------------*/
/*--- An ordered set implemented using an AVL tree.       m_oset.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005 Nicholas Nethercote
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
//   corrupted the metadata.
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
// Users have to allocate AvlNodes with OSet_AllocNode(), which allocates
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
// internally within this file (eg. we could implement OSet_Size() by
// stepping through with the iterator and counting nodes) because it's
// non-reentrant -- the user might be using it themselves, and the
// concurrent uses would screw things up.

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_oset.h"

/*--------------------------------------------------------------------*/
/*--- Types and constants                                          ---*/
/*--------------------------------------------------------------------*/

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
   Char     padding[sizeof(void*)-3];
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
   OSetFree_t  free;       // deallocator
   Int         nElems;     // number of elements in the tree
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
              " - node not allocated with VG_(OSet_AllocNode)()?\n"
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
static inline Int fast_cmp(void* k, AvlNode* n)
{
   return ( *(Int*)k - *(Int*)elem_of_node(n) );
}

// Compare a key and an element.  Inlining is *crucial*.
static inline Int slow_cmp(AvlTree* t, void* k, AvlNode* n)
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
static void stackPush(AvlTree* t, AvlNode* n, Int i)
{
   vg_assert(t->stackTop < STACK_MAX);
   vg_assert(1 <= i && i <= 3);
   t->nodeStack[t->stackTop] = n;
   t-> numStack[t->stackTop] = i;
   t->stackTop++;
}

// Pop from the iterator stack.
static Bool stackPop(AvlTree* t, AvlNode** n, Int* i)
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
AvlTree* VG_(OSet_Create)(OffT _keyOff, OSetCmp_t _cmp,
                          OSetAlloc_t _alloc, OSetFree_t _free)
{
   AvlTree* t;

   // Check the padding is right and the AvlNode is the expected size.
   vg_assert(sizeof(AvlNode) == 3*sizeof(void*));

   // Sanity check args
   vg_assert(_alloc);
   vg_assert(_free);
   if (!_cmp) vg_assert(0 == _keyOff);    // If no cmp, offset must be zero

   t           = _alloc(sizeof(AvlTree));
   t->keyOff   = _keyOff;
   t->cmp      = _cmp;
   t->alloc    = _alloc;
   t->free     = _free;
   t->nElems   = 0;
   t->root     = NULL;
   stackClear(t);

   return t;
}

// Destructor, frees up all memory held by remaining nodes.
void VG_(OSet_Destroy)(AvlTree* t)
{
   AvlNode* n;
   Int i, sz = 0;
   
   vg_assert(t);
   stackClear(t);
   if (t->root)
      stackPush(t, t->root, 1);

   // Free all the AvlNodes.  This is a post-order traversal, because we
   // must free all children of a node before the node itself.
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
         t->free(n);
         sz++;
         break;
      }
   }
   vg_assert(sz == t->nElems);

   // Free the AvlTree itself.
   t->free(t);
}

// Allocate and initialise a new node.
void* VG_(OSet_AllocNode)(AvlTree* t, SizeT elemSize)
{
   Int nodeSize = sizeof(AvlNode) + elemSize;
   AvlNode* n   = t->alloc( nodeSize );
   vg_assert(elemSize > 0);
   VG_(memset)(n, 0, nodeSize);
   n->magic = OSET_MAGIC;
   return elem_of_node(n);
}

void VG_(OSet_FreeNode)(AvlTree* t, void* e)
{
   t->free( node_of_elem(e) );
}

/*--------------------------------------------------------------------*/
/*--- Insertion                                                    ---*/
/*--------------------------------------------------------------------*/

static inline Int cmp_key_root(AvlTree* t, AvlNode* n)
{
   return t->cmp
          ? slow_cmp(t, slow_key_of_node(t, n), t->root)
          : fast_cmp(   fast_key_of_node(   n), t->root);
}

// Insert element e into the non-empty AVL tree t.
// Returns True if the depth of the tree has grown.
static Bool avl_insert(AvlTree* t, AvlNode* n)
{
   Int cmpres;

   cmpres = cmp_key_root(t, n);

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
      vg_assert2(0, "OSet_Insert: duplicate element added");
   }
}

// Insert element e into the AVL tree t.  This is just a wrapper for
// avl_insert() which doesn't return a Bool.
void VG_(OSet_Insert)(AvlTree* t, void* e)
{
   vg_assert(t);

   // Initialise.  Even though OSet_AllocNode zeroes these fields, we should
   // do it again in case a node is removed and then re-added to the tree.
   AvlNode* n = node_of_elem(e);
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

/*--------------------------------------------------------------------*/
/*--- Lookup                                                       ---*/
/*--------------------------------------------------------------------*/

// Find the *node* in t matching k, or NULL if not found.
static AvlNode* avl_lookup(AvlTree* t, void* k)
{
   Int      cmpres;
   AvlNode* curr;

   vg_assert(t);
   curr = t->root;

   if (t->cmp) {
      // General case
      while (True) {
         if (curr == NULL) return NULL;
         cmpres = slow_cmp(t, k, curr);
         if (cmpres < 0) curr = curr->left;  else
         if (cmpres > 0) curr = curr->right; else
         return curr;
      }
   } else {
      // Fast-track special case.  We use the no-check version of
      // elem_of_node because it saves about 10% on lookup time.  This
      // shouldn't be very dangerous because each node will have been
      // checked on insertion.
      Int kk = *(Int*)k;
      while (True) {
         if (curr == NULL) return NULL;
         cmpres = kk - *(Int*)elem_of_node_no_check(curr);
         if (cmpres < 0) curr = curr->left;  else
         if (cmpres > 0) curr = curr->right; else
         return curr;
      }
   }
}

// Find the *element* in t matching k, or NULL if not found.
void* VG_(OSet_Lookup)(AvlTree* t, void* k)
{
   AvlNode* n = avl_lookup(t, k);
   return ( n ? elem_of_node(n) : NULL );
}

// Is there an element matching k?
Bool VG_(OSet_Contains)(AvlTree* t, void* k)
{
   return (NULL != VG_(OSet_Lookup)(t, k));
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
   Int  cmpres;

   vg_assert(t && t->root);
   
   cmpres = cmp_key_root(t, n);

   if (cmpres < 0) {
      // Remove from the left subtree
      vg_assert(t->root->left);
      AvlTree left_subtree;
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
   Int ch;
   AvlNode* n;

   vg_assert(t && t->root);

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

// Remove and return the element matching the key 'k', or NULL if not present.
void* VG_(OSet_Remove)(AvlTree* t, void* k)
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

/*--------------------------------------------------------------------*/
/*--- Iterator                                                     ---*/
/*--------------------------------------------------------------------*/

// The iterator is implemented using in-order traversal with an explicit
// stack, which lets us do the traversal one step at a time and remember
// where we are between each call to OSet_Next().

void VG_(OSet_ResetIter)(AvlTree* t)
{
   vg_assert(t);
   stackClear(t);
   if (t->root)
      stackPush(t, t->root, 1);
}

void* VG_(OSet_Next)(AvlTree* t)
{
   Int i;
   OSetNode* n;
   
   vg_assert(t);

   // This in-order traversal requires each node to be pushed and popped
   // three times.  These could be avoided by updating nodes in-situ on the
   // top of the stack, but the push/pop cost is so small that it's worth
   // keeping this loop in this simpler form.
   while (stackPop(t, &n, &i)) {
      switch (i) {
      case 1: 
         stackPush(t, n, 2);
         if (n->left)  stackPush(t, n->left, 1);
         break;
      case 2: 
         stackPush(t, n, 3);
         return elem_of_node(n);
      case 3:
         if (n->right) stackPush(t, n->right, 1);
         break;
      }
   }

   // Stack empty, iterator is exhausted, return NULL
   return NULL;
}

/*--------------------------------------------------------------------*/
/*--- Miscellaneous operations                                     ---*/
/*--------------------------------------------------------------------*/

Int VG_(OSet_Size)(AvlTree* t)
{
   vg_assert(t);
   return t->nElems;
}

static void OSet_Print2( AvlTree* t, AvlNode* n,
                         Char*(*strElem)(void *), Int p )
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
static void OSet_Print( AvlTree* t, const HChar *where, Char*(*strElem)(void *) )
{
   VG_(printf)("-- start %s ----------------\n", where);
   OSet_Print2(t, t->root, strElem, 0);
   VG_(printf)("-- end   %s ----------------\n", where);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

