
/*--------------------------------------------------------------------*/
/*--- An AVL tree based finite map for word keys and word values.  ---*/
/*--- Inspired by Haskell's "FiniteMap" library.                   ---*/
/*---                                                   m_wordfm.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007-2017 Julian Seward
      jseward@acm.org

   This code is based on previous work by Nicholas Nethercote
   (coregrind/m_oset.c) which is

   Copyright (C) 2005-2017 Nicholas Nethercote
       njn@valgrind.org

   which in turn was derived partially from:

      AVL C library
      Copyright (C) 2000,2002  Daniel Nagy

      This program is free software; you can redistribute it and/or
      modify it under the terms of the GNU General Public License as
      published by the Free Software Foundation; either version 2 of
      the License, or (at your option) any later version.
      [...]

      (taken from libavl-0.4/debian/copyright)

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
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_wordfm.h"   /* self */


//------------------------------------------------------------------//
//---                           WordFM                           ---//
//---                       Implementation                       ---//
//------------------------------------------------------------------//

/* One element of the AVL tree */
typedef
   struct _AvlNode {
      UWord key;
      UWord val;
      struct _AvlNode* child[2]; /* [0] is left subtree, [1] is right */
      Char balance; /* do not make this unsigned */
   }
   AvlNode;

typedef 
   struct {
      UWord w;
      Bool b;
   }
   MaybeWord;

#define WFM_STKMAX    32    // At most 2**32 entries can be iterated over

struct _WordFM {
   AvlNode* root;
   void*    (*alloc_nofail)( const HChar*, SizeT );
   const HChar* cc;
   void     (*dealloc)(void*);
   Word     (*kCmp)(UWord,UWord);
   AvlNode* nodeStack[WFM_STKMAX]; // Iterator node stack
   Int      numStack[WFM_STKMAX];  // Iterator num stack
   Int      stackTop;              // Iterator stack pointer, one past end
}; 

/* forward */
static Bool avl_removeroot_wrk(AvlNode** t, Word(*kCmp)(UWord,UWord));

/* Swing to the left.  Warning: no balance maintenance. */
static void avl_swl ( AvlNode** root )
{
   AvlNode* a  = *root;
   AvlNode* b  = a->child[1];
   *root       = b;
   a->child[1] = b->child[0];
   b->child[0] = a;
}

/* Swing to the right.  Warning: no balance maintenance. */
static void avl_swr ( AvlNode** root )
{
   AvlNode* a  = *root;
   AvlNode* b  = a->child[0];
   *root       = b;
   a->child[0] = b->child[1];
   b->child[1] = a;
}

/* Balance maintenance after especially nasty swings. */
static void avl_nasty ( AvlNode* root )
{
   switch (root->balance) {
      case -1: 
         root->child[0]->balance = 0;
         root->child[1]->balance = 1;
         break;
      case 1:
         root->child[0]->balance = -1;
         root->child[1]->balance = 0;
         break;
      case 0:
         root->child[0]->balance = 0;
         root->child[1]->balance = 0;
         break;
      default:
         vg_assert(0);
   }
   root->balance=0;
}

/* Find size of a non-NULL tree. */
static UWord size_avl_nonNull ( const AvlNode* nd )
{
   return 1 + (nd->child[0] ? size_avl_nonNull(nd->child[0]) : 0)
            + (nd->child[1] ? size_avl_nonNull(nd->child[1]) : 0);
}

/* Unsignedly compare w1 and w2.  If w1 < w2, produce a negative
   number; if w1 > w2 produce a positive number, and if w1 == w2
   produce zero. */
static inline Word cmp_unsigned_Words ( UWord w1, UWord w2 ) {
   if (w1 < w2) return -1;
   if (w1 > w2) return 1;
   return 0;
}

/* Insert element a into the AVL tree t.  Returns True if the depth of
   the tree has grown.  If element with that key is already present,
   just copy a->val to existing node, first returning old ->val field
   of existing node in *oldV, so that the caller can finalize it
   however it wants.
*/
static 
Bool avl_insert_wrk ( AvlNode**         rootp, 
                      /*OUT*/MaybeWord* oldV,
                      AvlNode*          a, 
                      Word              (*kCmp)(UWord,UWord) )
{
   Word cmpres;

   /* initialize */
   a->child[0] = 0;
   a->child[1] = 0;
   a->balance  = 0;
   oldV->b     = False;

   /* insert into an empty tree? */
   if (!(*rootp)) {
      (*rootp) = a;
      return True;
   }

   cmpres = kCmp ? /*boxed*/   kCmp( (*rootp)->key, a->key )
                 : /*unboxed*/ cmp_unsigned_Words( (UWord)(*rootp)->key,
                                                   (UWord)a->key );

   if (cmpres > 0) {
      /* insert into the left subtree */
      if ((*rootp)->child[0]) {
         AvlNode* left_subtree = (*rootp)->child[0];
         if (avl_insert_wrk(&left_subtree, oldV, a, kCmp)) {
            switch ((*rootp)->balance--) {
               case  1: return False;
               case  0: return True;
               case -1: break;
               default: vg_assert(0);
            }
            if ((*rootp)->child[0]->balance < 0) {
               avl_swr( rootp );
               (*rootp)->balance = 0;
               (*rootp)->child[1]->balance = 0;
            } else {
               avl_swl( &((*rootp)->child[0]) );
               avl_swr( rootp );
               avl_nasty( *rootp );
            }
         } else {
            (*rootp)->child[0] = left_subtree;
         }
         return False;
      } else {
         (*rootp)->child[0] = a;
         if ((*rootp)->balance--) 
            return False;
         return True;
      }
      vg_assert(0);/*NOTREACHED*/
   }
   else 
   if (cmpres < 0) {
      /* insert into the right subtree */
      if ((*rootp)->child[1]) {
         AvlNode* right_subtree = (*rootp)->child[1];
         if (avl_insert_wrk(&right_subtree, oldV, a, kCmp)) {
            switch((*rootp)->balance++){
               case -1: return False;
               case  0: return True;
               case  1: break;
               default: vg_assert(0);
            }
            if ((*rootp)->child[1]->balance > 0) {
               avl_swl( rootp );
               (*rootp)->balance = 0;
               (*rootp)->child[0]->balance = 0;
            } else {
               avl_swr( &((*rootp)->child[1]) );
               avl_swl( rootp );
               avl_nasty( *rootp );
            }
         } else {
            (*rootp)->child[1] = right_subtree;
         }
         return False;
      } else {
         (*rootp)->child[1] = a;
         if ((*rootp)->balance++) 
            return False;
         return True;
      }
      vg_assert(0);/*NOTREACHED*/
   }
   else {
      /* cmpres == 0, a duplicate - replace the val, but don't
         incorporate the node in the tree */
      oldV->b = True;
      oldV->w = (*rootp)->val;
      (*rootp)->val = a->val;
      return False;
   }
}

/* Remove an element a from the AVL tree t.  a must be part of
   the tree.  Returns True if the depth of the tree has shrunk. 
*/
static
Bool avl_remove_wrk ( AvlNode** rootp, 
                      AvlNode*  a, 
                      Word(*kCmp)(UWord,UWord) )
{
   Bool ch;
   Word cmpres;
   cmpres = kCmp ? /*boxed*/   kCmp( (*rootp)->key, a->key )
                 : /*unboxed*/ cmp_unsigned_Words( (UWord)(*rootp)->key,
                                                   (UWord)a->key );

   if (cmpres > 0){
      /* remove from the left subtree */
      AvlNode* left_subtree = (*rootp)->child[0];
      vg_assert(left_subtree);
      ch = avl_remove_wrk(&left_subtree, a, kCmp);
      (*rootp)->child[0]=left_subtree;
      if (ch) {
         switch ((*rootp)->balance++) {
            case -1: return True;
            case  0: return False;
            case  1: break;
            default: vg_assert(0);
         }
         switch ((*rootp)->child[1]->balance) {
            case 0:
               avl_swl( rootp );
               (*rootp)->balance = -1;
               (*rootp)->child[0]->balance = 1;
               return False;
            case 1: 
               avl_swl( rootp );
               (*rootp)->balance = 0;
               (*rootp)->child[0]->balance = 0;
               return True;
            case -1:
               break;
            default:
               vg_assert(0);
         }
         avl_swr( &((*rootp)->child[1]) );
         avl_swl( rootp );
         avl_nasty( *rootp );
         return True;
      }
   }
   else
   if (cmpres < 0) {
      /* remove from the right subtree */
      AvlNode* right_subtree = (*rootp)->child[1];
      vg_assert(right_subtree);
      ch = avl_remove_wrk(&right_subtree, a, kCmp);
      (*rootp)->child[1] = right_subtree;
      if (ch) {
         switch ((*rootp)->balance--) {
            case  1: return True;
            case  0: return False;
            case -1: break;
            default: vg_assert(0);
         }
         switch ((*rootp)->child[0]->balance) {
            case 0:
               avl_swr( rootp );
               (*rootp)->balance = 1;
               (*rootp)->child[1]->balance = -1;
               return False;
            case -1:
               avl_swr( rootp );
               (*rootp)->balance = 0;
               (*rootp)->child[1]->balance = 0;
               return True;
            case 1:
               break;
            default:
               vg_assert(0);
         }
         avl_swl( &((*rootp)->child[0]) );
         avl_swr( rootp );
         avl_nasty( *rootp );
         return True;
      }
   }
   else {
      vg_assert(cmpres == 0);
      vg_assert((*rootp)==a);
      return avl_removeroot_wrk(rootp, kCmp);
   }
   return 0;
}

/* Remove the root of the AVL tree *rootp.
 * Warning: dumps core if *rootp is empty
 */
static 
Bool avl_removeroot_wrk ( AvlNode** rootp, 
                          Word(*kCmp)(UWord,UWord) )
{
   Bool     ch;
   AvlNode* a;
   if (!(*rootp)->child[0]) {
      if (!(*rootp)->child[1]) {
         (*rootp) = 0;
         return True;
      }
      (*rootp) = (*rootp)->child[1];
      return True;
   }
   if (!(*rootp)->child[1]) {
      (*rootp) = (*rootp)->child[0];
      return True;
   }
   if ((*rootp)->balance < 0) {
      /* remove from the left subtree */
      a = (*rootp)->child[0];
      while (a->child[1]) a = a->child[1];
   } else {
      /* remove from the right subtree */
      a = (*rootp)->child[1];
      while (a->child[0]) a = a->child[0];
   }
   ch = avl_remove_wrk(rootp, a, kCmp);
   a->child[0] = (*rootp)->child[0];
   a->child[1] = (*rootp)->child[1];
   a->balance  = (*rootp)->balance;
   (*rootp)    = a;
   if(a->balance == 0) return ch;
   return False;
}

static 
AvlNode* avl_find_node ( AvlNode* t, Word k, Word(*kCmp)(UWord,UWord) )
{
   if (kCmp) {
      /* Boxed comparisons */
      Word cmpresS;
      while (True) {
         if (t == NULL) return NULL;
         cmpresS = kCmp(t->key, k);
         if (cmpresS > 0) t = t->child[0]; else
         if (cmpresS < 0) t = t->child[1]; else
         return t;
      }
   } else {
      /* Unboxed comparisons */
      Word  cmpresS; /* signed */
      UWord cmpresU; /* unsigned */
      while (True) {
         if (t == NULL) return NULL; /* unlikely ==> predictable */
         cmpresS = cmp_unsigned_Words( (UWord)t->key, (UWord)k );
         if (cmpresS == 0) return t; /* unlikely ==> predictable */
         cmpresU = (UWord)cmpresS;
         cmpresU >>=/*unsigned*/ (8 * sizeof(cmpresU) - 1);
         t = t->child[cmpresU];
      }
   }
}

static
Bool avl_find_bounds ( const AvlNode* t, 
                       /*OUT*/UWord* kMinP, /*OUT*/UWord* vMinP,
                       /*OUT*/UWord* kMaxP, /*OUT*/UWord* vMaxP,
                       UWord minKey, UWord minVal,
                       UWord maxKey, UWord maxVal,
                       UWord key,
                       Word(*kCmp)(UWord,UWord) )
{
   UWord kLowerBound = minKey;
   UWord vLowerBound = minVal;
   UWord kUpperBound = maxKey;
   UWord vUpperBound = maxVal;
   while (t) {
      Word cmpresS = kCmp ? kCmp(t->key, key)
                          : cmp_unsigned_Words(t->key, key);
      if (cmpresS < 0) {
         kLowerBound = t->key;
         vLowerBound = t->val;
         t = t->child[1];
         continue;
      }
      if (cmpresS > 0) {
         kUpperBound = t->key;
         vUpperBound = t->val;
         t = t->child[0];
         continue;
      }
      /* We should never get here.  If we do, it means the given key
         is actually present in the tree, which means the original
         call was invalid -- an error on the caller's part, and we
         cannot give any meaningful values for the bounds.  (Well,
         maybe we could, but we're not gonna.  Ner!) */
      return False;
   }
   if (kMinP) *kMinP = kLowerBound;
   if (vMinP) *vMinP = vLowerBound;
   if (kMaxP) *kMaxP = kUpperBound;
   if (vMaxP) *vMaxP = vUpperBound;
   return True;
}

// Clear the iterator stack.
static void stackClear(WordFM* fm)
{
   Int i;
   vg_assert(fm);
   for (i = 0; i < WFM_STKMAX; i++) {
      fm->nodeStack[i] = NULL;
      fm->numStack[i]  = 0;
   }
   fm->stackTop = 0;
}

// Push onto the iterator stack.
static inline void stackPush(WordFM* fm, AvlNode* n, Int i)
{
   vg_assert(fm->stackTop < WFM_STKMAX);
   vg_assert(1 <= i && i <= 3);
   fm->nodeStack[fm->stackTop] = n;
   fm-> numStack[fm->stackTop] = i;
   fm->stackTop++;
}

// Pop from the iterator stack.
static inline Bool stackPop(WordFM* fm, AvlNode** n, Int* i)
{
   vg_assert(fm->stackTop <= WFM_STKMAX);

   if (fm->stackTop > 0) {
      fm->stackTop--;
      *n = fm->nodeStack[fm->stackTop];
      *i = fm-> numStack[fm->stackTop];
      vg_assert(1 <= *i && *i <= 3);
      fm->nodeStack[fm->stackTop] = NULL;
      fm-> numStack[fm->stackTop] = 0;
      return True;
   } else {
      return False;
   }
}

static 
AvlNode* avl_dopy ( const AvlNode* nd, 
                    UWord(*dopyK)(UWord), 
                    UWord(*dopyV)(UWord),
                    void*(alloc_nofail)(const HChar*,SizeT),
                    const HChar* cc )
{
   AvlNode* nyu;

   vg_assert(nd != NULL);

   nyu = alloc_nofail(cc, sizeof(AvlNode));
   
   nyu->child[0] = nd->child[0];
   nyu->child[1] = nd->child[1];
   nyu->balance = nd->balance;

   /* Copy key */
   if (dopyK) {
      nyu->key = dopyK( nd->key );
   } else {
      /* copying assumedly unboxed keys */
      nyu->key = nd->key;
   }

   /* Copy val */
   if (dopyV) {
      nyu->val = dopyV( nd->val );
   } else {
      /* copying assumedly unboxed vals */
      nyu->val = nd->val;
   }

   /* Copy subtrees */
   if (nyu->child[0]) {
      nyu->child[0] = avl_dopy( nyu->child[0], dopyK, dopyV, 
                                alloc_nofail, cc );
   }
   if (nyu->child[1]) {
      nyu->child[1] = avl_dopy( nyu->child[1], dopyK, dopyV,
                                alloc_nofail, cc );
   }

   return nyu;
}

/* Initialise a WordFM. */
static void initFM ( WordFM* fm,
                     void*   (*alloc_nofail)( const HChar*, SizeT ),
                     const HChar* cc,
                     void    (*dealloc)(void*),
                     Word    (*kCmp)(UWord,UWord) )
{
   fm->root         = 0;
   fm->kCmp         = kCmp;
   fm->alloc_nofail = alloc_nofail;
   fm->cc           = cc;
   fm->dealloc      = dealloc;
   fm->stackTop     = 0;
}

/* --- Public interface functions --- */

/* Allocate and initialise a WordFM.  If kCmp is non-NULL, elements in
   the set are ordered according to the ordering specified by kCmp,
   which becomes obvious if you use VG_(initIterFM),
   VG_(initIterAtFM), VG_(nextIterFM), VG_(doneIterFM) to iterate over
   sections of the map, or the whole thing.  If kCmp is NULL then the
   ordering used is unsigned word ordering (UWord) on the key
   values. */
WordFM* VG_(newFM) ( void* (*alloc_nofail)( const HChar*, SizeT ),
                     const HChar* cc,
                     void  (*dealloc)(void*),
                     Word  (*kCmp)(UWord,UWord) )
{
   WordFM* fm = alloc_nofail(cc, sizeof(WordFM));
   initFM(fm, alloc_nofail, cc, dealloc, kCmp);
   return fm;
}

static void avl_free ( AvlNode* nd, 
                       void(*kFin)(UWord),
                       void(*vFin)(UWord),
                       void(*dealloc)(void*) )
{
   if (!nd)
      return;
   if (nd->child[0])
      avl_free(nd->child[0], kFin, vFin, dealloc);
   if (nd->child[1])
      avl_free(nd->child[1], kFin, vFin, dealloc);
   if (kFin)
      kFin( nd->key );
   if (vFin)
      vFin( nd->val );
   VG_(memset)(nd, 0, sizeof(AvlNode));
   dealloc(nd);
}

/* Free up the FM.  If kFin is non-NULL, it is applied to keys
   before the FM is deleted; ditto with vFin for vals. */
void VG_(deleteFM) ( WordFM* fm, void(*kFin)(UWord), void(*vFin)(UWord) )
{
   void(*dealloc)(void*) = fm->dealloc;
   avl_free( fm->root, kFin, vFin, dealloc );
   VG_(memset)(fm, 0, sizeof(WordFM) );
   dealloc(fm);
}

/* Add (k,v) to fm. */
Bool VG_(addToFM) ( WordFM* fm, UWord k, UWord v )
{
   MaybeWord oldV;
   AvlNode* node;
   node = fm->alloc_nofail( fm->cc, sizeof(AvlNode) );
   node->key = k;
   node->val = v;
   oldV.b = False;
   oldV.w = 0;
   avl_insert_wrk( &fm->root, &oldV, node, fm->kCmp );
   //if (oldV.b && fm->vFin)
   //   fm->vFin( oldV.w );
   if (oldV.b)
      fm->dealloc(node);
   return oldV.b;
}

// Delete key from fm, returning associated key and val if found
Bool VG_(delFromFM) ( WordFM* fm,
                      /*OUT*/UWord* oldK, /*OUT*/UWord* oldV, UWord key )
{
   AvlNode* node = avl_find_node( fm->root, key, fm->kCmp );
   if (node) {
      avl_remove_wrk( &fm->root, node, fm->kCmp );
      if (oldK)
         *oldK = node->key;
      if (oldV)
         *oldV = node->val;
      fm->dealloc(node);
      return True;
   } else {
      return False;
   }
}

// Look up in fm, assigning found key & val at spec'd addresses
Bool VG_(lookupFM) ( const WordFM* fm, 
                     /*OUT*/UWord* keyP, /*OUT*/UWord* valP, UWord key )
{
   AvlNode* node = avl_find_node( fm->root, key, fm->kCmp );
   if (node) {
      if (keyP)
         *keyP = node->key;
      if (valP)
         *valP = node->val;
      return True;
   } else {
      return False;
   }
}

// See comment in pub_tool_wordfm.h for explanation
Bool VG_(findBoundsFM)( const WordFM* fm,
                        /*OUT*/UWord* kMinP, /*OUT*/UWord* vMinP,
                        /*OUT*/UWord* kMaxP, /*OUT*/UWord* vMaxP,
                        UWord minKey, UWord minVal,
                        UWord maxKey, UWord maxVal,
                        UWord key )
{
   /* really we should assert that minKey <= key <= maxKey,
      where <= is as defined by fm->kCmp. */
   return avl_find_bounds( fm->root, kMinP, vMinP,
                                     kMaxP, vMaxP,
                                     minKey, minVal, 
                                     maxKey, maxVal,
                                     key, fm->kCmp );
}

// See comment in pub_tool_wordfm.h for performance warning
UWord VG_(sizeFM) ( const WordFM* fm )
{
   // Hmm, this is a bad way to do this
   return fm->root ? size_avl_nonNull( fm->root ) : 0;
}

// NB UNTESTED!  TEST BEFORE USE!
//Bool VG_(isEmptyFM)( const WordFM* fm )
//{
//   return fm->root ? False : True;
//}

// set up FM for iteration
void VG_(initIterFM) ( WordFM* fm )
{
   vg_assert(fm);
   stackClear(fm);
   if (fm->root)
      stackPush(fm, fm->root, 1);
}

// set up FM for iteration so that the first key subsequently produced
// by VG_(nextIterFM) is the smallest key in the map >= start_at.
// Naturally ">=" is defined by the comparison function supplied to
// VG_(newFM), as documented above.
void VG_(initIterAtFM) ( WordFM* fm, UWord start_at )
{
   Int     i;
   AvlNode *n, *t;
   Word    cmpresS; /* signed */
   UWord   cmpresU; /* unsigned */

   vg_assert(fm);
   stackClear(fm);

   if (!fm->root) 
      return;

   n = NULL;
   // We need to do regular search and fill in the stack. 
   t = fm->root;

   while (True) {
      if (t == NULL) return;

      cmpresS 
         = fm->kCmp ? /*boxed*/   fm->kCmp( t->key, start_at )
                    : /*unboxed*/ cmp_unsigned_Words( t->key, start_at );

      if (cmpresS == 0) {
         // We found the exact key -- we are done. 
         // The iteration should start with this node.
         stackPush(fm, t, 2);
         // The stack now looks like {2, 2, ... ,2, 2}
         return;
      }
      cmpresU = (UWord)cmpresS;
      cmpresU >>=/*unsigned*/ (8 * sizeof(cmpresU) - 1);
      if (!cmpresU) {
         // Push this node only if we go to the left child. 
         stackPush(fm, t, 2);
      }
      t = t->child[cmpresU];
   }
   if (stackPop(fm, &n, &i)) {
      // If we've pushed something to stack and did not find the exact key, 
      // we must fix the top element of stack. 
      vg_assert(i == 2);
      stackPush(fm, n, 3);
      // the stack looks like {2, 2, ..., 2, 3}
   }
}

// get next key/val pair.  Will vg_assert if fm has been modified
// or looked up in since initIter{,At}FM was called.
Bool VG_(nextIterFM) ( WordFM* fm, /*OUT*/UWord* pKey, /*OUT*/UWord* pVal )
{
   Int i = 0;
   AvlNode* n = NULL;
   
   vg_assert(fm);

   // This in-order traversal requires each node to be pushed and popped
   // three times.  These could be avoided by updating nodes in-situ on the
   // top of the stack, but the push/pop cost is so small that it's worth
   // keeping this loop in this simpler form.
   while (stackPop(fm, &n, &i)) {
      switch (i) {
      case 1: case_1:
         stackPush(fm, n, 2);
         /* if (n->child[0])  stackPush(fm, n->child[0], 1); */
         if (n->child[0]) { n = n->child[0]; goto case_1; }
         break;
      case 2: 
         stackPush(fm, n, 3);
         if (pKey) *pKey = n->key;
         if (pVal) *pVal = n->val;
         return True;
      case 3:
         /* if (n->child[1]) stackPush(fm, n->child[1], 1); */
         if (n->child[1]) { n = n->child[1]; goto case_1; }
         break;
      default:
         vg_assert(0);
      }
   }

   // Stack empty, iterator is exhausted, return NULL
   return False;
}

// Finish an FM iteration
void VG_(doneIterFM) ( WordFM* fm )
{
}

WordFM* VG_(dopyFM) ( WordFM* fm, UWord(*dopyK)(UWord),
                      UWord(*dopyV)(UWord) )
{
   WordFM* nyu; 

   /* can't clone the fm whilst iterating on it */
   vg_assert(fm->stackTop == 0);

   nyu = fm->alloc_nofail( fm->cc, sizeof(WordFM) );

   *nyu = *fm;

   fm->stackTop = 0;
   VG_(memset)(fm->nodeStack, 0, sizeof(fm->nodeStack));
   VG_(memset)(fm->numStack, 0,  sizeof(fm->numStack));

   if (nyu->root) {
      nyu->root = avl_dopy( nyu->root, dopyK, dopyV,
                            fm->alloc_nofail, fm->cc );
      if (! nyu->root)
         return NULL;
   }

   return nyu;
}

//------------------------------------------------------------------//
//---                         end WordFM                         ---//
//---                       Implementation                       ---//
//------------------------------------------------------------------//

//------------------------------------------------------------------//
//---                WordBag (unboxed words only)                ---//
//---                       Implementation                       ---//
//------------------------------------------------------------------//

/* A trivial container, to make it opaque. */
struct _WordBag { 
   WordFM* fm; 
};

WordBag* VG_(newBag) ( void* (*alloc_nofail)( const HChar*, SizeT ),
                       const HChar* cc,
                       void  (*dealloc)(void*) )
{
   WordBag* bag = alloc_nofail(cc, sizeof(WordBag));
   bag->fm = VG_(newFM)( alloc_nofail, cc, dealloc, NULL );
   return bag;
}

void VG_(deleteBag) ( WordBag* bag )
{
   void (*dealloc)(void*) = bag->fm->dealloc;
   VG_(deleteFM)( bag->fm, NULL, NULL );
   VG_(memset)(bag, 0, sizeof(WordBag));
   dealloc(bag);
}

void VG_(addToBag)( WordBag* bag, UWord w )
{
   UWord key, count;
   if (VG_(lookupFM)(bag->fm, &key, &count, w)) {
      vg_assert(key == w);
      vg_assert(count >= 1);
      VG_(addToFM)(bag->fm, w, count+1);
   } else {
      VG_(addToFM)(bag->fm, w, 1);
   }
}

UWord VG_(elemBag) ( const WordBag* bag, UWord w )
{
   UWord key, count;
   if (VG_(lookupFM)( bag->fm, &key, &count, w)) {
      vg_assert(key == w);
      vg_assert(count >= 1);
      return count;
   } else {
      return 0;
   }
}

UWord VG_(sizeUniqueBag) ( const WordBag* bag )
{
   return VG_(sizeFM)( bag->fm );
}

static UWord sizeTotalBag_wrk ( const AvlNode* nd )
{
   /* unchecked pre: nd is non-NULL */
   UWord w = nd->val;
   vg_assert(w >= 1);
   if (nd->child[0])
      w += sizeTotalBag_wrk(nd->child[0]);
   if (nd->child[1])
      w += sizeTotalBag_wrk(nd->child[1]);
   return w;
}
UWord VG_(sizeTotalBag)( const WordBag* bag )
{
   if (bag->fm->root)
      return sizeTotalBag_wrk(bag->fm->root);
   else
      return 0;
}

Bool VG_(delFromBag)( WordBag* bag, UWord w )
{
   UWord key, count;
   if (VG_(lookupFM)(bag->fm, &key, &count, w)) {
      vg_assert(key == w);
      vg_assert(count >= 1);
      if (count > 1) {
         VG_(addToFM)(bag->fm, w, count-1);
      } else {
         vg_assert(count == 1);
         VG_(delFromFM)( bag->fm, NULL, NULL, w );
      }
      return True;
   } else {
      return False;
   }
}

Bool VG_(isEmptyBag)( const WordBag* bag )
{
   return VG_(sizeFM)(bag->fm) == 0;
}

Bool VG_(isSingletonTotalBag)( const WordBag* bag )
{
   AvlNode* nd;
   if (VG_(sizeFM)(bag->fm) != 1)
      return False;
   nd = bag->fm->root;
   vg_assert(nd);
   vg_assert(!nd->child[0]);
   vg_assert(!nd->child[1]);
   return nd->val == 1;
}

UWord VG_(anyElementOfBag)( const WordBag* bag )
{
   /* Return an arbitrarily chosen element in the bag.  We might as
      well return the one at the root of the underlying AVL tree. */
   AvlNode* nd = bag->fm->root;
   vg_assert(nd); /* if this fails, 'bag' is empty - caller is in error. */
   vg_assert(nd->val >= 1);
   return nd->key;
}

void VG_(initIterBag)( WordBag* bag )
{
   VG_(initIterFM)(bag->fm);
}

Bool VG_(nextIterBag)( WordBag* bag, /*OUT*/UWord* pVal, /*OUT*/UWord* pCount )
{
   return VG_(nextIterFM)( bag->fm, pVal, pCount );
}

void VG_(doneIterBag)( WordBag* bag )
{
   VG_(doneIterFM)( bag->fm );
}

//------------------------------------------------------------------//
//---             end WordBag (unboxed words only)               ---//
//---                       Implementation                       ---//
//------------------------------------------------------------------//

/*--------------------------------------------------------------------*/
/*--- end                                               m_wordfm.c ---*/
/*--------------------------------------------------------------------*/
