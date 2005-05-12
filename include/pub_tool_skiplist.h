
/*--------------------------------------------------------------------*/
/*--- SkipList: a skiplist implementaiton.     pub_tool_skiplist.h ---*/
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

#ifndef __PUB_TOOL_SKIPLIST_H
#define __PUB_TOOL_SKIPLIST_H

/* 
   The idea here is that the skiplist puts its per-element data at the
   end of the structure.  When you initialize the skiplist, you tell
   it what structure your list elements are going to be.  Then you
   should allocate them with VG_(SkipNode_Alloc), which will allocate
   enough memory for the extra bits.
 */

typedef struct _SkipList SkipList;
typedef struct _SkipNode SkipNode;

typedef Int (*SkipCmp_t)(const void *key1, const void *key2);

struct _SkipList {
   const Short     arena;              // allocation arena
   const UShort    size;               // structure size (excluding SkipNode)
   const UShort    keyoff;             // key offset
   const SkipCmp_t cmp;                // compare two keys
         Char *    (*strkey)(void *);  // stringify a key (for debugging)
         SkipNode  *head;              // list head
};

/* Use this macro to initialize your skiplist head.  The arguments are pretty self explanitory:
   _type is the type of your element structure
   _key is the field within that type which you want to use as the key
   _cmp is the comparison function for keys - it gets two typeof(_key) pointers as args
   _strkey is a function which can return a string of your key - it's only used for debugging
   _arena is the arena to use for allocation - -1 is the default
 */
#define VG_SKIPLIST_INIT(_type, _key, _cmp, _strkey, _arena)   \
        {                                                      \
           .arena    = _arena,                                 \
           .size     = sizeof(_type),                          \
           .keyoff   = offsetof(_type, _key),                  \
           .cmp      = _cmp,                                   \
           .strkey   = _strkey,                                \
           .head     = NULL,                                   \
        }

/* List operations:
   SkipList_Find_* search a list.  The 3 variants are:
      Before: returns a node which is <= key, or NULL if none
      Exact:  returns a node which is == key, or NULL if none
      After:  returns a node which is >= key, or NULL if none
   SkipList_Insert inserts a new element into the list.  Duplicates are
      forbidden.  The element must have been created with SkipList_Alloc!
   SkipList_Remove removes an element from the list and returns it.  It
      doesn't free the memory.
*/
extern void *VG_(SkipList_Find_Before)  (const SkipList *l, void *key);
extern void *VG_(SkipList_Find_Exact)   (const SkipList *l, void *key);
extern void *VG_(SkipList_Find_After)   (const SkipList *l, void *key);
extern void  VG_(SkipList_Insert)       (      SkipList *l, void *data);
extern void *VG_(SkipList_Remove)       (      SkipList *l, void *key);

/* Some useful standard comparisons */
extern Int  VG_(cmp_Addr)  (const void *a, const void *b);
extern Int  VG_(cmp_Int)   (const void *a, const void *b);
extern Int  VG_(cmp_UInt)  (const void *a, const void *b);
extern Int  VG_(cmp_string)(const void *a, const void *b);

/* Node (element) operations:
   SkipNode_Alloc: allocate memory for a new element on the list.  Must be
      used before an element can be inserted!  Returns NULL if not enough
      memory.
   SkipNode_Free: free memory allocated above
   SkipNode_First: return the first element on the list
   SkipNode_Next: return the next element after "data" on the list - 
      NULL for none

   You can iterate through a SkipList like this:

      for(x = VG_(SkipNode_First)(&list);    // or SkipList_Find
          x != NULL;
          x = VG_(SkipNode_Next)(&list, x)) { ... }
*/
extern void *VG_(SkipNode_Alloc) (const SkipList *l);
extern void  VG_(SkipNode_Free)  (const SkipList *l, void *p);
extern void *VG_(SkipNode_First) (const SkipList *l);
extern void *VG_(SkipNode_Next)  (const SkipList *l, void *data);


#endif   // __PUB_TOOL_SKIPLIST_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
