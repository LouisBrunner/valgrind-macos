
/*--------------------------------------------------------------------*/
/*--- A separately chained hash table.              vg_hashtable.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

#include "vg_include.h"

/*--------------------------------------------------------------------*/
/*--- Declarations                                                 ---*/
/*--------------------------------------------------------------------*/

/* Holds malloc'd but not freed blocks.  Static, so zero-inited by default. */

#define VG_N_CHAINS 997

#define VG_CHAIN_NO(aa) (((UInt)(aa)) % VG_N_CHAINS)

/*--------------------------------------------------------------------*/
/*--- Functions                                                    ---*/
/*--------------------------------------------------------------------*/

VgHashTable VG_(HT_construct)(void)
{
   /* VG_(malloc) initialises to zero */
   return VG_(malloc)(VG_N_CHAINS * sizeof(VgHashNode*));
}

Int VG_(HT_count_nodes) ( VgHashTable table )
{
   VgHashNode* node;
   UInt      chain;
   Int       n = 0;

   for (chain = 0; chain < VG_N_CHAINS; chain++)
      for (node = table[chain]; node != NULL; node = node->next)
         n++;
   return n;
}

/* Puts a new, heap allocated VgHashNode, into the malloclist. */
void VG_(HT_add_node) ( VgHashTable table, VgHashNode* node )
{
   UInt chain   = VG_CHAIN_NO(node->key);
   node->next   = table[chain];
   table[chain] = node;
}

/* Looks up a VgHashNode in the table.  Also returns the address of
   the previous node's `next' pointer which allows it to be removed from the
   list later without having to look it up again.  */
VgHashNode* VG_(HT_get_node) ( VgHashTable table, UInt key,
                             /*OUT*/VgHashNode*** next_ptr )
{
   VgHashNode *prev, *curr;
   Int       chain;

   chain = VG_CHAIN_NO(key);

   prev = NULL;
   curr = table[chain];
   while (True) {
      if (curr == NULL)
         break;
      if (key == curr->key)
         break;
      prev = curr;
      curr = curr->next;
   }

   if (NULL == prev)
      *next_ptr = & table[chain];
   else
      *next_ptr = & prev->next;

   return curr;
}

static
void sort_hash_array ( VgHashNode** shadows, UInt n_shadows )
{
   Int   incs[14] = { 1, 4, 13, 40, 121, 364, 1093, 3280,
                      9841, 29524, 88573, 265720,
                      797161, 2391484 };
   Int          lo = 0;
   Int          hi = n_shadows-1;
   Int          i, j, h, bigN, hp;
   VgHashNode* v;

   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (hp < 14 && incs[hp] < bigN) hp++; hp--;
   sk_assert(0 <= hp && hp < 14);

   for (; hp >= 0; hp--) {
      h = incs[hp];
      i = lo + h;
      while (1) {
         if (i > hi) break;
         v = shadows[i];
         j = i;
         while (shadows[j-h]->key > v->key) {
            shadows[j] = shadows[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         shadows[j] = v;
         i++;
      }
   }
}

/* Allocates a suitably-sized array, copies all the malloc'd block
   shadows into it, sorts it by the `key' field, then returns both the array
   and the size of it.  This is used by the memory-leak detector.
*/
VgHashNode** VG_(HT_to_sorted_array) ( VgHashTable table, 
                                       /*OUT*/ UInt* n_shadows )
{
   UInt       i, j;
   VgHashNode** arr;
   VgHashNode*  node;

   *n_shadows = 0;
   for (i = 0; i < VG_N_CHAINS; i++) {
      for (node = table[i]; node != NULL; node = node->next) {
         (*n_shadows)++;
      }
   }
   if (*n_shadows == 0) 
      return NULL;

   arr = VG_(malloc)( *n_shadows * sizeof(VgHashNode*) );

   j = 0;
   for (i = 0; i < VG_N_CHAINS; i++) {
      for (node = table[i]; node != NULL; node = node->next) {
         arr[j++] = node;
      }
   }
   sk_assert(j == *n_shadows);

   sort_hash_array(arr, *n_shadows);

   /* Sanity check; assert that the blocks are now in order */
   for (i = 0; i < *n_shadows-1; i++) {
      sk_assert( arr[i]->key < arr[i+1]->key );
   }

   return arr;
}

/* Return the first VgHashNode satisfying the predicate p. */
VgHashNode* VG_(HT_first_match) ( VgHashTable table, Bool (*p) ( VgHashNode* ))
{
   UInt      i;
   VgHashNode* node;

   for (i = 0; i < VG_N_CHAINS; i++)
      for (node = table[i]; node != NULL; node = node->next)
         if ( p(node) )
            return node;

   return NULL;
}

void VG_(HT_apply_to_all_nodes)( VgHashTable table, void (*f)(VgHashNode*) )
{
   UInt      i;
   VgHashNode* node;

   for (i = 0; i < VG_N_CHAINS; i++) {
      for (node = table[i]; node != NULL; node = node->next) {
         f(node);
      }
   }
}

void VG_(HT_destruct)(VgHashTable table)
{
   UInt      i;
   VgHashNode* node;
   
   for (i = 0; i < VG_N_CHAINS; i++) {
      for (node = table[i]; node != NULL; node = node->next) {
         VG_(free)(node);
      }
   }
   VG_(free)(table);
}

/*--------------------------------------------------------------------*/
/*--- end                                           vg_hashtable.c ---*/
/*--------------------------------------------------------------------*/
