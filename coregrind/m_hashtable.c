
/*--------------------------------------------------------------------*/
/*--- A separately chained hash table.               m_hashtable.c ---*/
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

#include "core.h"
#include "pub_core_hashtable.h"

/*--------------------------------------------------------------------*/
/*--- Declarations                                                 ---*/
/*--------------------------------------------------------------------*/

/* Holds malloc'd but not freed blocks.  Static, so zero-inited by default. */

#define VG_N_CHAINS 4999 /* a prime number */

#define VG_CHAIN_NO(aa) (((UWord)(aa)) % VG_N_CHAINS)

/*--------------------------------------------------------------------*/
/*--- Functions                                                    ---*/
/*--------------------------------------------------------------------*/

VgHashTable VG_(HT_construct)(void)
{
   /* Initialises to zero, ie. all entries NULL */
   return VG_(calloc)(VG_N_CHAINS, sizeof(VgHashNode*));
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
   the previous node's 'next' pointer which allows it to be removed from the
   list later without having to look it up again.  */
VgHashNode* VG_(HT_get_node) ( VgHashTable table, UWord key,
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

/* Allocates a suitably-sized array, copies all the malloc'd block
   shadows into it, then returns both the array and the size of it.  This is
   used by the memory-leak detector.
*/
VgHashNode** VG_(HT_to_array) ( VgHashTable table, /*OUT*/ UInt* n_shadows )
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
   vg_assert(j == *n_shadows);

   return arr;
}

/* Return the first VgHashNode satisfying the predicate p. */
VgHashNode* VG_(HT_first_match) ( VgHashTable table,
                                  Bool (*p) ( VgHashNode*, void* ),
                                  void* d )
{
   UInt      i;
   VgHashNode* node;

   for (i = 0; i < VG_N_CHAINS; i++)
      for (node = table[i]; node != NULL; node = node->next)
         if ( p(node, d) )
            return node;

   return NULL;
}

void VG_(HT_apply_to_all_nodes)( VgHashTable table,
                                 void (*f)(VgHashNode*, void*),
                                 void* d )
{
   UInt      i;
   VgHashNode* node;

   for (i = 0; i < VG_N_CHAINS; i++) {
      for (node = table[i]; node != NULL; node = node->next) {
         f(node, d);
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
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
