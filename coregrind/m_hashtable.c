
/*--------------------------------------------------------------------*/
/*--- A separately-chained hash table.               m_hashtable.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_hashtable.h"
#include "pub_core_libcassert.h"
#include "pub_core_mallocfree.h"

/*--------------------------------------------------------------------*/
/*--- Declarations                                                 ---*/
/*--------------------------------------------------------------------*/

#define CHAIN_NO(key,tbl) (((UWord)(key)) % tbl->n_chains)

struct _VgHashTable {
   UInt        n_chains;      // should be prime
   VgHashNode* iterNode;      // current iterator node
   UInt        iterChain;     // next chain to be traversed by the iterator
   VgHashNode* chains[0];     // must be last field in the struct!
};

/*--------------------------------------------------------------------*/
/*--- Functions                                                    ---*/
/*--------------------------------------------------------------------*/

VgHashTable VG_(HT_construct)(UInt n_chains)
{
   /* Initialises to zero, ie. all entries NULL */
   SizeT sz = sizeof(struct _VgHashTable) + n_chains*sizeof(VgHashNode*);
   VgHashTable table = VG_(calloc)(1, sz);
   table->n_chains = n_chains;
   return table;
}

Int VG_(HT_count_nodes) ( VgHashTable table )
{
   VgHashNode* node;
   UInt      chain;
   Int       n = 0;

   for (chain = 0; chain < table->n_chains; chain++)
      for (node = table->chains[chain]; node != NULL; node = node->next)
         n++;
   return n;
}

/* Puts a new, heap allocated VgHashNode, into the VgHashTable.  Prepends
   the node to the appropriate chain. */
void VG_(HT_add_node) ( VgHashTable table, void* vnode )
{
   VgHashNode* node     = (VgHashNode*)vnode;
   UInt chain           = CHAIN_NO(node->key, table);
   node->next           = table->chains[chain];
   table->chains[chain] = node;
}

/* Looks up a VgHashNode in the table.  Also returns the address of
   the previous node's 'next' pointer which allows it to be removed from the
   list later without having to look it up again.  */
void* VG_(HT_get_node) ( VgHashTable table, UWord key,
                         /*OUT*/VgHashNode*** next_ptr )
{
   VgHashNode *prev, *curr;
   Int       chain;

   chain = CHAIN_NO(key, table);

   prev = NULL;
   curr = table->chains[chain];
   while (True) {
      if (curr == NULL)
         break;
      if (key == curr->key)
         break;
      prev = curr;
      curr = curr->next;
   }

   if (NULL == prev)
      *next_ptr = & (table->chains[chain]);
   else
      *next_ptr = & (prev->next);

   return curr;
}

/* Looks up a VgHashNode in the table.  Returns NULL if not found. */
void* VG_(HT_lookup) ( VgHashTable table, UWord key )
{
   VgHashNode* curr = table->chains[ CHAIN_NO(key, table) ];

   while (curr) {
      if (key == curr->key) {
         return curr;
      }
      curr = curr->next;
   }
   return NULL;
}

/* Removes a VgHashNode from the table.  Returns NULL if not found. */
void* VG_(HT_remove) ( VgHashTable table, UWord key )
{
   Int          chain         = CHAIN_NO(key, table);
   VgHashNode*  curr          =   table->chains[chain];
   VgHashNode** prev_next_ptr = &(table->chains[chain]);

   while (curr) {
      if (key == curr->key) {
         *prev_next_ptr = curr->next;
         return curr;
      }
      prev_next_ptr = &(curr->next);
      curr = curr->next;
   }
   return NULL;
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
   for (i = 0; i < table->n_chains; i++) {
      for (node = table->chains[i]; node != NULL; node = node->next) {
         (*n_shadows)++;
      }
   }
   if (*n_shadows == 0) 
      return NULL;

   arr = VG_(malloc)( *n_shadows * sizeof(VgHashNode*) );

   j = 0;
   for (i = 0; i < table->n_chains; i++) {
      for (node = table->chains[i]; node != NULL; node = node->next) {
         arr[j++] = node;
      }
   }
   vg_assert(j == *n_shadows);

   return arr;
}

/* Return the first VgHashNode satisfying the predicate p. */
void* VG_(HT_first_match) ( VgHashTable table,
                             Bool (*p) ( VgHashNode*, void* ),
                             void* d )
{
   UInt      i;
   VgHashNode* node;

   for (i = 0; i < table->n_chains; i++)
      for (node = table->chains[i]; node != NULL; node = node->next)
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

   for (i = 0; i < table->n_chains; i++) {
      for (node = table->chains[i]; node != NULL; node = node->next) {
         f(node, d);
      }
   }
}

void VG_(HT_ResetIter)(VgHashTable table)
{
   vg_assert(table);
   table->iterNode  = NULL;
   table->iterChain = 0;
}

void* VG_(HT_Next)(VgHashTable table)
{
   Int i;
   vg_assert(table);
   
   if (table->iterNode && table->iterNode->next) {
      table->iterNode = table->iterNode->next;
      return table->iterNode;
   }

   for (i = table->iterChain; i < table->n_chains; i++) {
      if (table->chains[i]) {
         table->iterNode  = table->chains[i];
         table->iterChain = i + 1;  // Next chain to be traversed
         return table->iterNode;
      }
   }
   return NULL;
}

void VG_(HT_destruct)(VgHashTable table)
{
   UInt      i;
   VgHashNode* node;
   
   for (i = 0; i < table->n_chains; i++) {
      for (node = table->chains[i]; node != NULL; node = node->next) {
         VG_(free)(node);
      }
   }
   VG_(free)(table);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
