
/*--------------------------------------------------------------------*/
/*--- A separately-chained hash table.               m_hashtable.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2011 Nicholas Nethercote
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

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_hashtable.h"
#include "pub_core_libcassert.h"
#include "pub_core_mallocfree.h"

/*--------------------------------------------------------------------*/
/*--- Declarations                                                 ---*/
/*--------------------------------------------------------------------*/

#define CHAIN_NO(key,tbl) (((UWord)(key)) % tbl->n_chains)

struct _VgHashTable {
   UInt         n_chains;   // should be prime
   UInt         n_elements;
   VgHashNode*  iterNode;   // current iterator node
   UInt         iterChain;  // next chain to be traversed by the iterator
   VgHashNode** chains;     // expanding array of hash chains
   Bool         iterOK;     // table safe to iterate over?
   HChar*       name;       // name of table (for debugging only)
};

#define N_HASH_PRIMES 20

static SizeT primes[N_HASH_PRIMES] = {
         769UL,         1543UL,         3079UL,          6151UL,
       12289UL,        24593UL,        49157UL,         98317UL,
      196613UL,       393241UL,       786433UL,       1572869UL,
     3145739UL,      6291469UL,     12582917UL,      25165843UL,
    50331653UL,    100663319UL,    201326611UL,     402653189UL
};

/*--------------------------------------------------------------------*/
/*--- Functions                                                    ---*/
/*--------------------------------------------------------------------*/

VgHashTable VG_(HT_construct) ( HChar* name )
{
   /* Initialises to zero, ie. all entries NULL */
   SizeT       n_chains = primes[0];
   SizeT       sz       = n_chains * sizeof(VgHashNode*);
   VgHashTable table    = VG_(calloc)("hashtable.Hc.1",
                                      1, sizeof(struct _VgHashTable));
   table->chains        = VG_(calloc)("hashtable.Hc.2", 1, sz);
   table->n_chains      = n_chains;
   table->n_elements    = 0;
   table->iterOK        = True;
   table->name          = name;
   vg_assert(name);
   return table;
}

Int VG_(HT_count_nodes) ( VgHashTable table )
{
   return table->n_elements;
}

static void resize ( VgHashTable table )
{
   Int          i;
   SizeT        sz;
   SizeT        old_chains = table->n_chains;
   SizeT        new_chains = old_chains + 1;
   VgHashNode** chains;
   VgHashNode * node;

   /* If we've run out of primes, do nothing. */
   if (old_chains == primes[N_HASH_PRIMES-1])
      return;

   vg_assert(old_chains >= primes[0] 
             && old_chains < primes[N_HASH_PRIMES-1]);

   for (i = 0; i < N_HASH_PRIMES; i++) {
      if (primes[i] > new_chains) {
         new_chains = primes[i];
         break;
      }
   }

   vg_assert(new_chains > old_chains);
   vg_assert(new_chains > primes[0] 
             && new_chains <= primes[N_HASH_PRIMES-1]);

   VG_(debugLog)(
      1, "hashtable",
         "resizing table `%s' from %lu to %lu (total elems %lu)\n",
         table->name, (UWord)old_chains, (UWord)new_chains,
         (UWord)table->n_elements );

   table->n_chains = new_chains;
   sz = new_chains * sizeof(VgHashNode*);
   chains = VG_(calloc)("hashtable.resize.1", 1, sz);

   for (i = 0; i < old_chains; i++) {
      node = table->chains[i];
      while (node != NULL) {
         VgHashNode* next = node->next;
         UWord chain = CHAIN_NO(node->key, table);
         node->next = chains[chain];
         chains[chain] = node;
         node = next;
      }
   }

   VG_(free)(table->chains);
   table->chains = chains;
}

/* Puts a new, heap allocated VgHashNode, into the VgHashTable.  Prepends
   the node to the appropriate chain.  No duplicate key detection is done. */
void VG_(HT_add_node) ( VgHashTable table, void* vnode )
{
   VgHashNode* node     = (VgHashNode*)vnode;
   UWord chain          = CHAIN_NO(node->key, table);
   node->next           = table->chains[chain];
   table->chains[chain] = node;
   table->n_elements++;
   if ( (1 * (ULong)table->n_elements) > (1 * (ULong)table->n_chains) ) {
      resize(table);
   }

   /* Table has been modified; hence HT_Next should assert. */
   table->iterOK = False;
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
   UWord        chain         = CHAIN_NO(key, table);
   VgHashNode*  curr          =   table->chains[chain];
   VgHashNode** prev_next_ptr = &(table->chains[chain]);

   /* Table has been modified; hence HT_Next should assert. */
   table->iterOK = False;

   while (curr) {
      if (key == curr->key) {
         *prev_next_ptr = curr->next;
         table->n_elements--;
         return curr;
      }
      prev_next_ptr = &(curr->next);
      curr = curr->next;
   }
   return NULL;
}

/* Allocates a suitably-sized array, copies pointers to all the hashtable
   elements into it, then returns both the array and the size of it.  The
   array must be freed with VG_(free).
*/
VgHashNode** VG_(HT_to_array) ( VgHashTable table, /*OUT*/ UInt* n_elems )
{
   UInt       i, j;
   VgHashNode** arr;
   VgHashNode*  node;

   *n_elems = table->n_elements;
   if (*n_elems == 0)
      return NULL;

   arr = VG_(malloc)( "hashtable.Hta.1", *n_elems * sizeof(VgHashNode*) );

   j = 0;
   for (i = 0; i < table->n_chains; i++) {
      for (node = table->chains[i]; node != NULL; node = node->next) {
         arr[j++] = node;
      }
   }
   vg_assert(j == *n_elems);

   return arr;
}

void VG_(HT_ResetIter)(VgHashTable table)
{
   vg_assert(table);
   table->iterNode  = NULL;
   table->iterChain = 0;
   table->iterOK    = True;
}

void* VG_(HT_Next)(VgHashTable table)
{
   Int i;
   vg_assert(table);
   /* See long comment on HT_Next prototype in pub_tool_hashtable.h.
      In short if this fails, it means the caller tried to modify the
      table whilst iterating over it, which is a bug. */
   vg_assert(table->iterOK);

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
   UInt       i;
   VgHashNode *node, *node_next;

   for (i = 0; i < table->n_chains; i++) {
      for (node = table->chains[i]; node != NULL; node = node_next) {
         node_next = node->next;
         VG_(free)(node);
      }
   }
   VG_(free)(table->chains);
   VG_(free)(table);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
