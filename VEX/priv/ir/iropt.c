
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/iropt.c) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main/vex_util.h"


/*---------------------------------------------------------------*/
/*--- Finite mappery, of a sort                               ---*/
/*---------------------------------------------------------------*/

/* General map from 64-bit thing 64-bit thing.  Could be done faster
   by hashing. */

typedef
   struct {
      Bool*  inuse;
      ULong* key;
      ULong* val;
      Int    size;
      Int    used;
   }
   Hash64;

static Hash64* newH64 ( void )
{
   Hash64* h = LibVEX_Alloc(sizeof(Hash64));
   h->size   = 4;
   h->used   = 0;
   h->inuse  = LibVEX_Alloc(h->size * sizeof(Bool));
   h->key    = LibVEX_Alloc(h->size * sizeof(ULong));
   h->val    = LibVEX_Alloc(h->size * sizeof(ULong));
   return h;
}


/* Lookup key in the map. */

static Bool lookupH64 ( /*OUT*/ULong* val, Hash64* h, ULong key )
{
   Int i;
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         *val = h->val[i];
         return True;
      }
   }
   return False;
}


/* Delete any binding for key in h. */

static void delFromH64 ( Hash64* h, ULong key )
{
   Int i;
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         h->inuse[i] = False;
         return;
      }
   }
}



/* Add key->val to the map.  Replaces any existing binding for key. */

static void addToH64 ( Hash64* h, ULong key, ULong val )
{
   Int i, j;

   /* Find and replace existing binding, if any. */
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         h->val[i] = val;
         return;
      }
   }

   /* Ensure a space is available. */
   if (h->used == h->size) {
      /* Copy into arrays twice the size. */
      Bool*  inuse2 = LibVEX_Alloc(2 * h->size * sizeof(Bool));
      ULong* key2   = LibVEX_Alloc(2 * h->size * sizeof(ULong));
      ULong* val2   = LibVEX_Alloc(2 * h->size * sizeof(ULong));
      for (i = j = 0; i < h->size; i++) {
         if (!h->inuse[i]) continue;
         inuse2[j] = True;
         key2[j] = h->key[i];
         val2[j] = h->val[i];
         j++;
      }
      h->used = j;
      h->size *= 2;
      h->inuse = inuse2;
      h->key = key2;
      h->val = val2;
   }

   /* Finally, add it. */
   vassert(h->used < h->size);
   h->inuse[h->used] = True;
   h->key[h->used] = key;
   h->key[h->used] = val;
   h->used++;
}

/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.c ---*/
/*---------------------------------------------------------------*/
