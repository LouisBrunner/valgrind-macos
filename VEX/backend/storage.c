
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (storage.c) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "storage.h"
#include "storage_private.h"


static Cell heap[N_HEAP_CELLS];
static Int  heap_used = 1;  /* cell==0 is not allowed */

static Char* tag_name[N_TAGS];
static UChar tag_arity[N_TAGS]; /* 0xFF indicates unregistered. */

static Bool  init_done = False;

#define IS_VALID_CELL(cc) ((cc) < 0 && (cc) > -heap_used)
#define IS_VALID_TAG(tt)  ((tt) > 0 && (tt) < N_TAGS)


void panic ( Char* who )
{
  fprintf(stderr, "panic: %s\n", who);
  exit(1);
}


/* Initialise the storage subsystem. */
void storage_init ( void )
{
   Int i;
   assert(!init_done);
   init_done = True;
   for (i = 0; i < N_TAGS; i++) {
      tag_name[i] = NULL;
      tag_arity[i] = 0xFF; /* unregistered */
   }
   heap_used = 1;
   storage_register_tag ( TagCONS,    2, "cons" );
   storage_register_tag ( TagPAIR,    2, "pair" );
   storage_register_tag ( TagTRIPLE,  3, "triple" );
   storage_register_tag ( TagWord8,   1, "word8" );
   storage_register_tag ( TagWord16,  1, "word16" );
   storage_register_tag ( TagWord32,  1, "word32" );
   storage_register_tag ( TagWord64,  1, "word64" );
}

void storage_done ( void )
{
  printf ("storage_done: used %d cells\n", heap_used-1 );
}

void storage_register_tag ( Tag tag, Int arity, Char* name )
{
   assert(init_done);
   assert(IS_VALID_TAG(tag));
   assert(tag_arity[tag] == 0xFF);   /* check not already registered */
   assert(name != NULL);
   assert(arity >= 0 && arity <= 5);
   tag_name[tag]  = name;
   tag_arity[tag] = arity;
}


Tag getTag ( Cell c )
{
   Tag t;
   assert(IS_VALID_CELL(c));
   t = heap[-c];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] != 0xFF);
   return t;
}

/* ------------------ Construct/query for Size = 0 --------------------- */

Cell tuple0 ( Tag t )
{
   assert(init_done);
   assert(heap_used <= N_HEAP_CELLS-1);
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 0);
   heap[heap_used]   = t;
   heap_used += 1;
   return -(heap_used - 1);
}


/* ------------------ Construct/query for Size = 1 --------------------- */

Cell tuple1 ( Tag t, Cell x )
{
   assert(init_done);
   assert(heap_used <= N_HEAP_CELLS-2);
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 1);
   heap[heap_used]   = t;
   heap[heap_used+1] = x;
   heap_used += 2;
   return -(heap_used - 2);
}

Cell sel11 ( Cell tup )
{
   Tag t;
   assert(IS_VALID_CELL(tup));
   t = heap[-tup];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 1);
   return heap[-tup+1];
}

/* ------------------ Construct/query for Size = 2 --------------------- */

Cell tuple2 ( Tag t, Cell x, Cell y )
{
   assert(init_done);
   assert(heap_used <= N_HEAP_CELLS-3);
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 2);
   heap[heap_used]   = t;
   heap[heap_used+1] = x;
   heap[heap_used+2] = y;
   heap_used += 3;
   return -(heap_used - 3);
}

Cell sel21 ( Cell tup )
{
   Tag t;
   assert(IS_VALID_CELL(tup));
   t = heap[-tup];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 2);
   return heap[-tup+1];
}

Cell sel22 ( Cell tup )
{
   Tag t;
   assert(IS_VALID_CELL(tup));
   t = heap[-tup];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 2);
   return heap[-tup+2];
}


/* ------------------ Construct/query for Size = 3 --------------------- */

Cell tuple3 ( Tag t, Cell x, Cell y, Cell z )
{
   assert(init_done);
   assert(heap_used <= N_HEAP_CELLS-4);
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 3);
   heap[heap_used]   = t;
   heap[heap_used+1] = x;
   heap[heap_used+2] = y;
   heap[heap_used+3] = z;
   heap_used += 4;
   return -(heap_used - 4);
}

Cell sel31 ( Cell tup )
{
   Tag t;
   assert(IS_VALID_CELL(tup));
   t = heap[-tup];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 3);
   return heap[-tup+1];
}

Cell sel32 ( Cell tup )
{
   Tag t;
   assert(IS_VALID_CELL(tup));
   t = heap[-tup];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 3);
   return heap[-tup+2];
}

Cell sel33 ( Cell tup )
{
   Tag t;
   assert(IS_VALID_CELL(tup));
   t = heap[-tup];
   assert(IS_VALID_TAG(t));
   assert(tag_arity[t] == 3);
   return heap[-tup+3];
}


/* ------------------ Helpers for lists --------------------- */

Cell cons ( Cell hd, Cell tl ) 
{ 
  return tuple2(TagCONS, hd, tl); 
}

Cell head ( Cell list ) 
{
  if (isNil(list))             panic("head(NIL)");
  if (getTag(list) != TagCONS) panic("head(not-a-list)");
  return sel21(list);
}

Cell tail ( Cell list ) 
{
  if (isNil(list))             panic("tail(NIL)");
  if (getTag(list) != TagCONS) panic("tail(not-a-list)");
  return sel22(list);
}


/* ------------------ Helpers for Word* --------------------- */

Cell mkWord8 ( UInt w8 )
{
  return tuple1 ( TagWord8, w8 & 0xFF );
}

Cell mkWord16 ( UInt w16 )
{
  return tuple1 ( TagWord16, w16 & 0xFFFF );
}

Cell mkWord32 ( UInt w32 )
{
  return tuple1 ( TagWord32, w32 );
}

Cell mkWord64 ( ULong w64 )
{
  UInt hi = (UInt)(w64 >> 32);
  UInt lo = (UInt)(w64 & 0x00000000FFFFFFFFLL);
  return tuple2 ( TagWord64, hi, lo );
}

UInt getWord8 ( Cell c )
{
  assert(getTag(c) == TagWord8 );
  return sel11(c);
}

UInt getWord16 ( Cell c )
{
  assert(getTag(c) == TagWord16 );
  return sel11(c);
}

UInt getWord32 ( Cell c )
{
  assert(getTag(c) == TagWord32 );
  return sel11(c);
}

ULong getWord64 ( Cell c )
{
  UInt hi, lo;
  assert(getTag(c) == TagWord64 );
  hi = (UInt)sel21(c);
  lo = (UInt)sel22(c);
  return (((ULong)hi) << 32) | ((ULong)lo);
}

