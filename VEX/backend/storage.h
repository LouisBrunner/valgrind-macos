
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (storage.h) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __STORAGE_H
#define __STORAGE_H

/* Public header file for heap storage. */

/* Address ranges:
      -(N_HEAP_CELLS-1) .. -1        pointers into the heap
      0                              NIL
      1 .. N_TAGS-1                  tags
   All tags must be registered (register_tag) before use.
   Heap cell 0 is never used.  Tag 0 is never used.
   (0 means NIL).
*/

#include "basictypes.h"

extern void panic ( Char* );

typedef  Int   Cell;
typedef  Int   Tag;

/* Here are some common-use tags. */
#define TagCONS    1  /* arity 2 */
#define TagPAIR    2  /* arity 2 */
#define TagTRIPLE  3  /* arity 3 */
#define TagWord8   4  /* arity 1 */
#define TagWord16  5  /* arity 1 */
#define TagWord32  6  /* arity 1 */
#define TagWord64  7  /* arity 2 */


/* First you must initialise the storage subsystem.  This also
   initialises the "common use" tags. */
extern void storage_init ( void );

/* Shut down (print stats) */
extern void storage_done ( void );

/* Then you need to register some tags. */
extern void storage_register_tag ( Tag tag, Int arity, Char* name );

/* Now you can build closures using the tags you registered ..
   and take them apart again. 
*/
extern Cell tuple1 ( Tag, Cell );
extern Cell sel11 ( Cell );

extern Cell tuple2 ( Tag, Cell, Cell );
extern Cell sel21 ( Cell );
extern Cell sel22 ( Cell );

extern Cell tuple3 ( Tag, Cell, Cell, Cell );
extern Cell sel31 ( Cell );
extern Cell sel32 ( Cell );
extern Cell sel33 ( Cell );


/* Get the tag of a closure.  This must be given a reference to
   a closure header. */
extern Tag getTag ( Cell );

/* NILness */
#define NIL ((Cell)0)
#define isNil(cell)   ((cell)==NIL)
#define nonNil(cell)  (!(isNil(cell)))

/* Various helpful things to do with lists */
typedef Cell List;
extern List cons ( Cell, List );
extern List head ( List );
extern Cell tail ( List );

/* Give a name for association lists (a list of pairs). */
typedef Cell AList;

/* To do with the Word* types. */
extern Cell mkWord8  ( UInt );
extern Cell mkWord16 ( UInt );
extern Cell mkWord32 ( UInt );
extern Cell mkWord64 ( ULong );

extern UInt  getWord8  ( Cell );
extern UInt  getWord16 ( Cell );
extern UInt  getWord32 ( Cell );
extern ULong getWord64 ( Cell );


#endif /* ndef __STORAGE_H */
