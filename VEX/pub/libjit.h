
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libjit.h) is                                 ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBJIT_H
#define __LIBJIT_H


#include "libjit_basictypes.h"
#include "libjit_ir.h"


/*---------------------------------------------------------------*/
/*--- Top-level interface to the library.                     ---*/
/*---------------------------------------------------------------*/


/* Initialise the translator. */

extern void LibJIT_Init (
   /* failure exit function */
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( Char*, Int nbytes ),
   /* debug paranoia level */
   Int debuglevel,
   /* verbosity level */
   Int verbosity,
   /* Are we supporting valgrind checking? */
   Bool valgrind_support,
   /* Max # guest insns per bb */
   Int guest_insns_per_bb
);


/* Storage management: clear the area, and allocate from it. */

extern void LibJIT_Clear ( Bool show_stats );

extern void* LibVEX_Alloc ( Int nbytes );


/* Translate a basic block. */

typedef 
   enum { InsnSetX86, InsnSetARM }
   InsnSet;

typedef
   enum { TransOK, TransAccessFail, TransOutputFull }
   TranslateResult;

extern 
TranslateResult LibJIT_Translate (
   /* The instruction sets we are translating from and to. */
   InsnSet iset_guest,
   InsnSet iset_host,
   /* IN: the block to translate, and its guest address. */
   Char*  guest_bytes,
   Addr64 guest_bytes_addr,
   /* OUT: the number of bytes actually read */
   Int* guest_bytes_read,
   /* IN: a place to put the resulting code, and its size */
   Char* host_bytes,
   Int   host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int* host_bytes_used,
   /* IN: optionally, an instrumentation function. */
   IRBB* (*instrument) ( IRBB* ),
   /* IN: optionally, an access check function for guest code. */
   Bool (*byte_accessible) ( Addr64 )
);


/* Show accumulated statistics. */

extern void LibJIT_ShowStats ( void );


#endif /* ndef __LIBJIT_H */

/*---------------------------------------------------------------*/
/*---                                                libjit.h ---*/
/*---------------------------------------------------------------*/
