
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex.h) is                                 ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_H
#define __LIBVEX_H


#include "libvex_basictypes.h"
#include "libvex_ir.h"


/*---------------------------------------------------------------*/
/*--- Top-level interface to the library.                     ---*/
/*---------------------------------------------------------------*/


/* Initialise the translator. */

extern void LibVEX_Init (
   /* failure exit function */
   __attribute__ ((noreturn))
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

/* By default allocation occurs in the temporary area.  However, it is
   possible to switch to permanent area allocation if that's what you
   want.  Permanent area allocation is very limited, tho. */

typedef
   enum { AllocModeTEMPORARY, AllocModePERMANENT }
   AllocMode;

extern void      LibVEX_SetAllocMode ( AllocMode );
extern AllocMode LibVEX_GetAllocMode ( void );

extern void LibVEX_ClearTemporary ( Bool show_stats );

extern void* LibVEX_Alloc ( Int nbytes );


/* Translate a basic block. */

typedef 
   enum { InsnSetX86, InsnSetARM }
   InsnSet;

typedef
   enum { TransOK, TransAccessFail, TransOutputFull }
   TranslateResult;

extern 
TranslateResult LibVEX_Translate (
   /* The instruction sets we are translating from and to. */
   InsnSet iset_guest,
   InsnSet iset_host,
   /* IN: the block to translate, and its guest address. */
   UChar* guest_bytes,
   Addr64 guest_bytes_addr,
   /* OUT: the number of bytes actually read */
   Int* guest_bytes_read,
   /* IN: a place to put the resulting code, and its size */
   UChar* host_bytes,
   Int    host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int* host_bytes_used,
   /* IN: optionally, an instrumentation function. */
   IRBB* (*instrument) ( IRBB* ),
   /* IN: optionally, an access check function for guest code. */
   Bool (*byte_accessible) ( Addr64 ),
   /* IN: if > 0, use this verbosity for this bb */
   Int  bb_verbosity
);


/* Show accumulated statistics. */

extern void LibVEX_ShowStats ( void );


#endif /* ndef __LIBVEX_H */

/*---------------------------------------------------------------*/
/*---                                                libvex.h ---*/
/*---------------------------------------------------------------*/
