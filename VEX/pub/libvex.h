
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


/* Control of Vex's optimiser. */

typedef
   struct {
      /* Controls verbosity of iropt.  0 = no output. */
      Int iropt_verbosity;
      /* Control aggressiveness of iropt.  0 = no opt, 1 = simple
         opts, 2 (default) = max optimisation. */
      Int iropt_level;
      /* Ensure all integer registers are up to date at potential
         memory exception points?  True(default)=yes, False=no, only
         the guest's stack pointer. */
      Bool iropt_precise_memory_exns;
      /* How aggressive should iropt be in unrolling loops?  Higher
         numbers make it more enthusiastic about loop unrolling.
         Default=120.  A setting of zero disables unrolling.  */
      Int iropt_unroll_thresh;
      /* What's the maximum basic block length the front end(s) allow?
         BBs longer than this are split up.  Default=50 (guest
         insns). */
      Int guest_max_insns;
      /* How aggressive should front ends be in following
         unconditional branches to known destinations?  Default=10,
         meaning that if a block contains less than 10 guest insns so
         far, the front end(s) will attempt to chase into its
         successor. A setting of zero disables chasing.  */
      Int guest_chase_thresh;
   }
   VexControl;


/* Write the default settings into *vcon. */
extern void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon );


/* Initialise the translator. */

extern void LibVEX_Init (
   /* failure exit function */
   __attribute__ ((noreturn))
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( Char*, Int nbytes ),
   /* debug paranoia level */
   Int debuglevel,
   /* Are we supporting valgrind checking? */
   Bool valgrind_support,
   /* Control ... */
   /*READONLY*/VexControl* vcon
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


/* Describe the guest state enough that the instrumentation
   functions can work. */

/* The max number of indexable guest state sections we can describe.
   2 is enough for x86. */
#define VEXGLO_N_DESCRS 2

/* The max number of guest state chunks which we can describe as
   always defined (for the benefit of Memcheck). */
#define VEXGLO_N_ALWAYSDEFD  16

typedef
   struct {
      /* Total size of the guest state, in bytes.  Must be
         8-aligned. */
      Int total_sizeB;
      /* Whereabouts is the stack pointer? */
      Int offset_SP;
      Int sizeof_SP; /* 4 or 8 */
      /* Whereabouts is the instruction pointer? */
      Int offset_IP;
      Int sizeof_IP; /* 4 or 8 */
      /* Describe parts of the guest state regarded as 'always
         defined'. */
      Int n_alwaysDefd;
      struct {
         Int offset;
         Int size;
      } alwaysDefd[VEXGLO_N_ALWAYSDEFD];
   }
   VexGuestLayout;


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
   Bool   (*chase_into_ok) ( Addr64 ),
   /* OUT: the number of bytes actually read */
   Int* guest_bytes_read,
   /* IN: a place to put the resulting code, and its size */
   UChar* host_bytes,
   Int    host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int* host_bytes_used,
   /* IN: optionally, two instrumentation functions. */
   IRBB* (*instrument1) ( IRBB*, VexGuestLayout*, IRType hWordTy ),
   IRBB* (*instrument2) ( IRBB*, VexGuestLayout*, IRType hWordTy ),
   Bool  cleanup_after_instrumentation,
   /* IN: optionally, an access check function for guest code. */
   Bool (*byte_accessible) ( Addr64 ),
   /* IN: debug: trace vex activity at various points */
   Int  traceflags
);


/* Show accumulated statistics. */

extern void LibVEX_ShowStats ( void );



/* A note about baseblock layout.

   LibVEX defines the layout for the guest state, in the file
   pub/libvex_guest_<arch>.h.  The struct will have an 8-aligned size.
   Each translated bb is assumed to be entered with a specified
   register pointing at such a struct.  Beyond that is a shadow
   state area with the same size as the struct.  Beyond that is
   a spill area that LibVEX may spill into.  It must have size
   LibVEX_N_SPILL_BYTES, and this will be a 16-aligned number.

   On entry, the baseblock pointer register must be 8-aligned.
*/

#define LibVEX_N_SPILL_BYTES 768


#endif /* ndef __LIBVEX_H */

/*---------------------------------------------------------------*/
/*---                                                libvex.h ---*/
/*---------------------------------------------------------------*/
