
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex.h) is                                 ---*/
/*--- Copyright (c) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

#ifndef __LIBVEX_H
#define __LIBVEX_H


#include "libvex_basictypes.h"
#include "libvex_ir.h"


/*---------------------------------------------------------------*/
/*--- This file defines the top-level interface to LibVEX.    ---*/
/*---------------------------------------------------------------*/

/*-------------------------------------------------------*/
/*--- Architectures and architecture variants         ---*/
/*-------------------------------------------------------*/

typedef 
   enum { 
      VexArch_INVALID,
      VexArchX86, 
      VexArchAMD64, 
      VexArchARM,
      VexArchPPC32
   }
   VexArch;

typedef
   enum {
      VexSubArch_INVALID,
      VexSubArch_NONE,        /* Arch has no variants */
      VexSubArchX86_sse0,     /* has SSE state but no insns (Pentium II) */
      VexSubArchX86_sse1,     /* SSE1 support (Pentium III) */
      VexSubArchX86_sse2,     /* SSE2 support (Pentium 4) */
      VexSubArchARM_v4,       /* ARM version 4 */
      VexSubArchPPC32_noAV,   /* 32-bit PowerPC, no Altivec */
      VexSubArchPPC32_AV      /* 32-bit PowerPC with Altivec */
   }
   VexSubArch;

/* These return statically allocated strings. */

extern const HChar* LibVEX_ppVexArch    ( VexArch );
extern const HChar* LibVEX_ppVexSubArch ( VexSubArch );


/*-------------------------------------------------------*/
/*--- Control of Vex's optimiser (iropt).             ---*/
/*-------------------------------------------------------*/

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

extern 
void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon );


/*-------------------------------------------------------*/
/*--- Version information                             ---*/
/*-------------------------------------------------------*/

/* Returns the Vex SVN version, as a statically allocated string. */

extern const HChar* LibVEX_Version ( void );


/*-------------------------------------------------------*/
/*--- Storage management control                      ---*/
/*-------------------------------------------------------*/

/* Allocate in Vex's temporary allocation area.  Be careful with this.
   You can only call it inside an instrumentation or optimisation
   callback that you have previously specified in a call to
   LibVEX_Translate.  The storage allocated will only stay alive until
   translation of the current basic block is complete.
 */
extern void* LibVEX_Alloc ( Int nbytes );

/* Show Vex allocation statistics. */
extern void LibVEX_ShowAllocStats ( void );


/*-------------------------------------------------------*/
/*--- Describing guest state layout                   ---*/
/*-------------------------------------------------------*/

/* Describe the guest state enough that the instrumentation
   functions can work. */

/* The max number of guest state chunks which we can describe as
   always defined (for the benefit of Memcheck). */
#define VEXGLO_N_ALWAYSDEFD  19

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

/* A note about guest state layout.

   LibVEX defines the layout for the guest state, in the file
   pub/libvex_guest_<arch>.h.  The struct will have an 8-aligned size.
   Each translated bb is assumed to be entered with a specified
   register pointing at such a struct.  Beyond that is a shadow
   state area with the same size as the struct.  Beyond that is
   a spill area that LibVEX may spill into.  It must have size
   LibVEX_N_SPILL_BYTES, and this must be a 16-aligned number.

   On entry, the baseblock pointer register must be 8-aligned.
*/

#define LibVEX_N_SPILL_BYTES 1024


/*-------------------------------------------------------*/
/*--- Initialisation of the library                   ---*/
/*-------------------------------------------------------*/

/* Initialise the library.  You must call this first. */

extern void LibVEX_Init (
   /* failure exit function */
   __attribute__ ((noreturn))
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( HChar*, Int nbytes ),
   /* debug paranoia level */
   Int debuglevel,
   /* Are we supporting valgrind checking? */
   Bool valgrind_support,
   /* Control ... */
   /*READONLY*/VexControl* vcon
);


/*-------------------------------------------------------*/
/*--- Make a translation                              ---*/
/*-------------------------------------------------------*/

/* Describes the outcome of a translation attempt. */
typedef
   enum { 
      VexTransOK, 
      VexTransAccessFail, 
      VexTransOutputFull 
   }
   VexTranslateResult;


/* Describes precisely the pieces of guest code that a translation
   covers.  Now that Vex can chase across BB boundaries, the old
   scheme of describing a chunk of guest code merely by its start
   address and length is inadequate.

   Hopefully this struct is only 32 bytes long.  Space is important as
   clients will have to store one of these for each translation made.
*/
typedef
   struct {
      Addr64 base[3];
      UShort len[3];
      UShort n_used;
   }
   VexGuestExtents;


extern 
VexTranslateResult LibVEX_Translate (
   /* The instruction sets we are translating from and to. */
   VexArch    arch_guest,
   VexSubArch subarch_guest,
   VexArch    arch_host,
   VexSubArch subarch_host,
   /* IN: the block to translate, and its guest address. */
   UChar*  guest_bytes,
   Addr64  guest_bytes_addr,
   Bool    (*chase_into_ok) ( Addr64 ),
   /* OUT: which bits of guest code actually got translated */
   VexGuestExtents* guest_extents,
   /* IN: a place to put the resulting code, and its size */
   UChar*  host_bytes,
   Int     host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int*    host_bytes_used,
   /* IN: optionally, two instrumentation functions. */
   IRBB*   (*instrument1) ( IRBB*, VexGuestLayout*, 
                            IRType gWordTy, IRType hWordTy ),
   IRBB*   (*instrument2) ( IRBB*, VexGuestLayout*, 
                            IRType gWordTy, IRType hWordTy ),
   Bool    cleanup_after_instrumentation,
   /* IN: optionally, an access check function for guest code. */
   Bool    (*byte_accessible) ( Addr64 ),
   /* IN: debug: trace vex activity at various points */
   Int     traceflags
);


/*-------------------------------------------------------*/
/*--- Show accumulated statistics                     ---*/
/*-------------------------------------------------------*/

extern void LibVEX_ShowStats ( void );


/*-------------------------------------------------------*/
/*--- Notes                                           ---*/
/*-------------------------------------------------------*/

/* Code generation conventions that need to be recorded somewhere.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   x86
   ~~~
   Generated code should be entered using a CALL instruction.  On
   entry, %ebp should point to the guest state, and %esp should be a
   valid stack pointer.  The generated code may change %eax, %ebx,
   %ecx, %edx, %esi, %edi, all the FP registers and control state, and
   all the XMM registers.

   On entry, the FPU control word should be set to 0x027F, and the SSE
   control word (%mxcsr) should be set to 0x1F80.  On exit, they
   should still have those values (after masking off the lowest 6 bits
   of %mxcsr).  If they don't, there is a bug in VEX-generated code.

   Generated code returns to the scheduler using a RET instruction.
   %eax (or %eax:%edx, if simulating a 64-bit target) will contain the
   guest address of the next block to execute.

   CRITICAL ISSUES in x86 code generation.  The only known critical
   issue is that the host FPU and SSE state is not properly saved
   across calls to helper functions.  If any helper references any
   such state, it is likely (1) to misbehave itself, since the FP
   stack tags will not be as expected, and (2) after returning to
   generated code, the generated code is likely to go wrong.  This
   really should be fixed.
*/


#endif /* ndef __LIBVEX_H */

/*---------------------------------------------------------------*/
/*---                                                libvex.h ---*/
/*---------------------------------------------------------------*/
