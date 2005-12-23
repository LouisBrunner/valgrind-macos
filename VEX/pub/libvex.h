
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex.h) is                                 ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __LIBVEX_H
#define __LIBVEX_H


#include "libvex_basictypes.h"
#include "libvex_ir.h"


/*---------------------------------------------------------------*/
/*--- This file defines the top-level interface to LibVEX.    ---*/
/*---------------------------------------------------------------*/

/*-------------------------------------------------------*/
/*--- Architectures, variants, and other arch info    ---*/
/*-------------------------------------------------------*/

typedef 
   enum { 
      VexArch_INVALID,
      VexArchX86, 
      VexArchAMD64, 
      VexArchARM,
      VexArchPPC32,
      VexArchPPC64
   }
   VexArch;

typedef
   enum {
      VexSubArch_INVALID,
      VexSubArch_NONE,        /* Arch has no variants */
      VexSubArchX86_sse0,     /* no SSE state; or SSE state but no insns */
      VexSubArchX86_sse1,     /* SSE1 support (Pentium III) */
      VexSubArchX86_sse2,     /* SSE2 support (Pentium 4) */
      VexSubArchARM_v4,       /* ARM version 4 */
      VexSubArchPPC32_I,      /* 32-bit PowerPC, no FP, no Altivec */
      VexSubArchPPC32_FI,     /* 32-bit PowerPC, with FP but no Altivec */
      VexSubArchPPC32_VFI,    /* 32-bit PowerPC, with FP and Altivec */
      VexSubArchPPC64_FI,     /* 64-bit PowerPC, with FP but no Altivec */
      VexSubArchPPC64_VFI     /* 64-bit PowerPC, with FP and Altivec */
   }
   VexSubArch;

/* These return statically allocated strings. */

extern const HChar* LibVEX_ppVexArch    ( VexArch );
extern const HChar* LibVEX_ppVexSubArch ( VexSubArch );


/* This struct is a bit of a hack, but is needed to carry misc
   important bits of info about an arch.  Fields which are optional or
   ignored on some arch should be set to zero. */

typedef
   struct {
      /* This is the only mandatory field. */
      VexSubArch subarch;
      /* PPC32/PPC64 only: size of cache line */
      Int ppc_cache_line_szB;
   }
   VexArchInfo;

/* Write default settings info *vai. */
extern 
void LibVEX_default_VexArchInfo ( /*OUT*/VexArchInfo* vai );


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
extern HChar* private_LibVEX_alloc_first;
extern HChar* private_LibVEX_alloc_curr;
extern HChar* private_LibVEX_alloc_last;
extern void   private_LibVEX_alloc_OOM(void) __attribute__((noreturn));

static inline void* LibVEX_Alloc ( Int nbytes )
{
#if 0
  /* Nasty debugging hack, do not use. */
  return malloc(nbytes);
#else
   HChar* curr;
   HChar* next;
   Int    ALIGN;
   ALIGN  = sizeof(void*)-1;
   nbytes = (nbytes + ALIGN) & ~ALIGN;
   curr   = private_LibVEX_alloc_curr;
   next   = curr + nbytes;
   if (next >= private_LibVEX_alloc_last)
      private_LibVEX_alloc_OOM();
   private_LibVEX_alloc_curr = next;
   return curr;
#endif
}

/* Show Vex allocation statistics. */
extern void LibVEX_ShowAllocStats ( void );


/*-------------------------------------------------------*/
/*--- Describing guest state layout                   ---*/
/*-------------------------------------------------------*/

/* Describe the guest state enough that the instrumentation
   functions can work. */

/* The max number of guest state chunks which we can describe as
   always defined (for the benefit of Memcheck). */
#define VEXGLO_N_ALWAYSDEFD  22

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


/* A structure to carry arguments for LibVEX_Translate.  There are so
   many of them, it seems better to have a structure. */
typedef
   struct {
      /* IN: The instruction sets we are translating from and to. */
      VexArch      arch_guest;
      VexArchInfo  archinfo_guest;
      VexArch      arch_host;
      VexArchInfo  archinfo_host;

      /* IN: the block to translate, and its guest address. */
      /* where are the actual bytes in the host's address space? */
      UChar*  guest_bytes;
      /* where do the bytes really come from in the guest's aspace?
         This is the post-redirection guest address. */
      Addr64  guest_bytes_addr;
      /* where do the bytes claim to come from in the guest address
         space?  (what guest entry point address do they correspond
         to?)  This is the pre-redirection guest address. */
      Addr64  guest_bytes_addr_noredir;

      /* Is it OK to chase into this guest address?  May not be
	 NULL. */
      Bool    (*chase_into_ok) ( Addr64 );

      /* OUT: which bits of guest code actually got translated */
      VexGuestExtents* guest_extents;

      /* IN: a place to put the resulting code, and its size */
      UChar*  host_bytes;
      Int     host_bytes_size;
      /* OUT: how much of the output area is used. */
      Int*    host_bytes_used;

      /* IN: optionally, two instrumentation functions.  May be
	 NULL. */
      IRBB*   (*instrument1) ( IRBB*, VexGuestLayout*, 
                               Addr64, VexGuestExtents*,
                               IRType gWordTy, IRType hWordTy );
      IRBB*   (*instrument2) ( IRBB*, VexGuestLayout*, 
                               Addr64, VexGuestExtents*,
                               IRType gWordTy, IRType hWordTy );

      /* IN: should this translation be self-checking?  default: False */
      Bool    do_self_check;
      /* IN: debug: trace vex activity at various points */
      Int     traceflags;

      /* IN: address of the dispatcher entry point.  Describes the
         place where generated code should jump to at the end of each
         bb.

         At the end of each translation, the next guest address is
         placed in the host's standard return register (x86: %eax,
         amd64: %rax, ppc32: %r3, ppc64: %r3).  Optionally, the guest
         state pointer register (on host x86: %ebp; amd64: %rbp;
         ppc32/64: r31) may be set to a VEX_TRC_ value to indicate any
         special action required before the next block is run.

         Control is then passed back to the dispatcher (beyond Vex's
         control; caller supplies this) in the following way:

         - On host archs which lack a link register (x86, amd64), by a
           jump to the host address specified in 'dispatcher', which
           must be non-NULL.

         - On host archs which have a link register (ppc32, ppc64), by
           a branch to the link register (which is guaranteed to be
           unchanged from whatever it was at entry to the
           translation).  'dispatch' must be NULL.

         The aim is to get back and forth between translations and the
         dispatcher without creating memory traffic to store return
         addresses.
      */
      void* dispatch;
   }
   VexTranslateArgs;


extern 
VexTranslateResult LibVEX_Translate ( VexTranslateArgs* );


/* A subtlety re interaction between self-checking translations and
   bb-chasing.  The supplied chase_into_ok function should say NO
   (False) when presented with any address for which you might want to
   make a self-checking translation.

   If it doesn't do that, you may end up with Vex chasing from BB #1
   to BB #2 (fine); but if you wanted checking for #2 and not #1, that
   would not be the result.  Therefore chase_into_ok should disallow
   following into #2.  That will force the caller to eventually
   request a new translation starting at #2, at which point Vex will
   correctly observe the make-a-self-check flag.  */


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
   Generated code should be entered using a JMP instruction.  On
   entry, %ebp should point to the guest state, and %esp should be a
   valid stack pointer.  The generated code may change %eax, %ebx,
   %ecx, %edx, %esi, %edi, all the FP registers and control state, and
   all the XMM registers.

   On entry, the FPU control word should be set to 0x027F, and the SSE
   control word (%mxcsr) should be set to 0x1F80.  On exit, they
   should still have those values (after masking off the lowest 6 bits
   of %mxcsr).  If they don't, there is a bug in VEX-generated code.

   Generated code returns to the scheduler using a JMP instruction, to
   the address specified in the .dispatch field of VexTranslateArgs.
   %eax (or %eax:%edx, if simulating a 64-bit target) will contain the
   guest address of the next block to execute.  %ebp may be changed
   to a VEX_TRC_ value, otherwise it should be as it was at entry.

   CRITICAL ISSUES in x86 code generation.  The only known critical
   issue is that the host FPU and SSE state is not properly saved
   across calls to helper functions.  If any helper references any
   such state, it is likely (1) to misbehave itself, since the FP
   stack tags will not be as expected, and (2) after returning to
   generated code, the generated code is likely to go wrong.  This
   really should be fixed.

   amd64
   ~~~~~
   Analogous to x86.

   ppc32
   ~~~~~
   On entry, guest state pointer is r31.  .dispatch must be NULL.
   Control is returned with a branch to the link register.  Generated
   code will not change lr.  At return, r3 holds the next guest addr
   (or r3:r4 ?).  r31 may be may be changed to a VEX_TRC_ value,
   otherwise it should be as it was at entry.

   ppc64
   ~~~~~
   Same as ppc32.

   ALL GUEST ARCHITECTURES
   ~~~~~~~~~~~~~~~~~~~~~~~
   The architecture must contain two pseudo-registers, guest_TISTART
   and guest_TILEN.  These are used to pass the address of areas of
   guest code, translations of which are to be invalidated, back to
   the despatcher.  Both pseudo-regs must have size equal to the guest
   word size.
*/
#endif /* ndef __LIBVEX_H */

/*---------------------------------------------------------------*/
/*---                                                libvex.h ---*/
/*---------------------------------------------------------------*/
