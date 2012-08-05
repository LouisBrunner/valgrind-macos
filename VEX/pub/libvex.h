
/*---------------------------------------------------------------*/
/*--- begin                                          libvex.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2012 OpenWorks LLP
      info@open-works.net

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

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
      VexArchPPC64,
      VexArchS390X,
      VexArchMIPS32
   }
   VexArch;


/* For a given architecture, these specify extra capabilities beyond
   the minimum supported (baseline) capabilities.  They may be OR'd
   together, although some combinations don't make sense.  (eg, SSE2
   but not SSE1).  LibVEX_Translate will check for nonsensical
   combinations. */

/* x86: baseline capability is Pentium-1 (FPU, MMX, but no SSE), with
   cmpxchg8b. */
#define VEX_HWCAPS_X86_SSE1    (1<<1)  /* SSE1 support (Pentium III) */
#define VEX_HWCAPS_X86_SSE2    (1<<2)  /* SSE2 support (Pentium 4) */
#define VEX_HWCAPS_X86_SSE3    (1<<3)  /* SSE3 support (>= Prescott) */
#define VEX_HWCAPS_X86_LZCNT   (1<<4)  /* SSE4a LZCNT insn */

/* amd64: baseline capability is SSE2, with cmpxchg8b but not
   cmpxchg16b. */
#define VEX_HWCAPS_AMD64_SSE3  (1<<5)  /* SSE3 support */
#define VEX_HWCAPS_AMD64_CX16  (1<<6)  /* cmpxchg16b support */
#define VEX_HWCAPS_AMD64_LZCNT (1<<7)  /* SSE4a LZCNT insn */
#define VEX_HWCAPS_AMD64_AVX   (1<<8)  /* AVX instructions */

/* ppc32: baseline capability is integer only */
#define VEX_HWCAPS_PPC32_F     (1<<8)  /* basic (non-optional) FP */
#define VEX_HWCAPS_PPC32_V     (1<<9)  /* Altivec (VMX) */
#define VEX_HWCAPS_PPC32_FX    (1<<10) /* FP extns (fsqrt, fsqrts) */
#define VEX_HWCAPS_PPC32_GX    (1<<11) /* Graphics extns
                                          (fres,frsqrte,fsel,stfiwx) */
#define VEX_HWCAPS_PPC32_VX    (1<<12) /* Vector-scalar floating-point (VSX); implies ISA 2.06 or higher  */
#define VEX_HWCAPS_PPC32_DFP   (1<<17) /* Decimal Floating Point (DFP) -- e.g., dadd */

/* ppc64: baseline capability is integer and basic FP insns */
#define VEX_HWCAPS_PPC64_V     (1<<13) /* Altivec (VMX) */
#define VEX_HWCAPS_PPC64_FX    (1<<14) /* FP extns (fsqrt, fsqrts) */
#define VEX_HWCAPS_PPC64_GX    (1<<15) /* Graphics extns
                                          (fres,frsqrte,fsel,stfiwx) */
#define VEX_HWCAPS_PPC64_VX    (1<<16) /* Vector-scalar floating-point (VSX); implies ISA 2.06 or higher  */
#define VEX_HWCAPS_PPC64_DFP   (1<<18) /* Decimal Floating Point (DFP) -- e.g., dadd */

/* s390x: Hardware capability encoding

   Bits [26:31] encode the machine model (see VEX_S390X_MODEL... below)
   Bits [0:20]  encode specific hardware capabilities
                (see VEX_HWAPS_S390X_... below)
*/

/* Model numbers must be assigned in chronological order.
   They are used as array index. */
#define VEX_S390X_MODEL_Z900     0
#define VEX_S390X_MODEL_Z800     1
#define VEX_S390X_MODEL_Z990     2
#define VEX_S390X_MODEL_Z890     3
#define VEX_S390X_MODEL_Z9_EC    4
#define VEX_S390X_MODEL_Z9_BC    5
#define VEX_S390X_MODEL_Z10_EC   6
#define VEX_S390X_MODEL_Z10_BC   7
#define VEX_S390X_MODEL_Z196     8
#define VEX_S390X_MODEL_Z114     9
#define VEX_S390X_MODEL_UNKNOWN  10     /* always last in list */
#define VEX_S390X_MODEL_MASK     0x3F

#define VEX_HWCAPS_S390X_LDISP (1<<6)   /* Long-displacement facility */
#define VEX_HWCAPS_S390X_EIMM  (1<<7)   /* Extended-immediate facility */
#define VEX_HWCAPS_S390X_GIE   (1<<8)   /* General-instruction-extension facility */
#define VEX_HWCAPS_S390X_DFP   (1<<9)   /* Decimal floating point facility */
#define VEX_HWCAPS_S390X_FGX   (1<<10)  /* FPR-GR transfer facility */
#define VEX_HWCAPS_S390X_ETF2  (1<<11)  /* ETF2-enhancement facility */
#define VEX_HWCAPS_S390X_STFLE (1<<12)  /* STFLE facility */
#define VEX_HWCAPS_S390X_ETF3  (1<<13)  /* ETF3-enhancement facility */

/* Special value representing all available s390x hwcaps */
#define VEX_HWCAPS_S390X_ALL   (VEX_HWCAPS_S390X_LDISP | \
                                VEX_HWCAPS_S390X_EIMM  | \
                                VEX_HWCAPS_S390X_GIE   | \
                                VEX_HWCAPS_S390X_DFP   | \
                                VEX_HWCAPS_S390X_FGX   | \
                                VEX_HWCAPS_S390X_STFLE | \
                                VEX_HWCAPS_S390X_ETF3  | \
                                VEX_HWCAPS_S390X_ETF2)

#define VEX_HWCAPS_S390X(x)  ((x) & ~VEX_S390X_MODEL_MASK)
#define VEX_S390X_MODEL(x)   ((x) &  VEX_S390X_MODEL_MASK)

/* arm: baseline capability is ARMv4 */
/* Bits 5:0 - architecture level (e.g. 5 for v5, 6 for v6 etc) */
#define VEX_HWCAPS_ARM_VFP    (1<<6)  /* VFP extension */
#define VEX_HWCAPS_ARM_VFP2   (1<<7)  /* VFPv2 */
#define VEX_HWCAPS_ARM_VFP3   (1<<8)  /* VFPv3 */
/* Bits 15:10 reserved for (possible) future VFP revisions */
#define VEX_HWCAPS_ARM_NEON   (1<<16) /* Advanced SIMD also known as NEON */

/* Get an ARM architecure level from HWCAPS */
#define VEX_ARM_ARCHLEVEL(x) ((x) & 0x3f)

/* MIPS baseline capability */
/* Assigned Company values for bits 23:16 of the PRId Register
   (CP0 register 15, select 0).  As of the MIPS32 and MIPS64 specs from
   MTI, the PRId register is defined in this (backwards compatible)
   way:

  +----------------+----------------+----------------+----------------+
  | Company Options| Company ID     | Processor ID   | Revision       |
  +----------------+----------------+----------------+----------------+
   31            24 23            16 15             8 7

*/

#define VEX_PRID_COMP_MIPS      0x00010000
#define VEX_PRID_COMP_BROADCOM  0x00020000

/* These return statically allocated strings. */

extern const HChar* LibVEX_ppVexArch    ( VexArch );
extern const HChar* LibVEX_ppVexHwCaps  ( VexArch, UInt );


/* This struct is a bit of a hack, but is needed to carry misc
   important bits of info about an arch.  Fields which are meaningless
   or ignored for the platform in question should be set to zero. */

typedef
   struct {
      /* This is the only mandatory field. */
      UInt hwcaps;
      /* PPC32/PPC64 only: size of cache line */
      Int ppc_cache_line_szB;
      /* PPC32/PPC64 only: sizes zeroed by the dcbz/dcbzl instructions
       * (bug#135264) */
      UInt ppc_dcbz_szB;
      UInt ppc_dcbzl_szB; /* 0 means unsupported (SIGILL) */
   }
   VexArchInfo;

/* Write default settings info *vai. */
extern 
void LibVEX_default_VexArchInfo ( /*OUT*/VexArchInfo* vai );


/* This struct carries guest and host ABI variant information that may
   be needed.  Fields which are meaningless or ignored for the
   platform in question should be set to zero.

   Settings which are believed to be correct are:

   guest_stack_redzone_size
      guest is ppc32-linux                ==> 0
      guest is ppc64-linux                ==> 288
      guest is ppc32-aix5                 ==> 220
      guest is ppc64-aix5                 ==> unknown
      guest is amd64-linux                ==> 128
      guest is other                      ==> inapplicable

   guest_amd64_assume_fs_is_zero
      guest is amd64-linux                ==> True
      guest is amd64-darwin               ==> False
      guest is other                      ==> inapplicable

   guest_amd64_assume_gs_is_0x60
      guest is amd64-darwin               ==> True
      guest is amd64-linux                ==> False
      guest is other                      ==> inapplicable

   guest_ppc_zap_RZ_at_blr
      guest is ppc64-linux                ==> True
      guest is ppc32-linux                ==> False
      guest is ppc64-aix5                 ==> unknown
      guest is ppc32-aix5                 ==> False
      guest is other                      ==> inapplicable

   guest_ppc_zap_RZ_at_bl
      guest is ppc64-linux                ==> const True
      guest is ppc32-linux                ==> const False
      guest is ppc64-aix5                 ==> unknown
      guest is ppc32-aix5                 ==> True except for calls to
                                              millicode, $SAVEFn, $RESTFn
      guest is other                      ==> inapplicable

   guest_ppc_sc_continues_at_LR:
      guest is ppc32-aix5  or ppc64-aix5  ==> True
      guest is ppc32-linux or ppc64-linux ==> False
      guest is other                      ==> inapplicable

   host_ppc_calls_use_fndescrs:
      host is ppc32-linux                 ==> False
      host is ppc64-linux                 ==> True
      host is ppc32-aix5 or ppc64-aix5    ==> True
      host is other                       ==> inapplicable

   host_ppc32_regalign_int64_args:
      host is ppc32-linux                 ==> True
      host is ppc32-aix5                  ==> False
      host is other                       ==> inapplicable
*/

typedef
   struct {
      /* PPC and AMD64 GUESTS only: how many bytes below the 
         stack pointer are validly addressible? */
      Int guest_stack_redzone_size;

      /* AMD64 GUESTS only: should we translate %fs-prefixed
         instructions using the assumption that %fs always contains
         zero? */
      Bool guest_amd64_assume_fs_is_zero;

      /* AMD64 GUESTS only: should we translate %gs-prefixed
         instructions using the assumption that %gs always contains
         0x60? */
      Bool guest_amd64_assume_gs_is_0x60;

      /* PPC GUESTS only: should we zap the stack red zone at a 'blr'
         (function return) ? */
      Bool guest_ppc_zap_RZ_at_blr;

      /* PPC GUESTS only: should we zap the stack red zone at a 'bl'
         (function call) ?  Is supplied with the guest address of the
         target of the call since that may be significant.  If NULL,
         is assumed equivalent to a fn which always returns False. */
      Bool (*guest_ppc_zap_RZ_at_bl)(Addr64);

      /* PPC32/PPC64 GUESTS only: where does the kernel resume after
         'sc'?  False => Linux style, at the next insn.  True => AIX
         style, at the address stated in the link register. */
      Bool guest_ppc_sc_continues_at_LR;

      /* PPC32/PPC64 HOSTS only: does '&f' give us a pointer to a
         function descriptor on the host, or to the function code
         itself?  True => descriptor, False => code. */
      Bool host_ppc_calls_use_fndescrs;

      /* PPC32 HOSTS only: when generating code to pass a 64-bit value
         (actual parameter) in a pair of regs, should we skip an arg
         reg if it is even-numbered?  True => yes, False => no. */
      Bool host_ppc32_regalign_int64_args;
   }
   VexAbiInfo;

/* Write default settings info *vbi. */
extern 
void LibVEX_default_VexAbiInfo ( /*OUT*/VexAbiInfo* vbi );


/*-------------------------------------------------------*/
/*--- Control of Vex's optimiser (iropt).             ---*/
/*-------------------------------------------------------*/


/* VexRegisterUpdates specifies when to ensure that the guest state is
   up to date.

   VexRegUpdUnwindregsAtMemAccess : registers needed to make a stack trace are
   up to date at memory exception points.  Typically, these are PC/SP/FP. The
   minimal registers are described by the arch specific functions
   guest_<arch>_state_requires_precise_mem_exns.

   VexRegUpdAllregsAtMemAccess : all registers up to date at memory exception
   points.

   VexRegUpdAllregsAtEachInsn : all registers up to date at each instruction. */
typedef enum { VexRegUpdUnwindregsAtMemAccess,
               VexRegUpdAllregsAtMemAccess,
               VexRegUpdAllregsAtEachInsn } VexRegisterUpdates;

/* Control of Vex's optimiser. */

typedef
   struct {
      /* Controls verbosity of iropt.  0 = no output. */
      Int iropt_verbosity;
      /* Control aggressiveness of iropt.  0 = no opt, 1 = simple
         opts, 2 (default) = max optimisation. */
      Int iropt_level;
      /* Controls when registers are updated in guest state. */
      VexRegisterUpdates iropt_register_updates;
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
      /* EXPERIMENTAL: chase across conditional branches?  Not all
         front ends honour this.  Default: NO. */
      Bool guest_chase_cond;
   }
   VexControl;


/* Write the default settings into *vcon. */

extern 
void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon );


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
   struct align {
      char c;
      union {
         char c;
         short s;
         int i;
         long l;
         long long ll;
         float f;
         double d;
         /* long double is currently not used and would increase alignment
            unnecessarily. */
         /* long double ld; */
         void *pto;
         void (*ptf)(void);
      } x;
   };

#if 0
  /* Nasty debugging hack, do not use. */
  return malloc(nbytes);
#else
   HChar* curr;
   HChar* next;
   Int    ALIGN;
   ALIGN  = offsetof(struct align,x) - 1;
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
#define VEXGLO_N_ALWAYSDEFD  24

typedef
   struct {
      /* Total size of the guest state, in bytes.  Must be
         8-aligned. */
      Int total_sizeB;
      /* Whereabouts is the stack pointer? */
      Int offset_SP;
      Int sizeof_SP; /* 4 or 8 */
      /* Whereabouts is the frame pointer? */
      Int offset_FP;
      Int sizeof_FP; /* 4 or 8 */
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
   pub/libvex_guest_<arch>.h.  The struct will have an 16-aligned
   size.  Each translated bb is assumed to be entered with a specified
   register pointing at such a struct.  Beyond that is two copies of
   the shadow state area with the same size as the struct.  Beyond
   that is a spill area that LibVEX may spill into.  It must have size
   LibVEX_N_SPILL_BYTES, and this must be a 16-aligned number.

   On entry, the baseblock pointer register must be 16-aligned.

   There must be no holes in between the primary guest state, its two
   copies, and the spill area.  In short, all 4 areas must have a
   16-aligned size and be 16-aligned, and placed back-to-back.
*/

#define LibVEX_N_SPILL_BYTES 4096


/*-------------------------------------------------------*/
/*--- Initialisation of the library                   ---*/
/*-------------------------------------------------------*/

/* Initialise the library.  You must call this first. */

extern void LibVEX_Init (

   /* failure exit function */
#  if __cplusplus == 1 && __GNUC__ && __GNUC__ <= 3
   /* g++ 3.x doesn't understand attributes on function parameters.
      See #265762. */
#  else
   __attribute__ ((noreturn))
#  endif
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
   struct {
      /* overall status */
      enum { VexTransOK,
             VexTransAccessFail, VexTransOutputFull } status;
      /* The number of extents that have a self-check (0 to 3) */
      UInt n_sc_extents;
      /* Offset in generated code of the profile inc, or -1 if
         none.  Needed for later patching. */
      Int offs_profInc;
      /* Stats only: the number of guest insns included in the
         translation.  It may be zero (!). */
      UInt n_guest_instrs;
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
      /* IN: The instruction sets we are translating from and to.  And
         guest/host misc info. */
      VexArch      arch_guest;
      VexArchInfo  archinfo_guest;
      VexArch      arch_host;
      VexArchInfo  archinfo_host;
      VexAbiInfo   abiinfo_both;

      /* IN: an opaque value which is passed as the first arg to all
         callback functions supplied in this struct.  Vex has no idea
         what's at the other end of this pointer. */
      void*   callback_opaque;

      /* IN: the block to translate, and its guest address. */
      /* where are the actual bytes in the host's address space? */
      UChar*  guest_bytes;
      /* where do the bytes really come from in the guest's aspace?
         This is the post-redirection guest address.  Not that Vex
         understands anything about redirection; that is all done on
         the Valgrind side. */
      Addr64  guest_bytes_addr;

      /* Is it OK to chase into this guest address?  May not be
	 NULL. */
      Bool    (*chase_into_ok) ( /*callback_opaque*/void*, Addr64 );

      /* OUT: which bits of guest code actually got translated */
      VexGuestExtents* guest_extents;

      /* IN: a place to put the resulting code, and its size */
      UChar*  host_bytes;
      Int     host_bytes_size;
      /* OUT: how much of the output area is used. */
      Int*    host_bytes_used;

      /* IN: optionally, two instrumentation functions.  May be
	 NULL. */
      IRSB*   (*instrument1) ( /*callback_opaque*/void*, 
                               IRSB*, 
                               VexGuestLayout*, 
                               VexGuestExtents*,
                               IRType gWordTy, IRType hWordTy );
      IRSB*   (*instrument2) ( /*callback_opaque*/void*, 
                               IRSB*, 
                               VexGuestLayout*, 
                               VexGuestExtents*,
                               IRType gWordTy, IRType hWordTy );

      IRSB* (*finaltidy) ( IRSB* );

      /* IN: a callback used to ask the caller which of the extents,
         if any, a self check is required for.  Must not be NULL.
         The returned value is a bitmask with a 1 in position i indicating
         that the i'th extent needs a check.  Since there can be at most
         3 extents, the returned values must be between 0 and 7. */
      UInt (*needs_self_check)( /*callback_opaque*/void*,
                                VexGuestExtents* );

      /* IN: optionally, a callback which allows the caller to add its
         own IR preamble following the self-check and any other
         VEX-generated preamble, if any.  May be NULL.  If non-NULL,
         the IRSB under construction is handed to this function, which
         presumably adds IR statements to it.  The callback may
         optionally complete the block and direct bb_to_IR not to
         disassemble any instructions into it; this is indicated by
         the callback returning True.
      */
      Bool    (*preamble_function)(/*callback_opaque*/void*, IRSB*);

      /* IN: debug: trace vex activity at various points */
      Int     traceflags;

      /* IN: profiling: add a 64 bit profiler counter increment to the
         translation? */
      Bool    addProfInc;

      /* IN: address of the dispatcher entry points.  Describes the
         places where generated code should jump to at the end of each
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
           jump to the host address specified in
           'dispatcher_assisted', if the guest state pointer has been
           changed so as to request some action before the next block
           is run, or 'dispatcher_unassisted' (the fast path), in
           which it is assumed that the guest state pointer is
           unchanged and we wish to continue directly with the next
           translation.  Both of these must be non-NULL.

         - On host archs which have a link register (ppc32, ppc64), by
           a branch to the link register (which is guaranteed to be
           unchanged from whatever it was at entry to the
           translation).  'dispatch_assisted' and
           'dispatch_unassisted' must be NULL.

         The aim is to get back and forth between translations and the
         dispatcher without creating memory traffic to store return
         addresses.

         FIXME: update this comment
      */
      void* disp_cp_chain_me_to_slowEP;
      void* disp_cp_chain_me_to_fastEP;
      void* disp_cp_xindir;
      void* disp_cp_xassisted;
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
   correctly observe the make-a-self-check flag.

   FIXME: is this still up to date? */


/*-------------------------------------------------------*/
/*--- Patch existing translations                     ---*/
/*-------------------------------------------------------*/

/* Indicates a host address range for which callers to the functions
   below must request I-D cache syncing after the call.  ::len == 0 is
   ambiguous -- it could mean either zero bytes or the entire address
   space, so we mean the former. */
typedef
   struct {
      HWord start;
      HWord len;
   }
   VexInvalRange;

/* Chain an XDirect jump located at place_to_chain so it jumps to
   place_to_jump_to.  It is expected (and checked) that this site
   currently contains a call to the dispatcher specified by
   disp_cp_chain_me_EXPECTED. */
extern
VexInvalRange LibVEX_Chain ( VexArch arch_host,
                             void*   place_to_chain,
                             void*   disp_cp_chain_me_EXPECTED,
                             void*   place_to_jump_to );

/* Undo an XDirect jump located at place_to_unchain, so it is
   converted back into a call to disp_cp_chain_me.  It is expected
   (and checked) that this site currently contains a jump directly to
   the address specified by place_to_jump_to_EXPECTED. */
extern
VexInvalRange LibVEX_UnChain ( VexArch arch_host,
                               void*   place_to_unchain,
                               void*   place_to_jump_to_EXPECTED,
                               void*   disp_cp_chain_me );

/* Returns a constant -- the size of the event check that is put at
   the start of every translation.  This makes it possible to
   calculate the fast entry point address if the slow entry point
   address is known (the usual case), or vice versa. */
extern
Int LibVEX_evCheckSzB ( VexArch arch_host );


/* Patch the counter location into an existing ProfInc point.  The
   specified point is checked to make sure it is plausible. */
extern
VexInvalRange LibVEX_PatchProfInc ( VexArch arch_host,
                                    void*   place_to_patch,
                                    ULong*  location_of_counter );


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
   The guest state must contain two pseudo-registers, guest_TISTART
   and guest_TILEN.  These are used to pass the address of areas of
   guest code, translations of which are to be invalidated, back to
   the despatcher.  Both pseudo-regs must have size equal to the guest
   word size.

   The architecture must a third pseudo-register, guest_NRADDR, also
   guest-word-sized.  This is used to record the unredirected guest
   address at the start of a translation whose start has been
   redirected.  By reading this pseudo-register shortly afterwards,
   the translation can find out what the corresponding no-redirection
   address was.  Note, this is only set for wrap-style redirects, not
   for replace-style ones.
*/
#endif /* ndef __LIBVEX_H */

/*---------------------------------------------------------------*/
/*---                                                libvex.h ---*/
/*---------------------------------------------------------------*/
