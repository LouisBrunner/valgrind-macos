
/*---------------------------------------------------------------*/
/*--- begin                                guest_amd64_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2013 OpenWorks LLP
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

/* Only to be used within the guest-amd64 directory. */

#ifndef __VEX_GUEST_AMD64_DEFS_H
#define __VEX_GUEST_AMD64_DEFS_H

#include "libvex_basictypes.h"
#include "libvex_emnote.h"              // VexEmNote
#include "libvex_guest_amd64.h"         // VexGuestAMD64State
#include "guest_generic_bb_to_IR.h"     // DisResult

/*---------------------------------------------------------*/
/*--- amd64 to IR conversion                            ---*/
/*---------------------------------------------------------*/

/* Convert one amd64 insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_AMD64 ( IRSB*        irbb,
                           Bool         (*resteerOkFn) ( void*, Addr ),
                           Bool         resteerCisOk,
                           void*        callback_opaque,
                           const UChar* guest_code,
                           Long         delta,
                           Addr         guest_IP,
                           VexArch      guest_arch,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo*  abiinfo,
                           VexEndness   host_endness,
                           Bool         sigill_diag );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* guest_amd64_spechelper ( const HChar* function_name,
                                 IRExpr** args,
                                 IRStmt** precedingStmts,
                                 Int      n_precedingStmts );

/* Describes to the optimiser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_amd64_state_requires_precise_mem_exns ( Int, Int,
                                                   VexRegisterUpdates );

extern
VexGuestLayout amd64guest_layout;


/*---------------------------------------------------------*/
/*--- amd64 guest helpers                               ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

extern ULong amd64g_calculate_rflags_all ( 
                ULong cc_op, 
                ULong cc_dep1, ULong cc_dep2, ULong cc_ndep 
             );

extern ULong amd64g_calculate_rflags_c ( 
                ULong cc_op, 
                ULong cc_dep1, ULong cc_dep2, ULong cc_ndep 
             );

extern ULong amd64g_calculate_condition ( 
                ULong/*AMD64Condcode*/ cond, 
                ULong cc_op, 
                ULong cc_dep1, ULong cc_dep2, ULong cc_ndep 
             );

extern ULong amd64g_calculate_FXAM ( ULong tag, ULong dbl );

extern ULong amd64g_calculate_RCR  ( 
                ULong arg, ULong rot_amt, ULong rflags_in, Long sz 
             );

extern ULong amd64g_calculate_RCL  ( 
                ULong arg, ULong rot_amt, ULong rflags_in, Long sz 
             );

extern ULong amd64g_calculate_pclmul(ULong s1, ULong s2, ULong which);

extern ULong amd64g_check_fldcw ( ULong fpucw );

extern ULong amd64g_create_fpucw ( ULong fpround );

extern ULong amd64g_check_ldmxcsr ( ULong mxcsr );

extern ULong amd64g_create_mxcsr ( ULong sseround );

extern VexEmNote amd64g_dirtyhelper_FLDENV  ( VexGuestAMD64State*, HWord );
extern VexEmNote amd64g_dirtyhelper_FRSTOR  ( VexGuestAMD64State*, HWord );
extern VexEmNote amd64g_dirtyhelper_FRSTORS ( VexGuestAMD64State*, HWord );

extern void amd64g_dirtyhelper_FSTENV  ( VexGuestAMD64State*, HWord );
extern void amd64g_dirtyhelper_FNSAVE  ( VexGuestAMD64State*, HWord );
extern void amd64g_dirtyhelper_FNSAVES ( VexGuestAMD64State*, HWord );

/* Translate a guest virtual_addr into a guest linear address by
   consulting the supplied LDT/GDT structures.  Their representation
   must be as specified in pub/libvex_guest_amd64.h.  To indicate a
   translation failure, 1<<32 is returned.  On success, the lower 32
   bits of the returned result indicate the linear address.  
*/
//extern 
//ULong amd64g_use_seg_selector ( HWord ldt, HWord gdt, 
//                              UInt seg_selector, UInt virtual_addr );

extern ULong amd64g_calculate_mmx_pmaddwd  ( ULong, ULong );
extern ULong amd64g_calculate_mmx_psadbw   ( ULong, ULong );

extern ULong amd64g_calculate_sse_phminposuw ( ULong sLo, ULong sHi );

extern ULong amd64g_calc_crc32b ( ULong crcIn, ULong b );
extern ULong amd64g_calc_crc32w ( ULong crcIn, ULong w );
extern ULong amd64g_calc_crc32l ( ULong crcIn, ULong l );
extern ULong amd64g_calc_crc32q ( ULong crcIn, ULong q );

extern ULong amd64g_calc_mpsadbw ( ULong sHi, ULong sLo,
                                   ULong dHi, ULong dLo,
                                   ULong imm_and_return_control_bit );

extern ULong amd64g_calculate_pext  ( ULong, ULong );
extern ULong amd64g_calculate_pdep  ( ULong, ULong );

/* --- DIRTY HELPERS --- */

extern ULong amd64g_dirtyhelper_loadF80le  ( Addr/*addr*/ );

extern void  amd64g_dirtyhelper_storeF80le ( Addr/*addr*/, ULong/*data*/ );

extern void  amd64g_dirtyhelper_CPUID_baseline ( VexGuestAMD64State* st );
extern void  amd64g_dirtyhelper_CPUID_sse3_and_cx16 ( VexGuestAMD64State* st );
extern void  amd64g_dirtyhelper_CPUID_sse42_and_cx16 ( VexGuestAMD64State* st );
extern void  amd64g_dirtyhelper_CPUID_avx_and_cx16 ( VexGuestAMD64State* st );
extern void  amd64g_dirtyhelper_CPUID_avx2 ( VexGuestAMD64State* st );

extern void  amd64g_dirtyhelper_FINIT ( VexGuestAMD64State* );

extern void amd64g_dirtyhelper_XSAVE_COMPONENT_0
               ( VexGuestAMD64State* gst, HWord addr );
extern void amd64g_dirtyhelper_XSAVE_COMPONENT_1_EXCLUDING_XMMREGS 
               ( VexGuestAMD64State* gst, HWord addr );

extern VexEmNote amd64g_dirtyhelper_XRSTOR_COMPONENT_0
                    ( VexGuestAMD64State* gst, HWord addr );
extern VexEmNote amd64g_dirtyhelper_XRSTOR_COMPONENT_1_EXCLUDING_XMMREGS 
                    ( VexGuestAMD64State* gst, HWord addr );

extern ULong amd64g_dirtyhelper_RDTSC ( void );
extern void  amd64g_dirtyhelper_RDTSCP ( VexGuestAMD64State* st );

extern ULong amd64g_dirtyhelper_IN  ( ULong portno, ULong sz/*1,2 or 4*/ );
extern void  amd64g_dirtyhelper_OUT ( ULong portno, ULong data, 
                                      ULong sz/*1,2 or 4*/ );

extern void amd64g_dirtyhelper_SxDT ( void* address,
                                      ULong op /* 0 or 1 */ );

/* Helps with PCMP{I,E}STR{I,M}.

   CALLED FROM GENERATED CODE: DIRTY HELPER(s).  (But not really,
   actually it could be a clean helper, but for the fact that we can't
   pass by value 2 x V128 to a clean helper, nor have one returned.)
   Reads guest state, writes to guest state for the xSTRM cases, no
   accesses of memory, is a pure function.

   opc_and_imm contains (4th byte of opcode << 8) | the-imm8-byte so
   the callee knows which I/E and I/M variant it is dealing with and
   what the specific operation is.  4th byte of opcode is in the range
   0x60 to 0x63:
       istri  66 0F 3A 63
       istrm  66 0F 3A 62
       estri  66 0F 3A 61
       estrm  66 0F 3A 60

   gstOffL and gstOffR are the guest state offsets for the two XMM
   register inputs.  We never have to deal with the memory case since
   that is handled by pre-loading the relevant value into the fake
   XMM16 register.

   For ESTRx variants, edxIN and eaxIN hold the values of those two
   registers.

   In all cases, the bottom 16 bits of the result contain the new
   OSZACP %rflags values.  For xSTRI variants, bits[31:16] of the
   result hold the new %ecx value.  For xSTRM variants, the helper
   writes the result directly to the guest XMM0.

   Declarable side effects: in all cases, reads guest state at
   [gstOffL, +16) and [gstOffR, +16).  For xSTRM variants, also writes
   guest_XMM0.

   Is expected to be called with opc_and_imm combinations which have
   actually been validated, and will assert if otherwise.  The front
   end should ensure we're only called with verified values.
*/
extern ULong amd64g_dirtyhelper_PCMPxSTRx ( 
          VexGuestAMD64State*,
          HWord opc4_and_imm,
          HWord gstOffL, HWord gstOffR,
          HWord edxIN, HWord eaxIN
       );

/* Implementation of intel AES instructions as described in
   Intel  Advanced Vector Extensions
          Programming Reference
          MARCH 2008
          319433-002.

   CALLED FROM GENERATED CODE: DIRTY HELPER(s).  (But not really,
   actually it could be a clean helper, but for the fact that we can't
   pass by value 2 x V128 to a clean helper, nor have one returned.)
   Reads guest state, writes to guest state, no
   accesses of memory, is a pure function.

   opc4 contains the 4th byte of opcode. Front-end should only
   give opcode corresponding to AESENC/AESENCLAST/AESDEC/AESDECLAST/AESIMC.
   (will assert otherwise).

   gstOffL and gstOffR are the guest state offsets for the two XMM
   register inputs, gstOffD is the guest state offset for the XMM register
   output.  We never have to deal with the memory case since that is handled
   by pre-loading the relevant value into the fake XMM16 register.

*/
extern void amd64g_dirtyhelper_AES ( 
          VexGuestAMD64State* gst,
          HWord opc4, HWord gstOffD,
          HWord gstOffL, HWord gstOffR
       );

/* Implementation of AESKEYGENASSIST. 

   CALLED FROM GENERATED CODE: DIRTY HELPER(s).  (But not really,
   actually it could be a clean helper, but for the fact that we can't
   pass by value 1 x V128 to a clean helper, nor have one returned.)
   Reads guest state, writes to guest state, no
   accesses of memory, is a pure function.

   imm8 is the Round Key constant.

   gstOffL and gstOffR are the guest state offsets for the two XMM
   register input and output.  We never have to deal with the memory case since
   that is handled by pre-loading the relevant value into the fake
   XMM16 register.

*/
extern void amd64g_dirtyhelper_AESKEYGENASSIST ( 
          VexGuestAMD64State* gst,
          HWord imm8,
          HWord gstOffL, HWord gstOffR
       );

//extern void  amd64g_dirtyhelper_CPUID_sse0 ( VexGuestAMD64State* );
//extern void  amd64g_dirtyhelper_CPUID_sse1 ( VexGuestAMD64State* );
//extern void  amd64g_dirtyhelper_CPUID_sse2 ( VexGuestAMD64State* );

//extern void  amd64g_dirtyhelper_FSAVE ( VexGuestAMD64State*, HWord );

//extern VexEmNote
//            amd64g_dirtyhelper_FRSTOR ( VexGuestAMD64State*, HWord );

//extern void amd64g_dirtyhelper_FSTENV ( VexGuestAMD64State*, HWord );

//extern VexEmNote
//            amd64g_dirtyhelper_FLDENV ( VexGuestAMD64State*, HWord );



/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* rflags masks */
#define AMD64G_CC_SHIFT_O   11
#define AMD64G_CC_SHIFT_S   7
#define AMD64G_CC_SHIFT_Z   6
#define AMD64G_CC_SHIFT_A   4
#define AMD64G_CC_SHIFT_C   0
#define AMD64G_CC_SHIFT_P   2

#define AMD64G_CC_MASK_O    (1ULL << AMD64G_CC_SHIFT_O)
#define AMD64G_CC_MASK_S    (1ULL << AMD64G_CC_SHIFT_S)
#define AMD64G_CC_MASK_Z    (1ULL << AMD64G_CC_SHIFT_Z)
#define AMD64G_CC_MASK_A    (1ULL << AMD64G_CC_SHIFT_A)
#define AMD64G_CC_MASK_C    (1ULL << AMD64G_CC_SHIFT_C)
#define AMD64G_CC_MASK_P    (1ULL << AMD64G_CC_SHIFT_P)

/* additional rflags masks */
#define AMD64G_CC_SHIFT_ID  21
#define AMD64G_CC_SHIFT_AC  18
#define AMD64G_CC_SHIFT_D   10

#define AMD64G_CC_MASK_ID   (1ULL << AMD64G_CC_SHIFT_ID)
#define AMD64G_CC_MASK_AC   (1ULL << AMD64G_CC_SHIFT_AC)
#define AMD64G_CC_MASK_D    (1ULL << AMD64G_CC_SHIFT_D)

/* FPU flag masks */
#define AMD64G_FC_SHIFT_C3   14
#define AMD64G_FC_SHIFT_C2   10
#define AMD64G_FC_SHIFT_C1   9
#define AMD64G_FC_SHIFT_C0   8

#define AMD64G_FC_MASK_C3    (1ULL << AMD64G_FC_SHIFT_C3)
#define AMD64G_FC_MASK_C2    (1ULL << AMD64G_FC_SHIFT_C2)
#define AMD64G_FC_MASK_C1    (1ULL << AMD64G_FC_SHIFT_C1)
#define AMD64G_FC_MASK_C0    (1ULL << AMD64G_FC_SHIFT_C0)


/* %RFLAGS thunk descriptors.  A four-word thunk is used to record
   details of the most recent flag-setting operation, so the flags can
   be computed later if needed.  It is possible to do this a little
   more efficiently using a 3-word thunk, but that makes it impossible
   to describe the flag data dependencies sufficiently accurately for
   Memcheck.  Hence 4 words are used, with minimal loss of efficiency.

   The four words are:

      CC_OP, which describes the operation.

      CC_DEP1 and CC_DEP2.  These are arguments to the operation.
         We want Memcheck to believe that the resulting flags are
         data-dependent on both CC_DEP1 and CC_DEP2, hence the 
         name DEP.

      CC_NDEP.  This is a 3rd argument to the operation which is
         sometimes needed.  We arrange things so that Memcheck does
         not believe the resulting flags are data-dependent on CC_NDEP
         ("not dependent").

   To make Memcheck believe that (the definedness of) the encoded
   flags depends only on (the definedness of) CC_DEP1 and CC_DEP2
   requires two things:

   (1) In the guest state layout info (amd64guest_layout), CC_OP and
       CC_NDEP are marked as always defined.

   (2) When passing the thunk components to an evaluation function
       (calculate_condition, calculate_eflags, calculate_eflags_c) the
       IRCallee's mcx_mask must be set so as to exclude from
       consideration all passed args except CC_DEP1 and CC_DEP2.

   Strictly speaking only (2) is necessary for correctness.  However,
   (1) helps efficiency in that since (2) means we never ask about the
   definedness of CC_OP or CC_NDEP, we may as well not even bother to
   track their definedness.

   When building the thunk, it is always necessary to write words into
   CC_DEP1 and CC_DEP2, even if those args are not used given the
   CC_OP field (eg, CC_DEP2 is not used if CC_OP is CC_LOGIC1/2/4).
   This is important because otherwise Memcheck could give false
   positives as it does not understand the relationship between the
   CC_OP field and CC_DEP1 and CC_DEP2, and so believes that the 
   definedness of the stored flags always depends on both CC_DEP1 and
   CC_DEP2.

   However, it is only necessary to set CC_NDEP when the CC_OP value
   requires it, because Memcheck ignores CC_NDEP, and the evaluation
   functions do understand the CC_OP fields and will only examine
   CC_NDEP for suitable values of CC_OP.

   A summary of the field usages is:

   Operation          DEP1               DEP2               NDEP
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   add/sub/mul        first arg          second arg         unused

   adc/sbb            first arg          (second arg)
                                         XOR old_carry      old_carry

   and/or/xor         result             zero               unused

   inc/dec            result             zero               old_carry

   shl/shr/sar        result             subshifted-        unused
                                         result

   rol/ror            result             zero               old_flags

   copy               old_flags          zero               unused.


   Therefore Memcheck will believe the following:

   * add/sub/mul -- definedness of result flags depends on definedness
     of both args.

   * adc/sbb -- definedness of result flags depends on definedness of
     both args and definedness of the old C flag.  Because only two
     DEP fields are available, the old C flag is XOR'd into the second
     arg so that Memcheck sees the data dependency on it.  That means
     the NDEP field must contain a second copy of the old C flag
     so that the evaluation functions can correctly recover the second
     arg.

   * and/or/xor are straightforward -- definedness of result flags
     depends on definedness of result value.

   * inc/dec -- definedness of result flags depends only on
     definedness of result.  This isn't really true -- it also depends
     on the old C flag.  However, we don't want Memcheck to see that,
     and so the old C flag must be passed in NDEP and not in DEP2.
     It's inconceivable that a compiler would generate code that puts
     the C flag in an undefined state, then does an inc/dec, which
     leaves C unchanged, and then makes a conditional jump/move based
     on C.  So our fiction seems a good approximation.

   * shl/shr/sar -- straightforward, again, definedness of result
     flags depends on definedness of result value.  The subshifted
     value (value shifted one less) is also needed, but its
     definedness is the same as the definedness of the shifted value.

   * rol/ror -- these only set O and C, and leave A Z C P alone.
     However it seems prudent (as per inc/dec) to say the definedness
     of all resulting flags depends on the definedness of the result,
     hence the old flags must go in as NDEP and not DEP2.

   * rcl/rcr are too difficult to do in-line, and so are done by a
     helper function.  They are not part of this scheme.  The helper
     function takes the value to be rotated, the rotate amount and the
     old flags, and returns the new flags and the rotated value.
     Since the helper's mcx_mask does not have any set bits, Memcheck
     will lazily propagate undefinedness from any of the 3 args into 
     both results (flags and actual value).
*/
enum {
    AMD64G_CC_OP_COPY=0,  /* DEP1 = current flags, DEP2 = 0, NDEP = unused */
                          /* just copy DEP1 to output */

    AMD64G_CC_OP_ADDB,    /* 1 */
    AMD64G_CC_OP_ADDW,    /* 2 DEP1 = argL, DEP2 = argR, NDEP = unused */
    AMD64G_CC_OP_ADDL,    /* 3 */
    AMD64G_CC_OP_ADDQ,    /* 4 */

    AMD64G_CC_OP_SUBB,    /* 5 */
    AMD64G_CC_OP_SUBW,    /* 6 DEP1 = argL, DEP2 = argR, NDEP = unused */
    AMD64G_CC_OP_SUBL,    /* 7 */
    AMD64G_CC_OP_SUBQ,    /* 8 */

    AMD64G_CC_OP_ADCB,    /* 9 */
    AMD64G_CC_OP_ADCW,    /* 10 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    AMD64G_CC_OP_ADCL,    /* 11 */
    AMD64G_CC_OP_ADCQ,    /* 12 */

    AMD64G_CC_OP_SBBB,    /* 13 */
    AMD64G_CC_OP_SBBW,    /* 14 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    AMD64G_CC_OP_SBBL,    /* 15 */
    AMD64G_CC_OP_SBBQ,    /* 16 */

    AMD64G_CC_OP_LOGICB,  /* 17 */
    AMD64G_CC_OP_LOGICW,  /* 18 DEP1 = result, DEP2 = 0, NDEP = unused */
    AMD64G_CC_OP_LOGICL,  /* 19 */
    AMD64G_CC_OP_LOGICQ,  /* 20 */

    AMD64G_CC_OP_INCB,    /* 21 */
    AMD64G_CC_OP_INCW,    /* 22 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    AMD64G_CC_OP_INCL,    /* 23 */
    AMD64G_CC_OP_INCQ,    /* 24 */

    AMD64G_CC_OP_DECB,    /* 25 */
    AMD64G_CC_OP_DECW,    /* 26 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    AMD64G_CC_OP_DECL,    /* 27 */
    AMD64G_CC_OP_DECQ,    /* 28 */

    AMD64G_CC_OP_SHLB,    /* 29 DEP1 = res, DEP2 = res', NDEP = unused */
    AMD64G_CC_OP_SHLW,    /* 30 where res' is like res but shifted one bit less */
    AMD64G_CC_OP_SHLL,    /* 31 */
    AMD64G_CC_OP_SHLQ,    /* 32 */

    AMD64G_CC_OP_SHRB,    /* 33 DEP1 = res, DEP2 = res', NDEP = unused */
    AMD64G_CC_OP_SHRW,    /* 34 where res' is like res but shifted one bit less */
    AMD64G_CC_OP_SHRL,    /* 35 */
    AMD64G_CC_OP_SHRQ,    /* 36 */

    AMD64G_CC_OP_ROLB,    /* 37 */
    AMD64G_CC_OP_ROLW,    /* 38 DEP1 = res, DEP2 = 0, NDEP = old flags */
    AMD64G_CC_OP_ROLL,    /* 39 */
    AMD64G_CC_OP_ROLQ,    /* 40 */

    AMD64G_CC_OP_RORB,    /* 41 */
    AMD64G_CC_OP_RORW,    /* 42 DEP1 = res, DEP2 = 0, NDEP = old flags */
    AMD64G_CC_OP_RORL,    /* 43 */
    AMD64G_CC_OP_RORQ,    /* 44 */

    AMD64G_CC_OP_UMULB,   /* 45 */
    AMD64G_CC_OP_UMULW,   /* 46 DEP1 = argL, DEP2 = argR, NDEP = unused */
    AMD64G_CC_OP_UMULL,   /* 47 */
    AMD64G_CC_OP_UMULQ,   /* 48 */

    AMD64G_CC_OP_SMULB,   /* 49 */
    AMD64G_CC_OP_SMULW,   /* 50 DEP1 = argL, DEP2 = argR, NDEP = unused */
    AMD64G_CC_OP_SMULL,   /* 51 */
    AMD64G_CC_OP_SMULQ,   /* 52 */

    AMD64G_CC_OP_ANDN32,  /* 53 */
    AMD64G_CC_OP_ANDN64,  /* 54 DEP1 = res, DEP2 = 0, NDEP = unused */

    AMD64G_CC_OP_BLSI32,  /* 55 */
    AMD64G_CC_OP_BLSI64,  /* 56 DEP1 = res, DEP2 = arg, NDEP = unused */

    AMD64G_CC_OP_BLSMSK32,/* 57 */
    AMD64G_CC_OP_BLSMSK64,/* 58 DEP1 = res, DEP2 = arg, NDEP = unused */

    AMD64G_CC_OP_BLSR32,  /* 59 */
    AMD64G_CC_OP_BLSR64,  /* 60 DEP1 = res, DEP2 = arg, NDEP = unused */

    AMD64G_CC_OP_NUMBER
};

typedef
   enum {
      AMD64CondO      = 0,  /* overflow           */
      AMD64CondNO     = 1,  /* no overflow        */

      AMD64CondB      = 2,  /* below              */
      AMD64CondNB     = 3,  /* not below          */

      AMD64CondZ      = 4,  /* zero               */
      AMD64CondNZ     = 5,  /* not zero           */

      AMD64CondBE     = 6,  /* below or equal     */
      AMD64CondNBE    = 7,  /* not below or equal */

      AMD64CondS      = 8,  /* negative           */
      AMD64CondNS     = 9,  /* not negative       */

      AMD64CondP      = 10, /* parity even        */
      AMD64CondNP     = 11, /* not parity even    */

      AMD64CondL      = 12, /* less               */
      AMD64CondNL     = 13, /* not less           */

      AMD64CondLE     = 14, /* less or equal      */
      AMD64CondNLE    = 15, /* not less or equal  */

      AMD64CondAlways = 16  /* HACK */
   }
   AMD64Condcode;

#endif /* ndef __VEX_GUEST_AMD64_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                  guest_amd64_defs.h ---*/
/*---------------------------------------------------------------*/
