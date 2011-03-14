
/*---------------------------------------------------------------*/
/*--- begin                                  guest_x86_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
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

/* Only to be used within the guest-x86 directory. */

#ifndef __VEX_GUEST_X86_DEFS_H
#define __VEX_GUEST_X86_DEFS_H


/*---------------------------------------------------------*/
/*--- x86 to IR conversion                              ---*/
/*---------------------------------------------------------*/

/* Convert one x86 insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_X86 ( IRSB*        irbb,
                         Bool         put_IP,
                         Bool         (*resteerOkFn) ( void*, Addr64 ),
                         Bool         resteerCisOk,
                         void*        callback_opaque,
                         UChar*       guest_code,
                         Long         delta,
                         Addr64       guest_IP,
                         VexArch      guest_arch,
                         VexArchInfo* archinfo,
                         VexAbiInfo*  abiinfo,
                         Bool         host_bigendian );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* guest_x86_spechelper ( HChar*   function_name,
                               IRExpr** args,
                               IRStmt** precedingStmts,
                               Int      n_precedingStmts );

/* Describes to the optimiser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_x86_state_requires_precise_mem_exns ( Int, Int );

extern
VexGuestLayout x86guest_layout;


/*---------------------------------------------------------*/
/*--- x86 guest helpers                                 ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

extern UInt  x86g_calculate_eflags_all ( 
                UInt cc_op, UInt cc_dep1, UInt cc_dep2, UInt cc_ndep 
             );

VEX_REGPARM(3)
extern UInt  x86g_calculate_eflags_c ( 
                UInt cc_op, UInt cc_dep1, UInt cc_dep2, UInt cc_ndep 
             );

extern UInt  x86g_calculate_condition ( 
                UInt/*X86Condcode*/ cond, 
                UInt cc_op, 
                UInt cc_dep1, UInt cc_dep2, UInt cc_ndep 
             );

extern UInt  x86g_calculate_FXAM ( UInt tag, ULong dbl );

extern ULong x86g_calculate_RCR ( 
                UInt arg, UInt rot_amt, UInt eflags_in, UInt sz 
             );
extern ULong x86g_calculate_RCL ( 
                UInt arg, UInt rot_amt, UInt eflags_in, UInt sz 
             );

extern UInt x86g_calculate_daa_das_aaa_aas ( UInt AX_and_flags, UInt opcode );

extern UInt x86g_calculate_aad_aam ( UInt AX_and_flags, UInt opcode );

extern ULong x86g_check_fldcw ( UInt fpucw );

extern UInt  x86g_create_fpucw ( UInt fpround );

extern ULong x86g_check_ldmxcsr ( UInt mxcsr );

extern UInt  x86g_create_mxcsr ( UInt sseround );


/* Translate a guest virtual_addr into a guest linear address by
   consulting the supplied LDT/GDT structures.  Their representation
   must be as specified in pub/libvex_guest_x86.h.  To indicate a
   translation failure, 1<<32 is returned.  On success, the lower 32
   bits of the returned result indicate the linear address.  
*/
extern 
ULong x86g_use_seg_selector ( HWord ldt, HWord gdt, 
                              UInt seg_selector, UInt virtual_addr );

extern ULong x86g_calculate_mmx_pmaddwd  ( ULong, ULong );
extern ULong x86g_calculate_mmx_psadbw   ( ULong, ULong );
extern UInt  x86g_calculate_mmx_pmovmskb ( ULong );
extern UInt  x86g_calculate_sse_pmovmskb ( ULong w64hi, ULong w64lo );


/* --- DIRTY HELPERS --- */

extern ULong x86g_dirtyhelper_loadF80le  ( UInt );

extern void  x86g_dirtyhelper_storeF80le ( UInt, ULong );

extern void  x86g_dirtyhelper_CPUID_sse0 ( VexGuestX86State* );
extern void  x86g_dirtyhelper_CPUID_sse1 ( VexGuestX86State* );
extern void  x86g_dirtyhelper_CPUID_sse2 ( VexGuestX86State* );

extern void  x86g_dirtyhelper_FINIT ( VexGuestX86State* );

extern void  x86g_dirtyhelper_FXSAVE ( VexGuestX86State*, HWord );
extern void  x86g_dirtyhelper_FSAVE  ( VexGuestX86State*, HWord );
extern void  x86g_dirtyhelper_FSTENV ( VexGuestX86State*, HWord );

extern ULong x86g_dirtyhelper_RDTSC ( void );

extern UInt x86g_dirtyhelper_IN  ( UInt portno, UInt sz/*1,2 or 4*/ );
extern void x86g_dirtyhelper_OUT ( UInt portno, UInt data, 
                                   UInt sz/*1,2 or 4*/ );

extern void x86g_dirtyhelper_SxDT ( void* address,
                                    UInt op /* 0 or 1 */ );

extern VexEmWarn
            x86g_dirtyhelper_FXRSTOR ( VexGuestX86State*, HWord );

extern VexEmWarn
            x86g_dirtyhelper_FRSTOR ( VexGuestX86State*, HWord );

extern VexEmWarn 
            x86g_dirtyhelper_FLDENV ( VexGuestX86State*, HWord );


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* eflags masks */
#define X86G_CC_SHIFT_O   11
#define X86G_CC_SHIFT_S   7
#define X86G_CC_SHIFT_Z   6
#define X86G_CC_SHIFT_A   4
#define X86G_CC_SHIFT_C   0
#define X86G_CC_SHIFT_P   2

#define X86G_CC_MASK_O    (1 << X86G_CC_SHIFT_O)
#define X86G_CC_MASK_S    (1 << X86G_CC_SHIFT_S)
#define X86G_CC_MASK_Z    (1 << X86G_CC_SHIFT_Z)
#define X86G_CC_MASK_A    (1 << X86G_CC_SHIFT_A)
#define X86G_CC_MASK_C    (1 << X86G_CC_SHIFT_C)
#define X86G_CC_MASK_P    (1 << X86G_CC_SHIFT_P)

/* FPU flag masks */
#define X86G_FC_SHIFT_C3   14
#define X86G_FC_SHIFT_C2   10
#define X86G_FC_SHIFT_C1   9
#define X86G_FC_SHIFT_C0   8

#define X86G_FC_MASK_C3    (1 << X86G_FC_SHIFT_C3)
#define X86G_FC_MASK_C2    (1 << X86G_FC_SHIFT_C2)
#define X86G_FC_MASK_C1    (1 << X86G_FC_SHIFT_C1)
#define X86G_FC_MASK_C0    (1 << X86G_FC_SHIFT_C0)


/* %EFLAGS thunk descriptors.  A four-word thunk is used to record
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

   (1) In the guest state layout info (x86guest_layout), CC_OP and
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
    X86G_CC_OP_COPY=0,  /* DEP1 = current flags, DEP2 = 0, NDEP = unused */
                        /* just copy DEP1 to output */

    X86G_CC_OP_ADDB,    /* 1 */
    X86G_CC_OP_ADDW,    /* 2 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_ADDL,    /* 3 */

    X86G_CC_OP_SUBB,    /* 4 */
    X86G_CC_OP_SUBW,    /* 5 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_SUBL,    /* 6 */

    X86G_CC_OP_ADCB,    /* 7 */
    X86G_CC_OP_ADCW,    /* 8 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    X86G_CC_OP_ADCL,    /* 9 */

    X86G_CC_OP_SBBB,    /* 10 */
    X86G_CC_OP_SBBW,    /* 11 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    X86G_CC_OP_SBBL,    /* 12 */

    X86G_CC_OP_LOGICB,  /* 13 */
    X86G_CC_OP_LOGICW,  /* 14 DEP1 = result, DEP2 = 0, NDEP = unused */
    X86G_CC_OP_LOGICL,  /* 15 */

    X86G_CC_OP_INCB,    /* 16 */
    X86G_CC_OP_INCW,    /* 17 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    X86G_CC_OP_INCL,    /* 18 */

    X86G_CC_OP_DECB,    /* 19 */
    X86G_CC_OP_DECW,    /* 20 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    X86G_CC_OP_DECL,    /* 21 */

    X86G_CC_OP_SHLB,    /* 22 DEP1 = res, DEP2 = res', NDEP = unused */
    X86G_CC_OP_SHLW,    /* 23 where res' is like res but shifted one bit less */
    X86G_CC_OP_SHLL,    /* 24 */

    X86G_CC_OP_SHRB,    /* 25 DEP1 = res, DEP2 = res', NDEP = unused */
    X86G_CC_OP_SHRW,    /* 26 where res' is like res but shifted one bit less */
    X86G_CC_OP_SHRL,    /* 27 */

    X86G_CC_OP_ROLB,    /* 28 */
    X86G_CC_OP_ROLW,    /* 29 DEP1 = res, DEP2 = 0, NDEP = old flags */
    X86G_CC_OP_ROLL,    /* 30 */

    X86G_CC_OP_RORB,    /* 31 */
    X86G_CC_OP_RORW,    /* 32 DEP1 = res, DEP2 = 0, NDEP = old flags */
    X86G_CC_OP_RORL,    /* 33 */

    X86G_CC_OP_UMULB,   /* 34 */
    X86G_CC_OP_UMULW,   /* 35 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_UMULL,   /* 36 */

    X86G_CC_OP_SMULB,   /* 37 */
    X86G_CC_OP_SMULW,   /* 38 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_SMULL,   /* 39 */

    X86G_CC_OP_NUMBER
};

typedef
   enum {
      X86CondO      = 0,  /* overflow           */
      X86CondNO     = 1,  /* no overflow        */

      X86CondB      = 2,  /* below              */
      X86CondNB     = 3,  /* not below          */

      X86CondZ      = 4,  /* zero               */
      X86CondNZ     = 5,  /* not zero           */

      X86CondBE     = 6,  /* below or equal     */
      X86CondNBE    = 7,  /* not below or equal */

      X86CondS      = 8,  /* negative           */
      X86CondNS     = 9,  /* not negative       */

      X86CondP      = 10, /* parity even        */
      X86CondNP     = 11, /* not parity even    */

      X86CondL      = 12, /* jump less          */
      X86CondNL     = 13, /* not less           */

      X86CondLE     = 14, /* less or equal      */
      X86CondNLE    = 15, /* not less or equal  */

      X86CondAlways = 16  /* HACK */
   }
   X86Condcode;

#endif /* ndef __VEX_GUEST_X86_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                    guest_x86_defs.h ---*/
/*---------------------------------------------------------------*/
