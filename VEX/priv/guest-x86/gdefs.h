
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (x86guest_defs.h) is                          ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/* Only to be used within the guest-x86 directory. */

/* Some of this stuff is taken from QEMU, which is Copyright (c) 2003
   Fabrice Bellard, and licensed under the LGPL. */

#ifndef __LIBVEX_X86GUEST_DEFS_H
#define __LIBVEX_X86GUEST_DEFS_H


/*---------------------------------------------------------*/
/*--- x86 to IR conversion                              ---*/
/*---------------------------------------------------------*/

extern
IRBB* bbToIR_X86Instr ( UChar* x86code, 
                        Addr64 eip, 
                        Int*   guest_bytes_read, 
                        Bool   (*byte_accessible)(Addr64),
                        Bool   host_bigendian );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* x86guest_spechelper ( Char* function_name,
                              IRExpr** args );

/* Describes to the optimser which part of the guest state require
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
extern UInt  calculate_eflags_all ( UInt cc_op, UInt cc_res, UInt cc_aux );
extern UInt  calculate_eflags_c   ( UInt cc_op, UInt cc_res, UInt cc_aux );
extern UInt  calculate_condition  ( UInt/*Condcode*/ cond, 
                                    UInt cc_op, UInt cc_res, UInt cc_aux );
extern UInt  calculate_FXAM ( UInt tag, ULong dbl );
extern ULong calculate_RCR  ( UInt arg, UInt rot_amt, UInt eflags_in, UInt sz );

/* --- DIRTY HELPERS --- */
extern ULong loadF80le  ( UInt );
extern void  storeF80le ( UInt, ULong );
extern void  dirtyhelper_CPUID ( VexGuestX86State* );


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* eflags masks */
#define CC_SHIFT_O   11
#define CC_SHIFT_S   7
#define CC_SHIFT_Z   6
#define CC_SHIFT_A   4
#define CC_SHIFT_C   0
#define CC_SHIFT_P   2

#define CC_MASK_O    (1 << CC_SHIFT_O)
#define CC_MASK_S    (1 << CC_SHIFT_S)
#define CC_MASK_Z    (1 << CC_SHIFT_Z)
#define CC_MASK_A    (1 << CC_SHIFT_A)
#define CC_MASK_C    (1 << CC_SHIFT_C)
#define CC_MASK_P    (1 << CC_SHIFT_P)

/* FPU flag masks */
#define FC_MASK_C3   (1 << 14)
#define FC_MASK_C2   (1 << 10)
#define FC_MASK_C1   (1 << 9)
#define FC_MASK_C0   (1 << 8)

/* %EFLAGS thunk descriptors.  This encoding is slightly non-obvious,
   for the benefit of Memcheck.  The intention is to make RES be the
   actual operation result -- or, in the case of multiply longs, at
   least have a complete data dependency on the real result.  That
   means that the AUX field needs to carry -- in the case of {S,U}MULL
   -- a value which allows the real result to be recovered from the
   RES field.

   Note, for cases in which some or all of the old flags are also an
   input (INC, DEC, ROL, ROR) we could have chosen to xor in the old
   flags into the RES field.  The effect would be that Memcheck then
   considers the result of the operation to also be dependent on the
   definedness of the old flags.  This seems a step too far -- if the
   compiler creates conditional jumps/moves partially dependent on
   flags it is unsure of the definedness of -- then it is generating
   buggy code.  So we don't do this.

   In short, we need to roll into the RES field all inputs to the
   computation that we want Memcheck to think have a bearing on the
   definedness of the result.  We then tell Memcheck that only the RES
   field needs to have its definedness tracked, and the AUX and OP
   fields can be ignored.  The upshot is that we need to put in the
   AUX field whatever info is needed to actually compute the flags
   given the potentially strange mixture of stuff in the RES field.  
*/
enum {
    CC_OP_COPY,    /* RES = current flags, AUX = 0, do nothing */

    CC_OP_ADDB,    /* RES = argL+argR, AUX = argR */
    CC_OP_ADDW,
    CC_OP_ADDL,    /* 3 */

    CC_OP_ADCB,    /* RES = argL+argR, AUX = argR */
    CC_OP_ADCW,
    CC_OP_ADCL,    /* 6 */

    CC_OP_SUBB,    /* RES = argL-argR, AUX = argR */
    CC_OP_SUBW,
    CC_OP_SUBL,    /* 9 */

    CC_OP_SBBB,    /* RES = argL-argR, AUX = argR */
    CC_OP_SBBW,
    CC_OP_SBBL,    /* 12 */

    CC_OP_LOGICB,  /* RES = and/or/xor(argL,argR), AUX = 0 */
    CC_OP_LOGICW,
    CC_OP_LOGICL,  /* 15 */

    CC_OP_INCB,    /* RES = arg+1, AUX = old C flag (0 or 1) */
    CC_OP_INCW,
    CC_OP_INCL,    /* 18 */

    CC_OP_DECB,    /* RES = arg-1, AUX = old C flag (0 or 1) */
    CC_OP_DECW,
    CC_OP_DECL,    /* 21 */

    CC_OP_SHLB,    /* RES = res, AUX = res' */
    CC_OP_SHLW,    /* where res' is like res but shifted one bit less */
    CC_OP_SHLL,    /* 24 */

    CC_OP_SARB,    /* RES = res, AUX = res' */
    CC_OP_SARW,    /* where res' is like res but shifted one bit less */
    CC_OP_SARL,    /* 27 */

    CC_OP_ROLB,    /* RES = res, AUX = old flags */
    CC_OP_ROLW,
    CC_OP_ROLL,    /* 30 */

    CC_OP_RORB,    /* RES = res, AUX = old flags */
    CC_OP_RORW,
    CC_OP_RORL,    /* 33 */

    CC_OP_UMULB,   /* RES = hiHalf(result) ^ loHalf(result) */
    CC_OP_UMULW,   /* AUX = loHalf(result) */
    CC_OP_UMULL,   /* 36 */

    CC_OP_SMULB,   /* RES = hiHalf(result) ^ loHalf(result) */
    CC_OP_SMULW,   /* AUX = loHalf(result) */
    CC_OP_SMULL,   /* 39 */

    CC_OP_NUMBER
};

typedef
   enum {
      CondO      = 0,  /* overflow           */
      CondNO     = 1,  /* no overflow        */

      CondB      = 2,  /* below              */
      CondNB     = 3,  /* not below          */

      CondZ      = 4,  /* zero               */
      CondNZ     = 5,  /* not zero           */

      CondBE     = 6,  /* below or equal     */
      CondNBE    = 7,  /* not below or equal */

      CondS      = 8,  /* negative           */
      CondNS     = 9,  /* not negative       */

      CondP      = 10, /* parity even        */
      CondNP     = 11, /* not parity even    */

      CondL      = 12, /* jump less          */
      CondNL     = 13, /* not less           */

      CondLE     = 14, /* less or equal      */
      CondNLE    = 15, /* not less or equal  */

      CondAlways = 16  /* HACK */
   }
   Condcode;

#endif /* ndef __LIBVEX_X86GUEST_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     x86guest_defs.h ---*/
/*---------------------------------------------------------------*/
