
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

/* Used by the back end to look up addresses of helper
   function calls inserted by bbToIR_X86Instr. */
extern
Addr64 x86guest_findhelper ( Char* function_name );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* x86guest_spechelper ( Char* function_name,
                              IRExpr** args );


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* eflags masks */
#define CC_MASK_C    0x0001
#define CC_MASK_P    0x0004
#define CC_MASK_A    0x0010
#define CC_MASK_Z    0x0040
#define CC_MASK_S    0x0080
#define CC_MASK_O    0x0800

/* eflags thunk descriptors. */
enum {
    CC_OP_COPY, /* nothing to do -- ccs are in CC_SRC and up to date */

    CC_OP_ADDB, /* modify all flags, CC_DST = src1, CC_SRC = src2 */
    CC_OP_ADDW,
    CC_OP_ADDL, /* 3 */

    CC_OP_ADCB, /* modify all flags, CC_DST = src1, CC_SRC = src2 */
    CC_OP_ADCW,
    CC_OP_ADCL, /* 6 */

    CC_OP_SUBB, /* modify all flags, CC_DST = src1, CC_SRC = src2 */
    CC_OP_SUBW,
    CC_OP_SUBL, /* 9 */

    CC_OP_SBBB, /* modify all flags, CC_DST = src1, CC_SRC = src2 */
    CC_OP_SBBW,
    CC_OP_SBBL, /* 12 */

    CC_OP_LOGICB, /* modify all flags, CC_DST = res, CC_SRC not used */
    CC_OP_LOGICW,
    CC_OP_LOGICL, /* 15 */

    CC_OP_INCB, /* modify all flags except C, CC_DST = res, CC_SRC = old C */
    CC_OP_INCW,
    CC_OP_INCL, /* 18 */

    CC_OP_DECB, /* modify all flags except C, CC_DST = res, CC_SRC = old C  */
    CC_OP_DECW,
    CC_OP_DECL, /* 21 */

    CC_OP_SHLB, /* modify all flags, CC_DST = res, CC_SRC = res' */
    CC_OP_SHLW, /* where res' is like res but shifted one bit less */
    CC_OP_SHLL, /* 24 */

    CC_OP_SARB, /* modify all flags, CC_DST = res, CC_SRC = res' */
    CC_OP_SARW, /* where res' is like res but shifted one bit less */
    CC_OP_SARL, /* 27 */

    CC_OP_ROLB, /* modify C and O only.  CC_DST = res, CC_SRC = old flags */
    CC_OP_ROLW,
    CC_OP_ROLL, /* 30 */

    CC_OP_RORB, /* modify C and O only.  CC_DST = res, CC_SRC = old flags */
    CC_OP_RORW,
    CC_OP_RORL, /* 33 */

    CC_OP_UMULB, /* modify all flags, CC_DST = one arg */
    CC_OP_UMULW, /* CC_SRC = the other arg */
    CC_OP_UMULL, /* 36 */

    CC_OP_SMULB, /* modify all flags, CC_DST = one arg */
    CC_OP_SMULW, /* CC_SRC = the other arg */
    CC_OP_SMULL, /* 39 */

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


/*---------------------------------------------------------*/
/*--- Simulated state offsets                           ---*/
/*---------------------------------------------------------*/

/* Hmm, subregisters.  The simulated state is stored in memory in the
   host's byte ordering, so we can't say here what the offsets of %ax,
   %al, %ah etc are since that depends on the host's byte ordering,
   which we don't know. */

#define OFFB_EAX     (0*4)
#define OFFB_ECX     (1*4)
#define OFFB_EDX     (2*4)
#define OFFB_EBX     (3*4)
#define OFFB_ESP     (4*4)
#define OFFB_EBP     (5*4)
#define OFFB_ESI     (6*4)
#define OFFB_EDI     (7*4)
/* 3-word thunk used to calculate O S Z A C P flags. */
#define OFFB_CC_OP   (8*4)
#define OFFB_CC_SRC  (9*4)
#define OFFB_CC_DST  (10*4)
/* The D flag is stored here, as either -1 or +1 */
#define OFFB_DFLAG   (11*4)
/* EIP */
#define OFFB_EIP     (12*4)

/* FPU.  For now, just simulate 8 64-bit registers, their tags, and
   the reg-stack top pointer, of which only the least significant
   three bits are relevant.

   The model is:
     F0 .. F7 are the 8 registers.  FTOP[2:0] contains the 
     index of the current 'stack top' -- pretty meaningless, but
     still.  FTOP is a 32-bit value.

     When a value is pushed onto the stack, ftop is first replaced by 
     (ftop-1) & 7, and then F[ftop] is assigned the value.

     When a value is popped off the stack, the value is read from
     F[ftop], and then ftop is replaced by (ftop+1) & 7.

     In general, a reference to a register ST(i) actually references
     F[ (ftop+i) & 7 ].

   FTAG0 .. FTAG0+7 are the tags.  Each is a byte, zero means empty,
   non-zero means non-empty.

   The general rule appears to be that a read or modify of a register
   gets a stack underflow fault if the register is empty.  A write of
   a register (only a write, not a modify) gets a stack overflow fault
   if the register is full.  Note that "over" vs "under" is pretty
   meaningless since the FP stack pointer can move around arbitrarily,
   so it's really just two different kinds of exceptions:
   register-empty and register full.

   Naturally Intel (in its infinite wisdom) has seen fit to throw in
   some ad-hoc inconsistencies to the fault-generation rules of the
   above para, just to complicate everything.  Known inconsistencies:

   * fxam can read a register in any state without taking an underflow
     fault.

   * fst from st(0) to st(i) does not take an overflow fault even if the
     destination is already full.

   FPUCW[15:0] is the FPU's control word.  FPUCW[31:16] is unused.

   FC320 contains the C3, C2 and C0 bits in the same place they are in
   the FPU's status word.  (bits 14, 10 and 8 respectively).  All other
   bits should be zero.  The relevant mask to select just those bits
   is 0x4500.
*/
#define OFFB_FTOP    (13*4)
#define OFFB_F0      (14*4)
#define OFFB_F1      (16*4)
#define OFFB_F2      (18*4)
#define OFFB_F3      (20*4)
#define OFFB_F4      (22*4)
#define OFFB_F5      (24*4)
#define OFFB_F6      (26*4)
#define OFFB_F7      (28*4)
#define OFFB_FTAG0   (30*4) // up to 30*4 + 7
#define OFFB_FPUCW   (32*4)
#define OFFB_FC320   (33*4)

/* Don't forget to keep this up to date. */
#define SIZEOF_X86H_STATE  (OFFB_FC320 + 4)



#endif /* ndef __LIBVEX_X86GUEST_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     x86guest_defs.h ---*/
/*---------------------------------------------------------------*/
