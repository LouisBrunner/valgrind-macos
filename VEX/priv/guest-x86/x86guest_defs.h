
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

    CC_OP_MULB, /* modify all flags, C, O = (CC_SRC != 0) */
    CC_OP_MULW,
    CC_OP_MULL,

    CC_OP_ADDB, /* modify all flags, CC_DST = res, CC_SRC = src1 */
    CC_OP_ADDW,
    CC_OP_ADDL,

    CC_OP_ADCB, /* modify all flags, CC_DST = res, CC_SRC = src1 */
    CC_OP_ADCW,
    CC_OP_ADCL,

    CC_OP_SUBB, /* modify all flags, CC_DST = res, CC_SRC = src1 */
    CC_OP_SUBW,
    CC_OP_SUBL,

    CC_OP_SBBB, /* modify all flags, CC_DST = res, CC_SRC = src1 */
    CC_OP_SBBW,
    CC_OP_SBBL,

    CC_OP_LOGICB, /* modify all flags, CC_DST = res */
    CC_OP_LOGICW,
    CC_OP_LOGICL,

    CC_OP_INCB, /* modify all flags except, CC_DST = res, CC_SRC = C */
    CC_OP_INCW,
    CC_OP_INCL,

    CC_OP_DECB, /* modify all flags except, CC_DST = res, CC_SRC = C  */
    CC_OP_DECW,
    CC_OP_DECL,

    CC_OP_SHLB, /* modify all flags, CC_DST = res, CC_SRC = res' */
    CC_OP_SHLW, /* where res' is like res but shifted one bit less */
    CC_OP_SHLL,

    CC_OP_SARB, /* modify all flags, CC_DST = res, CC_SRC = res' */
    CC_OP_SARW, /* where res' is like res but shifted one bit less */
    CC_OP_SARL,

    CC_OP_NB,
};

/* called from generated code to evaluate the flags-thunk. */
extern UInt calculate_c   ( void );
extern UInt calculate_all ( void );


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
#define OFFB_EIF     (12*4)


#define SIZEOF_X86H_STATE OFFB_EIF



#endif /* ndef __LIBVEX_X86GUEST_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     x86guest_defs.h ---*/
/*---------------------------------------------------------------*/
