
/*---------------------------------------------------------------*/
/*--- begin                              host_nanomips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __VEX_HOST_NANOMIPS_DEFS_H
#define __VEX_HOST_NANOMIPS_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"             /* VexArch */
#include "host_generic_regs.h"  /* HReg */
#include "common_nanomips_defs.h"

/* --------- Registers. --------- */

#define ST_IN static inline

#define GPR(_enc, _ix) \
  mkHReg(False, HRcInt32, (_enc), (_ix))

ST_IN HReg hregNANOMIPS_GPR16(void) {
   return GPR(16,  0);
}
ST_IN HReg hregNANOMIPS_GPR17(void) {
   return GPR(17,  1);
}
ST_IN HReg hregNANOMIPS_GPR18(void) {
   return GPR(18,  2);
}
ST_IN HReg hregNANOMIPS_GPR19(void) {
   return GPR(19,  3);
}
ST_IN HReg hregNANOMIPS_GPR20(void) {
   return GPR(20,  4);
}
ST_IN HReg hregNANOMIPS_GPR21(void) {
   return GPR(21,  5);
}
ST_IN HReg hregNANOMIPS_GPR22(void) {
   return GPR(22,  6);
}

ST_IN HReg hregNANOMIPS_GPR12(void) {
   return GPR(12,  7);
}
ST_IN HReg hregNANOMIPS_GPR13(void) {
   return GPR(13,  8);
}
ST_IN HReg hregNANOMIPS_GPR14(void) {
   return GPR(14,  9);
}
ST_IN HReg hregNANOMIPS_GPR15(void) {
   return GPR(15, 10);
}
ST_IN HReg hregNANOMIPS_GPR24(void) {
   return GPR(24, 11);
}

ST_IN HReg hregNANOMIPS_GPR0(void) {
   return GPR( 0, 12);
}
ST_IN HReg hregNANOMIPS_GPR1(void) {
   return GPR( 1, 13);
}
ST_IN HReg hregNANOMIPS_GPR2(void) {
   return GPR( 2, 14);
}
ST_IN HReg hregNANOMIPS_GPR3(void) {
   return GPR( 3, 15);
}
ST_IN HReg hregNANOMIPS_GPR4(void) {
   return GPR( 4, 16);
}
ST_IN HReg hregNANOMIPS_GPR5(void) {
   return GPR( 5, 17);
}
ST_IN HReg hregNANOMIPS_GPR6(void) {
   return GPR( 6, 18);
}
ST_IN HReg hregNANOMIPS_GPR7(void) {
   return GPR( 7, 19);
}
ST_IN HReg hregNANOMIPS_GPR8(void) {
   return GPR( 8, 20);
}
ST_IN HReg hregNANOMIPS_GPR9(void) {
   return GPR( 9, 21);
}
ST_IN HReg hregNANOMIPS_GPR10(void) {
   return GPR(10, 22);
}
ST_IN HReg hregNANOMIPS_GPR11(void) {
   return GPR(11, 23);
}
ST_IN HReg hregNANOMIPS_GPR23(void) {
   return GPR(23, 24);
}
ST_IN HReg hregNANOMIPS_GPR25(void) {
   return GPR(25, 25);
}
ST_IN HReg hregNANOMIPS_GPR29(void) {
   return GPR(29, 26);
}
ST_IN HReg hregNANOMIPS_GPR31(void) {
   return GPR(31, 27);
}

#undef ST_IN
#undef GPR

#undef GuestStatePointer
#undef StackFramePointer
#undef StackPointer
#undef Zero

#define GuestStatePointer     hregNANOMIPS_GPR23()
#define StackFramePointer     hregNANOMIPS_GPR30()
#define StackPointer          hregNANOMIPS_GPR29()
#define Zero                  hregNANOMIPS_GPR0()

/* Num registers used for function calls */
/* a0, a1, a2, a3, a4, a5, a6, a7 */
# define NANOMIPS_N_REGPARMS 8

typedef enum {
   NMin_Imm,        /* Operation with word and imm (fake insn). */
   NMin_Unary,      /* Unary ops: clo, clz, neg and nop. */
   NMin_Alu,        /* Binary ops: add/sub/and/or/xor/nor/mul/div. */
   NMin_Cmp,        /* Word compare (fake insn). */
   NMin_Call,       /* Call to address in register. */

   /* The following 5 insns are mandated by translation chaining */
   NMin_XDirect,    /* Direct transfer to GA. */
   NMin_XIndir,     /* Indirect transfer to GA. */
   NMin_XAssisted,  /* Assisted transfer to GA. */
   NMin_EvCheck,    /* Event check. */
   NMin_ProfInc,    /* 64-bit profile counter increment. */

   NMin_Load,       /* Sign-extending load a 8|16|32 bit value from mem. */
   NMin_Store,      /* Store a 8|16|32 bit value to mem. */
   NMin_Cas,        /* Compare and swap. */
   NMin_LoadL,      /* Mips Load Linked Word - LL. */
   NMin_StoreC,     /* Mips Store Conditional Word - SC. */
   NMin_MoveCond,   /* Move Conditional. */
} NANOMIPSInstrTag;

typedef enum {
   NMimm_INVALID = -1,   /* Invalid / unknown op */
   NMimm_SLL     = 0x00, /* Shift left */
   NMimm_SRL     = 0x02, /* Logic shift right */
   NMimm_LI      = 0x03, /* Load immediate */
   NMimm_SRA     = 0x04, /* Arithetic shift right */
   NMimm_SGN     = 0x05, /* Sign extend from imm bits */
   NMimm_ORI     = 0x06, /* Logical or */
   NMimm_XORI    = 0x07, /* Logical xor */
   NMimm_ANDI    = 0x08, /* Logical and */
   NMimm_ROTX    = 0x09, /* Rotx */
} NANOMIPSImmOp;

typedef enum {
   NMun_CLO,
   NMun_CLZ,
   NMun_NOP,
} NANOMIPSUnaryOp;

typedef enum {
   NMalu_INVALID = -1,
   NMalu_SLL     = NMimm_SLL,
   NMalu_SRL     = NMimm_SRL,
   NMalu_SRA     = NMimm_SRA,
   NMalu_OR      = NMimm_ORI,
   NMalu_XOR     = NMimm_XORI,
   NMalu_AND     = NMimm_ANDI,
   NMalu_ADD,
   NMalu_SUB,
   NMalu_SLT,
   NMalu_NOR,
   NMalu_MUL,
   NMalu_MULU,
   NMalu_MUH,
   NMalu_MUHU,
   NMalu_DIV,
   NMalu_DIVU,
   NMalu_MOD,
   NMalu_MODU,
} NANOMIPSAluOp;

typedef enum {
   NMcc_INVALID, /* Invalid or unknown condition */
   NMcc_EQ,      /* equal */
   NMcc_NE,      /* not equal */

   NMcc_LTS,     /* signed less than */
   NMcc_LTU,     /* unsigned less than */

   NMcc_LES,     /* signed less than or equal */
   NMcc_LEU,     /* unsigned less than or equal */

   NMcc_AL,      /* always (unconditional) */
   NMcc_NV,      /* never (unconditional) */
} NANOMIPSCondCode;

typedef enum {
   NMMoveCond_movn      /* Move Conditional on Not Zero */
} NANOMIPSMoveCondOp;

typedef struct {
   NANOMIPSInstrTag tag;
   union {
      struct {
         NANOMIPSImmOp op;
         HReg src;
         HReg dst;
         UInt imm;
      } Imm;
      struct {
         NANOMIPSAluOp op;
         HReg dst;
         HReg srcL;
         HReg srcR;
      } Alu;
      /* Clz, Clo, not, nop */
      struct {
         NANOMIPSUnaryOp op;
         HReg dst;
         HReg src;
      } Unary;
      /* Word compare. Fake instruction, used for basic block ending. */
      struct {
         HReg dst;
         HReg srcL;
         HReg srcR;
         NANOMIPSCondCode cond;
      } Cmp;
      /* Pseudo-insn.  Call target (an absolute address), on given
         condition. */
      struct {
         Addr target;
         UInt argiregs;
         HReg guard;
         RetLoc rloc; /* Where the return value will be */
      } Call;
      /* Update the guest EIP value, then exit requesting to chain
         to it.  May be conditional. */
      struct {
         Addr dstGA;             /* next guest address */
         HReg addr;              /* Address register */
         Int addr_offset;        /* Offset */
         HReg cond;              /* Condition */
         Bool         toFastEP;  /* Chain to the slow or fast point? */
      } XDirect;
      /* Boring transfer to a guest address not known at JIT time.
         Not chainable.  May be conditional. */
      struct {
         HReg dstGA;
         HReg addr;              /* Address register */
         Int addr_offset;        /* Offset */
         HReg cond;              /* Condition */
      } XIndir;
      /* Assisted transfer to a guest address, most general case.
         Not chainable.  May be conditional. */
      struct {
         HReg dstGA;
         HReg addr;              /* Address register */
         Int addr_offset;        /* Offset */
         HReg cond;              /* Condition */
         IRJumpKind  jk;         /* Jump kind */
      } XAssisted;
      struct {
         HReg r_amCounter;
         Int offset_amCounter;
         HReg r_amFailAddr;
         Int offset_amFailAddr;
      } EvCheck;
      struct {
         /* No fields.  The address of the counter to inc is
            installed later, post-translation, by patching it in,
            as it is not known at translation time. */
      } ProfInc;
      /* Sign extending loads. Dst size is host word size */
      struct {
         UChar sz;         /* Must be 4 bytes for now. */
         HReg dst;         /* Destionation register */
         HReg addr;        /* Address register */
         Int addr_offset;  /* Offset */
      } Load;
      struct {
         UChar sz;         /* Must be 4 bytes for now. */
         HReg addr;        /* Address register */
         Int addr_offset;  /* Offset */
         HReg src;         /* Source register */
      } Store;
      struct {
         UChar sz;         /* Must be 4 bytes for now. */
         HReg  oldHi;
         HReg  oldLo;
         HReg  addr;
         HReg  expdHi;
         HReg  expdLo;
         HReg  dataHi;
         HReg  dataLo;
      } Cas;
      struct {
         UChar sz;         /* Must be 4 bytes for now. */
         HReg dst;         /* Destination register */
         HReg addr;        /* Address register */
         Int addr_offset;  /* Offset */
      } LoadL;
      struct {
         UChar sz;         /* Must be 4 bytes for now. */
         HReg addr;        /* Address register */
         Int addr_offset;  /* Offset */
         HReg src;         /* Sorce register */
      } StoreC;
      /* Conditional move. */
      struct {
         NANOMIPSMoveCondOp op;
         HReg dst;
         HReg src;
         HReg cond;
      } MoveCond;
   } NMin;
} NANOMIPSInstr;

extern NANOMIPSInstr *NANOMIPSInstr_Imm(NANOMIPSImmOp, HReg, HReg, UInt);
extern NANOMIPSInstr *NANOMIPSInstr_Unary(NANOMIPSUnaryOp op, HReg dst,
       HReg src);
extern NANOMIPSInstr *NANOMIPSInstr_Alu(NANOMIPSAluOp, HReg, HReg, HReg);
extern NANOMIPSInstr *NANOMIPSInstr_Cmp(NANOMIPSCondCode, HReg, HReg, HReg);
extern NANOMIPSInstr *NANOMIPSInstr_Call(Addr, UInt, HReg, RetLoc);
extern NANOMIPSInstr *NANOMIPSInstr_XDirect(Addr64 dstGA, HReg, Int,
                                            HReg cond, Bool toFastEP);
extern NANOMIPSInstr *NANOMIPSInstr_XIndir(HReg dstGA, HReg, Int,
                                           HReg cond);
extern NANOMIPSInstr *NANOMIPSInstr_XAssisted(HReg dstGA, HReg, Int,
                                              HReg cond, IRJumpKind jk);
extern NANOMIPSInstr *NANOMIPSInstr_EvCheck(HReg, Int, HReg, Int);
extern NANOMIPSInstr *NANOMIPSInstr_ProfInc(void);
extern NANOMIPSInstr *NANOMIPSInstr_Load(UChar sz, HReg dst, HReg src,
                                         Int addr_offset);
extern NANOMIPSInstr *NANOMIPSInstr_Store(UChar sz, HReg dst, Int addr_offset,
                                          HReg src);
extern NANOMIPSInstr *NANOMIPSInstr_Cas(UChar sz, HReg oldLo, HReg oldHi, HReg addr,
                                        HReg expdLo, HReg expdHi,
                                        HReg dataLo, HReg dataHi);
extern NANOMIPSInstr *NANOMIPSInstr_LoadL(UChar sz, HReg dst, HReg src,
                                          Int addr_offset);
extern NANOMIPSInstr *NANOMIPSInstr_StoreC(UChar sz, HReg dst, Int addr_offset,
                                           HReg src);
extern NANOMIPSInstr *NANOMIPSInstr_MoveCond(NANOMIPSMoveCondOp op, HReg dst,
                                             HReg src, HReg cond);
extern void ppNANOMIPSInstr(const NANOMIPSInstr *);
extern UInt ppHRegNANOMIPS(HReg);
extern void getRegUsage_NANOMIPSInstr (HRegUsage *, const NANOMIPSInstr *);
extern void mapRegs_NANOMIPSInstr     (HRegRemap *, NANOMIPSInstr *);
extern void genSpill_NANOMIPS ( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2,
                                        HReg rreg, Int offset, Bool mode64);
extern void genReload_NANOMIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2,
                                        HReg rreg, Int offset, Bool mode64);
extern NANOMIPSInstr* genMove_NANOMIPS(HReg from, HReg to);
extern HInstrArray *iselSB_NANOMIPS(const IRSB*,
                                    VexArch,
                                    const VexArchInfo*,
                                    const VexAbiInfo*,
                                    Int offs_Host_EvC_Counter,
                                    Int offs_Host_EvC_FailAddr,
                                    Bool chainingAllowed,
                                    Bool addProfInc,
                                    Addr max_ga);
extern Int emit_NANOMIPSInstr (/*MB_MOD*/Bool* is_profInc,
                               UChar* buf, Int nbuf,
                               const NANOMIPSInstr* i,
                               Bool mode64,
                               VexEndness endness_host,
                               const void* disp_cp_chain_me_to_slowEP,
                               const void* disp_cp_chain_me_to_fastEP,
                               const void* disp_cp_xindir,
                               const void* disp_cp_xassisted);
/* How big is an event check?  This is kind of a kludge because it
   depends on the offsets of host_EvC_FAILADDR and host_EvC_COUNTER,
   and so assumes that they are both <= 128, and so can use the short
   offset encoding.  This is all checked with assertions, so in the
   worst case we will merely assert at startup. */
extern Int evCheckSzB_NANOMIPS (void);
/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_NANOMIPS (VexEndness endness_host,
      void* place_to_chain,
      const void* disp_cp_chain_me_EXPECTED,
      const void* place_to_jump_to);
extern VexInvalRange unchainXDirect_NANOMIPS(VexEndness endness_host,
      void* place_to_unchain,
      const void* place_to_jump_to_EXPECTED,
      const void* disp_cp_chain_me);
/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_NANOMIPS (VexEndness endness_host,
      void*  place_to_patch,
      const ULong* location_of_counter);
extern const RRegUniverse* getRRegUniverse_NANOMIPS (Bool mode64);

#endif /* ndef __VEX_HOST_NANOMIPS_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                host-nanomips_defs.h ---*/
/*---------------------------------------------------------------*/
