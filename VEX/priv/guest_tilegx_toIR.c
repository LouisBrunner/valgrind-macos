
/*--------------------------------------------------------------------*/
/*--- begin                                    guest_tilegx_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2013  Tilera Corp.

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

/* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

/* Translates TILEGX code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_tilegx.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_tilegx_defs.h"
#include "tilegx_disasm.h"

/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly.  CONST means does
   not change during translation of the instruction.
*/

/* CONST: is the host bigendian?  This has to do with float vs double
   register accesses on VFP, but it's complex and not properly thought
   out. */
static VexEndness host_endness;

/* Pointer to the guest code area. */
static UChar *guest_code;

/* The guest address corresponding to guest_code[0]. */
static Addr64 guest_PC_bbstart;

/* CONST: The guest address for the instruction currently being
   translated. */
static Addr64 guest_PC_curr_instr;

/* MOD: The IRSB* into which we're generating code. */
static IRSB *irsb;

/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)                    \
  if (vex_traceflags & VEX_TRACE_FE)            \
    vex_printf(format, ## args)

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- tilegx insn stream.                                  ---*/
/*------------------------------------------------------------*/

static Int integerGuestRegOffset ( UInt iregNo )
{
  return 8 * (iregNo);
}

/*------------------------------------------------------------*/
/*---                           Field helpers              ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static IRExpr *mkU8 ( UInt i )
{
  return IRExpr_Const(IRConst_U8((UChar) i));
}

/* Create an expression node for a 32-bit integer constant */
static IRExpr *mkU32 ( UInt i )
{
  return IRExpr_Const(IRConst_U32(i));
}

/* Create an expression node for a 64-bit integer constant */
static IRExpr *mkU64 ( ULong i )
{
  return IRExpr_Const(IRConst_U64(i));
}

static IRExpr *mkexpr ( IRTemp tmp )
{
  return IRExpr_RdTmp(tmp);
}

static IRExpr *unop ( IROp op, IRExpr * a )
{
  return IRExpr_Unop(op, a);
}

static IRExpr *binop ( IROp op, IRExpr * a1, IRExpr * a2 )
{
  return IRExpr_Binop(op, a1, a2);
}

static IRExpr *load ( IRType ty, IRExpr * addr )
{
  IRExpr *load1 = NULL;

  load1 = IRExpr_Load(Iend_LE, ty, addr);
  return load1;
}

/* Add a statement to the list held by "irsb". */
static void stmt ( IRStmt * st )
{
  addStmtToIRSB(irsb, st);
}

#define OFFB_PC     offsetof(VexGuestTILEGXState, guest_pc)

static void putPC ( IRExpr * e )
{
  stmt(IRStmt_Put(OFFB_PC, e));
}

static void assign ( IRTemp dst, IRExpr * e )
{
  stmt(IRStmt_WrTmp(dst, e));
}

static void store ( IRExpr * addr, IRExpr * data )
{
  stmt(IRStmt_Store(Iend_LE, addr, data));
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
  vassert(isPlausibleIRType(ty));
  return newIRTemp(irsb->tyenv, ty);
}

static ULong extend_s_16to64 ( UInt x )
{
  return (ULong) ((((Long) x) << 48) >> 48);
}

static ULong extend_s_8to64 ( UInt x )
{
  return (ULong) ((((Long) x) << 56) >> 56);
}

static IRExpr *getIReg ( UInt iregNo )
{
  IRType ty = Ity_I64;
  if(!(iregNo < 56 || iregNo == 63 ||
       (iregNo >= 70 && iregNo <= 73))) {
    vex_printf("iregNo=%u\n", iregNo);
    vassert(0);
  }
  return IRExpr_Get(integerGuestRegOffset(iregNo), ty);
}

static void putIReg ( UInt archreg, IRExpr * e )
{
  IRType ty = Ity_I64;
  if(!(archreg < 56 || archreg == 63 || archreg == 70 ||
       archreg == 72 || archreg == 73)) {
    vex_printf("archreg=%u\n", archreg);
    vassert(0);
  }
  vassert(typeOfIRExpr(irsb->tyenv, e) == ty);
  if (archreg != 63)
    stmt(IRStmt_Put(integerGuestRegOffset(archreg), e));
}

/* Narrow 8/16/32 bit int expr to 8/16/32.  Clearly only some
   of these combinations make sense. */
static IRExpr *narrowTo ( IRType dst_ty, IRExpr * e )
{
  IRType src_ty = typeOfIRExpr(irsb->tyenv, e);
  if (src_ty == dst_ty)
    return e;
  if (src_ty == Ity_I32 && dst_ty == Ity_I16)
    return unop(Iop_32to16, e);
  if (src_ty == Ity_I32 && dst_ty == Ity_I8)
    return unop(Iop_32to8, e);

  if (src_ty == Ity_I64 && dst_ty == Ity_I8) {
    return unop(Iop_64to8, e);
  }
  if (src_ty == Ity_I64 && dst_ty == Ity_I16) {
    return unop(Iop_64to16, e);
  }
  if (src_ty == Ity_I64 && dst_ty == Ity_I32) {
    return unop(Iop_64to32, e);
  }

  if (vex_traceflags & VEX_TRACE_FE) {
    vex_printf("\nsrc, dst tys are: ");
    ppIRType(src_ty);
    vex_printf(", ");
    ppIRType(dst_ty);
    vex_printf("\n");
  }
  vpanic("narrowTo(tilegx)");
  return e;
}

#define signExtend(_e, _n)                                              \
  ((_n == 32) ?                                                         \
   unop(Iop_32Sto64, _e) :                                              \
   ((_n == 16) ?                                                        \
    unop(Iop_16Sto64, _e) :						\
    (binop(Iop_Sar64, binop(Iop_Shl64, _e, mkU8(63 - (_n))), mkU8(63 - (_n))))))

static IRStmt* dis_branch ( IRExpr* guard, ULong imm )
{
  IRTemp t0;

  t0 = newTemp(Ity_I1);
  assign(t0, guard);
  return IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                     IRConst_U64(imm), OFFB_PC);
}

#define  MARK_REG_WB(_rd, _td)                  \
  do {                                          \
    vassert(rd_wb_index < 6);                   \
    rd_wb_temp[rd_wb_index] = _td;              \
    rd_wb_reg[rd_wb_index] = _rd;               \
    rd_wb_index++;                              \
  } while(0)


/* Expand/repeat byte _X 8 times to a 64-bit value */
#define  V1EXP(_X)                                     \
  ({                                                   \
    _X = ((((UChar)(_X)) << 8) | ((UChar)(_X)));       \
    _X = (((_X) << 16) | (_X));                        \
    (((_X) << 32) | (_X));                             \
  })

/* Expand/repeat byte _X 4 times to a 64-bit value */
#define  V2EXP(_X)                                 \
  ({                                               \
    _X = ((((UChar)(_X)) << 16) | ((UChar)(_X)));  \
    (((_X) << 32) | (_X));                         \
  })

/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction bundle into IR.  The bundle is
   located in host memory at guest_instr, and has guest IP of
   guest_PC_curr_instr, which will have been set before the call
   here. */
static DisResult disInstr_TILEGX_WRK ( Bool(*resteerOkFn) (void *, Addr),
                                       Bool resteerCisOk,
                                       void *callback_opaque,
                                       Long delta64,
                                       const VexArchInfo * archinfo,
                                       const VexAbiInfo * abiinfo,
                                       Bool sigill_diag )
{
  struct tilegx_decoded_instruction
    decoded[TILEGX_MAX_INSTRUCTIONS_PER_BUNDLE];
  ULong  cins, opcode = -1, rd, ra, rb, imm = 0;
  ULong  opd[4];
  ULong  opd_src_map, opd_dst_map, opd_imm_map;
  Int    use_dirty_helper;
  IRTemp t0, t1, t2, t3, t4;
  IRTemp tb[4];
  IRTemp rd_wb_temp[6];
  ULong  rd_wb_reg[6];
  /* Tilegx is a VLIW processor, we have to commit register write after read.*/
  Int    rd_wb_index;
  Int    n = 0, nr_insn;
  DisResult dres;

  /* The running delta */
  Long delta = delta64;

  /* Holds pc at the start of the insn, so that we can print
     consistent error messages for unimplemented insns. */
  //Long delta_start = delta;

  UChar *code = (UChar *) (guest_code + delta);

  IRStmt *bstmt = NULL;  /* Branch statement. */
  IRExpr *next = NULL; /* Next bundle expr. */
  ULong  jumpkind =  Ijk_Boring;
  ULong  steering_pc;

  /* Set result defaults. */
  dres.whatNext = Dis_Continue;
  dres.len = 0;
  dres.continueAt = 0;
  dres.jk_StopHere = Ijk_INVALID;

  /* Verify the code addr is 8-byte aligned. */
  vassert((((Addr)code) & 7) == 0);

  /* Get the instruction bundle. */
  cins = *((ULong *)(Addr) code);

  /* "Special" instructions. */
  /* Spot the 16-byte preamble:   ****tilegx****
     0:02b3c7ff91234fff { moveli zero, 4660 ; moveli zero, 22136 }
     8:0091a7ff95678fff { moveli zero, 22136 ; moveli zero, 4660 }
  */
#define CL_W0 0x02b3c7ff91234fffULL
#define CL_W1 0x0091a7ff95678fffULL

  if (*((ULong*)(Addr)(code)) == CL_W0 &&
      *((ULong*)(Addr)(code + 8)) == CL_W1) {
    /* Got a "Special" instruction preamble.  Which one is it? */
    if (*((ULong*)(Addr)(code + 16)) ==
        0x283a69a6d1483000ULL /* or r13, r13, r13 */ ) {
      /* r0 = client_request ( r12 ) */
      DIP("r0 = client_request ( r12 )\n");

      putPC(mkU64(guest_PC_curr_instr + 24));

      dres.jk_StopHere = Ijk_ClientReq;
      dres.whatNext = Dis_StopHere;
      dres.len = 24;
      goto decode_success;

    } else if (*((ULong*)(Addr)(code + 16)) ==
               0x283a71c751483000ULL /* or r14, r14, r14 */ ) {
      /* r11 = guest_NRADDR */
      DIP("r11 = guest_NRADDR\n");
      dres.len = 24;
      putIReg(11, IRExpr_Get(offsetof(VexGuestTILEGXState, guest_NRADDR),
                             Ity_I64));
      putPC(mkU64(guest_PC_curr_instr + 8));
      goto decode_success;

    } else if (*((ULong*)(Addr)(code + 16)) ==
               0x283a79e7d1483000ULL  /* or r15, r15, r15 */ ) {
      /*  branch-and-link-to-noredir r12 */
      DIP("branch-and-link-to-noredir r12\n");
      dres.len = 24;
      putIReg(55, mkU64(guest_PC_curr_instr + 24));

      putPC(getIReg(12));

      dres.jk_StopHere = Ijk_NoRedir;
      dres.whatNext = Dis_StopHere;
      goto decode_success;

    }  else if (*((ULong*)(Addr)(code + 16)) ==
                0x283a5965d1483000ULL  /* or r11, r11, r11 */ ) {
      /*  vex-inject-ir */
      DIP("vex-inject-ir\n");
      dres.len = 24;

      vex_inject_ir(irsb, Iend_LE);

      stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_CMSTART),
                      mkU64(guest_PC_curr_instr)));
      stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_CMLEN),
                      mkU64(24)));

      /* 2 + 1 = 3 bundles. 24 bytes. */
      putPC(mkU64(guest_PC_curr_instr + 24));

      dres.jk_StopHere = Ijk_InvalICache;
      dres.whatNext = Dis_StopHere;
      goto decode_success;
    }

    /* We don't expect this. */
    vex_printf("%s: unexpect special bundles at %lx\n",
               __func__, (Addr)guest_PC_curr_instr);
    delta += 16;
    goto decode_failure;
    /*NOTREACHED*/
  }

  /* To decode the given instruction bundle. */
  nr_insn = parse_insn_tilegx((tilegx_bundle_bits)cins,
                              (ULong)(Addr)code,
                              decoded);

  if (vex_traceflags & VEX_TRACE_FE)
    decode_and_display(&cins, 1, (ULong)(Addr)code);

  /* Init. rb_wb_index */
  rd_wb_index = 0;

  steering_pc = -1ULL;

  for (n = 0; n < nr_insn; n++) {
    opcode = decoded[n].opcode->mnemonic;
    Int opi;

    rd = ra = rb = -1;
    opd[0] = opd[1] = opd[2] = opd[3] = -1;
    opd_dst_map = 0;
    opd_src_map = 0;
    opd_imm_map = 0;

    for (opi = 0; opi < decoded[n].opcode->num_operands; opi++) {
      const struct tilegx_operand *op = decoded[n].operands[opi];
      opd[opi] = decoded[n].operand_values[opi];

      /* Set the operands. rd, ra, rb and imm. */
      if (opi < 3) {
        if (op->is_dest_reg) {
          if (rd == -1)
            rd =  decoded[n].operand_values[opi];
          else if (ra == -1)
            ra =  decoded[n].operand_values[opi];
        } else if (op->is_src_reg) {
          if (ra == -1) {
            ra = decoded[n].operand_values[opi];
          } else if(rb == -1) {
            rb = decoded[n].operand_values[opi];
          } else {
            vassert(0);
          }
        } else {
          imm = decoded[n].operand_values[opi];
        }
      }

      /* Build bit maps of used dest, source registers
         and immediate. */
      if (op->is_dest_reg) {
        opd_dst_map |= 1ULL << opi;
        if(op->is_src_reg)
          opd_src_map |= 1ULL << opi;
      } else if(op->is_src_reg) {
        opd_src_map |= 1ULL << opi;
      } else {
        opd_imm_map |= 1ULL << opi;
      }
    }

    use_dirty_helper = 0;

    switch (opcode) {
    case 0:  /* "bpt" */  /* "raise" */
      /* "bpt" pseudo instruction is an illegal instruction */
      opd_imm_map |= (1 << 0);
      opd[0] = cins;
      use_dirty_helper = 1;
      break;
    case 1:  /* "info" */   /* Ignore this instruction. */
      break;
    case 2:  /* "infol" */   /* Ignore this instruction. */
      break;
    case 3:  /* "ld4s_tls" */   /* Ignore this instruction. */
      break;
    case 4:  /* "ld_tls" */    /* Ignore this instruction. */
      break;
    case 5:  /* "move" */
      t2 = newTemp(Ity_I64);
      assign(t2, getIReg(ra));
      MARK_REG_WB(rd, t2);
      break;
    case 6:  /* "movei" */
      t2 = newTemp(Ity_I64);
      assign(t2, mkU64(extend_s_8to64(imm)));
      MARK_REG_WB(rd, t2);
      break;
    case 7:  /* "moveli" */
      t2 = newTemp(Ity_I64);
      assign(t2, mkU64(extend_s_16to64(imm)));
      MARK_REG_WB(rd, t2);
      break;
    case 8:  /* "prefetch" */   /* Ignore. */
      break;
    case 9:  /* "prefetch_add_l1" */   /* Ignore. */
      break;
    case 10: /* "prefetch_add_l1_fault" */   /* Ignore. */
      break;
    case 11: /* "prefetch_add_l2" */   /* Ignore. */
      break;
    case 12: /* "prefetch_add_l2_fault" */   /* Ignore. */
      break;
    case 13: /* "prefetch_add_l3" */   /* Ignore. */
      break;
    case 14: /* "prefetch_add_l3_fault" */   /* Ignore. */
      break;
    case 15: /* "prefetch_l1" */  /* Ignore. */
      break;
    case 16: /* "prefetch_l1_fault" */   /* Ignore. */
      break;
    case 17: /* "prefetch_l2" */   /* Ignore. */
      break;
    case 18: /* "prefetch_l2_fault" */   /* Ignore. */
      break;
    case 19: /* "prefetch_l3" */   /* Ignore. */
      break;
    case 20: /* "prefetch_l3_fault" */   /* Ignore. */
      break;
    case 21: /* "raise" */
      /* "raise" pseudo instruction is an illegal instruction plusing
         a "moveli zero, <sig>", so we need save whole bundle in the
         opd[0], which will be used in the dirty helper. */
      opd_imm_map |= (1 << 0);
      opd[0] = cins;
      use_dirty_helper = 1;
      break;
    case 22: /* "add" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64, getIReg(ra), getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 23: /* "addi" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64, getIReg(ra),
                       mkU64(extend_s_8to64(imm))));
      MARK_REG_WB(rd, t2);
      break;
    case 24: /* "addli" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64, getIReg(ra),
                       mkU64(extend_s_16to64(imm))));
      MARK_REG_WB(rd, t2);
      break;
    case 25: /* "addx" */
      t2 = newTemp(Ity_I64);
      assign(t2, signExtend(binop(Iop_Add32,
                                  narrowTo(Ity_I32, getIReg(ra)),
                                  narrowTo(Ity_I32, getIReg(rb))),
                            32));
      MARK_REG_WB(rd, t2);
      break;
    case 26: /* "addxi" */
      t2 = newTemp(Ity_I64);
      assign(t2, signExtend(binop(Iop_Add32,
                                  narrowTo(Ity_I32, getIReg(ra)),
                                  mkU32(imm)), 32));
      MARK_REG_WB(rd, t2);
      break;
    case 27: /* "addxli" */
      t2 = newTemp(Ity_I64);
      assign(t2, signExtend(binop(Iop_Add32,
                                  narrowTo(Ity_I32, getIReg(ra)),
                                  mkU32(imm)), 32));

      MARK_REG_WB(rd, t2);
      break;
    case 28: /* "addxsc" */
      use_dirty_helper = 1;
      break;
    case 29: /* "and" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_And64, getIReg(ra), getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 30: /* "andi" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_And64, getIReg(ra),
                       mkU64(extend_s_8to64(imm))));
      MARK_REG_WB(rd, t2);
      break;
    case 31: /* "beqz" */
      /* Fall-through */
    case 32:
      /* "beqzt" */
      bstmt = dis_branch(binop(Iop_CmpEQ64, getIReg(ra), mkU64(0)),
                         imm);
      break;
    case 33: /* "bfexts" */
      {
        ULong imm0 = decoded[n].operand_values[3];
        ULong mask = ((-1ULL) ^ ((-1ULL << ((imm0 - imm) & 63)) << 1));
        t0 = newTemp(Ity_I64);
        t2 = newTemp(Ity_I64);
        assign(t0, binop(Iop_Xor64,
                         binop(Iop_Sub64,
                               binop(Iop_And64,
                                     binop(Iop_Shr64,
                                           getIReg(ra),
                                           mkU8(imm0)),
                                     mkU64(1)),
                               mkU64(1)),
                         mkU64(-1ULL)));
        assign(t2,
               binop(Iop_Or64,
                     binop(Iop_And64,
                           binop(Iop_Or64,
                                 binop(Iop_Shr64,
                                       getIReg(ra),
                                       mkU8(imm)),
                                 binop(Iop_Shl64,
                                       getIReg(ra),
                                       mkU8(64 - imm))),
                           mkU64(mask)),
                     binop(Iop_And64,
                           mkexpr(t0),
                           mkU64(~mask))));

        MARK_REG_WB(rd, t2);
      }
      break;
    case 34:  /* "bfextu" */
      {
        ULong imm0 = decoded[n].operand_values[3];
        ULong mask = 0;
        t2 = newTemp(Ity_I64);
        mask = ((-1ULL) ^ ((-1ULL << ((imm0 - imm) & 63)) << 1));

        assign(t2,
               binop(Iop_And64,
                     binop(Iop_Or64,
                           binop(Iop_Shr64,
                                 getIReg(ra),
                                 mkU8(imm)),
                           binop(Iop_Shl64,
                                 getIReg(ra),
                                 mkU8(64 - imm))),
                     mkU64(mask)));
        MARK_REG_WB(rd, t2);
      }
      break;
    case 35:  /* "bfins" */
      {
        ULong mask;
        ULong imm0 = decoded[n].operand_values[3];
        t0 = newTemp(Ity_I64);
        t2 = newTemp(Ity_I64);
        if (imm <= imm0)
        {
          mask = ((-1ULL << imm) ^ ((-1ULL << imm0) << 1));
        }
        else
        {
          mask = ((-1ULL << imm) | (-1ULL >> (63 - imm0)));
        }

        assign(t0, binop(Iop_Or64,
                         binop(Iop_Shl64,
                               getIReg(ra),
                               mkU8(imm)),
                         binop(Iop_Shr64,
                               getIReg(ra),
                               mkU8(64 - imm))));

        assign(t2, binop(Iop_Or64,
                         binop(Iop_And64,
                               mkexpr(t0),
                               mkU64(mask)),
                         binop(Iop_And64,
                               getIReg(rd),
                               mkU64(~mask))));

        MARK_REG_WB(rd, t2);
      }
      break;
    case 36:  /* "bgez" */
      /* Fall-through */
    case 37:  /* "bgezt" */
      bstmt = dis_branch(binop(Iop_CmpEQ64,
                               binop(Iop_And64,
                                     getIReg(ra),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x0)),
                         imm);
      break;
    case 38:  /* "bgtz" */
      /* Fall-through */
    case 39:
      /* "bgtzt" */
      bstmt = dis_branch(unop(Iop_Not1,
                              binop(Iop_CmpLE64S,
                                    getIReg(ra),
                                    mkU64(0))),
                         imm);
      break;
    case 40:  /* "blbc" */
      /* Fall-through */
    case 41:  /* "blbct" */
      bstmt = dis_branch(unop(Iop_64to1,
                              unop(Iop_Not64, getIReg(ra))),
                         imm);

      break;
    case 42:  /* "blbs" */
      /* Fall-through */
    case 43:
      /* "blbst" */
      bstmt = dis_branch(unop(Iop_64to1,
                              getIReg(ra)),
                         imm);
      break;
    case 44:  /* "blez" */
      bstmt = dis_branch(binop(Iop_CmpLE64S, getIReg(ra),
                               mkU64(0)),
                         imm);
      break;
    case 45:  /* "blezt" */
      bstmt = dis_branch(binop(Iop_CmpLE64S, getIReg(ra),
                               mkU64(0)),
                         imm);
      break;
    case 46:  /* "bltz" */
      bstmt = dis_branch(binop(Iop_CmpLT64S, getIReg(ra),
                               mkU64(0)),
                         imm);
      break;
    case 47:  /* "bltzt" */
      bstmt = dis_branch(binop(Iop_CmpLT64S, getIReg(ra),
                               mkU64(0)),
                         imm);
      break;
    case 48:  /* "bnez" */
      /* Fall-through */
    case 49:
      /* "bnezt" */
      bstmt = dis_branch(binop(Iop_CmpNE64, getIReg(ra),
                               mkU64(0)),
                         imm);
      break;
    case 50:  /* "clz" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_Clz64, getIReg(ra)));

      MARK_REG_WB(rd, t2);
      break;
    case 51:  /* "cmoveqz rd, ra, rb" */
      t2 = newTemp(Ity_I64);
      assign(t2, IRExpr_ITE(binop(Iop_CmpEQ64, getIReg(ra), mkU64(0)),
                            getIReg(rb), getIReg(rd)));
      MARK_REG_WB(rd, t2);
      break;
    case 52:  /* "cmovnez" */
      t2 = newTemp(Ity_I64);
      assign(t2, IRExpr_ITE(binop(Iop_CmpEQ64, getIReg(ra), mkU64(0)),
                            getIReg(rd), getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 53:  /* "cmpeq" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_1Uto64, binop(Iop_CmpEQ64,
                                         getIReg(ra), getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;

    case 54:  /* "cmpeqi" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_1Uto64, binop(Iop_CmpEQ64,
                                        getIReg(ra),
                                        mkU64(extend_s_8to64(imm)))));
      MARK_REG_WB(rd, t2);
      break;
    case 55:  /* "cmpexch" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);

      assign(t1, getIReg(rb));
      stmt( IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t2, Iend_LE,
                               getIReg(ra),
                               NULL, binop(Iop_Add64,
                                           getIReg(70),
                                           getIReg(71)),
                               NULL, mkexpr(t1))));
      MARK_REG_WB(rd, t2);
      break;
    case 56:  /* "cmpexch4" */
      t1 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I32);

      assign(t1, narrowTo(Ity_I32, getIReg(rb)));
      stmt( IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t3, Iend_LE,
                               getIReg(ra),
                               NULL,
                               narrowTo(Ity_I32, binop(Iop_Add64,
                                                       getIReg(70),
                                                       getIReg(71))),
                               NULL,
                               mkexpr(t1))));
      assign(t2, unop(Iop_32Uto64, mkexpr(t3)));
      MARK_REG_WB(rd, t2);
      break;
    case 57:  /* "cmples" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_1Uto64,
                      binop(Iop_CmpLE64S, getIReg(ra), getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 58:  /* "cmpleu" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_1Uto64,
                       binop(Iop_CmpLE64U, getIReg(ra), getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 59:  /* "cmplts" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_1Uto64,
                      binop(Iop_CmpLT64S, getIReg(ra), getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 60:  /* "cmpltsi" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_1Uto64,
                      binop(Iop_CmpLT64S,
                            getIReg(ra),
                            mkU64(extend_s_8to64(imm)))));
      MARK_REG_WB(rd, t2);
      break;
    case 61:

      /* "cmpltu" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_1Uto64,
                      binop(Iop_CmpLT64U, getIReg(ra), getIReg(rb))));
      MARK_REG_WB(rd, t2);


      break;
    case 62:  /* "cmpltui" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_1Uto64,
                       binop(Iop_CmpLT64U,
                             getIReg(ra),
                             mkU64(imm))));
      MARK_REG_WB(rd, t2);


      break;
    case 63:  /* "cmpne" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_1Uto64,
                      binop(Iop_CmpNE64, getIReg(ra), getIReg(rb))));
      MARK_REG_WB(rd, t2);


      break;
    case 64:
      /* Fall-through */
    case 65:
      /* Fall-through */
    case 66:
      /* Fall-through */
    case 67:
      /* Fall-through */
    case 68:
      /* Fall-through */
    case 69:
      /* Fall-through */
    case 70:
      /* Fall-through */
    case 71:
      /* Fall-through */
    case 72:
      use_dirty_helper = 1;
      break;
    case 73:  /* "ctz" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_Ctz64, getIReg(ra)));

      MARK_REG_WB(rd, t2);


      break;
    case 74:  /* "dblalign" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);

      /* t0 is the bit shift amount */
      assign(t0, binop(Iop_Shl64,
                       binop(Iop_And64,
                             getIReg(rb),
                             mkU64(7)),
                       mkU8(3)));
      assign(t1, binop(Iop_Sub64,
                       mkU64(64),
                       mkexpr(t0)));

      assign(t2, binop(Iop_Or64,
                       binop(Iop_Shl64,
                             getIReg(ra),
                             unop(Iop_64to8, mkexpr(t1))),
                       binop(Iop_Shr64,
                             getIReg(rd),
                             unop(Iop_64to8, mkexpr(t0)))));

      MARK_REG_WB(rd, t2);
      break;
    case 75:
      /* Fall-through */
    case 76:
      /* Fall-through */
    case 77:
      /* Fall-through */
    case 78:
      /* Fall-through */
    case 79:
      use_dirty_helper = 1;
      break;
    case 80:  /* "exch" */
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t2,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      mkU64(0x0),
                      NULL,
                      getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 81:  /* "exch4 rd, ra, rb" */
      t0 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t0,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      mkU32(0x0),
                      NULL,
                      narrowTo(Ity_I32,
                               getIReg(rb)))));
      assign(t2, unop(Iop_32Sto64, mkexpr(t0)));
      MARK_REG_WB(rd, t2);
      break;
    case 82:
      /* Fall-through */
    case 83:
      /* Fall-through */
    case 84:
      /* Fall-through */
    case 85:
      /* Fall-through */
    case 86:
      /* Fall-through */
    case 87:
      /* Fall-through */
    case 88:
      /* Fall-through */
    case 89:
      use_dirty_helper = 1;
      break;
    case 90:  /* "fetchadd" */
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t2,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      // fetchadd=3
                      mkU64(0x3),
                      NULL,
                      getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 91:  /* "fetchadd4" */
      t0 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t0,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      // fetchadd=3
                      mkU32(0x3),
                      NULL,
                      narrowTo(Ity_I32,
                               getIReg(rb)))));
      assign(t2, unop(Iop_32Sto64, mkexpr(t0)));
      MARK_REG_WB(rd, t2);

      break;
    case 92:  /* "fetchaddgez" */
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t2,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      // fetchaddgez=5
                      mkU64(0x5),
                      NULL,
                      getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 93:  /* "fetchaddgez4" */
      t0 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t0,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      // fetchaddgez=5
                      mkU32(0x5),
                      NULL,
                      narrowTo(Ity_I32,
                               getIReg(rb)))));
      assign(t2, unop(Iop_32Sto64, mkexpr(t0)));
      MARK_REG_WB(rd, t2);
      break;
    case 94:  /* "fetchand\n") */
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t2,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      mkU64(0x2),
                      NULL,
                      getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 95:
      /* mkIRCAS.
         0: xch###      1: cmpexch###,
         2: fetchand##  3: fetchadd##
         4: fetchor##   5: fetchaddgez
      */
      /* "fetchand4" */
      t0 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t0,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      mkU32(0x2),
                      NULL,
                      narrowTo(Ity_I32,
                               getIReg(rb)))));
      assign(t2, unop(Iop_32Sto64, mkexpr(t0)));
      MARK_REG_WB(rd, t2);
      break;
    case 96:  /* "fetchor" */
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t2,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      mkU64(0x4),
                      NULL,
                      getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 97:  /* "fetchor4" */
      t0 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I64);
      stmt( IRStmt_CAS(
              mkIRCAS(IRTemp_INVALID,
                      t0,
                      Iend_LE,
                      getIReg(ra),
                      NULL,
                      mkU32(0x4),
                      NULL,
                      narrowTo(Ity_I32,
                               getIReg(rb)))));
      assign(t2, unop(Iop_32Sto64, mkexpr(t0)));
      MARK_REG_WB(rd, t2);
      break;
    case 98:
      /* Fall-through */
    case 99:
      /* Fall-through */
    case 100:
      use_dirty_helper = 1;
      break;
    case 101: /* "fnop"  Ignore */
      break;
    case 102:
      /* Fall-through */
    case 103:
      /* Fall-through */
    case 104:
      /* Fall-through */
    case 105:
      /* Fall-through */
    case 106:
      /* Fall-through */
    case 107:
      /* Fall-through */
    case 108:
      use_dirty_helper = 1;
      break;
    case 109:
      /* Fall-through */
    case 110:
      /* Fall-through */
    case 111:
      use_dirty_helper = 1;
      break;
    case 112:  /* "iret" */
      next = mkU64(guest_PC_curr_instr + 8);
      jumpkind = Ijk_Ret;
      break;
    case 113:  /* "j" */
      next = mkU64(imm);
      /* set steering address. */
      steering_pc = imm;
      jumpkind = Ijk_Boring;
      break;
    case 114:
      t2 = newTemp(Ity_I64);
      assign(t2, mkU64(guest_PC_curr_instr + 8));
      /* set steering address. */
      steering_pc = imm;
      next = mkU64(imm);
      jumpkind = Ijk_Call;
      MARK_REG_WB(55, t2);
      break;
    case 115:  /* "jalr" */
      /* Fall-through */
    case 116:  /* "jalrp" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1, getIReg(ra));
      assign(t2, mkU64(guest_PC_curr_instr + 8));
      next = mkexpr(t1);
      jumpkind = Ijk_Call;
      MARK_REG_WB(55, t2);
      break;
    case 117:  /* "jr" */
      /* Fall-through */
    case 118:  /* "jrp" */
      next = getIReg(ra);
      jumpkind = Ijk_Boring;
      break;
    case 119:  /* "ld" */
      t2 = newTemp(Ity_I64);
      assign(t2, load(Ity_I64, (getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 120:  /* "ld1s" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_8Sto64,
                       load(Ity_I8, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);
      break;
    case 121:  /* "ld1s_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_8Sto64,
                       load(Ity_I8, (getIReg(ra)))));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 122:  /* "ld1u" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_8Uto64,
                       load(Ity_I8, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);

      break;
    case 123:  /* "ld1u_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1,  binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_8Uto64,
                       load(Ity_I8, (getIReg(ra)))));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 124:  /* "ld2s" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_16Sto64,
                       load(Ity_I16, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 125:  /* "ld2s_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1,  binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_16Sto64,
                       load(Ity_I16, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      MARK_REG_WB(ra, t1);
      break;
    case 126: /* "ld2u" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_16Uto64,
                       load(Ity_I16, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 127: /* "ld2u_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1,  binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_16Uto64,
                       load(Ity_I16, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      MARK_REG_WB(ra, t1);
      break;
    case 128: /* "ld4s" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Sto64,
                       load(Ity_I32, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);
      break;
    case 129: /* "ld4s_add" */
      t2 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      assign(t1,  binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_32Sto64,
                       load(Ity_I32, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);
      MARK_REG_WB(ra, t1);
      break;
    case 130:  /* "ld4u" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Uto64,
                       load(Ity_I32, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 131:  /* "ld4u_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_32Uto64,
                       load(Ity_I32, getIReg(ra))));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 132:  /* "ld_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1, load(Ity_I64, getIReg(ra)));
      assign(t2, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(ra, t2);
      MARK_REG_WB(rd, t1);
      break;
    case 133:  /* "ldna" */
      t2 = newTemp(Ity_I64);
      assign(t2, load(Ity_I64,
                      binop(Iop_And64,
                            getIReg(ra),
                            unop(Iop_Not64,
                                 mkU64(7)))));
      MARK_REG_WB(rd, t2);
      break;
    case 134:  /* "ldna_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);

      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2, load(Ity_I64,
                      binop(Iop_And64,
                            getIReg(ra),
                            unop(Iop_Not64,
                                 mkU64(7)))));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 135:  /* "ldnt" */
      /* Valgrind IR has no Non-Temp load. Use normal load. */
      t2 = newTemp(Ity_I64);
      assign(t2, load(Ity_I64, (getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 136:  /* "ldnt1s" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_8Sto64,
                       load(Ity_I8, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);
      break;
    case 137:  /* "ldnt1s_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_8Sto64,
                       load(Ity_I8, (getIReg(ra)))));
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 138:  /* "ldnt1u" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_8Uto64,
                       load(Ity_I8, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);
      break;
    case 139:  /* "ldnt1u_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);

      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      assign(t2,  unop(Iop_8Uto64,
                       load(Ity_I8, (getIReg(ra)))));

      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 140:  /* "ldnt2s" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_16Sto64,
                       load(Ity_I16, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 141:  /* "ldnt2s_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_16Sto64,
                       load(Ity_I16, getIReg(ra))));
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 142:  /* "ldnt2u" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_16Uto64,
                       load(Ity_I16, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 143:  /* "ldnt2u_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_16Uto64,
                       load(Ity_I16, getIReg(ra))));
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(ra, t1);
      MARK_REG_WB(rd, t2);
      break;
    case 144:  /* "ldnt4s" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Sto64,
                       load(Ity_I32, (getIReg(ra)))));
      MARK_REG_WB(rd, t2);
      break;
    case 145:  /* "ldnt4s_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Sto64,
                       load(Ity_I32, (getIReg(ra)))));
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(rd, t2);
      MARK_REG_WB(ra, t1);
      break;
    case 146:  /* "ldnt4u" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Uto64,
                       load(Ity_I32, getIReg(ra))));
      MARK_REG_WB(rd, t2);
      break;
    case 147:  /* "ldnt4u_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Uto64,
                       load(Ity_I32, getIReg(ra))));
      assign(t1, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(rd, t2);
      MARK_REG_WB(ra, t1);
      break;
    case 148:  /* "ldnt_add" */
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t1, load(Ity_I64, getIReg(ra)));
      assign(t2, binop(Iop_Add64, getIReg(ra), mkU64(imm)));
      MARK_REG_WB(rd, t1);
      MARK_REG_WB(ra, t2);
      break;
    case 149:  /* "lnk" */
      t2 = newTemp(Ity_I64);
      assign(t2,  mkU64(guest_PC_curr_instr + 8));
      MARK_REG_WB(rd, t2);
      break;
    case 150:  /* "mf" */
      use_dirty_helper = 1;
      break;
    case 151:  /* "mfspr" */
      t2 = newTemp(Ity_I64);
      if (imm == 0x2780) { // Get Cmpexch value
	 assign(t2, getIReg(70));
	 MARK_REG_WB(rd, t2);
      } else if (imm == 0x2580) { // Get EX_CONTEXT_0_0
         assign(t2, getIReg(576 / 8));
         MARK_REG_WB(rd, t2);
      } else if (imm == 0x2581) { // Get EX_CONTEXT_0_1
         assign(t2, getIReg(584 / 8));
         MARK_REG_WB(rd, t2);
      } else
        use_dirty_helper = 1;
      break;
    case 152:  /* "mm" */
      use_dirty_helper = 1;
      break;
    case 153:  /* "mnz" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_And64,
                       unop(Iop_1Sto64, binop(Iop_CmpNE64,
                                              getIReg(ra),
                                              mkU64(0))),
                       getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 154:  /* "mtspr imm, ra" */
      if (imm == 0x2780) // Set Cmpexch value
        putIReg(70, getIReg(ra));
      else if (imm == 0x2580) // set EX_CONTEXT_0_0
        putIReg(576/8, getIReg(ra));
      else if (imm == 0x2581) // set EX_CONTEXT_0_1
        putIReg(584/8, getIReg(ra));
      else
        use_dirty_helper = 1;
      break;
    case 155:  /* "mul_hs_hs" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullS32,
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(ra),
                                  mkU8(32))),
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(rb),
                                  mkU8(32)))));
      MARK_REG_WB(rd, t2);
      break;
    case 156:  /* "mul_hs_hu" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);

      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           binop(Iop_Shr64, getIReg(ra), mkU8(32)))));
      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, binop(Iop_Shr64, getIReg(rb), mkU8(32)))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, binop(Iop_Shr64, getIReg(rb), mkU8(32)))));
      assign(t2, binop(Iop_Add64,
                       mkexpr(t1),
                       binop(Iop_Shl64,
                             mkexpr(t3),
                             mkU8(32))));
      MARK_REG_WB(rd, t2);
      break;
    case 157:  /* "mul_hs_ls" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullS32,
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(ra),
                                  mkU8(32))),
                       unop(Iop_64to32,
                            getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 158:  /* "mul_hs_lu" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);

      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           binop(Iop_Shr64, getIReg(ra), mkU8(32)))));
      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t2, binop(Iop_Add64,
                       mkexpr(t1),
                       binop(Iop_Shl64,
                             mkexpr(t3),
                             mkU8(32))));
      MARK_REG_WB(rd, t2);
      break;
    case 159:  /* "mul_hu_hu" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullU32,
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(ra),
                                  mkU8(32))),
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(rb),
                                  mkU8(32)))));
      MARK_REG_WB(rd, t2);
      break;
    case 160:  /* "mul_hu_ls" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);

      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           getIReg(ra))));

      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, binop(Iop_Shr64, getIReg(rb), mkU8(32)))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, binop(Iop_Shr64, getIReg(rb), mkU8(32)))));
      assign(t2, binop(Iop_Add64,
                       mkexpr(t1),
                       binop(Iop_Shl64,
                             mkexpr(t3),
                             mkU8(32))));
      MARK_REG_WB(rd, t2);
      break;
    case 161:  /* "mul_hu_lu" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullU32,
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(ra),
                                  mkU8(32))),
                       unop(Iop_64to32,
                            getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 162:  /* "mul_ls_ls" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullS32,
                       unop(Iop_64to32, getIReg(ra)),
                       unop(Iop_64to32, getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 163:  /* "mul_ls_lu" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);

      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32, getIReg(ra))));
      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t2, binop(Iop_Add64,
                       mkexpr(t1),
                       binop(Iop_Shl64,
                             mkexpr(t3),
                             mkU8(32))));
      MARK_REG_WB(rd, t2);
      break;
    case 164:   /* "mul_lu_lu" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullU32,
                       unop(Iop_64to32, getIReg(ra)),
                       unop(Iop_64to32, getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 165:   /* "mula_hs_hs" */
      t0 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);

      assign(t0, binop(Iop_MullS32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              getIReg(ra), mkU8(32))),
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              getIReg(rb), mkU8(32)))));
      assign(t2, binop(Iop_Add64, getIReg(rd), mkexpr(t0)));
      MARK_REG_WB(rd, t2);
      break;
    case 166:   /* "mula_hs_hu" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);
      t4 = newTemp(Ity_I64);
      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           binop(Iop_Shr64, getIReg(ra), mkU8(32)))));
      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              getIReg(rb), mkU8(32)))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              getIReg(rb), mkU8(32)))));
      assign(t2, binop(Iop_Add64,
                       mkexpr(t1),
                       binop(Iop_Shl64,
                             mkexpr(t3),
                             mkU8(32))));
      assign(t4, binop(Iop_Add64, getIReg(rd), mkexpr(t2)));
      MARK_REG_WB(rd, t4);
      break;
    case 167:   /* "mula_hs_ls" */
      t2 = newTemp(Ity_I64);
      t4 = newTemp(Ity_I64);
      assign(t2, binop(Iop_MullS32,
                       unop(Iop_64to32,
                            binop(Iop_Shr64,
                                  getIReg(ra),
                                  mkU8(32))),
                       unop(Iop_64to32,
                            getIReg(rb))));
      assign(t4, binop(Iop_Add64, getIReg(rd), mkexpr(t2)));
      MARK_REG_WB(rd, t4);
      break;
    case 168:   /* "mula_hs_lu" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);
      t4 = newTemp(Ity_I64);
      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           binop(Iop_Shr64, getIReg(ra), mkU8(32)))));
      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t2, binop(Iop_Add64,
                       mkexpr(t1),
                       binop(Iop_Shl64,
                             mkexpr(t3),
                             mkU8(32))));
      assign(t4, binop(Iop_Add64, getIReg(rd), mkexpr(t2)));
      MARK_REG_WB(rd, t4);
      break;
    case 169:   /* "mula_hu_hu" */
      use_dirty_helper = 1;
      break;
    case 170:   /* "mula_hu_ls" */
      use_dirty_helper = 1;
      break;
    case 171:   /* "mula_hu_lu" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64,
                       binop(Iop_MullU32,
                             unop(Iop_64to32,
                                  binop(Iop_Shr64,
                                        getIReg(ra),
                                        mkU8(32))),
                             unop(Iop_64to32,
                                  getIReg(rb))),
                       getIReg(rd)));
      MARK_REG_WB(rd, t2);
      break;
    case 172:  /* "mula_ls_ls" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64,
                       getIReg(rd),
                       binop(Iop_MullS32,
                             unop(Iop_64to32, getIReg(ra)),
                             unop(Iop_64to32, getIReg(rb)))));
      MARK_REG_WB(rd, t2);
      break;
    case 173:  /* "mula_ls_lu" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);

      assign(t0, unop(Iop_32Sto64,
                      unop(Iop_64to32, getIReg(ra))));
      assign(t1, binop(Iop_MullU32,
                       unop(Iop_64to32, mkexpr(t0)),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t3, binop(Iop_MullU32,
                       unop(Iop_64to32, binop(Iop_Shr64,
                                              mkexpr(t0),
                                              mkU8(32))),
                       unop(Iop_64to32, getIReg(rb))));
      assign(t2, binop(Iop_Add64,
                       getIReg(rd),
                       binop(Iop_Add64,
                             mkexpr(t1),
                             binop(Iop_Shl64,
                                   mkexpr(t3),
                                   mkU8(32)))));
      MARK_REG_WB(rd, t2);
      break;
    case 174:  /* "mula_lu_lu" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64,
                       binop(Iop_MullU32,
                             unop(Iop_64to32,
                                  getIReg(ra)),
                             unop(Iop_64to32,
                                  getIReg(rb))),
                       getIReg(rd)));
      MARK_REG_WB(rd, t2);
      break;
    case 175:   /* "mulax" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           binop(Iop_Add64,
                                 getIReg(rd),
                                 binop(Iop_MullU32,
                                       narrowTo(Ity_I32, getIReg(ra)),
                                       narrowTo(Ity_I32, getIReg(rb)))))));
      MARK_REG_WB(rd, t2);
      break;
    case 176:   /* "mulx" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_32Sto64,
                      unop(Iop_64to32,
                           binop(Iop_MullU32,
                                 narrowTo(Ity_I32, getIReg(ra)),
                                 narrowTo(Ity_I32, getIReg(rb))))));
      MARK_REG_WB(rd, t2);
      break;
    case 177:   /* "mz" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_And64,
                       unop(Iop_1Sto64, binop(Iop_CmpEQ64,
                                              getIReg(ra),
                                              mkU64(0))),
                       getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 178:  /* "nap" */
      break;
    case 179:  /* "nop" */
      break;
    case 180:  /* "nor" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_Not64,
                      binop(Iop_Or64,
                            getIReg(ra),
                            getIReg(rb))));
      MARK_REG_WB(rd, t2);
      break;
    case 181:  /* "or" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Or64,
                       getIReg(ra),
                       getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 182:  /* "ori" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Or64,
                       getIReg(ra),
                       mkU64(imm)));
      MARK_REG_WB(rd, t2);
      break;
    case 183:
      /* Fall-through */
    case 184:
      /* Fall-through */
    case 185:
      use_dirty_helper = 1;
      break;
    case 186:  /* "rotl" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t0, binop(Iop_Shl64,
                       getIReg(ra),
                       unop(Iop_64to8, getIReg(rb))));
      assign(t1, binop(Iop_Shr64,
                       getIReg(ra),
                       unop(Iop_64to8, binop(Iop_Sub64,
                                             mkU64(0),
                                             getIReg(rb)))));
      assign(t2, binop(Iop_Or64, mkexpr(t0), mkexpr(t1)));
      MARK_REG_WB(rd, t2);
      break;
    case 187:  /* "rotli" */
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      assign(t0, binop(Iop_Shl64,
                       getIReg(ra),
                       mkU8(imm)));
      assign(t1, binop(Iop_Shr64,
                       getIReg(ra),
                       mkU8(0 - imm)));
      assign(t2, binop(Iop_Or64, mkexpr(t0), mkexpr(t1)));
      MARK_REG_WB(rd, t2);
      break;
    case 188:   /* "shl" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Shl64,
                       getIReg(ra),
                       unop(Iop_64to8, getIReg(rb))));
      MARK_REG_WB(rd, t2);

      break;
    case 189:   /* "shl16insli" */
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);
      assign(t3, binop(Iop_Shl64, getIReg(ra), mkU8(16)));
      imm &= 0xFFFFULL;
      if (imm & 0x8000)
      {
        t4 = newTemp(Ity_I64);
        assign(t4, mkU64(imm));
        assign(t2, binop(Iop_Add64, mkexpr(t3), mkexpr(t4)));
      }
      else
      {
        assign(t2, binop(Iop_Add64, mkexpr(t3), mkU64(imm)));
      }
      MARK_REG_WB(rd, t2);

      break;
    case 190:   /* "shl1add" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64,
                       binop(Iop_Shl64,
                             getIReg(ra), mkU8(1)),
                       getIReg(rb)));

      MARK_REG_WB(rd, t2);
      break;
    case 191:   /* "shl1addx" */
      t2 = newTemp(Ity_I64);
      assign(t2,
             unop(Iop_32Sto64,
                  unop(Iop_64to32,
                       binop(Iop_Add64,
                             binop(Iop_Shl64,
                                   getIReg(ra), mkU8(1)),
                             getIReg(rb)))));
      MARK_REG_WB(rd, t2);
      break;
    case 192:   /* "shl2add" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64,
                       binop(Iop_Shl64,
                             getIReg(ra), mkU8(2)),
                       getIReg(rb)));

      MARK_REG_WB(rd, t2);

      break;
    case 193:   /* "shl2addx" */
      t2 = newTemp(Ity_I64);
      assign(t2,
             unop(Iop_32Sto64,
                  unop(Iop_64to32,
                       binop(Iop_Add64,
                             binop(Iop_Shl64,
                                   getIReg(ra), mkU8(2)),
                             getIReg(rb)))));
      MARK_REG_WB(rd, t2);

      break;
    case 194:   /* "shl3add" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Add64,
                       binop(Iop_Shl64,
                             getIReg(ra), mkU8(3)),
                       getIReg(rb)));

      MARK_REG_WB(rd, t2);
      break;
    case 195:   /* "shl3addx" */
      t2 = newTemp(Ity_I64);
      assign(t2,
             unop(Iop_32Sto64,
                  unop(Iop_64to32,
                       binop(Iop_Add64,
                             binop(Iop_Shl64,
                                   getIReg(ra), mkU8(3)),
                             getIReg(rb)))));
      MARK_REG_WB(rd, t2);
      break;
    case 196:   /* "shli" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Shl64, getIReg(ra),
                       mkU8(imm)));
      MARK_REG_WB(rd, t2);
      break;
    case 197:   /* "shlx" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_32Sto64,
                      binop(Iop_Shl32,
                            narrowTo(Ity_I32, getIReg(ra)),
                            narrowTo(Ity_I8, getIReg(rb)))));
      MARK_REG_WB(rd, t2);
      break;
    case 198:   /* "shlxi" */
      t2 = newTemp(Ity_I64);
      assign(t2, signExtend(binop(Iop_Shl32,
                                  narrowTo(Ity_I32, getIReg(ra)),
                                  mkU8(imm)),
                            32));
      MARK_REG_WB(rd, t2);
      break;
    case 199:  /* "shrs" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Sar64, getIReg(ra),
                       narrowTo(Ity_I8, getIReg(rb))));

      MARK_REG_WB(rd, t2);
      break;
    case 200:  /* "shrsi" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Sar64, getIReg(ra),
                       mkU8(imm)));

      MARK_REG_WB(rd, t2);
      break;
    case 201:  /* "shru" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Shr64,
                       getIReg(ra),
                       narrowTo(Ity_I8, (getIReg(rb)))));

      MARK_REG_WB(rd, t2);
      break;
    case 202:  /* "shrui" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Shr64, getIReg(ra), mkU8(imm)));

      MARK_REG_WB(rd, t2);
      break;
    case 203:  /* "shrux" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_32Sto64,
                      (binop(Iop_Shr32,
                             narrowTo(Ity_I32, getIReg(ra)),
                             narrowTo(Ity_I8, getIReg(rb))))));
      MARK_REG_WB(rd, t2);
      break;
    case 204:  /* "shruxi" */
      t2 = newTemp(Ity_I64);
      assign(t2, unop(Iop_32Sto64,
                      (binop(Iop_Shr32,
                             narrowTo(Ity_I32, getIReg(ra)),
                             mkU8(imm)))));
      MARK_REG_WB(rd, t2);
      break;
    case 205:  /* "shufflebytes" */
      use_dirty_helper = 1;
      break;
    case 206:  /* "st" */
      store(getIReg(ra),  getIReg(rb));
      break;
    case 207:  /* "st1" */
      store(getIReg(ra),  narrowTo(Ity_I8, getIReg(rb)));
      break;
    case 208:  /* "st1_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  narrowTo(Ity_I8, getIReg(opd[1])));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 209:  /* "st2" */
      store(getIReg(ra),  narrowTo(Ity_I16, getIReg(rb)));
      break;
    case 210:  /* "st2_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  narrowTo(Ity_I16, getIReg(opd[1])));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 211:  /* "st4" */
      store(getIReg(ra),  narrowTo(Ity_I32, getIReg(rb)));
      break;
    case 212:  /* "st4_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  narrowTo(Ity_I32, getIReg(opd[1])));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 213:  /* "st_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  getIReg(opd[1]));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 214:  /* "stnt" */
      store(getIReg(ra),  getIReg(rb));
      break;
    case 215:  /* "stnt1" */
      store(getIReg(ra),  narrowTo(Ity_I8, getIReg(rb)));
      break;
    case 216:  /* "stnt1_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  narrowTo(Ity_I8, getIReg(opd[1])));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 217:  /* "stnt2" */
      store(getIReg(ra),  narrowTo(Ity_I16, getIReg(rb)));
      break;
    case 218:  /* "stnt2_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  narrowTo(Ity_I16, getIReg(opd[1])));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 219:  /* "stnt4" */
      store(getIReg(ra),  narrowTo(Ity_I32, getIReg(rb)));
      break;
    case 220:  /* "stnt4_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  narrowTo(Ity_I32, getIReg(opd[1])));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 221:  /* "stnt_add" */
      t2 = newTemp(Ity_I64);
      store(getIReg(opd[0]),  getIReg(opd[1]));
      assign(t2, binop(Iop_Add64, getIReg(opd[0]), mkU64(opd[2])));
      MARK_REG_WB(opd[0], t2);
      break;
    case 222:  /* "sub" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Sub64, getIReg(ra),
                       getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 223:  /* "subx" */
      t2 = newTemp(Ity_I64);
      assign(t2,  unop(Iop_32Sto64,
                       binop(Iop_Sub32,
                             narrowTo(Ity_I32, getIReg(ra)),
                             narrowTo(Ity_I32, getIReg(rb)))));
      MARK_REG_WB(rd, t2);
      break;
    case 224:  /* "subxsc" */
      use_dirty_helper = 1;
      break;
    case 225:  /* "swint0" */
      vex_printf( "\n *** swint0 ***\n");
      vassert(0);
      break;
    case 226:  /* "swint1" */
      next = mkU64(guest_PC_curr_instr + 8);
      jumpkind = Ijk_Sys_syscall;
      break;
    case 227:  /* "swint2" */
      vex_printf( "\n *** swint2 ***\n");
      vassert(0);
      break;
    case 228:  /* "swint3" */
      vex_printf( "\n *** swint3 ***\n");
      vassert(0);
      break;
    case 229:
      /* Fall-through */
    case 230:
      /* Fall-through */
    case 231:
      /* Fall-through */
    case 232:
      /* Fall-through */
    case 233:
      use_dirty_helper = 1;
      break;
    case 234:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 235:
      /* Fall-through */
    case 236:
      /* Fall-through */
    case 237:
      use_dirty_helper = 1;
      break;
    case 238:  /* "v1cmpeq" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_CmpEQ8x8, getIReg(ra),
                       getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 239:  /* "v1cmpeqi" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_CmpEQ8x8, getIReg(ra),
                       mkU64(imm)));

      MARK_REG_WB(rd, t2);
      break;
    case 240:
      /* Fall-through */
    case 241:
      /* Fall-through */
    case 242:
      use_dirty_helper = 1;
      break;
    case 243:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
      /* Fall-through */
    case 244:
      use_dirty_helper = 1;
      break;
    case 245:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 246:  /* "v1cmpne" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_CmpEQ8x8,
                       binop(Iop_CmpEQ8x8, getIReg(ra),
                             getIReg(rb)),
                       getIReg(63)));
      MARK_REG_WB(rd, t2);
      break;
    case 247:
      /* Fall-through */
    case 248:
      /* Fall-through */
    case 249:
      /* Fall-through */
    case 250:
      /* Fall-through */
    case 251:
      /* Fall-through */
    case 252:
      /* Fall-through */
    case 253:
      /* Fall-through */
    case 254:
      /* Fall-through */
    case 255:
      /* Fall-through */
    case 256:
      /* Fall-through */
    case 257:
      /* Fall-through */
    case 258:
      /* Fall-through */
    case 259:
      use_dirty_helper = 1;
      break;
    case 260:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 261:
      use_dirty_helper = 1;
      break;
    case 262:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 263:
      /* Fall-through */
    case 264:
      /* Fall-through */
    case 265:
      /* Fall-through */
    case 266:
      /* Fall-through */
    case 267:
      /* Fall-through */
    case 268:
      /* Fall-through */
    case 269:
      /* Fall-through */
    case 270:
      use_dirty_helper = 1;
      break;
    case 271:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 272:
      use_dirty_helper = 1;
      break;
    case 273:
      opd[3] = V1EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 274:
      use_dirty_helper = 1;
      break;
    case 275:  /* "v1shrui" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Shr8x8,
                       getIReg(ra),
                       mkU64(imm)));
      MARK_REG_WB(rd, t2);
      break;
    case 276:
      /* Fall-through */
    case 277:
      /* Fall-through */
    case 278:
      use_dirty_helper = 1;
      break;
    case 279:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 280:
      /* Fall-through */
    case 281:
      /* Fall-through */
    case 282:
      /* Fall-through */
    case 283:
      use_dirty_helper = 1;
      break;
    case 284:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 285:
      /* Fall-through */
    case 286:
      /* Fall-through */
    case 287:
      use_dirty_helper = 1;
      break;
    case 288:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 289:
      use_dirty_helper = 1;
      break;
    case 290:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 291:
      /* Fall-through */
    case 292:
      /* Fall-through */
    case 293:
      /* Fall-through */
    case 294:
      /* Fall-through */
    case 295:
      /* Fall-through */
    case 296:
      use_dirty_helper = 1;
      break;
    case 297:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 298:
      use_dirty_helper = 1;
      break;
    case 299:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 300:
      /* Fall-through */
    case 301:
      /* Fall-through */
    case 302:
      /* Fall-through */
    case 303:
      /* Fall-through */
    case 304:
      /* Fall-through */
    case 305:
      /* Fall-through */
    case 306:
      /* Fall-through */
    case 307:
      /* Fall-through */
    case 308:
      /* Fall-through */
    case 309:
      /* Fall-through */
    case 310:
      /* Fall-through */
    case 311:
      /* Fall-through */
    case 312:
      use_dirty_helper = 1;
      break;
    case 313:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 314:
      /* Fall-through */
    case 315:
      use_dirty_helper = 1;
      break;
    case 316:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 317:
      use_dirty_helper = 1;
      break;
    case 318:
      opd[3] = V2EXP(opd[3]);
      use_dirty_helper = 1;
      break;
    case 319:
      /* Fall-through */
    case 320:
      /* Fall-through */
    case 321:
      /* Fall-through */
    case 322:
      /* Fall-through */
    case 323:
      use_dirty_helper = 1;
      break;
    case 324:   /* "v4int_l" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Or64,
                       binop(Iop_Shl64,
                             getIReg(ra),
                             mkU8(32)),
                       binop(Iop_And64,
                             getIReg(rb),
                             mkU64(0xFFFFFFFF))));
      MARK_REG_WB(rd, t2);
      break;
    case 325:
      /* Fall-through */
    case 326:
      /* Fall-through */
    case 327:
      /* Fall-through */
    case 328:
      /* Fall-through */
    case 329:
      /* Fall-through */
    case 330:
      /* Fall-through */
    case 331:
      use_dirty_helper = 1;
      break;
    case 332:   /* "wh64" */     /* Ignore store hint */
      break;
    case 333:   /* "xor" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Xor64,
                       getIReg(ra),
                       getIReg(rb)));
      MARK_REG_WB(rd, t2);
      break;
    case 334:   /* "xori" */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_Xor64,
                       getIReg(ra),
                       mkU64(imm)));
      MARK_REG_WB(rd, t2);
      break;
    case 335:  /* "(null)" */   /* ignore */
      break;
    default:

    decode_failure:
      vex_printf("error: %d\n",  (Int)opcode);

      /* All decode failures end up here. */
      vex_printf("vex tilegx->IR: unhandled instruction: "
                 "%s 0x%llx 0x%llx 0x%llx 0x%llx\n",
                 decoded[n].opcode->name,
                 opd[0], opd[1], opd[2], opd[3]);

      /* Tell the dispatcher that this insn cannot be decoded, and so has
         not been executed, and (is currently) the next to be executed. */
      stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_pc),
                      mkU64(guest_PC_curr_instr)));
      dres.whatNext = Dis_StopHere;
      dres.len = 0;
      return dres;
    }

    /* Hook the dirty helper for rare instruxtions. */
    if (use_dirty_helper)
    {
      Int i = 0;
      Int wbc = 0;
      IRExpr *opc_oprand[5];

      opc_oprand[0] = mkU64(opcode);

      /* Get the operand registers or immediate. */
      for (i = 0 ; i < 4; i++)
      {
        opc_oprand[i + 1] = NULL;

        if (opd_dst_map & (1ULL << i))
        {
          tb[wbc] = newTemp(Ity_I64);
          wbc++;
          opc_oprand[i + 1] = getIReg(opd[i]);
        }
        else if (opd_imm_map & (1ULL << i))
          opc_oprand[i + 1] = mkU64(opd[i]);
        else if (opd_src_map & (1ULL << i))
          opc_oprand[i + 1] = getIReg(opd[i]);
        else
          opc_oprand[i + 1] = mkU64(0xfeee);
      }

      IRExpr **args = mkIRExprVec_5(opc_oprand[0], opc_oprand[1],
                                    opc_oprand[2], opc_oprand[3],
                                    opc_oprand[4]);
      IRDirty *genIR = NULL;

      switch (wbc) {
      case 0:
        {
          genIR = unsafeIRDirty_0_N (0/*regparms*/,
                                     "tilegx_dirtyhelper_gen",
                                     &tilegx_dirtyhelper_gen,
                                     args);
        }
        break;
      case 1:
        {
          genIR = unsafeIRDirty_1_N (tb[0],
                                     0/*regparms*/,
                                     "tilegx_dirtyhelper_gen",
                                     &tilegx_dirtyhelper_gen,
                                     args);
        }
        break;
      default:
        vex_printf("opc = %d\n", (Int)opcode);
        vassert(0);
      }

      stmt(IRStmt_Dirty(genIR));

      wbc = 0;
      for (i = 0 ; i < 4; i++)
      {
        if(opd_dst_map & (1 << i))
        {
          /* Queue the writeback destination registers. */
          MARK_REG_WB(opd[i], tb[wbc]);
          wbc++;
        }
      }
    }
  }

  /* Write back registers for a bundle. Note have to get all source registers
     for all instructions in a bundle before write the destinations b/c this is
     an VLIW processor. */
  for (n = 0; n < rd_wb_index; n++)
    putIReg(rd_wb_reg[n], mkexpr(rd_wb_temp[n]));

  /* Add branch IR if apply finally, only upto one branch per bundle. */
  if (bstmt) {
    stmt(bstmt);
    dres.whatNext = Dis_StopHere;

    dres.jk_StopHere = jumpkind;
    stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_pc),
                    mkU64(guest_PC_curr_instr + 8)));
  } else if (next) {
    if (steering_pc != -1ULL) {
      if (resteerOkFn(callback_opaque, steering_pc)) {
        dres.whatNext   = Dis_ResteerU;
        dres.continueAt = steering_pc;
        stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_pc),
                        mkU64(steering_pc)));
      } else {
        dres.whatNext = Dis_StopHere;
        dres.jk_StopHere = jumpkind;
        stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_pc),
                        mkU64(steering_pc)));
      }
    } else {
      dres.whatNext = Dis_StopHere;
      dres.jk_StopHere = jumpkind;
      stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_pc), next));
    }
  } else {
    /* As dafault dres.whatNext = Dis_Continue. */
    stmt(IRStmt_Put(offsetof(VexGuestTILEGXState, guest_pc),
                    mkU64(guest_PC_curr_instr + 8)));
  }

  irsb->jumpkind = Ijk_Boring;
  irsb->next = NULL;
  dres.len = 8;

 decode_success:

  return dres;
}

/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult
disInstr_TILEGX ( IRSB* irsb_IN,
                  Bool (*resteerOkFn) (void *, Addr),
                  Bool resteerCisOk,
                  void* callback_opaque,
                  const UChar* guest_code_IN,
                  Long delta,
                  Addr guest_IP,
                  VexArch guest_arch,
                  const VexArchInfo* archinfo,
                  const VexAbiInfo* abiinfo,
                  VexEndness host_endness_IN,
                  Bool sigill_diag_IN )
{
  DisResult dres;

  /* Set globals (see top of this file) */
  vassert(guest_arch == VexArchTILEGX);

  guest_code = (UChar*)(Addr)guest_code_IN;
  irsb = irsb_IN;
  host_endness = host_endness_IN;
  guest_PC_curr_instr = (Addr64) guest_IP;
  guest_PC_bbstart = (Addr64) toUInt(guest_IP - delta);

  dres = disInstr_TILEGX_WRK(resteerOkFn, resteerCisOk,
                             callback_opaque,
                             delta, archinfo, abiinfo, sigill_diag_IN);

  return dres;
}

/*--------------------------------------------------------------------*/
/*--- end                                      guest_tilegx_toIR.c ---*/
/*--------------------------------------------------------------------*/
