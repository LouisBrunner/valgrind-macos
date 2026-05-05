
/*--------------------------------------------------------------------*/
/*--- begin                                    host_riscv64_defs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_trc_values.h"

#include "host_riscv64_defs.h"
#include "main_util.h"

/*------------------------------------------------------------*/
/*--- Registers                                            ---*/
/*------------------------------------------------------------*/

UInt ppHRegRISCV64(HReg reg)
{
   static const HChar* inames[32] = {
      "zero", "ra", "sp", "gp", "tp",  "t0",  "t1", "t2", "s0", "s1", "a0",
      "a1",   "a2", "a3", "a4", "a5",  "a6",  "a7", "s2", "s3", "s4", "s5",
      "s6",   "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"};

   static const HChar* fnames[32] = {
      "ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
      "fs0", "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
      "fa6", "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
      "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11"};

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg))
      return ppHReg(reg);

   /* Be specific for real regs. */
   switch (hregClass(reg)) {
   case HRcInt64: {
      UInt r = hregEncoding(reg);
      vassert(r < 32);
      return vex_printf("%s", inames[r]);
   }
   case HRcFlt64: {
      UInt r = hregEncoding(reg);
      vassert(r < 32);
      return vex_printf("%s", fnames[r]);
   }
   default:
      vpanic("ppHRegRISCV64");
   }
}

static inline UInt iregEnc(HReg r)
{
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));

   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt fregEnc(HReg r)
{
   UInt n;
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

/*------------------------------------------------------------*/
/*--- Instructions                                         ---*/
/*------------------------------------------------------------*/

static const HChar* showRISCV64CSR(UInt csr)
{
   switch (csr) {
   case 0x001:
      return "fflags";
   case 0x002:
      return "frm";
   case 0x003:
      return "fcsr";
   }
   vpanic("showRISCV64CSR");
}

static const HChar* showRISCV64ALUOp(RISCV64ALUOp op)
{
   switch (op) {
   case RISCV64op_ADD:
      return "add";
   case RISCV64op_SUB:
      return "sub";
   case RISCV64op_ADDW:
      return "addw";
   case RISCV64op_SUBW:
      return "subw";
   case RISCV64op_XOR:
      return "xor";
   case RISCV64op_OR:
      return "or";
   case RISCV64op_AND:
      return "and";
   case RISCV64op_SLL:
      return "sll";
   case RISCV64op_SRL:
      return "srl";
   case RISCV64op_SRA:
      return "sra";
   case RISCV64op_SLLW:
      return "sllw";
   case RISCV64op_SRLW:
      return "srlw";
   case RISCV64op_SRAW:
      return "sraw";
   case RISCV64op_SLT:
      return "slt";
   case RISCV64op_SLTU:
      return "sltu";
   case RISCV64op_MUL:
      return "mul";
   case RISCV64op_MULH:
      return "mulh";
   case RISCV64op_MULHU:
      return "mulhu";
   case RISCV64op_DIV:
      return "div";
   case RISCV64op_DIVU:
      return "divu";
   case RISCV64op_REM:
      return "rem";
   case RISCV64op_REMU:
      return "remu";
   case RISCV64op_MULW:
      return "mulw";
   case RISCV64op_DIVW:
      return "divw";
   case RISCV64op_DIVUW:
      return "divuw";
   case RISCV64op_REMW:
      return "remw";
   case RISCV64op_REMUW:
      return "remuw";
   }
   vpanic("showRISCV64ALUOp");
}

static const HChar* showRISCV64ALUImmOp(RISCV64ALUImmOp op)
{
   switch (op) {
   case RISCV64op_ADDI:
      return "addi";
   case RISCV64op_ADDIW:
      return "addiw";
   case RISCV64op_XORI:
      return "xori";
   case RISCV64op_ANDI:
      return "andi";
   case RISCV64op_SLLI:
      return "slli";
   case RISCV64op_SRLI:
      return "srli";
   case RISCV64op_SRAI:
      return "srai";
   case RISCV64op_SLTIU:
      return "sltiu";
   }
   vpanic("showRISCV64ALUImmOp");
}

static const HChar* showRISCV64LoadOp(RISCV64LoadOp op)
{
   switch (op) {
   case RISCV64op_LD:
      return "ld";
   case RISCV64op_LW:
      return "lw";
   case RISCV64op_LH:
      return "lh";
   case RISCV64op_LB:
      return "lb";
   }
   vpanic("showRISCV64LoadOp");
}

static const HChar* showRISCV64StoreOp(RISCV64StoreOp op)
{
   switch (op) {
   case RISCV64op_SD:
      return "sd";
   case RISCV64op_SW:
      return "sw";
   case RISCV64op_SH:
      return "sh";
   case RISCV64op_SB:
      return "sb";
   }
   vpanic("showRISCV64StoreOp");
}

static const HChar* showRISCV64LoadROp(RISCV64LoadROp op)
{
   switch (op) {
   case RISCV64op_LR_W:
      return "lr.w";
   }
   vpanic("showRISCV64LoadROp");
}

static const HChar* showRISCV64StoreCOp(RISCV64StoreCOp op)
{
   switch (op) {
   case RISCV64op_SC_W:
      return "sc.w";
   }
   vpanic("showRISCV64StoreCOp");
}

static const HChar* showRISCV64FpUnaryOp(RISCV64FpUnaryOp op)
{
   switch (op) {
   case RISCV64op_FSQRT_S:
      return "fsqrt.s";
   case RISCV64op_FSQRT_D:
      return "fsqrt.d";
   }
   vpanic("showRISCV64FpUnaryOp");
}

static const HChar* showRISCV64FpBinaryOp(RISCV64FpBinaryOp op)
{
   switch (op) {
   case RISCV64op_FADD_S:
      return "fadd.s";
   case RISCV64op_FMUL_S:
      return "fmul.s";
   case RISCV64op_FDIV_S:
      return "fdiv.s";
   case RISCV64op_FSGNJN_S:
      return "fsgnjn.s";
   case RISCV64op_FSGNJX_S:
      return "fsgnjx.s";
   case RISCV64op_FMIN_S:
      return "fmin.s";
   case RISCV64op_FMAX_S:
      return "fmax.s";
   case RISCV64op_FADD_D:
      return "fadd.d";
   case RISCV64op_FSUB_D:
      return "fsub.d";
   case RISCV64op_FMUL_D:
      return "fmul.d";
   case RISCV64op_FDIV_D:
      return "fdiv.d";
   case RISCV64op_FSGNJN_D:
      return "fsgnjn.d";
   case RISCV64op_FSGNJX_D:
      return "fsgnjx.d";
   case RISCV64op_FMIN_D:
      return "fmin.d";
   case RISCV64op_FMAX_D:
      return "fmax.d";
   }
   vpanic("showRISCV64FpBinaryOp");
}

static const HChar* showRISCV64FpTernaryOp(RISCV64FpTernaryOp op)
{
   switch (op) {
   case RISCV64op_FMADD_S:
      return "fmadd.s";
   case RISCV64op_FMADD_D:
      return "fmadd.d";
   }
   vpanic("showRISCV64FpTernaryOp");
}

static const HChar* showRISCV64FpMoveOp(RISCV64FpMoveOp op)
{
   switch (op) {
   case RISCV64op_FMV_X_W:
      return "fmv.x.w";
   case RISCV64op_FMV_W_X:
      return "fmv.w.x";
   case RISCV64op_FMV_D:
      return "fmv.d";
   case RISCV64op_FMV_X_D:
      return "fmv.x.d";
   case RISCV64op_FMV_D_X:
      return "fmv.d.x";
   }
   vpanic("showRISCV64FpMoveOp");
}

static const HChar* showRISCV64FpConvertOp(RISCV64FpConvertOp op)
{
   switch (op) {
   case RISCV64op_FCVT_W_S:
      return "fcvt.w.s";
   case RISCV64op_FCVT_WU_S:
      return "fcvt.wu.s";
   case RISCV64op_FCVT_S_W:
      return "fcvt.s.w";
   case RISCV64op_FCVT_S_WU:
      return "fcvt.s.wu";
   case RISCV64op_FCVT_L_S:
      return "fcvt.l.s";
   case RISCV64op_FCVT_LU_S:
      return "fcvt.lu.s";
   case RISCV64op_FCVT_S_L:
      return "fcvt.s.l";
   case RISCV64op_FCVT_S_LU:
      return "fcvt.s.lu";
   case RISCV64op_FCVT_S_D:
      return "fcvt.s.d";
   case RISCV64op_FCVT_D_S:
      return "fcvt.d.s";
   case RISCV64op_FCVT_W_D:
      return "fcvt.w.d";
   case RISCV64op_FCVT_WU_D:
      return "fcvt.wu.d";
   case RISCV64op_FCVT_D_W:
      return "fcvt.d.w";
   case RISCV64op_FCVT_D_WU:
      return "fcvt.d.wu";
   case RISCV64op_FCVT_L_D:
      return "fcvt.l.d";
   case RISCV64op_FCVT_LU_D:
      return "fcvt.lu.d";
   case RISCV64op_FCVT_D_L:
      return "fcvt.d.l";
   case RISCV64op_FCVT_D_LU:
      return "fcvt.d.lu";
   }
   vpanic("showRISCV64FpConvertOp");
}

static const HChar* showRISCV64FpCompareOp(RISCV64FpCompareOp op)
{
   switch (op) {
   case RISCV64op_FEQ_S:
      return "feq.s";
   case RISCV64op_FLT_S:
      return "flt.s";
   case RISCV64op_FEQ_D:
      return "feq.d";
   case RISCV64op_FLT_D:
      return "flt.d";
   }
   vpanic("showRISCV64FpCompareOp");
}

static const HChar* showRISCV64FpLdStOp(RISCV64FpLdStOp op)
{
   switch (op) {
   case RISCV64op_FLW:
      return "flw";
   case RISCV64op_FLD:
      return "fld";
   case RISCV64op_FSW:
      return "fsw";
   case RISCV64op_FSD:
      return "fsd";
   }
   vpanic("showRISCV64FpLdStOp");
}

RISCV64Instr* RISCV64Instr_LI(HReg dst, ULong imm64)
{
   RISCV64Instr* i       = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                = RISCV64in_LI;
   i->RISCV64in.LI.dst   = dst;
   i->RISCV64in.LI.imm64 = imm64;
   return i;
}

RISCV64Instr* RISCV64Instr_MV(HReg dst, HReg src)
{
   RISCV64Instr* i     = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag              = RISCV64in_MV;
   i->RISCV64in.MV.dst = dst;
   i->RISCV64in.MV.src = src;
   return i;
}

RISCV64Instr* RISCV64Instr_ALU(RISCV64ALUOp op, HReg dst, HReg src1, HReg src2)
{
   RISCV64Instr* i       = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                = RISCV64in_ALU;
   i->RISCV64in.ALU.op   = op;
   i->RISCV64in.ALU.dst  = dst;
   i->RISCV64in.ALU.src1 = src1;
   i->RISCV64in.ALU.src2 = src2;
   return i;
}

RISCV64Instr*
RISCV64Instr_ALUImm(RISCV64ALUImmOp op, HReg dst, HReg src, Int imm12)
{
   RISCV64Instr* i           = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                    = RISCV64in_ALUImm;
   i->RISCV64in.ALUImm.op    = op;
   i->RISCV64in.ALUImm.dst   = dst;
   i->RISCV64in.ALUImm.src   = src;
   i->RISCV64in.ALUImm.imm12 = imm12;
   return i;
}

RISCV64Instr*
RISCV64Instr_Load(RISCV64LoadOp op, HReg dst, HReg base, Int soff12)
{
   RISCV64Instr* i          = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                   = RISCV64in_Load;
   i->RISCV64in.Load.op     = op;
   i->RISCV64in.Load.dst    = dst;
   i->RISCV64in.Load.base   = base;
   i->RISCV64in.Load.soff12 = soff12;
   return i;
}

RISCV64Instr*
RISCV64Instr_Store(RISCV64StoreOp op, HReg src, HReg base, Int soff12)
{
   RISCV64Instr* i           = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                    = RISCV64in_Store;
   i->RISCV64in.Store.op     = op;
   i->RISCV64in.Store.src    = src;
   i->RISCV64in.Store.base   = base;
   i->RISCV64in.Store.soff12 = soff12;
   return i;
}

RISCV64Instr* RISCV64Instr_LoadR(RISCV64LoadROp op, HReg dst, HReg addr)
{
   RISCV64Instr* i         = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                  = RISCV64in_LoadR;
   i->RISCV64in.LoadR.op   = op;
   i->RISCV64in.LoadR.dst  = dst;
   i->RISCV64in.LoadR.addr = addr;
   return i;
}

RISCV64Instr*
RISCV64Instr_StoreC(RISCV64StoreCOp op, HReg res, HReg src, HReg addr)
{
   RISCV64Instr* i          = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                   = RISCV64in_StoreC;
   i->RISCV64in.StoreC.op   = op;
   i->RISCV64in.StoreC.res  = res;
   i->RISCV64in.StoreC.src  = src;
   i->RISCV64in.StoreC.addr = addr;
   return i;
}

RISCV64Instr* RISCV64Instr_CSRRW(HReg dst, HReg src, UInt csr)
{
   RISCV64Instr* i        = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                 = RISCV64in_CSRRW;
   i->RISCV64in.CSRRW.dst = dst;
   i->RISCV64in.CSRRW.src = src;
   i->RISCV64in.CSRRW.csr = csr;
   return i;
}

RISCV64Instr* RISCV64Instr_FpUnary(RISCV64FpUnaryOp op, HReg dst, HReg src)
{
   RISCV64Instr* i          = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                   = RISCV64in_FpUnary;
   i->RISCV64in.FpUnary.op  = op;
   i->RISCV64in.FpUnary.dst = dst;
   i->RISCV64in.FpUnary.src = src;
   return i;
}

RISCV64Instr*
RISCV64Instr_FpBinary(RISCV64FpBinaryOp op, HReg dst, HReg src1, HReg src2)
{
   RISCV64Instr* i            = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                     = RISCV64in_FpBinary;
   i->RISCV64in.FpBinary.op   = op;
   i->RISCV64in.FpBinary.dst  = dst;
   i->RISCV64in.FpBinary.src1 = src1;
   i->RISCV64in.FpBinary.src2 = src2;
   return i;
}

RISCV64Instr* RISCV64Instr_FpTernary(
   RISCV64FpTernaryOp op, HReg dst, HReg src1, HReg src2, HReg src3)
{
   RISCV64Instr* i             = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                      = RISCV64in_FpTernary;
   i->RISCV64in.FpTernary.op   = op;
   i->RISCV64in.FpTernary.dst  = dst;
   i->RISCV64in.FpTernary.src1 = src1;
   i->RISCV64in.FpTernary.src2 = src2;
   i->RISCV64in.FpTernary.src3 = src3;
   return i;
}

RISCV64Instr* RISCV64Instr_FpMove(RISCV64FpMoveOp op, HReg dst, HReg src)
{
   RISCV64Instr* i         = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                  = RISCV64in_FpMove;
   i->RISCV64in.FpMove.op  = op;
   i->RISCV64in.FpMove.dst = dst;
   i->RISCV64in.FpMove.src = src;
   return i;
}

RISCV64Instr* RISCV64Instr_FpConvert(RISCV64FpConvertOp op, HReg dst, HReg src)
{
   RISCV64Instr* i            = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                     = RISCV64in_FpConvert;
   i->RISCV64in.FpConvert.op  = op;
   i->RISCV64in.FpConvert.dst = dst;
   i->RISCV64in.FpConvert.src = src;
   return i;
}

RISCV64Instr*
RISCV64Instr_FpCompare(RISCV64FpCompareOp op, HReg dst, HReg src1, HReg src2)
{
   RISCV64Instr* i             = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                      = RISCV64in_FpCompare;
   i->RISCV64in.FpCompare.op   = op;
   i->RISCV64in.FpCompare.dst  = dst;
   i->RISCV64in.FpCompare.src1 = src1;
   i->RISCV64in.FpCompare.src2 = src2;
   return i;
}

RISCV64Instr*
RISCV64Instr_FpLdSt(RISCV64FpLdStOp op, HReg reg, HReg base, Int soff12)
{
   RISCV64Instr* i            = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                     = RISCV64in_FpLdSt;
   i->RISCV64in.FpLdSt.op     = op;
   i->RISCV64in.FpLdSt.reg    = reg;
   i->RISCV64in.FpLdSt.base   = base;
   i->RISCV64in.FpLdSt.soff12 = soff12;
   return i;
}

RISCV64Instr*
RISCV64Instr_FpCSEL(HReg dst, HReg iftrue, HReg iffalse, HReg cond)
{
   RISCV64Instr* i             = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                      = RISCV64in_FpCSEL;
   i->RISCV64in.FpCSEL.dst     = dst;
   i->RISCV64in.FpCSEL.iftrue  = iftrue;
   i->RISCV64in.FpCSEL.iffalse = iffalse;
   i->RISCV64in.FpCSEL.cond    = cond;
   return i;
}

RISCV64Instr*
RISCV64Instr_CAS(RISCV64CASOp op, HReg old, HReg addr, HReg expd, HReg data)
{
   RISCV64Instr* i       = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                = RISCV64in_CAS;
   i->RISCV64in.CAS.op   = op;
   i->RISCV64in.CAS.old  = old;
   i->RISCV64in.CAS.addr = addr;
   i->RISCV64in.CAS.expd = expd;
   i->RISCV64in.CAS.data = data;
   return i;
}

RISCV64Instr* RISCV64Instr_FENCE(void)
{
   RISCV64Instr* i = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag          = RISCV64in_FENCE;
   return i;
}

RISCV64Instr* RISCV64Instr_CSEL(HReg dst, HReg iftrue, HReg iffalse, HReg cond)
{
   RISCV64Instr* i           = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                    = RISCV64in_CSEL;
   i->RISCV64in.CSEL.dst     = dst;
   i->RISCV64in.CSEL.iftrue  = iftrue;
   i->RISCV64in.CSEL.iffalse = iffalse;
   i->RISCV64in.CSEL.cond    = cond;
   return i;
}

RISCV64Instr* RISCV64Instr_Call(
   RetLoc rloc, Addr64 target, HReg cond, UChar nArgRegs, UChar nFArgRegs)
{
   RISCV64Instr* i             = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                      = RISCV64in_Call;
   i->RISCV64in.Call.rloc      = rloc;
   i->RISCV64in.Call.target    = target;
   i->RISCV64in.Call.cond      = cond;
   i->RISCV64in.Call.nArgRegs  = nArgRegs;
   i->RISCV64in.Call.nFArgRegs = nFArgRegs;
   return i;
}

RISCV64Instr* RISCV64Instr_XDirect(
   Addr64 dstGA, HReg base, Int soff12, HReg cond, Bool toFastEP)
{
   RISCV64Instr* i               = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                        = RISCV64in_XDirect;
   i->RISCV64in.XDirect.dstGA    = dstGA;
   i->RISCV64in.XDirect.base     = base;
   i->RISCV64in.XDirect.soff12   = soff12;
   i->RISCV64in.XDirect.cond     = cond;
   i->RISCV64in.XDirect.toFastEP = toFastEP;
   return i;
}

RISCV64Instr* RISCV64Instr_XIndir(HReg dstGA, HReg base, Int soff12, HReg cond)
{
   RISCV64Instr* i            = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                     = RISCV64in_XIndir;
   i->RISCV64in.XIndir.dstGA  = dstGA;
   i->RISCV64in.XIndir.base   = base;
   i->RISCV64in.XIndir.soff12 = soff12;
   i->RISCV64in.XIndir.cond   = cond;
   return i;
}

RISCV64Instr* RISCV64Instr_XAssisted(
   HReg dstGA, HReg base, Int soff12, HReg cond, IRJumpKind jk)
{
   RISCV64Instr* i               = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag                        = RISCV64in_XAssisted;
   i->RISCV64in.XAssisted.dstGA  = dstGA;
   i->RISCV64in.XAssisted.base   = base;
   i->RISCV64in.XAssisted.soff12 = soff12;
   i->RISCV64in.XAssisted.cond   = cond;
   i->RISCV64in.XAssisted.jk     = jk;
   return i;
}

RISCV64Instr* RISCV64Instr_EvCheck(HReg base_amCounter,
                                   Int  soff12_amCounter,
                                   HReg base_amFailAddr,
                                   Int  soff12_amFailAddr)
{
   RISCV64Instr* i = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag          = RISCV64in_EvCheck;
   i->RISCV64in.EvCheck.base_amCounter    = base_amCounter;
   i->RISCV64in.EvCheck.soff12_amCounter  = soff12_amCounter;
   i->RISCV64in.EvCheck.base_amFailAddr   = base_amFailAddr;
   i->RISCV64in.EvCheck.soff12_amFailAddr = soff12_amFailAddr;
   return i;
}

RISCV64Instr* RISCV64Instr_ProfInc(void)
{
   RISCV64Instr* i = LibVEX_Alloc_inline(sizeof(RISCV64Instr));
   i->tag          = RISCV64in_ProfInc;
   return i;
}

void ppRISCV64Instr(const RISCV64Instr* i, Bool mode64)
{
   vassert(mode64 == True);

   switch (i->tag) {
   case RISCV64in_LI:
      vex_printf("li      ");
      ppHRegRISCV64(i->RISCV64in.LI.dst);
      vex_printf(", 0x%llx", i->RISCV64in.LI.imm64);
      return;
   case RISCV64in_MV:
      vex_printf("mv      ");
      ppHRegRISCV64(i->RISCV64in.MV.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.MV.src);
      return;
   case RISCV64in_ALU:
      vex_printf("%-7s ", showRISCV64ALUOp(i->RISCV64in.ALU.op));
      ppHRegRISCV64(i->RISCV64in.ALU.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.ALU.src1);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.ALU.src2);
      return;
   case RISCV64in_ALUImm:
      vex_printf("%-7s ", showRISCV64ALUImmOp(i->RISCV64in.ALUImm.op));
      ppHRegRISCV64(i->RISCV64in.ALUImm.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.ALUImm.src);
      vex_printf(", %d", i->RISCV64in.ALUImm.imm12);
      return;
   case RISCV64in_Load:
      vex_printf("%-7s ", showRISCV64LoadOp(i->RISCV64in.Load.op));
      ppHRegRISCV64(i->RISCV64in.Load.dst);
      vex_printf(", %d(", i->RISCV64in.Load.soff12);
      ppHRegRISCV64(i->RISCV64in.Load.base);
      vex_printf(")");
      return;
   case RISCV64in_Store:
      vex_printf("%-7s ", showRISCV64StoreOp(i->RISCV64in.Store.op));
      ppHRegRISCV64(i->RISCV64in.Store.src);
      vex_printf(", %d(", i->RISCV64in.Store.soff12);
      ppHRegRISCV64(i->RISCV64in.Store.base);
      vex_printf(")");
      return;
   case RISCV64in_LoadR:
      vex_printf("%-7s ", showRISCV64LoadROp(i->RISCV64in.LoadR.op));
      ppHRegRISCV64(i->RISCV64in.LoadR.dst);
      vex_printf(", (");
      ppHRegRISCV64(i->RISCV64in.LoadR.addr);
      vex_printf(")");
      return;
   case RISCV64in_StoreC:
      vex_printf("%-7s ", showRISCV64StoreCOp(i->RISCV64in.StoreC.op));
      ppHRegRISCV64(i->RISCV64in.StoreC.res);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.StoreC.src);
      vex_printf(", (");
      ppHRegRISCV64(i->RISCV64in.StoreC.addr);
      vex_printf(")");
      return;
   case RISCV64in_CSRRW:
      vex_printf("csrrw   ");
      ppHRegRISCV64(i->RISCV64in.CSRRW.dst);
      vex_printf(", %s, ", showRISCV64CSR(i->RISCV64in.CSRRW.csr));
      ppHRegRISCV64(i->RISCV64in.CSRRW.src);
      return;
   case RISCV64in_FpUnary:
      vex_printf("%-7s ", showRISCV64FpUnaryOp(i->RISCV64in.FpUnary.op));
      ppHRegRISCV64(i->RISCV64in.FpUnary.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpUnary.src);
      return;
   case RISCV64in_FpBinary:
      vex_printf("%-7s ", showRISCV64FpBinaryOp(i->RISCV64in.FpBinary.op));
      ppHRegRISCV64(i->RISCV64in.FpBinary.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpBinary.src1);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpBinary.src2);
      return;
   case RISCV64in_FpTernary:
      vex_printf("%-7s ", showRISCV64FpTernaryOp(i->RISCV64in.FpTernary.op));
      ppHRegRISCV64(i->RISCV64in.FpTernary.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpTernary.src1);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpTernary.src2);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpTernary.src3);
      return;
   case RISCV64in_FpMove:
      vex_printf("%-7s ", showRISCV64FpMoveOp(i->RISCV64in.FpMove.op));
      ppHRegRISCV64(i->RISCV64in.FpMove.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpMove.src);
      return;
   case RISCV64in_FpConvert:
      vex_printf("%-7s ", showRISCV64FpConvertOp(i->RISCV64in.FpConvert.op));
      ppHRegRISCV64(i->RISCV64in.FpConvert.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpConvert.src);
      return;
   case RISCV64in_FpCompare:
      vex_printf("%-7s ", showRISCV64FpCompareOp(i->RISCV64in.FpCompare.op));
      ppHRegRISCV64(i->RISCV64in.FpCompare.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpCompare.src1);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpCompare.src2);
      return;
   case RISCV64in_FpLdSt:
      vex_printf("%-7s ", showRISCV64FpLdStOp(i->RISCV64in.FpLdSt.op));
      ppHRegRISCV64(i->RISCV64in.FpLdSt.reg);
      vex_printf(", %d(", i->RISCV64in.FpLdSt.soff12);
      ppHRegRISCV64(i->RISCV64in.FpLdSt.base);
      vex_printf(")");
      return;
   case RISCV64in_FpCSEL: {
      vex_printf("(FpCSEL) beq ");
      ppHRegRISCV64(i->RISCV64in.FpCSEL.cond);
      vex_printf(", zero, 1f; fmv.d ");
      ppHRegRISCV64(i->RISCV64in.FpCSEL.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpCSEL.iftrue);
      vex_printf("; c.j 2f; 1: fmv.d ");
      ppHRegRISCV64(i->RISCV64in.FpCSEL.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.FpCSEL.iffalse);
      vex_printf("; 2:");
      return;
   }
   case RISCV64in_CAS: {
      vassert(i->RISCV64in.CAS.op == RISCV64op_CAS_D ||
              i->RISCV64in.CAS.op == RISCV64op_CAS_W);
      Bool is_d = i->RISCV64in.CAS.op == RISCV64op_CAS_D;
      vex_printf("(%s) 1: %s ", is_d ? "CAS_D" : "CAS_W",
                 is_d ? "lr.d" : "lr.w");
      ppHRegRISCV64(i->RISCV64in.CAS.old);
      vex_printf(", (");
      ppHRegRISCV64(i->RISCV64in.CAS.addr);
      vex_printf("); bne ");
      ppHRegRISCV64(i->RISCV64in.CAS.old);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.CAS.expd);
      vex_printf(", 2f; %s t0, ", is_d ? "sc.d" : "sc.w");
      ppHRegRISCV64(i->RISCV64in.CAS.data);
      vex_printf(", (");
      ppHRegRISCV64(i->RISCV64in.CAS.addr);
      vex_printf("); bne t0, zero, 1b; 2:");
      return;
   }
   case RISCV64in_FENCE:
      vex_printf("fence");
      return;
   case RISCV64in_CSEL:
      vex_printf("(CSEL) beq ");
      ppHRegRISCV64(i->RISCV64in.CSEL.cond);
      vex_printf(", zero, 1f; c.mv ");
      ppHRegRISCV64(i->RISCV64in.CSEL.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.CSEL.iftrue);
      vex_printf("; c.j 2f; 1: c.mv ");
      ppHRegRISCV64(i->RISCV64in.CSEL.dst);
      vex_printf(", ");
      ppHRegRISCV64(i->RISCV64in.CSEL.iffalse);
      vex_printf("; 2:");
      return;
   case RISCV64in_Call:
      vex_printf("(Call) ");
      if (!hregIsInvalid(i->RISCV64in.Call.cond)) {
         vex_printf("beq ");
         ppHRegRISCV64(i->RISCV64in.Call.cond);
         vex_printf(", zero, 1f; ");
      }
      vex_printf("li t0, 0x%llx; c.jalr 0(t0) [nArgRegs=%u, nFArgRegs=%u, ",
                 i->RISCV64in.Call.target, i->RISCV64in.Call.nArgRegs,
                 i->RISCV64in.Call.nFArgRegs);
      ppRetLoc(i->RISCV64in.Call.rloc);
      vex_printf("]; 1:");
      return;
   case RISCV64in_XDirect:
      vex_printf("(xDirect) ");
      if (!hregIsInvalid(i->RISCV64in.XDirect.cond)) {
         vex_printf("beq ");
         ppHRegRISCV64(i->RISCV64in.XDirect.cond);
         vex_printf(", zero, 1f; ");
      }
      vex_printf("li t0, 0x%llx; sd t0, %d(", i->RISCV64in.XDirect.dstGA,
                 i->RISCV64in.XDirect.soff12);
      ppHRegRISCV64(i->RISCV64in.XDirect.base);
      vex_printf("); li t0, <%s>; c.jalr 0(t0); 1:",
                 i->RISCV64in.XDirect.toFastEP ? "disp_cp_chain_me_to_fastEP"
                                               : "disp_cp_chain_me_to_slowEP");
      return;
   case RISCV64in_XIndir:
      vex_printf("(xIndir) ");
      if (!hregIsInvalid(i->RISCV64in.XIndir.cond)) {
         vex_printf("beq ");
         ppHRegRISCV64(i->RISCV64in.XIndir.cond);
         vex_printf(", zero, 1f; ");
      }
      vex_printf("sd ");
      ppHRegRISCV64(i->RISCV64in.XIndir.dstGA);
      vex_printf(", %d(", i->RISCV64in.XIndir.soff12);
      ppHRegRISCV64(i->RISCV64in.XIndir.base);
      vex_printf("); li t0, <disp_cp_xindir>; c.jr 0(t0); 1:");
      return;
   case RISCV64in_XAssisted:
      vex_printf("(xAssisted) ");
      if (!hregIsInvalid(i->RISCV64in.XAssisted.cond)) {
         vex_printf("beq ");
         ppHRegRISCV64(i->RISCV64in.XAssisted.cond);
         vex_printf(", zero, 1f; ");
      }
      vex_printf("sd ");
      ppHRegRISCV64(i->RISCV64in.XAssisted.dstGA);
      vex_printf(", %d(", i->RISCV64in.XAssisted.soff12);
      ppHRegRISCV64(i->RISCV64in.XAssisted.base);
      vex_printf("); mv s0, $IRJumpKind_to_TRCVAL(%d)",
                 (Int)i->RISCV64in.XAssisted.jk);
      vex_printf("; li t0, <disp_cp_xassisted>; c.jr 0(t0); 1:");
      return;
   case RISCV64in_EvCheck:
      vex_printf("(evCheck) lw t0, %d(", i->RISCV64in.EvCheck.soff12_amCounter);
      ppHRegRISCV64(i->RISCV64in.EvCheck.base_amCounter);
      vex_printf("); c.addiw t0, -1; sw t0, %d(",
                 i->RISCV64in.EvCheck.soff12_amCounter);
      ppHRegRISCV64(i->RISCV64in.EvCheck.base_amCounter);
      vex_printf("); bge t0, zero, 1f; ld t0, %d(",
                 i->RISCV64in.EvCheck.soff12_amFailAddr);
      ppHRegRISCV64(i->RISCV64in.EvCheck.base_amFailAddr);
      vex_printf("); c.jr 0(t0); 1:");
      return;
   case RISCV64in_ProfInc:
      vex_printf("(profInc) li t1, $NotKnownYet; "
                 "ld t0, 0(t1); c.addi t0, t0, 1; sd t0, 0(t1)");
      return;
   default:
      vpanic("ppRISCV64Instr");
   }
}

/*------------------------------------------------------------*/
/*--- Helpers for register allocation                      ---*/
/*------------------------------------------------------------*/

/* Initialise and return the "register universe", i.e. a list of all hardware
   registers. Called once. */
const RRegUniverse* getRRegUniverse_RISCV64(void)
{
   static RRegUniverse all_regs;
   static Bool         initialised = False;
   RRegUniverse*       ru          = &all_regs;

   if (LIKELY(initialised))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers that are available to the register allocator. */
   ru->allocable_start[HRcInt64] = ru->size;
   ru->regs[ru->size++]          = hregRISCV64_x18(); /* s2 */
   ru->regs[ru->size++]          = hregRISCV64_x19(); /* s3 */
   ru->regs[ru->size++]          = hregRISCV64_x20(); /* s4 */
   ru->regs[ru->size++]          = hregRISCV64_x21(); /* s5 */
   ru->regs[ru->size++]          = hregRISCV64_x22(); /* s6 */
   ru->regs[ru->size++]          = hregRISCV64_x23(); /* s7 */
   ru->regs[ru->size++]          = hregRISCV64_x24(); /* s8 */
   ru->regs[ru->size++]          = hregRISCV64_x25(); /* s9 */
   ru->regs[ru->size++]          = hregRISCV64_x26(); /* s10 */
   ru->regs[ru->size++]          = hregRISCV64_x27(); /* s11 */
   ru->regs[ru->size++]          = hregRISCV64_x10(); /* a0 */
   ru->regs[ru->size++]          = hregRISCV64_x11(); /* a1 */
   ru->regs[ru->size++]          = hregRISCV64_x12(); /* a2 */
   ru->regs[ru->size++]          = hregRISCV64_x13(); /* a3 */
   ru->regs[ru->size++]          = hregRISCV64_x14(); /* a4 */
   ru->regs[ru->size++]          = hregRISCV64_x15(); /* a5 */
   ru->regs[ru->size++]          = hregRISCV64_x16(); /* a6 */
   ru->regs[ru->size++]          = hregRISCV64_x17(); /* a7 */
   ru->allocable_end[HRcInt64]   = ru->size - 1;

   /* Floating-point registers, all of which are caller-saved. */
   ru->allocable_start[HRcFlt64] = ru->size;
   ru->regs[ru->size++]          = hregRISCV64_f0();  /* ft0 */
   ru->regs[ru->size++]          = hregRISCV64_f1();  /* ft1 */
   ru->regs[ru->size++]          = hregRISCV64_f2();  /* ft2 */
   ru->regs[ru->size++]          = hregRISCV64_f3();  /* ft3 */
   ru->regs[ru->size++]          = hregRISCV64_f4();  /* ft4 */
   ru->regs[ru->size++]          = hregRISCV64_f5();  /* ft5 */
   ru->regs[ru->size++]          = hregRISCV64_f6();  /* ft6 */
   ru->regs[ru->size++]          = hregRISCV64_f7();  /* ft7 */
   ru->regs[ru->size++]          = hregRISCV64_f10(); /* fa0 */
   ru->regs[ru->size++]          = hregRISCV64_f11(); /* fa1 */
   ru->regs[ru->size++]          = hregRISCV64_f12(); /* fa2 */
   ru->regs[ru->size++]          = hregRISCV64_f13(); /* fa3 */
   ru->regs[ru->size++]          = hregRISCV64_f14(); /* fa4 */
   ru->regs[ru->size++]          = hregRISCV64_f15(); /* fa5 */
   ru->regs[ru->size++]          = hregRISCV64_f16(); /* fa6 */
   ru->regs[ru->size++]          = hregRISCV64_f17(); /* fa7 */
   ru->regs[ru->size++]          = hregRISCV64_f28(); /* ft8 */
   ru->regs[ru->size++]          = hregRISCV64_f29(); /* ft9 */
   ru->regs[ru->size++]          = hregRISCV64_f30(); /* ft10 */
   ru->regs[ru->size++]          = hregRISCV64_f31(); /* ft11 */
   ru->allocable_end[HRcFlt64]   = ru->size - 1;
   ru->allocable                 = ru->size;

   /* Add the registers that are not available for allocation. */
   ru->regs[ru->size++] = hregRISCV64_x0(); /* zero */
   ru->regs[ru->size++] = hregRISCV64_x2(); /* sp */
   ru->regs[ru->size++] = hregRISCV64_x8(); /* s0 */

   initialised = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}

/* Tell the register allocator how the given instruction uses the registers it
   refers to. */
void getRegUsage_RISCV64Instr(HRegUsage* u, const RISCV64Instr* i, Bool mode64)
{
   vassert(mode64 == True);

   initHRegUsage(u);
   switch (i->tag) {
   case RISCV64in_LI:
      addHRegUse(u, HRmWrite, i->RISCV64in.LI.dst);
      return;
   case RISCV64in_MV:
      addHRegUse(u, HRmWrite, i->RISCV64in.MV.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.MV.src);
      return;
   case RISCV64in_ALU:
      addHRegUse(u, HRmWrite, i->RISCV64in.ALU.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.ALU.src1);
      addHRegUse(u, HRmRead, i->RISCV64in.ALU.src2);
      return;
   case RISCV64in_ALUImm:
      addHRegUse(u, HRmWrite, i->RISCV64in.ALUImm.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.ALUImm.src);
      return;
   case RISCV64in_Load:
      addHRegUse(u, HRmWrite, i->RISCV64in.Load.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.Load.base);
      return;
   case RISCV64in_Store:
      addHRegUse(u, HRmRead, i->RISCV64in.Store.src);
      addHRegUse(u, HRmRead, i->RISCV64in.Store.base);
      return;
   case RISCV64in_LoadR:
      addHRegUse(u, HRmWrite, i->RISCV64in.LoadR.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.LoadR.addr);
      return;
   case RISCV64in_StoreC:
      addHRegUse(u, HRmWrite, i->RISCV64in.StoreC.res);
      addHRegUse(u, HRmRead, i->RISCV64in.StoreC.src);
      addHRegUse(u, HRmRead, i->RISCV64in.StoreC.addr);
      return;
   case RISCV64in_CSRRW:
      addHRegUse(u, HRmWrite, i->RISCV64in.CSRRW.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.CSRRW.src);
      return;
   case RISCV64in_FpUnary:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpUnary.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpUnary.src);
      return;
   case RISCV64in_FpBinary:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpBinary.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpBinary.src1);
      addHRegUse(u, HRmRead, i->RISCV64in.FpBinary.src2);
      return;
   case RISCV64in_FpTernary:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpTernary.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpTernary.src1);
      addHRegUse(u, HRmRead, i->RISCV64in.FpTernary.src2);
      addHRegUse(u, HRmRead, i->RISCV64in.FpTernary.src3);
      return;
   case RISCV64in_FpMove:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpMove.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpMove.src);
      return;
   case RISCV64in_FpConvert:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpConvert.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpConvert.src);
      return;
   case RISCV64in_FpCompare:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpCompare.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpCompare.src1);
      addHRegUse(u, HRmRead, i->RISCV64in.FpCompare.src2);
      return;
   case RISCV64in_FpLdSt:
      switch (i->RISCV64in.FpLdSt.op) {
      case RISCV64op_FLW:
      case RISCV64op_FLD:
         addHRegUse(u, HRmWrite, i->RISCV64in.FpLdSt.reg);
         break;
      case RISCV64op_FSW:
      case RISCV64op_FSD:
         addHRegUse(u, HRmRead, i->RISCV64in.FpLdSt.reg);
         break;
      }
      addHRegUse(u, HRmRead, i->RISCV64in.FpLdSt.base);
      return;
   case RISCV64in_FpCSEL:
      addHRegUse(u, HRmWrite, i->RISCV64in.FpCSEL.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.FpCSEL.iftrue);
      addHRegUse(u, HRmRead, i->RISCV64in.FpCSEL.iffalse);
      addHRegUse(u, HRmRead, i->RISCV64in.FpCSEL.cond);
      return;
   case RISCV64in_CAS:
      addHRegUse(u, HRmWrite, i->RISCV64in.CAS.old);
      addHRegUse(u, HRmRead, i->RISCV64in.CAS.addr);
      addHRegUse(u, HRmRead, i->RISCV64in.CAS.expd);
      addHRegUse(u, HRmRead, i->RISCV64in.CAS.data);
      return;
   case RISCV64in_FENCE:
      return;
   case RISCV64in_CSEL:
      addHRegUse(u, HRmWrite, i->RISCV64in.CSEL.dst);
      addHRegUse(u, HRmRead, i->RISCV64in.CSEL.iftrue);
      addHRegUse(u, HRmRead, i->RISCV64in.CSEL.iffalse);
      addHRegUse(u, HRmRead, i->RISCV64in.CSEL.cond);
      return;
   case RISCV64in_Call:
      /* Logic and comments copied/modified from the arm64 backend. */
      /* First off, claim it trashes all the caller-saved registers which fall
         within the register allocator's jurisdiction. */
      addHRegUse(u, HRmWrite, hregRISCV64_x10());
      addHRegUse(u, HRmWrite, hregRISCV64_x11());
      addHRegUse(u, HRmWrite, hregRISCV64_x12());
      addHRegUse(u, HRmWrite, hregRISCV64_x13());
      addHRegUse(u, HRmWrite, hregRISCV64_x14());
      addHRegUse(u, HRmWrite, hregRISCV64_x15());
      addHRegUse(u, HRmWrite, hregRISCV64_x16());
      addHRegUse(u, HRmWrite, hregRISCV64_x17());
      addHRegUse(u, HRmWrite, hregRISCV64_f0());
      addHRegUse(u, HRmWrite, hregRISCV64_f1());
      addHRegUse(u, HRmWrite, hregRISCV64_f2());
      addHRegUse(u, HRmWrite, hregRISCV64_f3());
      addHRegUse(u, HRmWrite, hregRISCV64_f4());
      addHRegUse(u, HRmWrite, hregRISCV64_f5());
      addHRegUse(u, HRmWrite, hregRISCV64_f6());
      addHRegUse(u, HRmWrite, hregRISCV64_f7());
      addHRegUse(u, HRmWrite, hregRISCV64_f10());
      addHRegUse(u, HRmWrite, hregRISCV64_f11());
      addHRegUse(u, HRmWrite, hregRISCV64_f12());
      addHRegUse(u, HRmWrite, hregRISCV64_f13());
      addHRegUse(u, HRmWrite, hregRISCV64_f14());
      addHRegUse(u, HRmWrite, hregRISCV64_f15());
      addHRegUse(u, HRmWrite, hregRISCV64_f16());
      addHRegUse(u, HRmWrite, hregRISCV64_f17());
      addHRegUse(u, HRmWrite, hregRISCV64_f28());
      addHRegUse(u, HRmWrite, hregRISCV64_f29());
      addHRegUse(u, HRmWrite, hregRISCV64_f30());
      addHRegUse(u, HRmWrite, hregRISCV64_f31());
      /* Now we have to state any parameter-carrying registers which might be
         read. This depends on nArgRegs and nFArgRegs. */
      switch (i->RISCV64in.Call.nArgRegs) {
      case 8:
         addHRegUse(u, HRmRead, hregRISCV64_x17()); /*fallthru*/
      case 7:
         addHRegUse(u, HRmRead, hregRISCV64_x16()); /*fallthru*/
      case 6:
         addHRegUse(u, HRmRead, hregRISCV64_x15()); /*fallthru*/
      case 5:
         addHRegUse(u, HRmRead, hregRISCV64_x14()); /*fallthru*/
      case 4:
         addHRegUse(u, HRmRead, hregRISCV64_x13()); /*fallthru*/
      case 3:
         addHRegUse(u, HRmRead, hregRISCV64_x12()); /*fallthru*/
      case 2:
         addHRegUse(u, HRmRead, hregRISCV64_x11()); /*fallthru*/
      case 1:
         addHRegUse(u, HRmRead, hregRISCV64_x10());
         break;
      case 0:
         break;
      default:
         vpanic("getRegUsage_RISCV64Instr:Call:regparms");
      }
      switch (i->RISCV64in.Call.nFArgRegs) {
      case 8:
         addHRegUse(u, HRmRead, hregRISCV64_f17()); /*fallthru*/
      case 7:
         addHRegUse(u, HRmRead, hregRISCV64_f16()); /*fallthru*/
      case 6:
         addHRegUse(u, HRmRead, hregRISCV64_f15()); /*fallthru*/
      case 5:
         addHRegUse(u, HRmRead, hregRISCV64_f14()); /*fallthru*/
      case 4:
         addHRegUse(u, HRmRead, hregRISCV64_f13()); /*fallthru*/
      case 3:
         addHRegUse(u, HRmRead, hregRISCV64_f12()); /*fallthru*/
      case 2:
         addHRegUse(u, HRmRead, hregRISCV64_f11()); /*fallthru*/
      case 1:
         addHRegUse(u, HRmRead, hregRISCV64_f10());
         break;
      case 0:
         break;
      default:
         vpanic("getRegUsage_RISCV64Instr:Call:fregparms");
      }
      /* Finally, add the condition register. */
      if (!hregIsInvalid(i->RISCV64in.Call.cond))
         addHRegUse(u, HRmRead, i->RISCV64in.Call.cond);
      return;
   /* XDirect/XIndir/XAssisted are also a bit subtle. They conditionally exit
      the block. Hence we only need to list (1) the registers that they read,
      and (2) the registers that they write in the case where the block is not
      exited. (2) is empty, hence only (1) is relevant here. */
   case RISCV64in_XDirect:
      addHRegUse(u, HRmRead, i->RISCV64in.XDirect.base);
      if (!hregIsInvalid(i->RISCV64in.XDirect.cond))
         addHRegUse(u, HRmRead, i->RISCV64in.XDirect.cond);
      return;
   case RISCV64in_XIndir:
      addHRegUse(u, HRmRead, i->RISCV64in.XIndir.dstGA);
      addHRegUse(u, HRmRead, i->RISCV64in.XIndir.base);
      if (!hregIsInvalid(i->RISCV64in.XIndir.cond))
         addHRegUse(u, HRmRead, i->RISCV64in.XIndir.cond);
      return;
   case RISCV64in_XAssisted:
      addHRegUse(u, HRmRead, i->RISCV64in.XAssisted.dstGA);
      addHRegUse(u, HRmRead, i->RISCV64in.XAssisted.base);
      if (!hregIsInvalid(i->RISCV64in.XAssisted.cond))
         addHRegUse(u, HRmRead, i->RISCV64in.XAssisted.cond);
      return;
   case RISCV64in_EvCheck:
      /* We expect both amodes only to mention x8/s0, so this is in fact
         pointless, since the register isn't allocatable, but anyway.. */
      addHRegUse(u, HRmRead, i->RISCV64in.EvCheck.base_amCounter);
      addHRegUse(u, HRmRead, i->RISCV64in.EvCheck.base_amFailAddr);
      return;
   case RISCV64in_ProfInc:
      /* Does not use any registers known to RA. */
      return;
   default:
      ppRISCV64Instr(i, mode64);
      vpanic("getRegUsage_RISCV64Instr");
   }
}

/* Local helper. */
static void mapReg(HRegRemap* m, HReg* r) { *r = lookupHRegRemap(m, *r); }

/* Map the registers of the given instruction. */
void mapRegs_RISCV64Instr(HRegRemap* m, RISCV64Instr* i, Bool mode64)
{
   vassert(mode64 == True);

   switch (i->tag) {
   case RISCV64in_LI:
      mapReg(m, &i->RISCV64in.LI.dst);
      return;
   case RISCV64in_MV:
      mapReg(m, &i->RISCV64in.MV.dst);
      mapReg(m, &i->RISCV64in.MV.src);
      return;
   case RISCV64in_ALU:
      mapReg(m, &i->RISCV64in.ALU.dst);
      mapReg(m, &i->RISCV64in.ALU.src1);
      mapReg(m, &i->RISCV64in.ALU.src2);
      return;
   case RISCV64in_ALUImm:
      mapReg(m, &i->RISCV64in.ALUImm.dst);
      mapReg(m, &i->RISCV64in.ALUImm.src);
      return;
   case RISCV64in_Load:
      mapReg(m, &i->RISCV64in.Load.dst);
      mapReg(m, &i->RISCV64in.Load.base);
      return;
   case RISCV64in_Store:
      mapReg(m, &i->RISCV64in.Store.src);
      mapReg(m, &i->RISCV64in.Store.base);
      return;
   case RISCV64in_LoadR:
      mapReg(m, &i->RISCV64in.LoadR.dst);
      mapReg(m, &i->RISCV64in.LoadR.addr);
      return;
   case RISCV64in_StoreC:
      mapReg(m, &i->RISCV64in.StoreC.res);
      mapReg(m, &i->RISCV64in.StoreC.src);
      mapReg(m, &i->RISCV64in.StoreC.addr);
      return;
   case RISCV64in_CSRRW:
      mapReg(m, &i->RISCV64in.CSRRW.dst);
      mapReg(m, &i->RISCV64in.CSRRW.src);
      return;
   case RISCV64in_FpUnary:
      mapReg(m, &i->RISCV64in.FpUnary.dst);
      mapReg(m, &i->RISCV64in.FpUnary.src);
      return;
   case RISCV64in_FpBinary:
      mapReg(m, &i->RISCV64in.FpBinary.dst);
      mapReg(m, &i->RISCV64in.FpBinary.src1);
      mapReg(m, &i->RISCV64in.FpBinary.src2);
      return;
   case RISCV64in_FpTernary:
      mapReg(m, &i->RISCV64in.FpTernary.dst);
      mapReg(m, &i->RISCV64in.FpTernary.src1);
      mapReg(m, &i->RISCV64in.FpTernary.src2);
      mapReg(m, &i->RISCV64in.FpTernary.src3);
      return;
   case RISCV64in_FpMove:
      mapReg(m, &i->RISCV64in.FpMove.dst);
      mapReg(m, &i->RISCV64in.FpMove.src);
      return;
   case RISCV64in_FpConvert:
      mapReg(m, &i->RISCV64in.FpConvert.dst);
      mapReg(m, &i->RISCV64in.FpConvert.src);
      return;
   case RISCV64in_FpCompare:
      mapReg(m, &i->RISCV64in.FpCompare.dst);
      mapReg(m, &i->RISCV64in.FpCompare.src1);
      mapReg(m, &i->RISCV64in.FpCompare.src2);
      return;
   case RISCV64in_FpLdSt:
      mapReg(m, &i->RISCV64in.FpLdSt.reg);
      mapReg(m, &i->RISCV64in.FpLdSt.base);
      return;
   case RISCV64in_FpCSEL:
      mapReg(m, &i->RISCV64in.FpCSEL.dst);
      mapReg(m, &i->RISCV64in.FpCSEL.iftrue);
      mapReg(m, &i->RISCV64in.FpCSEL.iffalse);
      mapReg(m, &i->RISCV64in.FpCSEL.cond);
      return;
   case RISCV64in_CAS:
      mapReg(m, &i->RISCV64in.CAS.old);
      mapReg(m, &i->RISCV64in.CAS.addr);
      mapReg(m, &i->RISCV64in.CAS.expd);
      mapReg(m, &i->RISCV64in.CAS.data);
      return;
   case RISCV64in_FENCE:
      return;
   case RISCV64in_CSEL:
      mapReg(m, &i->RISCV64in.CSEL.dst);
      mapReg(m, &i->RISCV64in.CSEL.iftrue);
      mapReg(m, &i->RISCV64in.CSEL.iffalse);
      mapReg(m, &i->RISCV64in.CSEL.cond);
      return;
   case RISCV64in_Call:
      if (!hregIsInvalid(i->RISCV64in.Call.cond))
         mapReg(m, &i->RISCV64in.Call.cond);
      return;
   case RISCV64in_XDirect:
      mapReg(m, &i->RISCV64in.XDirect.base);
      if (!hregIsInvalid(i->RISCV64in.XDirect.cond))
         mapReg(m, &i->RISCV64in.XDirect.cond);
      return;
   case RISCV64in_XIndir:
      mapReg(m, &i->RISCV64in.XIndir.dstGA);
      mapReg(m, &i->RISCV64in.XIndir.base);
      if (!hregIsInvalid(i->RISCV64in.XIndir.cond))
         mapReg(m, &i->RISCV64in.XIndir.cond);
      return;
   case RISCV64in_XAssisted:
      mapReg(m, &i->RISCV64in.XAssisted.dstGA);
      mapReg(m, &i->RISCV64in.XAssisted.base);
      if (!hregIsInvalid(i->RISCV64in.XAssisted.cond))
         mapReg(m, &i->RISCV64in.XAssisted.cond);
      return;
   case RISCV64in_EvCheck:
      /* We expect both amodes only to mention x8/s0, so this is in fact
         pointless, since the register isn't allocatable, but anyway.. */
      mapReg(m, &i->RISCV64in.EvCheck.base_amCounter);
      mapReg(m, &i->RISCV64in.EvCheck.base_amFailAddr);
      return;
   case RISCV64in_ProfInc:
      /* Hardwires x5/t0 and x6/t1 -- nothing to modify. */
      return;
   default:
      ppRISCV64Instr(i, mode64);
      vpanic("mapRegs_RISCV64Instr");
   }
}

/* Generate riscv64 spill/reload instructions under the direction of the
   register allocator. Note it's critical these don't write the condition
   codes. */
void genSpill_RISCV64(/*OUT*/ HInstr** i1,
                      /*OUT*/ HInstr** i2,
                      HReg             rreg,
                      Int              offsetB,
                      Bool             mode64)
{
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == True);

   HReg base   = get_baseblock_register();
   Int  soff12 = offsetB - BASEBLOCK_OFFSET_ADJUSTMENT;
   vassert(soff12 >= -2048 && soff12 < 2048);

   HRegClass rclass = hregClass(rreg);
   switch (rclass) {
   case HRcInt64:
      *i1 = RISCV64Instr_Store(RISCV64op_SD, rreg, base, soff12);
      return;
   case HRcFlt64:
      *i1 = RISCV64Instr_FpLdSt(RISCV64op_FSD, rreg, base, soff12);
      return;
   default:
      ppHRegClass(rclass);
      vpanic("genSpill_RISCV64: unimplemented regclass");
   }
}

void genReload_RISCV64(/*OUT*/ HInstr** i1,
                       /*OUT*/ HInstr** i2,
                       HReg             rreg,
                       Int              offsetB,
                       Bool             mode64)
{
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == True);

   HReg base   = get_baseblock_register();
   Int  soff12 = offsetB - BASEBLOCK_OFFSET_ADJUSTMENT;
   vassert(soff12 >= -2048 && soff12 < 2048);

   HRegClass rclass = hregClass(rreg);
   switch (rclass) {
   case HRcInt64:
      *i1 = RISCV64Instr_Load(RISCV64op_LD, rreg, base, soff12);
      return;
   case HRcFlt64:
      *i1 = RISCV64Instr_FpLdSt(RISCV64op_FLD, rreg, base, soff12);
      return;
   default:
      ppHRegClass(rclass);
      vpanic("genReload_RISCV64: unimplemented regclass");
   }
}

RISCV64Instr* genMove_RISCV64(HReg from, HReg to, Bool mode64)
{
   vassert(mode64 == True);

   HRegClass rclass = hregClass(from);
   switch (rclass) {
   case HRcInt64:
      return RISCV64Instr_MV(to, from);
   case HRcFlt64:
      return RISCV64Instr_FpMove(RISCV64op_FMV_D, to, from);
   default:
      ppHRegClass(rclass);
      vpanic("genMove_RISCV64: unimplemented regclass");
   }
}

/*------------------------------------------------------------*/
/*--- Functions to emit a sequence of bytes                ---*/
/*------------------------------------------------------------*/

static inline UChar* emit16(UChar* p, UShort val)
{
   *p++ = (val >> 0) & 0xff;
   *p++ = (val >> 8) & 0xff;
   return p;
}

static inline UChar* emit32(UChar* p, UInt val)
{
   *p++ = (val >> 0) & 0xff;
   *p++ = (val >> 8) & 0xff;
   *p++ = (val >> 16) & 0xff;
   *p++ = (val >> 24) & 0xff;
   return p;
}

/*------------------------------------------------------------*/
/*--- Functions to emit various instruction formats        ---*/
/*------------------------------------------------------------*/

/* Emit an R-type instruction. */
static UChar* emit_R(
   UChar* p, UInt opcode, UInt rd, UInt funct3, UInt rs1, UInt rs2, UInt funct7)
{
   vassert(opcode >> 7 == 0);
   vassert(rd >> 5 == 0);
   vassert(funct3 >> 3 == 0);
   vassert(rs1 >> 5 == 0);
   vassert(rs2 >> 5 == 0);
   vassert(funct7 >> 7 == 0);

   UInt the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= rd << 7;
   the_insn |= funct3 << 12;
   the_insn |= rs1 << 15;
   the_insn |= rs2 << 20;
   the_insn |= funct7 << 25;

   return emit32(p, the_insn);
}

/* Emit an I-type instruction. */
static UChar*
emit_I(UChar* p, UInt opcode, UInt rd, UInt funct3, UInt rs1, UInt imm11_0)
{
   vassert(opcode >> 7 == 0);
   vassert(rd >> 5 == 0);
   vassert(funct3 >> 3 == 0);
   vassert(rs1 >> 5 == 0);
   vassert(imm11_0 >> 12 == 0);

   UInt the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= rd << 7;
   the_insn |= funct3 << 12;
   the_insn |= rs1 << 15;
   the_insn |= imm11_0 << 20;

   return emit32(p, the_insn);
}

/* Emit an S-type instruction. */
static UChar*
emit_S(UChar* p, UInt opcode, UInt imm11_0, UInt funct3, UInt rs1, UInt rs2)
{
   vassert(opcode >> 7 == 0);
   vassert(imm11_0 >> 12 == 0);
   vassert(funct3 >> 3 == 0);
   vassert(rs1 >> 5 == 0);
   vassert(rs2 >> 5 == 0);

   UInt imm4_0  = (imm11_0 >> 0) & 0x1f;
   UInt imm11_5 = (imm11_0 >> 5) & 0x7f;

   UInt the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= imm4_0 << 7;
   the_insn |= funct3 << 12;
   the_insn |= rs1 << 15;
   the_insn |= rs2 << 20;
   the_insn |= imm11_5 << 25;

   return emit32(p, the_insn);
}

/* Emit a B-type instruction. */
static UChar*
emit_B(UChar* p, UInt opcode, UInt imm12_1, UInt funct3, UInt rs1, UInt rs2)
{
   vassert(opcode >> 7 == 0);
   vassert(imm12_1 >> 12 == 0);
   vassert(funct3 >> 3 == 0);
   vassert(rs1 >> 5 == 0);
   vassert(rs2 >> 5 == 0);

   UInt imm11_11 = (imm12_1 >> 10) & 0x1;
   UInt imm4_1   = (imm12_1 >> 0) & 0xf;
   UInt imm10_5  = (imm12_1 >> 4) & 0x3f;
   UInt imm12_12 = (imm12_1 >> 11) & 0x1;

   UInt the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= imm11_11 << 7;
   the_insn |= imm4_1 << 8;
   the_insn |= funct3 << 12;
   the_insn |= rs1 << 15;
   the_insn |= rs2 << 20;
   the_insn |= imm10_5 << 25;
   the_insn |= imm12_12 << 31;

   return emit32(p, the_insn);
}

/* Emit a U-type instruction. */
static UChar* emit_U(UChar* p, UInt opcode, UInt rd, UInt imm31_12)
{
   vassert(opcode >> 7 == 0);
   vassert(rd >> 5 == 0);
   vassert(imm31_12 >> 20 == 0);

   UInt the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= rd << 7;
   the_insn |= imm31_12 << 12;

   return emit32(p, the_insn);
}

/* Emit a CR-type instruction. */
static UChar* emit_CR(UChar* p, UInt opcode, UInt rs2, UInt rd, UInt funct4)
{
   vassert(opcode >> 2 == 0);
   vassert(rs2 >> 5 == 0);
   vassert(rd >> 5 == 0);
   vassert(funct4 >> 4 == 0);

   UShort the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= rs2 << 2;
   the_insn |= rd << 7;
   the_insn |= funct4 << 12;

   return emit16(p, the_insn);
}

/* Emit a CI-type instruction. */
static UChar* emit_CI(UChar* p, UInt opcode, UInt imm5_0, UInt rd, UInt funct3)
{
   vassert(opcode >> 2 == 0);
   vassert(imm5_0 >> 6 == 0);
   vassert(rd >> 5 == 0);
   vassert(funct3 >> 3 == 0);

   UInt imm4_0 = (imm5_0 >> 0) & 0x1f;
   UInt imm5_5 = (imm5_0 >> 5) & 0x1;

   UShort the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= imm4_0 << 2;
   the_insn |= rd << 7;
   the_insn |= imm5_5 << 12;
   the_insn |= funct3 << 13;

   return emit16(p, the_insn);
}

/* Emit a CJ-type instruction. */
static UChar* emit_CJ(UChar* p, UInt opcode, UInt imm11_1, UInt funct3)
{
   vassert(opcode >> 2 == 0);
   vassert(imm11_1 >> 11 == 0);
   vassert(funct3 >> 3 == 0);

   UInt imm5_5   = (imm11_1 >> 4) & 0x1;
   UInt imm3_1   = (imm11_1 >> 0) & 0x7;
   UInt imm7_7   = (imm11_1 >> 6) & 0x1;
   UInt imm6_6   = (imm11_1 >> 5) & 0x1;
   UInt imm10_10 = (imm11_1 >> 9) & 0x1;
   UInt imm9_8   = (imm11_1 >> 7) & 0x3;
   UInt imm4_4   = (imm11_1 >> 3) & 0x1;
   UInt imm11_11 = (imm11_1 >> 10) & 0x1;

   UShort the_insn = 0;

   the_insn |= opcode << 0;
   the_insn |= imm5_5 << 2;
   the_insn |= imm3_1 << 3;
   the_insn |= imm7_7 << 6;
   the_insn |= imm6_6 << 7;
   the_insn |= imm10_10 << 8;
   the_insn |= imm9_8 << 9;
   the_insn |= imm4_4 << 11;
   the_insn |= imm11_11 << 12;
   the_insn |= funct3 << 13;

   return emit16(p, the_insn);
}

/*------------------------------------------------------------*/
/*--- Code generation                                      ---*/
/*------------------------------------------------------------*/

/* Get an immediate into a register, using only that register. */
static UChar* imm64_to_ireg(UChar* p, UInt dst, ULong imm64)
{
   vassert(dst > 0 && dst <= 31);

   Long simm64 = imm64;

   if (simm64 >= -32 && simm64 <= 31) {
      /* c.li dst, simm64[5:0] */
      return emit_CI(p, 0b01, imm64 & 0x3f, dst, 0b010);
   }

   /* TODO Add implementation with addi only and c.lui+addi. */

   if (simm64 >= -2147483648 && simm64 <= 2147483647) {
      /* lui dst, simm64[31:12]+simm64[11] */
      p = emit_U(p, 0b0110111, dst, ((imm64 + 0x800) >> 12) & 0xfffff);
      if ((imm64 & 0xfff) == 0)
         return p;
      /* addiw dst, dst, simm64[11:0] */
      return emit_I(p, 0b0011011, dst, 0b000, dst, imm64 & 0xfff);
   }

   /* Handle a constant that is out of the 32-bit signed integer range. */
   /* Strip the low 12 bits. */
   ULong imm11_0 = imm64 & 0xfff;

   /* Get the remaining adjusted upper bits. */
   ULong rem   = (simm64 + 0x800) >> 12;
   UInt  sham6 = 12 + __builtin_ctzll(rem);
   vassert(sham6 < 64);
   rem = vex_sx_to_64(rem >> (sham6 - 12), 64 - sham6);

   /* Generate instructions to load the upper bits. */
   p = imm64_to_ireg(p, dst, rem);
   /* c.slli dst, sham6 */
   p = emit_CI(p, 0b10, sham6, dst, 0b000);

   /* Add the low bits in. */
   if (imm11_0 == 0)
      return p;
   UInt imm5_0 = imm11_0 & 0x3f;
   if (vex_sx_to_64(imm5_0, 6) == vex_sx_to_64(imm11_0, 12)) {
      /* c.addi dst, imm5_0 */
      p = emit_CI(p, 0b01, imm5_0, dst, 0b000);
   } else {
      /* addi dst, dst, imm11_0 */
      p = emit_I(p, 0b0010011, dst, 0b000, dst, imm11_0);
   }

   return p;
}

/* Get a 48-bit address into a register, using only that register, and
   generating a constant number of instructions with 18 bytes in size,
   regardless of the value of the address. This is used when generating
   sections of code that need to be patched later, so as to guarantee a
   specific size.

   Notice that this function is designed to support target systems that use the
   Sv39 or Sv48 virtual-memory system. The input address is checked to be in
   the Sv48 format, that is bits [63:48] must be all equal to bit 47.
   Utilizing the fact that the address is only 48-bits in size allows to save 2
   instructions compared to materializing a full 64-bit address.
   */
static UChar* addr48_to_ireg_EXACTLY_18B(UChar* p, UInt dst, ULong imm48)
{
   vassert(imm48 >> 47 == 0 || imm48 >> 47 == 0x1ffff);

   ULong rem = imm48;
   ULong imm47_28, imm27_16, imm15_4, imm3_0;
   imm3_0   = rem & 0xf;
   rem      = (rem + 0x8) >> 4;
   imm15_4  = rem & 0xfff;
   rem      = (rem + 0x800) >> 12;
   imm27_16 = rem & 0xfff;
   rem      = (rem + 0x800) >> 12;
   imm47_28 = rem & 0xfffff;

   /* lui dst, imm47_28 */
   p = emit_U(p, 0b0110111, dst, imm47_28);
   /* addiw dst, dst, imm27_16 */
   p = emit_I(p, 0b0011011, dst, 0b000, dst, imm27_16);
   /* c.slli dst, 12 */
   p = emit_CI(p, 0b10, 12, dst, 0b000);
   /* addi dst, dst, imm15_4 */
   p = emit_I(p, 0b0010011, dst, 0b000, dst, imm15_4);
   /* c.slli dst, 4 */
   p = emit_CI(p, 0b10, 4, dst, 0b000);
   if (imm3_0 != 0) {
      /* c.addi dst, imm3_0 */
      p = emit_CI(p, 0b01, vex_sx_to_64(imm3_0, 4) & 0x3f, dst, 0b000);
   } else {
      /* c.nop */
      p = emit_CI(p, 0b01, 0, 0, 0b000);
   }

   return p;
}

/* Check whether p points at an instruction sequence cooked up by
   addr48_to_ireg_EXACTLY_18B(). */
static Bool is_addr48_to_ireg_EXACTLY_18B(UChar* p, UInt dst, ULong imm48)
{
   UChar  tmp[18];
   UChar* q;

   q = addr48_to_ireg_EXACTLY_18B(&tmp[0], dst, imm48);
   if (q - &tmp[0] != 18)
      return False;

   q = &tmp[0];
   for (UInt i = 0; i < 18; i++) {
      if (*p != *q)
         return False;
      p++;
      q++;
   }
   return True;
}

/* Emit an instruction into buf and return the number of bytes used. Note that
   buf is not the insn's final place, and therefore it is imperative to emit
   position-independent code. If the emitted instruction was a profiler inc, set
   *is_profInc to True, else leave it unchanged. */
Int emit_RISCV64Instr(/*MB_MOD*/ Bool*    is_profInc,
                      UChar*              buf,
                      Int                 nbuf,
                      const RISCV64Instr* i,
                      Bool                mode64,
                      const VexArchInfo*  archinfo_host,
                      const void*         disp_cp_chain_me_to_slowEP,
                      const void*         disp_cp_chain_me_to_fastEP,
                      const void*         disp_cp_xindir,
                      const void*         disp_cp_xassisted)
{
   vassert(nbuf >= 32);
   vassert(mode64 == True);
   vassert(((HWord)buf & 1) == 0);

   UChar* p = &buf[0];

   switch (i->tag) {
   case RISCV64in_LI:
      p = imm64_to_ireg(p, iregEnc(i->RISCV64in.LI.dst), i->RISCV64in.LI.imm64);
      goto done;
   case RISCV64in_MV: {
      /* c.mv dst, src */
      UInt dst = iregEnc(i->RISCV64in.MV.dst);
      UInt src = iregEnc(i->RISCV64in.MV.src);

      p = emit_CR(p, 0b10, src, dst, 0b1000);
      goto done;
   }
   case RISCV64in_ALU: {
      /* <op> dst, src1, src2 */
      UInt dst  = iregEnc(i->RISCV64in.ALU.dst);
      UInt src1 = iregEnc(i->RISCV64in.ALU.src1);
      UInt src2 = iregEnc(i->RISCV64in.ALU.src2);
      switch (i->RISCV64in.ALU.op) {
      case RISCV64op_ADD:
         p = emit_R(p, 0b0110011, dst, 0b000, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SUB:
         p = emit_R(p, 0b0110011, dst, 0b000, src1, src2, 0b0100000);
         goto done;
      case RISCV64op_ADDW:
         p = emit_R(p, 0b0111011, dst, 0b000, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SUBW:
         p = emit_R(p, 0b0111011, dst, 0b000, src1, src2, 0b0100000);
         goto done;
      case RISCV64op_XOR:
         p = emit_R(p, 0b0110011, dst, 0b100, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_OR:
         p = emit_R(p, 0b0110011, dst, 0b110, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_AND:
         p = emit_R(p, 0b0110011, dst, 0b111, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SLL:
         p = emit_R(p, 0b0110011, dst, 0b001, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SRL:
         p = emit_R(p, 0b0110011, dst, 0b101, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SRA:
         p = emit_R(p, 0b0110011, dst, 0b101, src1, src2, 0b0100000);
         goto done;
      case RISCV64op_SLLW:
         p = emit_R(p, 0b0111011, dst, 0b001, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SRLW:
         p = emit_R(p, 0b0111011, dst, 0b101, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SRAW:
         p = emit_R(p, 0b0111011, dst, 0b101, src1, src2, 0b0100000);
         goto done;
      case RISCV64op_SLT:
         p = emit_R(p, 0b0110011, dst, 0b010, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_SLTU:
         p = emit_R(p, 0b0110011, dst, 0b011, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_MUL:
         p = emit_R(p, 0b0110011, dst, 0b000, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_MULH:
         p = emit_R(p, 0b0110011, dst, 0b001, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_MULHU:
         p = emit_R(p, 0b0110011, dst, 0b011, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_DIV:
         p = emit_R(p, 0b0110011, dst, 0b100, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_DIVU:
         p = emit_R(p, 0b0110011, dst, 0b101, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_REM:
         p = emit_R(p, 0b0110011, dst, 0b110, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_REMU:
         p = emit_R(p, 0b0110011, dst, 0b111, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_MULW:
         p = emit_R(p, 0b0111011, dst, 0b000, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_DIVW:
         p = emit_R(p, 0b0111011, dst, 0b100, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_DIVUW:
         p = emit_R(p, 0b0111011, dst, 0b101, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_REMW:
         p = emit_R(p, 0b0111011, dst, 0b110, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_REMUW:
         p = emit_R(p, 0b0111011, dst, 0b111, src1, src2, 0b0000001);
         goto done;
      }
      break;
   }
   case RISCV64in_ALUImm: {
      /* <op> dst, src, imm12 */
      UInt dst   = iregEnc(i->RISCV64in.ALUImm.dst);
      UInt src   = iregEnc(i->RISCV64in.ALUImm.src);
      Int  imm12 = i->RISCV64in.ALUImm.imm12;
      switch (i->RISCV64in.ALUImm.op) {
      case RISCV64op_ADDI:
         vassert(imm12 >= -2048 && imm12 < 2048);
         p = emit_I(p, 0b0010011, dst, 0b000, src, imm12 & 0xfff);
         goto done;
      case RISCV64op_ADDIW:
         vassert(imm12 >= -2048 && imm12 < 2048);
         p = emit_I(p, 0b0011011, dst, 0b000, src, imm12 & 0xfff);
         goto done;
      case RISCV64op_XORI:
         vassert(imm12 >= -2048 && imm12 < 2048);
         p = emit_I(p, 0b0010011, dst, 0b100, src, imm12 & 0xfff);
         goto done;
      case RISCV64op_ANDI:
         vassert(imm12 >= -2048 && imm12 < 2048);
         p = emit_I(p, 0b0010011, dst, 0b111, src, imm12 & 0xfff);
         goto done;
      case RISCV64op_SLLI:
         vassert(imm12 >= 0 && imm12 < 64);
         p = emit_I(p, 0b0010011, dst, 0b001, src, (0b000000 << 6) | imm12);
         goto done;
      case RISCV64op_SRLI:
         vassert(imm12 >= 0 && imm12 < 64);
         p = emit_I(p, 0b0010011, dst, 0b101, src, (0b000000 << 6) | imm12);
         goto done;
      case RISCV64op_SRAI:
         vassert(imm12 >= 0 && imm12 < 64);
         p = emit_I(p, 0b0010011, dst, 0b101, src, (0b010000 << 6) | imm12);
         goto done;
      case RISCV64op_SLTIU:
         vassert(imm12 >= -2048 && imm12 < 2048);
         p = emit_I(p, 0b0010011, dst, 0b011, src, imm12 & 0xfff);
         goto done;
      }
      break;
   }
   case RISCV64in_Load: {
      /* l<size> dst, soff12(base) */
      UInt dst    = iregEnc(i->RISCV64in.Load.dst);
      UInt base   = iregEnc(i->RISCV64in.Load.base);
      Int  soff12 = i->RISCV64in.Load.soff12;
      vassert(soff12 >= -2048 && soff12 < 2048);
      UInt imm11_0 = soff12 & 0xfff;
      switch (i->RISCV64in.Load.op) {
      case RISCV64op_LD:
         p = emit_I(p, 0b0000011, dst, 0b011, base, imm11_0);
         goto done;
      case RISCV64op_LW:
         p = emit_I(p, 0b0000011, dst, 0b010, base, imm11_0);
         goto done;
      case RISCV64op_LH:
         p = emit_I(p, 0b0000011, dst, 0b001, base, imm11_0);
         goto done;
      case RISCV64op_LB:
         p = emit_I(p, 0b0000011, dst, 0b000, base, imm11_0);
         goto done;
      }
      break;
   }
   case RISCV64in_Store: {
      /* s<size> src, soff12(base) */
      UInt src    = iregEnc(i->RISCV64in.Store.src);
      UInt base   = iregEnc(i->RISCV64in.Store.base);
      Int  soff12 = i->RISCV64in.Store.soff12;
      vassert(soff12 >= -2048 && soff12 < 2048);
      UInt imm11_0 = soff12 & 0xfff;
      switch (i->RISCV64in.Store.op) {
      case RISCV64op_SD:
         p = emit_S(p, 0b0100011, imm11_0, 0b011, base, src);
         goto done;
      case RISCV64op_SW:
         p = emit_S(p, 0b0100011, imm11_0, 0b010, base, src);
         goto done;
      case RISCV64op_SH:
         p = emit_S(p, 0b0100011, imm11_0, 0b001, base, src);
         goto done;
      case RISCV64op_SB:
         p = emit_S(p, 0b0100011, imm11_0, 0b000, base, src);
         goto done;
      }
      goto done;
   }
   case RISCV64in_LoadR: {
      /* lr.<size> dst, (addr) */
      UInt dst  = iregEnc(i->RISCV64in.LoadR.dst);
      UInt addr = iregEnc(i->RISCV64in.LoadR.addr);
      switch (i->RISCV64in.LoadR.op) {
      case RISCV64op_LR_W:
         p = emit_R(p, 0b0101111, dst, 0b010, addr, 0b00000, 0b0001000);
         goto done;
      }
      break;
   }
   case RISCV64in_StoreC: {
      /* sc.<size> res, dst, (addr) */
      UInt res  = iregEnc(i->RISCV64in.StoreC.res);
      UInt src  = iregEnc(i->RISCV64in.StoreC.src);
      UInt addr = iregEnc(i->RISCV64in.StoreC.addr);
      switch (i->RISCV64in.StoreC.op) {
      case RISCV64op_SC_W:
         p = emit_R(p, 0b0101111, res, 0b010, addr, src, 0b0001100);
         goto done;
      }
      break;
   }
   case RISCV64in_CSRRW: {
      /* csrrw dst, csr, src */
      UInt dst = iregEnc(i->RISCV64in.CSRRW.dst);
      UInt src = iregEnc(i->RISCV64in.CSRRW.src);
      UInt csr = i->RISCV64in.CSRRW.csr;
      vassert(csr < 4096);

      p = emit_I(p, 0b1110011, dst, 0b001, src, csr);
      goto done;
   }
   case RISCV64in_FpUnary: {
      /* f<op> dst, src */
      UInt dst = fregEnc(i->RISCV64in.FpUnary.dst);
      UInt src = fregEnc(i->RISCV64in.FpUnary.src);
      switch (i->RISCV64in.FpUnary.op) {
      case RISCV64op_FSQRT_S:
         p = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b0101100);
         goto done;
      case RISCV64op_FSQRT_D:
         p = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b0101101);
         goto done;
      }
      break;
   }
   case RISCV64in_FpBinary: {
      /* f<op> dst, src1, src2 */
      UInt dst  = fregEnc(i->RISCV64in.FpBinary.dst);
      UInt src1 = fregEnc(i->RISCV64in.FpBinary.src1);
      UInt src2 = fregEnc(i->RISCV64in.FpBinary.src2);
      switch (i->RISCV64in.FpBinary.op) {
      case RISCV64op_FADD_S:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0000000);
         goto done;
      case RISCV64op_FMUL_S:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0001000);
         goto done;
      case RISCV64op_FDIV_S:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0001100);
         goto done;
      case RISCV64op_FSGNJN_S:
         p = emit_R(p, 0b1010011, dst, 0b001, src1, src2, 0b0010000);
         goto done;
      case RISCV64op_FSGNJX_S:
         p = emit_R(p, 0b1010011, dst, 0b010, src1, src2, 0b0010000);
         goto done;
      case RISCV64op_FMIN_S:
         p = emit_R(p, 0b1010011, dst, 0b000, src1, src2, 0b0010100);
         goto done;
      case RISCV64op_FMAX_S:
         p = emit_R(p, 0b1010011, dst, 0b001, src1, src2, 0b0010100);
         goto done;
      case RISCV64op_FADD_D:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0000001);
         goto done;
      case RISCV64op_FSUB_D:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0000101);
         goto done;
      case RISCV64op_FMUL_D:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0001001);
         goto done;
      case RISCV64op_FDIV_D:
         p = emit_R(p, 0b1010011, dst, 0b111, src1, src2, 0b0001101);
         goto done;
      case RISCV64op_FSGNJN_D:
         p = emit_R(p, 0b1010011, dst, 0b001, src1, src2, 0b0010001);
         goto done;
      case RISCV64op_FSGNJX_D:
         p = emit_R(p, 0b1010011, dst, 0b010, src1, src2, 0b0010001);
         goto done;
      case RISCV64op_FMIN_D:
         p = emit_R(p, 0b1010011, dst, 0b000, src1, src2, 0b0010101);
         goto done;
      case RISCV64op_FMAX_D:
         p = emit_R(p, 0b1010011, dst, 0b001, src1, src2, 0b0010101);
         goto done;
      }
      break;
   }
   case RISCV64in_FpTernary: {
      /* f<op> dst, src1, src2, src3 */
      UInt dst  = fregEnc(i->RISCV64in.FpTernary.dst);
      UInt src1 = fregEnc(i->RISCV64in.FpTernary.src1);
      UInt src2 = fregEnc(i->RISCV64in.FpTernary.src2);
      UInt src3 = fregEnc(i->RISCV64in.FpTernary.src3);
      switch (i->RISCV64in.FpTernary.op) {
      case RISCV64op_FMADD_S:
         p = emit_R(p, 0b1000011, dst, 0b111, src1, src2, src3 << 2 | 0b00);
         goto done;
      case RISCV64op_FMADD_D:
         p = emit_R(p, 0b1000011, dst, 0b111, src1, src2, src3 << 2 | 0b01);
         goto done;
      }
      break;
   }
   case RISCV64in_FpMove: {
      /* f<op> dst, src */
      UInt dst, src;
      switch (i->RISCV64in.FpMove.op) {
      case RISCV64op_FMV_X_W:
         dst = iregEnc(i->RISCV64in.FpMove.dst);
         src = fregEnc(i->RISCV64in.FpMove.src);
         p   = emit_R(p, 0b1010011, dst, 0b000, src, 0b00000, 0b1110000);
         goto done;
      case RISCV64op_FMV_W_X:
         dst = fregEnc(i->RISCV64in.FpMove.dst);
         src = iregEnc(i->RISCV64in.FpMove.src);
         p   = emit_R(p, 0b1010011, dst, 0b000, src, 0b00000, 0b1111000);
         goto done;
      case RISCV64op_FMV_D:
         dst = fregEnc(i->RISCV64in.FpMove.dst);
         src = fregEnc(i->RISCV64in.FpMove.src);
         p   = emit_R(p, 0b1010011, dst, 0b000, src, src, 0b0010001);
         goto done;
      case RISCV64op_FMV_X_D:
         dst = iregEnc(i->RISCV64in.FpMove.dst);
         src = fregEnc(i->RISCV64in.FpMove.src);
         p   = emit_R(p, 0b1010011, dst, 0b000, src, 0b00000, 0b1110001);
         goto done;
      case RISCV64op_FMV_D_X:
         dst = fregEnc(i->RISCV64in.FpMove.dst);
         src = iregEnc(i->RISCV64in.FpMove.src);
         p   = emit_R(p, 0b1010011, dst, 0b000, src, 0b00000, 0b1111001);
         goto done;
      }
      break;
   }
   case RISCV64in_FpConvert: {
      /* f<op> dst, src */
      UInt dst, src;
      switch (i->RISCV64in.FpConvert.op) {
      case RISCV64op_FCVT_W_S:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b1100000);
         goto done;
      case RISCV64op_FCVT_WU_S:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00001, 0b1100000);
         goto done;
      case RISCV64op_FCVT_S_W:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b1101000);
         goto done;
      case RISCV64op_FCVT_S_WU:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00001, 0b1101000);
         goto done;
      case RISCV64op_FCVT_L_S:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00010, 0b1100000);
         goto done;
      case RISCV64op_FCVT_LU_S:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00011, 0b1100000);
         goto done;
      case RISCV64op_FCVT_S_L:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00010, 0b1101000);
         goto done;
      case RISCV64op_FCVT_S_LU:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00011, 0b1101000);
         goto done;
      case RISCV64op_FCVT_S_D:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00001, 0b0100000);
         goto done;
      case RISCV64op_FCVT_D_S:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b0100001);
         goto done;
      case RISCV64op_FCVT_W_D:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b1100001);
         goto done;
      case RISCV64op_FCVT_WU_D:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00001, 0b1100001);
         goto done;
      case RISCV64op_FCVT_D_W:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00000, 0b1101001);
         goto done;
      case RISCV64op_FCVT_D_WU:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00001, 0b1101001);
         goto done;
      case RISCV64op_FCVT_L_D:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00010, 0b1100001);
         goto done;
      case RISCV64op_FCVT_LU_D:
         dst = iregEnc(i->RISCV64in.FpConvert.dst);
         src = fregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00011, 0b1100001);
         goto done;
      case RISCV64op_FCVT_D_L:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00010, 0b1101001);
         goto done;
      case RISCV64op_FCVT_D_LU:
         dst = fregEnc(i->RISCV64in.FpConvert.dst);
         src = iregEnc(i->RISCV64in.FpConvert.src);
         p   = emit_R(p, 0b1010011, dst, 0b111, src, 0b00011, 0b1101001);
         goto done;
      }
      break;
   }
   case RISCV64in_FpCompare: {
      /* f<op> dst, src1, src2 */
      UInt dst  = iregEnc(i->RISCV64in.FpCompare.dst);
      UInt src1 = fregEnc(i->RISCV64in.FpCompare.src1);
      UInt src2 = fregEnc(i->RISCV64in.FpCompare.src2);
      switch (i->RISCV64in.FpCompare.op) {
      case RISCV64op_FEQ_S:
         p = emit_R(p, 0b1010011, dst, 0b010, src1, src2, 0b1010000);
         goto done;
      case RISCV64op_FLT_S:
         p = emit_R(p, 0b1010011, dst, 0b001, src1, src2, 0b1010000);
         goto done;
      case RISCV64op_FEQ_D:
         p = emit_R(p, 0b1010011, dst, 0b010, src1, src2, 0b1010001);
         goto done;
      case RISCV64op_FLT_D:
         p = emit_R(p, 0b1010011, dst, 0b001, src1, src2, 0b1010001);
         goto done;
      }
      break;
   }
   case RISCV64in_FpLdSt: {
      /* f<op> reg, soff12(base) */
      UInt reg    = fregEnc(i->RISCV64in.FpLdSt.reg);
      UInt base   = iregEnc(i->RISCV64in.FpLdSt.base);
      Int  soff12 = i->RISCV64in.FpLdSt.soff12;
      vassert(soff12 >= -2048 && soff12 < 2048);
      UInt imm11_0 = soff12 & 0xfff;
      switch (i->RISCV64in.FpLdSt.op) {
      case RISCV64op_FLW:
         p = emit_I(p, 0b0000111, reg /*dst*/, 0b010, base, imm11_0);
         goto done;
      case RISCV64op_FLD:
         p = emit_I(p, 0b0000111, reg /*dst*/, 0b011, base, imm11_0);
         goto done;
      case RISCV64op_FSW:
         p = emit_S(p, 0b0100111, imm11_0, 0b010, base, reg /*src*/);
         goto done;
      case RISCV64op_FSD:
         p = emit_S(p, 0b0100111, imm11_0, 0b011, base, reg /*src*/);
         goto done;
      }
      break;
   }
   case RISCV64in_FpCSEL: {
      /*    beq cond, zero, 1f
            fmv.d dst, iftrue
            c.j 2f
         1: fmv.d dst, iffalse
         2:
       */
      UInt dst     = fregEnc(i->RISCV64in.FpCSEL.dst);
      UInt iftrue  = fregEnc(i->RISCV64in.FpCSEL.iftrue);
      UInt iffalse = fregEnc(i->RISCV64in.FpCSEL.iffalse);
      UInt cond    = iregEnc(i->RISCV64in.FpCSEL.cond);
      p = emit_B(p, 0b1100011, (10 >> 1) & 0xfff, 0b000, cond, 0 /*x0/zero*/);
      p = emit_R(p, 0b1010011, dst, 0b000, iftrue, iftrue, 0b0010001);
      p = emit_CJ(p, 0b01, (6 >> 1) & 0x7ff, 0b101);
      p = emit_R(p, 0b1010011, dst, 0b000, iffalse, iffalse, 0b0010001);
      goto done;
   }
   case RISCV64in_CAS: {
      /* 1: lr.<size> old, (addr)
            bne old, expd, 2f
            sc.<size> t0, data, (addr)
            bne t0, zero, 1b
         2:
       */
      UInt old  = iregEnc(i->RISCV64in.CAS.old);
      UInt addr = iregEnc(i->RISCV64in.CAS.addr);
      UInt expd = iregEnc(i->RISCV64in.CAS.expd);
      UInt data = iregEnc(i->RISCV64in.CAS.data);
      switch (i->RISCV64in.CAS.op) {
      case RISCV64op_CAS_D:
         p = emit_R(p, 0b0101111, old, 0b011, addr, 0b00000, 0b0001000);
         p = emit_B(p, 0b1100011, (12 >> 1) & 0xfff, 0b001, old, expd);
         p = emit_R(p, 0b0101111, 5 /*x5/t0*/, 0b011, addr, data, 0b0001100);
         p = emit_B(p, 0b1100011, (-12 >> 1) & 0xfff, 0b001, 5 /*x5/t0*/,
                    0 /*x0/zero*/);
         goto done;
      case RISCV64op_CAS_W:
         p = emit_R(p, 0b0101111, old, 0b010, addr, 0b00000, 0b0001000);
         p = emit_B(p, 0b1100011, (12 >> 1) & 0xfff, 0b001, old, expd);
         p = emit_R(p, 0b0101111, 5 /*x5/t0*/, 0b010, addr, data, 0b0001100);
         p = emit_B(p, 0b1100011, (-12 >> 1) & 0xfff, 0b001, 5 /*x5/t0*/,
                    0 /*x0/zero*/);
         goto done;
      }
      break;
   }
   case RISCV64in_FENCE: {
      /* fence */
      p = emit_I(p, 0b0001111, 0b00000, 0b000, 0b00000, 0b000011111111);
      goto done;
   }
   case RISCV64in_CSEL: {
      /*    beq cond, zero, 1f
            c.mv dst, iftrue
            c.j 2f
         1: c.mv dst, iffalse
         2:
       */
      UInt dst     = iregEnc(i->RISCV64in.CSEL.dst);
      UInt iftrue  = iregEnc(i->RISCV64in.CSEL.iftrue);
      UInt iffalse = iregEnc(i->RISCV64in.CSEL.iffalse);
      UInt cond    = iregEnc(i->RISCV64in.CSEL.cond);

      p = emit_B(p, 0b1100011, (8 >> 1) & 0xfff, 0b000, cond, 0 /*x0/zero*/);
      p = emit_CR(p, 0b10, iftrue, dst, 0b1000);
      p = emit_CJ(p, 0b01, (4 >> 1) & 0x7ff, 0b101);
      p = emit_CR(p, 0b10, iffalse, dst, 0b1000);
      goto done;
   }
   case RISCV64in_Call: {
      /*    beq cond, zero, 1f
            li t0, target
            c.jalr 0(t0)
         1:
       */
      UChar* ptmp = NULL;
      if (!hregIsInvalid(i->RISCV64in.Call.cond)) {
         ptmp = p;
         p += 4;
      }

      /* li t0, target */
      p = imm64_to_ireg(p, 5 /*x5/t0*/, i->RISCV64in.Call.target);

      /* c.jalr 0(t0) */
      p = emit_CR(p, 0b10, 0 /*x0/zero*/, 5 /*x5/t0*/, 0b1001);

      /* Fix up the conditional jump, if there was one. */
      if (!hregIsInvalid(i->RISCV64in.Call.cond)) {
         /* beq cond, zero, delta */
         UInt cond  = iregEnc(i->RISCV64in.Call.cond);
         UInt delta = p - ptmp;
         /* delta_min = 4 (beq) + 2 (c.li) + 2 (c.jalr) = 8 */
         vassert(delta >= 8 && delta < 4096 && (delta & 1) == 0);
         UInt imm12_1 = (delta >> 1) & 0xfff;

         emit_B(ptmp, 0b1100011, imm12_1, 0b000, cond, 0 /*x0/zero*/);
      }

      goto done;
   }

   case RISCV64in_XDirect: {
      /* NB: what goes on here has to be very closely coordinated with the
         chainXDirect_RISCV64() and unchainXDirect_RISCV64() below. */
      /* We're generating chain-me requests here, so we need to be sure this is
         actually allowed -- no-redir translations can't use chain-me's.
         Hence: */
      vassert(disp_cp_chain_me_to_slowEP != NULL);
      vassert(disp_cp_chain_me_to_fastEP != NULL);

      /* First off, if this is conditional, create a conditional jump over the
         rest of it. Or at least, leave a space for it that we will shortly fill
         in. */
      UChar* ptmp = NULL;
      if (!hregIsInvalid(i->RISCV64in.XDirect.cond)) {
         ptmp = p;
         p += 4;
      }

      /* Update the guest pc. */
      {
         /* li t0, dstGA */
         p = imm64_to_ireg(p, 5 /*x5/t0*/, i->RISCV64in.XDirect.dstGA);

         /* sd t0, soff12(base) */
         UInt base   = iregEnc(i->RISCV64in.XDirect.base);
         Int  soff12 = i->RISCV64in.XDirect.soff12;
         vassert(soff12 >= -2048 && soff12 < 2048);
         UInt imm11_0 = soff12 & 0xfff;

         p = emit_S(p, 0b0100011, imm11_0, 0b011, base, 5 /*x5/t0*/);
      }

      /* --- FIRST PATCHABLE BYTE follows --- */
      /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're calling to) backs
         up the return address, so as to find the address of the first patchable
         byte. So: don't change the number of instructions (3) below. */
      /* li t0, VG_(disp_cp_chain_me_to_{slowEP,fastEP}) */
      const void* disp_cp_chain_me = i->RISCV64in.XDirect.toFastEP
                                        ? disp_cp_chain_me_to_fastEP
                                        : disp_cp_chain_me_to_slowEP;

      p = addr48_to_ireg_EXACTLY_18B(p, 5 /*x5/t0*/, (ULong)(HWord)disp_cp_chain_me);

      /* c.jalr 0(t0) */
      p = emit_CR(p, 0b10, 0 /*x0/zero*/, 5 /*x5/t0*/, 0b1001);
      /* --- END of PATCHABLE BYTES --- */

      /* Fix up the conditional jump, if there was one. */
      if (!hregIsInvalid(i->RISCV64in.XDirect.cond)) {
         /* beq cond, zero, delta */
         UInt cond  = iregEnc(i->RISCV64in.XDirect.cond);
         UInt delta = p - ptmp;
         /* delta_min = 4 (beq) + 2 (c.li) + 4 (sd) + 18 (addr48) + 2 (c.jalr)
                      = 30 */
         vassert(delta >= 30 && delta < 4096 && (delta & 1) == 0);
         UInt imm12_1 = (delta >> 1) & 0xfff;

         emit_B(ptmp, 0b1100011, imm12_1, 0b000, cond, 0 /*x0/zero*/);
      }

      goto done;
   }

   case RISCV64in_XIndir: {
      /* We're generating transfers that could lead indirectly to a chain-me, so
         we need to be sure this is actually allowed -- no-redir translations
         are not allowed to reach normal translations without going through the
         scheduler. That means no XDirects or XIndirs out from no-redir
         translations. Hence: */
      vassert(disp_cp_xindir != NULL);

      /* First off, if this is conditional, create a conditional jump over the
         rest of it. Or at least, leave a space for it that we will shortly fill
         in. */
      UChar* ptmp = NULL;
      if (!hregIsInvalid(i->RISCV64in.XIndir.cond)) {
         ptmp = p;
         p += 4;
      }

      /* Update the guest pc. */
      {
         /* sd r-dstGA, soff12(base) */
         UInt src    = iregEnc(i->RISCV64in.XIndir.dstGA);
         UInt base   = iregEnc(i->RISCV64in.XIndir.base);
         Int  soff12 = i->RISCV64in.XIndir.soff12;
         vassert(soff12 >= -2048 && soff12 < 2048);
         UInt imm11_0 = soff12 & 0xfff;

         p = emit_S(p, 0b0100011, imm11_0, 0b011, base, src);
      }

      /* li t0, VG_(disp_cp_xindir) */
      p = imm64_to_ireg(p, 5 /*x5/t0*/, (ULong)(HWord)disp_cp_xindir);

      /* c.jr 0(t0) */
      p = emit_CR(p, 0b10, 0 /*x0/zero*/, 5 /*x5/t0*/, 0b1000);

      /* Fix up the conditional jump, if there was one. */
      if (!hregIsInvalid(i->RISCV64in.XIndir.cond)) {
         /* beq cond, zero, delta */
         UInt cond  = iregEnc(i->RISCV64in.XIndir.cond);
         UInt delta = p - ptmp;
         /* delta_min = 4 (beq) + 4 (sd) + 2 (c.li) + 2 (c.jr) = 12 */
         vassert(delta >= 12 && delta < 4096 && (delta & 1) == 0);
         UInt imm12_1 = (delta >> 1) & 0xfff;

         emit_B(ptmp, 0b1100011, imm12_1, 0b000, cond, 0 /*x0/zero*/);
      }

      goto done;
   }

   case RISCV64in_XAssisted: {
      /* First off, if this is conditional, create a conditional jump over the
         rest of it. Or at least, leave a space for it that we will shortly fill
         in. */
      UChar* ptmp = NULL;
      if (!hregIsInvalid(i->RISCV64in.XAssisted.cond)) {
         ptmp = p;
         p += 4;
      }

      /* Update the guest pc. */
      {
         /* sd r-dstGA, soff12(base) */
         UInt src    = iregEnc(i->RISCV64in.XAssisted.dstGA);
         UInt base   = iregEnc(i->RISCV64in.XAssisted.base);
         Int  soff12 = i->RISCV64in.XAssisted.soff12;
         vassert(soff12 >= -2048 && soff12 < 2048);
         UInt imm11_0 = soff12 & 0xfff;

         p = emit_S(p, 0b0100011, imm11_0, 0b011, base, src);
      }

      /* li s0, $magic_number */
      UInt trcval = 0;
      switch (i->RISCV64in.XAssisted.jk) {
      case Ijk_ClientReq:
         trcval = VEX_TRC_JMP_CLIENTREQ;
         break;
      case Ijk_Sys_syscall:
         trcval = VEX_TRC_JMP_SYS_SYSCALL;
         break;
      case Ijk_NoDecode:
         trcval = VEX_TRC_JMP_NODECODE;
         break;
      case Ijk_InvalICache:
         trcval = VEX_TRC_JMP_INVALICACHE;
         break;
      case Ijk_NoRedir:
         trcval = VEX_TRC_JMP_NOREDIR;
         break;
      case Ijk_SigTRAP:
         trcval = VEX_TRC_JMP_SIGTRAP;
         break;
      case Ijk_Boring:
         trcval = VEX_TRC_JMP_BORING;
         break;
      default:
         ppIRJumpKind(i->RISCV64in.XAssisted.jk);
         vpanic("emit_RISCV64Instr.RISCV64in_XAssisted: unexpected jump kind");
      }
      vassert(trcval != 0);
      p = imm64_to_ireg(p, 8 /*x8/s0*/, trcval);

      /* li t0, VG_(disp_cp_xassisted) */
      p = imm64_to_ireg(p, 5 /*x5/t0*/, (ULong)(HWord)disp_cp_xassisted);

      /* c.jr 0(t0) */
      p = emit_CR(p, 0b10, 0 /*x0/zero*/, 5 /*x5/t0*/, 0b1000);

      /* Fix up the conditional jump, if there was one. */
      if (!hregIsInvalid(i->RISCV64in.XAssisted.cond)) {
         /* beq cond, zero, delta */
         UInt cond  = iregEnc(i->RISCV64in.XAssisted.cond);
         UInt delta = p - ptmp;
         /* delta_min = 4 (beq) + 4 (sd) + 2 (c.li) + 2 (c.li) + 2 (c.jr)
                      = 14 */
         vassert(delta >= 14 && delta < 4096 && (delta & 1) == 0);
         UInt imm12_1 = (delta >> 1) & 0xfff;

         emit_B(ptmp, 0b1100011, imm12_1, 0b000, cond, 0 /*x0/zero*/);
      }

      goto done;
   }

   case RISCV64in_EvCheck: {
      /*    lw t0, soff12_amCounter(base_amCounter)
            c.addiw t0, -1
            sw t0, soff12_amCounter(base_amCounter)
            bge t0, zero, 1f
            ld t0, soff12_amFailAddr(base_amFailAddr)
            c.jr 0(t0)
         1:
      */
      UInt base_amCounter   = iregEnc(i->RISCV64in.EvCheck.base_amCounter);
      Int  soff12_amCounter = i->RISCV64in.EvCheck.soff12_amCounter;
      vassert(soff12_amCounter >= -2048 && soff12_amCounter < 2048);
      UInt imm11_0_amCounter = soff12_amCounter & 0xfff;

      UInt base_amFailAddr   = iregEnc(i->RISCV64in.EvCheck.base_amFailAddr);
      Int  soff12_amFailAddr = i->RISCV64in.EvCheck.soff12_amFailAddr;
      vassert(soff12_amFailAddr >= -2048 && soff12_amFailAddr < 2048);
      UInt imm11_0_amFailAddr = soff12_amFailAddr & 0xfff;

      p = emit_I(p, 0b0000011, 5 /*x5/t0*/, 0b010, base_amCounter,
                 imm11_0_amCounter);
      p = emit_CI(p, 0b01, -1 & 0x3f, 5 /*x5/t0*/, 0b001);
      p = emit_S(p, 0b0100011, imm11_0_amCounter, 0b010, base_amCounter,
                 5 /*x5/t0*/);
      p = emit_B(p, 0b1100011, (10 >> 1) & 0xfff, 0b101, 5 /*x5/t0*/,
                 0 /*x0/zero*/);
      p = emit_I(p, 0b0000011, 5 /*x5/t0*/, 0b011, base_amFailAddr,
                 imm11_0_amFailAddr);
      p = emit_CR(p, 0b10, 0 /*x0/zero*/, 5 /*x5/t0*/, 0b1000);

      /* Crosscheck. */
      vassert(evCheckSzB_RISCV64() == p - buf);
      goto done;
   }

   case RISCV64in_ProfInc: {
      /* Generate a code template to increment a memory location whose address
         will be known later as an immediate value. This code template will be
         patched by LibVEX_PatchProfInc() once the memory location is known. For
         now do this with address == 0x0000'6555'7555'8566.

         li t1, 0x655575558566
         ld t0, 0(t1)
         c.addi t0, t0, 1
         sd t0, 0(t1)
       */
      p = addr48_to_ireg_EXACTLY_18B(p, 6 /*x6/t1*/, 0x655575558566ULL);
      p = emit_I(p, 0b0000011, 5 /*x5/t0*/, 0b011, 6 /*x6/t1*/, 0);
      p = emit_CI(p, 0b01, 1, 5 /*x5/t0*/, 0b000);
      p = emit_S(p, 0b0100011, 0, 0b011, 6 /*x6/t1*/, 5 /*x5/t0*/);
      /* Tell the caller .. */
      vassert(!*is_profInc);
      *is_profInc = True;
      goto done;
   }

   default:
      goto bad;
   }

bad:
   ppRISCV64Instr(i, mode64);
   vpanic("emit_RISCV64Instr");
   /*NOTREACHED*/

done:
   vassert(p - &buf[0] <= 44);
   return p - &buf[0];
}

/* Return the number of bytes emitted for an RISCV64in_EvCheck, as produced by
   emit_RISCV64Instr(). */
Int evCheckSzB_RISCV64(void) { return 20; }

/* NB: what goes on here has to be very closely coordinated with the emitInstr
   case for XDirect, above. */
VexInvalRange chainXDirect_RISCV64(VexEndness  endness_host,
                                   void*       place_to_chain,
                                   const void* disp_cp_chain_me_EXPECTED,
                                   const void* place_to_jump_to)
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
        lui t0, disp_cp_chain_me_to_EXPECTED[47:28]'
        addiw t0, t0, disp_cp_chain_me_to_EXPECTED[27:16]'
        c.slli t0, 12
        addi t0, t0, disp_cp_chain_me_to_EXPECTED[15:4]'
        c.slli t0, 4
        c.addi t0, disp_cp_chain_me_to_EXPECTED[3:0]'
        c.jalr 0(t0)
      viz
        <18 bytes generated by addr48_to_ireg_EXACTLY_18B>
        82 92
   */
   UChar* p = place_to_chain;
   vassert(((HWord)p & 1) == 0);
   vassert(is_addr48_to_ireg_EXACTLY_18B(p, 5 /*x5/t0*/,
                                         (ULong)(HWord)disp_cp_chain_me_EXPECTED));
   vassert(p[18] == 0x82 && p[19] == 0x92);

   /* And what we want to change it to is:
        lui t0, place_to_jump[47:28]'
        addiw t0, t0, place_to_jump[27:16]'
        c.slli t0, 12
        addi t0, t0, place_to_jump[15:4]'
        c.slli t0, 4
        c.addi t0, place_to_jump[3:0]'
        c.jr 0(t0)
      viz
        <18 bytes generated by addr48_to_ireg_EXACTLY_18B>
        82 82

      The replacement has the same length as the original.
   */
   (void)addr48_to_ireg_EXACTLY_18B(p, 5 /*x5/t0*/, (ULong)(HWord)place_to_jump_to);
   p[18] = 0x82;
   p[19] = 0x82;

   VexInvalRange vir = {(HWord)p, 20};
   return vir;
}

/* NB: what goes on here has to be very closely coordinated with the emitInstr
   case for XDirect, above. */
VexInvalRange unchainXDirect_RISCV64(VexEndness  endness_host,
                                     void*       place_to_unchain,
                                     const void* place_to_jump_to_EXPECTED,
                                     const void* disp_cp_chain_me)
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
        lui t0, place_to_jump_to_EXPECTED[47:28]'
        addiw t0, t0, place_to_jump_to_EXPECTED[27:16]'
        c.slli t0, 12
        addi t0, t0, place_to_jump_to_EXPECTED[15:4]'
        c.slli t0, 4
        c.addi t0, place_to_jump_to_EXPECTED[3:0]'
        c.jr 0(t0)
      viz
        <18 bytes generated by addr48_to_ireg_EXACTLY_18B>
        82 82
   */
   UChar* p = place_to_unchain;
   vassert(((HWord)p & 1) == 0);
   vassert(is_addr48_to_ireg_EXACTLY_18B(p, 5 /*x5/t0*/,
                                         (ULong)(HWord)place_to_jump_to_EXPECTED));
   vassert(p[18] == 0x82 && p[19] == 0x82);

   /* And what we want to change it to is:
        lui t0, disp_cp_chain_me[47:28]'
        addiw t0, t0, disp_cp_chain_me[27:16]'
        c.slli t0, 12
        addi t0, t0, disp_cp_chain_me[15:4]'
        c.slli t0, 4
        c.addi t0, disp_cp_chain_me[3:0]'
        c.jalr 0(t0)
      viz
        <18 bytes generated by addr48_to_ireg_EXACTLY_18B>
        82 92

      The replacement has the same length as the original.
   */
   (void)addr48_to_ireg_EXACTLY_18B(p, 5 /*x5/t0*/, (ULong)(HWord)disp_cp_chain_me);
   p[18] = 0x82;
   p[19] = 0x89;

   VexInvalRange vir = {(HWord)p, 20};
   return vir;
}

/* Patch the counter address into a profile inc point, as previously created by
   the RISCV64in_ProfInc case for emit_RISCV64Instr(). */
VexInvalRange patchProfInc_RISCV64(VexEndness   endness_host,
                                   void*        place_to_patch,
                                   const ULong* location_of_counter)
{
   vassert(sizeof(ULong*) == 8);
   vassert(endness_host == VexEndnessLE);
   UChar* p = place_to_patch;
   vassert(((HWord)p & 3) == 0);
   vassert(is_addr48_to_ireg_EXACTLY_18B(p, 6 /*x6/t1*/, 0x655575558566ULL));
   vassert(p[18] == 0x83 && p[19] == 0x32 && p[20] == 0x03 && p[21] == 0x00);
   vassert(p[22] == 0x85 && p[23] == 0x02);
   vassert(p[24] == 0x23 && p[25] == 0x30 && p[26] == 0x53 && p[27] == 0x00);
   (void)addr48_to_ireg_EXACTLY_18B(p, 6 /*x6/t1*/, (ULong)(HWord)location_of_counter);
   VexInvalRange vir = {(HWord)p, 28};
   return vir;
}

/*--------------------------------------------------------------------*/
/*--- end                                      host_riscv64_defs.c ---*/
/*--------------------------------------------------------------------*/
