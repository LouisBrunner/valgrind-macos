/*---------------------------------------------------------------*/
/*--- begin                              host_NANOMIPS_defs.c ---*/
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

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_nanomips_defs.h"

/* Register number for guest state pointer in host code. */
#define GuestSP 23


NANOMIPSInstr *NANOMIPSInstr_Imm(NANOMIPSImmOp op, HReg dst, HReg src,
                                 UInt imm)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_Imm;
   i->NMin.Imm.op = op;
   i->NMin.Imm.dst = dst;
   i->NMin.Imm.src = src;
   i->NMin.Imm.imm = imm;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Alu(NANOMIPSAluOp op, HReg dst, HReg srcL,
                                 HReg srcR)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_Alu;
   i->NMin.Alu.op = op;
   i->NMin.Alu.dst = dst;
   i->NMin.Alu.srcL = srcL;
   i->NMin.Alu.srcR = srcR;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Unary(NANOMIPSUnaryOp op, HReg dst, HReg src)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_Unary;
   i->NMin.Unary.op = op;
   i->NMin.Unary.dst = dst;
   i->NMin.Unary.src = src;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Cmp(NANOMIPSCondCode cond, HReg dst, HReg srcL,
                                 HReg srcR)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag            = NMin_Cmp;
   i->NMin.Cmp.dst   = dst;
   i->NMin.Cmp.srcL  = srcL;
   i->NMin.Cmp.srcR  = srcR;
   i->NMin.Cmp.cond  = cond;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Call(Addr target,
                                  UInt argiregs, HReg guard, RetLoc rloc)
{
   UInt mask;
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_Call;
   i->NMin.Call.target = target;
   i->NMin.Call.argiregs = argiregs;
   i->NMin.Call.guard = guard;
   i->NMin.Call.rloc = rloc;
   /* Only $4 ... $11 inclusive may be used as arg regs.*/
   mask = (1 << 4) | (1 << 5) | (1 << 6) | (1 << 7) | (1 << 8) | (1 << 9)
          | (1 << 10) | (1 << 11);
   vassert(0 == (argiregs & ~mask));
   vassert(is_sane_RetLoc(rloc));
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_XDirect(Addr64 dstGA, HReg address, Int offset,
                                     HReg cond, Bool toFastEP)
{
   NANOMIPSInstr* i            = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag                      = NMin_XDirect;
   i->NMin.XDirect.dstGA       = dstGA;
   i->NMin.XDirect.addr        = address;
   i->NMin.XDirect.addr_offset = offset;
   i->NMin.XDirect.cond        = cond;
   i->NMin.XDirect.toFastEP    = toFastEP;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_XIndir(HReg dstGA, HReg address, Int offset,
                                    HReg cond)
{
   NANOMIPSInstr* i           = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag                     = NMin_XIndir;
   i->NMin.XIndir.dstGA       = dstGA;
   i->NMin.XIndir.addr        = address;
   i->NMin.XIndir.addr_offset = offset;
   i->NMin.XIndir.cond        = cond;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_XAssisted(HReg dstGA, HReg address, Int offset,
                                       HReg cond, IRJumpKind jk)
{
   NANOMIPSInstr* i              = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag                        = NMin_XAssisted;
   i->NMin.XAssisted.dstGA       = dstGA;
   i->NMin.XAssisted.addr        = address;
   i->NMin.XAssisted.addr_offset = offset;
   i->NMin.XAssisted.cond        = cond;
   i->NMin.XAssisted.jk          = jk;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Load(UChar sz, HReg dst,
                                  HReg addr, Int addr_offset)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_Load;
   i->NMin.Load.sz = sz;
   i->NMin.Load.addr = addr;
   i->NMin.Load.addr_offset = addr_offset;
   i->NMin.Load.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4);
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Store(UChar sz, HReg addr, Int addr_offset,
                                   HReg src)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_Store;
   i->NMin.Store.sz = sz;
   i->NMin.Store.src = src;
   i->NMin.Store.addr = addr;
   i->NMin.Store.addr_offset = addr_offset;
   vassert(sz == 1 || sz == 2 || sz == 4);
   vassert(addr_offset < 0x1000);
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_LoadL(UChar sz, HReg dst,
                                   HReg addr, Int addr_offset)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_LoadL;
   i->NMin.LoadL.sz  = sz;
   i->NMin.LoadL.addr = addr;
   i->NMin.LoadL.addr_offset = addr_offset;
   vassert(sz == 4);
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_Cas(UChar sz, HReg oldLo, HReg oldHi, HReg addr,
                                 HReg expdLo, HReg expdHi,
                                 HReg dataLo, HReg dataHi)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag           = NMin_Cas;
   i->NMin.Cas.sz   = sz;
   i->NMin.Cas.oldLo  = oldLo;
   i->NMin.Cas.addr = addr;
   i->NMin.Cas.expdLo = expdLo;
   i->NMin.Cas.dataLo = dataLo;

   vassert((sz == 4) || (sz == 8));

   if (sz == 8) {
      i->NMin.Cas.oldHi  = oldHi;
      i->NMin.Cas.expdHi = expdHi;
      i->NMin.Cas.dataHi = dataHi;
   }
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_StoreC(UChar sz, HReg addr, Int addr_offset,
                                    HReg src)
{
   NANOMIPSInstr *i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag = NMin_StoreC;
   i->NMin.StoreC.sz  = sz;
   i->NMin.StoreC.src = src;
   i->NMin.StoreC.addr = addr;
   i->NMin.StoreC.addr_offset = addr_offset;
   vassert(sz == 4);
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_MoveCond(NANOMIPSMoveCondOp op, HReg dst,
                                      HReg src, HReg cond)
{
   NANOMIPSInstr *i        = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag                  = NMin_MoveCond;
   i->NMin.MoveCond.op     = op;
   i->NMin.MoveCond.dst    = dst;
   i->NMin.MoveCond.src    = src;
   i->NMin.MoveCond.cond   = cond;
   return i;
}

NANOMIPSInstr *NANOMIPSInstr_EvCheck(HReg r_amCounter,
                                     Int offset_amCounter,
                                     HReg r_amFailAddr,
                                     Int offset_amFailAddr)
{
   NANOMIPSInstr* i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag                            = NMin_EvCheck;
   i->NMin.EvCheck.r_amCounter       = r_amCounter;
   i->NMin.EvCheck.offset_amCounter  = offset_amCounter;
   i->NMin.EvCheck.r_amFailAddr      = r_amFailAddr;
   i->NMin.EvCheck.offset_amFailAddr = offset_amFailAddr;
   return i;
}

NANOMIPSInstr* NANOMIPSInstr_ProfInc ( void )
{
   NANOMIPSInstr* i = LibVEX_Alloc_inline(sizeof(NANOMIPSInstr));
   i->tag       = NMin_ProfInc;
   return i;
}

UInt ppHRegNANOMIPS(HReg r)
{
   static const HChar* regnames[32] = {
      "zero", "at", "t4", "t5", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
      "t0", "t1", "t2", "t3", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
      "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"
   };
   UChar r_no;

   if (hregIsVirtual(r)) {
      return ppHReg(r);
   }

   vassert(hregClass(r) == HRcInt32);
   r_no = hregEncoding(r);
   vassert(r_no < 32);

   return vex_printf("%s", regnames[r_no]);
}

void ppNANOMIPSInstr(const NANOMIPSInstr* i)
{
   switch (i->tag) {
      case NMin_Imm:
         switch (i->NMin.Imm.op) {
            case NMimm_SLL:
               vex_printf("sll ");
               break;

            case NMimm_SRL:
               vex_printf("srl ");
               break;

            case NMimm_LI:
               vex_printf("LI ");
               break;

            case NMimm_SRA:
               vex_printf("sra ");
               break;

            case NMimm_SGN:
               vex_printf("SGN ");
               break;

            case NMimm_ORI:
               vex_printf("ori ");
               break;

            case NMimm_XORI:
               vex_printf("xori ");
               break;

            case NMimm_ANDI:
               vex_printf("andi ");
               break;
            case NMimm_ROTX:
               vex_printf("rotx ");
               break;

            default:
               vassert(0);
         }

         ppHRegNANOMIPS(i->NMin.Imm.dst);
         vex_printf(", ");

         if (i->NMin.Imm.op != NMimm_LI) {
            ppHRegNANOMIPS(i->NMin.Imm.src);
            vex_printf(", ");
         }

         if (i->NMin.Imm.op == NMimm_ROTX)
            vex_printf("%u, %u, %u", (i->NMin.Imm.imm >> 7) & 0xF,
                       (i->NMin.Imm.imm >> 6) & 1, i->NMin.Imm.imm & 0x1F);
         else
            vex_printf("0x%X (%d)", i->NMin.Imm.imm, (Int)i->NMin.Imm.imm);
         break;

      case NMin_Alu:

         switch (i->NMin.Alu.op) {
            case NMalu_SLL:
               vex_printf("sllv ");
               break;

            case NMalu_SRL:
               vex_printf("srlv ");
               break;

            case NMalu_SRA:
               vex_printf("srav ");
               break;

            case NMalu_OR:
               if (sameHReg(i->NMin.Alu.srcL, i->NMin.Alu.srcR))
                  vex_printf("move ");
               else
                  vex_printf("or ");

               break;

            case NMalu_XOR:
               vex_printf("xor ");
               break;

            case NMalu_AND:
               vex_printf("and ");
               break;

            case NMalu_ADD:
               vex_printf("add ");
               break;

            case NMalu_SUB:
               vex_printf("sub ");
               break;

            case NMalu_SLT:
               vex_printf("slt ");
               break;

            case NMalu_NOR:
               vex_printf("nor ");
               break;

            case NMalu_MUL:
               vex_printf("mul ");
               break;

            case NMalu_MULU:
               vex_printf("mulu ");
               break;

            case NMalu_MUH:
               vex_printf("muh ");
               break;

            case NMalu_MUHU:
               vex_printf("muhu ");
               break;

            case NMalu_DIV:
               vex_printf("div ");
               break;

            case NMalu_DIVU:
               vex_printf("divu ");
               break;

            case NMalu_MOD:
               vex_printf("mod ");
               break;

            case NMalu_MODU:
               vex_printf("modu ");
               break;

            default:
               vassert(0);
         }

         ppHRegNANOMIPS(i->NMin.Alu.dst);
         vex_printf(", ");
         ppHRegNANOMIPS(i->NMin.Alu.srcL);

         if ((i->NMin.Alu.op != NMalu_OR) ||
               !sameHReg(i->NMin.Alu.srcL, i->NMin.Alu.srcR)) {
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Alu.srcR);
         }

         break;

      case NMin_Unary:
         switch (i->NMin.Unary.op) {
            case NMun_CLO:
               vex_printf("clo");
               break;

            case NMun_CLZ:
               vex_printf("clz");
               break;

            case NMun_NOP:
               vex_printf("nop");
               break;

            default:
               vassert(0);
         }
         if (i->NMin.Unary.op != NMun_NOP)
         {
            ppHRegNANOMIPS(i->NMin.Unary.dst);
            vex_printf(",");
            ppHRegNANOMIPS(i->NMin.Unary.src);
         }
         break;
      case NMin_Cmp:

         switch (i->NMin.Cmp.cond) {
            case NMcc_EQ:
               vex_printf("EQ ");
               break;

            case NMcc_NE:
               vex_printf("NE ");
               break;

            case NMcc_LTS:
               vex_printf("LTS ");
               break;

            case NMcc_LTU:
               vex_printf("LTU ");
               break;

            case NMcc_LES:
               vex_printf("LES ");
               break;

            case NMcc_LEU:
               vex_printf("LEU ");
               break;

            case NMcc_AL:
               vex_printf("AL ");
               break;

            case NMcc_NV:
               vex_printf("NV ");
               break;

            default:
               vassert(0);
         }

         ppHRegNANOMIPS(i->NMin.Cmp.dst);
         vex_printf(", ");
         ppHRegNANOMIPS(i->NMin.Cmp.srcL);
         vex_printf(", ");
         ppHRegNANOMIPS(i->NMin.Cmp.srcR);

         break;

      case NMin_Call:
         vex_printf("CALL 0x%lX, #%X, ", i->NMin.Call.target,
                    i->NMin.Call.argiregs);
         ppHRegNANOMIPS(i->NMin.Call.guard);
         break;

      case NMin_XDirect:
         vex_printf("(xDirect) ");
         if (!hregIsInvalid(i->NMin.XDirect.cond)) {
            vex_printf("beqc ");
            ppHRegNANOMIPS(i->NMin.XDirect.cond);
            vex_printf(", zero, 12; ");
         }
         vex_printf("LI a5, 0x%08lX; ", i->NMin.XDirect.dstGA);
         vex_printf("sw a5, %d(", i->NMin.XDirect.addr_offset);
         ppHRegNANOMIPS(i->NMin.XDirect.addr);
         vex_printf("); LI a5, <%s>; ", i->NMin.XDirect.toFastEP ?
            "disp_cp_chain_me_to_fastEP" : "disp_cp_chain_me_to_slowEP");
         vex_printf("jalrc a5");
         break;

      case NMin_XIndir:
         vex_printf("(xIndir) ");
         if (!hregIsInvalid(i->NMin.XIndir.cond)) {
            vex_printf("beqc ");
            ppHRegNANOMIPS(i->NMin.XIndir.cond);
            vex_printf(", zero, 16; ");
         }
         vex_printf("sw ");
         ppHRegNANOMIPS(i->NMin.XIndir.dstGA);
         vex_printf(", %d(", i->NMin.XIndir.addr_offset);
         ppHRegNANOMIPS(i->NMin.XIndir.addr);
         vex_printf("); LI a5, <disp_cp_xindir>; ");
         vex_printf("jalrc a5");
         break;

      case NMin_XAssisted:
         vex_printf("(xAssisted) ");
         if (!hregIsInvalid(i->NMin.XAssisted.cond)) {
            vex_printf("beqc ");
            ppHRegNANOMIPS(i->NMin.XAssisted.cond);
            vex_printf(", zero, 24; ");
         }
         vex_printf("sw ");
         ppHRegNANOMIPS(i->NMin.XAssisted.dstGA);
         vex_printf(", %d(", i->NMin.XAssisted.addr_offset);
         ppHRegNANOMIPS(i->NMin.XAssisted.addr);
         vex_printf("); move a5, $IRJumpKind_to_TRCVAL(%d)",
                    (Int)i->NMin.XAssisted.jk);
         vex_printf("; LI a5, <disp_cp_xassisted>; ");
         vex_printf("jalrc a5");
         break;

      case NMin_EvCheck:
         vex_printf("(evCheck) ");
         vex_printf("lw a5, %d(", i->NMin.EvCheck.offset_amCounter);
         ppHRegNANOMIPS(i->NMin.EvCheck.r_amCounter);
         vex_printf("); addiu $9, $9, -1");
         vex_printf("; sw a5, %d(", i->NMin.EvCheck.offset_amCounter);
         ppHRegNANOMIPS(i->NMin.EvCheck.r_amCounter);
         vex_printf("); begc a5, zero, nofail;");
         vex_printf("lw a5, %d(", i->NMin.EvCheck.offset_amFailAddr);
         ppHRegNANOMIPS(i->NMin.EvCheck.r_amFailAddr);
         vex_printf("); jalrc a5; nofail:");
         break;

      case NMin_ProfInc:
         vex_printf("(profInc) li a5, ($NotKnownYet); "
                       "lw a4, 0(a5); "
                       "addiu $a4, a4, 1; "
                       "sw $a4, 0(a5); "
                       "sltiu at, a4, 1; "
                       "lw a4, 4(a5); "
                       "addu a4, a4, at; "
                       "sw a4, 4(a5); " );
         break;

      case NMin_Load:
         switch (i->NMin.Load.sz) {
            case 1:
               vex_printf("lb ");
               break;

            case 2:
               vex_printf("lh ");
               break;

            case 4:
               vex_printf("lw ");
               break;
         }

         ppHRegNANOMIPS(i->NMin.Load.dst);
         vex_printf(", (%d)", i->NMin.Load.addr_offset);
         ppHRegNANOMIPS(i->NMin.Load.addr);
         break;

      case NMin_Store:
         switch (i->NMin.Store.sz) {
            case 1:
               vex_printf("sb ");
               break;

            case 2:
               vex_printf("sh ");
               break;

            case 4:
               vex_printf("sw ");
               break;
         }

         ppHRegNANOMIPS(i->NMin.Store.src);
         vex_printf(", (%d)", i->NMin.Store.addr_offset);
         ppHRegNANOMIPS(i->NMin.Store.addr);
         break;

      case NMin_Cas:
         vex_printf("cas: \n");
         if (i->NMin.Cas.sz == 4) {
            vex_printf("ll ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", 0(");
            ppHRegNANOMIPS(i->NMin.Cas.addr);
            vex_printf("); ");

            vex_printf("bnec ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.expdLo);
            vex_printf(", end; ");

            vex_printf("addiu ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", 1; ");

            vex_printf("sc ");
            ppHRegNANOMIPS(i->NMin.Cas.dataLo);
            vex_printf(", 0(");
            ppHRegNANOMIPS(i->NMin.Cas.addr);
            vex_printf("); ");

            vex_printf("movn ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.expdLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.dataLo);
            vex_printf("; end:");
         } else {
            vex_printf("llwp ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.oldHi);
            vex_printf(", 0(");
            ppHRegNANOMIPS(i->NMin.Cas.addr);
            vex_printf("); ");

            vex_printf("bnec ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.expdLo);
            vex_printf(", end; ");

            vex_printf("bnec ");
            ppHRegNANOMIPS(i->NMin.Cas.oldHi);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.expdHi);
            vex_printf(", end; ");

            vex_printf("addiu ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", 1; ");

            vex_printf("addiu ");
            ppHRegNANOMIPS(i->NMin.Cas.oldHi);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.oldHi);
            vex_printf(", 1; ");

            vex_printf("scwp ");
            ppHRegNANOMIPS(i->NMin.Cas.dataLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.dataHi);
            vex_printf(", 0(");
            ppHRegNANOMIPS(i->NMin.Cas.addr);
            vex_printf("); ");

            vex_printf("movn ");
            ppHRegNANOMIPS(i->NMin.Cas.oldLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.expdLo);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.dataLo);

            vex_printf("movn ");
            ppHRegNANOMIPS(i->NMin.Cas.oldHi);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.expdHi);
            vex_printf(", ");
            ppHRegNANOMIPS(i->NMin.Cas.dataHi);
            vex_printf("; end:");
         }
         break;

      case NMin_LoadL:
         vex_printf("ll ");
         ppHRegNANOMIPS(i->NMin.LoadL.dst);
         vex_printf(", %d(", i->NMin.LoadL.addr_offset);
         ppHRegNANOMIPS(i->NMin.LoadL.addr);
         vex_printf("); ");
         break;

      case NMin_StoreC:
         vex_printf("sc ");
         ppHRegNANOMIPS(i->NMin.StoreC.src);
         vex_printf(", %d(", i->NMin.StoreC.addr_offset);
         ppHRegNANOMIPS(i->NMin.StoreC.addr);
         vex_printf("); ");
         break;

      case NMin_MoveCond:
         vassert(i->NMin.MoveCond.op == NMMoveCond_movn);
         vex_printf("movn ");
         ppHRegNANOMIPS(i->NMin.MoveCond.dst);
         vex_printf(", ");
         ppHRegNANOMIPS(i->NMin.MoveCond.src);
         vex_printf(", ");
         ppHRegNANOMIPS(i->NMin.MoveCond.cond);
         break;
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_NANOMIPSInstr(HRegUsage* u, const NANOMIPSInstr* i)
{
   initHRegUsage(u);

   switch (i->tag) {
      case NMin_Imm:
         addHRegUse(u, HRmWrite, i->NMin.Imm.dst);

         if (!hregIsInvalid(i->NMin.Imm.src))
            addHRegUse(u, HRmRead, i->NMin.Imm.src);

         return;

      case NMin_Alu:
         addHRegUse(u, HRmRead, i->NMin.Alu.srcL);
         addHRegUse(u, HRmRead, i->NMin.Alu.srcR);
         addHRegUse(u, HRmWrite, i->NMin.Alu.dst);

         /* or Rd,Rs,Rs == mr Rd,Rs */
         if ((i->NMin.Alu.op == NMalu_OR)
               && sameHReg(i->NMin.Alu.srcR, i->NMin.Alu.srcL)) {
            u->isRegRegMove = True;
            u->regMoveSrc   = i->NMin.Alu.srcL;
            u->regMoveDst   = i->NMin.Alu.dst;
         }

         return;

      case NMin_Cmp:
         addHRegUse(u, HRmRead, i->NMin.Cmp.srcL);
         addHRegUse(u, HRmRead, i->NMin.Cmp.srcR);
         addHRegUse(u, HRmWrite, i->NMin.Cmp.dst);
         return;

      case NMin_Unary:
         addHRegUse(u, HRmRead, i->NMin.Unary.src);
         addHRegUse(u, HRmWrite, i->NMin.Unary.dst);
         return;

      case NMin_Call: {
         UInt argir = i->NMin.Call.argiregs;

         if (!hregIsInvalid(i->NMin.Call.guard))
            addHRegUse(u, HRmRead, i->NMin.Call.guard);

         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR1());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR2());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR3());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR4());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR5());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR6());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR7());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR8());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR9());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR10());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR11());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR12());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR13());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR14());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR15());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR24());
         addHRegUse(u, HRmWrite, hregNANOMIPS_GPR25());

         if (argir & (1 << 11)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR11());

         if (argir & (1 << 10)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR10());

         if (argir & (1 << 9)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR9());

         if (argir & (1 << 8)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR8());

         if (argir & (1 << 7)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR7());

         if (argir & (1 << 6)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR6());

         if (argir & (1 << 5)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR5());

         if (argir & (1 << 4)) addHRegUse(u, HRmRead, hregNANOMIPS_GPR4());

         vassert(0 == (argir & ~((1 << 4) | (1 << 5) | (1 << 6)
                                 | (1 << 7) | (1 << 8) | (1 << 9) | (1 << 10)
                                 | (1 << 11))));
         return;
      }

      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case NMin_XDirect:
         addHRegUse(u, HRmRead, i->NMin.XDirect.addr);

         if (!hregIsInvalid(i->NMin.XDirect.cond))
            addHRegUse(u, HRmRead, i->NMin.XDirect.cond);

         return;

      case NMin_XIndir:
         addHRegUse(u, HRmRead, i->NMin.XIndir.dstGA);
         addHRegUse(u, HRmRead, i->NMin.XIndir.addr);

         if (!hregIsInvalid(i->NMin.XDirect.cond))
            addHRegUse(u, HRmRead, i->NMin.XDirect.cond);

         return;

      case NMin_XAssisted:
         addHRegUse(u, HRmRead, i->NMin.XAssisted.dstGA);
         addHRegUse(u, HRmRead, i->NMin.XAssisted.addr);

         if (!hregIsInvalid(i->NMin.XAssisted.cond))
            addHRegUse(u, HRmRead, i->NMin.XAssisted.cond);

         return;

      case NMin_Load:
         addHRegUse(u, HRmRead, i->NMin.Load.addr);
         addHRegUse(u, HRmWrite, i->NMin.Load.dst);
         return;

      case NMin_Store:
         addHRegUse(u, HRmRead, i->NMin.Store.addr);
         addHRegUse(u, HRmRead, i->NMin.Store.src);
         return;

      case NMin_LoadL:
         addHRegUse(u, HRmRead, i->NMin.LoadL.addr);
         addHRegUse(u, HRmWrite, i->NMin.LoadL.dst);
         return;

      case NMin_Cas:
         addHRegUse(u, HRmWrite, i->NMin.Cas.oldLo);
         addHRegUse(u, HRmRead, i->NMin.Cas.addr);
         addHRegUse(u, HRmRead, i->NMin.Cas.expdLo);
         addHRegUse(u, HRmModify, i->NMin.Cas.dataLo);
         if (i->NMin.Cas.sz == 8) {
            addHRegUse(u, HRmWrite, i->NMin.Cas.oldHi);
            addHRegUse(u, HRmRead, i->NMin.Cas.expdHi);
            addHRegUse(u, HRmModify, i->NMin.Cas.dataHi);
         }
         return;

      case NMin_StoreC:
         addHRegUse(u, HRmRead, i->NMin.StoreC.addr);
         addHRegUse(u, HRmWrite, i->NMin.StoreC.src);
         addHRegUse(u, HRmRead, i->NMin.StoreC.src);
         return;

      case NMin_MoveCond:
         addHRegUse(u, HRmWrite, i->NMin.MoveCond.dst);
         addHRegUse(u, HRmRead, i->NMin.MoveCond.src);
         addHRegUse(u, HRmRead, i->NMin.MoveCond.cond);
         return;

      case NMin_EvCheck:
         addHRegUse(u, HRmRead, i->NMin.EvCheck.r_amCounter);
         addHRegUse(u, HRmRead, i->NMin.EvCheck.r_amFailAddr);
         return;

      case NMin_ProfInc:
         /* does not use any registers. */
         return;

      default:
         ppNANOMIPSInstr(i);
         vpanic("getRegUsage_NANOMIPSInstr");
         break;
   }
}

/* local helper */
static void mapReg(HRegRemap * m, HReg * r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_NANOMIPSInstr(HRegRemap * m, NANOMIPSInstr * i)
{
   switch (i->tag) {
      case NMin_Imm:
         mapReg(m, &i->NMin.Imm.dst);

         if (!hregIsInvalid(i->NMin.Imm.src))
            mapReg(m, &i->NMin.Imm.src);

         break;

      case NMin_Alu:
         mapReg(m, &i->NMin.Alu.srcL);
         mapReg(m, &i->NMin.Alu.srcR);
         mapReg(m, &i->NMin.Alu.dst);
         return;

      case NMin_Cmp:
         mapReg(m, &i->NMin.Cmp.srcL);
         mapReg(m, &i->NMin.Cmp.srcR);
         mapReg(m, &i->NMin.Cmp.dst);
         return;

      case NMin_Unary:
         mapReg(m, &i->NMin.Unary.src);
         mapReg(m, &i->NMin.Unary.dst);
         return;

      case NMin_Call: {
         if (!hregIsInvalid(i->NMin.Call.guard))
            mapReg(m, &i->NMin.Call.guard);

         return;
      }

      case NMin_XDirect:
         mapReg(m, &i->NMin.XDirect.addr);

         if (!hregIsInvalid(i->NMin.XDirect.cond))
            mapReg(m, &i->NMin.XDirect.cond);

         return;

      case NMin_XIndir:
         mapReg(m, &i->NMin.XIndir.dstGA);
         mapReg(m, &i->NMin.XIndir.addr);

         if (!hregIsInvalid(i->NMin.XIndir.cond))
            mapReg(m, &i->NMin.XIndir.cond);

         return;

      case NMin_XAssisted:
         mapReg(m, &i->NMin.XAssisted.dstGA);
         mapReg(m, &i->NMin.XAssisted.addr);

         if (!hregIsInvalid(i->NMin.XAssisted.cond))
            mapReg(m, &i->NMin.XAssisted.cond);

         return;

      case NMin_Load:
         mapReg(m, &i->NMin.Load.addr);
         mapReg(m, &i->NMin.Load.dst);
         return;

      case NMin_Store:
         mapReg(m, &i->NMin.Store.addr);
         mapReg(m, &i->NMin.Store.src);
         return;

      case NMin_LoadL:
         mapReg(m, &i->NMin.LoadL.addr);
         mapReg(m, &i->NMin.LoadL.dst);
         return;

      case NMin_Cas:
         mapReg(m, &i->NMin.Cas.oldLo);
         mapReg(m, &i->NMin.Cas.addr);
         mapReg(m, &i->NMin.Cas.expdLo);
         mapReg(m, &i->NMin.Cas.dataLo);
         if (i->NMin.Cas.sz == 8) {
            mapReg(m, &i->NMin.Cas.oldHi);
            mapReg(m, &i->NMin.Cas.expdHi);
            mapReg(m, &i->NMin.Cas.dataHi);
         }
         return;

      case NMin_StoreC:
         mapReg(m, &i->NMin.StoreC.addr);
         mapReg(m, &i->NMin.StoreC.src);
         return;

      case NMin_MoveCond:
         mapReg(m, &i->NMin.MoveCond.dst);
         mapReg(m, &i->NMin.MoveCond.src);
         mapReg(m, &i->NMin.MoveCond.cond);
         return;

      case NMin_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         mapReg(m, &i->NMin.EvCheck.r_amCounter);
         mapReg(m, &i->NMin.EvCheck.r_amFailAddr);
         return;

      case NMin_ProfInc:
         /* does not use any registers. */
         return;

      default:
         ppNANOMIPSInstr(i);
         vpanic("mapRegs_NANOMIPSInstr");
         break;
   }
}

/* Generate NANOMIPS spill/reload instructions under the direction of the
   register allocator. */
void genSpill_NANOMIPS( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2, HReg rreg,
                                Int offsetB, Bool mode64)
{
   vassert(offsetB >= 0);
   vassert(offsetB < 0x1000);
   vassert(!mode64);
   vassert(!hregIsVirtual(rreg));
   vassert(hregClass(rreg) == HRcInt32);
   *i2 = NULL;
   *i1 = NANOMIPSInstr_Store(4, GuestStatePointer, offsetB, rreg);
}

void genReload_NANOMIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                                 Int offsetB, Bool mode64)
{
   vassert(offsetB >= 0);
   vassert(offsetB < 0x1000);
   vassert(!mode64);
   vassert(!hregIsVirtual(rreg));
   vassert(hregClass(rreg) == HRcInt32);
   *i2 = NULL;
   *i1 = NANOMIPSInstr_Load(4, rreg, GuestStatePointer, offsetB);
}

NANOMIPSInstr* genMove_NANOMIPS(HReg r_src, HReg r_dst)
{
   vassert(hregClass(r_dst) == hregClass(r_src));
   vassert(hregClass(r_src) == HRcInt32);
   return NANOMIPSInstr_Alu(NMalu_OR, r_dst, r_src, r_src);
}

/* --------- The NANOMIPS assembler --------- */

inline static UInt iregNo(HReg r)
{
   UInt n;
   vassert(hregClass(r) == (HRcInt32));
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 32);
   return n;
}

/* Emit 32bit instruction */
static UChar *emit32(UChar * p, UInt w32)
{
#if defined (_MIPSEB)
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 8) & 0x000000FF);
   *p++ = toUChar(w32 & 0x000000FF);
#else
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   *p++ = toUChar(w32 & 0x000000FF);
   *p++ = toUChar((w32 >> 8) & 0x000000FF);
#endif
   return p;
}

static UChar *mkFormNano2Regs12imm(UChar * p, UInt opc, UInt rt, UInt rs,
                                   UInt opc2, UInt imm)
{
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rs < 0x20);
   vassert(rt < 0x20);
   vassert(opc2 < 0x10);
   vassert(imm < 0x1000);
   theInstr = ((opc << 26) | (rt << 21) | (rs << 16) | (opc2 << 12) | (imm));
   return emit32(p, theInstr);
}

static UChar *mkFormNano2Regs16imm(UChar * p, UInt opc, UInt rt, UInt rs,
                                   UShort imm)
{
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rs < 0x20);
   vassert(rt < 0x20);
   theInstr = ((opc << 26) | (rt << 21) | (rs << 16) | (imm));
   return emit32(p, theInstr);
}

static UChar *mkFormNano1Reg(UChar * p, UInt opc, UInt rt, UInt opc2,
                             UInt imm)
{
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rt < 0x20);

   switch (opc) {
      case 0x38: /* LUI */
         theInstr = ((opc << 26) | (rt << 21) | (imm & 0x1FF000) |
                     ((imm & 0x7FE00000) >> 19) | ((imm & 0x80000000) >> 31));
         return emit32(p, theInstr);

      default:
         vassert(0);
   }
}

static UChar* mkFormNanoPShift(UChar * p, UInt rt, UInt rs, UInt opc2,
                               UInt imm)
{
   UInt theInstr;
   vassert(rt < 0x20);
   vassert(rs < 0x20);
   vassert(opc2 < 0x10);
   vassert(imm < 0x20);

   switch (opc2) {
      case PSLL:      /* SLL  */
      case SRL32:     /* SRL  */
      case SRA:       /* SRA  */
         theInstr = ((PU12 << 26) | (rt << 21) | (rs << 16) |
                     (PU12_PSHIFT << 12) | (opc2 << 5) | (imm));
         return emit32(p, theInstr);

      default:
         vassert(0);
   }
}

static UChar *mkFormNanoP32A0(UChar * p, UInt rt, UInt rs, UInt rd, UInt opc2)
{
   UInt theInstr;
   vassert(rt < 0x20);
   vassert(rs < 0x20);
   vassert(rd < 0x20);
   vassert(opc2 < 0x80);

   switch (opc2) {
      case _POOL32A0_ADDU32: /* ADDU */
      case _POOL32A0_AND32:  /* AND  */
      case _POOL32A0_SUBU32: /* SUBU */
      case _POOL32A0_SLLV:   /* SLLV */
      case _POOL32A0_SRLV:   /* SRLV */
      case _POOL32A0_SRAV:   /* SRAV */
      case _POOL32A0_XOR32:  /* XOR  */
      case _POOL32A0_SLT:    /* SLT  */
      case _POOL32A0_OR32:   /* OR   */
      case _POOL32A0_NOR:    /* NOR  */
      case _POOL32A0_PSLTU:  /* SLTU */
      case _POOL32A0_DIV:    /* DIV  */
      case _POOL32A0_DIVU:   /* DIVU */
      case _POOL32A0_MOD:    /* MOD  */
      case _POOL32A0_MODU:   /* MODU */
      case _POOL32A0_MUL32:  /* MUL  */
      case _POOL32A0_MULU:   /* MULU */
      case _POOL32A0_MUH:    /* MUH  */
      case _POOL32A0_MUHU:   /* MUHU */
         theInstr = ((P32A << 26) | (rt << 21) | (rs << 16) | (rd << 11) |
                     (opc2 << 3));
         return emit32(p, theInstr);

      case _POOL32A0_PCMOVE: /* MOVN */
         theInstr = ((P32A << 26) | (rt << 21) | (rs << 16) | (rd << 11) |
                     (1 << 10) | (opc2 << 3));
         return emit32(p, theInstr);

      default:
         vassert(0);
   }
}

static UChar *mkFormNanoPU12(UChar * p, UInt rt, UInt rs, UInt opc2, UInt imm)
{
   UInt theInstr;
   vassert(rt < 0x20);
   vassert(rs < 0x20);
   vassert(opc2 < 0x10);
   vassert(imm < 0x1000);

   switch (opc2) {
      case PU12_ANDI:      /* ANDI      */
      case PU12_ADDIU_NEG: /* ADDIU_NEG */
      case PU12_ORI:       /* ORI       */
      case PU12_SLTIU:     /* SLTIU     */
      case PU12_XORI:      /* XORI      */
      case PU12_PROTX:     /* ROTX      */
         theInstr = ((PU12 << 26) | (rt << 21) | (rs << 16) | (opc2 << 12) |
                     (imm));
         return emit32(p, theInstr);

      default:
         vassert(0);
   }
}

static UChar *mkFormNanoPBR1(UChar * p, UInt rt, UInt rs, UInt opc2, UInt imm)
{
   UInt theInstr;
   vassert(rt < 0x20);
   vassert(rs < 0x20);
   vassert(opc2 < 0x04);
   vassert(imm < 0x4000);

   theInstr = ((PBR1 << 26) | (rt << 21) | (rs << 16) | (opc2 << 14) |
               (imm & 0x3FFE) | (imm >> 14));
   return emit32(p, theInstr);
}

static UChar *mkFormNanoPBR2(UChar * p, UInt rt, UInt rs, UInt opc2, UInt imm)
{
   UInt theInstr;
   vassert(rt < 0x20);
   vassert(rs < 0x20);
   vassert(opc2 < 0x04);
   vassert(imm < 0x4000);

   theInstr = ((PBR2 << 26) | (rt << 21) | (rs << 16) | (opc2 << 14) |
               (imm & 0x3FFE) | (imm >> 14));
   return emit32(p, theInstr);
}

static UChar *mkFormNanoPLSS9(UChar * p, UInt rt, UInt rs, nanoPLSS9 opc,
                              UInt opc1, UInt opc2, UInt imm_ru)
{
   UInt theInstr;
   vassert(rt < 0x20);
   vassert(rs < 0x20);
   vassert(opc < 0x04);
   vassert(opc1 < 0x10);
   vassert(opc2 < 0x02);

   switch (opc2){
         case LL: /* LL/SC */
            vassert(imm_ru < 0x4000);
            theInstr = ((PLSS9 << 26) | (rt << 21) | (rs << 16) | (opc << 8) |
               (opc1 << 11) | opc2 | (imm_ru & 0xFC) | ((imm_ru & 0x100) << 7));
            break;
         case LLWP: /* LLWP/SCWP */
            vassert(imm_ru < 0x20);
            theInstr = ((PLSS9 << 26) | (rt << 21) | (rs << 16) | (opc << 8) |
               (opc1 << 11) | ( imm_ru << 3 ) | opc2);
            break;
         default:
            vassert(0);

   }
   return emit32(p, theInstr);
}

static UChar *doMemAccess_IR(UChar *p, UChar sz, UChar r_dst,
                             HReg addr, Int addr_offset, Bool isLoad)
{
   UInt rA, opc2;
   vassert(((UInt)addr_offset) < 0x1000);
   rA = iregNo(addr);
   opc2 = isLoad ? 0x00 : 0x01;

   switch (sz) {
      case 1:
         break;

      case 2:
         opc2 = opc2 | 0x04;
         break;

      case 4:
         opc2 = opc2 | 0x08;
         break;

      default:
         vassert(0);
   }

   p = mkFormNano2Regs12imm(p, 0x21, r_dst, rA, opc2, addr_offset);
   return p;
}

/* Load 32-bit immediate in exactely two 32-bit instructions even if it
   could generate fewer. This is needed for generating fixed sized patchable
   sequences. */
static inline UChar* mkLoadImm32_EXACTLY2(UChar* p, UInt r_dst, UInt imm)
{
   vassert(r_dst < 0x20);
   /* lui r_dst, (imm >> 20) */
   p = mkFormNano1Reg(p, 0x38, r_dst, 0, imm);
   /* ori r_dst, r_dst, (imm & 0xFFF) */
   p = mkFormNanoPU12(p, r_dst, r_dst, PU12_ORI, imm & 0xFFF);
   return p;
}

/* Load imm to r_dst */
static UChar *mkLoadImm(UChar * p, UInt r_dst, UInt imm)
{

   if (imm <= 0xFFFF) {
      /* addiu[32] r_dst, 0, imm */
      p = mkFormNano2Regs16imm(p, 0x00, r_dst, 0, imm & 0xFFFF);
   } else if (imm > 0xFFFFF000ULL) {
      /* addiu[neg] r_dst, 0, imm */
      p = mkFormNano2Regs12imm(p, 0x20, r_dst, 0, 0x08, (~imm + 1) & 0xFFF);
   } else {
      /* lui r_dst, (imm >> 20) */
      p = mkFormNano1Reg(p, 0x38, r_dst, 0, imm);
      imm &= 0xFFF;

      if (imm != 0) {
         /* ori r_dst, r_dst, (imm & 0xFFF) */
         p = mkFormNanoPU12(p, r_dst, r_dst, PU12_ORI, imm & 0xFFF);
      }
   }

   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */
Int emit_NANOMIPSInstr ( /*MB_MOD*/Bool* is_profInc,
                                   UChar* buf, Int nbuf,
                                   const NANOMIPSInstr* i,
                                   Bool mode64,
                                   VexEndness endness_host,
                                   const void* disp_cp_chain_me_to_slowEP,
                                   const void* disp_cp_chain_me_to_fastEP,
                                   const void* disp_cp_xindir,
                                   const void* disp_cp_xassisted )
{
   UChar *p = &buf[0];
   vassert(nbuf >= 32);
   vassert(!mode64);

   switch (i->tag) {
      case NMin_Imm: {
         UInt r_dst = iregNo(i->NMin.Imm.dst);
         UInt r_src = hregIsInvalid(i->NMin.Imm.src) ?
                      0 : iregNo(i->NMin.Imm.src);

         switch (i->NMin.Imm.op) {
            case NMimm_LI:
               p = mkLoadImm(p, r_dst, i->NMin.Imm.imm);
               break;

            case NMimm_SLL:
            case NMimm_SRL:
            case NMimm_SRA:
               p = mkFormNanoPShift(p, r_dst, r_src, i->NMin.Imm.op,
                                    i->NMin.Imm.imm);
               break;

            case NMimm_SGN:
               p = mkFormNanoPShift(p, r_dst, r_src, NMimm_SLL,
                                    32 - i->NMin.Imm.imm);
               p = mkFormNanoPShift(p, r_dst, r_dst, NMimm_SRA,
                                    32 - i->NMin.Imm.imm);
               break;

            case NMimm_ANDI:
            case NMimm_ORI:
            case NMimm_XORI:
               p = mkFormNanoPU12(p, r_dst, r_src, i->NMin.Imm.op - 0x6,
                                  i->NMin.Imm.imm);
               break;
            case NMimm_ROTX:
               p = mkFormNanoPU12(p, r_dst, r_src, PU12_PROTX, i->NMin.Imm.imm);
               break;

            default:
               goto bad;
         };

         goto done;
      }

      case NMin_Alu: {
         UInt r_dst = iregNo(i->NMin.Alu.dst);
         UInt r_srcL = iregNo(i->NMin.Alu.srcL);
         UInt r_srcR = iregNo(i->NMin.Alu.srcR);

         switch (i->NMin.Alu.op) {
            /* NMalu_ADD, NMalu_SUB, NMalu_AND, NMalu_OR, NMalu_NOR,
               NMalu_XOR, NMalu_SLT */
            case NMalu_ADD:
               /* addu[32] */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_ADDU32);
               break;

            case NMalu_SUB:
               /* subu[32] */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_SUBU32);
               break;

            case NMalu_AND:
               /* and */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_AND32);
               break;

            case NMalu_OR:
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_OR32);
               break;

            case NMalu_NOR:
               /* nor */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_NOR);
               break;

            case NMalu_XOR:
               /* xor */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_XOR32);
               break;

            case NMalu_SLT:
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_SLT);
               break;

            case NMalu_SLL:
               /* sllv */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_SLLV);
               break;

            case NMalu_SRL:
               /* srlv */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_SRLV);
               break;

            case NMalu_SRA:
               /* srav */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_SRAV);
               break;

            case NMalu_DIV:
               /* div */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_DIV);
               break;

            case NMalu_DIVU:
               /* divu */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_DIVU);
               break;

            case NMalu_MOD:
               /* mod */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_MOD);
               break;

            case NMalu_MODU:
               /* modu */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_MODU);
               break;

            case NMalu_MUL:
               /* mul */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_MUL32);
               break;

            case NMalu_MULU:
               /* mulu */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_MULU);
               break;

            case NMalu_MUH:
               /* muh */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_MUH);
               break;

            case NMalu_MUHU:
               /* muhu */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_MUHU);
               break;

            default:
               goto bad;
         }

         goto done;
      }

      case NMin_Unary: {
         UInt r_dst = iregNo(i->NMin.Unary.dst);
         UInt r_src = iregNo(i->NMin.Unary.src);

         switch (i->NMin.Unary.op) {
            /* NMun_CLO, NMun_CLZ, NMun_NOP */
            case NMun_CLO:  /* clo */
               p = mkFormNano2Regs16imm(p, 0x08, r_dst, r_src, 0x4B3F);
               break;

            case NMun_CLZ:  /* clz */
               p = mkFormNano2Regs16imm(p, 0x08, r_dst, r_src, 0x5B3F);
               break;

            case NMun_NOP:  /* nop (sll r0,r0,0) */
               p = mkFormNano2Regs16imm(p, 0x20, 0, 0, 0xC000);
               break;
         }

         goto done;
      }

      case NMin_Cmp: {
         UInt r_srcL = iregNo(i->NMin.Cmp.srcL);
         UInt r_srcR = iregNo(i->NMin.Cmp.srcR);
         UInt r_dst = iregNo(i->NMin.Cmp.dst);

         switch (i->NMin.Cmp.cond) {
            case NMcc_EQ:
               /* xor r_dst, r_srcL, r_srcR
                  sltiu r_dst, r_dst, 1 */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_XOR32);
               p = mkFormNanoPU12(p, r_dst, r_dst, PU12_SLTIU, 1);
               break;

            case NMcc_NE:
               /* xor r_dst, r_srcL, r_srcR
                  sltu r_dst, zero, r_dst */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_XOR32);
               p = mkFormNanoP32A0(p, r_dst, 0, r_dst, _POOL32A0_PSLTU);
               break;

            case NMcc_LTS:
               /* slt r_dst, r_srcL, r_srcR */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_SLT);
               break;

            case NMcc_LTU:
               /* sltu r_dst, r_srcL, r_srcR */
               p = mkFormNanoP32A0(p, r_srcR, r_srcL, r_dst, _POOL32A0_PSLTU);
               break;

            case NMcc_LES:
               /* slt r_dst, r_srcR, r_srcL
                  xori r_dst, r_dst, 1 */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_SLT);
               p = mkFormNanoPU12(p, r_dst, r_dst, PU12_XORI, 1);
               break;

            case NMcc_LEU:
               /* sltu r_dst, r_srcR, r_srcL
                  xori r_dst, r_dst, 1 */
               p = mkFormNanoP32A0(p, r_srcL, r_srcR, r_dst, _POOL32A0_PSLTU);
               p = mkFormNanoPU12(p, r_dst, r_dst, PU12_XORI, 1);
               break;

            default:
               goto bad;
         }

         goto done;
      }

      case NMin_Call: {
         /* If this is conditional, create a conditional
            jump over the rest of it. */
         if (!hregIsInvalid(i->NMin.Call.guard)) {
            switch (i->NMin.Call.rloc.pri) {
               case RLPri_2Int:
                  /* li $a0, 0x55555555 */
                  p = mkLoadImm(p, 4, 0x55555555);
                  /* move $a1, $a0 */
                  p = mkFormNanoP32A0(p, 0, 4, 5, _POOL32A0_OR32);
                  break;

               case RLPri_Int:
                  /* li $a1, 0x55555555 */
                  p = mkLoadImm(p, 4, 0x55555555);
                  break;

               case RLPri_None:
                  break;

               default:
                  vassert(0);
            }

            /* Skip 3 instructions
               beqc $[cond], $0, 12 */
            p = mkFormNanoPBR1(p, iregNo(i->NMin.Call.guard), 0,
                               PBR1_BEQC32, 12);
         }

         /* li $25, #target */
         p = mkLoadImm32_EXACTLY2(p, 25, i->NMin.Call.target);
         /* jalrc $25 */
         p = mkFormNano2Regs16imm(p, 0x12, 31, 25, 0);

         goto done;
      }

      case NMin_XDirect: {
         /* NB: what goes on here has to be very closely coordinated
            with the chainXDirect_NANOMIPS and
            unchainXDirect_NANOMIPS below. */
         /* We're generating chain-me requests here, so we need to be
            sure this is actually allowed -- no-redir translations
            can't use chain-me's.  Hence: */
         vassert(disp_cp_chain_me_to_slowEP != NULL);
         vassert(disp_cp_chain_me_to_fastEP != NULL);

         /* Use ptmp for backpatching conditional jumps. */

         /* If this is conditional, create a conditional
            jump over the rest of it. */
         if (!hregIsInvalid(i->NMin.XDirect.cond)) {
            /* Skip 6 instructions
               beqc $[cond], $0, 24 */
            p = mkFormNanoPBR1(p, iregNo(i->NMin.XDirect.cond), 0,
                               PBR1_BEQC32, 24);
         }

         /* Update the guest PC. */
         /* li r9, dstGA */
         /* sw r9, (offset)addr */
         p = mkLoadImm32_EXACTLY2(p, 9, i->NMin.XDirect.dstGA);
         p = doMemAccess_IR(p, 4, 9, i->NMin.XDirect.addr,
                            i->NMin.XDirect.addr_offset,
                            False /* Store */);
         /* --- FIRST PATCHABLE BYTE follows --- */
         /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
            calling to) backs up the return address, so as to find the
            address of the first patchable byte.  So: don't change the
            number of instructions (3) below. */
         /* move r9, VG_(disp_cp_chain_me_to_{slowEP,fastEP}) */
         /* jr  r9  */
         const void* disp_cp_chain_me
            = i->NMin.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP
              : disp_cp_chain_me_to_slowEP;
         p = mkLoadImm32_EXACTLY2(p, /*r*/ 9,
                                  (Addr)disp_cp_chain_me);

         /* jalrc r9 */
         p = mkFormNano2Regs16imm(p, 0x12, 31, 9, 0); /* p += 4 */
         /* --- END of PATCHABLE BYTES --- */

         goto done;
      }

      case NMin_XIndir: {
         /* We're generating transfers that could lead indirectly to a
            chain-me, so we need to be sure this is actually allowed --
            no-redir translations are not allowed to reach normal
            translations without going through the scheduler.  That means
            no XDirects or XIndirs out from no-redir translations.
            Hence: */
         vassert(disp_cp_xindir != NULL);

         /* If this is conditional, create a conditional
            jump over the rest of it. */
         if (!hregIsInvalid(i->NMin.XDirect.cond)) {
            /* Skip 4 instructions
               beqc $[cond], $0, 16 */
            p = mkFormNanoPBR1(p, iregNo(i->NMin.XIndir.cond), 0,
                               PBR1_BEQC32, 16);
         }

         /* sw r-dstGA, amPC */
         p = doMemAccess_IR(p, 4,  iregNo(i->NMin.XIndir.dstGA),
                            i->NMin.XIndir.addr,
                            i->NMin.XIndir.addr_offset,
                            False /* Store */);

         /* move r9, VG_(disp_cp_xindir) */
         p = mkLoadImm32_EXACTLY2(p, /*r*/ 9,
                                  (Addr)disp_cp_xindir);
         /* jalrc r9 */
         p = mkFormNano2Regs16imm(p, 0x12, 31, 9, 0); /* p += 4 */

         goto done;
      }

      case NMin_XAssisted: {
         /* First off, if this is conditional, create a conditional jump
            over the rest of it.  Or at least, leave a space for it that
            we will shortly fill in. */

         /* If this is conditional, create a conditional
            jump over the rest of it. */
         if (!hregIsInvalid(i->NMin.XAssisted.cond)) {
            /* Skip 4 instructions
               beqc $[cond], $0, 12 */
            p = mkFormNanoPBR1(p, iregNo(i->NMin.XAssisted.cond), 0,
                               PBR1_BEQC32, 24);
         }
         /* sw r-dstGA, amPC */
         p = doMemAccess_IR(p, 4,  iregNo(i->NMin.XAssisted.dstGA),
                            i->NMin.XAssisted.addr,
                            i->NMin.XAssisted.addr_offset,
                            False /* Store */);

         UInt trcval = 0;

         switch (i->NMin.XAssisted.jk) {
            case Ijk_ClientReq:
               trcval = VEX_TRC_JMP_CLIENTREQ;
               break;

            case Ijk_Sys_syscall:
               trcval = VEX_TRC_JMP_SYS_SYSCALL;
               break;

            /* case Ijk_Sys_int128:
                  trcval = VEX_TRC_JMP_SYS_INT128;
                  break;
            */

            case Ijk_Yield:
               trcval = VEX_TRC_JMP_YIELD;
               break;

            case Ijk_EmWarn:
               trcval = VEX_TRC_JMP_EMWARN;
               break;

            case Ijk_EmFail:
               trcval = VEX_TRC_JMP_EMFAIL;
               break;

            /* case Ijk_MapFail:
                  trcval = VEX_TRC_JMP_MAPFAIL;
                  break;
            */

            case Ijk_NoDecode:
               trcval = VEX_TRC_JMP_NODECODE;
               break;

            case Ijk_InvalICache:
               trcval = VEX_TRC_JMP_INVALICACHE;
               break;

            case Ijk_NoRedir:
               trcval = VEX_TRC_JMP_NOREDIR;
               break;

            case Ijk_SigILL:
               trcval = VEX_TRC_JMP_SIGILL;
               break;

            case Ijk_SigTRAP:
               trcval = VEX_TRC_JMP_SIGTRAP;
               break;

            /* case Ijk_SigSEGV:
                  trcval = VEX_TRC_JMP_SIGSEGV;
                  break;
            */

            case Ijk_SigBUS:
               trcval = VEX_TRC_JMP_SIGBUS;
               break;

            case Ijk_SigFPE_IntDiv:
               trcval = VEX_TRC_JMP_SIGFPE_INTDIV;
               break;

            case Ijk_SigFPE_IntOvf:
               trcval = VEX_TRC_JMP_SIGFPE_INTOVF;
               break;

            case Ijk_Boring:
               trcval = VEX_TRC_JMP_BORING;
               break;

            /* We don't expect to see the following being assisted.
               case Ijk_Ret:
               case Ijk_Call:
            fallthrough */
            default:
               ppIRJumpKind(i->NMin.XAssisted.jk);
               vpanic("emit_NANOMIPSInstr.NMin_XAssisted: unexpected jump"
                      "kind");
         }

         vassert(trcval != 0);
         p = mkLoadImm32_EXACTLY2(p, /*r*/ GuestSP, trcval);

         /* move r9, VG_(disp_cp_xassisted) */
         p = mkLoadImm32_EXACTLY2(p, /*r*/ 9,
                                  (ULong)(Addr)disp_cp_xassisted);
         /* jalrc r9 */
         p = mkFormNano2Regs16imm(p, 0x12, 31, 9, 0); /* p += 4 */

         goto done;
      }

      case NMin_Load:
         p = doMemAccess_IR(p, i->NMin.Load.sz, iregNo(i->NMin.Load.dst),
                            i->NMin.Load.addr,
                            i->NMin.Load.addr_offset,
                            True /* Load */);
         goto done;
         break;

      case NMin_Store:
         p = doMemAccess_IR(p, i->NMin.Store.sz, iregNo(i->NMin.Store.src),
                            i->NMin.Store.addr,
                            i->NMin.Store.addr_offset,
                            False /* Store */);
         goto done;
         break;

      case NMin_LoadL: {
         p = mkFormNanoPLSS9(p, iregNo(i->NMin.LoadL.dst),
                                iregNo(i->NMin.LoadL.addr),
                                PLSS1, PLL, LL, i->NMin.LoadL.addr_offset);
         goto done;
         break;
      }

      case NMin_StoreC: {
         p = mkFormNanoPLSS9(p, iregNo(i->NMin.StoreC.src),
                                iregNo(i->NMin.StoreC.addr),
                                PLSS1, PSC, PSC, i->NMin.StoreC.addr_offset);
         goto done;
         break;
      }

      case NMin_Cas: {
         vassert((i->NMin.Cas.sz == 4) || (i->NMin.Cas.sz == 8));
         UInt oldLo  = iregNo(i->NMin.Cas.oldLo);
         UInt addr = iregNo(i->NMin.Cas.addr);
         UInt expdLo = iregNo(i->NMin.Cas.expdLo);
         UInt dataLo = iregNo(i->NMin.Cas.dataLo);
         UInt oldHi = 0, expdHi = 0, dataHi = 0;
         if (i->NMin.Cas.sz == 8) {
            oldHi  = iregNo(i->NMin.Cas.oldHi);
            expdHi = iregNo(i->NMin.Cas.expdHi);
            dataHi = iregNo(i->NMin.Cas.dataHi);
         }

         if (i->NMin.Cas.sz == 4) {
         /*
          * ll       old,  0(addr)
          * bnec     old,  expd, end
          * addiu    old,  old,  1
          * sc       data, 0(addr)
          * movn     old,  expd, data
          * end:
          */
            p = mkFormNanoPLSS9(p, oldLo, addr, PLSS1, PLL, LL, 0);
            p = mkFormNanoPBR2(p, oldLo, expdLo, PBR2_BNEC32, 12);
            p = mkFormNano2Regs16imm(p, 0x00, oldLo, oldLo, 1);
            p = mkFormNanoPLSS9(p, dataLo, addr, PLSS1, PSC, SC, 0);
            p = mkFormNanoP32A0(p, dataLo, expdLo, oldLo, _POOL32A0_PCMOVE);
         } else {
         /*
          * llwp     oldLo, oldHi  0(addr)
          * bnec     oldLo, expdLo, end
          * bnec     oldHi, expdHi, end
          * addiu    oldLo,  oldLo,  1
          * addiu    oldHi,  oldHi,  1
          * scwp     dataLo, dataHi, 0(addr)
          * movn     oldLo, expdLo, dataLo
          * movn     oldHi, expdHi, dataHi
          * end:
          */
            p = mkFormNanoPLSS9(p, oldLo, addr, PLSS1, PLL, LLWP, oldHi);
            p = mkFormNanoPBR2(p, oldLo, expdLo, PBR2_BNEC32, 24);
            p = mkFormNanoPBR2(p, oldHi, expdHi, PBR2_BNEC32, 20);
            p = mkFormNano2Regs16imm(p, 0x00, oldLo, oldLo, 1);
            p = mkFormNano2Regs16imm(p, 0x00, oldHi, oldHi, 1);
            p = mkFormNanoPLSS9(p, dataLo, addr, PLSS1, PSC, SCWP, dataHi);
            p = mkFormNanoP32A0(p, dataLo, expdLo, oldLo, _POOL32A0_PCMOVE);
            p = mkFormNanoP32A0(p, dataHi, expdHi, oldHi, _POOL32A0_PCMOVE);
         }
         goto done;
      }

      case NMin_MoveCond: {
         UInt r_dst = iregNo(i->NMin.MoveCond.dst);
         UInt r_src = iregNo(i->NMin.MoveCond.src);
         UInt r_cond = iregNo(i->NMin.MoveCond.cond);

         switch (i->NMin.MoveCond.op) {
            case NMMoveCond_movn: {
               p = mkFormNanoP32A0(p, r_cond, r_src, r_dst, _POOL32A0_PCMOVE);
               break;
            }

            default:
               vassert(0);
         }

         goto done;
      }

      case NMin_EvCheck: {
         /* This requires a 32-bit dec/test in 32 mode. */
         /* We generate:
               lw      r9, amCounter
               addiu   r9, r9, -1
               sw      r9, amCounter
               bgec    r9, zero, nofail
               lw      r9, amFailAddr
               jalrc   r9
              nofail:
         */
         UChar* p0 = p;
         /* lw  r9, amCounter */
         p = doMemAccess_IR(p, 4, /*r*/ 9, i->NMin.EvCheck.r_amCounter,
                            i->NMin.EvCheck.offset_amCounter,
                            True /* Load */);
         /* addiu r9,r9,-1 */
         p = mkFormNanoPU12(p, 9, 9, PU12_ADDIU_NEG, 1);
         /* sw r9, amCounter */
         p = doMemAccess_IR(p, 4, /*r*/ 9, i->NMin.EvCheck.r_amCounter,
                            i->NMin.EvCheck.offset_amCounter,
                            False /* Store */);
         /* bgec r9, zero, nofail */
         p = emit32(p, 0x88098008);
         /* lw r9, amFailAddr */
         p = doMemAccess_IR(p, sizeof(Addr), /*r*/ 9,
                            i->NMin.EvCheck.r_amFailAddr,
                            i->NMin.EvCheck.offset_amFailAddr,
                            True /* Load */);
         /* jalrc[32] r9  */
         p = mkFormNano2Regs16imm(p, 0x12, 31, 9, 0);  /* p += 4 */
         /* nofail: */
         /* Crosscheck */
         vassert(evCheckSzB_NANOMIPS() == (UChar*)p - (UChar*)p0);
         goto done;
      }

      case NMin_ProfInc: {
         /* 32-bit:
               li r9, 0x65556555
               lw r8, 0(r9)
               addiu r8, r8, 1         # add least significant word
               sw r8, 0(r9)
               sltiu r1, r8, 1         # set carry-in bit
               lw r8, 4(r9)
               addu r8, r8, r1
               sw r8, 4(r9) */

         /* li r9, 0x65556555 */
         p = mkLoadImm32_EXACTLY2(p, 9, 0x65556555);

         /* lw r8, 0(r9) */
         p = mkFormNano2Regs12imm(p, 0x21, 8, 9, 0x8, 0);

         /* addiu r8, r8, 1 */
         p = mkFormNano2Regs16imm(p, 0x00, 8, 8, 0x01);

         /* sw r8, 0(r9) */
         p = mkFormNano2Regs12imm(p, 0x21, 8, 9, 0x9, 0);

         /* sltiu r1, r8, 1 */
         p = mkFormNanoPU12(p, 1, 8, PU12_SLTIU, 1);

         /* lw r8, 4(r9) */
         p = mkFormNano2Regs12imm(p, 0x21, 8, 9, 0x8, 4);

         /* addu r8, r8, r1 */
         p = mkFormNanoP32A0(p, 8, 1, 8, _POOL32A0_ADDU32);

         /* sw r8, 0(r9) */
         p = mkFormNano2Regs12imm(p, 0x21, 8, 9, 0x9, 4);

         break;
      }

      default:
         goto bad;
   }

bad:
   vex_printf("\n=> ");
   ppNANOMIPSInstr(i);
   vpanic("emit_NANOMIPSInstr");
/* NOTREACHED */ done:
   vassert(p - &buf[0] <= 128);
   return p - &buf[0];
}

/* How big is an event check?  See case for Min_EvCheck in
   emit_MIPSInstr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_NANOMIPS(void)
{
   return 6 * 4;
}

VexInvalRange chainXDirect_NANOMIPS(VexEndness endness_host,
                                    void* place_to_chain,
                                    const void* disp_cp_chain_me_EXPECTED,
                                    const void* place_to_jump_to)
{
   UInt tmp[3];
   UInt* p = (UInt*)place_to_chain;
   /* li r9, disp_cp_chain_me_EXPECTED */
   mkLoadImm32_EXACTLY2((UChar*)tmp, 9, (Addr)disp_cp_chain_me_EXPECTED);
   /* jalrc r9  */
   mkFormNano2Regs16imm((UChar*)(tmp + 2), 0x12, 31, 9, 0);
   vassert((tmp[0] == p[0]) && (tmp[1] == p[1]) && (tmp[2] == p[2]));
   /* li r9, place_to_jump_to */
   mkLoadImm32_EXACTLY2((UChar*)place_to_chain, 9, (Addr)place_to_jump_to);
   VexInvalRange vir = {(HWord)place_to_chain, 8};
   return vir;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_NANOMIPS ( VexEndness endness_host,
                                        void* place_to_unchain,
                                        const void* place_to_jump_to_EXPECTED,
                                        const void* disp_cp_chain_me)
{
   UInt tmp[3];
   UInt* p = (UInt*)place_to_unchain;
   /* li r9, disp_cp_chain_me_EXPECTED */
   mkLoadImm32_EXACTLY2((UChar*)tmp, 9, (Addr)place_to_jump_to_EXPECTED);
   /* jalrc r9  */
   mkFormNano2Regs16imm((UChar*)(tmp + 2), 0x12, 31, 9, 0);
   vassert((tmp[0] == p[0]) && (tmp[1] == p[1]) && (tmp[2] == p[2]));
   /* li r9, place_to_jump_to */
   mkLoadImm32_EXACTLY2((UChar*)place_to_unchain, 9, (Addr)disp_cp_chain_me);
   VexInvalRange vir = {(HWord)place_to_unchain, 8};
   return vir;
}

/* Patch the counter address into a profile inc point, as previously
   created by the Min_ProfInc case for emit_NANOMIPSInstr. */
VexInvalRange patchProfInc_NANOMIPS ( VexEndness endness_host,
                                      void*  place_to_patch,
                                      const ULong* location_of_counter)
{
   UInt tmp[9];
   UInt* p = (UInt*)place_to_patch;

   vassert(endness_host == VexEndnessLE || endness_host == VexEndnessBE);
   vassert(sizeof(ULong*) == 4);
   vassert(0 == (3 & (HWord)p));

   mkLoadImm32_EXACTLY2((UChar*)tmp, 9, 0x65556555);
   mkFormNano2Regs12imm((UChar*)tmp, 0x21, 8, 9, 0x8, 0);
   mkFormNano2Regs16imm((UChar*)tmp, 0x00, 8, 8, 0x01);
   mkFormNano2Regs12imm((UChar*)tmp, 0x21, 8, 9, 0x9, 0);
   mkFormNanoPU12((UChar*)tmp, 1, 8, PU12_SLTIU, 1);
   mkFormNano2Regs12imm((UChar*)tmp, 0x21, 8, 9, 0x8, 4);
   mkFormNanoP32A0((UChar*)tmp, 8, 1, 8, _POOL32A0_ADDU32);
   mkFormNano2Regs12imm((UChar*)tmp, 0x21, 8, 9, 0x9, 4);

   for(int i = 0; i < 9; i++)
      vassert(tmp[i] == p[i]);

   /* li r9, place_to_jump_to */
   mkLoadImm32_EXACTLY2((UChar*)place_to_patch, 9, (Addr)location_of_counter);
   VexInvalRange vir = {(HWord)place_to_patch, 8};
   return vir;
}

const RRegUniverse* getRRegUniverse_NANOMIPS ( Bool mode64 )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once.  rRegUniverse_MIPS_initted values: 0=not initted,
      1=initted for 32-bit-mode, 2=initted for 64-bit-mode */
   static RRegUniverse rRegUniverse_MIPS;
   static UInt         rRegUniverse_MIPS_initted = 0;
   UInt gpr;

   RRegUniverse* ru = &rRegUniverse_MIPS;

   if (LIKELY(rRegUniverse_MIPS_initted == 1))
      return ru;

   vassert(!mode64);

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->allocable_start[HRcInt32] = ru->size;

   for (gpr = 16; gpr <= 22; gpr++) {
      ru->regs[ru->size] = mkHReg(False, HRcInt32, gpr, ru->size);
      ru->size++;
   }

   for (gpr = 12; gpr <= 15; gpr++) {
      ru->regs[ru->size] = mkHReg(False, HRcInt32, gpr, ru->size);
      ru->size++;
   }

   ru->regs[ru->size] = mkHReg(False, HRcInt32, 24, ru->size);

   ru->allocable_end[HRcInt32] = ru->size;

   ru->allocable = ++ru->size;

   for (gpr = 0; gpr <= 11; gpr++) {
      ru->regs[ru->size] = mkHReg(False, HRcInt32, gpr, ru->size);
      ru->size++;
   }

   ru->regs[ru->size] = mkHReg(False, HRcInt32, 23, ru->size);
   ru->size++;
   ru->regs[ru->size] = mkHReg(False, HRcInt32, 25, ru->size);
   ru->size++;
   ru->regs[ru->size] = mkHReg(False, HRcInt32, 29, ru->size);
   ru->size++;
   ru->regs[ru->size] = mkHReg(False, HRcInt32, 31, ru->size);
   ru->size++;

   rRegUniverse_MIPS_initted = 1;

   RRegUniverse__check_is_sane(ru);
   return ru;
}

/*---------------------------------------------------------------*/
/*--- end                                host_NANOMIPS_defs.c ---*/
/*---------------------------------------------------------------*/
