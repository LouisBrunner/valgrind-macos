#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_mips32.h"
#include "libvex_guest_mips64.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_mips_defs.h"
#include "mips_defs.h"


/* Put value to DSPControl register. Expression e is written to DSPControl as
   is. If only certain bits of DSPControl need to be changed, it should be done
   before calling putDSPControl(). It could be done by reading DSPControl and
   ORing it with appropriate mask. */
static void putDSPControl(IRExpr * e)
{
   vassert(!mode64);
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_DSPControl), e));
}

/* Put value to accumulator(helper function for MIPS32 DSP ASE instructions). */
static void putAcc(UInt acNo, IRExpr * e)
{
   vassert(!mode64);
   vassert(acNo <= 3);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   stmt(IRStmt_Put(accumulatorGuestRegOffset(acNo), e));

   /* If acNo = 0, split value to HI and LO regs in order to maintain compatibility
      between MIPS32 and MIPS DSP ASE insn sets. */
   if (0 == acNo) {
      putLO(unop(Iop_64to32, e));
      putHI(unop(Iop_64HIto32, e));
   }
}
/*------------------------------------------------------------*/
/*---       Disassemble a single DSP ASE instruction       ---*/
/*------------------------------------------------------------*/

static UInt disDSPInstr_MIPS_WRK_Special ( UInt cins )
{
   IRTemp t1;
   UInt rs, rt, rd, function, ac, ac_mfhilo;
   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   function = get_function(cins);
   ac = get_acNo(cins);
   ac_mfhilo = get_acNo_mfhilo(cins);

   switch (function) {
      case 0x10: {  /* MFHI */
         DIP("mfhi ac%u r%u", ac_mfhilo, rd);
         putIReg(rd, unop(Iop_64HIto32, getAcc(ac_mfhilo)));
         break;
      }

      case 0x11: {  /* MTHI */
         DIP("mthi ac%u r%u", ac, rs);
         t1 = newTemp(Ity_I32);
         assign(t1, unop(Iop_64to32, getAcc(ac)));
         putAcc(ac, binop(Iop_32HLto64, getIReg(rs), mkexpr(t1)));
         break;
      }

      case 0x12: {  /* MFLO */
         DIP("mflo ac%u r%u", ac_mfhilo, rd);
         putIReg(rd, unop(Iop_64to32, getAcc(ac_mfhilo)));
         break;
      }

      case 0x13: {  /* MTLO */
         DIP("mtlo ac%u r%u", ac, rs);
         t1 = newTemp(Ity_I32);
         assign(t1, unop(Iop_64HIto32, getAcc(ac)));
         putAcc(ac, binop(Iop_32HLto64, mkexpr(t1), getIReg(rs)));
         break;
      }

      case 0x18: {  /* MULT */
         DIP("mult ac%u r%u, r%u", ac, rs, rt);
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_MullS32, mkNarrowTo32(Ity_I32, getIReg(rs)),
                          mkNarrowTo32(Ity_I32, getIReg(rt))));
         putAcc(ac, mkexpr(t1));
         break;
      }

      case 0x19: {  /* MULTU */
         DIP("multu ac%u r%u, r%u", ac, rs, rt);
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_MullU32, mkNarrowTo32(Ity_I32, getIReg(rs)),
                          mkNarrowTo32(Ity_I32,
                                       getIReg(rt))));
         putAcc(ac, mkexpr(t1));
         break;
      }
   }

   return 0;
}


static UInt disDSPInstr_MIPS_WRK_Special2 ( UInt cins )
{
   IRTemp t1 = 0, t2, t3;
   UInt rs, rt, function, ac;
   rs = get_rs(cins);
   rt = get_rt(cins);
   function = get_function(cins);
   ac = get_acNo(cins);

   switch (function) {
      case 0x00: {  /* MADD */
         DIP("madd ac%u, r%u, r%u", ac, rs, rt);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t1, getAcc(ac));
         assign(t2, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
         assign(t3, binop(Iop_Add64, mkexpr(t1), mkexpr(t2)));

         putAcc(ac, mkexpr(t3));
         break;
      }

      case 0x01: {  /* MADDU */
         DIP("maddu ac%u r%u, r%u", ac, rs, rt);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t1, getAcc(ac));
         assign(t2, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
         assign(t3, binop(Iop_Add64, mkexpr(t2), mkexpr(t1)));

         putAcc(ac, mkexpr(t3));
         break;
      }

      case 0x04: {  /* MSUB */
         DIP("msub ac%u r%u, r%u", ac, rs, rt);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t1, getAcc(ac));
         assign(t2, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
         assign(t3, binop(Iop_Sub64, mkexpr(t1), mkexpr(t2)));

         putAcc(ac, mkexpr(t3));
         break;
      }

      case 0x05: {  /* MSUBU */
         DIP("msubu ac%u r%u, r%u", ac, rs, rt);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t1, getAcc(ac));
         assign(t2, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
         assign(t3, binop(Iop_Sub64, mkexpr(t1), mkexpr(t2)));

         putAcc(ac, mkexpr(t3));
         break;
      }
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_ABSQ_SPH( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14,
              t15, t16, t17;
   UInt   rt, rd, sa, dsp_imm;

   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);
   dsp_imm = get_dspImm(cins);

   switch (sa) {
      case 0x1: {  /* ABSQ_S.QB */
         DIP("absq_s.qb r%u, r%u", rd, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I8);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I8);
         t4 = newTemp(Ity_I8);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I8);
         t8 = newTemp(Ity_I8);
         t9 = newTemp(Ity_I1);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I8);
         t12 = newTemp(Ity_I8);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I1);
         t15 = newTemp(Ity_I8);
         t16 = newTemp(Ity_I32);
         t17 = newTemp(Ity_I32);

         /* Absolute value of the rightmost byte (bits 7-0). */
         /* t0 - rightmost byte. */
         assign(t0, unop(Iop_16to8, unop(Iop_32to16, getIReg(rt))));
         /* t1 holds 1 if t0 is equal to 0x80, or 0 otherwise. */
         assign(t1, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32, mkexpr(t0)),
                          mkU32(0x00000080)));
         /* t2 holds 1 if value in t0 is negative, 0 otherwise. */
         assign(t2, unop(Iop_32to1,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     getIReg(rt),
                                     mkU32(0x00000080)),
                               mkU8(0x7))));
         /* t3 holds abs(t0). */
         assign(t3, IRExpr_ITE(mkexpr(t1),
                               mkU8(0x7F),
                               IRExpr_ITE(mkexpr(t2),
                                          binop(Iop_Add8,
                                                unop(Iop_Not8,
                                                      mkexpr(t0)),
                                                mkU8(0x1)),
                                          mkexpr(t0))));

         /* Absolute value of bits 15-8. */
         /* t4 - input byte. */
         assign(t4,
                unop(Iop_16HIto8, unop(Iop_32to16, getIReg(rt))));
         /* t5 holds 1 if t4 is equal to 0x80, or 0 otherwise. */
         assign(t5, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32, mkexpr(t4)),
                          mkU32(0x00000080)));
         /* t6 holds 1 if value in t4 is negative, 0 otherwise. */
         assign(t6, unop(Iop_32to1,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     getIReg(rt),
                                     mkU32(0x00008000)),
                               mkU8(15))));
         /* t3 holds abs(t4). */
         assign(t7, IRExpr_ITE(mkexpr(t5),
                               mkU8(0x7F),
                               IRExpr_ITE(mkexpr(t6),
                                          binop(Iop_Add8,
                                                unop(Iop_Not8,
                                                      mkexpr(t4)),
                                                mkU8(0x1)),
                                          mkexpr(t4))));

         /* Absolute value of bits 23-15. */
         /* t8 - input byte. */
         assign(t8,
                unop(Iop_16to8, unop(Iop_32HIto16, getIReg(rt))));
         /* t9 holds 1 if t8 is equal to 0x80, or 0 otherwise. */
         assign(t9, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32, mkexpr(t8)),
                          mkU32(0x00000080)));
         /* t6 holds 1 if value in t8 is negative, 0 otherwise. */
         assign(t10, unop(Iop_32to1,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      getIReg(rt),
                                      mkU32(0x00800000)),
                                mkU8(23))));
         /* t3 holds abs(t8). */
         assign(t11, IRExpr_ITE(mkexpr(t9),
                                mkU8(0x7F),
                                IRExpr_ITE(mkexpr(t10),
                                           binop(Iop_Add8,
                                                 unop(Iop_Not8,
                                                       mkexpr(t8)),
                                                 mkU8(0x1)),
                                           mkexpr(t8))));

         /* Absolute value of bits 31-24. */
         /* t12 - input byte. */
         assign(t12,
                unop(Iop_16HIto8, unop(Iop_32HIto16, getIReg(rt))));
         /* t13 holds 1 if t12 is equal to 0x80, or 0 otherwise. */
         assign(t13, binop(Iop_CmpEQ32,
                           unop(Iop_8Uto32, mkexpr(t12)),
                           mkU32(0x00000080)));
         /* t14 holds 1 if value in t12 is negative, 0 otherwise. */
         assign(t14, unop(Iop_32to1,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      getIReg(rt),
                                      mkU32(0x80000000)),
                                mkU8(31))));
         /* t15 holds abs(t12). */
         assign(t15, IRExpr_ITE(mkexpr(t13),
                                mkU8(0x7F),
                                IRExpr_ITE(mkexpr(t14),
                                           binop(Iop_Add8,
                                                 unop(Iop_Not8,
                                                       mkexpr(t12)),
                                                 mkU8(0x1)),
                                           mkexpr(t12))));

         /* t16 holds !0 if any of input bytes is 0x80 or 0
            otherwise. */
         assign(t16,
                binop(Iop_Or32,
                      binop(Iop_Or32,
                            binop(Iop_Or32,
                                  unop(Iop_1Sto32, mkexpr(t13)),
                                  unop(Iop_1Sto32, mkexpr(t9))),
                            unop(Iop_1Sto32, mkexpr(t5))),
                      unop(Iop_1Sto32, mkexpr(t1))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        mkexpr(t16),
                                        mkU32(0x0)),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000))));

         /* t17 = t15|t11|t7|t3 */
         assign(t17,
                binop(Iop_16HLto32,
                      binop(Iop_8HLto16, mkexpr(t15), mkexpr(t11)),
                      binop(Iop_8HLto16, mkexpr(t7), mkexpr(t3))));

         putIReg(rd, mkexpr(t17));
         break;
      }

      case 0x2: {  /* REPL.QB */
         DIP("repl.qb r%u, %u", rd, dsp_imm);
         vassert(!mode64);

         putIReg(rd, mkU32((dsp_imm << 24) | (dsp_imm << 16) |
                           (dsp_imm << 8) | (dsp_imm)));
         break;
      }

      case 0x3: {  /* REPLV.QB */
         DIP("replv.qb r%u, r%u", rd, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I8);

         assign(t0, unop(Iop_32to8,
                         binop(Iop_And32, getIReg(rt), mkU32(0xff))));
         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16, mkexpr(t0), mkexpr(t0)),
                       binop(Iop_8HLto16, mkexpr(t0), mkexpr(t0))));
         break;
      }

      case 0x4: {  /* PRECEQU.PH.QBL */
         DIP("precequ.ph.qbl r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0xff000000)),
                                 mkU8(1)),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x00ff0000)),
                                 mkU8(9))));
         break;
      }

      case 0x5: {  /* PRECEQU.PH.QBR */
         DIP("precequ.ph.qbr r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shl32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x0000ff00)),
                                 mkU8(15)),
                           binop(Iop_Shl32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x000000ff)),
                                 mkU8(7))));
         break;
      }

      case 0x6: {  /* PRECEQU.PH.QBLA */
         DIP("precequ.ph.qbla r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0xff000000)),
                                 mkU8(1)),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x0000ff00)),
                                 mkU8(1))));
         break;
      }

      case 0x7: {  /* PRECEQU.PH.QBRA */
         DIP("precequ.ph.qbra r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shl32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x00ff0000)),
                                 mkU8(7)),
                           binop(Iop_Shl32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x000000ff)),
                                 mkU8(7))));
         break;
      }

      case 0x9: {  /* ABSQ_S.PH */
         DIP("absq_s.ph r%u, r%u", rd, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I16);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I16);
         t4 = newTemp(Ity_I16);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I16);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);

         /* t0 holds lower 16 bits of value in rt. */
         assign(t0, unop(Iop_32to16, getIReg(rt)));
         /* t1 holds 1 if t0 is equal to 0x8000. */
         assign(t1, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32, mkexpr(t0)),
                          mkU32(0x00008000)));
         /* t2 holds 1 if value in t0 is negative, 0 otherwise. */
         assign(t2, unop(Iop_32to1,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     getIReg(rt),
                                     mkU32(0x00008000)),
                               mkU8(15))));
         /* t3 holds abs(t0). */
         assign(t3, IRExpr_ITE(mkexpr(t1),
                               mkU16(0x7FFF),
                               IRExpr_ITE(mkexpr(t2),
                                          binop(Iop_Add16,
                                                unop(Iop_Not16,
                                                      mkexpr(t0)),
                                                mkU16(0x1)),
                                          mkexpr(t0))));

         /* t4 holds lower 16 bits of value in rt. */
         assign(t4, unop(Iop_32HIto16, getIReg(rt)));
         /* t5 holds 1 if t4 is equal to 0x8000. */
         assign(t5, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32, mkexpr(t4)),
                          mkU32(0x00008000)));
         /* t6 holds 1 if value in t4 is negative, 0 otherwise. */
         assign(t6, unop(Iop_32to1,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     getIReg(rt),
                                     mkU32(0x80000000)),
                               mkU8(31))));
         /* t7 holds abs(t4). */
         assign(t7, IRExpr_ITE(mkexpr(t5),
                               mkU16(0x7FFF),
                               IRExpr_ITE(mkexpr(t6),
                                          binop(Iop_Add16,
                                                unop(Iop_Not16,
                                                      mkexpr(t4)),
                                                mkU16(0x1)),
                                          mkexpr(t4))));
         /* If any of the two input halfwords is equal 0x8000,
            set bit 20 in DSPControl register. */
         assign(t8, binop(Iop_Or32,
                          unop(Iop_1Sto32, mkexpr(t5)),
                          unop(Iop_1Sto32, mkexpr(t1))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        mkexpr(t8),
                                        mkU32(0x0)),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000))));

         /* t9 = t7|t3 */
         assign(t9, binop(Iop_16HLto32, mkexpr(t7), mkexpr(t3)));

         putIReg(rd, mkexpr(t9));
         break;
      }

      case 0xA: {  /* REPL.PH */
         DIP("repl.ph r%u, %u", rd, dsp_imm);
         vassert(!mode64);
         UShort immediate = extend_s_10to16(dsp_imm);

         putIReg(rd, mkU32(immediate << 16 | immediate));
         break;
      }

      case 0xB: {  /* REPLV.PH */
         DIP("replv.ph r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, getIReg(rt)),
                           unop(Iop_32to16, getIReg(rt))));
         break;
      }

      case 0xC: {  /* PRECEQ.W.PHL */
         DIP("preceq.w.phl r%u, r%u", rd, rt);
         vassert(!mode64);
         putIReg(rd, binop(Iop_And32,
                           getIReg(rt),
                           mkU32(0xffff0000)));
         break;
      }

      case 0xD: {  /* PRECEQ.W.PHR */
         DIP("preceq.w.phr r%u, r%u", rd, rt);
         vassert(!mode64);
         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, getIReg(rt)),
                           mkU16(0x0)));
         break;
      }

      case 0x11: {  /* ABSQ_S.W */
         DIP("absq_s.w r%u, r%u", rd, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I1);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);

         assign(t0,
                binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x80000000)));

         putDSPControl(IRExpr_ITE(mkexpr(t0),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         assign(t1, binop(Iop_CmpLT32S, getIReg(rt), mkU32(0x0)));

         assign(t2, IRExpr_ITE(mkexpr(t0),
                               mkU32(0x7FFFFFFF),
                               IRExpr_ITE(mkexpr(t1),
                                          binop(Iop_Add32,
                                                unop(Iop_Not32,
                                                      getIReg(rt)),
                                                mkU32(0x1)),
                                          getIReg(rt))));
         putIReg(rd, mkexpr(t2));
         break;
      }

      case 0x1B: {  /* BITREV */
         DIP("bitrev r%u, r%u", rd, rt);
         vassert(!mode64);
         /* 32bit reversal as seen on Bit Twiddling Hacks site
            http://graphics.stanford.edu/~seander/bithacks.html
            section ReverseParallel */
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);

         assign(t1, binop(Iop_Or32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      getIReg(rt),
                                      mkU32(0xaaaaaaaa)),
                                mkU8(0x1)),
                          binop(Iop_Shl32,
                                binop(Iop_And32,
                                      getIReg(rt),
                                      mkU32(0x55555555)),
                                mkU8(0x1))));
         assign(t2, binop(Iop_Or32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t1),
                                      mkU32(0xcccccccc)),
                                mkU8(0x2)),
                          binop(Iop_Shl32,
                                binop(Iop_And32,
                                      mkexpr(t1),
                                      mkU32(0x33333333)),
                                mkU8(0x2))));
         assign(t3, binop(Iop_Or32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t2),
                                      mkU32(0xf0f0f0f0)),
                                mkU8(0x4)),
                          binop(Iop_Shl32,
                                binop(Iop_And32,
                                      mkexpr(t2),
                                      mkU32(0x0f0f0f0f)),
                                mkU8(0x4))));
         assign(t4, binop(Iop_Or32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t3),
                                      mkU32(0xff00ff00)),
                                mkU8(0x8)),
                          binop(Iop_Shl32,
                                binop(Iop_And32,
                                      mkexpr(t3),
                                      mkU32(0x00ff00ff)),
                                mkU8(0x8))));
         assign(t5, binop(Iop_Or32,
                          binop(Iop_Shr32,
                                mkexpr(t4),
                                mkU8(0x10)),
                          binop(Iop_Shl32,
                                mkexpr(t4),
                                mkU8(0x10))));
         putIReg(rd, binop(Iop_Shr32,
                           mkexpr(t5),
                           mkU8(16)));
         break;
      }

      case 0x1C: {  /* PRECEU.PH.QBL */
         DIP("preceu.ph.qbl r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0xff000000)),
                                 mkU8(8)),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x00ff0000)),
                                 mkU8(16))));
         break;
      }

      case 0x1E: {  /* PRECEU.PH.QBLA */
         DIP("preceu.ph.qbla r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0xff000000)),
                                 mkU8(8)),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x0000ff00)),
                                 mkU8(8))));
         break;
      }

      case 0x1D: {  /* PRECEU.PH.QBR */
         DIP("preceu.ph.qbr r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Shl32,
                                 binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0x0000ff00)),
                                 mkU8(8)),
                           binop(Iop_And32,
                                 getIReg(rt),
                                 mkU32(0x000000ff))));
         break;
      }

      case 0x1F: {  /* PRECEU.PH.QBRA */
         DIP("preceu.ph.qbra r%u, r%u", rd, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_And32,
                                 getIReg(rt),
                                 mkU32(0x00ff0000)),
                           binop(Iop_And32,
                                 getIReg(rt),
                                 mkU32(0x000000ff))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_EXTR_W( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14,
              t15, t16, t17;
   UInt   rs, rt, rd, sa, ac, rddsp_mask,
          wrdsp_mask, shift;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);
   ac = get_acNo(cins);
   rddsp_mask = get_rddspMask(cins);
   wrdsp_mask = get_wrdspMask(cins);
   shift = get_shift(cins);

   switch (sa) {
      case 0x0: {  /* EXTR.W */
         DIP("extr.w r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));

         if (0 == rs) {
            assign(t1, mkexpr(t0));
         } else {
            assign(t1, binop(Iop_Sar64, mkexpr(t0), mkU8(rs)));
         }

         /* Check if bits 63..31 of the result in t1 aren't 0. */
         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0)));
         /* Check if bits 63..31 of the result in t1 aren't
            0x1ffffffff. */
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0x80000000)));
         /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
            control register. */
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t3)),
                                unop(Iop_1Sto32, mkexpr(t4))),
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t5)),
                                unop(Iop_1Sto32, mkexpr(t6)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t7),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         /* If the last discarded bit is 1, there would be carry
            when rounding, otherwise there wouldn't. We use that
            fact and just add the value of the last discarded bit
            to the least sifgnificant bit of the shifted value
            from acc. */
         if (0 == rs) {
            assign(t8, mkU64(0x0ULL));
         } else {
            assign(t8, binop(Iop_And64,
                             binop(Iop_Shr64,
                                   mkexpr(t0),
                                   mkU8(rs - 1)),
                             mkU64(0x1ULL)));
         }

         assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

         /* Repeat previous steps for the rounded value. */
         assign(t10, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0)));
         assign(t11, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0)));

         assign(t12, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0xffffffff)));
         assign(t13, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0x80000000)));

         assign(t14, binop(Iop_And32,
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t10)),
                                 unop(Iop_1Sto32, mkexpr(t11))),
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t12)),
                                 unop(Iop_1Sto32, mkexpr(t13)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t14),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         if (0 == rs) {
            putIReg(rt, unop(Iop_64to32, mkexpr(t0)));
         } else {
            putIReg(rt, unop(Iop_64to32, mkexpr(t1)));
         }

         break;
      }

      case 0x1: {  /* EXTRV.W */
         DIP("extrv.w r%u, ac%u, r%u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I32);
         t15 = newTemp(Ity_I8);

         assign(t15, unop(Iop_32to8,
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0x1f))));
         assign(t0, getAcc(ac));
         assign(t1, binop(Iop_Sar64, mkexpr(t0), mkexpr(t15)));
         putIReg(rt, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           mkexpr(t15)),
                                      mkU32(0)),
                                unop(Iop_64to32, mkexpr(t0)),
                                unop(Iop_64to32, mkexpr(t1))));

         /* Check if bits 63..31 of the result in t1 aren't 0. */
         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0)));
         /* Check if bits 63..31 of the result in t1 aren't
            0x1ffffffff. */
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0x80000000)));
         /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
            control register. */
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t3)),
                                unop(Iop_1Sto32, mkexpr(t4))),
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t5)),
                                unop(Iop_1Sto32, mkexpr(t6)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t7),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         /* If the last discarded bit is 1, there would be carry
            when rounding, otherwise there wouldn't. We use that
            fact and just add the value of the last discarded bit
            to the least sifgnificant bit of the shifted value
            from acc. */
         assign(t8,
                IRExpr_ITE(binop(Iop_CmpEQ32,
                                 unop(Iop_8Uto32,
                                      mkexpr(t15)),
                                 mkU32(0)),
                           mkU64(0x0ULL),
                           binop(Iop_And64,
                                 binop(Iop_Shr64,
                                       mkexpr(t0),
                                       unop(Iop_32to8,
                                            binop(Iop_Sub32,
                                                  unop(Iop_8Uto32,
                                                        mkexpr(t15)),
                                                  mkU32(1)))),
                                 mkU64(0x1ULL))));

         assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

         /* Repeat previous steps for the rounded value. */
         assign(t10, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0)));
         assign(t11, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0)));

         assign(t12, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0xffffffff)));
         assign(t13, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0x80000000)));

         assign(t14, binop(Iop_And32,
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t10)),
                                 unop(Iop_1Sto32, mkexpr(t11))),
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t12)),
                                 unop(Iop_1Sto32, mkexpr(t13)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t14),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         break;
      }

      case 0x2: {  /* EXTP */
         DIP("extp r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I8);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));
         /* Extract pos field of DSPControl register. */
         assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

         /* Check if (pos - size) >= 0 [size <= pos]
            if (pos < size)
               put 1 to EFI field of DSPControl register
            else
               extract bits from acc and put 0 to EFI field of
               DSPCtrl */
         assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkU32(rs)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        binop(Iop_And32,
                                              getDSPControl(),
                                              mkU32(0xffffbfff)),
                                        mkU32(0x4000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xffffbfff))));

         /* If pos <= 31, shift right the value from the acc
            (pos-size) times and take (size+1) bits from the least
            significant positions. Otherwise, shift left the value
            (63-pos) times, take (size+1) bits from the most
            significant positions and shift right (31-size) times.*/
         assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

         assign(t4,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkexpr(t1), mkU32(rs))),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkU32(63), mkexpr(t1)))));

         assign(t5, IRExpr_ITE(mkexpr(t3),
                               binop(Iop_Shr64,
                                     mkexpr(t0), mkexpr(t4)),
                               binop(Iop_Shl64,
                                     mkexpr(t0), mkexpr(t4))));

         /* t6 holds a mask for bit extraction */
         assign(t6,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_Not64,
                                binop(Iop_Shl64,
                                      mkU64(0xffffffffffffffffULL),
                                      mkU8(rs + 1))),
                           unop(Iop_Not64,
                                binop(Iop_Shr64,
                                      mkU64(0xffffffffffffffffULL),
                                      mkU8(rs + 1)))));

         assign(t7, IRExpr_ITE(mkexpr(t3),
                               unop(Iop_64to32,
                                    binop(Iop_And64,
                                          mkexpr(t5),
                                          mkexpr(t6))),
                               binop(Iop_Shr32,
                                     unop(Iop_64HIto32,
                                          binop(Iop_And64,
                                                mkexpr(t5),
                                                mkexpr(t6))),
                                     mkU8(31 - rs))));

         putIReg(rt, mkexpr(t7));
         break;
      }

      case 0x3: {  /* EXTPV */
         DIP("extpv r%u, ac%u, r%u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I8);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t8, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
         assign(t0, getAcc(ac));
         /* Extract pos field of DSPControl register. */
         assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

         /* Check if (pos - size) >= 0 [size <= pos]
            if (pos < size)
               put 1 to EFI field of DSPControl register
            else
               extract bits from acc and put 0 to EFI field of
               DSPCtrl */
         assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkexpr(t8)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        binop(Iop_And32,
                                              getDSPControl(),
                                              mkU32(0xffffbfff)),
                                        mkU32(0x4000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xffffbfff))));

         /* If pos <= 31, shift right the value from the acc
            (pos-size) times and take (size+1) bits from the least
            significant positions. Otherwise, shift left the value
            (63-pos) times, take (size+1) bits from the most
            significant positions and shift right (31-size)
            times. */
         assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

         assign(t4,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkexpr(t1), mkexpr(t8))),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkU32(63), mkexpr(t1)))));

         assign(t5, IRExpr_ITE(mkexpr(t3),
                               binop(Iop_Shr64,
                                     mkexpr(t0), mkexpr(t4)),
                               binop(Iop_Shl64,
                                     mkexpr(t0), mkexpr(t4))));

         /* t6 holds a mask for bit extraction. */
         assign(t6,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_Not64,
                                binop(Iop_Shl64,
                                      mkU64(0xffffffffffffffffULL),
                                      unop(Iop_32to8,
                                           binop(Iop_Add32,
                                                 mkexpr(t8),
                                                 mkU32(1))))),
                           unop(Iop_Not64,
                                binop(Iop_Shr64,
                                      mkU64(0xffffffffffffffffULL),
                                      unop(Iop_32to8,
                                           binop(Iop_Add32,
                                                 mkexpr(t8),
                                                 mkU32(1)))))));

         assign(t7, IRExpr_ITE(mkexpr(t3),
                               unop(Iop_64to32,
                                    binop(Iop_And64,
                                          mkexpr(t5),
                                          mkexpr(t6))),
                               binop(Iop_Shr32,
                                     unop(Iop_64HIto32,
                                          binop(Iop_And64,
                                                mkexpr(t5),
                                                mkexpr(t6))),
                                     unop(Iop_32to8,
                                          binop(Iop_Sub32,
                                                mkU32(31),
                                                mkexpr(t8))))));

         putIReg(rt, mkexpr(t7));
         break;
      }

      case 0x4: {  /* EXTR_R.W */
         DIP("extr_r.w r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I32);
         t15 = newTemp(Ity_I64);
         t16 = newTemp(Ity_I1);

         assign(t0, getAcc(ac));
         assign(t16, binop(Iop_CmpEQ32,
                           mkU32(rs),
                           mkU32(0)));
         assign(t1, IRExpr_ITE(mkexpr(t16),
                               mkexpr(t0),
                               binop(Iop_Sar64,
                                     mkexpr(t0),
                                     mkU8(rs))));
         /* If the last discarded bit is 1, there would be carry
            when rounding, otherwise there wouldn't. We use that
            fact and just add the value of the last discarded bit
            to the least significant bit of the shifted value
            from acc. */
         assign(t15, binop(Iop_Shr64,
                           mkexpr(t0),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      binop(Iop_And32,
                                            mkU32(rs),
                                            mkU32(0x1f)),
                                      mkU32(1)))));

         assign(t8,
                IRExpr_ITE(mkexpr(t16),
                           mkU64(0x0ULL),
                           binop(Iop_And64,
                                 mkexpr(t15),
                                 mkU64(0x0000000000000001ULL))));
         assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));
         putIReg(rt, unop(Iop_64to32, mkexpr(t9)));

         /* Check if bits 63..31 of the result in t1 aren't 0. */
         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0)));

         /* Check if bits 63..31 of the result in t1 aren't
            0x1ffffffff. */
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0x80000000)));
         /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
            control register. */
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t3)),
                                unop(Iop_1Sto32, mkexpr(t4))),
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t5)),
                                unop(Iop_1Sto32, mkexpr(t6)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t7),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         /* Repeat previous steps for the rounded value. */
         assign(t10, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0)));
         assign(t11, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0)));

         assign(t12, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0xffffffff)));
         assign(t13, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0x80000000)));

         assign(t14, binop(Iop_And32,
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t10)),
                                 unop(Iop_1Sto32, mkexpr(t11))),
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t12)),
                                 unop(Iop_1Sto32, mkexpr(t13)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t14),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         break;
      }

      case 0x5: {  /* EXTRV_R.W */
         DIP("extrv_r.w r%u, ac%u, r%u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I32);
         t15 = newTemp(Ity_I8);

         assign(t15, unop(Iop_32to8,
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0x1f))));
         assign(t0, getAcc(ac));
         assign(t1, binop(Iop_Sar64, mkexpr(t0), mkexpr(t15)));

         /* Check if bits 63..31 of the result in t1 aren't 0. */
         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0)));
         /* Check if bits 63..31 of the result in t1 aren't
            0x1ffffffff. */
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0x80000000)));
         /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
            control register. */
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t3)),
                                unop(Iop_1Sto32, mkexpr(t4))),
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t5)),
                                unop(Iop_1Sto32, mkexpr(t6)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t7),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         /* If the last discarded bit is 1, there would be carry
            when rounding, otherwise there wouldn't. We use that
            fact and just add the value of the last discarded bit
            to the least sifgnificant bit of the shifted value
            from acc. */
         assign(t8,
                IRExpr_ITE(binop(Iop_CmpEQ32,
                                 unop(Iop_8Uto32,
                                      mkexpr(t15)),
                                 mkU32(0)),
                           mkU64(0x0ULL),
                           binop(Iop_And64,
                                 binop(Iop_Shr64,
                                       mkexpr(t0),
                                       unop(Iop_32to8,
                                            binop(Iop_Sub32,
                                                  unop(Iop_8Uto32,
                                                        mkexpr(t15)),
                                                  mkU32(1)))),
                                 mkU64(0x1ULL))));

         assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));
         /* Put rounded value in destination register. */
         putIReg(rt, unop(Iop_64to32, mkexpr(t9)));

         /* Repeat previous steps for the rounded value. */
         assign(t10, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0)));
         assign(t11, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0)));

         assign(t12, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0xffffffff)));
         assign(t13, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0x80000000)));

         assign(t14, binop(Iop_And32,
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t10)),
                                 unop(Iop_1Sto32, mkexpr(t11))),
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t12)),
                                 unop(Iop_1Sto32, mkexpr(t13)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t14),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         break;
      }

      case 0x6: {  /* EXTR_RS.W */
         DIP("extr_rs.w r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I32);
         t16 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));

         if (0 == rs) {
            assign(t1, mkexpr(t0));
         } else {
            assign(t1, binop(Iop_Sar64, mkexpr(t0), mkU8(rs)));
         }

         /* Check if bits 63..31 of the result in t1 aren't 0. */
         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0)));
         /* Check if bits 63..31 of the result in t1 aren't
            0x1ffffffff. */
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0x80000000)));
         /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
            control register. */
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t3)),
                                unop(Iop_1Sto32, mkexpr(t4))),
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t5)),
                                unop(Iop_1Sto32, mkexpr(t6)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t7),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         /* If the last discarded bit is 1, there would be carry
            when rounding, otherwise there wouldn't. We use that
            fact and just add the value of the last discarded bit
            to the least sifgnificant bit of the shifted value
            from acc. */
         if (0 == rs) {
            assign(t8, mkU64(0x0ULL));
         } else {
            assign(t8, binop(Iop_And64,
                             binop(Iop_Shr64,
                                   mkexpr(t0),
                                   mkU8(rs - 1)),
                             mkU64(0x1ULL)));
         }

         assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

         /* Repeat previous steps for the rounded value. */
         assign(t10, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0)));
         assign(t11, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0)));

         assign(t12, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0xffffffff)));
         assign(t13, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0x80000000)));

         assign(t14, binop(Iop_And32,
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t10)),
                                 unop(Iop_1Sto32, mkexpr(t11))),
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t12)),
                                 unop(Iop_1Sto32, mkexpr(t13)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t14),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         assign(t16, binop(Iop_And32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0x80000000)));
         putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkexpr(t14),
                                      mkU32(0)),
                                IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 mkexpr(t16),
                                                 mkU32(0)),
                                           mkU32(0x7fffffff),
                                           mkU32(0x80000000)),
                                unop(Iop_64to32, mkexpr(t9))));
         break;
      }

      case 0x7: {  /* EXTRV_RS.W */
         DIP("extrv_rs.w r%u, ac%u, r%u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I32);
         t15 = newTemp(Ity_I32);
         t16 = newTemp(Ity_I32);
         t17 = newTemp(Ity_I1);

         assign(t15, binop(Iop_And32,
                           getIReg(rs),
                           mkU32(0x1f)));
         assign(t17, binop(Iop_CmpEQ32,
                           mkexpr(t15),
                           mkU32(0)));
         assign(t0, getAcc(ac));
         assign(t1, IRExpr_ITE(mkexpr(t17),
                               mkexpr(t0),
                               binop(Iop_Sar64,
                                     mkexpr(t0),
                                     unop(Iop_32to8,
                                          mkexpr(t15)))));

         /* Check if bits 63..31 of the result in t1 aren't 0. */
         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0)));
         /* Check if bits 63..31 of the result in t1 aren't
            0x1ffffffff. */
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32,
                               mkexpr(t1)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64to32,
                                     mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU32(0x80000000)));
         /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
            control register. */
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t3)),
                                unop(Iop_1Sto32, mkexpr(t4))),
                          binop(Iop_Or32,
                                unop(Iop_1Sto32, mkexpr(t5)),
                                unop(Iop_1Sto32, mkexpr(t6)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t7),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         /* If the last discarded bit is 1, there would be carry
            when rounding, otherwise there wouldn't. We use that
            fact and just add the value of the last discarded bit
            to the least sifgnificant bit of the shifted value
            from acc. */
         assign(t8,
                IRExpr_ITE(mkexpr(t17),
                           mkU64(0x0ULL),
                           binop(Iop_And64,
                                 binop(Iop_Shr64,
                                       mkexpr(t0),
                                       unop(Iop_32to8,
                                            binop(Iop_Sub32,
                                                  mkexpr(t15),
                                                  mkU32(1)))),
                                 mkU64(0x1ULL))));

         assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

         /* Repeat previous steps for the rounded value. */
         assign(t10, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0)));
         assign(t11, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0)));

         assign(t12, binop(Iop_CmpNE32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0xffffffff)));
         assign(t13, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 unop(Iop_64to32,
                                      mkexpr(t9)),
                                 mkU32(0x80000000)),
                           mkU32(0x80000000)));

         assign(t14, binop(Iop_And32,
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t10)),
                                 unop(Iop_1Sto32, mkexpr(t11))),
                           binop(Iop_Or32,
                                 unop(Iop_1Sto32, mkexpr(t12)),
                                 unop(Iop_1Sto32, mkexpr(t13)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t14),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));

         assign(t16, binop(Iop_And32,
                           unop(Iop_64HIto32,
                                mkexpr(t9)),
                           mkU32(0x80000000)));
         putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkexpr(t14),
                                      mkU32(0)),
                                IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 mkexpr(t16),
                                                 mkU32(0)),
                                           mkU32(0x7fffffff),
                                           mkU32(0x80000000)),
                                unop(Iop_64to32, mkexpr(t9))));
         break;
      }

      case 0xA: {  /* EXTPDP */
         DIP("extpdp r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I8);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));
         /* Extract pos field of DSPControl register. */
         assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

         /* Check if (pos - size) >= 0 [size <= pos]
            if (pos < size)
               put 1 to EFI field of DSPControl register
            else
               extract bits from acc and put 0 to EFI field of
               DSPCtrl */
         assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkU32(rs)));

         assign(t8, binop(Iop_Or32,
                          binop(Iop_And32,
                                getDSPControl(),
                                mkU32(0xffffbfc0)),
                          binop(Iop_And32,
                                binop(Iop_Sub32,
                                      binop(Iop_And32,
                                            getDSPControl(),
                                            mkU32(0x3f)),
                                      mkU32(rs + 1)),
                                mkU32(0x3f))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        binop(Iop_And32,
                                              getDSPControl(),
                                              mkU32(0xffffbfff)),
                                        mkU32(0x4000)),
                                  mkexpr(t8)));

         /* If pos <= 31, shift right the value from the acc
            (pos-size) times and take (size+1) bits from the least
            significant positions. Otherwise, shift left the value
            (63-pos) times, take (size+1) bits from the most
            significant positions and shift right (31-size) times.
         */
         assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

         assign(t4,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkexpr(t1), mkU32(rs))),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkU32(63), mkexpr(t1)))));

         assign(t5, IRExpr_ITE(mkexpr(t3),
                               binop(Iop_Shr64,
                                     mkexpr(t0), mkexpr(t4)),
                               binop(Iop_Shl64,
                                     mkexpr(t0), mkexpr(t4))));

         /* t6 holds a mask for bit extraction. */
         assign(t6,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_Not64,
                                binop(Iop_Shl64,
                                      mkU64(0xffffffffffffffffULL),
                                      mkU8(rs + 1))),
                           unop(Iop_Not64,
                                binop(Iop_Shr64,
                                      mkU64(0xffffffffffffffffULL),
                                      mkU8(rs + 1)))));

         assign(t7, IRExpr_ITE(mkexpr(t3),
                               unop(Iop_64to32,
                                    binop(Iop_And64,
                                          mkexpr(t5),
                                          mkexpr(t6))),
                               binop(Iop_Shr32,
                                     unop(Iop_64HIto32,
                                          binop(Iop_And64,
                                                mkexpr(t5),
                                                mkexpr(t6))),
                                     mkU8(31 - rs))));

         putIReg(rt, mkexpr(t7));
         break;
      }

      case 0xB: {  /* EXTPDPV */
         DIP("extpdpv r%u, ac%u, r%u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I8);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);

         assign(t8, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
         assign(t0, getAcc(ac));
         /* Extract pos field of DSPControl register. */
         assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

         /* Check if (pos - size) >= 0 [size <= pos]
            if (pos < size)
               put 1 to EFI field of DSPControl register
            else
               extract bits from acc and put 0 to EFI field of
               DSPCtrl */
         assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkexpr(t8)));

         assign(t9, binop(Iop_Or32,
                          binop(Iop_And32,
                                getDSPControl(),
                                mkU32(0xffffbfc0)),
                          binop(Iop_And32,
                                binop(Iop_Sub32,
                                      binop(Iop_And32,
                                            getDSPControl(),
                                            mkU32(0x3f)),
                                      binop(Iop_Add32,
                                            mkexpr(t8),
                                            mkU32(0x1))),
                                mkU32(0x3f))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        binop(Iop_And32,
                                              getDSPControl(),
                                              mkU32(0xffffbfff)),
                                        mkU32(0x4000)),
                                  mkexpr(t9)));

         /* If pos <= 31, shift right the value from the acc
            (pos-size) times and take (size+1) bits from the least
            significant positions. Otherwise, shift left the value
            (63-pos) times, take (size+1) bits from the most
            significant positions and shift right (31-size) times.
         */
         assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

         assign(t4,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkexpr(t1), mkexpr(t8))),
                           unop(Iop_32to8,
                                binop(Iop_Sub32,
                                      mkU32(63), mkexpr(t1)))));

         assign(t5, IRExpr_ITE(mkexpr(t3),
                               binop(Iop_Shr64,
                                     mkexpr(t0), mkexpr(t4)),
                               binop(Iop_Shl64,
                                     mkexpr(t0), mkexpr(t4))));

         /* t6 holds a mask for bit extraction. */
         assign(t6,
                IRExpr_ITE(mkexpr(t3),
                           unop(Iop_Not64,
                                binop(Iop_Shl64,
                                      mkU64(0xffffffffffffffffULL),
                                      unop(Iop_32to8,
                                           binop(Iop_Add32,
                                                 mkexpr(t8),
                                                 mkU32(1))))),
                           unop(Iop_Not64,
                                binop(Iop_Shr64,
                                      mkU64(0xffffffffffffffffULL),
                                      unop(Iop_32to8,
                                           binop(Iop_Add32,
                                                 mkexpr(t8),
                                                 mkU32(1)))))));

         assign(t7, IRExpr_ITE(mkexpr(t3),
                               unop(Iop_64to32,
                                    binop(Iop_And64,
                                          mkexpr(t5),
                                          mkexpr(t6))),
                               binop(Iop_Shr32,
                                     unop(Iop_64HIto32,
                                          binop(Iop_And64,
                                                mkexpr(t5),
                                                mkexpr(t6))),
                                     unop(Iop_32to8,
                                          binop(Iop_Sub32,
                                                mkU32(31),
                                                mkexpr(t8))))));

         putIReg(rt, mkexpr(t7));
         break;
      }

      case 0xE: {  /* EXTR_S.H */
         DIP("extr_s.h r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Sar64, mkexpr(t0), mkU8(rs)));

         assign(t2, binop(Iop_Or32,
                          getDSPControl(), mkU32(0x00800000)));

         assign(t9, binop(Iop_And32,
                          unop(Iop_64to32,
                               mkexpr(t1)),
                          mkU32(0x80000000)));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t9),
                                        binop(Iop_And32,
                                              unop(Iop_64HIto32,
                                                    mkexpr(t0)),
                                              mkU32(0x80000000))),
                                  mkexpr(t2),
                                  getDSPControl()));

         /* Check if t1 > 0x7fff ((t1 - 0x7fff) > 0)
            1. subtract 0x7fff from t1
            2. if the resulting number is positive (sign bit = 0)
               and any of the other bits is 1, the value is > 0. */
         assign(t3, binop(Iop_Sub64,
                          mkexpr(t1),
                          mkU64(0x0000000000007fffULL)));
         assign(t4, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           binop(Iop_And32,
                                                 unop(Iop_64HIto32,
                                                       mkexpr(t3)),
                                                 mkU32(0x7fffffff)))),
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           unop(Iop_64to32,
                                                mkexpr(t3))))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_64HIto32,
                                                mkexpr(t3)),
                                           mkU32(0x80000000)),
                                     mkU32(0)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkU32(0),
                                        mkexpr(t4)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         /* Check if t1<0xffffffffffff8000 (0xffffffffffff8000-t1)>0
            1. subtract t1 from 0xffffffffffff8000
            2. if the resulting number is positive (sign bit = 0)
                and any of the other bits is 1, the value is > 0 */
         assign(t6, binop(Iop_Sub64,
                          mkU64(0xffffffffffff8000ULL),
                          mkexpr(t1)));
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           binop(Iop_And32,
                                                 unop(Iop_64HIto32,
                                                       mkexpr(t6)),
                                                 mkU32(0x7fffffff)))),
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           unop(Iop_64to32,
                                                mkexpr(t6))))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_64HIto32,
                                                mkexpr(t6)),
                                           mkU32(0x80000000)),
                                     mkU32(0)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkU32(0),
                                        mkexpr(t7)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkU32(0),
                                      mkexpr(t4)),
                                mkU32(0x00007fff),
                                IRExpr_ITE(binop(Iop_CmpNE32,
                                                 mkU32(0),
                                                 mkexpr(t7)),
                                           mkU32(0xffff8000),
                                           unop(Iop_64to32,
                                                mkexpr(t1)))));
         break;
      }

      case 0xF: {  /* EXTRV_S.H */
         DIP("extrv_s.h r%u, ac%u, %u", rt, ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Sar64,
                          mkexpr(t0),
                          unop(Iop_32to8,
                               binop(Iop_And32,
                                     getIReg(rs),
                                     mkU32(0x1f)))));

         assign(t2, binop(Iop_Or32,
                          getDSPControl(), mkU32(0x00800000)));

         assign(t9, binop(Iop_And32,
                          unop(Iop_64to32,
                               mkexpr(t1)),
                          mkU32(0x80000000)));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t9),
                                        binop(Iop_And32,
                                              unop(Iop_64HIto32,
                                                    mkexpr(t0)),
                                              mkU32(0x80000000))),
                                  mkexpr(t2),
                                  getDSPControl()));

         /* Check if t1 > 0x7fff ((t1 - 0x7fff) > 0)
            1. subtract 0x7fff from t1
            2. if the resulting number is positive (sign bit = 0)
               and any of the other bits is 1, the value is > 0. */
         assign(t3, binop(Iop_Sub64,
                          mkexpr(t1),
                          mkU64(0x0000000000007fffULL)));
         assign(t4, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           binop(Iop_And32,
                                                 unop(Iop_64HIto32,
                                                       mkexpr(t3)),
                                                 mkU32(0x7fffffff)))),
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           unop(Iop_64to32,
                                                mkexpr(t3))))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_64HIto32,
                                                mkexpr(t3)),
                                           mkU32(0x80000000)),
                                     mkU32(0)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkU32(0),
                                        mkexpr(t4)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         /* Check if t1<0xffffffffffff8000 (0xffffffffffff8000-t1)>0
            1. subtract t1 from 0xffffffffffff8000
            2. if the resulting number is positive (sign bit = 0)
                and any of the other bits is 1, the value is > 0 */
         assign(t6, binop(Iop_Sub64,
                          mkU64(0xffffffffffff8000ULL),
                          mkexpr(t1)));
         assign(t7, binop(Iop_And32,
                          binop(Iop_Or32,
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           binop(Iop_And32,
                                                 unop(Iop_64HIto32,
                                                       mkexpr(t6)),
                                                 mkU32(0x7fffffff)))),
                                unop(Iop_1Sto32,
                                     binop(Iop_CmpNE32,
                                           mkU32(0),
                                           unop(Iop_64to32,
                                                mkexpr(t6))))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_64HIto32,
                                                mkexpr(t6)),
                                           mkU32(0x80000000)),
                                     mkU32(0)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkU32(0),
                                        mkexpr(t7)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00800000)),
                                  getDSPControl()));
         putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkU32(0),
                                      mkexpr(t4)),
                                mkU32(0x00007fff),
                                IRExpr_ITE(binop(Iop_CmpNE32,
                                                 mkU32(0),
                                                 mkexpr(t7)),
                                           mkU32(0xffff8000),
                                           unop(Iop_64to32,
                                                mkexpr(t1)))));
         break;
      }

      case 0x12: {  /* RDDSP*/
         DIP("rddsp r%u, mask 0x%x", rd, rddsp_mask);
         vassert(!mode64);

         putIReg(rd, mkU32(0x0));

         if ((rddsp_mask & 0x1) == 0x1) {
            /* Read pos field (bits 5-0) of DSPControl register. */
            putIReg(rd, binop(Iop_Or32,
                              getIReg(rd),
                              binop(Iop_And32,
                                    getDSPControl(),
                                    mkU32(0x0000003F))));
         }

         if ((rddsp_mask & 0x2) == 0x2) {
            /* Read scount field (bits 12-7) of DSPControl
               register. */
            putIReg(rd, binop(Iop_Or32,
                              getIReg(rd),
                              binop(Iop_And32,
                                    getDSPControl(),
                                    mkU32(0x00001F80))));
         }

         if ((rddsp_mask & 0x4) == 0x4) {
            /* Read C field (bit 13) of DSPControl register. */
            putIReg(rd, binop(Iop_Or32,
                              getIReg(rd),
                              binop(Iop_And32,
                                    getDSPControl(),
                                    mkU32(0x00002000))));
         }

         if ((rddsp_mask & 0x8) == 0x8) {
            /* Read outflag field (bit s 23-16) of DSPControl
               register. */
            putIReg(rd, binop(Iop_Or32,
                              getIReg(rd),
                              binop(Iop_And32,
                                    getDSPControl(),
                                    mkU32(0x00FF0000))));
         }

         if ((rddsp_mask & 0x10) == 0x10) {
            /* Read ccond field (bits 31-24) of DSPControl
               register. */
            putIReg(rd, binop(Iop_Or32,
                              getIReg(rd),
                              binop(Iop_And32,
                                    getDSPControl(),
                                    mkU32(0xFF000000))));
         }

         if ((rddsp_mask & 0x20) == 0x20) {
            /* Read EFI field (bit 14) of DSPControl register. */
            putIReg(rd, binop(Iop_Or32,
                              getIReg(rd),
                              binop(Iop_And32,
                                    getDSPControl(),
                                    mkU32(0x00004000))));
         }

         if ((rddsp_mask & 0x3f) == 0x3f) {
            /* Read all fields of DSPControl register. */
            putIReg(rd, getDSPControl());
         }

         break;
      }

      case 0x13: {  /* WRDSP */
         DIP("wrdsp r%u, mask 0x%x", rs, wrdsp_mask);
         vassert(!mode64);

         if ((wrdsp_mask & 0x3f) == 0x3f) {
            /* If mips64 put all fields of rs, except bit 15 and bit
               6, to DSPControl register, otherwise put all except
               bits 15, 6 and bits 31..28. */
            putDSPControl(mode64 ?
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0xffff7fbf)) :
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0x0fff7fbf)));
         } else {
            if ((wrdsp_mask & 0x1) == 0x1) {
               /* Put bits 5-0 of rs to DSPControl register pos
                  field. */
               putDSPControl(binop(Iop_Or32,
                                   binop(Iop_And32,
                                         getDSPControl(),
                                         mkU32(0xFFFF7F40)),
                                   binop(Iop_And32,
                                         getIReg(rs),
                                         mkU32(0x0000003F))));
            }

            if ((wrdsp_mask & 0x2) == 0x2) {
               /* Put bits 12-7 of rs to DSPControl scount field. */
               putDSPControl(binop(Iop_Or32,
                                   binop(Iop_And32,
                                         getDSPControl(),
                                         mkU32(0xFFFFE03F)),
                                   binop(Iop_And32,
                                         getIReg(rs),
                                         mkU32(0x00001F80))));
            }

            if ((wrdsp_mask & 0x4) == 0x4) {
               /* Put bit 13 of rs to DSPControl register C
                  field. */
               putDSPControl(binop(Iop_Or32,
                                   binop(Iop_And32,
                                         getDSPControl(),
                                         mkU32(0xFFFF5FBF)),
                                   binop(Iop_And32,
                                         getIReg(rs),
                                         mkU32(0x00002000))));
            }

            if ((wrdsp_mask & 0x8) == 0x8) {
               /* Put bits 23-16 of rs to DSPControl reg outflag
                  field. */
               putDSPControl(binop(Iop_Or32,
                                   binop(Iop_And32,
                                         getDSPControl(),
                                         mkU32(0xFF007FBF)),
                                   binop(Iop_And32,
                                         getIReg(rs),
                                         mkU32(0x00FF0000))));
            }

            if ((wrdsp_mask & 0x10) == 0x10) {
               /* Put bits 31-24 of rs to DSPControl reg ccond
                  field. */
               putDSPControl(binop(Iop_Or32,
                                   binop(Iop_And32,
                                         getDSPControl(),
                                         mkU32(0x00FF7FBF)),
                                   binop(Iop_And32,
                                         getIReg(rs),
                                         mode64 ? mkU32(0xFF000000)
                                         : mkU32(0x0F000000))
                                  )
                            );
            }

            if ((wrdsp_mask & 0x20) == 0x20) {
               /* Put bit 14 of rs to DSPControl register EFI
                  field. */
               putDSPControl(binop(Iop_Or32,
                                   binop(Iop_And32,
                                         getDSPControl(),
                                         mkU32(0xFFFF3FBF)),
                                   binop(Iop_And32,
                                         getIReg(rs),
                                         mkU32(0x00004000))));
            }
         }

         break;
      }

      case 0x1A: {  /* SHILO */
         DIP("shilo ac%u, %u", ac, shift);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));

         putAcc(ac, mkexpr(t0));

         if (0x20 == (shift & 0x3f)) {
            putAcc(ac, binop(Iop_32HLto64,
                             unop(Iop_64to32, mkexpr(t0)),
                             mkU32(0x0)));
         } else if (0x20 == (shift & 0x20)) {
            assign(t1, binop(Iop_Shl64,
                             mkexpr(t0),
                             unop(Iop_32to8,
                                  binop(Iop_Add32,
                                        unop(Iop_Not32,
                                             mkU32(shift)),
                                        mkU32(0x1)))));

            putAcc(ac, mkexpr(t1));
         } else {
            assign(t1, binop(Iop_Shr64, mkexpr(t0), mkU8(shift)));

            putAcc(ac, mkexpr(t1));
         }

         break;
      }

      case 0x1B: {  /* SHILOV */
         DIP("shilov ac%u, r%u", ac, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));
         assign(t1, binop(Iop_And32, getIReg(rs), mkU32(0x3f)));
         assign(t2, binop(Iop_CmpEQ32, mkexpr(t1), mkU32(0x20)));
         assign(t3, binop(Iop_Shl64,
                          mkexpr(t0),
                          unop(Iop_32to8,
                               binop(Iop_Add32,
                                     unop(Iop_Not32,
                                          mkexpr(t1)),
                                     mkU32(0x1)))));
         assign(t4, binop(Iop_Shr64,
                          mkexpr(t0),
                          unop(Iop_32to8,
                               mkexpr(t1))));

         putAcc(ac,
                IRExpr_ITE(mkexpr(t2),
                           binop(Iop_32HLto64,
                                 unop(Iop_64to32, mkexpr(t0)),
                                 mkU32(0x0)),
                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  mkexpr(t1),
                                                  mkU32(0x20)),
                                            mkU32(0x20)),
                                      mkexpr(t3),
                                      mkexpr(t4))));
         break;
      }

      case 0x1F: {  /* MTHLIP */
         DIP("mthlip r%u, ac%u", rs, ac);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);

         assign(t0, getAcc(ac));
         putAcc(ac, binop(Iop_32HLto64,
                          unop(Iop_64to32, mkexpr(t0)),
                          getIReg(rs)));
         assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpLE32U,
                                        mkU32(32),
                                        mkexpr(t1)),
                                  binop(Iop_Or32,
                                        binop(Iop_Sub32,
                                              mkexpr(t1),
                                              mkU32(32)),
                                        binop(Iop_And32,
                                              getDSPControl(),
                                              mkU32(0xffffffc0))),
                                  binop(Iop_Or32,
                                        binop(Iop_Add32,
                                              mkexpr(t1),
                                              mkU32(32)),
                                        binop(Iop_And32,
                                              getDSPControl(),
                                              mkU32(0xffffffc0)))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_LX( UInt cins )
{
   IRTemp t0;
   UInt   rs, rt, rd, sa;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x0: {  /* LWX */
         DIP("lwx r%u, r%u(r%u)", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);

         assign(t0, binop(Iop_Add32, getIReg(rt), getIReg(rs)));

         putIReg(rd, load(Ity_I32, mkexpr(t0)));
         break;
      }

      case 0x4: {  /* LHX */
         DIP("lhx r%u, r%u(r%u)", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);

         assign(t0, binop(Iop_Add32, getIReg(rt), getIReg(rs)));

         putIReg(rd, unop(Iop_16Sto32, load(Ity_I16, mkexpr(t0))));
         break;
      }

      case 0x6: {  /* LBUX */
         DIP("lbux r%u, r%u(r%u)", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);

         assign(t0, binop(Iop_Add32, getIReg(rt), getIReg(rs)));

         putIReg(rd, unop(Iop_8Uto32, load(Ity_I8, mkexpr(t0))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_INSV( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t6, t7, t8;
   UInt   rs, rt, sa;

   rs = get_rs(cins);
   rt = get_rt(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x0: {  /* INSV */
         DIP("insv r%u, r%u", rt, rs);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I8);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         /* t0 <- pos field of DSPControl register. */
         assign(t0, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));
         /* t1 <- scount field of DSPControl register. */
         assign(t1, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getDSPControl(),
                                mkU32(0x1f80)),
                          mkU8(7)));

         assign(t2, unop(Iop_32to8,
                         binop(Iop_Add32,
                               mkexpr(t1),
                               mkexpr(t0))));

         /* 32-(pos+size) most significant bits of rt. */
         assign(t6, binop(Iop_Shl32,
                          binop(Iop_Shr32,
                                getIReg(rt),
                                mkexpr(t2)),
                          mkexpr(t2)));

         assign(t3, unop(Iop_32to8,
                         binop(Iop_Sub32,
                               mkU32(32),
                               mkexpr(t0))));
         /* Pos least significant bits of rt. */
         assign(t7, binop(Iop_Shr32,
                          binop(Iop_Shl32,
                                getIReg(rt),
                                mkexpr(t3)),
                          mkexpr(t3)));

         /* Size least significant bits of rs,
            shifted to appropriate position. */
         assign(t8, binop(Iop_Shl32,
                          binop(Iop_And32,
                                getIReg(rs),
                                unop(Iop_Not32,
                                     binop(Iop_Shl32,
                                           mkU32(0xffffffff),
                                           unop(Iop_32to8,
                                                mkexpr(t1))))),
                          unop(Iop_32to8,
                               mkexpr(t0))));

         putIReg(rt, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      mkexpr(t0),
                                      mkU32(0)),
                                IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 mkexpr(t1),
                                                 mkU32(32)),
                                           getIReg(rs),
                                           binop(Iop_Or32,
                                                 mkexpr(t6),
                                                 mkexpr(t8))),
                                IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 unop(Iop_8Uto32,
                                                       mkexpr(t2)),
                                                 mkU32(32)),
                                           binop(Iop_Or32,
                                                 mkexpr(t7),
                                                 mkexpr(t8)),
                                           binop(Iop_Or32,
                                                 binop(Iop_Or32,
                                                       mkexpr(t6),
                                                       mkexpr(t7)),
                                                 mkexpr(t8)))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_ADDU_QB( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12;
   UInt   rs, rt, rd, sa;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x00: {  /* ADDU.QB */
         DIP("addu.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I32);

         /* Add rightmost bytes of rs and rt. */
         assign(t0,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* t1 will be 1 if there is overflow, 0 otherwise. */
         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         /* Add bits 15-8 of rs and rt. */
         assign(t2,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* t3 will be 1 if there is overflow, 0 otherwise. */
         assign(t3, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t2),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         /* Add bits 15-8 of rs and rt. */
         assign(t4,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         /* t5 will be 1 if there is overflow, 0 otherwise. */
         assign(t5, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t4),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         /* Add bits 15-8 of rs and rt. */
         assign(t6,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         /* t7 will be 1 if there is overflow, 0 otherwise. */
         assign(t7, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t6),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         assign(t8,
                binop(Iop_Or32,
                      binop(Iop_Or32,
                            binop(Iop_Or32,
                                  unop(Iop_1Sto32, mkexpr(t7)),
                                  unop(Iop_1Sto32,  mkexpr(t5))),
                            unop(Iop_1Sto32, mkexpr(t3))),
                      unop(Iop_1Sto32, mkexpr(t1))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        mkexpr(t8),
                                        mkU32(0x0)),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000))));

         putIReg(rd, binop(Iop_16HLto32,
                           binop(Iop_8HLto16,
                                 unop(Iop_32to8, mkexpr(t6)),
                                 unop(Iop_32to8, mkexpr(t4))),
                           binop(Iop_8HLto16,
                                 unop(Iop_32to8, mkexpr(t2)),
                                 unop(Iop_32to8, mkexpr(t0)))));
         break;
      }

      case 0x1: {  /* SUBU.QB */
         DIP("subu.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I32);

         /* Subtract rightmost bytes of rs and rt. */
         assign(t0,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* t1 will be 1 if there is overflow, 0 otherwise. */
         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         /* Subtract bits 15-8 of rs and rt. */
         assign(t2,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* t3 will be 1 if there is overflow, 0 otherwise. */
         assign(t3, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t2),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         /* Subtract bits 15-8 of rs and rt. */
         assign(t4,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         /* t5 will be 1 if there is overflow, 0 otherwise. */
         assign(t5, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t4),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         /* Subtract bits 15-8 of rs and rt. */
         assign(t6,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         /* t7 will be 1 if there is overflow, 0 otherwise. */
         assign(t7, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t6),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));

         assign(t8, binop(Iop_Or32,
                          binop(Iop_Or32,
                                binop(Iop_Or32,
                                      unop(Iop_1Sto32, mkexpr(t7)),
                                      unop(Iop_1Sto32, mkexpr(t5))),
                                unop(Iop_1Sto32, mkexpr(t3))),
                          unop(Iop_1Sto32, mkexpr(t1))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        mkexpr(t8),
                                        mkU32(0x0)),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000))));

         putIReg(rd, binop(Iop_16HLto32,
                           binop(Iop_8HLto16,
                                 unop(Iop_32to8, mkexpr(t6)),
                                 unop(Iop_32to8, mkexpr(t4))),
                           binop(Iop_8HLto16,
                                 unop(Iop_32to8, mkexpr(t2)),
                                 unop(Iop_32to8, mkexpr(t0)))));
         break;
      }

      case 0x04: {  /* ADDU_S.QB */
         DIP("addu_s.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I8);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I8);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I8);
         t12 = newTemp(Ity_I32);

         /* Add rightmost bytes of rs and rt. */
         assign(t0,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* t1 will be 1 if there is overflow, 0 otherwise. */
         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         /* Saturate if necessary. */
         assign(t2, IRExpr_ITE(mkexpr(t1),
                               mkU8(0xff),
                               unop(Iop_32to8, mkexpr(t0))));

         /* Add bits 15-8 of rs and rt. */
         assign(t3,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* t4 will be 1 if there is overflow, 0 otherwise. */
         assign(t4, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t3),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         /* Saturate if necessary. */
         assign(t5, IRExpr_ITE(mkexpr(t4),
                               mkU8(0xff),
                               unop(Iop_32to8, mkexpr(t3))));

         /* Add bits 15-8 of rs and rt. */
         assign(t6,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         /* t7 will be 1 if there is overflow, 0 otherwise. */
         assign(t7, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t6),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         /* Saturate if necessary. */
         assign(t8, IRExpr_ITE(mkexpr(t7),
                               mkU8(0xff),
                               unop(Iop_32to8, mkexpr(t6))));

         /* Add bits 15-8 of rs and rt. */
         assign(t9,
                binop(Iop_Add32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         /* t10 will be 1 if there is overflow, 0 otherwise. */
         assign(t10, binop(Iop_CmpEQ32,
                           binop(Iop_And32,
                                 mkexpr(t9),
                                 mkU32(0x00000100)),
                           mkU32(0x00000100)));
         /* Saturate if necessary. */
         assign(t11, IRExpr_ITE(mkexpr(t10),
                                mkU8(0xff),
                                unop(Iop_32to8, mkexpr(t9))));

         assign(t12,
                binop(Iop_Or32,
                      binop(Iop_Or32,
                            binop(Iop_Or32,
                                  unop(Iop_1Sto32, mkexpr(t10)),
                                  unop(Iop_1Sto32, mkexpr(t7))),
                            unop(Iop_1Sto32, mkexpr(t4))),
                      unop(Iop_1Sto32, mkexpr(t1))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        mkexpr(t12),
                                        mkU32(0x0)),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000))));

         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16, mkexpr(t11), mkexpr(t8)),
                       binop(Iop_8HLto16, mkexpr(t5), mkexpr(t2))));
         break;
      }

      case 0x05: {  /* SUBU_S.QB */
         DIP("subu_s.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);

         /* Use C function to easily calculate the result
            and write it in the register more conveniently
            Underflow is checked using step by step subtraction. */
         assign(t1, binop(Iop_QSub8Ux4, getIReg(rs), getIReg(rt)));

         /* Subtract each byte of rs and rt. */
         assign(t6,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t7,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t8,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t9,
                binop(Iop_Sub32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rt))))));

         /* Put 1 to bit 20 in DSPControl if there is underflow
            in either byte. */
         assign(t2, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t6),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         assign(t3, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t7),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         assign(t4, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t8),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         assign(t5, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                mkexpr(t9),
                                mkU32(0x00000100)),
                          mkU32(0x00000100)));
         putDSPControl(IRExpr_ITE(mkexpr(t5),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         putIReg(rd, mkexpr(t1));
         break;
      }

      case 0x6: {  /* MULEU_S.PH.QBL */
         DIP("muleu_s.ph.qbl r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);

         assign(t0,
                unop(Iop_64to32,
                     binop(Iop_MullU32,
                           unop(Iop_8Uto32,
                                unop(Iop_16HIto8,
                                     unop(Iop_32HIto16,
                                          getIReg(rs)))),
                           unop(Iop_16Uto32,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t1,
                unop(Iop_64to32,
                     binop(Iop_MullU32,
                           unop(Iop_8Uto32,
                                unop(Iop_16to8,
                                     unop(Iop_32HIto16,
                                          getIReg(rs)))),
                           unop(Iop_16Uto32,
                                unop(Iop_32to16, getIReg(rt))))));

         assign(t2, binop(Iop_CmpNE32,
                          mkU32(0x0),
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x03ff0000))));
         assign(t3, binop(Iop_CmpNE32,
                          mkU32(0x0),
                          binop(Iop_And32,
                                mkexpr(t1),
                                mkU32(0x03ff0000))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x200000)),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x200000)),
                                             getDSPControl())));
         putIReg(rd,
                 binop(Iop_16HLto32,
                       IRExpr_ITE(mkexpr(t2),
                                  mkU16(0xffff),
                                  unop(Iop_32to16, mkexpr(t0))),
                       IRExpr_ITE(mkexpr(t3),
                                  mkU16(0xffff),
                                  unop(Iop_32to16, mkexpr(t1)))));
         break;
      }

      case 0x7: {  /* MULEU_S.PH.QBR */
         DIP("muleu_s.ph.qbr r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);

         assign(t0, unop(Iop_64to32,
                         binop(Iop_MullU32,
                               unop(Iop_8Uto32,
                                    unop(Iop_16HIto8,
                                         unop(Iop_32to16,
                                              getIReg(rs)))),
                               unop(Iop_16Uto32,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t1, unop(Iop_64to32,
                         binop(Iop_MullU32,
                               unop(Iop_8Uto32,
                                    unop(Iop_16to8,
                                         unop(Iop_32to16,
                                              getIReg(rs)))),
                               unop(Iop_16Uto32,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));

         assign(t2, binop(Iop_CmpNE32,
                          mkU32(0x0),
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x03ff0000))));
         assign(t3, binop(Iop_CmpNE32,
                          mkU32(0x0),
                          binop(Iop_And32,
                                mkexpr(t1),
                                mkU32(0x03ff0000))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x200000)),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x200000)),
                                             getDSPControl())));
         putIReg(rd, binop(Iop_16HLto32,
                           IRExpr_ITE(mkexpr(t2),
                                      mkU16(0xffff),
                                      unop(Iop_32to16,
                                           mkexpr(t0))),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU16(0xffff),
                                      unop(Iop_32to16,
                                           mkexpr(t1)))));
         break;
      }

      case 0x08: {  /* ADDU.PH */
         DIP("addu.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);

         /* Add lower halves. */
         assign(t0, binop(Iop_Add32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Detect overflow. */
         assign(t1, binop(Iop_CmpLT32U,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, mkexpr(t0))),
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs)))));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         /* Add higher halves. */
         assign(t2, binop(Iop_Add32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Detect overflow. */
         assign(t3, binop(Iop_CmpLT32U,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, mkexpr(t2))),
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16,
                                    getIReg(rs)))));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, mkexpr(t2)),
                           unop(Iop_32to16, mkexpr(t0))));
         break;
      }

      case 0x9: {  /* SUBU.PH */
         DIP("subu.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);

         /* Substract lower halves. */
         assign(t0, binop(Iop_Sub32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Detect underflow. */
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x00010000)),
                          mkU32(0x0)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         /* Subtract higher halves. */
         assign(t2, binop(Iop_Sub32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Detect underflow. */
         assign(t3, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t2),
                                mkU32(0x00010000)),
                          mkU32(0x0)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, mkexpr(t2)),
                           unop(Iop_32to16, mkexpr(t0))));
         break;
      }

      case 0xA: {  /* ADDQ.PH */
         DIP("addq.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);

         /* Add lower halves. */
         assign(t0, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t6, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t0))),
                          mkU32(0x1)));
         /* Detect overflow. */
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t0),
                                      mkU32(0x8000)),
                                mkU8(15)),
                          mkexpr(t6)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         /* Add higher halves. */
         assign(t2, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t7, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0x1)));
         /* Detect overflow. */
         assign(t3, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t2),
                                      mkU32(0x00008000)),
                                mkU8(15)),
                          mkexpr(t7)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, mkexpr(t2)),
                           unop(Iop_32to16, mkexpr(t0))));
         break;
      }

      case 0xB: {  /* SUBQ.PH */
         DIP("subq.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);

         /* Subtract lower halves. */
         assign(t0, binop(Iop_Sub32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t6, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t0))),
                          mkU32(0x1)));
         /* Compare the signs of input value and the result. */
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t0),
                                      mkU32(0x8000)),
                                mkU8(15)),
                          mkexpr(t6)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         /* Subtract higher halves. */
         assign(t2, binop(Iop_Sub32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t7, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0x1)));
         /* Compare the signs of input value and the result. */
         assign(t3, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t2),
                                      mkU32(0x00008000)),
                                mkU8(15)),
                          mkexpr(t7)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, mkexpr(t2)),
                           unop(Iop_32to16, mkexpr(t0))));
         break;
      }

      case 0xC: {  /* ADDU_S.PH */
         DIP("addu_s.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);

         /* Add lower halves. */
         assign(t0, binop(Iop_Add32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Detect overflow. */
         assign(t1, binop(Iop_CmpLT32U,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, mkexpr(t0))),
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs)))));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         /* Add higher halves. */
         assign(t2, binop(Iop_Add32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Detect overflow. */
         assign(t3, binop(Iop_CmpLT32U,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, mkexpr(t2))),
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs)))));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, binop(Iop_16HLto32,
                           IRExpr_ITE(mkexpr(t3),
                                      mkU16(0xffff),
                                      unop(Iop_32to16,
                                           mkexpr(t2))),
                           IRExpr_ITE(mkexpr(t1),
                                      mkU16(0xffff),
                                      unop(Iop_32to16,
                                           mkexpr(t0)))));
         break;
      }

      case 0xD: {  /* SUBU_S.PH */
         DIP("subu_s.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);

         /* Subtract lower halves. */
         assign(t0, binop(Iop_Sub32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Detect underflow. */
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t0), mkU32(0x00010000)),
                          mkU32(0x0)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         /* Subtract higher halves. */
         assign(t2, binop(Iop_Sub32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Detect underflow. */
         assign(t3, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t2), mkU32(0x00010000)),
                          mkU32(0x0)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd,
                 binop(Iop_16HLto32,
                       IRExpr_ITE(mkexpr(t3),
                                  mkU16(0x0000),
                                  unop(Iop_32to16, mkexpr(t2))),
                       IRExpr_ITE(mkexpr(t1),
                                  mkU16(0x0000),
                                  unop(Iop_32to16, mkexpr(t0)))));
         break;
      }

      case 0xE: {  /* ADDQ_S.PH */
         DIP("addq_s.ph r%u r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I16);
         t5 = newTemp(Ity_I16);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);

         /* Add lower halves. */
         assign(t0, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t6, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t0))),
                          mkU32(0x1)));
         /* Detect overflow. */
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t0),
                                      mkU32(0x8000)),
                                mkU8(15)),
                          mkexpr(t6)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         /* Saturate if needed. */
         assign(t4, IRExpr_ITE(mkexpr(t1),
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                mkexpr(t6),
                                                mkU32(0x0)),
                                          mkU16(0x7fff),
                                          mkU16(0x8000)),
                               unop(Iop_32to16, mkexpr(t0))));

         /* Add higher halves. */
         assign(t2, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t7, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0x1)));
         /* Detect overflow. */
         assign(t3, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t2),
                                      mkU32(0x00008000)),
                                mkU8(15)),
                          mkexpr(t7)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         /* Saturate if needed. */
         assign(t5, IRExpr_ITE(mkexpr(t3),
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                mkexpr(t7),
                                                mkU32(0x0)),
                                          mkU16(0x7fff),
                                          mkU16(0x8000)),
                               unop(Iop_32to16, mkexpr(t2))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t5), mkexpr(t4)));
         break;
      }

      case 0xF: {  /* SUBQ_S.PH */
         DIP("subq_s.ph r%u r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I16);
         t5 = newTemp(Ity_I16);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);

         /* Subtract lower halves. */
         assign(t0, binop(Iop_Sub32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t6, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t0))),
                          mkU32(0x1)));
         /* Detect overflow or underflow. */
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t0),
                                      mkU32(0x8000)),
                                mkU8(15)),
                          mkexpr(t6)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         /* Saturate if needed. */
         assign(t4, IRExpr_ITE(mkexpr(t1),
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                mkexpr(t6),
                                                mkU32(0x0)),
                                          mkU16(0x7fff),
                                          mkU16(0x8000)),
                               unop(Iop_32to16, mkexpr(t0))));

         /* Subtract higher halves. */
         assign(t2, binop(Iop_Sub32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));

         /* Bit 16 of the result. */
         assign(t7, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0x1)));
         /* Detect overflow or underflow. */
         assign(t3, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      mkexpr(t2),
                                      mkU32(0x00008000)),
                                mkU8(15)),
                          mkexpr(t7)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         /* Saturate if needed. */
         assign(t5, IRExpr_ITE(mkexpr(t3),
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                mkexpr(t7),
                                                mkU32(0x0)),
                                          mkU16(0x7fff),
                                          mkU16(0x8000)),
                               unop(Iop_32to16, mkexpr(t2))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t5), mkexpr(t4)));
         break;
      }

      case 0x10: {  /* ADDSC */
         DIP("addsc r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I1);

         /* The carry bit result out of the addition operation is
            written to bit 13(the c field) of the DSPControl reg. */
         assign(t0, binop(Iop_Add64,
                          unop(Iop_32Uto64, getIReg(rs)),
                          unop(Iop_32Uto64, getIReg(rt))));

         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t0)),
                                mkU32(0x1)),
                          mkU32(0x1)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x2000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xffffdfff))));

         putIReg(rd, unop(Iop_64to32, mkexpr(t0)));
         break;
      }

      case 0x11: {  /* ADDWC */
         DIP("addwc r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I1);

         /* Get carry bit from DSPControl register. */
         assign(t0, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getDSPControl(),
                                mkU32(0x2000)),
                          mkU8(0xd)));
         assign(t1, binop(Iop_Add64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64,
                               binop(Iop_Add32,
                                     getIReg(rt),
                                     mkexpr(t0)))));

         /* Extract bits 32 and 31. */
         assign(t2, binop(Iop_And32,
                          unop(Iop_64HIto32, mkexpr(t1)),
                          mkU32(0x1)));
         assign(t3, binop(Iop_Shr32,
                          binop(Iop_And32,
                                unop(Iop_64to32, mkexpr(t1)),
                                mkU32(0x80000000)),
                          mkU8(31)));
         assign(t4, binop(Iop_CmpNE32, mkexpr(t2), mkexpr(t3)));

         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));
         putIReg(rd, unop(Iop_64to32, mkexpr(t1)));
         break;
      }

      case 0x12: {  /* MODSUB */
         DIP("modsub r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);

         /* decr_7..0 */
         assign(t0,
                unop(Iop_8Uto32,
                     unop(Iop_16to8,
                          unop(Iop_32to16, getIReg(rt)))));

         /* lastindex_15..0 */
         assign(t1,
                unop(Iop_16Uto32,
                     binop(Iop_8HLto16,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))),
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         /* temp_15..0 */
         assign(t2,
                IRExpr_ITE(binop(Iop_CmpEQ32,
                                 getIReg(rs),
                                 mkU32(0x00000000)),
                           mkexpr(t1),
                           binop(Iop_Sub32,
                                 getIReg(rs), mkexpr(t0))));
         putIReg(rd, mkexpr(t2));
         break;
      }

      case 0x14: {  /* RADDU.W.QB */
         DIP("raddu.w.qb r%u, r%u", rd, rs);
         vassert(!mode64);
         putIReg(rd, binop(Iop_Add32,
                           binop(Iop_Add32,
                                 unop(Iop_8Uto32,
                                      unop(Iop_16to8,
                                           unop(Iop_32to16,
                                                getIReg(rs)))),
                                 unop(Iop_8Uto32,
                                      unop(Iop_16HIto8,
                                           unop(Iop_32to16,
                                                getIReg(rs))))),
                           binop(Iop_Add32,
                                 unop(Iop_8Uto32,
                                      unop(Iop_16to8,
                                           unop(Iop_32HIto16,
                                                getIReg(rs)))),
                                 unop(Iop_8Uto32,
                                      unop(Iop_16HIto8,
                                           unop(Iop_32HIto16,
                                                getIReg(rs)))))));
         break;
      }

      case 0x16: {  /* ADDQ_S.W */
         DIP("addq_s.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);

         assign(t0, binop(Iop_Add64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64, getIReg(rt))));

         assign(t3, binop(Iop_And32,
                          unop(Iop_64HIto32, mkexpr(t0)),
                          mkU32(0x1)));
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t0)),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          mkexpr(t3)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 mkexpr(t3),
                                                 mkU32(0x0)),
                                           mkU32(0x7fffffff),
                                           mkU32(0x80000000)),
                                unop(Iop_64to32, mkexpr(t0))));
         break;
      }

      case 0x17: {  /* SUBQ_S.W */
         DIP("subq_s.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);

         assign(t0, binop(Iop_Sub64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64, getIReg(rt))));

         assign(t3, binop(Iop_And32,
                          unop(Iop_64HIto32, mkexpr(t0)),
                          mkU32(0x1)));
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t0)),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          mkexpr(t3)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00100000)),
                                  getDSPControl()));

         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 mkexpr(t3),
                                                 mkU32(0x0)),
                                           mkU32(0x7fffffff),
                                           mkU32(0x80000000)),
                                unop(Iop_64to32, mkexpr(t0))));
         break;
      }

      case 0x1C: {  /* MULEQ_S.W.PHL */
         DIP("muleq_s.w.phl r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I32);

         assign(t0,
                binop(Iop_Shl32,
                      binop(Iop_Mul32,
                            unop(Iop_16Sto32,
                                 unop(Iop_32HIto16, getIReg(rt))),
                            unop(Iop_16Sto32,
                                 unop(Iop_32HIto16, getIReg(rs)))),
                      mkU8(0x1)));
         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0xffff0000)),
                          mkU32(0x80000000)));
         assign(t2, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0xffff0000)),
                          mkU32(0x80000000)));
         assign(t3, IRExpr_ITE(mkexpr(t1),
                               IRExpr_ITE(mkexpr(t2),
                                          binop(Iop_Or32,
                                                getDSPControl(),
                                                mkU32(0x00200000)),
                                          getDSPControl()),
                               getDSPControl()));
         putDSPControl(mkexpr(t3));

         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x7fffffff),
                                           mkexpr(t0)),
                                mkexpr(t0)));
         break;
      }

      case 0x1D: {  /* MULEQ_S.W.PHR */
         DIP("muleq_s.w.phr r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);

         assign(t0,
                binop(Iop_Shl32,
                      binop(Iop_Mul32,
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, getIReg(rt))),
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, getIReg(rs)))),
                      mkU8(0x1)));
         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0xffff)),
                          mkU32(0x8000)));
         assign(t2, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0xffff)),
                          mkU32(0x8000)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  IRExpr_ITE(mkexpr(t2),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x7fffffff),
                                           mkexpr(t0)),
                                mkexpr(t0)));
         break;
      }

      case 0x1E: {  /* MULQ_S.PH */
         DIP("mulq_s.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I16);
         t3 = newTemp(Ity_I16);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t5,
                unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rs))));
         assign(t6,
                unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rt))));

         assign(t7,
                unop(Iop_16Sto32, unop(Iop_32HIto16, getIReg(rs))));
         assign(t8,
                unop(Iop_16Sto32, unop(Iop_32HIto16, getIReg(rt))));

         assign(t0, binop(Iop_And32,
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t5),
                                           mkU32(0xffff)),
                                     mkU32(0x8000))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t6),
                                           mkU32(0xffff)),
                                     mkU32(0x8000)))));
         assign(t1, binop(Iop_And32,
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t7),
                                           mkU32(0xffff)),
                                     mkU32(0x8000))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t8),
                                           mkU32(0xffff)),
                                     mkU32(0x8000)))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        binop(Iop_Or32,
                                              mkexpr(t0),
                                              mkexpr(t1)),
                                        mkU32(0x0)),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x200000))));

         assign(t2, unop(Iop_32HIto16,
                         binop(Iop_Shl32,
                               unop(Iop_64to32,
                                    binop(Iop_MullS32,
                                          mkexpr(t7),
                                          mkexpr(t8))),
                               mkU8(0x1))));
         assign(t3, unop(Iop_32HIto16,
                         binop(Iop_Shl32,
                               unop(Iop_64to32,
                                    binop(Iop_MullS32,
                                          mkexpr(t5),
                                          mkexpr(t6))),
                               mkU8(0x1))));
         putIReg(rd, binop(Iop_16HLto32,
                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                            mkexpr(t1),
                                            mkU32(0x0)),
                                      mkexpr(t2),
                                      mkU16(0x7fff)),
                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                            mkexpr(t0),
                                            mkU32(0x0)),
                                      mkexpr(t3),
                                      mkU16(0x7fff))));
         break;
      }

      case 0x1F: {  /* MULQ_RS.PH */
         DIP("mulq_rs.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I16);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I16);

         /* Multiply and round lower halfwords. */
         assign(t0, binop(Iop_Add32,
                          binop(Iop_Shl32,
                                binop(Iop_Mul32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16,
                                                getIReg(rt))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16,
                                                getIReg(rs)))),
                                mkU8(0x1)),
                          mkU32(0x00008000)));
         assign(t1, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rt), mkU32(0xffff)),
                          mkU32(0x8000)));
         assign(t2, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rs), mkU32(0xffff)),
                          mkU32(0x8000)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  IRExpr_ITE(mkexpr(t2),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));
         assign(t3, IRExpr_ITE(mkexpr(t1),
                               IRExpr_ITE(mkexpr(t2),
                                          mkU16(0x7fff),
                                          unop(Iop_32HIto16,
                                               mkexpr(t0))),
                               unop(Iop_32HIto16, mkexpr(t0))));

         /* Multiply and round higher halfwords. */
         assign(t4, binop(Iop_Add32,
                          binop(Iop_Shl32,
                                binop(Iop_Mul32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16,
                                                getIReg(rt))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16,
                                                getIReg(rs)))),
                                mkU8(0x1)),
                          mkU32(0x00008000)));
         assign(t5, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0xffff0000)),
                          mkU32(0x80000000)));
         assign(t6, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0xffff0000)),
                          mkU32(0x80000000)));
         putDSPControl(IRExpr_ITE(mkexpr(t5),
                                  IRExpr_ITE(mkexpr(t6),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)),
                                             getDSPControl()),
                                  getDSPControl()));
         assign(t7, IRExpr_ITE(mkexpr(t5),
                               IRExpr_ITE(mkexpr(t6),
                                          mkU16(0x7fff),
                                          unop(Iop_32HIto16,
                                               mkexpr(t4))),
                               unop(Iop_32HIto16, mkexpr(t4))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t7), mkexpr(t3)));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_CMPU_EQ_QB( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14,
              t15;
   UInt   rs, rt, rd, sa;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x0: {  /* CMPU.EQ.QB */
         DIP("cmpu.eq.qb r%u, r%u", rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);

         assign(t1,
                binop(Iop_CmpEQ32,
                      binop(Iop_And32, getIReg(rs), mkU32(0xff)),
                      binop(Iop_And32, getIReg(rt), mkU32(0xff))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));

         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x04000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfbffffff))));

         assign(t4, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x08000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xf7ffffff))));
         break;
      }

      case 0x1: {  /* CMPU.LT.QB */
         DIP("cmpu.lt.qb r%u, r%u", rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);

         assign(t1, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));

         assign(t3, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x04000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfbffffff))));

         assign(t4, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x08000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xf7ffffff))));
         break;
      }

      case 0x2: {  /* CMPU.LE.QB */
         DIP("cmpu.le.qb r%u, r%u", rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);

         assign(t1, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));

         assign(t3, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x04000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfbffffff))));

         assign(t4, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x08000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xf7ffffff))));
         break;
      }

      case 0x3: {  /* PICK.QB */
         DIP("pick.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I8);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I8);
         t4 = newTemp(Ity_I8);

         assign(t0, getDSPControl());
         assign(t1, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x01000000)),
                                     mkU32(0x0)),
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rs))),
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt)))));
         assign(t2, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x02000000)),
                                     mkU32(0x0)),
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs))),
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt)))));
         assign(t3, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x04000000)),
                                     mkU32(0x0)),
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs))),
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt)))));
         assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x08000000)),
                                     mkU32(0x0)),
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs))),
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt)))));
         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16, mkexpr(t4), mkexpr(t3)),
                       binop(Iop_8HLto16, mkexpr(t2), mkexpr(t1))));
         break;
      }

      case 0x4: {  /* CMPGU.EQ.QB */
         DIP("cmpgu.eq.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t1, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x00000001), mkU32(0)));

         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t6, IRExpr_ITE(mkexpr(t2),
                               mkU32(0x00000002), mkU32(0)));

         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t7, IRExpr_ITE(mkexpr(t3),
                               mkU32(0x00000004), mkU32(0)));

         assign(t4, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t8, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x00000008), mkU32(0)));

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32,
                                       mkexpr(t5), mkexpr(t6)),
                                 mkexpr(t7)),
                           mkexpr(t8)));
         break;
      }

      case 0x5: {  /* CMPGU.LT.QB */
         DIP("cmpgu.lt.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t1, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x00000001), mkU32(0)));

         assign(t2, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t6, IRExpr_ITE(mkexpr(t2),
                               mkU32(0x00000002), mkU32(0)));

         assign(t3, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t7, IRExpr_ITE(mkexpr(t3),
                               mkU32(0x00000004), mkU32(0)));

         assign(t4, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t8, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x00000008), mkU32(0)));
         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32,
                                       mkexpr(t5), mkexpr(t6)),
                                 mkexpr(t7)),
                           mkexpr(t8)));
         break;
      }

      case 0x6: {  /* CMPGU.LE.QB */
         DIP("cmpgu.le.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t1, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x00000001), mkU32(0)));

         assign(t2, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t6, IRExpr_ITE(mkexpr(t2),
                               mkU32(0x00000002), mkU32(0)));

         assign(t3, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t7, IRExpr_ITE(mkexpr(t3),
                               mkU32(0x00000004), mkU32(0)));

         assign(t4, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t8, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x00000008), mkU32(0)));
         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32,
                                       mkexpr(t5), mkexpr(t6)),
                                 mkexpr(t7)),
                           mkexpr(t8)));
         break;
      }

      case 0x8: {  /* CMP.EQ.PH */
         DIP("cmp.eq.ph r%u, r%u", rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);

         assign(t1, binop(Iop_CmpEQ16,
                          unop(Iop_32to16, getIReg(rs)),
                          unop(Iop_32to16, getIReg(rt))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));
         assign(t2, binop(Iop_CmpEQ16,
                          unop(Iop_32HIto16, getIReg(rs)),
                          unop(Iop_32HIto16, getIReg(rt))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));
         break;
      }

      case 0x9: {  /* CMP.LT.PH */
         DIP("cmp.lt.ph r%u, r%u", rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);

         assign(t1, binop(Iop_CmpLT32S,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpLT32S,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));
         break;
      }

      case 0xA: {  /* CMP.LE.PH */
         DIP("cmp.le.ph r%u, r%u", rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);

         assign(t1, binop(Iop_CmpLE32S,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpLE32S,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));
         break;
      }

      case 0xB: {  /* PICK.PH */
         DIP("pick.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I16);
         t2 = newTemp(Ity_I16);

         assign(t0, getDSPControl());

         assign(t1, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x01000000)),
                                     mkU32(0x0)),
                               unop(Iop_32to16, getIReg(rs)),
                               unop(Iop_32to16, getIReg(rt))));

         assign(t2, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x02000000)),
                                     mkU32(0x0)),
                               unop(Iop_32HIto16, getIReg(rs)),
                               unop(Iop_32HIto16, getIReg(rt))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t2), mkexpr(t1)));
         break;
      }

      case 0xC: {  /* PRECRQ.QB.PH */
         DIP("precrq.qb.ph r%u, r%u, %u", rd, rs, rt);
         vassert(!mode64);
         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16,
                             unop(Iop_16HIto8,
                                  unop(Iop_32HIto16, getIReg(rs))),
                             unop(Iop_16HIto8,
                                  unop(Iop_32to16, getIReg(rs)))),
                       binop(Iop_8HLto16,
                             unop(Iop_16HIto8,
                                  unop(Iop_32HIto16, getIReg(rt))),
                             unop(Iop_16HIto8,
                                  unop(Iop_32to16, getIReg(rt))))));
         break;
      }

      case 0xD: {  /* PRECR.QB.PH */
         DIP("precr.qb.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);

         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16,
                             unop(Iop_16to8,
                                  unop(Iop_32HIto16, getIReg(rs))),
                             unop(Iop_16to8,
                                  unop(Iop_32to16, getIReg(rs)))),
                       binop(Iop_8HLto16,
                             unop(Iop_16to8,
                                  unop(Iop_32HIto16, getIReg(rt))),
                             unop(Iop_16to8,
                                  unop(Iop_32to16, getIReg(rt))))));
         break;
      }

      case 0xF: {  /* PRECRQU_S.QB.PH */
         DIP("precrqu_s.qb.ph r%u, r%u, %u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I8);
         t1 = newTemp(Ity_I8);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I8);
         t4 = newTemp(Ity_I8);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I8);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I8);
         t11 = newTemp(Ity_I1);
         t12 = newTemp(Ity_I32);
         t13 = newTemp(Ity_I8);
         t14 = newTemp(Ity_I1);
         t15 = newTemp(Ity_I32);

         assign(t4, IRExpr_ITE(binop(Iop_CmpLT32U,
                                     mkU32(0x7f80),
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32to16,
                                                      getIReg(rs))),
                                           mkU32(0x7fff))),
                               mkU8(0xff),
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         binop(Iop_Shl32,
                                               getIReg(rs),
                                               mkU8(1))))));
         assign(t0, IRExpr_ITE(binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32to16,
                                                      getIReg(rs))),
                                           mkU32(0x00008000)),
                                     mkU32(0x0)),
                               mkexpr(t4),
                               mkU8(0x0)));
         assign(t5, binop(Iop_And32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16,
                                    getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t6, binop(Iop_CmpLT32U,
                          mkU32(0x7f80),
                          binop(Iop_And32,
                                unop(Iop_16Uto32,
                                     unop(Iop_32to16,
                                          getIReg(rs))),
                                mkU32(0x7fff))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        mkexpr(t5),
                                        mkU32(0x0)),
                                  IRExpr_ITE(mkexpr(t6),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00400000)
                                                  ),
                                             getDSPControl()),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00400000))));

         assign(t7, IRExpr_ITE(binop(Iop_CmpLT32U,
                                     mkU32(0x7f80),
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                           mkU32(0x7fff))),
                               mkU8(0xff),
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         binop(Iop_Shl32,
                                               getIReg(rs),
                                               mkU8(1))))));
         assign(t1, IRExpr_ITE(binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                           mkU32(0x00008000)),
                                     mkU32(0x0)),
                               mkexpr(t7),
                               mkU8(0x0)));
         assign(t8, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                unop(Iop_16Uto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rs))),
                                mkU32(0x00008000)),
                          mkU32(0x0)));
         assign(t9, IRExpr_ITE(binop(Iop_CmpLT32U,
                                     mkU32(0x7f80),
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                           mkU32(0x7fff))),
                               binop(Iop_Or32,
                                     getDSPControl(),
                                     mkU32(0x00400000)),
                               getDSPControl()));
         putDSPControl(IRExpr_ITE(mkexpr(t8),
                                  mkexpr(t9),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00400000))));

         assign(t10, IRExpr_ITE(binop(Iop_CmpLT32U,
                                      mkU32(0x7f80),
                                      binop(Iop_And32,
                                            unop(Iop_16Uto32,
                                                 unop(Iop_32to16,
                                                       getIReg(rt))),
                                            mkU32(0x7fff))),
                                mkU8(0xff),
                                unop(Iop_16HIto8,
                                     unop(Iop_32to16,
                                          binop(Iop_Shl32,
                                                getIReg(rt),
                                                mkU8(1))))));
         assign(t2, IRExpr_ITE(binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32to16,
                                                      getIReg(rt))),
                                           mkU32(0x00008000)),
                                     mkU32(0x0)),
                               mkexpr(t10),
                               mkU8(0x0)));
         assign(t11, binop(Iop_CmpEQ32,
                           binop(Iop_And32,
                                 unop(Iop_16Uto32,
                                      unop(Iop_32to16,
                                           getIReg(rt))),
                                 mkU32(0x00008000)),
                           mkU32(0x0)));
         assign(t12, IRExpr_ITE(binop(Iop_CmpLT32U,
                                      mkU32(0x7f80),
                                      binop(Iop_And32,
                                            unop(Iop_16Uto32,
                                                 unop(Iop_32to16,
                                                       getIReg(rt))),
                                            mkU32(0x7fff))),
                                binop(Iop_Or32,
                                      getDSPControl(),
                                      mkU32(0x00400000)),
                                getDSPControl()));
         putDSPControl(IRExpr_ITE(mkexpr(t11),
                                  mkexpr(t12),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00400000))));

         assign(t13, IRExpr_ITE(binop(Iop_CmpLT32U,
                                      mkU32(0x7f80),
                                      binop(Iop_And32,
                                            unop(Iop_16Uto32,
                                                 unop(Iop_32HIto16,
                                                       getIReg(rt))),
                                            mkU32(0x7fff))),
                                mkU8(0xff),
                                unop(Iop_16HIto8,
                                     unop(Iop_32HIto16,
                                          binop(Iop_Shl32,
                                                getIReg(rt),
                                                mkU8(1))))));
         assign(t3, IRExpr_ITE(binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           unop(Iop_16Uto32,
                                                unop(Iop_32HIto16,
                                                      getIReg(rt))),
                                           mkU32(0x00008000)),
                                     mkU32(0x0)),
                               mkexpr(t13),
                               mkU8(0x0)));
         assign(t14, binop(Iop_CmpEQ32,
                           binop(Iop_And32,
                                 unop(Iop_16Uto32,
                                      unop(Iop_32HIto16,
                                           getIReg(rt))),
                                 mkU32(0x00008000)),
                           mkU32(0x0)));
         assign(t15, IRExpr_ITE(binop(Iop_CmpLT32U,
                                      mkU32(0x7f80),
                                      binop(Iop_And32,
                                            unop(Iop_16Uto32,
                                                 unop(Iop_32HIto16,
                                                       getIReg(rt))),
                                            mkU32(0x7fff))),
                                binop(Iop_Or32,
                                      getDSPControl(),
                                      mkU32(0x00400000)),
                                getDSPControl()));
         putDSPControl(IRExpr_ITE(mkexpr(t14),
                                  mkexpr(t15),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00400000))));

         putIReg(rd, binop(Iop_16HLto32,
                           binop(Iop_8HLto16,
                                 mkexpr(t1), mkexpr(t0)),
                           binop(Iop_8HLto16,
                                 mkexpr(t3), mkexpr(t2))));
         break;
      }

      case 0x14: {  /* PRECRQ.PH.W */
         DIP("precrq.ph.w r%u, r%u, %u", rd, rs, rt);
         vassert(!mode64);
         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32HIto16, getIReg(rs)),
                           unop(Iop_32HIto16, getIReg(rt))));
         break;
      }

      case 0x15: {  /* PRECRQ_RS.PH.W */
         DIP("precrq_rs.ph.w r%u, r%u, %u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);

         assign(t0, binop(Iop_Add64,
                          binop(Iop_32HLto64,
                                binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x80000000)),
                                      mkU8(31)),
                                getIReg(rs)),
                          mkU64(0x0000000000008000ULL)));
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t0)),
                                mkU32(0x1)),
                          binop(Iop_And32,
                                binop(Iop_Shr32,
                                      unop(Iop_64to32, mkexpr(t0)),
                                      mkU8(31)),
                                mkU32(0x1))));
         assign(t2, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x7fffffff),
                               unop(Iop_64to32, mkexpr(t0))));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  getDSPControl()));
         assign(t3, binop(Iop_Add64,
                          binop(Iop_32HLto64,
                                binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0x80000000)),
                                      mkU8(31)),
                                getIReg(rt)),
                          mkU64(0x0000000000008000ULL)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t3)),
                                mkU32(0x1)),
                          binop(Iop_And32,
                                binop(Iop_Shr32,
                                      unop(Iop_64to32, mkexpr(t3)),
                                      mkU8(31)),
                                mkU32(0x1))));
         assign(t5, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x7fffffff),
                               unop(Iop_64to32, mkexpr(t3))));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  getDSPControl()));
         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32HIto16, mkexpr(t2)),
                           unop(Iop_32HIto16, mkexpr(t5))));
         break;
      }

      case 0x1E: {  /* PRECR_SRA.PH.W */
         DIP("precr_sra.ph.w r%u, r%u, %u", rt, rs, rd);
         vassert(!mode64);

         if (0 == rd) {
            putIReg(rt, binop(Iop_16HLto32,
                              unop(Iop_32to16, getIReg(rt)),
                              unop(Iop_32to16, getIReg(rs))));
         } else {
            putIReg(rt, binop(Iop_16HLto32,
                              unop(Iop_32to16, binop(Iop_Sar32,
                                    getIReg(rt),
                                    mkU8(rd))),
                              unop(Iop_32to16, binop(Iop_Sar32,
                                    getIReg(rs),
                                    mkU8(rd)))));
         }

         break;
      }

      case 0x1F: {  /* PRECR_SRA_R.PH.W */
         DIP("precr_sra_r.ph.w r%u, r%u, %u", rt, rs, rd);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);

         if (0 == rd) {
            putIReg(rt, binop(Iop_16HLto32,
                              unop(Iop_32to16, getIReg(rt)),
                              unop(Iop_32to16, getIReg(rs))));
         } else {
            assign(t0, binop(Iop_Shr32,
                             binop(Iop_Add32,
                                   binop(Iop_Sar32,
                                         getIReg(rt),
                                         mkU8(rd - 1)),
                                   mkU32(0x1)),
                             mkU8(0x1)));
            assign(t1, binop(Iop_Shr32,
                             binop(Iop_Add32,
                                   binop(Iop_Sar32,
                                         getIReg(rs),
                                         mkU8(rd - 1)),
                                   mkU32(0x1)),
                             mkU8(0x1)));
            putIReg(rt, binop(Iop_16HLto32,
                              unop(Iop_32to16, mkexpr(t0)),
                              unop(Iop_32to16, mkexpr(t1))));
         };

         break;
      }

      case 0xE: {  /* PACKRL.PH */
         DIP("packrl.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, getIReg(rs)),
                           unop(Iop_32HIto16, getIReg(rt))));
         break;
      }

      case 0x18: {  /* CMPGDU.EQ.QB */
         DIP("cmpgdu.eq.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t1,
                binop(Iop_CmpEQ32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x00000001), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t6, IRExpr_ITE(mkexpr(t2),
                               mkU32(0x00000002), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));

         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t7, IRExpr_ITE(mkexpr(t3),
                               mkU32(0x00000004), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x04000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfbffffff))));

         assign(t4, binop(Iop_CmpEQ32,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t8, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x00000008), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x08000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xf7ffffff))));

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32,
                                       mkexpr(t5), mkexpr(t6)),
                                 mkexpr(t7)),
                           mkexpr(t8)));
         break;
      }

      case 0x19: {  /* CMPGDU.LT.QB */
         DIP("cmpgdu.lt.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t1, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x00000001), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t6, IRExpr_ITE(mkexpr(t2),
                               mkU32(0x00000002), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));

         assign(t3, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t7, IRExpr_ITE(mkexpr(t3),
                               mkU32(0x00000004), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x04000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfbffffff))));

         assign(t4, binop(Iop_CmpLT32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t8, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x00000008), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x08000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xf7ffffff))));

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32,
                                       mkexpr(t5), mkexpr(t6)),
                                 mkexpr(t7)),
                           mkexpr(t8)));
         break;
      }

      case 0x1A: {  /* CMPGDU.LE.QB */
         DIP("cmpgdu.le.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         assign(t1, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               mkU32(0x00000001),
                               mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x01000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfeffffff))));

         assign(t2, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16, getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32to16,
                                         getIReg(rt))))));
         assign(t6, IRExpr_ITE(mkexpr(t2),
                               mkU32(0x00000002), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x02000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfdffffff))));

         assign(t3, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16to8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t7, IRExpr_ITE(mkexpr(t3),
                               mkU32(0x00000004), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x04000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xfbffffff))));

         assign(t4, binop(Iop_CmpLE32U,
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rs)))),
                          unop(Iop_8Uto32,
                               unop(Iop_16HIto8,
                                    unop(Iop_32HIto16,
                                         getIReg(rt))))));
         assign(t8, IRExpr_ITE(mkexpr(t4),
                               mkU32(0x00000008), mkU32(0)));
         putDSPControl(IRExpr_ITE(mkexpr(t4),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x08000000)),
                                  binop(Iop_And32,
                                        getDSPControl(),
                                        mkU32(0xf7ffffff))));

         putIReg(rd, binop(Iop_Or32,
                           binop(Iop_Or32,
                                 binop(Iop_Or32,
                                       mkexpr(t5), mkexpr(t6)),
                                 mkexpr(t7)),
                           mkexpr(t8)));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_SHLL_QB( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14,
              t15, t16, t17;
   UInt   rs, rt, rd, sa;


   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x0: {  /* SHLL.QB */
         DIP("shll.qb r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I1);
         t10 = newTemp(Ity_I1);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            /* Shift bits 7..0 and 23..16. */
            assign(t0, binop(Iop_Shl32,
                             binop(Iop_And32,
                                   getIReg(rt),
                                   mkU32(0x00ff00ff)),
                             mkU8(rs)));
            assign(t1, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t0),
                                   mkU32(0xff000000)),
                             mkU32(0x00000000)));
            assign(t2, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t0),
                                   mkU32(0xff000000)),
                             mkU32(0xff000000)));
            assign(t7, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t0),
                                   mkU32(0x0000ff00)),
                             mkU32(0x00000000)));
            assign(t8, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t0),
                                   mkU32(0x0000ff00)),
                             mkU32(0x000ff00)));
            /* Shift bits 15..8 and 31..24. */
            assign(t3, binop(Iop_Shl32,
                             binop(Iop_Shr32,
                                   binop(Iop_And32,
                                         getIReg(rt),
                                         mkU32(0xff00ff00)),
                                   mkU8(8)),
                             mkU8(rs)));
            assign(t4, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t3),
                                   mkU32(0xff000000)),
                             mkU32(0x00000000)));
            assign(t5, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t3),
                                   mkU32(0xff000000)),
                             mkU32(0xff000000)));
            assign(t9, binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   mkexpr(t3),
                                   mkU32(0x0000ff00)),
                             mkU32(0x00000000)));
            assign(t10, binop(Iop_CmpNE32,
                              binop(Iop_And32,
                                    mkexpr(t3),
                                    mkU32(0x0000ff00)),
                              mkU32(0x0000ff00)));

            assign(t6, binop(Iop_Or32,
                             binop(Iop_Or32,
                                   binop(Iop_And32,
                                         unop(Iop_1Uto32,
                                              mkexpr(t1)),
                                         unop(Iop_1Uto32,
                                              mkexpr(t2))),
                                   binop(Iop_And32,
                                         unop(Iop_1Uto32,
                                              mkexpr(t7)),
                                         unop(Iop_1Uto32,
                                              mkexpr(t8)))),
                             binop(Iop_Or32,
                                   binop(Iop_And32,
                                         unop(Iop_1Uto32,
                                              mkexpr(t4)),
                                         unop(Iop_1Uto32,
                                              mkexpr(t5))),
                                   binop(Iop_And32,
                                         unop(Iop_1Uto32,
                                              mkexpr(t9)),
                                         unop(Iop_1Uto32,
                                              mkexpr(t10))))));

            putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                           mkexpr(t6),
                                           mkU32(0x0)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            putIReg(rd, binop(Iop_Or32,
                              binop(Iop_Shl32,
                                    binop(Iop_And32,
                                          mkexpr(t3),
                                          mkU32(0x00ff00ff)),
                                    mkU8(8)),
                              binop(Iop_And32,
                                    mkexpr(t0),
                                    mkU32(0x00ff00ff))));
         }

         break;
      }

      case 0x3: {  /* SHRL.QB */
         DIP("shrl.qb r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I8);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I8);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I8);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I8);
         t9 = newTemp(Ity_I32);

         assign(t9, binop(Iop_And32, getIReg(rs), mkU32(0x7)));
         assign(t0, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t1, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               mkexpr(t0),
                               unop(Iop_32to8, mkexpr(t9)))));

         assign(t2, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t3, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               mkexpr(t2),
                               unop(Iop_32to8, mkexpr(t9)))));

         assign(t4, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32HIto16, getIReg(rt)))));
         assign(t5, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               mkexpr(t4),
                               unop(Iop_32to8, mkexpr(t9)))));

         assign(t6, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32HIto16, getIReg(rt)))));
         assign(t7, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               mkexpr(t6),
                               unop(Iop_32to8, mkexpr(t9)))));
         putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      mkexpr(t9),
                                      mkU32(0x0)),
                                getIReg(rt),
                                binop(Iop_16HLto32,
                                      binop(Iop_8HLto16,
                                            mkexpr(t7),
                                            mkexpr(t5)),
                                      binop(Iop_8HLto16,
                                            mkexpr(t3),
                                            mkexpr(t1)))));
         break;
      }

      case 0x2: {  /* SHLLV.QB */
         DIP("shllv.qb r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I1);
         t10 = newTemp(Ity_I1);
         t11 = newTemp(Ity_I8);

         assign(t11, unop(Iop_32to8,
                          binop(Iop_And32,
                                getIReg(rs),
                                mkU32(0x7))));
         /* Shift bits 7..0 and 23..16. */
         assign(t0, binop(Iop_Shl32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0x00ff00ff)),
                          mkexpr(t11)));
         assign(t1, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0xff000000)),
                          mkU32(0x00000000)));
         assign(t2, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0xff000000)),
                          mkU32(0xff000000)));
         assign(t7, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x0000ff00)),
                          mkU32(0x00000000)));
         assign(t8, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t0),
                                mkU32(0x0000ff00)),
                          mkU32(0x000ff00)));
         /* Shift bits 15..8 and 31..24. */
         assign(t3, binop(Iop_Shl32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      getIReg(rt),
                                      mkU32(0xff00ff00)),
                                mkU8(8)),
                          mkexpr(t11)));
         assign(t4, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t3),
                                mkU32(0xff000000)),
                          mkU32(0x00000000)));
         assign(t5, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t3),
                                mkU32(0xff000000)),
                          mkU32(0xff000000)));
         assign(t9, binop(Iop_CmpNE32,
                          binop(Iop_And32,
                                mkexpr(t3),
                                mkU32(0x0000ff00)),
                          mkU32(0x00000000)));
         assign(t10, binop(Iop_CmpNE32,
                           binop(Iop_And32,
                                 mkexpr(t3),
                                 mkU32(0x0000ff00)),
                           mkU32(0x0000ff00)));

         assign(t6, binop(Iop_Or32,
                          binop(Iop_Or32,
                                binop(Iop_And32,
                                      unop(Iop_1Uto32,
                                           mkexpr(t1)),
                                      unop(Iop_1Uto32,
                                           mkexpr(t2))),
                                binop(Iop_And32,
                                      unop(Iop_1Uto32,
                                           mkexpr(t7)),
                                      unop(Iop_1Uto32,
                                           mkexpr(t8)))),
                          binop(Iop_Or32,
                                binop(Iop_And32,
                                      unop(Iop_1Uto32,
                                           mkexpr(t4)),
                                      unop(Iop_1Uto32,
                                           mkexpr(t5))),
                                binop(Iop_And32,
                                      unop(Iop_1Uto32,
                                           mkexpr(t9)),
                                      unop(Iop_1Uto32,
                                           mkexpr(t10))))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t6),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  getDSPControl()));
         putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32, mkexpr(t11)),
                                      mkU32(0)),
                                getIReg(rt),
                                binop(Iop_Or32,
                                      binop(Iop_Shl32,
                                            binop(Iop_And32,
                                                  mkexpr(t3),
                                                  mkU32(0xff00ff)),
                                            mkU8(8)),
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x00ff00ff)))));
         break;
      }

      case 0x1: {  /* SHRLV.QB */
         DIP("shrlv.qb r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I8);
         t1 = newTemp(Ity_I8);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I8);

         assign(t0, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               unop(Iop_8Uto32,
                                    unop(Iop_32to8, getIReg(rt))),
                               mkU8(rs))));
         assign(t1, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               unop(Iop_8Uto32,
                                    unop(Iop_16HIto8,
                                         unop(Iop_32to16,
                                              getIReg(rt)))),
                               mkU8(rs))));
         assign(t2, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               unop(Iop_8Uto32,
                                    unop(Iop_16to8,
                                         unop(Iop_32HIto16,
                                              getIReg(rt)))),
                               mkU8(rs))));
         assign(t3, unop(Iop_32to8,
                         binop(Iop_Shr32,
                               unop(Iop_8Uto32,
                                    unop(Iop_16HIto8,
                                         unop(Iop_32HIto16,
                                              getIReg(rt)))),
                               mkU8(rs))));
         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16, mkexpr(t3), mkexpr(t2)),
                       binop(Iop_8HLto16, mkexpr(t1), mkexpr(t0))));
         break;
      }

      case 0x4: {  /* SHRA.QB */
         DIP("shra.qb r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I32);
         t11 = newTemp(Ity_I32);

         /* ========== GPR[rt]_31..24 ========== */
         assign(t1,
                unop(Iop_8Uto32,
                     unop(Iop_16HIto8,
                          unop(Iop_32HIto16, getIReg(rt)))));
         assign(t2,
                binop(Iop_Shr32, mkexpr(t1), mkU8(rs)));
         /* tempD_7..0 */
         assign(t0,
                binop(Iop_Or32,
                      mkexpr(t2),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t1),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

         /* ========== GPR[rt]_23..16 ========== */
         assign(t4,
                unop(Iop_8Uto32,
                     unop(Iop_16to8,
                          unop(Iop_32HIto16, getIReg(rt)))));
         assign(t5, binop(Iop_Shr32, mkexpr(t4), mkU8(rs)));
         /* tempC_7..0 */
         assign(t3,
                binop(Iop_Or32,
                      mkexpr(t5),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t4),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

         /* ========== GPR[rt]_15..8 ========== */
         assign(t7,
                unop(Iop_8Uto32,
                     unop(Iop_16HIto8,
                          unop(Iop_32to16, getIReg(rt)))));
         assign(t8, binop(Iop_Shr32, mkexpr(t7), mkU8(rs)));
         /* tempB_7..0 */
         assign(t6,
                binop(Iop_Or32,
                      mkexpr(t8),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t7),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

         /* ========== GPR[rt]_7..0 ========== */
         assign(t10,
                unop(Iop_8Uto32,
                     unop(Iop_16to8,
                          unop(Iop_32to16, getIReg(rt)))));
         assign(t11, binop(Iop_Shr32, mkexpr(t10), mkU8(rs)));
         /* tempB_7..0 */
         assign(t9,
                binop(Iop_Or32,
                      mkexpr(t11),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t10),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16,
                             unop(Iop_32to8, mkexpr(t0)),
                             unop(Iop_32to8, mkexpr(t3))),
                       binop(Iop_8HLto16,
                             unop(Iop_32to8, mkexpr(t6)),
                             unop(Iop_32to8, mkexpr(t9)))));
         break;
      }

      case 0x5: {  /* SHRA_R.QB */
         DIP("shra_r.qb r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I8);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I8);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I8);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I8);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            assign(t0, unop(Iop_8Sto32,
                            unop(Iop_16to8,
                                 unop(Iop_32to16, getIReg(rt)))));
            assign(t1, unop(Iop_32to8,
                            binop(Iop_Sar32,
                                  binop(Iop_Add32,
                                        mkexpr(t0),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(rs - 1))),
                                  mkU8(rs))));

            assign(t2, unop(Iop_8Sto32,
                            unop(Iop_16HIto8,
                                 unop(Iop_32to16, getIReg(rt)))));
            assign(t3, unop(Iop_32to8,
                            binop(Iop_Sar32,
                                  binop(Iop_Add32,
                                        mkexpr(t2),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(rs - 1))),
                                  mkU8(rs))));

            assign(t4, unop(Iop_8Sto32,
                            unop(Iop_16to8,
                                 unop(Iop_32HIto16, getIReg(rt)))));
            assign(t5, unop(Iop_32to8,
                            binop(Iop_Sar32,
                                  binop(Iop_Add32,
                                        mkexpr(t4),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(rs - 1))),
                                  mkU8(rs))));

            assign(t6, unop(Iop_8Sto32,
                            unop(Iop_16HIto8,
                                 unop(Iop_32HIto16, getIReg(rt)))));
            assign(t7, unop(Iop_32to8,
                            binop(Iop_Sar32,
                                  binop(Iop_Add32,
                                        mkexpr(t6),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(rs - 1))),
                                  mkU8(rs))));
            putIReg(rd, binop(Iop_16HLto32,
                              binop(Iop_8HLto16,
                                    mkexpr(t7), mkexpr(t5)),
                              binop(Iop_8HLto16,
                                    mkexpr(t3), mkexpr(t1))));
         }

         break;
      }

      case 0x6: {  /* SHRAV.QB */
         DIP("shrav.qb r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);

         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);

         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I32);
         t11 = newTemp(Ity_I32);

         /* ========== GPR[rt]_31..24 ========== */
         assign(t1,
                unop(Iop_8Uto32,
                     unop(Iop_16HIto8,
                          unop(Iop_32HIto16, getIReg(rt)))));
         assign(t2,
                binop(Iop_Shr32,
                      mkexpr(t1),
                      unop(Iop_32to8, binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x7)))));
         /* tempD_7..0 */
         assign(t0,
                binop(Iop_Or32,
                      mkexpr(t2),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t1),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8,
                                  mkU8(0x8),
                                  unop(Iop_32to8, binop(Iop_And32,
                                        getIReg(rs),
                                        mkU32(0x7)))
                                 ))));

         /* ========== GPR[rt]_23..16 ========== */
         assign(t4,
                unop(Iop_8Uto32,
                     unop(Iop_16to8,
                          unop(Iop_32HIto16, getIReg(rt)))));
         assign(t5,
                binop(Iop_Shr32,
                      mkexpr(t4),
                      unop(Iop_32to8, binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x7)))));
         /* tempC_7..0 */
         assign(t3,
                binop(Iop_Or32,
                      mkexpr(t5),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t4),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8,
                                  mkU8(0x8),
                                  unop(Iop_32to8, binop(Iop_And32,
                                        getIReg(rs),
                                        mkU32(0x7)))
                                 ))));

         /* ========== GPR[rt]_15..8 ========== */
         assign(t7,
                unop(Iop_8Uto32,
                     unop(Iop_16HIto8,
                          unop(Iop_32to16, getIReg(rt)))));
         assign(t8,
                binop(Iop_Shr32,
                      mkexpr(t7),
                      unop(Iop_32to8, binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x7)))));
         /* tempB_7..0 */
         assign(t6,
                binop(Iop_Or32,
                      mkexpr(t8),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t7),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8,
                                  mkU8(0x8),
                                  unop(Iop_32to8, binop(Iop_And32,
                                        getIReg(rs),
                                        mkU32(0x7)))
                                 ))));

         /* ========== GPR[rt]_7..0 ========== */
         assign(t10,
                unop(Iop_8Uto32,
                     unop(Iop_16to8,
                          unop(Iop_32to16, getIReg(rt)))));
         assign(t11,
                binop(Iop_Shr32,
                      mkexpr(t10),
                      unop(Iop_32to8, binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x7)))));
         /* tempB_7..0 */
         assign(t9,
                binop(Iop_Or32,
                      mkexpr(t11),
                      binop(Iop_Shl32,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkexpr(t10),
                                                   mkU32(0x00000080)
                                                  ),
                                             mkU32(0x00000080)),
                                       mkU32(0xFFFFFFFF),
                                       mkU32(0x00000000)),
                            binop(Iop_Sub8,
                                  mkU8(0x8),
                                  unop(Iop_32to8, binop(Iop_And32,
                                        getIReg(rs),
                                        mkU32(0x7)))
                                 ))));

         putIReg(rd,
                 binop(Iop_16HLto32,
                       binop(Iop_8HLto16,
                             unop(Iop_32to8,
                                  IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkU32(rs),
                                                   mkU32(0x7)
                                                  ),
                                             mkU32(0x0)),
                                             mkexpr(t1),
                                             mkexpr(t0))),
                             unop(Iop_32to8,
                                  IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkU32(rs),
                                                   mkU32(0x7)
                                                  ),
                                             mkU32(0x0)),
                                             mkexpr(t2),
                                             mkexpr(t3)))),
                       binop(Iop_8HLto16,
                             unop(Iop_32to8,
                                  IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkU32(rs),
                                                   mkU32(0x7)
                                                  ),
                                             mkU32(0x0)),
                                             mkexpr(t5),
                                             mkexpr(t6))),
                             unop(Iop_32to8,
                                  IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   mkU32(rs),
                                                   mkU32(0x7)
                                                  ),
                                             mkU32(0x0)),
                                             mkexpr(t8),
                                             mkexpr(t9))))));
         break;
      }

      case 0x7: {  /* SHRAV_R.QB */
         DIP("shrav_r.qb r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I8);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I8);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I8);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I8);
         t8 = newTemp(Ity_I8);
         t9 = newTemp(Ity_I32);

         assign(t9, binop(Iop_And32, getIReg(rs), mkU32(0x7)));
         assign(t8, unop(Iop_32to8,
                         binop(Iop_Sub32, mkexpr(t9), mkU32(0x1))));
         assign(t0, unop(Iop_8Sto32,
                         unop(Iop_16to8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t1, unop(Iop_32to8,
                         binop(Iop_Sar32,
                               binop(Iop_Add32,
                                     mkexpr(t0),
                                     binop(Iop_Shl32,
                                           mkU32(0x1),
                                           mkexpr(t8))),
                               unop(Iop_32to8,
                                    mkexpr(t9)))));

         assign(t2, unop(Iop_8Sto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t3, unop(Iop_32to8,
                         binop(Iop_Sar32,
                               binop(Iop_Add32,
                                     mkexpr(t2),
                                     binop(Iop_Shl32,
                                           mkU32(0x1),
                                           mkexpr(t8))),
                               unop(Iop_32to8, mkexpr(t9)))));

         assign(t4, unop(Iop_8Sto32,
                         unop(Iop_16to8,
                              unop(Iop_32HIto16, getIReg(rt)))));
         assign(t5, unop(Iop_32to8,
                         binop(Iop_Sar32,
                               binop(Iop_Add32,
                                     mkexpr(t4),
                                     binop(Iop_Shl32,
                                           mkU32(0x1),
                                           mkexpr(t8))),
                               unop(Iop_32to8, mkexpr(t9)))));

         assign(t6, unop(Iop_8Sto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32HIto16, getIReg(rt)))));
         assign(t7, unop(Iop_32to8,
                         binop(Iop_Sar32,
                               binop(Iop_Add32,
                                     mkexpr(t6),
                                     binop(Iop_Shl32,
                                           mkU32(0x1),
                                           mkexpr(t8))),
                               unop(Iop_32to8, mkexpr(t9)))));
         putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      mkexpr(t9),
                                      mkU32(0x0)),
                                getIReg(rt),
                                binop(Iop_16HLto32,
                                      binop(Iop_8HLto16,
                                            mkexpr(t7),
                                            mkexpr(t5)),
                                      binop(Iop_8HLto16,
                                            mkexpr(t3),
                                            mkexpr(t1)))));
         break;
      }

      case 0x8: {  /* SHLL.PH */
         DIP("shll.ph r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            /* Shift lower 16 bits. */
            assign(t0, binop(Iop_Shl32,
                             unop(Iop_16Sto32,
                                  unop(Iop_32to16, getIReg(rt))),
                             mkU8(rs)));

            assign(t1, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t0),
                                        mkU8(16)),
                                  mkU32(0))));
            assign(t2, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t0),
                                        mkU8(16)),
                                  mkU32(0xffffffff))));
            assign(t3, binop(Iop_And32,
                             mkexpr(t1),
                             mkexpr(t2)));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           mkexpr(t3),
                                           mkU32(0x1)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x00008000)),
                                           binop(Iop_And32,
                                                 mkexpr(t0),
                                                 mkU32(0x00008000))
                                          ),
                                     getDSPControl(),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000))));
            /* Shift higher 16 bits. */
            assign(t4, binop(Iop_Shl32,
                             unop(Iop_16Sto32,
                                  unop(Iop_32HIto16, getIReg(rt))),
                             mkU8(rs)));

            assign(t5, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t4),
                                        mkU8(16)),
                                  mkU32(0))));
            assign(t6, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t4),
                                        mkU8(16)),
                                  mkU32(0xffffffff))));
            assign(t7, binop(Iop_And32,
                             mkexpr(t5),
                             mkexpr(t6)));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           mkexpr(t7),
                                           mkU32(0x1)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           mkexpr(t7),
                                           mkU32(0x1)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x80000000)),
                                           binop(Iop_Shl32,
                                                 binop(Iop_And32,
                                                       mkexpr(t4),
                                                       mkU32(0x00008000)),
                                                 mkU8(16))
                                          ),
                                     getDSPControl(),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000))));
            putIReg(rd, binop(Iop_16HLto32,
                              unop(Iop_32to16, mkexpr(t4)),
                              unop(Iop_32to16, mkexpr(t0))));
         }

         break;
      }

      case 0x9: {  /* SHRA.PH */
         DIP("shra.ph r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            assign(t0, binop(Iop_Sar32,
                             unop(Iop_16Sto32,
                                  unop(Iop_32to16, getIReg(rt))),
                             mkU8(rs)));
            assign(t1, binop(Iop_Sar32,
                             unop(Iop_16Sto32,
                                  unop(Iop_32HIto16, getIReg(rt))),
                             mkU8(rs)));
            putIReg(rd, binop(Iop_16HLto32,
                              unop(Iop_32to16, mkexpr(t1)),
                              unop(Iop_32to16, mkexpr(t0))));
         }

         break;
      }

      case 0xA: {  /* SHLLV.PH */
         DIP("shllv.ph r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I32);
         t11 = newTemp(Ity_I32);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);

         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));

         /* Shift lower 16 bits. */
         assign(t2, binop(Iop_Shl32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));

         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0x00000000)));
         assign(t4, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0xffffffff)));
         assign(t10, binop(Iop_And32,
                           unop(Iop_1Sto32, mkexpr(t3)),
                           unop(Iop_1Sto32, mkexpr(t4))));
         assign(t5, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0x00008000)),
                          mkU8(15)));
         assign(t12, binop(Iop_CmpEQ32,
                           mkexpr(t5),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       mkexpr(t2),
                                       mkU32(0x00008000)),
                                 mkU8(15))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t10),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  IRExpr_ITE(mkexpr(t12),
                                             getDSPControl(),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x400000)))
                                 ));
         /* Shift higher 16 bits. */
         assign(t6, binop(Iop_Shl32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));

         assign(t7, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t6))),
                          mkU32(0x00000000)));
         assign(t8, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t6))),
                          mkU32(0xffffffff)));
         assign(t11, binop(Iop_And32,
                           unop(Iop_1Sto32, mkexpr(t7)),
                           unop(Iop_1Sto32, mkexpr(t8))));

         assign(t9, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0x80000000)),
                          mkU8(31)));
         assign(t13, binop(Iop_CmpEQ32,
                           mkexpr(t9),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       mkexpr(t6),
                                       mkU32(0x00008000)),
                                 mkU8(15))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t11),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  IRExpr_ITE(mkexpr(t13),
                                             getDSPControl(),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x400000)))
                                 ));

         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, mkexpr(t6)),
                           unop(Iop_32to16, mkexpr(t2))));
         break;
      }

      case 0xB: {  /* SHRAV.PH */
         DIP("shrav.ph r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);

         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));
         assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));
         assign(t2, binop(Iop_Sar32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));
         assign(t3, binop(Iop_Sar32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));
         putIReg(rd,
                 binop(Iop_16HLto32,
                       IRExpr_ITE(mkexpr(t1),
                                  unop(Iop_32HIto16, getIReg(rt)),
                                  unop(Iop_32to16, mkexpr(t3))),
                       IRExpr_ITE(mkexpr(t1),
                                  unop(Iop_32to16, getIReg(rt)),
                                  unop(Iop_32to16, mkexpr(t2)))));
         break;
      }

      case 0xC: {  /* SHLL_S.PH */
         DIP("shll_s.ph r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I32);
         t11 = newTemp(Ity_I32);
         t12 = newTemp(Ity_I32);
         t13 = newTemp(Ity_I32);
         t14 = newTemp(Ity_I32);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            /* Shift lower 16 bits. */
            assign(t0, binop(Iop_Shl32,
                             unop(Iop_16Sto32,
                                  unop(Iop_32to16, getIReg(rt))),
                             mkU8(rs)));

            assign(t1, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t0),
                                        mkU8(16)),
                                  mkU32(0))));
            assign(t2, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t0),
                                        mkU8(16)),
                                  mkU32(0xffffffff))));
            assign(t3, binop(Iop_And32,
                             mkexpr(t1),
                             mkexpr(t2)));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           mkexpr(t3),
                                           mkU32(0x1)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x00008000)),
                                           binop(Iop_And32,
                                                 mkexpr(t0),
                                                 mkU32(0x00008000))
                                          ),
                                     getDSPControl(),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000))));
            assign(t8,
                   IRExpr_ITE(binop(Iop_CmpEQ32,
                                    mkexpr(t3),
                                    mkU32(0x1)),
                              IRExpr_ITE(binop(Iop_CmpEQ32,
                                               binop(Iop_And32,
                                                     getIReg(rt),
                                                     mkU32(0x8000)),
                                               mkU32(0)),
                                         mkU32(0x00007fff),
                                         mkU32(0x00008000)),
                              binop(Iop_And32,
                                    mkexpr(t0),
                                    mkU32(0x0000ffff))));
            assign(t10,
                   IRExpr_ITE(binop(Iop_CmpEQ32,
                                    binop(Iop_And32,
                                          getIReg(rt),
                                          mkU32(0x00008000)),
                                    binop(Iop_And32,
                                          mkexpr(t0),
                                          mkU32(0x00008000))),
                              mkexpr(t8),
                              IRExpr_ITE(binop(Iop_CmpEQ32,
                                               binop(Iop_And32,
                                                     getIReg(rt),
                                                     mkU32(0x8000)),
                                               mkU32(0)),
                                         mkU32(0x00007fff),
                                         mkU32(0x00008000))));
            /* Shift higher 16 bits. */
            assign(t4, binop(Iop_Shl32,
                             unop(Iop_16Sto32,
                                  unop(Iop_32HIto16, getIReg(rt))),
                             mkU8(rs)));

            assign(t5, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t4),
                                        mkU8(16)),
                                  mkU32(0))));
            assign(t6, unop(Iop_1Uto32,
                            binop(Iop_CmpNE32,
                                  binop(Iop_Sar32,
                                        mkexpr(t4),
                                        mkU8(16)),
                                  mkU32(0xffffffff))));
            assign(t7, binop(Iop_And32,
                             mkexpr(t5),
                             mkexpr(t6)));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           mkexpr(t7),
                                           mkU32(0x1)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           mkexpr(t7),
                                           mkU32(0x1)),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000)),
                                     getDSPControl()));
            assign(t12, binop(Iop_Shl32,
                              binop(Iop_And32,
                                    mkexpr(t4),
                                    mkU32(0x8000)),
                              mkU8(16)));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x80000000)),
                                           mkexpr(t12)),
                                     getDSPControl(),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000))));
            assign(t13, IRExpr_ITE(binop(Iop_CmpEQ32,
                                         binop(Iop_And32,
                                               getIReg(rt),
                                               mkU32(0x80000000)),
                                         mkU32(0)),
                                   mkU32(0x7fff0000),
                                   mkU32(0x80000000)));
            assign(t9,
                   IRExpr_ITE(binop(Iop_CmpEQ32,
                                    mkexpr(t7),
                                    mkU32(0x1)),
                              mkexpr(t13),
                              binop(Iop_Shl32,
                                    binop(Iop_And32,
                                          mkexpr(t4),
                                          mkU32(0x0000ffff)),
                                    mkU8(16))));
            assign(t14, IRExpr_ITE(binop(Iop_CmpEQ32,
                                         binop(Iop_And32,
                                               getIReg(rt),
                                               mkU32(0x80000000)),
                                         mkU32(0)),
                                   mkU32(0x7fff0000),
                                   mkU32(0x80000000)));
            assign(t11,
                   IRExpr_ITE(binop(Iop_CmpEQ32,
                                    binop(Iop_And32,
                                          getIReg(rt),
                                          mkU32(0x80000000)),
                                    binop(Iop_Shl32,
                                          binop(Iop_And32,
                                                mkexpr(t4),
                                                mkU32(0x00008000)),
                                          mkU8(16))),
                              mkexpr(t9),
                              mkexpr(t14)));
            putIReg(rd, binop(Iop_Or32,
                              mkexpr(t10),
                              mkexpr(t11)));
         }

         break;
      }

      case 0xD: {  /* SHRA_R.PH */
         DIP("shra.ph r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            assign(t0, binop(Iop_Sar32,
                             binop(Iop_Add32,
                                   unop(Iop_16Sto32,
                                        unop(Iop_32to16,
                                             getIReg(rt))),
                                   binop(Iop_Shl32,
                                         mkU32(0x1),
                                         mkU8(rs - 1))),
                             mkU8(rs)));
            assign(t1, binop(Iop_Sar32,
                             binop(Iop_Add32,
                                   unop(Iop_16Sto32,
                                        unop(Iop_32HIto16,
                                             getIReg(rt))),
                                   binop(Iop_Shl32,
                                         mkU32(0x1),
                                         mkU8(rs - 1))),
                             mkU8(rs)));
            putIReg(rd, binop(Iop_16HLto32,
                              unop(Iop_32to16, mkexpr(t1)),
                              unop(Iop_32to16, mkexpr(t0))));
         }

         break;
      }

      case 0xE: {  /* SHLLV_S.PH */
         DIP("shllv_s.ph r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I32);
         t11 = newTemp(Ity_I32);
         t12 = newTemp(Ity_I1);
         t13 = newTemp(Ity_I1);
         t14 = newTemp(Ity_I16);
         t15 = newTemp(Ity_I16);
         t16 = newTemp(Ity_I16);
         t17 = newTemp(Ity_I16);

         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));

         /* Shift lower 16 bits. */
         assign(t2, binop(Iop_Shl32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));

         assign(t3, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0x00000000)));
         assign(t4, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t2))),
                          mkU32(0xffffffff)));
         assign(t10, binop(Iop_And32,
                           unop(Iop_1Sto32, mkexpr(t3)),
                           unop(Iop_1Sto32, mkexpr(t4))));
         assign(t5, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0x00008000)),
                          mkU8(15)));
         assign(t12, binop(Iop_CmpEQ32,
                           mkexpr(t5),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       mkexpr(t2),
                                       mkU32(0x00008000)),
                                 mkU8(15))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t10),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  IRExpr_ITE(mkexpr(t12),
                                             getDSPControl(),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x400000)))
                                 ));
         assign(t14, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkexpr(t5),
                                      mkU32(0x0)),
                                mkU16(0x8000),
                                mkU16(0x7fff)));
         assign(t15, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkexpr(t10),
                                      mkU32(0x0)),
                                mkexpr(t14),
                                IRExpr_ITE(mkexpr(t12),
                                           unop(Iop_32to16,
                                                mkexpr(t2)),
                                           mkexpr(t14))));
         /* Shift higher 16 bits. */
         assign(t6, binop(Iop_Shl32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));

         assign(t7, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t6))),
                          mkU32(0x00000000)));
         assign(t8, binop(Iop_CmpNE32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, mkexpr(t6))),
                          mkU32(0xffffffff)));
         assign(t11, binop(Iop_And32,
                           unop(Iop_1Sto32, mkexpr(t7)),
                           unop(Iop_1Sto32, mkexpr(t8))));

         assign(t9, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0x80000000)),
                          mkU8(31)));
         assign(t13, binop(Iop_CmpEQ32,
                           mkexpr(t9),
                           binop(Iop_Shr32,
                                 binop(Iop_And32,
                                       mkexpr(t6),
                                       mkU32(0x00008000)),
                                 mkU8(15))));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t11),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  IRExpr_ITE(mkexpr(t13),
                                             getDSPControl(),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x400000)))
                                 ));

         assign(t16, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkexpr(t9),
                                      mkU32(0x0)),
                                mkU16(0x8000),
                                mkU16(0x7fff)));
         assign(t17, IRExpr_ITE(binop(Iop_CmpNE32,
                                      mkexpr(t11),
                                      mkU32(0x0)),
                                mkexpr(t16),
                                IRExpr_ITE(mkexpr(t13),
                                           unop(Iop_32to16,
                                                mkexpr(t6)),
                                           mkexpr(t16))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t17), mkexpr(t15)));
         break;
      }

      case 0xF: {  /* SHRAV_R.PH */
         DIP("shrav_r.ph r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);

         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));
         assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));
         assign(t2, unop(Iop_32to8,
                         binop(Iop_Sub32, mkexpr(t0), mkU32(1))));

         assign(t3, binop(Iop_Sar32,
                          binop(Iop_Add32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rt))),
                                binop(Iop_Shl32,
                                      mkU32(0x1),
                                      mkexpr(t2))),
                          unop(Iop_32to8, mkexpr(t0))));
         assign(t4, binop(Iop_Sar32,
                          binop(Iop_Add32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rt))),
                                binop(Iop_Shl32,
                                      mkU32(0x1),
                                      mkexpr(t2))),
                          unop(Iop_32to8, mkexpr(t0))));

         putIReg(rd, binop(Iop_16HLto32,
                           IRExpr_ITE(mkexpr(t1),
                                      unop(Iop_32HIto16,
                                           getIReg(rt)),
                                      unop(Iop_32to16,
                                           mkexpr(t4))),
                           IRExpr_ITE(mkexpr(t1),
                                      unop(Iop_32to16, getIReg(rt)),
                                      unop(Iop_32to16,
                                           mkexpr(t3)))));
         break;
      }

      case 0x14: {  /* SHLL_S.W */
         DIP("shll_s.w r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            /* t0-bits that will be discarded, sign extended to
               32bits. */
            assign(t0, binop(Iop_Sar32,
                             binop(Iop_And32,
                                   getIReg(rt),
                                   binop(Iop_Sar32,
                                         mkU32(0x80000000),
                                         mkU8(rs - 1))),
                             mkU8(32 - rs)));

            assign(t1, IRExpr_ITE(binop(Iop_CmpEQ32,
                                        binop(Iop_And32,
                                              getIReg(rt),
                                              mkU32(0x80000000)),
                                        mkU32(0x0)),
                                  mkU32(0x7fffffff),
                                  mkU32(0x80000000)));

            assign(t2, binop(Iop_Shl32, getIReg(rt), mkU8(rs)));
            assign(t3, IRExpr_ITE(binop(Iop_CmpEQ32,
                                        binop(Iop_And32,
                                              getIReg(rt),
                                              mkU32(0x80000000)),
                                        binop(Iop_And32,
                                              mkexpr(t2),
                                              mkU32(0x80000000))),
                                  mkexpr(t2),
                                  mkexpr(t1)));

            assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t0),
                                        mkU32(0x0)),
                                  IRExpr_ITE(binop(Iop_CmpNE32,
                                                   mkexpr(t0),
                                                   mkU32(0xffffffff)
                                                  ),
                                             mkexpr(t1),
                                             mkexpr(t3)),
                                  mkexpr(t3)));
            assign(t5, IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t0),
                                        mkU32(0xffffffff)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                           mkexpr(t0),
                                           mkU32(0x0)),
                                     mkexpr(t5),
                                     getDSPControl()));
            putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x80000000)),
                                           binop(Iop_And32,
                                                 mkexpr(t2),
                                                 mkU32(0x80000000))
                                          ),
                                     getDSPControl(),
                                     binop(Iop_Or32,
                                           getDSPControl(),
                                           mkU32(0x400000))));
            putIReg(rd, mkexpr(t4));
         }

         break;
      }

      case 0x15: {  /* SHRA_R.W */
         DIP("shra_r.w r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);

         if (0 == rs) {
            putIReg(rd, getIReg(rt));
         } else {
            putIReg(rd, binop(Iop_Add32,
                              binop(Iop_Sar32,
                                    getIReg(rt), mkU8(rs)),
                              binop(Iop_Shr32,
                                    binop(Iop_And32,
                                          getIReg(rt),
                                          binop(Iop_Shl32,
                                                mkU32(0x1),
                                                mkU8(rs - 1))),
                                    mkU8(rs - 1))));
         }

         break;
      }

      case 0x16: {  /* SHLLV_S.W */
         DIP("shllv_s.w r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I1);
         t5 = newTemp(Ity_I1);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I32);

         /* Check if shift amount is zero. */
         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
         assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));

         /* t2 = sign of the input value. */
         assign(t2, binop(Iop_Shr32,
                          binop(Iop_And32,
                                getIReg(rt),
                                mkU32(0x80000000)),
                          mkU8(31)));
         /* Shift left input value and check for overflow. */
         assign(t3, binop(Iop_Shl64,
                          unop(Iop_32Sto64, getIReg(rt)),
                          unop(Iop_32to8, mkexpr(t0))));
         assign(t4, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32, mkexpr(t3)),
                          mkU32(0x00000000)));
         assign(t5, binop(Iop_CmpNE32,
                          unop(Iop_64HIto32, mkexpr(t3)),
                          mkU32(0xffffffff)));
         assign(t6, binop(Iop_And32,
                          unop(Iop_1Uto32, mkexpr(t4)),
                          unop(Iop_1Uto32, mkexpr(t5))));
         assign(t7, binop(Iop_CmpEQ32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      getIReg(rt),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t3)),
                                      mkU32(0x80000000)),
                                mkU8(31))));

         putDSPControl(IRExpr_ITE(unop(Iop_32to1, mkexpr(t6)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x400000)),
                                  IRExpr_ITE(mkexpr(t7),
                                             getDSPControl(),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x400000)))
                                 ));

         assign(t8, IRExpr_ITE(unop(Iop_32to1,
                                    mkexpr(t2)),
                               mkU32(0x80000000),
                               mkU32(0x7fffffff)));
         putIReg(rd, IRExpr_ITE(unop(Iop_32to1, mkexpr(t6)),
                                IRExpr_ITE(unop(Iop_32to1,
                                                mkexpr(t2)),
                                           mkU32(0x80000000),
                                           mkU32(0x7fffffff)),
                                IRExpr_ITE(mkexpr(t7),
                                           unop(Iop_64to32,
                                                mkexpr(t3)),
                                           mkexpr(t8))));
         break;
      }

      case 0x17: {  /* SHRAV_R.W */
         DIP("shrav_r.w r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I32);

         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
         assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));
         assign(t2, unop(Iop_32to8,
                         binop(Iop_Sub32, mkexpr(t0), mkU32(1))));

         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                getIReg(rt),
                                binop(Iop_Sar32,
                                      binop(Iop_Add32,
                                            binop(Iop_Sar32,
                                                  getIReg(rt),
                                                  mkexpr(t2)),
                                            mkU32(0x1)),
                                      mkU8(1))));
         break;
      }

      case 0x19: {  /* SHRL.PH */
         DIP("shrl.ph r%u, r%u, %u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         assign(t0, binop(Iop_Shr32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU8(rs)));
         assign(t1, binop(Iop_Shr32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU8(rs)));
         putIReg(rd, binop(Iop_16HLto32,
                           unop(Iop_32to16, mkexpr(t1)),
                           unop(Iop_32to16, mkexpr(t0))));
         break;
      }

      case 0x1B: {  /* SHRLV.PH */
         DIP("shrlv.ph r%u, r%u, r%u", rd, rt, rs);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I16);
         t5 = newTemp(Ity_I16);

         /* Get shift amount from lower 5 bits of rs
            and check if it is zero. */
         assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));
         assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));

         assign(t2, binop(Iop_Shr32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));
         assign(t3, binop(Iop_Shr32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          unop(Iop_32to8, mkexpr(t0))));

         assign(t4, IRExpr_ITE(mkexpr(t1),
                               unop(Iop_32HIto16, getIReg(rt)),
                               unop(Iop_32to16, mkexpr(t3))));
         assign(t5, IRExpr_ITE(mkexpr(t1),
                               unop(Iop_32to16, getIReg(rt)),
                               unop(Iop_32to16, mkexpr(t2))));
         putIReg(rd, binop(Iop_16HLto32, mkexpr(t4), mkexpr(t5)));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_ADDUH_QB( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12;
   UInt   rs, rt, rd, sa;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x00: {  /* ADDUH.QB */
         DIP("adduh.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);

         assign(t0, binop(Iop_HAdd8Ux4, getIReg(rs), getIReg(rt)));

         putIReg(rd, mkexpr(t0));
         break;
      }

      case 0x1: {  /* SUBUH.QB */
         DIP("subuh.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);

         assign(t0, binop(Iop_HSub8Ux4, getIReg(rs), getIReg(rt)));

         putIReg(rd, mkexpr(t0));
         break;
      }

      case 0x02: {  /* ADDUH_R.QB */
         DIP("adduh_r.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I8);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I8);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I8);
         t9 = newTemp(Ity_I32);
         t10 = newTemp(Ity_I32);
         t11 = newTemp(Ity_I8);

         /* Extract input bytes, add values, add 1 and half the
            result. */
         assign(t0, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32to16, getIReg(rs)))));
         assign(t1, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t2, unop(Iop_16to8,
                         unop(Iop_32to16,
                              binop(Iop_Shr32,
                                    binop(Iop_Add32,
                                          binop(Iop_Add32,
                                                mkexpr(t0),
                                                mkexpr(t1)),
                                          mkU32(0x00000001)),
                                    mkU8(0x01)))));

         assign(t3, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32to16, getIReg(rs)))));
         assign(t4, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t5, unop(Iop_16to8,
                         unop(Iop_32to16,
                              binop(Iop_Shr32,
                                    binop(Iop_Add32,
                                          binop(Iop_Add32,
                                                mkexpr(t3),
                                                mkexpr(t4)),
                                          mkU32(0x00000001)),
                                    mkU8(0x01)))));

         assign(t6, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32HIto16, getIReg(rs)))));
         assign(t7, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32HIto16, getIReg(rt)))));
         assign(t8, unop(Iop_16to8,
                         unop(Iop_32to16,
                              binop(Iop_Shr32,
                                    binop(Iop_Add32,
                                          binop(Iop_Add32,
                                                mkexpr(t7),
                                                mkexpr(t6)),
                                          mkU32(0x00000001)),
                                    mkU8(0x01)))));

         assign(t9, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32HIto16, getIReg(rs)))));
         assign(t10, unop(Iop_8Uto32,
                          unop(Iop_16HIto8,
                               unop(Iop_32HIto16, getIReg(rt)))));
         assign(t11, unop(Iop_16to8,
                          unop(Iop_32to16,
                               binop(Iop_Shr32,
                                     binop(Iop_Add32,
                                           binop(Iop_Add32,
                                                 mkexpr(t9),
                                                 mkexpr(t10)),
                                           mkU32(0x00000001)),
                                     mkU8(0x01)))));

         putIReg(rd, binop(Iop_16HLto32,
                           binop(Iop_8HLto16,
                                 mkexpr(t11), mkexpr(t8)),
                           binop(Iop_8HLto16,
                                 mkexpr(t5), mkexpr(t2))));
         break;
      }

      case 0x3: {  /* SUBUH_R.QB */
         DIP("subuh_r.qb r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I8);
         t10 = newTemp(Ity_I8);
         t11 = newTemp(Ity_I8);
         t12 = newTemp(Ity_I8);

         /* Extract each byte of rs and rt. */
         assign(t1, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32to16, getIReg(rs)))));
         assign(t2, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32to16, getIReg(rs)))));
         assign(t3, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32HIto16, getIReg(rs)))));
         assign(t4, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32HIto16, getIReg(rs)))));

         assign(t5, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t6, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32to16, getIReg(rt)))));
         assign(t7, unop(Iop_8Uto32,
                         unop(Iop_16to8,
                              unop(Iop_32HIto16, getIReg(rt)))));
         assign(t8, unop(Iop_8Uto32,
                         unop(Iop_16HIto8,
                              unop(Iop_32HIto16, getIReg(rt)))));

         /* Add 1 to each resulting byte and half the results. */
         assign(t9, unop(Iop_16to8,
                         unop(Iop_32to16,
                              binop(Iop_Shr32,
                                    binop(Iop_Add32,
                                          binop(Iop_Sub32,
                                                mkexpr(t1),
                                                mkexpr(t5)),
                                          mkU32(0x00000001)),
                                    mkU8(0x01)))));
         assign(t10, unop(Iop_16to8,
                          unop(Iop_32to16,
                               binop(Iop_Shr32,
                                     binop(Iop_Add32,
                                           binop(Iop_Sub32,
                                                 mkexpr(t2),
                                                 mkexpr(t6)),
                                           mkU32(0x00000001)),
                                     mkU8(0x01)))));
         assign(t11, unop(Iop_16to8,
                          unop(Iop_32to16,
                               binop(Iop_Shr32,
                                     binop(Iop_Add32,
                                           binop(Iop_Sub32,
                                                 mkexpr(t3),
                                                 mkexpr(t7)),
                                           mkU32(0x00000001)),
                                     mkU8(0x01)))));
         assign(t12, unop(Iop_16to8,
                          unop(Iop_32to16,
                               binop(Iop_Shr32,
                                     binop(Iop_Add32,
                                           binop(Iop_Sub32,
                                                 mkexpr(t4),
                                                 mkexpr(t8)),
                                           mkU32(0x00000001)),
                                     mkU8(0x01)))));

         putIReg(rd, binop(Iop_16HLto32,
                           binop(Iop_8HLto16,
                                 mkexpr(t12), mkexpr(t11)),
                           binop(Iop_8HLto16,
                                 mkexpr(t10), mkexpr(t9))));
         break;
      }

      case 0x8: {  /* ADDQH.PH */
         DIP("addqh.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I16);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I16);

         /* Add lower halfs of rs and rt
            and right shift the result by 1. */
         assign(t0, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));
         assign(t1, unop(Iop_32to16,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     mkexpr(t0),
                                     mkU32(0x0001fffe)),
                               mkU8(0x1))));
         /* Add higher halfs of rs and rt
            and right shift the result by 1. */
         assign(t2, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));
         assign(t3, unop(Iop_32to16,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     mkexpr(t2),
                                     mkU32(0x0001fffe)),
                               mkU8(0x1))));
         putIReg(rd, binop(Iop_16HLto32, mkexpr(t3), mkexpr(t1)));
         break;
      }

      case 0x9: {  /* SUBQH.PH */
         DIP("subqh.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);

         putIReg(rd, binop(Iop_HSub16Sx2,
                           getIReg(rs), getIReg(rt)));
         break;
      }

      case 0xA: {/* ADDQH_R.PH */
         DIP("addqh_r.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I16);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I16);

         /* Add lower halfs of rs and rt, add 1
            and right shift the result by 1. */
         assign(t0, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));
         assign(t1, unop(Iop_32to16,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     binop(Iop_Add32,
                                           mkexpr(t0),
                                           mkU32(0x1)),
                                     mkU32(0x0001fffe)),
                               mkU8(0x1))));
         /* Add higher halfs of rs and rt, add 1
            and right shift the result by 1. */
         assign(t2, binop(Iop_Add32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));
         assign(t3, unop(Iop_32to16,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     binop(Iop_Add32,
                                           mkexpr(t2),
                                           mkU32(0x1)),
                                     mkU32(0x0001fffe)),
                               mkU8(0x1))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t3), mkexpr(t1)));
         break;
      }

      case 0xB: {  /* SUBQH_R.PH */
         DIP("subqh_r.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I16);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I16);

         /* Sub lower halfs of rs and rt, add 1
            and right shift the result by 1. */
         assign(t0, binop(Iop_Sub32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));
         assign(t1, unop(Iop_32to16,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     binop(Iop_Add32,
                                           mkexpr(t0),
                                           mkU32(0x1)),
                                     mkU32(0x0001fffe)),
                               mkU8(0x1))));
         /* Sub higher halfs of rs and rt, add 1
            and right shift the result by 1. */
         assign(t2, binop(Iop_Sub32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt)))));
         assign(t3, unop(Iop_32to16,
                         binop(Iop_Shr32,
                               binop(Iop_And32,
                                     binop(Iop_Add32,
                                           mkexpr(t2),
                                           mkU32(0x1)),
                                     mkU32(0x0001fffe)),
                               mkU8(0x1))));

         putIReg(rd, binop(Iop_16HLto32, mkexpr(t3), mkexpr(t1)));
         break;
      }

      case 0xC: {  /* MUL.PH */
         DIP("mul.ph r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);

         assign(t0,
                binop(Iop_Mul32,
                      unop(Iop_16Sto32,
                           unop(Iop_32HIto16, getIReg(rs))),
                      unop(Iop_16Sto32,
                           unop(Iop_32HIto16, getIReg(rt)))));
         /* DSP Control flag. */
         putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                       binop(Iop_CmpLE32S,
                                             mkexpr(t0),
                                             mkU32(0x7FFF))),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00200000)),
                                  IRExpr_ITE(binop(Iop_CmpLT32S,
                                             mkexpr(t0),
                                             mkU32(0xFFFF8000)
                                                  ),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl())));

         assign(t1,
                binop(Iop_Mul32,
                      unop(Iop_16Sto32,
                           unop(Iop_32to16, getIReg(rs))),
                      unop(Iop_16Sto32,
                           unop(Iop_32to16, getIReg(rt)))));
         /* DSP Control flag. */
         putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                       binop(Iop_CmpLE32S,
                                             mkexpr(t1),
                                             mkU32(0x7FFF))),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00200000)),
                                  IRExpr_ITE(binop(Iop_CmpLT32S,
                                             mkexpr(t1),
                                             mkU32(0xFFFF8000)
                                                  ),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl())));

         assign(t2, binop(Iop_16HLto32,
                          unop(Iop_32to16, mkexpr(t0)),
                          unop(Iop_32to16, mkexpr(t1))));
         putIReg(rd, mkexpr(t2));
         break;
      }

      case 0xE: {  /* MUL_S.PH */
         DIP("mul_s.ph r%u r%u, r%u", rd, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);

         /* t0 - signed intermediate result. */
         assign(t0,
                binop(Iop_Mul32,
                      unop(Iop_16Sto32,
                           unop(Iop_32HIto16, getIReg(rs))),
                      unop(Iop_16Sto32,
                           unop(Iop_32HIto16, getIReg(rt)))));

         assign(t1,
                IRExpr_ITE(unop(Iop_Not1,
                                binop(Iop_CmpLE32S,
                                      mkexpr(t0),
                                      mkU32(0x7FFF))),
                           mkU32(0x00007FFF),
                           IRExpr_ITE(binop(Iop_CmpLT32S,
                                            mkexpr(t0),
                                            mkU32(0xFFFF8000)),
                                      mkU32(0xFFFF8000),
                                      mkexpr(t0))));

         /* DSP Control flag. */
         putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                       binop(Iop_CmpLE32S,
                                             mkexpr(t0),
                                             mkU32(0x7FFF))),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00200000)),
                                  IRExpr_ITE(binop(Iop_CmpLT32S,
                                             mkexpr(t0),
                                             mkU32(0xFFFF8000)
                                                  ),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl())));

         /* t2 - signed intermediate result. */
         assign(t2, binop(Iop_Mul32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt)))));

         assign(t3, IRExpr_ITE(unop(Iop_Not1,
                                    binop(Iop_CmpLE32S,
                                          mkexpr(t2),
                                          mkU32(0x7FFF))),
                               mkU32(0x00007FFF),
                               IRExpr_ITE(binop(Iop_CmpLT32S,
                                                mkexpr(t2),
                                                mkU32(0xFFFF8000)),
                                          mkU32(0xFFFF8000),
                                          mkexpr(t2))));

         /* DSP Control flag. */
         putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                       binop(Iop_CmpLE32S,
                                             mkexpr(t2),
                                             mkU32(0x7FFF))),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        mkU32(0x00200000)),
                                  IRExpr_ITE(binop(Iop_CmpLT32S,
                                             mkexpr(t2),
                                             mkU32(0xFFFF8000)
                                                  ),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl())));

         assign(t4, binop(Iop_16HLto32,
                          unop(Iop_32to16, mkexpr(t1)),
                          unop(Iop_32to16, mkexpr(t3))));
         putIReg(rd, mkexpr(t4));
         break;
      }

      case 0x10: {  /* ADDQH.W */
         DIP("addqh.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);

         assign(t0, binop(Iop_Add64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64, getIReg(rt))));
         assign(t1, binop(Iop_And64,
                          mkexpr(t0),
                          mkU64(0x00000001fffffffeULL)));
         putIReg(rd, unop(Iop_64to32,
                          binop(Iop_Shr64, mkexpr(t1), mkU8(0x1))));
         break;
      }

      case 0x11: {  /* SUBQH.W */
         DIP("subqh.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);

         assign(t0, binop(Iop_Sub64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64, getIReg(rt))));
         assign(t1, binop(Iop_And64,
                          mkexpr(t0),
                          mkU64(0x00000001fffffffeULL)));
         putIReg(rd, unop(Iop_64to32,
                          binop(Iop_Shr64, mkexpr(t1), mkU8(0x1))));
         break;
      }

      case 0x12: {  /* ADDQH_R.W */
         DIP("addqh_r.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);

         assign(t0, binop(Iop_Add64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64, getIReg(rt))));
         assign(t1, binop(Iop_Add64,
                          mkexpr(t0),
                          mkU64(0x0000000000000001ULL)));
         assign(t2, binop(Iop_And64,
                          mkexpr(t1),
                          mkU64(0x00000001fffffffeULL)));
         putIReg(rd, unop(Iop_64to32,
                          binop(Iop_Shr64, mkexpr(t2), mkU8(0x1))));
         break;
      }

      case 0x13: {  /* SUBQH_R.W */
         DIP("subqh_r.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);

         assign(t0, binop(Iop_Sub64,
                          unop(Iop_32Sto64, getIReg(rs)),
                          unop(Iop_32Sto64, getIReg(rt))));
         assign(t1, binop(Iop_Add64,
                          mkexpr(t0),
                          mkU64(0x0000000000000001ULL)));
         assign(t2, binop(Iop_And64,
                          mkexpr(t1),
                          mkU64(0x00000001fffffffeULL)));
         putIReg(rd, unop(Iop_64to32,
                          binop(Iop_Shr64, mkexpr(t2), mkU8(0x1))));
         break;
      }

      case 0x16: {  /* MULQ_S.W */
         DIP("mulq_s.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);

         assign(t0, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                getIReg(rt), getIReg(rs)),
                          mkU8(0x1)));
         assign(t1, binop(Iop_CmpEQ32,
                          getIReg(rt), mkU32(0x80000000)));
         assign(t2, binop(Iop_CmpEQ32,
                          getIReg(rs), mkU32(0x80000000)));

         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  IRExpr_ITE(mkexpr(t2),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x7fffffff),
                                           unop(Iop_64HIto32,
                                                mkexpr(t0))),
                                unop(Iop_64HIto32, mkexpr(t0))));
         break;
      }

      case 0x17: {  /* MULQ_RS.W */
         DIP("mulq_rs.w r%u, r%u, r%u", rd, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I1);

         assign(t0, binop(Iop_Add64,
                          binop(Iop_Shl64,
                                binop(Iop_MullS32,
                                      getIReg(rt),
                                      getIReg(rs)),
                                mkU8(0x1)),
                          mkU64(0x0000000080000000ULL)));
         assign(t1,
                binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x80000000)));
         assign(t2,
                binop(Iop_CmpEQ32, getIReg(rs), mkU32(0x80000000)));
         putDSPControl(IRExpr_ITE(mkexpr(t1),
                                  IRExpr_ITE(mkexpr(t2),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   mkU32(0x00200000)
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x7fffffff),
                                           unop(Iop_64HIto32,
                                                mkexpr(t0))),
                                unop(Iop_64HIto32, mkexpr(t0))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_DPAW_PH( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10;
   UInt   rs, rt, sa, ac;

   rs = get_rs(cins);
   rt = get_rt(cins);
   sa = get_sa(cins);
   ac = get_acNo(cins);

   switch (sa) {
      case 0x0: {  /* DPA.W.PH */
         DIP("dpa.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);

         assign(t0,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t1,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t2,
                binop(Iop_Add64,
                      getAcc(ac),
                      binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
         putAcc(ac, mkexpr(t2));
         break;
      }

      case 0x1: {  /* DPS.W.PH */
         DIP("dps.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);

         assign(t0,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t1,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t2,
                binop(Iop_Sub64,
                      getAcc(ac),
                      binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
         putAcc(ac, mkexpr(t2));
         break;
      }

      case 0x2: {  /* MULSA.W.PH */
         DIP("mulsa.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);

         assign(t4, getAcc(ac));
         assign(t0, binop(Iop_Mul32,
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rt))),
                          unop(Iop_16Sto32,
                               unop(Iop_32to16, getIReg(rs)))));
         assign(t1, binop(Iop_Mul32,
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          unop(Iop_16Sto32,
                               unop(Iop_32HIto16, getIReg(rs)))));
         assign(t2, binop(Iop_Sub32, mkexpr(t1), mkexpr(t0)));
         putAcc(ac, binop(Iop_Add64,
                          mkexpr(t4),
                          unop(Iop_32Sto64, mkexpr(t2))));
         break;
      }

      case 0x3: {  /* DPAU.H.QBL */
         DIP("dpau.h.qbl ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t0,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t1,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t2,
                unop(Iop_32Uto64,
                     binop(Iop_Add32,
                           mkexpr(t0),
                           mkexpr(t1))));
         assign(t3,
                binop(Iop_Add64, getAcc(ac), mkexpr(t2)));
         putAcc(ac, mkexpr(t3));
         break;
      }

      case 0x4: {  /* DPAQ_S.W.PH */
         DIP("dpaq_s.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rt)))),
                          mkU8(0x1)));
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t5, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rt)))
                               ),
                          mkU8(0x1)));
         assign(t6, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t7, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t8,
                IRExpr_ITE(mkexpr(t6),
                           IRExpr_ITE(mkexpr(t7),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t5)),
                           mkexpr(t5)));

         putDSPControl(IRExpr_ITE(mkexpr(t6),
                                  IRExpr_ITE(mkexpr(t7),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t9, binop(Iop_Add64,
                          binop(Iop_Add64, mkexpr(t4), mkexpr(t8)),
                          mkexpr(t0)));
         putAcc(ac, mkexpr(t9));
         break;
      }

      case 0x5: {  /* DPSQ_S.W.PH */
         DIP("dpsq_s.w.ph ac%u r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rt)))),
                          mkU8(0x1)));
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t5,
                binop(Iop_Shl64,
                      binop(Iop_MullS32,
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, getIReg(rs))),
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, getIReg(rt)))),
                      mkU8(0x1)));
         assign(t6, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t7, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t8,
                IRExpr_ITE(mkexpr(t6),
                           IRExpr_ITE(mkexpr(t7),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t5)),
                           mkexpr(t5)));

         putDSPControl(IRExpr_ITE(mkexpr(t6),
                                  IRExpr_ITE(mkexpr(t7),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t9,
                binop(Iop_Sub64,
                      mkexpr(t0),
                      binop(Iop_Add64, mkexpr(t4), mkexpr(t8))));
         putAcc(ac, mkexpr(t9));
         break;
      }

      case 0x6: {  /* MULSAQ_S.W.PH */
         DIP("mulsaq_s.w.ph ac%u r%u, r%u", ac, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I64);
         t8 = newTemp(Ity_I32);
         t9 = newTemp(Ity_I32);

         assign(t0, unop(Iop_16Sto32,
                         unop(Iop_32HIto16, getIReg(rs))));
         assign(t1, unop(Iop_16Sto32,
                         unop(Iop_32HIto16, getIReg(rt))));

         assign(t8, binop(Iop_And32,
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     unop(Iop_16Uto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rs))),
                                     mkU32(0x8000))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     unop(Iop_16Uto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rt))),
                                     mkU32(0x8000)))));
         /* DSPControl_outflag:16+acc <- 1 */
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t8),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x00010000),
                                              mkU8(ac))),
                                  getDSPControl()));

         /* tempB_31..0 */
         assign(t2,
                IRExpr_ITE(binop(Iop_CmpNE32,
                                 mkexpr(t8), mkU32(0x0)),
                           mkU32(0x7FFFFFFF),
                           binop(Iop_Shl32,
                                 binop(Iop_Mul32,
                                       mkexpr(t0), mkexpr(t1)),
                                 mkU8(1))));

         assign(t3, unop(Iop_16Sto32,
                         unop(Iop_32to16, getIReg(rs))));
         assign(t4, unop(Iop_16Sto32,
                         unop(Iop_32to16, getIReg(rt))));

         assign(t9, binop(Iop_And32,
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     unop(Iop_16Uto32,
                                          unop(Iop_32to16,
                                               getIReg(rs))),
                                     mkU32(0x8000))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     unop(Iop_16Uto32,
                                          unop(Iop_32to16,
                                               getIReg(rt))),
                                     mkU32(0x8000)))));
         /* DSPControl_outflag:16+acc <- 1 */
         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        mkexpr(t9),
                                        mkU32(0x0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x00010000),
                                              mkU8(ac))),
                                  getDSPControl()));
         /* tempA_31..0 */
         assign(t5,
                IRExpr_ITE(binop(Iop_CmpNE32,
                                 mkexpr(t9),
                                 mkU32(0x0)),
                           mkU32(0x7FFFFFFF),
                           binop(Iop_Shl32,
                                 binop(Iop_Mul32,
                                       mkexpr(t3),
                                       mkexpr(t4)),
                                 mkU8(1))));
         /* dotp_63..0 */
         assign(t6,
                binop(Iop_Sub64,
                      unop(Iop_32Sto64, mkexpr(t2)),
                      unop(Iop_32Sto64, mkexpr(t5))));
         /* tempC_63..0 */
         assign(t7, binop(Iop_Add64, getAcc(ac), mkexpr(t6)));

         putAcc(ac, mkexpr(t7));
         break;
      }

      case 0x7: {  /* DPAU.H.QBR */
         DIP("dpau.h.qbr ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t0,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t1,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t2, unop(Iop_32Uto64,
                         binop(Iop_Add32, mkexpr(t0), mkexpr(t1))));
         assign(t3, binop(Iop_Add64, getAcc(ac), mkexpr(t2)));
         putAcc(ac, mkexpr(t3));
         break;
      }

      case 0x8: {  /* DPAX.W.PH */
         DIP("dpax.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);

         assign(t0,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t1,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t2,
                binop(Iop_Add64,
                      getAcc(ac),
                      binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
         putAcc(ac, mkexpr(t2));
         break;
      }

      case 0x9: {  /* DPSX.W.PH */
         DIP("dpsx.w.ph ac%u r%u, r%u", ac, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);

         assign(t0,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t1,
                unop(Iop_32Sto64,
                     binop(Iop_Mul32,
                           unop(Iop_16Sto32,
                                unop(Iop_32to16, getIReg(rs))),
                           unop(Iop_16Sto32,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t2,
                binop(Iop_Sub64,
                      getAcc(ac),
                      binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
         putAcc(ac, mkexpr(t2));
         break;
      }

      case 0xB: {  /* DPSU.H.QBL */
         DIP("dpsu.h.qbl ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t0,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t1,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32HIto16, getIReg(rt))))));
         assign(t2,
                unop(Iop_32Uto64,
                     binop(Iop_Add32, mkexpr(t0), mkexpr(t1))));
         assign(t3,
                binop(Iop_Sub64, getAcc(ac), mkexpr(t2)));
         putAcc(ac, mkexpr(t3));
         break;
      }

      case 0xC: {  /* DPAQ_SA.L.W */
         DIP("dpaq_sa.l.w ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I64);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I1);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                getIReg(rs), getIReg(rt)),
                          mkU8(0x1)));

         assign(t2, binop(Iop_CmpEQ32,
                          getIReg(rs),
                          mkU32(0x80000000)));
         assign(t3, binop(Iop_CmpEQ32,
                          getIReg(rt),
                          mkU32(0x80000000)));

         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x7fffffffffffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t5, binop(Iop_Add64,
                          unop(Iop_32Uto64,
                               unop(Iop_64to32, mkexpr(t0))),
                          unop(Iop_32Uto64,
                               unop(Iop_64to32, mkexpr(t4)))));
         assign(t6,
                binop(Iop_Add64,
                      binop(Iop_Add64,
                            unop(Iop_32Sto64,
                                 unop(Iop_64HIto32, mkexpr(t0))),
                            unop(Iop_32Sto64,
                                 unop(Iop_64HIto32, mkexpr(t4)))),
                      unop(Iop_32Uto64,
                           binop(Iop_And32,
                                 unop(Iop_64HIto32, mkexpr(t5)),
                                 mkU32(0x1)))));
         assign(t7, binop(Iop_32HLto64,
                          unop(Iop_64to32, mkexpr(t6)),
                          unop(Iop_64to32, mkexpr(t5))));
         assign(t8, binop(Iop_CmpEQ32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t6)),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t6)),
                                mkU32(0x00000001))));
         assign(t9, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                unop(Iop_64HIto32,
                                     mkexpr(t6)),
                                mkU32(0x00000001)),
                          mkU32(0x1)));
         putDSPControl(IRExpr_ITE(mkexpr(t8),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));
         putAcc(ac,
                IRExpr_ITE(mkexpr(t8),
                           mkexpr(t7),
                           IRExpr_ITE(mkexpr(t9),
                                      mkU64(0x8000000000000000ULL),
                                      mkU64(0x7fffffffffffffffULL)))
               );
         break;
      }

      case 0xD: {  /* DPSQ_SA.L.W */
         DIP("dpsq_sa.l.w ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I64);
         t8 = newTemp(Ity_I1);
         t9 = newTemp(Ity_I1);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                getIReg(rs), getIReg(rt)),
                          mkU8(0x1)));

         assign(t2, binop(Iop_CmpEQ32,
                          getIReg(rs),
                          mkU32(0x80000000)));
         assign(t3, binop(Iop_CmpEQ32,
                          getIReg(rt),
                          mkU32(0x80000000)));

         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x7fffffffffffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t5, binop(Iop_Sub64,
                          unop(Iop_32Uto64,
                               unop(Iop_64to32, mkexpr(t0))),
                          unop(Iop_32Uto64,
                               unop(Iop_64to32, mkexpr(t4)))));
         assign(t6, binop(Iop_Sub64,
                          binop(Iop_Add64,
                                unop(Iop_32Sto64,
                                     unop(Iop_64HIto32, mkexpr(t0))
                                    ),
                                unop(Iop_32Sto64,
                                     unop(Iop_1Sto32,
                                          binop(Iop_CmpLT32U,
                                                unop(Iop_64to32,
                                                      mkexpr(t0)),
                                                unop(Iop_64to32,
                                                      mkexpr(t4)))))),
                          unop(Iop_32Sto64,
                               unop(Iop_64HIto32, mkexpr(t4)))));
         assign(t7, binop(Iop_32HLto64,
                          unop(Iop_64to32, mkexpr(t6)),
                          unop(Iop_64to32, mkexpr(t5))));
         assign(t8, binop(Iop_CmpEQ32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t6)),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t6)),
                                mkU32(0x00000001))));
         assign(t9, binop(Iop_CmpEQ32,
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t6)),
                                mkU32(0x00000001)),
                          mkU32(0x1)));
         putDSPControl(IRExpr_ITE(mkexpr(t8),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));
         putAcc(ac,
                IRExpr_ITE(mkexpr(t8),
                           mkexpr(t7),
                           IRExpr_ITE(mkexpr(t9),
                                      mkU64(0x8000000000000000ULL),
                                      mkU64(0x7fffffffffffffffULL)))
               );
         break;
      }

      case 0xF: {  /* DPSU.H.QBR */
         DIP("dpsu.h.qbr ac%u r%u, r%u", ac, rs, rt);
         vassert(!mode64);

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);

         assign(t0,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16HIto8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t1,
                binop(Iop_Mul32,
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rs)))),
                      unop(Iop_8Uto32,
                           unop(Iop_16to8,
                                unop(Iop_32to16, getIReg(rt))))));
         assign(t2, unop(Iop_32Uto64,
                         binop(Iop_Add32, mkexpr(t0), mkexpr(t1))));
         assign(t3, binop(Iop_Sub64, getAcc(ac), mkexpr(t2)));
         putAcc(ac, mkexpr(t3));

         break;
      }

      case 0x10: {  /* MAQ_SA.W.PHL */
         DIP("maq_sa.w.phl ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));
         assign(t1, unop(Iop_32Sto64,
                         binop(Iop_Shl32,
                               binop(Iop_Mul32,
                                     unop(Iop_16Sto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rs))),
                                     unop(Iop_16Sto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rt)))),
                               mkU8(0x1))));

         /* If both input arguments are equal 0x8000, saturate
            intermediate product and write to DSPControl register.
         */
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));

         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));
         /* Add intermediate product and value in the
            accumulator. */
         assign(t5, binop(Iop_Add64, mkexpr(t0), mkexpr(t4)));

         /* Compare bits 31 and 32 of the value in t5. */
         assign(t6, binop(Iop_CmpEQ32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t5)),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t5)),
                                mkU32(1))));
         putDSPControl(IRExpr_ITE(mkexpr(t6),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));
         assign(t7,
                IRExpr_ITE(mkexpr(t6),
                           mkexpr(t5),
                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_64HIto32,
                                                        mkexpr(t5)),
                                                  mkU32(1)),
                                            mkU32(0x0)),
                                      mkU64(0x000000007fffffffULL),
                                      mkU64(0xffffffff80000000ULL)))
               );
         putAcc(ac, mkexpr(t7));
         break;
      }

      case 0x12: {  /* MAQ_SA.W.PHR */
         DIP("maq_sa.w.phr ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));
         assign(t1, unop(Iop_32Sto64,
                         binop(Iop_Shl32,
                               binop(Iop_Mul32,
                                     unop(Iop_16Sto32,
                                          unop(Iop_32to16,
                                               getIReg(rs))),
                                     unop(Iop_16Sto32,
                                          unop(Iop_32to16,
                                               getIReg(rt)))),
                               mkU8(0x1))));

         /* If both input arguments are equal 0x8000, saturate
            intermediate product and write to DSPControl
            register. */
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));

         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));
         /* Add intermediate product and value in the
            accumulator. */
         assign(t5, binop(Iop_Add64, mkexpr(t0), mkexpr(t4)));

         /* Compare bits 31 and 32 of the value in t5. */
         assign(t6, binop(Iop_CmpEQ32,
                          binop(Iop_Shr32,
                                binop(Iop_And32,
                                      unop(Iop_64to32, mkexpr(t5)),
                                      mkU32(0x80000000)),
                                mkU8(31)),
                          binop(Iop_And32,
                                unop(Iop_64HIto32, mkexpr(t5)),
                                mkU32(1))));
         putDSPControl(IRExpr_ITE(mkexpr(t6),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));
         assign(t7,
                IRExpr_ITE(mkexpr(t6),
                           mkexpr(t5),
                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_64HIto32,
                                                        mkexpr(t5)),
                                                  mkU32(1)),
                                            mkU32(0x0)),
                                      mkU64(0x000000007fffffffULL),
                                      mkU64(0xffffffff80000000ULL)))
               );
         putAcc(ac, mkexpr(t7));
         break;
      }

      case 0x14: {  /* MAQ_S.W.PHL */
         DIP("maq_s.w.phl ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I64);

         assign(t5, getAcc(ac));

         assign(t0, unop(Iop_16Sto32,
                         unop(Iop_32HIto16, getIReg(rs))));
         assign(t1, unop(Iop_16Sto32,
                         unop(Iop_32HIto16, getIReg(rt))));

         assign(t2, binop(Iop_And32,
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0xffff)),
                                     mkU32(0x8000))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t1),
                                           mkU32(0xffff)),
                                     mkU32(0x8000)))));

         assign(t3, binop(Iop_CmpEQ32, mkexpr(t2), mkU32(0x0)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));

         assign(t4, unop(Iop_64to32,
                         binop(Iop_MullS32,
                               mkexpr(t0), mkexpr(t1))));
         putAcc(ac, IRExpr_ITE(mkexpr(t3),
                               binop(Iop_Add64,
                                     unop(Iop_32Sto64,
                                          binop(Iop_Shl32,
                                                mkexpr(t4),
                                                mkU8(0x1))),
                                     mkexpr(t5)),
                               binop(Iop_Add64,
                                     mkexpr(t5),
                                     unop(Iop_32Sto64,
                                          mkU32(0x7fffffff)))));
         break;
      }

      case 0x16: {  /* MAQ_S.W.PHR */
         DIP("maq_s.w.phr ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I64);

         assign(t5, getAcc(ac));

         assign(t0, unop(Iop_16Sto32,
                         unop(Iop_32to16, getIReg(rs))));
         assign(t1, unop(Iop_16Sto32,
                         unop(Iop_32to16, getIReg(rt))));

         assign(t2, binop(Iop_And32,
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0xffff)),
                                     mkU32(0x8000))),
                          unop(Iop_1Sto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           mkexpr(t1),
                                           mkU32(0xffff)),
                                     mkU32(0x8000)))));

         assign(t3, binop(Iop_CmpEQ32, mkexpr(t2), mkU32(0x0)));

         putDSPControl(IRExpr_ITE(mkexpr(t3),
                                  getDSPControl(),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));

         assign(t4, unop(Iop_64to32,
                         binop(Iop_MullS32,
                               mkexpr(t0), mkexpr(t1))));
         putAcc(ac, IRExpr_ITE(mkexpr(t3),
                               binop(Iop_Add64,
                                     unop(Iop_32Sto64,
                                          binop(Iop_Shl32,
                                                mkexpr(t4),
                                                mkU8(0x1))),
                                     mkexpr(t5)),
                               binop(Iop_Add64,
                                     mkexpr(t5),
                                     unop(Iop_32Sto64,
                                          mkU32(0x7fffffff)))));
         break;
      }

      case 0x18: {  /* DPAQX_S.W.PH */
         DIP("dpaqx_s.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16,
                                          getIReg(rt)))),
                          mkU8(0x1)));
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16))),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t5, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16,
                                          getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rt)))),
                          mkU8(0x1)));
         assign(t6, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t7, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t8,
                IRExpr_ITE(mkexpr(t6),
                           IRExpr_ITE(mkexpr(t7),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t5)),
                           mkexpr(t5)));

         putDSPControl(IRExpr_ITE(mkexpr(t6),
                                  IRExpr_ITE(mkexpr(t7),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t9, binop(Iop_Add64,
                          binop(Iop_Add64, mkexpr(t4), mkexpr(t8)),
                          mkexpr(t0)));
         putAcc(ac, mkexpr(t9));
         break;
      }

      case 0x19: {  /* DPSQX_S.W.PH */
         DIP("dpsqx_s.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);

         assign(t0, getAcc(ac));

         assign(t1, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16,
                                          getIReg(rt)))),
                          mkU8(0x1)));
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t4,
                IRExpr_ITE(mkexpr(t2),
                           IRExpr_ITE(mkexpr(t3),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t1)),
                           mkexpr(t1)));

         putDSPControl(IRExpr_ITE(mkexpr(t2),
                                  IRExpr_ITE(mkexpr(t3),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t5, binop(Iop_Shl64,
                          binop(Iop_MullS32,
                                unop(Iop_16Sto32,
                                     unop(Iop_32to16,
                                          getIReg(rs))),
                                unop(Iop_16Sto32,
                                     unop(Iop_32HIto16,
                                          getIReg(rt)))),
                          mkU8(0x1)));
         assign(t6, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t7, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));
         assign(t8,
                IRExpr_ITE(mkexpr(t6),
                           IRExpr_ITE(mkexpr(t7),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t5)),
                           mkexpr(t5)));

         putDSPControl(IRExpr_ITE(mkexpr(t6),
                                  IRExpr_ITE(mkexpr(t7),
                                             binop(Iop_Or32,
                                                   getDSPControl(),
                                                   binop(Iop_Shl32,
                                                         mkU32(0x1),
                                                         mkU8(ac + 16)
                                                        )
                                                  ),
                                             getDSPControl()),
                                  getDSPControl()));

         assign(t9, binop(Iop_Sub64,
                          mkexpr(t0),
                          binop(Iop_Add64, mkexpr(t4), mkexpr(t8))));
         putAcc(ac, mkexpr(t9));
         break;
      }

      case 0x1A: {  /* DPAQX_SA.W.PH */
         DIP("dpaqx_sa.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));
         /* Calculate the first cross dot product and saturate if
            needed. */
         assign(t1, unop(Iop_32Sto64,
                         binop(Iop_Shl32,
                               binop(Iop_Mul32,
                                     unop(Iop_16Sto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rs))),
                                     unop(Iop_16Sto32,
                                          unop(Iop_32to16,
                                               getIReg(rt)))),
                               mkU8(0x1))));

         /* If both input arguments are equal 0x8000, saturate
            intermediate product and write to DSPControl
            register. */
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));

         assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           unop(Iop_1Sto32,
                                                mkexpr(t2)),
                                           unop(Iop_1Sto32,
                                                mkexpr(t3))),
                                     mkU32(0)),
                               mkU64(0x000000007fffffffULL),
                               mkexpr(t1)));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              unop(Iop_1Sto32,
                                                    mkexpr(t2)),
                                              unop(Iop_1Sto32,
                                                    mkexpr(t3))),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16))),
                                  getDSPControl()));
         /* Calculate second cross dot product and saturate if
            needed. */
         assign(t5, unop(Iop_32Sto64,
                         binop(Iop_Shl32,
                               binop(Iop_Mul32,
                                     unop(Iop_16Sto32,
                                          unop(Iop_32to16,
                                               getIReg(rs))),
                                     unop(Iop_16Sto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rt)))),
                               mkU8(0x1))));

         /* If both input arguments are equal 0x8000, saturate
            intermediate product and write to DSPControl
            register. */
         assign(t6, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t7, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));

         assign(t8, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           unop(Iop_1Sto32,
                                                mkexpr(t6)),
                                           unop(Iop_1Sto32,
                                                mkexpr(t7))),
                                     mkU32(0)),
                               mkU64(0x000000007fffffffULL),
                               mkexpr(t5)));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              unop(Iop_1Sto32,
                                                    mkexpr(t6)),
                                              unop(Iop_1Sto32,
                                                    mkexpr(t7))),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16))),
                                  getDSPControl()));
         /* Subtract intermediate products from value in the
            accumulator. */
         assign(t9,
                binop(Iop_Add64,
                      mkexpr(t0),
                      binop(Iop_Add64, mkexpr(t8), mkexpr(t4))));

         putAcc(ac,
                IRExpr_ITE(binop(Iop_CmpEQ32,
                                 binop(Iop_And32,
                                       unop(Iop_64HIto32,
                                            mkexpr(t9)),
                                       mkU32(0x80000000)),
                                 mkU32(0x0)),
                           IRExpr_ITE(binop(Iop_CmpNE32,
                                            unop(Iop_64HIto32,
                                                 binop(Iop_Shl64,
                                                       mkexpr(t9),
                                                       mkU8(1))),
                                            mkU32(0x0)),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t9)),
                           IRExpr_ITE(binop(Iop_CmpNE32,
                                            unop(Iop_64HIto32,
                                                 binop(Iop_Shl64,
                                                       mkexpr(t9),
                                                       mkU8(1))),
                                            mkU32(0xffffffff)),
                                      mkU64(0xffffffff80000000ULL),
                                      mkexpr(t9))));
         assign(t10, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      unop(Iop_64to32,
                                           mkexpr(t9)),
                                      unop(Iop_64to32,
                                           getAcc(ac))),
                                getDSPControl(),
                                binop(Iop_Or32,
                                      getDSPControl(),
                                      binop(Iop_Shl32,
                                            mkU32(0x1),
                                            mkU8(ac + 16)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        unop(Iop_64HIto32,
                                             mkexpr(t9)),
                                        unop(Iop_64HIto32,
                                             getAcc(ac))),
                                  mkexpr(t10),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));
         break;
      }

      case 0x1B: {  /* DPSQX_SA.W.PH */
         DIP("dpsqx_sa.w.ph ac%u, r%u, r%u", ac, rs, rt);
         vassert(!mode64);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I1);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I1);
         t7 = newTemp(Ity_I1);
         t8 = newTemp(Ity_I64);
         t9 = newTemp(Ity_I64);
         t10 = newTemp(Ity_I32);

         assign(t0, getAcc(ac));
         /* Calculate the first cross dot product and saturate if
            needed. */
         assign(t1, unop(Iop_32Sto64,
                         binop(Iop_Shl32,
                               binop(Iop_Mul32,
                                     unop(Iop_16Sto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rs))),
                                     unop(Iop_16Sto32,
                                          unop(Iop_32to16,
                                               getIReg(rt)))),
                               mkU8(0x1))));

         /* If both input arguments are equal 0x8000, saturate
            intermediate product and write to DSPControl
            register. */
         assign(t2, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t3, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rt))),
                          mkU32(0x00008000)));

         assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           unop(Iop_1Sto32,
                                                mkexpr(t2)),
                                           unop(Iop_1Sto32,
                                                mkexpr(t3))),
                                     mkU32(0)),
                               mkU64(0x000000007fffffffULL),
                               mkexpr(t1)));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              unop(Iop_1Sto32,
                                                    mkexpr(t2)),
                                              unop(Iop_1Sto32,
                                                    mkexpr(t3))),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16))),
                                  getDSPControl()));
         /* Calculate second cross dot product and saturate if
            needed. */
         assign(t5, unop(Iop_32Sto64,
                         binop(Iop_Shl32,
                               binop(Iop_Mul32,
                                     unop(Iop_16Sto32,
                                          unop(Iop_32to16,
                                               getIReg(rs))),
                                     unop(Iop_16Sto32,
                                          unop(Iop_32HIto16,
                                               getIReg(rt)))),
                               mkU8(0x1))));

         /* If both input arguments are equal 0x8000, saturate
            intermediate product and write to DSPControl
            register. */
         assign(t6, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32to16, getIReg(rs))),
                          mkU32(0x00008000)));
         assign(t7, binop(Iop_CmpEQ32,
                          unop(Iop_16Uto32,
                               unop(Iop_32HIto16, getIReg(rt))),
                          mkU32(0x00008000)));

         assign(t8, IRExpr_ITE(binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           unop(Iop_1Sto32,
                                                mkexpr(t6)),
                                           unop(Iop_1Sto32,
                                                mkexpr(t7))),
                                     mkU32(0)),
                               mkU64(0x000000007fffffffULL),
                               mkexpr(t5)));

         putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              unop(Iop_1Sto32,
                                                    mkexpr(t6)),
                                              unop(Iop_1Sto32,
                                                    mkexpr(t7))),
                                        mkU32(0)),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16))),
                                  getDSPControl()));
         /* Subtract intermediate products from value in the
            accumulator. */
         assign(t9,
                binop(Iop_Sub64,
                      mkexpr(t0),
                      binop(Iop_Add64, mkexpr(t8), mkexpr(t4))));

         putAcc(ac,
                IRExpr_ITE(binop(Iop_CmpEQ32,
                                 binop(Iop_And32,
                                       unop(Iop_64HIto32,
                                            mkexpr(t9)),
                                       mkU32(0x80000000)),
                                 mkU32(0x0)),
                           IRExpr_ITE(binop(Iop_CmpNE32,
                                            unop(Iop_64HIto32,
                                                 binop(Iop_Shl64,
                                                       mkexpr(t9),
                                                       mkU8(1))),
                                            mkU32(0x0)),
                                      mkU64(0x000000007fffffffULL),
                                      mkexpr(t9)),
                           IRExpr_ITE(binop(Iop_CmpNE32,
                                            unop(Iop_64HIto32,
                                                 binop(Iop_Shl64,
                                                       mkexpr(t9),
                                                       mkU8(1))),
                                            mkU32(0xffffffff)),
                                      mkU64(0xffffffff80000000ULL),
                                      mkexpr(t9))));
         assign(t10, IRExpr_ITE(binop(Iop_CmpEQ32,
                                      unop(Iop_64to32,
                                           mkexpr(t9)),
                                      unop(Iop_64to32,
                                           getAcc(ac))),
                                getDSPControl(),
                                binop(Iop_Or32,
                                      getDSPControl(),
                                      binop(Iop_Shl32,
                                            mkU32(0x1),
                                            mkU8(ac + 16)))));
         putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                        unop(Iop_64HIto32,
                                             mkexpr(t9)),
                                        unop(Iop_64HIto32,
                                             getAcc(ac))),
                                  mkexpr(t10),
                                  binop(Iop_Or32,
                                        getDSPControl(),
                                        binop(Iop_Shl32,
                                              mkU32(0x1),
                                              mkU8(ac + 16)))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3_APPEND( UInt cins )
{
   IRTemp t1 = 0, t2, t3;
   UInt   rs, rt, rd, sa;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);

   switch (sa) {
      case 0x0: {  /* APPEND */
         DIP("append r%u, r%u, %u", rt, rs, rd);
         vassert(!mode64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);

         assign(t1, binop(Iop_Shl32, getIReg(rt), mkU8(rd)));

         if (31 == rd) {
            putIReg(rt, binop(Iop_Or32,
                              mkexpr(t1),
                              binop(Iop_And32,
                                    getIReg(rs),
                                    mkU32(0x7fffffff))));
         } else if (1 == rd) {
            putIReg(rt,
                    binop(Iop_Or32,
                          mkexpr(t1),
                          binop(Iop_And32,
                                getIReg(rs), mkU32(0x1))));
         } else {
            assign(t2,
                   unop(Iop_Not32,
                        binop(Iop_Shl32,
                              mkU32(0xffffffff), mkU8(rd))));

            putIReg(rt, binop(Iop_Or32,
                              mkexpr(t1),
                              binop(Iop_And32,
                                    getIReg(rs), mkexpr(t2))));
         }

         break;
      }

      case 0x1: {  /* PREPEND */
         DIP("prepend r%u, r%u, %u", rt, rs, rd);
         vassert(!mode64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);

         if (0 != rd) {
            assign(t1, binop(Iop_Shr32, getIReg(rt), mkU8(rd)));

            if (31 == rd) {
               putIReg(rt, binop(Iop_Or32,
                                 mkexpr(t1),
                                 binop(Iop_Shl32,
                                       binop(Iop_And32,
                                             getIReg(rs),
                                             mkU32(0x7fffffff)),
                                       mkU8(1))));
            } else if (1 == rd) {
               putIReg(rt, binop(Iop_Or32,
                                 mkexpr(t1),
                                 binop(Iop_Shl32,
                                       binop(Iop_And32,
                                             getIReg(rs),
                                             mkU32(0x1)),
                                       mkU8(31))));
            } else {
               assign(t2, binop(Iop_Add32, mkU32(rd), mkU32(0x1)));

               assign(t3, unop(Iop_Not32,
                               binop(Iop_Shl32,
                                     mkU32(0xffffffff),
                                     unop(Iop_32to8, mkexpr(t2)))));

               putIReg(rt, binop(Iop_Or32,
                                 mkexpr(t1),
                                 binop(Iop_Shl32,
                                       binop(Iop_And32,
                                             getIReg(rs),
                                             mkexpr(t3)),
                                       mkU8(32 - rd))));
            }
         }

         break;
      }

      case 0x10: {  /* BALIGN */
         DIP("balign r%u, r%u, %u", rt, rs, rd);
         vassert(!mode64);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);

         if ((2 != rd) && (0 != rd)) {
            assign(t1, binop(Iop_Shl32,
                             binop(Iop_And32,
                                   mkU32(rd), mkU32(0x3)),
                             mkU8(0x3)));
            assign(t2, binop(Iop_Shl32,
                             getIReg(rt),
                             unop(Iop_32to8, mkexpr(t1))));
            assign(t3, binop(Iop_Shr32,
                             getIReg(rs),
                             unop(Iop_32to8,
                                  binop(Iop_Shl32,
                                        binop(Iop_Sub32,
                                              mkU32(0x4),
                                              binop(Iop_And32,
                                                    mkU32(rd),
                                                    mkU32(0x3))),
                                        mkU8(0x3)))));
            putIReg(rt, binop(Iop_Or32, mkexpr(t2), mkexpr(t3)));
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disDSPInstr_MIPS_WRK_Special3( UInt cins )
{
   switch (get_function(cins)) {
      case 0x12: {  /* ABSQ_S.PH */
         return disDSPInstr_MIPS_WRK_Special3_ABSQ_SPH(cins);
      }

      case 0x38: {  /* EXTR.W */
         return disDSPInstr_MIPS_WRK_Special3_EXTR_W(cins);
      }

      case 0xA: {  /* LX */
         return disDSPInstr_MIPS_WRK_Special3_LX(cins);
      }

      case 0xC: {  /* INSV */
         return disDSPInstr_MIPS_WRK_Special3_INSV(cins);
      }

      case 0x10: {  /* ADDU.QB */
         return disDSPInstr_MIPS_WRK_Special3_ADDU_QB(cins);
      }

      case 0x11: {  /* CMPU.EQ.QB */
         return disDSPInstr_MIPS_WRK_Special3_CMPU_EQ_QB(cins);
      }

      case 0x13: {  /* SHLL.QB */
         return disDSPInstr_MIPS_WRK_Special3_SHLL_QB(cins);
      }

      case 0x18: {  /* ADDUH.QB/MUL.PH */
         return disDSPInstr_MIPS_WRK_Special3_ADDUH_QB(cins);
      }

      case 0x30: {  /* DPA.W.PH */
         return disDSPInstr_MIPS_WRK_Special3_DPAW_PH(cins);
      }

      case 0x31: {  /* APPEND */
         return disDSPInstr_MIPS_WRK_Special3_APPEND(cins);
      }

      default:
         return -1;
   }
}

UInt disDSPInstr_MIPS_WRK ( UInt cins )
{
   UInt opcode = get_opcode(cins);

   switch (opcode) {
      case 0x00: {  /* Special */
         return disDSPInstr_MIPS_WRK_Special(cins);
      }

      case 0x1C: {  /* Special2 */
         return disDSPInstr_MIPS_WRK_Special2(cins);
      }

      case 0x1F: {  /* Special3 */
         return disDSPInstr_MIPS_WRK_Special3(cins);
      }

      default:
         return -1;
   }
}
