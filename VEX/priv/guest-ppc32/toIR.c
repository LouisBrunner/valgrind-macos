
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-ppc32/toIR.c) is                            ---*/
/*--- Copyright (c) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

/* Translates PPC32 code to IR. */

/* References
   All page references, unless otherwise indicated, refer to IBM's
   "PowerPC Microprocessor Family:
    The Programming Environments for 32-Bit Microprocessors"
    02/21/2000
    http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600719DF2

   Other refs:
   "PowerPC Microprocessor Family:
    Programming Environments Manual for 64 and 32-Bit Microprocessors
    Version 2.0"
    06/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/F6153E213FDD912E87256D49006C6541
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_ppc32.h"

#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "guest-ppc32/gdefs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a BB, so that we
   don't have to pass them around endlessly.  CONST means does not
   change during translation of a bb. 
*/

/* Pointer to the guest code area. */
/* CONST */
static UChar* guest_code;

/* The guest address corresponding to guest_code[0]. */
/* CONST */
static Addr32 guest_pc_bbstart;

/* The guest address for the instruction currently being
   translated. */
/* CONST for any specific insn, not for the entire BB */
static Addr32 guest_cia_curr_instr;

/* The IRBB* into which we're generating code. */
static IRBB* irbb;


/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define PPC32_TOIR_DEBUG 0

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

#define DIS(buf, format, args...)      \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_sprintf(buf, format, ## args)


/*------------------------------------------------------------*/
/*--- Offsets of various parts of the ppc32 guest state.   ---*/
/*------------------------------------------------------------*/

#define OFFB_GPR0       offsetof(VexGuestPPC32State,guest_GPR0)
#define OFFB_GPR1       offsetof(VexGuestPPC32State,guest_GPR1)
#define OFFB_GPR2       offsetof(VexGuestPPC32State,guest_GPR2)
#define OFFB_GPR3       offsetof(VexGuestPPC32State,guest_GPR3)
#define OFFB_GPR4       offsetof(VexGuestPPC32State,guest_GPR4)
#define OFFB_GPR5       offsetof(VexGuestPPC32State,guest_GPR5)
#define OFFB_GPR6       offsetof(VexGuestPPC32State,guest_GPR6)
#define OFFB_GPR7       offsetof(VexGuestPPC32State,guest_GPR7)
#define OFFB_GPR8       offsetof(VexGuestPPC32State,guest_GPR8)
#define OFFB_GPR9       offsetof(VexGuestPPC32State,guest_GPR9)
#define OFFB_GPR10      offsetof(VexGuestPPC32State,guest_GPR10)
#define OFFB_GPR11      offsetof(VexGuestPPC32State,guest_GPR11)
#define OFFB_GPR12      offsetof(VexGuestPPC32State,guest_GPR12)
#define OFFB_GPR13      offsetof(VexGuestPPC32State,guest_GPR13)
#define OFFB_GPR14      offsetof(VexGuestPPC32State,guest_GPR14)
#define OFFB_GPR15      offsetof(VexGuestPPC32State,guest_GPR15)
#define OFFB_GPR16      offsetof(VexGuestPPC32State,guest_GPR16)
#define OFFB_GPR17      offsetof(VexGuestPPC32State,guest_GPR17)
#define OFFB_GPR18      offsetof(VexGuestPPC32State,guest_GPR18)
#define OFFB_GPR19      offsetof(VexGuestPPC32State,guest_GPR19)
#define OFFB_GPR20      offsetof(VexGuestPPC32State,guest_GPR20)
#define OFFB_GPR21      offsetof(VexGuestPPC32State,guest_GPR21)
#define OFFB_GPR22      offsetof(VexGuestPPC32State,guest_GPR22)
#define OFFB_GPR23      offsetof(VexGuestPPC32State,guest_GPR23)
#define OFFB_GPR24      offsetof(VexGuestPPC32State,guest_GPR24)
#define OFFB_GPR25      offsetof(VexGuestPPC32State,guest_GPR25)
#define OFFB_GPR26      offsetof(VexGuestPPC32State,guest_GPR26)
#define OFFB_GPR27      offsetof(VexGuestPPC32State,guest_GPR27)
#define OFFB_GPR28      offsetof(VexGuestPPC32State,guest_GPR28)
#define OFFB_GPR29      offsetof(VexGuestPPC32State,guest_GPR29)
#define OFFB_GPR30      offsetof(VexGuestPPC32State,guest_GPR30)
#define OFFB_GPR31      offsetof(VexGuestPPC32State,guest_GPR31)

#define OFFB_CIA        offsetof(VexGuestPPC32State,guest_CIA)
#define OFFB_LR         offsetof(VexGuestPPC32State,guest_LR)
#define OFFB_CTR        offsetof(VexGuestPPC32State,guest_CTR)

#define OFFB_CC_OP      offsetof(VexGuestPPC32State,guest_CC_OP)
#define OFFB_CC_DEP1    offsetof(VexGuestPPC32State,guest_CC_DEP1)
#define OFFB_CC_DEP2    offsetof(VexGuestPPC32State,guest_CC_DEP2)

#define OFFB_CR0to6     offsetof(VexGuestPPC32State,guest_CR0to6)

#define OFFB_XER        offsetof(VexGuestPPC32State,guest_XER)

#define OFFB_TISTART    offsetof(VexGuestPPC32State,guest_TISTART)
#define OFFB_TILEN      offsetof(VexGuestPPC32State,guest_TILEN)


/*------------------------------------------------------------*/
/*--- Abstract register interface (non-gpr|fpr)           --- */
/*------------------------------------------------------------*/

/* Offsets of bitfields within various ppc32 registers */
#define SHIFT_XER_SO 31
#define SHIFT_XER_OV 30
#define SHIFT_XER_CA 29
#define SHIFT_XER_BC 0

#define SHIFT_CR_LT 8
#define SHIFT_CR_GT 4
#define SHIFT_CR_EQ 2
#define SHIFT_CR_SO 1

// Special purpose (i.e. non-gpr/fpr) registers
typedef enum {
    PPC32_SPR_CIA,    // Current Instruction Address
    PPC32_SPR_LR,     // Link Register
    PPC32_SPR_CTR,    // Count Register
    PPC32_SPR_XER,    // Summary Overflow
    PPC32_SPR_CR,     // Condition Register
    PPC32_SPR_MAX
} PPC32SPR;

/* Gets from SPR (non-GPR|FPR) registers */
static IRExpr* getReg_masked ( PPC32SPR reg, UInt mask );
static IRExpr* getReg        ( PPC32SPR reg );
static IRExpr* getReg_field  ( PPC32SPR reg, UInt field_idx );
static IRExpr* getReg_bit    ( PPC32SPR reg, UInt bit_idx );

/* Puts to SPR (non-GPR|FPR) registers */
static void putReg_masked ( PPC32SPR reg, IRExpr* src, UInt mask );
static void putReg        ( PPC32SPR reg, IRExpr* src );
static void putReg_field  ( PPC32SPR reg, IRExpr* src, UInt field_idx );
static void putReg_bit    ( PPC32SPR reg, IRExpr* src, UInt bit_idx );




/*------------------------------------------------------------*/
/*--- Misc Helpers                                         ---*/
/*------------------------------------------------------------*/

static UInt MASK( UInt begin, UInt end )
{
   UInt m1 = ((UInt)(-1)) << begin;
   UInt m2 = ((UInt)(-1)) << (end + 1);
   UInt mask = m1 ^ m2;
   if (begin > end) mask = ~mask;  // wrap mask
   return mask;
}

#if PPC32_TOIR_DEBUG
static void vex_printf_binary( UInt x, UInt len, Bool spaces )
{
   UInt i;
   vassert(len > 0 && len <= 32);
   
   for (i=len; i>0; i--) {
      vex_printf("%d", ((x & (1<<(len-1))) != 0) );
      x = x << 1;
      if (((i-1)%4)==0 && (i > 1) && spaces) {
         vex_printf(" ");
      }
   }
}
#endif



/*------------------------------------------------------------*/
/*--- Disassemble an entire basic block                    ---*/
/*------------------------------------------------------------*/

/* The results of disassembling an instruction.  There are three
   possible outcomes.  For Dis_Resteer, the disassembler _must_
   continue at the specified address.  For Dis_StopHere, the
   disassembler _must_ terminate the BB.  For Dis_Continue, we may at
   our option either disassemble the next insn, or terminate the BB;
   but in the latter case we must set the bb's ->next field to point
   to the next instruction.  */

typedef
   enum { 
      Dis_StopHere, /* this insn terminates the BB; we must stop. */
      Dis_Continue, /* we can optionally continue into the next insn */
      Dis_Resteer   /* followed a branch; continue at the spec'd addr */
   }
   DisResult;


/* forward decls .. */
static IRExpr* mkU32 ( UInt i );
static void stmt ( IRStmt* st );


/* disInstr disassembles an instruction located at &guest_code[delta],
   and sets *size to its size.  If the returned value is Dis_Resteer,
   the next guest address is assigned to *whereNext.  disInstr is not
   permitted to return Dis_Resteer if either (1) resteerOK is False,
   or (2) resteerOkFn, when applied to the address which it wishes to
   resteer into, returns False.  */
   
static DisResult disInstr ( /*IN*/  Bool    resteerOK,
                            /*IN*/  Bool    (*resteerOkFn) ( Addr64 ),
                            /*IN*/  UInt    delta, 
                            /*OUT*/ Int*    size,
                            /*OUT*/ Addr64* whereNext );


/* This is the main (only, in fact) entry point for this module. */

/* Disassemble a complete basic block, starting at guest_pc_start, and
   dumping the IR into global irbb.  Returns the size, in bytes, of
   the basic block.  
*/
IRBB* bbToIR_PPC32 ( UChar*           ppc32code, 
                     Addr64           guest_pc_start, 
                     VexGuestExtents* vge, 
                     Bool             (*byte_accessible)(Addr64),
                     Bool             (*chase_into_ok)(Addr64),
                     Bool             host_bigendian,
                     VexSubArch       subarch_guest )
{
   UInt       delta;
   Int        i, n_instrs, size, first_stmt_idx;
   Addr64     guest_next;
   Bool       resteerOK;
   DisResult  dres;
   static Int n_resteers = 0;
   Int        d_resteers = 0;

   /* check sanity .. */
   vassert(vex_control.guest_max_insns >= 1);
   vassert(vex_control.guest_max_insns < 1000);
   vassert(vex_control.guest_chase_thresh >= 0);
   vassert(vex_control.guest_chase_thresh < vex_control.guest_max_insns);

   vassert(subarch_guest == VexSubArchPPC32_noAV
           || subarch_guest == VexSubArchPPC32_AV);

   /* Start a new, empty extent. */
   vge->n_used  = 1;
   vge->base[0] = guest_pc_start;
   vge->len[0]  = 0;

   /* Set up globals. */
   guest_code        = ppc32code;
   guest_pc_bbstart  = (Addr32)guest_pc_start;
   irbb              = emptyIRBB();

   vassert((guest_pc_start >> 32) == 0);

   /* Delta keeps track of how far along the x86code array we
      have so far gone. */
   delta             = 0;
   n_instrs          = 0;

   while (True) {
      vassert(n_instrs < vex_control.guest_max_insns);

      guest_next = 0;
      resteerOK = toBool(n_instrs < vex_control.guest_chase_thresh);
      first_stmt_idx = irbb->stmts_used;

      guest_cia_curr_instr = guest_pc_bbstart + delta;

      if (n_instrs > 0) {
         /* for the first insn, the dispatch loop will have set
            CIA, but for all the others we have to do it ourselves. */
         putReg( PPC32_SPR_CIA, mkU32(guest_cia_curr_instr) );
      }

      dres = disInstr( resteerOK, chase_into_ok, 
                       delta, &size, &guest_next );

      /* Print the resulting IR, if needed. */
      if (vex_traceflags & VEX_TRACE_FE) {
         for (i = first_stmt_idx; i < irbb->stmts_used; i++) {
            vex_printf("              ");
            ppIRStmt(irbb->stmts[i]);
            vex_printf("\n");
         }
      }
   
      if (dres == Dis_StopHere) {
         vassert(irbb->next != NULL);
         if (vex_traceflags & VEX_TRACE_FE) {
            vex_printf("              ");
            vex_printf( "goto {");
            ppIRJumpKind(irbb->jumpkind);
            vex_printf( "} ");
            ppIRExpr( irbb->next );
            vex_printf( "\n");
         }
      }

      delta += size;
      vge->len[vge->n_used-1] += size;
      n_instrs++;
      DIP("\n");

      vassert(size == 0 || size == 4);
      if (!resteerOK) 
         vassert(dres != Dis_Resteer);
      if (dres != Dis_Resteer) 
         vassert(guest_next == 0);

      switch (dres) {
      case Dis_Continue:
         vassert(irbb->next == NULL);
         if (n_instrs < vex_control.guest_max_insns) {
            /* keep going */
         } else {
            irbb->next = mkU32(((Addr32)guest_pc_start)+delta);
            return irbb;
         }
         break;
      case Dis_StopHere:
         vassert(irbb->next != NULL);
         return irbb;
      case Dis_Resteer:
         vassert(irbb->next == NULL);
         /* figure out a new delta to continue at. */
         vassert(chase_into_ok(guest_next));
         delta = toUInt(guest_next - guest_pc_start);
         n_resteers++;
         d_resteers++;
         if (0 && (n_resteers & 0xFF) == 0)
            vex_printf("resteer[%d,%d] to %p (delta = %d)\n",
                       n_resteers, d_resteers,
                       ULong_to_Ptr(guest_next), (Int)delta);
         break;

      default: vpanic("bbToIR_PPC32(ppc32)");
      }
   }
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- ppc32 insn stream.                                   ---*/
/*------------------------------------------------------------*/

/* Add a statement to the list held by "irbb". */
static void stmt ( IRStmt* st )
{
   addStmtToIRBB( irbb, st );
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp( irbb->tyenv, ty );
}

#if 0
/* Bomb out if we can't handle something. */
__attribute__ ((noreturn))
static void unimplemented ( Char* str )
{
   vex_printf("ppc32ToIR: unimplemented feature\n");
   vpanic(str);
}
#endif

/* Various simple conversions */

#if 0
static UInt extend_s_8to32( UInt x )
{
   return (UInt)((((Int)x) << 24) >> 24);
}
#endif

static UInt extend_s_16to32 ( UInt x )
{
   return (UInt)((((Int)x) << 16) >> 16);
}

static UInt extend_s_24to32 ( UInt x )
{
   return (UInt)((((Int)x) << 8) >> 8);
}

/* Do a big-endian load of a 32-bit word, regardless of the endianness
   of the underlying host. */
static UInt getUIntBigendianly ( UChar* p )
{
   UInt w = 0;
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
   return w;
}


/*------------------------------------------------------------*/
/*--- Helpers for constructing IR.                         ---*/
/*------------------------------------------------------------*/

static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
//   vassert(!host_is_bigendian);
   // jrs: probably not necessary; only matters if we reference sub-parts
   // of the ppc32 registers, but that isn't the case

   switch (archreg) {
   case  0: return offsetof(VexGuestPPC32State, guest_GPR0);
   case  1: return offsetof(VexGuestPPC32State, guest_GPR1);
   case  2: return offsetof(VexGuestPPC32State, guest_GPR2);
   case  3: return offsetof(VexGuestPPC32State, guest_GPR3);
   case  4: return offsetof(VexGuestPPC32State, guest_GPR4);
   case  5: return offsetof(VexGuestPPC32State, guest_GPR5);
   case  6: return offsetof(VexGuestPPC32State, guest_GPR6);
   case  7: return offsetof(VexGuestPPC32State, guest_GPR7);
   case  8: return offsetof(VexGuestPPC32State, guest_GPR8);
   case  9: return offsetof(VexGuestPPC32State, guest_GPR9);
   case 10: return offsetof(VexGuestPPC32State, guest_GPR10);
   case 11: return offsetof(VexGuestPPC32State, guest_GPR11);
   case 12: return offsetof(VexGuestPPC32State, guest_GPR12);
   case 13: return offsetof(VexGuestPPC32State, guest_GPR13);
   case 14: return offsetof(VexGuestPPC32State, guest_GPR14);
   case 15: return offsetof(VexGuestPPC32State, guest_GPR15);
   case 16: return offsetof(VexGuestPPC32State, guest_GPR16);
   case 17: return offsetof(VexGuestPPC32State, guest_GPR17);
   case 18: return offsetof(VexGuestPPC32State, guest_GPR18);
   case 19: return offsetof(VexGuestPPC32State, guest_GPR19);
   case 20: return offsetof(VexGuestPPC32State, guest_GPR20);
   case 21: return offsetof(VexGuestPPC32State, guest_GPR21);
   case 22: return offsetof(VexGuestPPC32State, guest_GPR22);
   case 23: return offsetof(VexGuestPPC32State, guest_GPR23);
   case 24: return offsetof(VexGuestPPC32State, guest_GPR24);
   case 25: return offsetof(VexGuestPPC32State, guest_GPR25);
   case 26: return offsetof(VexGuestPPC32State, guest_GPR26);
   case 27: return offsetof(VexGuestPPC32State, guest_GPR27);
   case 28: return offsetof(VexGuestPPC32State, guest_GPR28);
   case 29: return offsetof(VexGuestPPC32State, guest_GPR29);
   case 30: return offsetof(VexGuestPPC32State, guest_GPR30);
   case 31: return offsetof(VexGuestPPC32State, guest_GPR31);
   }

   vpanic("integerGuestRegOffset(ppc32,le)"); /*notreached*/
}

static IRExpr* getIReg ( UInt archreg )
{
   vassert(archreg < 32);
   return IRExpr_Get( integerGuestRegOffset(archreg), Ity_I32 );
}

/* Ditto, but write to a reg instead. */
static void putIReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I32);
   stmt( IRStmt_Put(integerGuestRegOffset(archreg), e) );
}

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_Tmp(dst, e) );
}

static void storeBE ( IRExpr* addr, IRExpr* data )
{
   stmt( IRStmt_STle(addr,data) );
}

static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_Tmp(tmp);
}

static IRExpr* mkU1 ( UInt i )
{
   vassert(i < 2);
   return IRExpr_Const(IRConst_U1( toBool(i) ));
}

static IRExpr* mkU8 ( UChar i )
{
   return IRExpr_Const(IRConst_U8(i));
}

static IRExpr* mkU16 ( UShort i )
{
   return IRExpr_Const(IRConst_U16(i));
}

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* loadBE ( IRType ty, IRExpr* data )
{
   return IRExpr_LDle(ty,data);
}

// ROTL(src32, rot_amt5)
static IRExpr* ROTL32 ( IRExpr* src, IRExpr* rot_amt )
{
   IRTemp rot_amt5;
   vassert(typeOfIRExpr(irbb->tyenv,src) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,rot_amt) == Ity_I8);
   
   /* By masking the rotate amount thusly, the IR-level Shl/Shr
      expressions never shift beyond the word size and thus remain
      well defined. */
   rot_amt5 = newTemp(Ity_I8);
   assign(rot_amt5, binop(Iop_And8, rot_amt, mkU8(0x1F)));
   
   // (src << rot_amt) | (src >> (32-rot_amt))
   return binop(Iop_Or32,
                binop(Iop_Shl32, src, mkexpr(rot_amt5)),
                binop(Iop_Shr32, src,
                      binop(Iop_Sub8, mkU8(32), mkexpr(rot_amt5))));
}






/*------------------------------------------------------------*/
/*--- Helpers for %flags.                                 ---*/
/*------------------------------------------------------------*/

/* -------------- Evaluating the flags-thunk. -------------- */

/* Calculate CR7 (IBM CR0) conditional flags */
static IRExpr* mk_ppc32g_calculate_cr7_all ( void )
{
   IRExpr** args =
      mkIRExprVec_3( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                     IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                     IRExpr_Get(OFFB_CC_DEP2, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr7_all", &ppc32g_calculate_cr7_all,
           args
        );

// TODO
// 02/02/05 - leaving definedness stuff 'till get memcheck working well.

   /* Exclude OP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
//   call->Iex.CCall.cee->mcx_mask = 1;

   return call;
}

/* Calculate XER_OV flag */
static IRExpr* mk_ppc32g_calculate_xer_ov ( UInt op, IRExpr* res,
                                            IRExpr* argL, IRExpr* argR )
{
   IRExpr** args;
   IRExpr*  call;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);

   args = mkIRExprVec_4( mkU32(op), res, argL, argR );

   call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/,
           "ppc32g_calculate_xer_ov", &ppc32g_calculate_xer_ov,
           args
        );
   return binop(Iop_And32, mkU32(1), call);
}

/* Calculate XER_CA flag */
static IRExpr* mk_ppc32g_calculate_xer_ca ( UInt op, IRExpr* res,
                                            IRExpr* argL, IRExpr* argR )
{
   IRExpr*  xer_ca;
   IRExpr** args;
   IRExpr*  call;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);

   xer_ca = getReg_bit( PPC32_SPR_XER, SHIFT_XER_CA );

   args = mkIRExprVec_5( mkU32(op), res, argL, argR, xer_ca );

   call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/,
           "ppc32g_calculate_xer_ca", &ppc32g_calculate_xer_ca,
           args
        );
   return binop(Iop_And32, mkU32(1), call);
}


/* -------------- Building the flags-thunk. -------------- */

/* The machinery in this section builds the flag-thunk following a
   flag-setting operation.  Hence the various setFlags_* functions.
*/

/* Set the flags thunk OP=0, DEP1, DEP2 fields. */
static void setFlags_CR7 ( IRExpr* result )
{
   IRExpr* xer_so;
   vassert(typeOfIRExpr(irbb->tyenv,result) == Ity_I32);

   xer_so = getReg_bit( PPC32_SPR_XER, SHIFT_XER_SO );

   // => Delaying calculating result until needed...
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(0) ));
   stmt( IRStmt_Put( OFFB_CC_DEP1, result   ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, xer_so   ));
}

static void setFlags_XER_OV_SO( UInt op, IRExpr* res,
                                IRExpr* argL, IRExpr* argR )
{
   IRExpr* xer_ov;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);

   // => Calculate result immediately
   xer_ov = mk_ppc32g_calculate_xer_ov(op, res, argL, argR);

   putReg_bit( PPC32_SPR_XER, xer_ov, SHIFT_XER_OV );
   putReg_bit( PPC32_SPR_XER, xer_ov, SHIFT_XER_SO );
}

static void setFlags_XER_CA( UInt op, IRExpr* res,
                             IRExpr* argL, IRExpr* argR )
{
   IRExpr* xer_ca;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);

   // Calculate new xer_ca immediately:
   xer_ca = mk_ppc32g_calculate_xer_ca(op, res, argL, argR );

   putReg_bit( PPC32_SPR_XER, xer_ca, SHIFT_XER_CA );
}





/*------------------------------------------------------------*/
/*--- Abstract register interface                         --- */
/*------------------------------------------------------------*/
/* Most registers are represented directly in the cpu_state,
   but CR is represented by a thunk */


/* Get a masked word from the given reg */
static IRExpr* getReg_masked ( PPC32SPR reg, UInt mask )
{
   IRTemp val = newTemp(Ity_I32);
   vassert( reg < PPC32_SPR_MAX );
    
   switch (reg) {
   case PPC32_SPR_CIA:
      vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
      assign( val, IRExpr_Get(OFFB_CIA, Ity_I32) );
      break;

   case PPC32_SPR_LR:
      vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
      assign( val, IRExpr_Get(OFFB_LR, Ity_I32) );
      break;

   case PPC32_SPR_CTR:
      assign( val, IRExpr_Get(OFFB_CTR, Ity_I32) );
      break;

   case PPC32_SPR_XER:
      vassert((mask & 0xF000007F) == mask); // Only valid bits of xer
      // actually, bit28 not valid, but sometimes asked for anyway - always 0:
      mask = mask & ~(1<<28);
      assign( val, IRExpr_Get(OFFB_XER, Ity_I32) );
      break;

   case PPC32_SPR_CR:
      if (mask & 0xF0000000) {
         // Call helper function to calculate latest CR7 from thunk:
         assign( val, binop(Iop_Or32, mk_ppc32g_calculate_cr7_all(),
                            IRExpr_Get(OFFB_CR0to6, Ity_I32)) );
      } else {
         assign( val, IRExpr_Get(OFFB_CR0to6, Ity_I32) );
      }
      break;

   default:
      vpanic("getReg(ppc32)");
   }

   if (mask != 0xFFFFFFFF) {
      return binop(Iop_And32, mkexpr(val), mkU32(mask));
   } else {
      return mkexpr(val);
   }
}

/* Get word from the given reg */
static IRExpr* getReg ( PPC32SPR reg )
{
   vassert( reg < PPC32_SPR_MAX );
   switch (reg) {
   case PPC32_SPR_XER:
      return getReg_masked( reg, 0xE000007F ); // Only valid bits of xer
   default:
      return getReg_masked( reg, 0xFFFFFFFF );
   }
}

/* Get a right-shifted nibble from given reg[field_idx]
   returns zero padded word */
static IRExpr* getReg_field ( PPC32SPR reg, UInt field_idx )
{
   IRExpr* fld;
   vassert( field_idx < 8 );
   vassert( reg < PPC32_SPR_MAX );
   
   fld = getReg_masked( reg, (0xF << (field_idx*4)) );
   
   if (field_idx != 0) {
      fld = binop(Iop_Shr32, fld, mkU8(toUChar(field_idx * 4)));
   }
   return fld;
}

/* Get a right-shifted bit from given reg[bit_idx]
   returns zero padded word */
static IRExpr* getReg_bit ( PPC32SPR reg, UInt bit_idx )
{
   IRExpr* val;
   vassert( bit_idx <= 32 );
   vassert( reg < PPC32_SPR_MAX );
   
   val = getReg_masked( reg, 1<<bit_idx );

   if (bit_idx != 0) {
      val = binop(Iop_Shr32, val, mkU8(toUChar(bit_idx)));
   }
   return val;
}



/* Write masked src to the given reg */
static void putReg_masked ( PPC32SPR reg, IRExpr* src, UInt mask )
{
   IRTemp src_mskd = newTemp(Ity_I32);
   IRTemp reg_old  = newTemp(Ity_I32);

   vassert( reg < PPC32_SPR_MAX );
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   
   switch (reg) {
   case PPC32_SPR_CIA:
      vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
      stmt( IRStmt_Put( OFFB_CIA, src ) );
      break;

   case PPC32_SPR_LR:
      vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
      stmt( IRStmt_Put( OFFB_LR, src ) );
      break;

   case PPC32_SPR_CTR:
      vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
      stmt( IRStmt_Put( OFFB_CTR, src ) );
      break;

   case PPC32_SPR_XER:
      vassert((mask & 0xF000007F) == mask); // Only valid bits of xer
      // actually, bit28 not valid, but sometimes asked for anyway - always 0:
      mask = mask & ~(1<<28);
      assign( src_mskd, binop(Iop_And32, src, mkU32(mask)) );
      assign( reg_old, getReg_masked( PPC32_SPR_XER, (~mask & 0xE000007F) ) );

      stmt( IRStmt_Put( OFFB_XER,
                        binop(Iop_Or32, mkexpr(src_mskd), mkexpr(reg_old)) ));
      break;

   case PPC32_SPR_CR: {
      if (mask & 0xF0000000) { // CR 7:
         /* Write exactly the given flags to field CR7
            Set the flags thunk OP=1, DEP1=flags, DEP2=0(unused). */

         // => Delaying calculation until needed...
         stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(1) ) );
         stmt( IRStmt_Put( OFFB_CC_DEP1, src      ) );  // masked in helper.
         stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ) );
      }
      // CR 0 to 6:
      assign( src_mskd, binop(Iop_And32, src, mkU32(mask & 0x0FFFFFFF)) );
      assign( reg_old, getReg_masked( PPC32_SPR_CR, (~mask & 0x0FFFFFFF) ) );

      stmt( IRStmt_Put( OFFB_CR0to6,
                        binop(Iop_Or32, mkexpr(src_mskd), mkexpr(reg_old)) ));
      break;
   }

   default:
      vpanic("putReg(ppc32)");
   }
}

/* Write src to the given reg */
static void putReg ( PPC32SPR reg, IRExpr* src )
{
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   vassert( reg < PPC32_SPR_MAX );

   switch (reg) {
   case PPC32_SPR_XER:
      putReg_masked( reg, src, 0xE000007F ); // Only valid bits of xer
      break;
   default:
      putReg_masked( reg, src, 0xFFFFFFFF );
      break;
   }
}

/* Write least-significant nibble of src to reg[field_idx] */
static void putReg_field ( PPC32SPR reg, IRExpr* src, UInt field_idx )
{
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   vassert( field_idx < 8 );
   vassert( reg < PPC32_SPR_MAX );
   
   if (field_idx != 0) {
      src = binop(Iop_Shl32, src, mkU8(toUChar(field_idx * 4)));
   }   
   putReg_masked( reg, src, (0xF << (field_idx*4)) );
}

/* Write least-significant bit of src to reg[bit_idx] */
static void putReg_bit ( PPC32SPR reg, IRExpr* src, UInt bit_idx )
{
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   vassert( bit_idx < 32 );
   vassert( reg < PPC32_SPR_MAX );
   
   if (bit_idx != 0) {
      src = binop(Iop_Shl32, src, mkU8(toUChar(bit_idx)));
   }   
   putReg_masked( reg, src, (1<<bit_idx) );
}








/*------------------------------------------------------------*/
/*--- Integer Instruction Translation                     --- */
/*------------------------------------------------------------*/

/*
  Integer Arithmetic Instructions
*/
static Bool dis_int_arith ( UInt theInstr )
{
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar Rd_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   
   /* D-Form */
   UInt  SIMM_16 =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
   
   /* XO-Form */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UChar flag_OE = toUChar((theInstr >> 10) & 1);     /* theInstr[10]    */
   UInt  opc2    =         (theInstr >>  1) & 0x1FF;  /* theInstr[1:9]   */
   UChar flag_Rc = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */
   
   UInt EXTS_SIMM = 0;
   
   IRTemp Ra     = newTemp(Ity_I32);
   IRTemp Rb     = newTemp(Ity_I32);
   IRTemp Rd     = newTemp(Ity_I32);
   IRTemp res64  = newTemp(Ity_I64);  // multiplies need this.
   IRTemp xer_ca = newTemp(Ity_I32);

   UInt flag_op = PPC32G_FLAG_OP_NUMBER;
   Bool do_rc = False;

   assign( Ra, getIReg(Ra_addr) );
   assign( Rb, getIReg(Rb_addr) );         // XO-Form: Rd, Ra, Rb
   EXTS_SIMM = extend_s_16to32(SIMM_16);   // D-Form:  Rd, Ra, EXTS(SIMM)

   assign( xer_ca, getReg_bit( PPC32_SPR_XER, SHIFT_XER_CA ) );
   
   switch (opc1) {
   /* D-Form */
   case 0x0C: // addic  (Add Immediate Carrying, PPC32 p351
      DIP("addic r%d,r%d,0x%x\n", Rd_addr, Ra_addr, EXTS_SIMM);
      assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
      flag_op = PPC32G_FLAG_OP_ADD;
      setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(EXTS_SIMM) );
      break;
      
   case 0x0D: // addic. (Add Immediate Carrying and Record, PPC32 p352)
      DIP("addic. r%d,r%d,0x%x\n", Rd_addr, Ra_addr, EXTS_SIMM);
      assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
      flag_op = PPC32G_FLAG_OP_ADD;
      setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(EXTS_SIMM) );
      do_rc = True;  // Always record to CR
      flag_Rc = 1;
      break;

   case 0x0E: // addi   (Add Immediate, PPC32 p350)
      // li rD,val   == addi rD,0,val
      // la disp(rA) == addi rD,rA,disp
      DIP("addi r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
      if ( Ra_addr == 0 ) {
         assign( Rd, mkU32(EXTS_SIMM) );
      } else {
         assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
      }
      break;

   case 0x0F: // addis  (Add Immediate Shifted, PPC32 p353)
      // lis rD,val == addis rD,0,val
      DIP("addis r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
      if ( Ra_addr == 0 ) {
         assign( Rd, mkU32(SIMM_16 << 16) );
      } else {
         assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkU32(SIMM_16 << 16)) );
      }
      break;

   case 0x07: // mulli    (Multiply Low Immediate, PPC32 p490)
      DIP("mulli r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
      assign( res64, binop(Iop_MullS32, mkexpr(Ra), mkU32(EXTS_SIMM)) );
      assign( Rd, unop(Iop_64to32, mkexpr(res64)) );
      break;

   case 0x08: // subfic   (Subtract from Immediate Carrying, PPC32 p540)
      DIP("subfic r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
      // rD = exts_simm - rA
      assign( Rd, binop(Iop_Sub32, mkU32(EXTS_SIMM), mkexpr(Ra)) );
      flag_op = PPC32G_FLAG_OP_SUBFI;
      setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(EXTS_SIMM) );
      break;


   /* XO-Form */
   case 0x1F:
      do_rc = True;    // All below record to CR
      
      switch (opc2) {
      case 0x10A: // add  (Add, PPC32 p347)
         DIP("add%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
         if (flag_OE) {
            flag_op = PPC32G_FLAG_OP_ADD;
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x00A: // addc      (Add Carrying, PPC32 p348)
         DIP("addc%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
         flag_op = PPC32G_FLAG_OP_ADD;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
         
      case 0x08A: // adde      (Add Extended, PPC32 p349)
         DIP("adde%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rA + rB + XER[CA]
         assign( Rd, binop(Iop_Add32, mkexpr(Ra),
                           binop(Iop_Add32, mkexpr(Rb), mkexpr(xer_ca))) );
         flag_op = PPC32G_FLAG_OP_ADDE;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
         
      case 0x0EA: // addme      (Add to Minus One Extended, PPC32 p354)
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(addme,Rb_addr)\n");
            return False;
         }
         DIP("addme%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rA + (-1) + XER[CA]
         // => Just another form of adde
         assign( Rd, binop(Iop_Add32, mkexpr(Ra),
                           binop(Iop_Add32, mkU32(-1), mkexpr(xer_ca)) ));
         flag_op = PPC32G_FLAG_OP_ADDE;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(-1) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(-1) );
         }
         break;
         
      case 0x0CA: // addze      (Add to Zero Extended, PPC32 p355)
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(addze,Rb_addr)\n");
            return False;
         }
         DIP("addze%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rA + (0) + XER[CA]
         // => Just another form of adde
         assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(xer_ca)) );
         flag_op = PPC32G_FLAG_OP_ADDE;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(0) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(0) );
         }
         break;

      case 0x1EB: // divw       (Divide Word, PPC32 p388)
         DIP("divw%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( Rd, binop(Iop_DivS32, mkexpr(Ra), mkexpr(Rb)) );
         if (flag_OE) {
            flag_op = PPC32G_FLAG_OP_DIVW;
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         /* Note:
            if (0x8000_0000 / -1) or (x / 0)
            => Rd=undef, if(flag_Rc) CR7=undef, if(flag_OE) XER_OV=1
            => But _no_ exception raised. */
         break;

      case 0x1CB: // divwu      (Divide Word Unsigned, PPC32 p389)
         DIP("divwu%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( Rd, binop(Iop_DivU32, mkexpr(Ra), mkexpr(Rb)) );
         if (flag_OE) {
            flag_op = PPC32G_FLAG_OP_DIVWU;
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         /* Note: ditto comment divw, for (x / 0) */
         break;

      case 0x04B: // mulhw      (Multiply High Word, PPC32 p488)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(PPC32)(mulhw,flag_OE)\n");
            return False;
         }
         DIP("mulhw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( res64, binop(Iop_MullS32, mkexpr(Ra), mkexpr(Rb)) );
         assign( Rd, unop(Iop_64HIto32, mkexpr(res64)) );
         break;

      case 0x00B: // mulhwu     (Multiply High Word Unsigned, PPC32 p489)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(PPC32)(mulhwu,flag_OE)\n");
            return False;
         }
         DIP("mulhwu%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( res64, binop(Iop_MullU32, mkexpr(Ra), mkexpr(Rb)) );
         assign( Rd, unop(Iop_64HIto32, mkexpr(res64)) );
         break;
         
      case 0x0EB: // mullw      (Multiply Low Word, PPC32 p491)
         DIP("mullw%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( res64, binop(Iop_MullU32, mkexpr(Ra), mkexpr(Rb)) );
         assign( Rd, unop(Iop_64to32, mkexpr(res64)) );
         if (flag_OE) {
            flag_op = PPC32G_FLAG_OP_MULLW;
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x068: // neg        (Negate, PPC32 p493)
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(neg,Rb_addr)\n");
            return False;
         }
         DIP("neg%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr);
         // rD = (log not)rA + 1
         assign( Rd, binop(Iop_Add32,
                           unop(Iop_Not32, mkexpr(Ra)), mkU32(1)) );
         if (flag_OE) {
            flag_op = PPC32G_FLAG_OP_NEG;
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x028: // subf       (Subtract From, PPC32 p537)
         DIP("subf%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rB - rA
         assign( Rd, binop(Iop_Sub32, mkexpr(Rb), mkexpr(Ra)) );
         if (flag_OE) {
            flag_op = PPC32G_FLAG_OP_SUBF;
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x008: // subfc      (Subtract from Carrying, PPC32 p538)
         DIP("subfc%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rB - rA
         assign( Rd, binop(Iop_Sub32, mkexpr(Rb), mkexpr(Ra)) );
         flag_op = PPC32G_FLAG_OP_SUBFC;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
         
      case 0x088: // subfe      (Subtract from Extended, PPC32 p539)
         DIP("subfe%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = (log not)rA + rB + XER[CA]
         assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
                           binop(Iop_Add32, mkexpr(Rb), mkexpr(xer_ca))) );
         flag_op = PPC32G_FLAG_OP_SUBFE;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
         
      case 0x0E8: // subfme     (Subtract from Minus One Extended, PPC32 p541)
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(subfme,Rb_addr)\n");
            return False;
         }
         DIP("subfme%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr);
         // rD = (log not)rA + (-1) + XER[CA]
         // => Just another form of subfe
         assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
                           binop(Iop_Add32, mkU32(-1), mkexpr(xer_ca))) );
         flag_op = PPC32G_FLAG_OP_SUBFE;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(-1) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(-1) );
         }
         break;
         
      case 0x0C8: // subfze     (Subtract from Zero Extended, PPC32 p542)
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(subfze,Rb_addr)\n");
            return False;
         }
         DIP("subfze%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr);
         // rD = (log not)rA + (0) + XER[CA]
         // => Just another form of subfe
         assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)), mkexpr(xer_ca)) );
         flag_op = PPC32G_FLAG_OP_SUBFE;
         setFlags_XER_CA( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(0) );
         if (flag_OE) {
            setFlags_XER_OV_SO( flag_op, mkexpr(Rd), mkexpr(Ra), mkU32(0) );
         }
         break;

      default:
         vex_printf("dis_int_arith(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_arith(PPC32)(opc1)\n");
      return False;
   }

   putIReg( Rd_addr, mkexpr(Rd) );
   if (do_rc && flag_Rc) {
      setFlags_CR7( mkexpr(Rd) );
   }
   return True;
}



/*
  Integer Compare Instructions
*/
static Bool dis_int_cmp ( UInt theInstr )
{
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar crfD    = toUChar((theInstr >> 23) & 0x7);   /* theInstr[23:25] */
   UChar b9      = toUChar((theInstr >> 22) & 0x1);   /* theInstr[22]    */
   UChar flag_L  = toUChar((theInstr >> 21) & 0x1);   /* theInstr[21]    */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   
   /* D-Form */
   UInt  SIMM_16 =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
   UInt  UIMM_16 =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
   
   /* X-Form */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF;  /* theInstr[1:10]  */
   UChar b0      = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */
   
   UInt EXTS_SIMM = 0;
   IRTemp Ra      = newTemp(Ity_I32);
   IRTemp Rb      = newTemp(Ity_I32);
   IRTemp xer_so  = newTemp(Ity_I32);
   IRTemp cr7     = newTemp(Ity_I32);
   IRTemp mux1    = newTemp(Ity_I32);
   IRTemp mux2    = newTemp(Ity_I32);
   IRExpr* irx_cmp_lt;
   IRExpr* irx_cmp_eq;

   assign( Ra, getIReg(Ra_addr) );
   
   if (flag_L==1) {  // L==1 invalid for 32 bit.
      vex_printf("dis_int_cmp(PPC32)(flag_L)\n");
      return False;
   }
   
   if (b9 != 0) {
      vex_printf("dis_int_cmp(PPC32)(b9)\n");
      return False;
   }
   
   switch (opc1) {
   case 0x0B: // cmpi (Compare Immediate, PPC32 p368)
      EXTS_SIMM = extend_s_16to32(SIMM_16);
      DIP("cmpi crf%d,%d,r%d,0x%x\n", crfD, flag_L, Ra_addr, EXTS_SIMM);
      irx_cmp_lt = binop(Iop_CmpLT32S, mkexpr(Ra), mkU32(EXTS_SIMM));
      irx_cmp_eq = binop(Iop_CmpEQ32, mkexpr(Ra), mkU32(EXTS_SIMM));
      break;
          
   case 0x0A: // cmpli (Compare Logical Immediate, PPC32 p370)
      DIP("cmpli crf%d,%d,r%d,0x%x\n", crfD, flag_L, Ra_addr, UIMM_16);
      irx_cmp_lt = binop(Iop_CmpLT32U, mkexpr(Ra), mkU32(UIMM_16));
      irx_cmp_eq = binop(Iop_CmpEQ32, mkexpr(Ra), mkU32(UIMM_16));
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_cmp(PPC32)(0x1F,b0)\n");
         return False;
      }
      assign( Rb, getIReg(Rb_addr) );
      irx_cmp_eq = binop(Iop_CmpEQ32, mkexpr(Ra), mkexpr(Rb));

      switch (opc2) {
      case 0x000: // cmp (Compare, PPC32 p367)
         DIP("cmp crf%d,%d,r%d,r%d\n", crfD, flag_L,
             Ra_addr, Rb_addr);
         irx_cmp_lt = binop(Iop_CmpLT32S, mkexpr(Ra), mkexpr(Rb));
         break;
         
       case 0x020: // cmpl (Compare Logical, PPC32 p369)
          DIP("cmpl crf%d,%d,r%d,r%d\n", crfD, flag_L,
              Ra_addr, Rb_addr);
          irx_cmp_lt = binop(Iop_CmpLT32U, mkexpr(Ra), mkexpr(Rb));
          break;

       default:
          vex_printf("dis_int_cmp(PPC32)(opc2)\n");
          return False;
      }
      break;

   default:
      vex_printf("dis_int_cmp(PPC32)(opc1)\n");
      return False;
   }
   
   irx_cmp_lt = unop(Iop_1Uto8, irx_cmp_lt);
   irx_cmp_eq = unop(Iop_1Uto8, irx_cmp_eq);

   // mux_shift_bit = (argL < argR) ? LT : GT (or EQ...)
   assign( mux1, IRExpr_Mux0X( irx_cmp_lt, mkU32(SHIFT_CR_GT), mkU32(SHIFT_CR_LT) ));

   // mux_shift_bit = (argL == argR) ? EQ : GT|LT
   assign( mux2, IRExpr_Mux0X( irx_cmp_eq, mkexpr(mux1), mkU32(SHIFT_CR_EQ) ));
   
   assign( xer_so, getReg_bit( PPC32_SPR_XER, SHIFT_XER_SO ) );
   assign( cr7, binop(Iop_Or32, mkexpr(mux2), mkexpr(xer_so)) );
   putReg_field( PPC32_SPR_CR, mkexpr(cr7), 7-crfD );
   return True;
}


/*
  Integer Logical Instructions
*/
static Bool dis_int_logic ( UInt theInstr )
{
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar Rs_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   
   /* D-Form */
   UInt  UIMM_16 =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
   
   /* X-Form */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF;  /* theInstr[1:10]  */
   UChar flag_Rc = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */
   
   Bool do_rc = False;
   
   IRTemp Rs = newTemp(Ity_I32);
   IRTemp Ra = newTemp(Ity_I32);
   IRTemp Rb = newTemp(Ity_I32);
   IRTemp Sign = newTemp(Ity_I32);
   IRExpr* irx;
   
   assign( Rs, getIReg(Rs_addr) );
   assign( Rb, getIReg(Rb_addr) );
   
   switch (opc1) {
   case 0x1C: // andi. (AND Immediate, PPC32 p358)
      DIP("andi. r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
      assign( Ra, binop(Iop_And32, mkexpr(Rs), mkU32(UIMM_16)) );
      do_rc = True;  // Always record to CR
      flag_Rc = 1;
      break;
      
   case 0x1D: // andis. (AND Immediate Shifted, PPC32 p359)
      DIP("andis r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
      assign( Ra, binop(Iop_And32, mkexpr(Rs), mkU32(UIMM_16 << 16)) );
      do_rc = True;  // Always record to CR
      flag_Rc = 1;
      break;

   case 0x18: // ori (OR Immediate, PPC32 p497)
      DIP("ori r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
      assign( Ra, binop(Iop_Or32, mkexpr(Rs), mkU32(UIMM_16)) );
      break;

   case 0x19: // oris (OR Immediate Shifted, PPC32 p498)
      DIP("oris r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
      assign( Ra, binop(Iop_Or32, mkexpr(Rs), mkU32(UIMM_16 << 16)) );
      break;

   case 0x1A: // xori (XOR Immediate, PPC32 p550)
      DIP("xori r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
      assign( Ra, binop(Iop_Xor32, mkexpr(Rs), mkU32(UIMM_16)) );
      break;

   case 0x1B: // xoris (XOR Immediate Shifted, PPC32 p551)
      DIP("xoris r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
      assign( Ra, binop(Iop_Xor32, mkexpr(Rs), mkU32(UIMM_16 << 16)) );
      break;

   /* X Form */
   case 0x1F:
      do_rc = True;    // All below record to CR

      switch (opc2) {
      case 0x01C: // and (AND, PPC32 p356)
         DIP("and%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign(Ra, binop(Iop_And32, mkexpr(Rs), mkexpr(Rb)));
         break;
         
      case 0x03C: // andc (AND with Complement, PPC32 p357)
         DIP("andc%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign(Ra, binop(Iop_And32, mkexpr(Rs),
                          unop(Iop_Not32, mkexpr(Rb))));
         break;
         
      case 0x01A: // cntlzw (Count Leading Zeros Word, PPC32 p371)
         if (Rb_addr!=0) {
            vex_printf("dis_int_logic(PPC32)(cntlzw,Rb_addr)\n");
            return False;
         }
         DIP("cntlzw%s r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr);
         
         // Iop_Clz32 undefined for arg==0, so deal with that case:
         irx =  binop(Iop_CmpNE32, mkexpr(Rs), mkU32(0));
         assign(Ra, IRExpr_Mux0X( unop(Iop_1Uto8, irx),
                                  mkU32(32),
                                  unop(Iop_Clz32, mkexpr(Rs)) ));
         break;

      case 0x11C: // eqv (Equivalent, PPC32 p396)
         DIP("eqv%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign( Ra, unop(Iop_Not32, binop(Iop_Xor32,
                                           mkexpr(Rs), mkexpr(Rb))) );
         break;

      case 0x3BA: // extsb (Extend Sign Byte, PPC32 p397
         if (Rb_addr!=0) {
            vex_printf("dis_int_logic(PPC32)(extsb,Rb_addr)\n");
            return False;
         }
         DIP("extsb%s r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr);
         assign( Sign, binop(Iop_And32, mkU32(0x80), mkexpr(Rs)) );
         irx = binop(Iop_CmpEQ32, mkexpr(Sign), mkU32(0));
         assign( Ra, IRExpr_Mux0X(
                    unop(Iop_1Uto8, irx),
                    binop(Iop_Or32,  mkU32(0xFFFFFF00), mkexpr(Rs)),
                    binop(Iop_And32, mkU32(0x000000FF), mkexpr(Rs)) ));
         break;

      case 0x39A: // extsh (Extend Sign Half Word, PPC32 p398)
         if (Rb_addr!=0) {
            vex_printf("dis_int_logic(PPC32)(extsh,Rb_addr)\n");
            return False;
         }
         DIP("extsh%s r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr);
         assign( Sign, binop(Iop_And32, mkU32(0x8000), mkexpr(Rs)) );
         irx = binop(Iop_CmpEQ32, mkexpr(Sign), mkU32(0));
         assign( Ra, IRExpr_Mux0X(
                    unop(Iop_1Uto8, irx),
                    binop(Iop_Or32,  mkU32(0xFFFF0000), mkexpr(Rs)),
                    binop(Iop_And32, mkU32(0x0000FFFF), mkexpr(Rs)) ));
         break;

      case 0x1DC: // nand (NAND, PPC32 p492)
         DIP("nand%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign( Ra, unop(Iop_Not32,
                          binop(Iop_And32, mkexpr(Rs), mkexpr(Rb))) );
         break;
         
      case 0x07C: // nor (NOR, PPC32 p494)
         DIP("nor%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign( Ra, unop(Iop_Not32,
                          binop(Iop_Or32, mkexpr(Rs), mkexpr(Rb))) );
         break;

      case 0x1BC: // or (OR, PPC32 p495)
         DIP("or%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign( Ra, binop(Iop_Or32, mkexpr(Rs), mkexpr(Rb)) );
         break;

      case 0x19C: // orc  (OR with Complement, PPC32 p496)
         DIP("orc%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign( Ra, binop(Iop_Or32, mkexpr(Rs),
                           unop(Iop_Not32, mkexpr(Rb))) );
         break;
         
      case 0x13C: // xor (XOR, PPC32 p549)
         DIP("xor%s r%d,r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
         assign( Ra, binop(Iop_Xor32, mkexpr(Rs), mkexpr(Rb)) );
         break;

      default:
         vex_printf("dis_int_logic(PPC32)(opc2)\n");
         return False;
      }
      break;
      
   default:
      vex_printf("dis_int_logic(PPC32)(opc1)\n");
      return False;
   }

   putIReg( Ra_addr, mkexpr(Ra) );
   if (do_rc && flag_Rc) {
      setFlags_CR7( mkexpr(Ra) );
   }
   return True;
}



/*
  Integer Rotate Instructions
*/
static Bool dis_int_rot ( UInt theInstr )
{
   /* M-Form */
   UChar opc1      = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar Rs_addr   = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Ra_addr   = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar Rb_addr   = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UChar sh_imm    = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UChar MaskBegin = toUChar((theInstr >>  6) & 0x1F); /* theInstr[6:10]  */
   UChar MaskEnd   = toUChar((theInstr >>  1) & 0x1F); /* theInstr[1:5]   */
   UChar flag_Rc   = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   UInt mask = MASK(31-MaskEnd, 31-MaskBegin);
   IRTemp rot_amt = newTemp(Ity_I8);
   IRTemp Rs = newTemp(Ity_I32);
   IRTemp Ra = newTemp(Ity_I32);
   IRTemp Rb = newTemp(Ity_I32);
   
   assign( Rs, getIReg(Rs_addr) );
   assign( Rb, getIReg(Rb_addr) );
      
   switch (opc1) {
   case 0x14: // rlwimi (Rotate Left Word Immediate then Mask Insert, PPC32 p500)
      DIP("rlwimi%s r%d,r%d,%d,%d,%d\n", flag_Rc ? "." : "",
          Ra_addr, Rs_addr, sh_imm, MaskBegin, MaskEnd);
      // Ra = (ROTL(Rs, Imm) & mask) | (Ra & ~mask);
      assign( Ra, binop(Iop_Or32,
                        binop(Iop_And32, mkU32(mask),
                              ROTL32(mkexpr(Rs), mkU8(sh_imm))),
                        binop(Iop_And32, getIReg(Ra_addr), mkU32(~mask))) );
      break;

   case 0x15: // rlwinm (Rotate Left Word Immediate then AND with Mask, PPC32 p501)
      DIP("rlwinm%s r%d,r%d,%d,%d,%d\n", flag_Rc ? "." : "",
          Ra_addr, Rs_addr, sh_imm, MaskBegin, MaskEnd);
      // Ra = ROTL(Rs, Imm) & mask
      assign( Ra, binop(Iop_And32, ROTL32(mkexpr(Rs),
                                          mkU8(sh_imm)), mkU32(mask)) );
      break;

   case 0x17: // rlwnm (Rotate Left Word then AND with Mask, PPC32 p503
      DIP("rlwnm%s r%d,r%d,r%d,%d,%d\n", flag_Rc ? "." : "",
          Ra_addr, Rs_addr, Rb_addr, MaskBegin, MaskEnd);
      // Ra = ROTL(Rs, Rb[0-4]) & mask
      assign( rot_amt,
              unop(Iop_32to8, binop(Iop_And32, mkexpr(Rb), mkU32(0x1F))) );
      assign( Ra, binop(Iop_And32,
                        ROTL32(mkexpr(Rs), mkexpr(rot_amt)), mkU32(mask)) );
      break;

   default:
      vex_printf("dis_int_rot(PPC32)(opc1)\n");
      return False;
   }

   putIReg( Ra_addr, mkexpr(Ra) );
   if (flag_Rc) {
      setFlags_CR7( mkexpr(Ra) );
   }
   return True;
}



/*
  Integer Load Instructions
*/
static Bool dis_int_load ( UInt theInstr )
{
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar Rd_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   
   /* D-Form */
   UInt  d_imm   =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15] */
   
   /* X-Form */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF;  /* theInstr[1:10]  */
   UChar b0      = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */
   
   UInt exts_d_imm = extend_s_16to32(d_imm);
   
   IRTemp Ra_or_0 = newTemp(Ity_I32);
   IRTemp EA_imm  = newTemp(Ity_I32);
   IRTemp EA_reg  = newTemp(Ity_I32);
   IRTemp Ra      = newTemp(Ity_I32);
   IRTemp Rb      = newTemp(Ity_I32);
   
   assign( Ra, getIReg(Ra_addr) );
   assign( Rb, getIReg(Rb_addr) );
   
   assign( Ra_or_0, ((Ra_addr == 0) ? mkU32(0) : mkexpr(Ra)) );

   assign( EA_imm, binop(Iop_Add32, mkexpr(Ra_or_0), mkU32(exts_d_imm)) );
   
   switch (opc1) {
   case 0x22: // lbz (Load B & Zero, PPC32 p433)
      DIP("lbz r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_8Uto32,
                             loadBE(Ity_I8, mkexpr(EA_imm))) );
      break;
      
   case 0x23: // lbzu (Load B & Zero with Update, PPC32 p434)
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lbzu,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lbzu r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_8Uto32,
                             loadBE(Ity_I8, mkexpr(EA_imm))) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;
      
   case 0x2A: // lha (Load HW Algebraic, PPC32 p445)
      DIP("lha r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Sto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      break;

   case 0x2B: // lhau (Load HW Algebraic with Update, PPC32 p446)
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lhau,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lhau r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Sto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;
      
   case 0x28: // lhz (Load HW & Zero, PPC32 p450)
      DIP("lhz r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Uto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      break;
      
   case 0x29: // lhzu (Load HW & and Zero with Update, PPC32 p451)
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lhzu,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lhzu r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Uto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;

   case 0x20: // lwz (Load W & Zero, PPC32 p460)
      DIP("lwz r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
      break;
      
   case 0x21: // lwzu (Load W & Zero with Update, PPC32 p461))
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lwzu,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lwzu r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_load(PPC32)(Ox1F,b0)\n");
         return False;
      }
      assign( EA_reg, binop(Iop_Add32, mkexpr(Ra_or_0), mkexpr(Rb)) );

      switch (opc2) {
      case 0x077: // lbzux (Load B & Zero with Update Indexed, PPC32 p435)
         DIP("lbzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         if (Ra_addr == 0 || Ra_addr == Rd_addr) {
            vex_printf("dis_int_load(PPC32)(lwzux,Ra_addr|Rd_addr)\n");
            return False;
         }
         putIReg( Rd_addr, unop(Iop_8Uto32,
                                loadBE(Ity_I8, mkexpr(EA_reg))) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;
         
      case 0x057: // lbzx (Load B & Zero Indexed, PPC32 p436)
         DIP("lbzx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, unop(Iop_8Uto32,
                                loadBE(Ity_I8, mkexpr(EA_reg))) );
         break;
         
      case 0x177: // lhaux (Load HW Algebraic with Update Indexed, PPC32 p447)
         if (Ra_addr == 0 || Ra_addr == Rd_addr) {
            vex_printf("dis_int_load(PPC32)(lhaux,Ra_addr|Rd_addr)\n");
            return False;
         }
         DIP("lhaux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, unop(Iop_16Sto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;
         
      case 0x157: // lhax (Load HW Algebraic Indexed, PPC32 p448)
         DIP("lhax r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, unop(Iop_16Sto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         break;
         
      case 0x137: // lhzux (Load HW & Zero with Update Indexed, PPC32 p452)
         if (Ra_addr == 0 || Ra_addr == Rd_addr) {
            vex_printf("dis_int_load(PPC32)(lhzux,Ra_addr|Rd_addr)\n");
            return False;
         }
         DIP("lhzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, unop(Iop_16Uto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;
         
      case 0x117: // lhzx (Load HW & Zero Indexed, PPC32 p453)
         DIP("lhzx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, unop(Iop_16Uto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         break;

      case 0x037: // lwzux (Load W & Zero with Update Indexed, PPC32 p462)
         if (Ra_addr == 0 || Ra_addr == Rd_addr) {
            vex_printf("dis_int_load(PPC32)(lwzux,Ra_addr|Rd_addr)\n");
            return False;
         }
         DIP("lwzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_reg)) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;
         
      case 0x017: // lwzx (Load W & Zero Indexed, PPC32 p463)
         DIP("lwzx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_reg)) );
         break;

      default:
         vex_printf("dis_int_load(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_load(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Store Instructions
*/
static Bool dis_int_store ( UInt theInstr )
{
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar Ra_addr  = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   
   /* D-Form */
   UInt  d_imm   =          (theInstr >>  0) & 0xFFFF; /* theInstr[0:15] */
   
   /* X-Form */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F);   /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF;   /* theInstr[1:10]  */
   UChar b0      = toUChar((theInstr >>  0) & 1);      /* theInstr[0]     */
   
   UInt exts_d_imm = extend_s_16to32(d_imm);
   
   IRTemp Ra      = newTemp(Ity_I32);
   IRTemp Ra_or_0 = newTemp(Ity_I32);
   IRTemp Rb      = newTemp(Ity_I32);
   IRTemp Rs      = newTemp(Ity_I32);
   IRTemp Rs_8    = newTemp(Ity_I8);
   IRTemp Rs_16   = newTemp(Ity_I16);
   IRTemp EA_imm  = newTemp(Ity_I32);
   IRTemp EA_reg  = newTemp(Ity_I32);
   
   assign( Ra, getIReg(Ra_addr) );
   assign( Rb, getIReg(Rb_addr) );
   assign( Rs, getIReg(Rs_addr) );
   assign( Rs_8, unop(Iop_32to8, mkexpr(Rs)) );
   assign( Rs_16, unop(Iop_32to16, mkexpr(Rs)) );
   
   if (Ra_addr == 0) {
      assign( Ra_or_0, mkU32(0) );
   } else {
      assign( Ra_or_0, mkexpr(Ra) );
   }
   assign( EA_imm, binop(Iop_Add32, mkexpr(Ra_or_0), mkU32(exts_d_imm)) );
   
   switch (opc1) {
   case 0x26: // stb (Store B, PPC32 p509)
      DIP("stb r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      storeBE( mkexpr(EA_imm), mkexpr(Rs_8) );
      break;
      
   case 0x27: // stbu (Store B with Update, PPC32 p510)
      if (Ra_addr == 0 ) {
         vex_printf("dis_int_store(PPC32)(stbu,Ra_addr)\n");
         return False;
      }
      DIP("stbu r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      storeBE( mkexpr(EA_imm), mkexpr(Rs_8) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;

   case 0x2C: // sth (Store HW, PPC32 p522)
      DIP("sth r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      storeBE( mkexpr(EA_imm), mkexpr(Rs_16) );
      break;
      
   case 0x2D: // sthu (Store HW with Update, PPC32 p524)
      if (Ra_addr == 0) {
         vex_printf("dis_int_store(PPC32)(sthu,Ra_addr)\n");
         return False;
      }
      DIP("sthu r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      assign( Rs_16, binop(Iop_And16, mkexpr(Rs), mkU16(0xFFFF)) );
      storeBE( mkexpr(EA_imm), mkexpr(Rs_16) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;

   case 0x24: // stw (Store W, PPC32 p530)
      DIP("stw r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      storeBE( mkexpr(EA_imm), mkexpr(Rs) );
      break;

   case 0x25: // stwu (Store W with Update, PPC32 p534)
      if (Ra_addr == 0) {
         vex_printf("dis_int_store(PPC32)(stwu,Ra_addr)\n");
         return False;
      }
      DIP("stwu r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      storeBE( mkexpr(EA_imm), mkexpr(Rs) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_store(PPC32)(0x1F,b0)\n");
         return False;
      }
      assign( EA_reg, binop(Iop_Add32, mkexpr(Ra_or_0), mkexpr(Rb)) );

      switch (opc2) {
      case 0x0F7: // stbux (Store B with Update Indexed, PPC32 p511)
         if (Ra_addr == 0) {
            vex_printf("dis_int_store(PPC32)(stbux,Ra_addr)\n");
            return False;
         }
         DIP("stbux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), mkexpr(Rs_8) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;
         
      case 0x0D7: // stbx (Store B Indexed, PPC32 p512)
         DIP("stbx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), mkexpr(Rs_8) );
         break;
         
      case 0x1B7: // sthux (Store HW with Update Indexed, PPC32 p525)
         if (Ra_addr == 0) {
            vex_printf("dis_int_store(PPC32)(sthux,Ra_addr)\n");
            return False;
         }
         DIP("sthux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), mkexpr(Rs_16) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;
         
      case 0x197: // sthx (Store HW Indexed, PPC32 p526)
         DIP("sthx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), mkexpr(Rs_16) );
         break;
         
      case 0x0B7: // stwux (Store W with Update Indexed, PPC32 p535)
         if (Ra_addr == 0) {
            vex_printf("dis_int_store(PPC32)(stwux,Ra_addr)\n");
            return False;
         }
         DIP("stwux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), mkexpr(Rs) );
         putIReg( Ra_addr, mkexpr(EA_reg) );
         break;

      case 0x097: // stwx (Store W Indexed, PPC32 p536)
         DIP("stwx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), mkexpr(Rs) );
         break;
         
      default:
         vex_printf("dis_int_store(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_store(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Load/Store Multiple Instructions
*/
static Bool dis_int_ldst_mult ( UInt theInstr )
{
   /* D-Form */
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar Rd_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar Rs_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   UInt  d_imm   =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
   
   UInt exts_d_imm = extend_s_16to32(d_imm);
   UInt reg_idx    = 0;
   UInt offset     = 0;
   
   IRTemp Ra = newTemp(Ity_I32);
   IRTemp EA = newTemp(Ity_I32);
   
   IRExpr* irx_addr;
   
   if (Ra_addr == 0) {
      assign( EA, binop(Iop_And32, mkU32(0), mkU32(exts_d_imm)) );
   } else {
      assign( Ra, getIReg(Ra_addr) );
      assign( EA, binop(Iop_And32, mkexpr(Ra), mkU32(exts_d_imm)) );
   }
   
   switch (opc1) {
   case 0x2E: // lmw (Load Multiple Word, PPC32 p454)
      if (Ra_addr >= reg_idx) {
         vex_printf("dis_int_ldst_mult(PPC32)(lmw,Ra_addr)\n");
         return False;
      }
      DIP("lmw r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
      for (reg_idx = Rd_addr; reg_idx<=31; reg_idx++) {
         irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(offset));
         putIReg( reg_idx, loadBE(Ity_I32, irx_addr ) );
         offset +=4;
      }
      break;
      
   case 0x2F: // stmw (Store Multiple Word, PPC32 p527)
      DIP("stmw r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
      for (reg_idx = Rs_addr; reg_idx<=31; reg_idx++) {
         irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(offset));
         storeBE( irx_addr, getIReg(reg_idx) );
         offset +=4;
      }
      break;
      
   default:
      vex_printf("dis_int_ldst_mult(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Load/Store String Instructions
*/
static Bool dis_int_ldst_str ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Ra_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar NumBytes = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UChar Rb_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   UInt reg_idx, bit_idx, n_byte;
   UInt EA_offset = 0;
   UInt n_regs, reg_first, reg_last;
   
   IRTemp Ra = newTemp(Ity_I32);
//    IRTemp Rb = newTemp(Ity_I32);
   IRTemp EA = newTemp(Ity_I32);
   IRTemp b_EA = newTemp(Ity_I32);
   IRExpr* irx_byte;
   IRExpr* irx_shl;
   
   if (Ra_addr == 0) {
      assign( b_EA, mkU32(0) );
   } else {
      assign( Ra, getIReg(Ra_addr) );
      assign( b_EA, mkexpr(Ra) );
   }    
   
   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_int_ldst_str(PPC32)(opc1)\n");
      return False;
   }

   switch (opc2) {
   case 0x255: // lswi (Load String Word Immediate, PPC32 p455)
      n_regs = (NumBytes / 4) + (NumBytes%4 == 0 ? 0:1); // ceil(nb/4)
      reg_first = Rd_addr;
      reg_last = Rd_addr + n_regs - 1;
      
      if (reg_last < reg_first) {
         if (Ra_addr >= reg_first || Ra_addr <= reg_last) {
            vex_printf("dis_int_ldst_str(PPC32)(lswi,Ra_addr,1)\n");
            return False;
         }
      } else {
         if (Ra_addr >= reg_first && Ra_addr <= reg_last) {
            vex_printf("dis_int_ldst_str(PPC32)(lswi,Ra_addr,2)\n");
            return False;
         }
      }
      DIP("lswi r%d,r%d,%d\n", Rd_addr, Ra_addr, NumBytes);
      
      assign( EA, mkexpr(b_EA) );
      
      bit_idx = 0;
      reg_idx = Rd_addr - 1;
      n_byte = NumBytes;
      if (n_byte == 0) { n_byte = 32; }
      
      for (; n_byte>0; n_byte--) {
         if (bit_idx == 0) {
            reg_idx++;
            if (reg_idx == 32) reg_idx = 0;
            putIReg( reg_idx, mkU32(0) );
         }
         irx_byte = loadBE(Ity_I8, binop(Iop_Add32,
                                         mkexpr(EA),
                                         mkU32(EA_offset)));
         irx_shl = binop(Iop_Shl32, irx_byte, 
                         mkU8(toUChar(24 - bit_idx)));
         putIReg( reg_idx, binop(Iop_Or32, getIReg(reg_idx), irx_shl) );
         bit_idx += 8;
         if (bit_idx == 32) { bit_idx = 0; }
         EA_offset++;
      }
      
   case 0x215: // lswx (Load String Word Indexed, PPC32 p456)
      DIP("lswx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
      return False;

   case 0x2D5: // stswi (Store String Word Immediate, PPC32 p528)
      DIP("stswi r%d,r%d,%d\n", Rs_addr, Ra_addr, NumBytes);
      if (Ra_addr == 0) {
         assign( EA, mkU32(0) );
      } else {
         assign( EA, mkexpr(b_EA) );
      }
      
      n_byte = NumBytes;
      if (n_byte == 0) { n_byte = 32; }
      reg_idx = Rs_addr - 1;
      bit_idx = 0;
      
      for (; n_byte>0; n_byte--) {
         if (bit_idx == 0) {
            reg_idx++;
            if (reg_idx==32) reg_idx = 0;
         }
         irx_byte = unop(Iop_32to8,
                         binop(Iop_Shr32,
                               getIReg(reg_idx), 
                               mkU8(toUChar(24 - bit_idx))));
         storeBE( binop(Iop_Add32, mkexpr(EA), mkU32(EA_offset)),
                  irx_byte );
         
         bit_idx += 8;
         if (bit_idx == 32) { bit_idx = 0; }
         EA_offset++;
      }
      break;

   case 0x295: // stswx (Store String Word Indexed, PPC32 p529)
      DIP("stswx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
      return False;
#if 0
// CAB: Might something like this work ?
// won't produce very nice code (ir_ctr will get _rather_ long...), but hey.
// or perhaps arrays of IRTemp...
   assign( NumBytes, AND(get(xer_bc), 0x1F) );
   IRExpr* irx_ea;
   IRExpr* irx_orig_byte;
   IRExpr* irx_tostore;
   IRExpr* ir_ctr = mkU8(0);
   Uint EA_offset = 0;
   UInt start = Rs_addr;
   UInt reg_idx;
   UInt i;
   for (i=0; i<128; i++) {
      bit_idx = (i % 4) * 8;
      reg_idx = (i / 4) + start;
      reg_idx = reg_idx % 32;
      word = getIReg(reg_idx);
      byte = get_byte(word, bit_idx);
      
      irx_ea = (EA + EA_offset);
      irx_orig_byte = loadBE(Ity_I8, irx_ea);
      irx_tostore = IRExpr_Mux0X( (ir_ctr <= NumBytes),
                                  irx_orig_byte,
                                  mkexpr(byte0) );
      storeBE( irx_ea, irx_tostore );
      
      ir_ctr = binop(Iop_And8, ir_ctr, mkU8(1));
      EA_offset++;
   }
   break;
#endif

   default:
      vex_printf("dis_int_ldst_str(PPC32)(opc2)\n");
      return False;
   }
   return True;
}



/*
  Branch helper function
  ok = BO[2] | ((CTR[0] != 0) ^ BO[1])
*/
static IRExpr* branch_ctr_ok( UInt BO )
{
   IRTemp ok = newTemp(Ity_I1);
   IRTemp ctr_0 = newTemp(Ity_I1);
   
   if ((BO >> 2) & 1) {
      assign( ok, mkU1(1) );
   } else {
      assign( ctr_0, unop(Iop_32to1, getReg_bit( PPC32_SPR_CTR, 0 )) );

      if ((BO >> 1) & 1) {
         assign( ok, unop(Iop_Not1, mkexpr(ctr_0)) );
      } else {
         assign( ok, mkexpr(ctr_0) );
      }
   }
   return mkexpr(ok);
}

/*
  Branch helper function
  cond_ok = BO[4] | (CR[BI] == BO[3])
*/
static IRExpr* branch_cond_ok( UInt BO, UInt BI )
{
   IRTemp ok = newTemp(Ity_I1);
   IRTemp cr_bi = newTemp(Ity_I32);
   
   if (BO >> 4) {
      assign( ok, mkU1(1) );
   } else {
      // ok = (CR[31-BI] == BO[3])
      assign( cr_bi, getReg_bit( PPC32_SPR_CR, (31-BI) ) );

      if ((BO >> 3) & 1) {
         assign( ok, binop(Iop_CmpEQ32, mkU32(1), mkexpr(cr_bi)) );
      } else {
         assign( ok, binop(Iop_CmpEQ32, mkU32(0), mkexpr(cr_bi)) );
      }
   }
   return mkexpr(ok);
}



/*
  Integer Branch Instructions
*/
static Bool dis_branch ( UInt theInstr, DisResult *whatNext )
{
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F);    /* theInstr[26:31] */
   UChar BO       = toUChar((theInstr >> 21) & 0x1F);    /* theInstr[21:25] */
   UChar BI       = toUChar((theInstr >> 16) & 0x1F);    /* theInstr[16:20] */
   UInt  BD       =         (theInstr >>  2) & 0x3FFF;   /* theInstr[2:15]  */
   UChar b11to15  = toUChar((theInstr >> 11) & 0x1F);    /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF;    /* theInstr[1:10]  */
   UInt  LI_24    =         (theInstr >>  2) & 0xFFFFFF; /* theInstr[2:25]  */
   UChar flag_AA  = toUChar((theInstr >>  1) & 1);       /* theInstr[1]     */
   UChar flag_LK  = toUChar((theInstr >>  0) & 1);       /* theInstr[0]     */
   
   Int exts_BD = (Int)extend_s_16to32(BD << 2);
   Int exts_LI = (Int)extend_s_24to32(LI_24 << 2);
   
   Addr32 nia = 0;
   
   IRTemp ctr       = newTemp(Ity_I32);
   IRTemp lr        = newTemp(Ity_I32);
   IRTemp ir_nia    = newTemp(Ity_I32);
   IRTemp do_branch = newTemp(Ity_I32);
   IRTemp ctr_ok    = newTemp(Ity_I1);
   IRTemp cond_ok   = newTemp(Ity_I1);
   
   assign( ctr, getReg( PPC32_SPR_CTR ) );

   /* Hack to pass through code that just wants to read the PC */
   if (theInstr == 0x429F0005) {
      DIP("bcl 0x%x, 0x%x,\n", BO, BI);
      putReg( PPC32_SPR_LR, mkU32(guest_cia_curr_instr + 4) );
      return True;
    }
    
   switch (opc1) {
   case 0x12: // b     (Branch, PPC32 p360)
      if (flag_AA) {
         nia = (UInt)exts_LI;
      } else {
         nia = (UInt)((Int)guest_cia_curr_instr + exts_LI);
      }
      DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", nia);

      if (flag_LK) {
         putReg( PPC32_SPR_LR, mkU32(guest_cia_curr_instr+4) );
      }      
      irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
      irbb->next     = mkU32(nia);
      break;
      
   case 0x10: // bc    (Branch Conditional, PPC32 p361)
      DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
          flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, exts_BD);
      
      if (!(BO & 0x4)) {
         putReg( PPC32_SPR_CTR, binop(Iop_Sub32, mkexpr(ctr), mkU32(1)) );
      }
      assign( ctr_ok, branch_ctr_ok( BO ) );
      assign( cond_ok, branch_cond_ok( BO, BI ) );
      
      assign( do_branch, binop(Iop_And32,
                               unop(Iop_1Uto32, mkexpr(ctr_ok)),
                               unop(Iop_1Uto32, mkexpr(cond_ok))) );
      if (flag_AA) {
         nia = (UInt)exts_BD;
      } else {
         nia = (UInt)((Int)guest_cia_curr_instr + exts_BD);
      }
      if (flag_LK) {
         assign( lr, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(do_branch)),
                                   getReg( PPC32_SPR_LR ),
                                   mkU32(guest_cia_curr_instr + 4)));
         putReg( PPC32_SPR_LR, mkexpr(lr) );
      }
      
      stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(do_branch)),
                         flag_LK ? Ijk_Call : Ijk_Boring,
                         IRConst_U32(nia) ));
      
      irbb->jumpkind = Ijk_Boring;
      irbb->next     = mkU32(guest_cia_curr_instr + 4);
      break;
      
   case 0x13:
      if (b11to15!=0) {
         vex_printf("dis_int_branch(PPC32)(0x13,b11to15)\n");
         return False;
      }

      switch (opc2) {
      case 0x210: // bcctr (Branch Cond. to Count Register, PPC32 p363) 
         if ((BO & 0x4) == 0) { // "decrement and test CTR" option invalid
            vex_printf("dis_int_branch(PPC32)(bcctr,BO)\n");
            return False;
         }
         DIP("bcctr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);
         
         assign( cond_ok, branch_cond_ok( BO, BI ) );
         
         assign( ir_nia, binop(Iop_And32, mkU32(0xFFFFFFFC), mkexpr(ctr)) );
         
         if (flag_LK) {
            assign( lr, IRExpr_Mux0X( unop(Iop_1Uto8, mkexpr(cond_ok)),
                                      getReg( PPC32_SPR_LR ),
                                      mkU32(guest_cia_curr_instr + 4)));
            putReg( PPC32_SPR_LR, mkexpr(lr) );
         }
         
         stmt( IRStmt_Exit( unop(Iop_Not1, mkexpr(cond_ok)),
                            Ijk_Boring,
                            IRConst_U32(guest_cia_curr_instr + 4) ));
         
         irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irbb->next     = mkexpr(ir_nia);
         break;
         
      case 0x010: // bclr (Branch Cond. to Link Register, PPC32 p365) 
         DIP("bclr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);

         if (!(BO & 0x4)) {
            putReg( PPC32_SPR_CTR, binop(Iop_Sub32, mkexpr(ctr), mkU32(1)) );
         }
         
         assign( ctr_ok, branch_ctr_ok(BO) );
         assign( cond_ok, branch_cond_ok(BO, BI) );
         
         assign( do_branch, binop(Iop_And32,
                                  unop(Iop_1Uto32, mkexpr(ctr_ok)),
                                  unop(Iop_1Uto32, mkexpr(cond_ok))) );
         
         assign( ir_nia, binop(Iop_And32,
                               getReg( PPC32_SPR_LR ),
                               mkU32(0xFFFFFFFC)) );
         if (flag_LK) {
            assign( lr, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(do_branch)),
                                      getReg( PPC32_SPR_LR ),
                                      mkU32(guest_cia_curr_instr + 4)) );
            putReg( PPC32_SPR_LR, mkexpr(lr) );
         }
         
         stmt( IRStmt_Exit( unop(Iop_Not1, unop(Iop_32to1, mkexpr(do_branch))),
                            Ijk_Boring,
                            IRConst_U32(guest_cia_curr_instr + 4) ));
         
         irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irbb->next     = mkexpr(ir_nia);
         break;
         
      default:
         vex_printf("dis_int_branch(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_branch(PPC32)(opc1)\n");
      return False;
   }
    
   *whatNext = Dis_StopHere;
   return True;
}



/*
  Condition Register Logical Instructions
*/
static Bool dis_cond_logic ( UInt theInstr )
{
   /* XL-Form */
   UChar opc1      = toUChar((theInstr >> 26) & 0x3F);   /* theInstr[26:31] */
   UChar crbD_addr = toUChar((theInstr >> 21) & 0x1F);   /* theInstr[21:25] */
   UChar crfD_addr = toUChar((theInstr >> 23) & 0x7);    /* theInstr[23:25] */
   UChar crbA_addr = toUChar((theInstr >> 16) & 0x1F);   /* theInstr[16:20] */
   UChar crfS_addr = toUChar((theInstr >> 18) & 0x7);    /* theInstr[18:20] */
   UChar crbB_addr = toUChar((theInstr >> 11) & 0x1F);   /* theInstr[11:15] */
   UInt  opc2      =         (theInstr >>  1) & 0x3FF;   /* theInstr[1:10]  */
   UChar b0        = toUChar((theInstr >>  0) & 1);      /* theInstr[0]     */

   IRTemp crbD = newTemp(Ity_I32);
   IRTemp crbA = newTemp(Ity_I32);
   IRTemp crbB = newTemp(Ity_I32);
   IRTemp tmp  = newTemp(Ity_I32);

   if (opc1 != 19 || b0 != 0) {
      vex_printf("dis_cond_logic(PPC32)(opc1)\n");
      return False;
   }

   if (opc2 == 0) {  // mcrf    (Move Cond Reg Field, PPC32 p464)
      if (((crbD_addr & 0x3) != 0) ||
          ((crbA_addr & 0x3) != 0) || (crbB != 0))
         return False;
      DIP("mcrf crf%d,crf%d\n", crfD_addr, crfS_addr);
      assign( tmp,  getReg_field( PPC32_SPR_CR, (7-crfS_addr) ) );
      putReg_field( PPC32_SPR_CR, mkexpr(tmp),  (7-crfD_addr) );
   } else {
      assign( crbA, getReg_bit( PPC32_SPR_CR, (31-crbA_addr) ) );
      assign( crbB, getReg_bit( PPC32_SPR_CR, (31-crbB_addr) ) );

      switch (opc2) {
      case 0x101: // crand   (Cond Reg AND, PPC32 p372)
         DIP("crand crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_And32, mkexpr(crbA), mkexpr(crbB)) );
         break;
      case 0x081: // crandc  (Cond Reg AND w. Complement, PPC32 p373)
         DIP("crandc crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_And32, mkexpr(crbA),
                             unop(Iop_Not32, mkexpr(crbB))) );
         break;
      case 0x121: // creqv   (Cond Reg Equivalent, PPC32 p374)
         DIP("creqv crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_Xor32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x0E1: // crnand  (Cond Reg NAND, PPC32 p375)
         DIP("crnand crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_And32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x021: // crnor   (Cond Reg NOR, PPC32 p376)
         DIP("crnor crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_Or32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x1C1: // cror    (Cond Reg OR, PPC32 p377)
         DIP("cror crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Or32, mkexpr(crbA), mkexpr(crbB)) );
         break;
      case 0x1A1: // crorc   (Cond Reg OR w. Complement, PPC32 p378)
         DIP("crorc crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Or32, mkexpr(crbA),
                             unop(Iop_Not32, mkexpr(crbB))) );
         break;
      case 0x0C1: // crxor   (Cond Reg XOR, PPC32 p379)
         DIP("crxor crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Xor32, mkexpr(crbA), mkexpr(crbB)) );
         break;

      default:
         vex_printf("dis_cond_logic(PPC32)(opc2)\n");
         return False;
      }

      putReg_masked( PPC32_SPR_CR, mkexpr(crbD), 1<<(31-crbD_addr) );
   }
   return True;
}



/*
  System Linkage Instructions
*/
static Bool dis_syslink ( UInt theInstr, DisResult *whatNext )
{
   if (theInstr != 0x44000002) {
      vex_printf("dis_int_syslink(PPC32)(theInstr)\n");
      return False;
   }

   // sc  (System Call, PPC32 p504)
   DIP("sc\n");
   
   /* It's important that all ArchRegs carry their up-to-date value
      at this point.  So we declare an end-of-block here, which
      forces any TempRegs caching ArchRegs to be flushed. */
   irbb->next     = mkU32( guest_cia_curr_instr + 4 );
   irbb->jumpkind = Ijk_Syscall;
   
   *whatNext = Dis_StopHere;
   return True;
}


/*
  Memory Synchronization Instructions
*/
static Bool dis_memsync ( UInt theInstr )
{
   /* X-Form, XL-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F);   /* theInstr[26:31] */
   UInt  b11to25  =         (theInstr >> 11) & 0x7FFF;  /* theInstr[11:25] */
   UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F);   /* theInstr[21:25] */
   UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F);   /* theInstr[21:25] */
   UChar Ra_addr  = toUChar((theInstr >> 16) & 0x1F);   /* theInstr[16:20] */
   UChar Rb_addr  = toUChar((theInstr >> 11) & 0x1F);   /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF;   /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);      /* theInstr[0]     */
   
   IRTemp EA = newTemp(Ity_I32);
   IRTemp Ra = newTemp(Ity_I32);
   IRTemp Rb = newTemp(Ity_I32);
   IRTemp Rs = newTemp(Ity_I32);
   IRTemp xer_so = newTemp(Ity_I32);
   IRTemp cr_f7  = newTemp(Ity_I32);
   
   switch (opc1) {
    /* XL-Form */
   case 0x13:   // isync (Instruction Synchronize, PPC32 p432)
      if (opc2 != 0x096) {
         vex_printf("dis_int_memsync(PPC32)(0x13,opc2)\n");
         return False;
      }
      if (b11to25 != 0 || b0 != 0) {
         vex_printf("dis_int_memsync(PPC32)(0x13,b11to25|b0)\n");
         return False;
      }
      DIP("isync\n");
      
      stmt( IRStmt_MFence() );
      break;

   /* X-Form */
   case 0x1F:
      switch (opc2) {
      case 0x356: // eieio (Enforce In-Order Execution of I/O, PPC32 p394)
         if (b11to25 != 0 || b0 != 0) {
            vex_printf("dis_int_memsync(PPC32)(eiei0,b11to25|b0)\n");
            return False;
         }
         DIP("eieio\n");
         return False;

      case 0x014: // lwarx (Load Word and Reserve Indexed, PPC32 p458)
         /* Note: RESERVE, RESERVE_ADDR not implemented.
            stwcx. is assumed to be always successful
         */
         if (b0 != 0) {
            vex_printf("dis_int_memsync(PPC32)(lwarx,b0)\n");
            return False;
         }
         DIP("lwarx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
         assign( Rb, getIReg(Rb_addr) );
         if (Ra_addr == 0) {
            assign( EA, mkexpr(Rb) );
         } else {
            assign( Ra, getIReg(Ra_addr) );
            assign( EA, binop(Iop_And32, mkexpr(Ra), mkexpr(Rb)) );
         }
         putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA)) );
         break;
         
      case 0x096: // stwcx. (Store Word Conditional Indexed, PPC32 p532)
         /* Note: RESERVE, RESERVE_ADDR not implemented.
            stwcx. is assumed to be always successful
         */
         if (b0 != 1) {
            vex_printf("dis_int_memsync(PPC32)(stwcx.,b0)\n");
            return False;
         }
         DIP("stwcx. r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         
         assign( Rb, getIReg(Rb_addr) );
         assign( Rs, getIReg(Rs_addr) );
         if (Ra_addr == 0) {
            assign( EA, mkexpr(Rb) );
         } else {
            assign( Ra, getIReg(Ra_addr) );
            assign( EA, binop(Iop_And32, mkexpr(Ra), mkexpr(Rb)) );
         }
         storeBE( mkexpr(EA), mkexpr(Rs) );
         
         // Set CR7[LT GT EQ S0] = 0b001 || XER[SO]
         assign( xer_so, getReg_bit( PPC32_SPR_XER, SHIFT_XER_SO ) );
         assign( cr_f7, binop(Iop_Or32, mkU32(2), mkexpr(xer_so)) );
         putReg_field( PPC32_SPR_CR, mkexpr(cr_f7), 7 );
         break;
         
      case 0x256: // sync (Synchronize, PPC32 p543)
         if (b11to25 != 0 || b0 != 0) {
            vex_printf("dis_int_memsync(PPC32)(sync,b11to25|b0)\n");
            return False;
         }
         DIP("sync\n");
         /* Insert a memory fence.  It's sometimes important that these
            are carried through to the generated code. */
         stmt( IRStmt_MFence() );
         break;
         
      default:
         vex_printf("dis_int_memsync(PPC32)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_int_memsync(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Shift Instructions
*/
static Bool dis_int_shift ( UInt theInstr )
{
   /* X-Form */
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar Rs_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UChar sh_imm  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar flag_Rc = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   UInt flag_op  = PPC32G_FLAG_OP_NUMBER;
   
   IRTemp sh_amt = newTemp(Ity_I8);
   IRTemp sign   = newTemp(Ity_I32);
   IRTemp rb_b5  = newTemp(Ity_I32);
   IRTemp sext   = newTemp(Ity_I32);
   IRTemp Rs     = newTemp(Ity_I32);
   IRTemp Rs_sh  = newTemp(Ity_I32);
   IRTemp Rs_msk = newTemp(Ity_I32);
   IRTemp Ra     = newTemp(Ity_I32);
   IRTemp Rb     = newTemp(Ity_I32);
   IRTemp mask   = newTemp(Ity_I32);
   
   assign( Rs, getIReg(Rs_addr) );
   assign( Rb, getIReg(Rb_addr) );
   
   if (opc1 == 0x1F) {
      switch (opc2) {
      case 0x018: // slw (Shift Left Word, PPC32 p505)
         DIP("slw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
             Ra_addr, Rs_addr, Rb_addr);
         assign( sh_amt, binop(Iop_And8, mkU8(0x1F),
                               unop(Iop_32to8, mkexpr(Rb))) );
         assign( Rs_sh, binop(Iop_Shl32, mkexpr(Rs), mkexpr(sh_amt)) );
         assign( rb_b5, binop(Iop_And32, mkexpr(Rb), mkU32(1<<5)) );
         assign( Ra, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(rb_b5)),
                                   mkexpr(Rs_sh), mkU32(0) ));
         break;
         
      case 0x318: // sraw (Shift Right Algebraic Word, PPC32 p506)
         DIP("sraw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
             Ra_addr, Rs_addr, Rb_addr);
         
         assign( sh_amt, binop(Iop_And8, mkU8(0x1F),
                               unop(Iop_32to8, mkexpr(Rb))) );
         // Rs_shift = Rs >> sh_amt
         assign( Rs_sh, binop(Iop_Shr32, mkexpr(Rs), mkexpr(sh_amt)) );
         // rb_b5 = Rb[5]
         assign( rb_b5, binop(Iop_And32, mkexpr(Rb), mkU32(1<<5)) );
         // sign = Rs[31]
         assign( sign, binop(Iop_Shr32, mkexpr(Rs), mkU8(31)) );
         // mask = rb_b5==0 ? (-1 >> sh_amt) : 0
         assign( mask,
                 IRExpr_Mux0X( unop(Iop_32to8, mkexpr(rb_b5)),
                               binop(Iop_Shr32, mkU32(-1), mkexpr(sh_amt)),
                               mkU32(0) ));
         // sign_ext = sign==0 ? 0 : ~mask
         assign( sext, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(sign)),
                                     mkU32(0),
                                     unop(Iop_Not32, mkexpr(mask)) ));
         // Rs_msk = (Rs_sh & mask)
         assign( Rs_msk, binop(Iop_And32, mkexpr(Rs_sh), mkexpr(mask)) );
         // Ra = Rs_msk | sext
         assign( Ra, binop(Iop_Or32, mkexpr(Rs_msk), mkexpr(sext)) );
         flag_op = PPC32G_FLAG_OP_SRAW;
         setFlags_XER_CA( flag_op, mkexpr(Ra), mkexpr(Rs), mkexpr(Rb) );
         break;
         
      case 0x338: // srawi (Shift Right Algebraic Word Immediate, PPC32 p507)
         DIP("srawi%s r%d,r%d,%d\n", flag_Rc ? "." : "",
             Ra_addr, Rs_addr, sh_imm);
         
         assign( sh_amt, mkU8(sh_imm) );
         // Rs_shift = Rs >> sh_amt
         assign( Rs_sh, binop(Iop_Shr32, mkexpr(Rs), mkexpr(sh_amt)) );
         // sign = Rs[31]
         assign( sign, binop(Iop_And32, mkU32(1),
                             binop(Iop_Shr32, mkexpr(Rs), mkU8(31))) );
         // mask = (-1 >> sh_amt)
         assign( mask, binop(Iop_Shr32, mkU32(-1), mkexpr(sh_amt)) );
         // sign_ext = sign==0 ? 0 : ~mask
         assign( sext, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(sign)),
                                     mkU32(0),
                                     unop(Iop_Not32, mkexpr(mask)) ));
         // Ra = Rs_shift | sext
         assign( Ra, binop(Iop_Or32, mkexpr(sext), mkexpr(Rs_sh)) );
         flag_op = PPC32G_FLAG_OP_SRAWI;
         setFlags_XER_CA( flag_op, mkexpr(Ra), mkexpr(Rs), mkU32(sh_imm) );
         break;
      
      case 0x218: // srw (Shift Right Word, PPC32 p508)
         DIP("srw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
             Ra_addr, Rs_addr, Rb_addr);
         assign( sh_amt, binop(Iop_And8, mkU8(0x1F),
                               unop(Iop_32to8, mkexpr(Rb))) );
         assign( Rs_sh, binop(Iop_Shr32, mkexpr(Rs), mkexpr(sh_amt)) );
         assign( rb_b5, binop(Iop_And32, mkexpr(Rb), mkU32(1<<5)) );
         assign( Ra, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(rb_b5)),
                                   mkexpr(Rs_sh), mkU32(0) ));
         break;
         
      default:
         vex_printf("dis_int_shift(PPC32)(opc2)\n");
         return False;
      }
   } else {
      vex_printf("dis_int_shift(PPC32)(opc1)\n");
      return False;
   }

   putIReg( Ra_addr, mkexpr(Ra) );
   
   if (flag_Rc) {
      setFlags_CR7( mkexpr(Ra) );
   }
   return True;
}



/*
  Integer Load/Store Reverse Instructions
*/
static Bool dis_int_ldst_rev ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Ra_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar Rb_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   IRTemp EA    = newTemp(Ity_I32);
   IRTemp Rd    = newTemp(Ity_I32);
   IRTemp Rs    = newTemp(Ity_I32);
   IRTemp byte0 = newTemp(Ity_I32);
   IRTemp byte1 = newTemp(Ity_I32);
   IRTemp byte2 = newTemp(Ity_I32);
   IRTemp byte3 = newTemp(Ity_I32);
   IRTemp tmp16 = newTemp(Ity_I16);
   IRTemp tmp32 = newTemp(Ity_I32);
   
   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_int_ldst_rev(PPC32)(opc1|b0)\n");
      return False;
   }
   
   if (Ra_addr == 0) {
      assign( EA, getIReg(Rb_addr));
   } else {
      assign( EA, binop(Iop_Add32, getIReg(Ra_addr), getIReg(Rb_addr)) );
   }
   
   switch (opc2) {
   case 0x316: // lhbrx (Load Half Word Byte-Reverse Indexed, PPC32 p449)
      DIP("lhbrx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
      assign( byte0, loadBE(Ity_I8, mkexpr(EA)) );
      assign( byte1, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(1))) );
      assign( Rd, binop(Iop_Or32,
                        binop(Iop_Shl32, mkexpr(byte1), mkU8(8)),
                        mkexpr(byte0)) );
      putIReg( Rd_addr, mkexpr(Rd));
      break;
       
   case 0x216: // lwbrx (Load Word Byte-Reverse Indexed, PPC32 p459)
      DIP("lwbrx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
      assign( byte0, loadBE(Ity_I8, mkexpr(EA)) );
      assign( byte1, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(1))) );
      assign( byte2, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(2))) );
      assign( byte3, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(3))) );
      assign( Rd, binop(Iop_Or32,
                        binop(Iop_Or32,
                              binop(Iop_Shl32, mkexpr(byte3), mkU8(24)),
                              binop(Iop_Shl32, mkexpr(byte2), mkU8(16))),
                        binop(Iop_Or32,
                              binop(Iop_Shl32, mkexpr(byte1), mkU8(8)),
                              mkexpr(byte0))) );
      putIReg( Rd_addr, mkexpr(Rd));
      break;
      
   case 0x396: // sthbrx (Store Half Word Byte-Reverse Indexed, PPC32 p523)
      DIP("sthbrx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
      assign( Rs, getIReg(Rs_addr) );
      assign( byte0, binop(Iop_And32, mkexpr(Rs), mkU32(0x00FF)) );
      assign( byte1, binop(Iop_And32, mkexpr(Rs), mkU32(0xFF00)) );
      
      assign( tmp16,
              unop(Iop_32to16,
                   binop(Iop_Or32,
                         binop(Iop_Shl32, mkexpr(byte0), mkU8(8)),
                         binop(Iop_Shr32, mkexpr(byte1), mkU8(8)))) );
      storeBE( mkexpr(EA), getIReg(tmp16) );
      break;
      
   case 0x296: // stwbrx (Store Word Byte-Reverse Indexed, PPC32 p531)
      DIP("stwbrx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
      assign( Rs, getIReg(Rs_addr) );
      assign( byte0, binop(Iop_And32, mkexpr(Rs), mkU32(0x000000FF)) );
      assign( byte1, binop(Iop_And32, mkexpr(Rs), mkU32(0x0000FF00)) );
      assign( byte2, binop(Iop_And32, mkexpr(Rs), mkU32(0x00FF0000)) );
      assign( byte3, binop(Iop_And32, mkexpr(Rs), mkU32(0xFF000000)) );
      
      assign( tmp32,
              binop(Iop_Or32,
                    binop(Iop_Or32,
                          binop(Iop_Shl32, mkexpr(byte0), mkU8(24)),
                          binop(Iop_Shl32, mkexpr(byte1), mkU8(8))),
                    binop(Iop_Or32,
                          binop(Iop_Shr32, mkexpr(byte2), mkU8(8)),
                          binop(Iop_Shr32, mkexpr(byte3), mkU8(24)))) );
      storeBE( mkexpr(EA), mkexpr(tmp32) );
      break;
      
   default:
      vex_printf("dis_int_ldst_rev(PPC32)(opc2)\n");
      return False;
   }
   return True;
}



/*
  Processor Control Instructions
*/
static Bool dis_proc_ctl ( UInt theInstr )
{
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   
   /* X-Form */
   UChar crfD     = toUChar((theInstr >> 23) & 0x7);  /* theInstr[23:25] */
   UChar b21to22  = toUChar((theInstr >> 21) & 0x3);  /* theInstr[21:22] */
   UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UInt  b11to20  =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
   
   /* XFX-Form */
   UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UInt  SPR      =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
   UInt  TBR      =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
   UChar b20      = toUChar((theInstr >> 11) & 0x1);  /* theInstr[11]    */
   UInt  CRM      =         (theInstr >> 12) & 0xFF;  /* theInstr[12:19] */
   UChar b11      = toUChar((theInstr >> 11) & 0x1);  /* theInstr[20]    */
   
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   UInt  SPR_flipped = ((SPR & 0x1F) << 5) | ((SPR >> 5) & 0x1F);

   IRTemp Rs  = newTemp(Ity_I32);
   IRTemp tmp = newTemp(Ity_I32);

   assign( Rs, getIReg(Rs_addr) );
   
   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_proc_ctl(PPC32)(opc1|b0)\n");
      return False;
   }
   
   switch (opc2) {
   /* X-Form */
   case 0x200: // mcrxr (Move to Condition Register from XER, PPC32 p466)
      if (b21to22 != 0 || b11to20 != 0) {
         vex_printf("dis_proc_ctl(PPC32)(mcrxr,b21to22|b11to20)\n");
         return False;
      }
      DIP("mcrxr crf%d\n", crfD);
      
      // CR[7-crfD] = XER[28-31]
      assign( tmp, getReg_field( PPC32_SPR_XER, 7 ) );
      putReg_field( PPC32_SPR_CR, mkexpr(tmp), 7-crfD );
      
      // Clear XER[28 - 31]
      putReg_field( PPC32_SPR_XER, mkU32(0), 7 );
      break;
      
   case 0x013: // mfcr (Move from Condition Register, PPC32 p467)
      if (b11to20 != 0) {
         vex_printf("dis_proc_ctl(PPC32)(mfcr,b11to20)\n");
         return False;
      }
      DIP("mfcr crf%d\n", Rd_addr);
      putIReg( Rd_addr, getReg( PPC32_SPR_CR ) );
      break;
      
   /* XFX-Form */
   case 0x153: // mfspr (Move from Special-Purpose Register, PPC32 p470)
      DIP("mfspr r%d,0x%x\n", Rd_addr, SPR_flipped);
      
      switch (SPR_flipped) {  // Choose a register...
      case 0x1: putIReg( Rd_addr, getReg( PPC32_SPR_XER ) ); break;
      case 0x8: putIReg( Rd_addr, getReg( PPC32_SPR_LR  ) ); break;
      case 0x9: putIReg( Rd_addr, getReg( PPC32_SPR_CTR ) ); break;
             
      case 0x012: case 0x013: case 0x016:
      case 0x019: case 0x01A: case 0x01B:
      case 0x110: case 0x111: case 0x112: case 0x113:
//      case 0x118: // 64bit only
      case 0x11A: case 0x11F:
      case 0x210: case 0x211: case 0x212: case 0x213:
      case 0x214: case 0x215: case 0x216: case 0x217:
      case 0x218: case 0x219: case 0x21A: case 0x21B:
      case 0x21C: case 0x21D: case 0x21E: case 0x21F:
      case 0x3F5:
         vex_printf("dis_proc_ctl(PPC32)(mfspr) - supervisor level op\n");
         return False;

      default:
         vex_printf("dis_proc_ctl(PPC32)(mfspr,SPR_flipped)\n");
         return False;
      }
      break;
      
   case 0x173: // mftb (Move from Time Base, PPC32 p475)
      DIP("mftb r%d,0x%x\n", Rd_addr, TBR);
      return False;
      
   case 0x090: { // mtcrf (Move to Condition Register Fields, PPC32 p477)
      UInt mask=0, i=0;
      if (b11 != 0 || b20 != 0) {
         vex_printf("dis_proc_ctl(PPC32)(mtcrf,b11|b20)\n");
         return False;
      }
      DIP("mtcrf 0x%x,r%d\n", CRM, Rs_addr);
      for (i=0; i<8; i++) {
         if (CRM & (1<<i)) {
            mask = mask | (0xF << (7-i)*4);
         }
      }
      putReg_masked( PPC32_SPR_CR, mkexpr(Rs), mask );
      break;
   }

   case 0x1D3: // mtspr (Move to Special-Purpose Register, PPC32 p483)
      DIP("mtspr 0x%x,r%d\n", SPR_flipped, Rs_addr);
      
      switch (SPR_flipped) {  // Choose a register...
      case 0x1: putReg( PPC32_SPR_XER, mkexpr(Rs) ); break;
      case 0x8: putReg( PPC32_SPR_LR,  mkexpr(Rs) ); break;
      case 0x9: putReg( PPC32_SPR_CTR, mkexpr(Rs) ); break;

      case 0x012: case 0x013: case 0x016:
      case 0x019: case 0x01A: case 0x01B:
      case 0x110: case 0x111: case 0x112: case 0x113:
//      case 0x118: // 64bit only
      case 0x11A: case 0x11C: case 0x11D:
      case 0x210: case 0x211: case 0x212: case 0x213:
      case 0x214: case 0x215: case 0x216: case 0x217:
      case 0x218: case 0x219: case 0x21A: case 0x21B:
      case 0x21C: case 0x21D: case 0x21E: case 0x21F:
      case 0x3F5:
         vex_printf("dis_proc_ctl(PPC32)(mtspr) - supervisor level op\n");
         return False;

      default:
         vex_printf("dis_proc_ctl(PPC32)(mtspr,SPR_flipped)\n");
         return False;
      }
      break;
      
   default:
      vex_printf("dis_proc_ctl(PPC32)(opc2)\n");
      return False;
   }
   return True;
}


/*
  Cache Management Instructions
*/
static Bool dis_cache_manage ( UInt theInstr, DisResult* whatNext )
{
   /* X-Form */
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar b21to25 = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0      = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   if (opc1 != 0x1F || b21to25 != 0 || b0 != 0) {
      vex_printf("dis_cache_manage(PPC32)(opc1|b21to25|b0)\n");
      return False;
   }
   
   switch (opc2) {
   case 0x2F6: // dcba (Data Cache Block Allocate, PPC32 p380)
      DIP("dcba r%d,r%d\n", Ra_addr, Rb_addr);
      if (1) vex_printf("vex ppc32->IR: kludged dcba\n");
      break;
      
   case 0x056: // dcbf (Data Cache Block Flush, PPC32 p382)
      DIP("dcbf r%d,r%d\n", Ra_addr, Rb_addr);
      if (0) vex_printf("vex ppc32->IR: kludged dcbf\n");
      break;
      
   case 0x036: // dcbst (Data Cache Block Store, PPC32 p384)
      DIP("dcbst r%d,r%d\n", Ra_addr, Rb_addr);
      if (1) vex_printf("vex ppc32->IR: kludged dcbst\n");
      break;

   case 0x116: // dcbt (Data Cache Block Touch, PPC32 p385)
      DIP("dcbt r%d,r%d\n", Ra_addr, Rb_addr);
      if (1) vex_printf("vex ppc32->IR: kludged dcbt\n");
      break;
      
   case 0x0F6: // dcbtst (Data Cache Block Touch for Store, PPC32 p386)
      DIP("dcbtst r%d,r%d\n", Ra_addr, Rb_addr);
      if (1) vex_printf("vex ppc32->IR: kludged dcbtst\n");
      break;
      
   case 0x3F6: // dcbz (Data Cache Block Clear to Zero, PPC32 p387)
      DIP("dcbz r%d,r%d\n", Ra_addr, Rb_addr);
      if (1) vex_printf("vex ppc32->IR: kludged dcbz\n");
      break;

   case 0x3D6: { 
      // icbi (Instruction Cache Block Invalidate, PPC32 p431)
      /* Invalidate all translations containing code from the cache
         block at (rA|0) + rB.  Since we don't know what the cache
         line size is, let's assume 256 -- no real I1 cache would ever
         have a line size that large, so that's safe. */
      IRTemp addr = newTemp(Ity_I32);
      UInt   assumed_line_size = 256;
      DIP("icbi r%d,r%d\n", Ra_addr, Rb_addr);

      assign( addr,
              binop( Iop_Add32, 
                     getIReg(Rb_addr), 
                     Ra_addr==0 ? mkU32(0) : getIReg(Ra_addr)) );

      /* Round addr down to the start of the containing block. */
      stmt( IRStmt_Put(
               OFFB_TISTART,
               binop( Iop_And32, 
                      mkexpr(addr), 
                      mkU32( ~(assumed_line_size-1) ))) );

      stmt( IRStmt_Put(OFFB_TILEN, mkU32(assumed_line_size) ) );

      /* be paranoid ... */
      stmt( IRStmt_MFence() );

      irbb->jumpkind = Ijk_TInval;
      irbb->next     = mkU32(guest_cia_curr_instr + 4);
      *whatNext      = Dis_StopHere;
      break;
   }

   default:
      vex_printf("dis_cache_manage(PPC32)(opc2)\n");
      return False;
   }
   return True;
}







/*------------------------------------------------------------*/
/*--- Floating Point Instruction Translation               ---*/
/*------------------------------------------------------------*/

/*
  Floating Point Load Instructions
*/
static Bool dis_fp_load ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar rA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar rB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   /* D-Form */
   UInt  d_imm   =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */

   switch(opc1) {
   case 0x30: // lfsx (Load Float Single Indexed, PPC32 p444)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfsx)\n");
         return False;
      }
      DIP("lfsx fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x31: // lfsux (Load Float Single with Update Indexed, PPC32 p443)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfsux)\n");
         return False;
      }
      DIP("lfsux fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x32: // lfdx (Load Float Double Indexed, PPC32 p439)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfdx)\n");
         return False;
      }
      DIP("lfdx fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x33: // lfdux (Load Float Double with Update Indexed, PPC32 p439)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfdux)\n");
         return False;
      }
      DIP("lfdux fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x1F:
      switch(opc2) {
      case 0x217: // lfs (Load Float Single, PPC32 p441)
         DIP("lfs fr%d,%u(r%d)\n", frD_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x237: // lfsu (Load Float Single with Update, PPC32 p442)
         DIP("lfsu fr%d,%u(r%d)\n", frD_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x257: // lfd (Load Float Double, PPC32 p437)
         DIP("lfd fr%d,%u(r%d)\n", frD_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x277: // lfdu (Load Float Double with Update, PPC32 p438)
         DIP("lfdu fr%d,%u(r%d)\n", frD_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      default:
         vex_printf("dis_fp_load(PPC32)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_load(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Store Instructions
*/
static Bool dis_fp_store ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar frS_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar rA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar rB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   /* D-Form */
   UInt  d_imm   =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */

   switch(opc1) {
   case 0x34: // stfsx (Store Float Single Indexed, PPC32 p521)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,stfsx)\n");
         return False;
      }
      DIP("stfsx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x35: // stfsux (Store Float Single with Update Indexed, PPC32 p520)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,stfsux)\n");
         return False;
      }
      DIP("stfsux fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x36: // stfdx (Store Float Double Indexed, PPC32 p516)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,stfdx)\n");
         return False;
      }
      DIP("stfdx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x37: // stfdux (Store Float Double with Update Indexed, PPC32 p515)
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,stfdux)\n");
         return False;
      }
      DIP("stfdux fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x1F:
      switch(opc2) {
      case 0x297: // stfs (Store Float Single, PPC32 p518)
         DIP("stfs fr%d,%u(r%d)\n", frS_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x2B7: // stfsu (Store Float Single with Update, PPC32 p519)
         DIP("stfsu fr%d,%u(r%d)\n", frS_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x2D7: // stfd (Store Float Double, PPC32 p513)
         DIP("stfd fr%d,%u(r%d)\n", frS_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x2F7: // stfdu (Store Float Double with Update, PPC32 p514)
         DIP("stfdu fr%d,%u(r%d)\n", frS_addr, d_imm, rA_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x3D7: // stfiwx (Store Float as Int, Indexed, PPC32 p517)
         if (b0 != 0) {
            vex_printf("dis_fp_load(PPC32)(instr,stfiwx)\n");
            return False;
         }
         DIP("stfiwx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
         DIP(" => not implemented\n");
         return False;

      default:
         vex_printf("dis_fp_store(PPC32)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_store(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Arith Instructions
*/
static Bool dis_fp_arith ( UInt theInstr )
{
   /* A-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar frA_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UChar frC_addr = toUChar((theInstr >>  6) & 0x1F); /* theInstr[6:10]  */
   UChar opc2     = toUChar((theInstr >>  1) & 0x1F); /* theInstr[1:5]   */
   UChar flag_Rc  = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x12: // fdivs (Floating Divide Single, PPC32 p407)
         if (frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fdivs)\n");
            return False;
         }
         DIP("fdivs%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x14: // fsubs (Floating Subtract Single, PPC32 p430)
         if (frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fsubs)\n");
            return False;
         }
         DIP("fsubs%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x15: // fadds (Floating Add Single, PPC32 p401)
         if (frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fadds)\n");
            return False;
         }
         DIP("fadds%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x16: // fsqrts (Floating SqRt (Single-Precision), PPC32 p428)
         if (frA_addr != 0 || frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fsqrts)\n");
            return False;
         }
         DIP("fsqrts%s fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x18: // fres (Floating Reciprocal Estimate Single, PPC32 p421)
         if (frA_addr != 0 || frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fres)\n");
            return False;
         }
         DIP("fres%s fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x19: // fmuls (Floating Multiply Single, PPC32 p414)
         if (frB_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fmuls)\n");
            return False;
         }
         DIP("fmuls%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr);
         DIP(" => not implemented\n");
         return False;

      default:
         vex_printf("dis_fp_arith(PPC32)(3B: opc2)\n");
         return False;
      }
   case 0x3F:
      switch (opc2) {           
      case 0x12: // fdiv (Floating Divide (Double-Precision), PPC32 p406)
         if (frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fdiv)\n");
            return False;
         }
         DIP("fdiv%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x14: // fsub (Floating Subtract (Double-Precision), PPC32 p429)
         if (frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fsub)\n");
            return False;
         }
         DIP("fsub%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x15: // fadd (Floating Add (Double-Precision), PPC32 p400)
         if (frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fadd)\n");
            return False;
         }
         DIP("fadd%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x16: // fsqrt (Floating SqRt (Double-Precision), PPC32 p427)
         if (frA_addr != 0 || frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fsqrt)\n");
            return False;
         }
         DIP("fsqrt%s fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x17: // fsel (Floating Select, PPC32 p426)
         DIP("fsel%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x19: // fmul (Floating Multiply (Double Precision), PPC32 p413)
         if (frB_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,fmul)\n");
            return False;
         }
         DIP("fmul%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1A: // frsqrte (Floating Reciprocal SqRt Estimate, PPC32 p424)
         if (frA_addr != 0 || frC_addr != 0) {
            vex_printf("dis_fp_cmp(PPC32)(instr,frsqrte)\n");
            return False;
         }
         DIP("frsqrte%s fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      default:
         vex_printf("dis_fp_arith(PPC32)(3F: opc2)\n");
         return False;
      }
   default:
      vex_printf("dis_fp_arith(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Mult-Add Instructions
*/
static Bool dis_fp_multadd ( UInt theInstr )
{
   /* A-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar frA_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UChar frC_addr = toUChar((theInstr >>  6) & 0x1F); /* theInstr[6:10]  */
   UChar opc2     = toUChar((theInstr >>  1) & 0x1F); /* theInstr[1:5]   */
   UChar flag_Rc  = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x1C: // fmsubs (Floating Mult-Subtr Single, PPC32 p409)
         DIP("fmsubs%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1D: // fmadds (Floating Mult-Add Single, PPC32 p409)
         DIP("fmadds%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1E: // fnmsubs (Float Neg Mult-Subtr Single, PPC32 p420)
         DIP("fnmsubs%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1F: // fnmadds (Floating Negative Multiply-Add Single, PPC32 p418)
         DIP("fnmadds%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      default:
         vex_printf("dis_fp_multadd(PPC32)(3B: opc2)\n");
         return False;
      }
   case 0x3F:
      switch (opc2) {           
      case 0x1C: // fmsub (Float Mult-Subtr (Double Precision), PPC32 p411)
         DIP("fmsub%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1D: // fmadd (Float Mult-Add (Double Precision), PPC32 p408)
         DIP("fmadd%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1E: // fnmsub (Float Neg Mult-Subtr (Double Precision), PPC32 p419)
         DIP("fnmsub%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      case 0x1F: // fnmadd (Float Neg Mult-Add (Double Precision), PPC32 p417)
         DIP("fnmadd%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         DIP(" => not implemented\n");
         return False;

      default:
         vex_printf("dis_fp_multadd(PPC32)(3F: opc2)\n");
         return False;
      }
   default:
      vex_printf("dis_fp_multadd(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Compare Instructions
*/
static Bool dis_fp_cmp ( UInt theInstr )
{   
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar crfD     = toUChar((theInstr >> 23) & 0x7);  /* theInstr[23:25] */
   UChar b21to22  = toUChar((theInstr >> 21) & 0x3);  /* theInstr[21:22] */
   UChar frA_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */   
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   if (opc1 != 0x3F || b21to22 != 0 || b0 != 0) {
      vex_printf("dis_fp_cmp(PPC32)(instr)\n");
      return False;
   }

   opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
   switch (opc2) {
   case 0x000: // fcmpu (Floating Compare Unordered, PPC32 p403)
      DIP("fcmpu crf%d,fr%d,fr%d\n", crfD, frA_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x020: // fcmpo (Floating Compare Ordered, PPC32 p402)
      DIP("fcmpo crf%d,fr%d,fr%d\n", crfD, frA_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   default:
      vex_printf("dis_fp_cmp(PPC32)(opc2)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Rounding/Conversion Instructions
*/
static Bool dis_fp_round ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar b16to20  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar flag_Rc  = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_round(PPC32)(instr)\n");
      return False;
   }

   opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
   switch (opc2) {
   case 0x00C: // frsp (Floating Round to Single, PPC32 p423)
      DIP("frsp%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x00E: // fctiw (Floating Conv to Int, PPC32 p404)
      DIP("fctiw%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x00F: // fctiwz (Floating Conv to Int, Round to Zero, PPC32 p405)
      DIP("fctiwz%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   default:
      vex_printf("dis_fp_round(PPC32)(opc2)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Move Instructions
*/
static Bool dis_fp_move ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar b16to20  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar flag_Rc  = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_move(PPC32)(instr)\n");
      return False;
   }

   opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
   switch (opc2) {
   case 0x028: // fneg (Floating Negate, PPC32 p416)
      DIP("fneg%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x048: // fmr (Floating Move Register, PPC32 p410)
      DIP("fmr%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x088: // fnabs (Floating Negative Absolute Value, PPC32 p415)
      DIP("fnabs%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x108: // fabs (Floating Absolute Value, PPC32 p399)
      DIP("fabs%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
      DIP(" => not implemented\n");
      return False;

   default:
      vex_printf("dis_fp_move(PPC32)(opc2)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Status/Control Register Instructions
*/
static Bool dis_fp_scr ( UInt theInstr )
{
   /* X-Form */
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   /* Too many forms - see each switch case */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar flag_Rc = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   if (opc1 != 0x3F) {
      vex_printf("dis_fp_scr(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x026: { // mtfsb1 (Move to FPSCR Bit 1, PPC32 p479)
      UChar crbD    = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
      UInt  b11to20 =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsb1)\n");
         return False;
      }
      DIP("mtfsb1%s crb%d \n", flag_Rc ? "." : "", crbD);
      DIP(" => not implemented\n");
      return False;
   }

   case 0x040: { // mcrfs (Move to Condition Register from FPSCR, PPC32 p465)
      UChar crfD    = toUChar((theInstr >> 23) & 0x7);  /* theInstr[23:25] */
      UChar b21to22 = toUChar((theInstr >> 21) & 0x3);  /* theInstr[21:22] */
      UChar crfS    = toUChar((theInstr >> 18) & 0x7);  /* theInstr[18:20] */
      UChar b11to17 = toUChar((theInstr >> 11) & 0x7F); /* theInstr[11:17] */

      if (b21to22 != 0 || b11to17 != 0 || flag_Rc != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mcrfs)\n");
         return False;
      }
      DIP("mcrfs crf%d,crf%d\n", crfD, crfS);
      DIP(" => not implemented\n");
      return False;
   }

   case 0x046: { // mtfsb0 (Move to FPSCR Bit 0, PPC32 p478)
      UChar crbD    = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
      UInt  b11to20 =         (theInstr >> 16) & 0x3FF; /* theInstr[11:20] */

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsb0)\n");
         return False;
      }      
      DIP("mtfsb0%s crb%d\n", flag_Rc ? "." : "", crbD);
      DIP(" => not implemented\n");
      return False;
   }

   case 0x086: { // mtfsfi (Move to FPSCR Field Immediate, PPC32 p481)
      UChar crfD    = toUChar((theInstr >> 23) & 0x7);  /* theInstr[23:25] */
      UChar b16to22 = toUChar((theInstr >> 16) & 0x7F); /* theInstr[16:22] */
      UChar IMM     = toUChar((theInstr >> 12) & 0xF);  /* theInstr[11:15] */
      UChar b11     = toUChar((theInstr >> 11) & 0x1);  /* theInstr[11]    */

      if (b16to22 != 0 || b11 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsfi)\n");
         return False;
      }      
      DIP("mtfsfi%s crf%d,%d\n", flag_Rc ? "." : "", crfD, IMM);
      DIP(" => not implemented\n");
      return False;
   }

   case 0x247: { // mffs (Move from FPSCR, PPC32 p481)
      UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
      UInt  b11to20  =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mffs)\n");
         return False;
      }      
      DIP("mffs%s fr%d\n", flag_Rc ? "." : "", frD_addr);
      DIP(" => not implemented\n");
      return False;
   }

   case 0x2C7: { // mtfsf (Move to FPSCR Fields, PPC32 p480)
      UChar b25      = toUChar((theInstr >> 25) & 0x1);  /* theInstr[25]    */
      UChar FM       = toUChar((theInstr >> 17) & 0xFF); /* theInstr[17:24] */
      UChar b16      = toUChar((theInstr >> 16) & 0x1);  /* theInstr[16]    */
      UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */

      if (b25 != 0 || b16 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsf)\n");
         return False;
      }      
      DIP("mtfsf%s %d,fr%d\n", flag_Rc ? "." : "", FM, frB_addr);
      DIP(" => not implemented\n");
      return False;
   }

   default:
      vex_printf("dis_fp_scr(PPC32)(opc2)\n");
      return False;
   }
   return True;
}










/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta].
   Set *size to be the size of the instruction.
   If the returned value is Dis_Resteer,
   the next guest address is assigned to *whereNext.  If resteerOK
   is False, disInstr may not return Dis_Resteer. */
   
static DisResult disInstr ( /*IN*/  Bool    resteerOK,
                            /*IN*/  Bool    (*resteerOkFn) ( Addr64 ),
                            /*IN*/  UInt    delta, 
                            /*OUT*/ Int*    size,
                            /*OUT*/ Addr64* whereNext )
{
   UChar opc1;
   UInt  opc2;
   DisResult whatNext = Dis_Continue;
   UInt      theInstr;


   /* At least this is simple on PPC32: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */

   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_PPC32 abort if we fail the
      decode. */
   *size = 0;

   theInstr = getUIntBigendianly( (UChar*)(&guest_code[delta]) );

   DIP("\t0x%x:  ", guest_pc_bbstart+delta);


   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
      UInt* code = (UInt*)(guest_code + delta);

      /* Spot this:                                       
         0x60000000   ori 0,0,0            => r0 = r0 | 0
         0x5400E800   rlwinm 0,0,29,0,0    => r0 = rotl(r0,29)
         0x54001800   rlwinm 0,0,3,0,0     => r0 = rotl(r0,3)
         0x54006800   rlwinm 0,0,13,0,0    => r0 = rotl(r0,13)
         0x54009800   rlwinm 0,0,19,0,0    => r0 = rotl(r0,19)
         0x60000000   ori 0,0,0            => r0 = r0 | 0
      */
      if (code[0] == 0x60000000 &&
          code[1] == 0x5400E800 &&
          code[2] == 0x54001800 &&
          code[3] == 0x54006800 &&
          code[4] == 0x54009800 &&
          code[5] == 0x60000000) {

         // TODO: possibly r0 = client_request(r0)
         DIP("? = client_request ( ? )\n");

         *size = 24;
         
         irbb->next     = mkU32(guest_pc_bbstart+delta);
         irbb->jumpkind = Ijk_ClientReq;
         
         whatNext = Dis_StopHere;
         goto decode_success;
      }
   }


   opc1 = toUChar((theInstr >> 26) & 0x3F );   /* theInstr[26:31] */
   opc2 =        ((theInstr >> 1 ) & 0x3FF);   /* theInstr[1:10]  */

#if PPC32_TOIR_DEBUG
   vex_printf("\ndisInstr(ppc32): instr:   0x%x\n", theInstr);
   vex_printf("disInstr(ppc32): instr:   ");
   vex_printf_binary( theInstr, 32, True );
   vex_printf("\n");
#endif


   if (theInstr == 0x7C0042A6) {
      // CAB: what's this?
      DIP("Invalid instruction! Would be 'mfspr 0,256', which doesn't exist!.\n");
      goto decode_failure;
//      DIP("Passing through for now...\n");
//      goto decode_success;
   }

   // Note: all 'reserved' bits must be cleared, else invalid
   switch (opc1) {

   /* Integer Arithmetic Instructions */
   case 0x0C: case 0x0D: case 0x0E:  // addic, addic., addi
   case 0x0F: case 0x07: case 0x08:  // addis, mulli,  subfic
      if (dis_int_arith( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Compare Instructions */
   case 0x0B: case 0x0A: // cmpi, cmpli
      if (dis_int_cmp( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Logical Instructions */
   case 0x1C: case 0x1D: case 0x18: // andi., andis., ori
   case 0x19: case 0x1A: case 0x1B: // oris,  xori,   xoris
      if (dis_int_logic( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Rotate Instructions */
   case 0x14: case 0x15:  case 0x17: // rlwimi, rlwinm, rlwnm
      if (dis_int_rot( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Load Instructions */
   case 0x22: case 0x23: case 0x2A: // lbz,  lbzu, lha
   case 0x2B: case 0x28: case 0x29: // lhau, lhz,  lhzu
   case 0x20: case 0x21:            // lwz,  lwzu
      if (dis_int_load( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Store Instructions */
   case 0x26: case 0x27: case 0x2C: // stb,  stbu, sth
   case 0x2D: case 0x24: case 0x25: // sthu, stw,  stwu
      if (dis_int_store( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Load and Store Multiple Instructions */
   case 0x2E: case 0x2F: // lmw, stmw
      if (dis_int_ldst_mult( theInstr )) goto decode_success;
      goto decode_failure;

   /* Branch Instructions */
   case 0x12: case 0x10: // b, bc
      if (dis_branch(theInstr, &whatNext)) goto decode_success;
      goto decode_failure;

   /* System Linkage Instructions */
   case 0x11: // sc
      if (dis_syslink(theInstr, &whatNext)) goto decode_success;
      goto decode_failure;

   /* Trap Instructions */
   case 0x03: // twi
      DIP("trap op (twi) => not implemented\n");
      goto decode_failure;

   /* Floating Point Load Instructions */
   case 0x30: case 0x31: case 0x32: // lfsx, lfsux, lfdx
   case 0x33:                       // lfdux
      if (dis_fp_load( theInstr )) goto decode_success;
      goto decode_failure;

   /* Floating Point Store Instructions */
   case 0x34: case 0x35: case 0x36: // stfsx, stfsux, stfdx
   case 0x37:                       // stfdux
      if (dis_fp_store( theInstr )) goto decode_success;
      goto decode_failure;

   case 0x3B:
      opc2 = (theInstr >> 1) & 0x1F;    /* theInstr[1:5] */
      switch (opc2) {
      /* Floating Point Arith Instructions */
      case 0x12: case 0x14: case 0x15: // fdivs,  fsubs, fadds
      case 0x16: case 0x18: case 0x19: // fsqrts, fres,  fmuls
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;

      /* Floating Point Mult-Add Instructions */
      case 0x1C: case 0x1D: case 0x1E: // fmsubs, fmadds, fnmsubs
      case 0x1F:                       // fnmadds
         if (dis_fp_multadd(theInstr)) goto decode_success;
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;

   case 0x3F:
      /* Instrs using opc[1:5] never overlap with instrs using opc[1:10],
         so we can simply fall through the first switch statement */

      opc2 = (theInstr >> 1) & 0x1F;    /* theInstr[1:5] */
      switch (opc2) {
      /* Floating Point Arith Instructions */
      case 0x12: case 0x14: case 0x15: // fdiv,  fsub, fadd
      case 0x16: case 0x17: case 0x19: // fsqrt, fsel, fmul
      case 0x1A:                       // frsqrte
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;

      /* Floating Point Mult-Add Instructions */         
      case 0x1C: case 0x1D: case 0x1E: // fmsub, fmadd, fnmsub
      case 0x1F:                       // fnmadd
         if (dis_fp_multadd(theInstr)) goto decode_success;
         goto decode_failure;
         
      default:
         break; // Fall through
      }

      opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
      switch (opc2) {
      /* Floating Point Compare Instructions */         
      case 0x000: // fcmpu
      case 0x020: // fcmpo
         if (dis_fp_cmp(theInstr)) goto decode_success;
         goto decode_failure;

      /* Floating Point Rounding/Conversion Instructions */         
      case 0x00C: // frsp
      case 0x00E: // fctiw
      case 0x00F: // fctiwz
         if (dis_fp_round(theInstr)) goto decode_success;
         goto decode_failure;

      /* Floating Point Move Instructions */         
      case 0x028: // fneg
      case 0x048: // fmr
      case 0x088: // fnabs
      case 0x108: // fabs
         if (dis_fp_move( theInstr )) goto decode_success;
         goto decode_failure;

      /* Floating Point Status/Control Register Instructions */         
      case 0x026: // mtfsb1
      case 0x040: // mcrfs
      case 0x046: // mtfsb0
      case 0x086: // mtfsfi
      case 0x247: // mffs
      case 0x2C7: // mtfsf
         if (dis_fp_scr( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;

   case 0x13:
      switch (opc2) {

      /* Condition Register Logical Instructions */
      case 0x101: case 0x081: case 0x121: // crand,  crandc, creqv
      case 0x0E1: case 0x021: case 0x1C1: // crnand, crnor,  cror
      case 0x1A1: case 0x0C1: case 0x000: // crorc,  crxor,  mcrf
         if (dis_cond_logic( theInstr )) goto decode_success;
         goto decode_failure;

      /* Branch Instructions */
      case 0x210: case 0x010: // bcctr, bclr
         if (dis_branch(theInstr, &whatNext)) goto decode_success;
         goto decode_failure;

      /* Memory Synchronization Instructions */
      case 0x096: // isync
         if (dis_memsync( theInstr )) goto decode_success;
         goto decode_failure;
         
      default:
         goto decode_failure;
      }
      break;


   case 0x1F:

      /* For arith instns, bit10 is the OE flag (overflow enable) */

      opc2 = (theInstr >> 1) & 0x1FF;    /* theInstr[1:9] */
      switch (opc2) {
      /* Integer Arithmetic Instructions */
      case 0x10A: case 0x00A: case 0x08A: // add,   addc,  adde
      case 0x0EA: case 0x0CA: case 0x1EB: // addme, addze, divw
      case 0x1CB: case 0x04B: case 0x00B: // divwu, mulhw, mulhwu
      case 0x0EB: case 0x068: case 0x028: // mullw, neg,   subf
      case 0x008: case 0x088: case 0x0E8: // subfc, subfe, subfme
      case 0x0C8:                         // subfze
         if (dis_int_arith( theInstr )) goto decode_success;
         goto decode_failure;
      default:
         break;  // Fall through...
      }


      /* All remaining opcodes use full 10 bits. */

      opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
      switch (opc2) {
      /* Integer Compare Instructions  */
      case 0x000: case 0x020: // cmp, cmpl
         if (dis_int_cmp( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Logical Instructions */
      case 0x01C: case 0x03C: case 0x01A: // and,  andc,  cntlzw
      case 0x11C: case 0x3BA: case 0x39A: // eqv,  extsb, extsh
      case 0x1DC: case 0x07C: case 0x1BC: // nand, nor,   or
      case 0x19C: case 0x13C:             // orc,  xor
         if (dis_int_logic( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Shift Instructions */
      case 0x018: case 0x318: case 0x338: // slw, sraw, srawi
      case 0x218:                         // srw
         if (dis_int_shift( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Load Instructions */
      case 0x057: case 0x077: case 0x157: // lbzx,  lbzux, lhax
      case 0x177: case 0x117: case 0x137: // lhaux, lhzx,  lhzux
      case 0x017: case 0x037:             // lwzx,  lwzux
         if (dis_int_load( theInstr )) goto decode_success;
         goto decode_failure;

         /* Integer Store Instructions */
      case 0x0F7: case 0x0D7: case 0x1B7: // stbux, stbx,  sthux
      case 0x197: case 0x0B7: case 0x097: // sthx,  stwux, stwx
         if (dis_int_store( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Load and Store with Byte Reverse Instructions */
      case 0x316: case 0x216: case 0x396: // lhbrx, lwbrx, sthbrx
      case 0x296:                         // stwbrx
         if (dis_int_ldst_rev( theInstr )) goto decode_success;
         goto decode_failure;
         
      /* Integer Load and Store String Instructions */
      case 0x255: case 0x215: case 0x2D5: // lswi, lswx, stswi
      case 0x295:                         // stswx
         if (dis_int_ldst_str( theInstr )) goto decode_success;
         goto decode_failure;

      /* Memory Synchronization Instructions */
      case 0x356: case 0x014: case 0x096: // eieio, lwarx, stwcx.
      case 0x256:                         // sync
         if (dis_memsync( theInstr )) goto decode_success;
         goto decode_failure;
         
      /* Processor Control Instructions */
      case 0x200: case 0x013: case 0x153: // mcrxr, mfcr,  mfspr
      case 0x173: case 0x090: case 0x1D3: // mftb,  mtcrf, mtspr
         if (dis_proc_ctl( theInstr )) goto decode_success;
         goto decode_failure;

      /* Cache Management Instructions */
      case 0x2F6: case 0x056: case 0x036: // dcba, dcbf,   dcbst
      case 0x116: case 0x0F6: case 0x3F6: // dcbt, dcbtst, dcbz
      case 0x3D6:                         // icbi
         if (dis_cache_manage( theInstr, &whatNext )) goto decode_success;
         goto decode_failure;

      /* External Control Instructions */
      case 0x136: case 0x1B6: // eciwx, ecowx
         DIP("external control op => not implemented\n");
         goto decode_failure;

      /* Trap Instructions */
      case 0x004: // tw
         DIP("trap op (tw) => not implemented\n");
         goto decode_failure;

      /* Floating Point Load Instructions */
      case 0x217: case 0x237: case 0x257: // lfs, lfsu, lfd
      case 0x277:                         // lfdu
         if (dis_fp_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* Floating Point Store Instructions */
      case 0x297: case 0x2B7: case 0x2D7: // stfs,  stfsu, stfd
      case 0x2F7: case 0x3D7:             // stfdu, stfiwx
         if (dis_fp_store( theInstr )) goto decode_success;
         goto decode_failure;

      /* AltiVec instructions */
      case 0x0E7: // stvx
         DIP("Altivec op (stvx) => not implemented\n");
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;

   default:
   decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(ppc32): unhandled instruction: "
              "0x%x\n", theInstr);
   
#if PPC32_TOIR_DEBUG
   vex_printf("disInstr(ppc32): instr:   ");
   vex_printf_binary( theInstr, 32, True );
   vex_printf("\n");

   vex_printf("disInstr(ppc32): opcode1: ");
   vex_printf_binary( opc1, 6, False );
   vex_printf("\n");

   vex_printf("disInstr(ppc32): opcode2: ");
   vex_printf_binary( opc2, 10, False );
   vex_printf("\n\n");
#endif


   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      CIA should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   putReg( PPC32_SPR_CIA, mkU32(guest_cia_curr_instr) );
   irbb->next = mkU32(guest_cia_curr_instr);
   irbb->jumpkind = Ijk_NoDecode;
   whatNext = Dis_StopHere;
   *size = 0;
   return whatNext;

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
//   vex_printf("disInstr(ppc32): success");
   DIP("\n");

   *size = 4;
   return whatNext;
}

#undef DIP
#undef DIS

/*--------------------------------------------------------------------*/
/*--- end                                       guest-ppc32/toIR.c ---*/
/*--------------------------------------------------------------------*/
