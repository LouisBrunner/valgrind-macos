
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-ppc32/toIR.c) is                            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

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

/* We need to know this to do sub-register accesses correctly. */
/* CONST */
static Bool host_is_bigendian;

/* Pointer to the guest code area. */
/* CONST */
static UChar* guest_code;

/* The guest address corresponding to guest_code[0]. */
/* CONST */
static Addr32 guest_pc_bbstart;

/* The IRBB* into which we're generating code. */
static IRBB* irbb;


/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

#define DIS(buf, format, args...)      \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_sprintf(buf, format, ## args)




/*------------------------------------------------------------*/
/*--- Offsets of various parts of the ppc32 guest state.     ---*/
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

#define OFFB_CC_OP     offsetof(VexGuestPPC32State,guest_CC_OP)
#define OFFB_CC_DEP1   offsetof(VexGuestPPC32State,guest_CC_DEP1)
#define OFFB_CC_DEP2   offsetof(VexGuestPPC32State,guest_CC_DEP2)

#define OFFB_CR1to7      offsetof(VexGuestPPC32State,guest_CR1to7)

#define OFFB_XER_SO     offsetof(VexGuestPPC32State,guest_XER_SO)
#define OFFB_XER_OV     offsetof(VexGuestPPC32State,guest_XER_OV)
#define OFFB_XER_CA     offsetof(VexGuestPPC32State,guest_XER_CA)




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
                            /*OUT*/ UInt*   size,
                            /*OUT*/ Addr64* whereNext );


/* This is the main (only, in fact) entry point for this module. */

/* Disassemble a complete basic block, starting at guest_pc_start, and
   dumping the IR into global irbb.  Returns the size, in bytes, of
   the basic block.  
*/
IRBB* bbToIR_PPC32 ( UChar*     ppc32Code, 
                   Addr64     guest_pc_start, 
                   Int*       guest_bytes_read, 
                   Bool       (*byte_accessible)(Addr64),
                   Bool       (*chase_into_ok)(Addr64),
                   Bool       host_bigendian,
                   VexSubArch subarch_guest )
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

//   vassert(subarch_guest == VexSubArchPPC_32);

   /* Set up globals. */
   host_is_bigendian = host_bigendian;
   guest_code        = ppc32Code;
   guest_pc_bbstart  = (Addr32)guest_pc_start;
   irbb              = emptyIRBB();

   vassert((guest_pc_start >> 32) == 0);

   /* Delta keeps track of how far along the x86code array we
      have so far gone. */
   delta             = 0;
   n_instrs          = 0;
   *guest_bytes_read = 0;

   while (True) {
      vassert(n_instrs < vex_control.guest_max_insns);

      guest_next = 0;
      resteerOK = n_instrs < vex_control.guest_chase_thresh;
      first_stmt_idx = irbb->stmts_used;

      if (n_instrs > 0) {
         /* for the first insn, the dispatch loop will have set
            GPR1, but for all the others we have to do it ourselves. */
         stmt( IRStmt_Put( OFFB_GPR1, mkU32(guest_pc_bbstart + delta)) );
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
      *guest_bytes_read += size;
      n_instrs++;
      DIP("\n");

      vassert(size > 0 && size <= 18);
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
            delta = (UInt)(guest_next - guest_pc_start);
            n_resteers++;
            d_resteers++;
            if (0 && (n_resteers & 0xFF) == 0)
            vex_printf("resteer[%d,%d] to %p (delta = %d)\n",
                       n_resteers, d_resteers,
                       (void*)(UInt)(guest_next), delta);
            break;
      }
   }
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- x86 insn stream.                                     ---*/
/*------------------------------------------------------------*/

/* Add a statement to the list held by "irbb". */
static void stmt ( IRStmt* st )
{
   addStmtToIRBB( irbb, st );
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleType(ty));
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

#if 0
static UInt extend_s_14to32 ( UInt x )
{
   return (UInt)((((Int)x) << 14) >> 14);
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


/*------------------------------------------------------------*/
/*--- Helpers for constructing IR.                         ---*/
/*------------------------------------------------------------*/

/* Create a 1/2/4 byte read of an x86 integer registers.  For 16/8 bit
   register references, we need to take the host endianness into
   account.  Supplied value is 0 .. 7 and in the Intel instruction
   encoding. */

#if 0
static IRType szToITy ( Int n )
{
   switch (n) {
      case 1: return Ity_I8;
      case 2: return Ity_I16;
      case 4: return Ity_I32;
      default: vpanic("szToITy(PPC32)");
   }
}
#endif


static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);

   vassert(!host_is_bigendian);   //TODO: is this necessary?
   // jrs: probably not; only matters if we reference sub-parts
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
   stmt( IRStmt_Put(integerGuestRegOffset(archreg), e) );
}

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_Tmp(dst, e) );
}

#if 0
static void storeBE ( IRExpr* addr, IRExpr* data )
{
   stmt( IRStmt_STle(addr,data) );
}
#endif

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

static IRExpr* mkU8 ( UInt i )
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8(i));
}

#if 0
static IRExpr* mkU16 ( UInt i )
{
   vassert(i < 65536);
   return IRExpr_Const(IRConst_U16(i));
}
#endif

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

#if 0
static IRExpr* mkU ( IRType ty, UInt i )
{
   if (ty == Ity_I8)  return mkU8(i);
   if (ty == Ity_I16) return mkU16(i);
   if (ty == Ity_I32) return mkU32(i);
   /* If this panics, it usually means you passed a size (1,2,4)
      value as the IRType, rather than a real IRType. */
   vpanic("mkU(PPC32)");
}
#endif

#if 0
static IRExpr* loadBE ( IRType ty, IRExpr* data )
{
   return IRExpr_LDle(ty,data);
}
#endif

#if 0
static IROp mkSizedOp ( IRType ty, IROp op8 )
{
   Int adj;
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);
   vassert(op8 == Iop_Add8 || op8 == Iop_Sub8 
           || op8 == Iop_Mul8 
           || op8 == Iop_Or8 || op8 == Iop_And8 || op8 == Iop_Xor8
           || op8 == Iop_Shl8 || op8 == Iop_Shr8 || op8 == Iop_Sar8
           || op8 == Iop_CmpEQ8 || op8 == Iop_CmpNE8
           || op8 == Iop_Not8 );
   adj = ty==Ity_I8 ? 0 : (ty==Ity_I16 ? 1 : 2);
   return adj + op8;
}
#endif

#if 0
static IROp mkWidenOp ( Int szSmall, Int szBig, Bool signd )
{
   if (szSmall == 1 && szBig == 4) {
      return signd ? Iop_8Sto32 : Iop_8Uto32;
   }
   if (szSmall == 1 && szBig == 2) {
      return signd ? Iop_8Sto16 : Iop_8Uto16;
   }
   if (szSmall == 2 && szBig == 4) {
      return signd ? Iop_16Sto32 : Iop_16Uto32;
   }
   vpanic("mkWidenOp(PPC32,guest)");
}
#endif














/*------------------------------------------------------------*/
/*--- Helpers for %flags.                                 ---*/
/*------------------------------------------------------------*/

/* -------------- Evaluating the flags-thunk. -------------- */

static IRExpr** get_ppc32g_cr0_args ( void )
{
    return mkIRExprVec_3( IRExpr_Get(OFFB_CC_OP,   Ity_I8),
			  IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
			  IRExpr_Get(OFFB_CC_DEP2, Ity_I8) );
}

static IRExpr* mk_ppc32g_calculate_cr0_all ( void )
{
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr0_all", &ppc32g_calculate_cr0_all,
           get_ppc32g_cr0_args()
        );

   /* Exclude OP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
//   call->Iex.CCall.cee->mcx_mask = 1;

// CAB: Haven't looked at the whole 'definedness' stuff...

   return call;
}

#if 0
static IRExpr* mk_ppc32g_calculate_cr0_bit0 ( void )
{
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr0_bit0", &ppc32g_calculate_cr0_bit0,
           get_ppc32g_cr0_args()
        );
   return call;
}

static IRExpr* mk_ppc32g_calculate_cr0_bit1 ( void )
{
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr0_bit1", &ppc32g_calculate_cr0_bit1,
           get_ppc32g_cr0_args()
        );
   return call;
}

static IRExpr* mk_ppc32g_calculate_cr0_bit2 ( void )
{
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr0_bit2", &ppc32g_calculate_cr0_bit2,
           get_ppc32g_cr0_args()
        );
   return call;
}

static IRExpr* mk_ppc32g_calculate_cr0_bit3 ( void )
{
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr0_bit3", &ppc32g_calculate_cr0_bit3,
           get_ppc32g_cr0_args()
        );
   return call;
}
#endif


// Calculate XER_OV flag
static IRExpr* mk_ppc32g_calculate_xer_ov ( UInt op, IRTemp res,
					    IRTemp arg1, IRTemp arg2 )
{
    IRExpr** args =
	mkIRExprVec_5(
	    mkU32(op), mkexpr(res), mkexpr(arg1), mkexpr(arg2),
	    IRExpr_Get(OFFB_XER_OV, Ity_I8) );

   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/,
           "ppc32g_calculate_xer_ov", &ppc32g_calculate_xer_ov,
           args
        );
   return call;
}

// Calculate XER_CA flag
static IRExpr* mk_ppc32g_calculate_xer_ca ( UInt op, IRTemp res,
					    IRTemp arg1, IRTemp arg2 )
{
    IRExpr** args =
	mkIRExprVec_5(
	    mkU32(op), mkexpr(res), mkexpr(arg1), mkexpr(arg2),
	    IRExpr_Get(OFFB_XER_CA, Ity_I8) );

   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/,
           "ppc32g_calculate_xer_ca", &ppc32g_calculate_xer_ca,
           args
        );
   return call;
}



// Helper to set XER_OV,SO flags
static void mk_ppc32g_set_xer_ov_so( UInt op, IRTemp res,
				     IRTemp arg1, IRTemp arg2 )
{
    IRTemp ov = newTemp(Ity_I8);
    assign( ov, mk_ppc32g_calculate_xer_ov( op, res, arg1, arg2 ) );
    stmt( IRStmt_Put( OFFB_XER_OV, mkexpr(ov) ));
    stmt( IRStmt_Put( OFFB_XER_SO, mkexpr(ov) ));
}

// Helper to set XER_CA flag
static void mk_ppc32g_set_xer_ca( UInt op, IRTemp res,
				  IRTemp arg1, IRTemp arg2 )
{
    stmt( IRStmt_Put( OFFB_XER_CA, 
		      mk_ppc32g_calculate_xer_ca( op, res, arg1, arg2 ) ) );
}








/* -------------- Building the flags-thunk. -------------- */

/* The machinery in this section builds the flag-thunk following a
   flag-setting operation.  Hence the various setFlags_* functions.
*/

#if 0
/* U-widen 8/16/32 bit int expr to 32. */
static IRExpr* widenUto32 ( IRExpr* e )
{
   switch (typeOfIRExpr(irbb->tyenv,e)) {
      case Ity_I32: return e;
      case Ity_I16: return unop(Iop_16Uto32,e);
      case Ity_I8:  return unop(Iop_8Uto32,e);
      default: vpanic("widenUto32");
   }
}


/* Narrow 8/16/32 bit int expr to 8/16/32.  Clearly only some
   of these combinations make sense. */
static IRExpr* narrowTo ( IRType dst_ty, IRExpr* e )
{
   IRType src_ty = typeOfIRExpr(irbb->tyenv,e);
   if (src_ty == dst_ty)
      return e;
   if (src_ty == Ity_I32 && dst_ty == Ity_I16)
      return unop(Iop_32to16, e);
   if (src_ty == Ity_I32 && dst_ty == Ity_I8)
      return unop(Iop_32to8, e);

   vex_printf("\nsrc, dst tys are: ");
   ppIRType(src_ty);
   vex_printf(", ");
   ppIRType(dst_ty);
   vex_printf("\n");
   vpanic("narrowTo(PPC32)");
}

#endif


/* Set the flags thunk OP, DEP1, DEP2 fields. */
static 
void setFlags_CR0_Result ( IRTemp result )
{
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU8(0)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(result)) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, IRExpr_Get(OFFB_XER_SO, Ity_I8) ) );
}

/* Set the flags thunk OP, DEP1 fields, write 0 to DEP2. */
static 
void setFlags_CR0_Flags ( IRTemp flags_cr0 )
{
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU8(1)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(flags_cr0)) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU8(0)) );
}













/*
  Integer Arithmetic Instructions
*/
static Bool dis_int_arith ( UInt theInstr, UChar form )
{
    UChar opc1    = (theInstr) & 0x3F;          /* theInstr[0:5]   */
    UChar Rd_addr = (theInstr >> 6 ) & 0x1F;    /* theInstr[6:10]  */
    UChar Ra_addr = (theInstr >> 11) & 0x1F;    /* theInstr[11:15] */
    UInt  SIMM_16 = (theInstr >> 16) & 0xFFFF;  /* theInstr[16:31] */

    UChar Rb_addr = (theInstr >> 16) & 0x1F;    /* theInstr[16:20] */
    UChar flag_OE = (theInstr >> 21) & 1;       /* theInstr[21]    */
    UInt  opc2    = (theInstr >> 22) & 0x1FF;   /* theInstr[22:30] */
    UChar flag_Rc = (theInstr >> 31) & 1;       /* theInstr[31]    */

    UInt EXTS_SIMM = 0;

    IRTemp Ra = newTemp(Ity_I32);
    IRTemp Rb = newTemp(Ity_I32);
    IRTemp Rd = newTemp(Ity_I32);
    IRTemp tmp = newTemp(Ity_I32);

    assign( Ra, getIReg(Ra_addr) );

    if (form == 0) { // D-Form:  rA, rD, EXTS(SIMM)
	EXTS_SIMM = extend_s_16to32(SIMM_16);
    } else {         // XO-Form: rA, rB, rD
	assign( Rb, getIReg(Rb_addr) );
    }

    switch (opc1) {

    /* D-Form */
    case 0x0C: // addi     (Add Immediate)
	if ( Ra_addr == 0 ) {
	    assign( Rd, mkU32(EXTS_SIMM) );
	} else {
	    assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	}

	DIP("addi %i,%i,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	break;

    case 0x0D: // addic    (Add Immediate Carrying)
	assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );

	DIP("addic %i,%i,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	break;
	
    case 0x0E: // addic.   (Add Immediate Carrying and Record)
	assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	setFlags_CR0_Result( Rd );

	DIP("addic. %i,%i,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	break;

    case 0x0F: // addis    (Add Immediate Shifted)
	if ( Ra_addr == 0 ) {
	    assign( Rd, mkU32(EXTS_SIMM << 16) );
	} else {
	    assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM << 16) ) );
	}

	DIP("addis %i,%i,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	break;


    /* XO-Form */
    case 0x1F:
       switch (opc2) {
       case 0x10A: // add       (Add)
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
	   if (flag_Rc)	{ setFlags_CR0_Result( Rd ); }
	   if (flag_OE) { mk_ppc32g_set_xer_ov_so( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb ); }

	   DIP("add%s%s %i,%i,%i\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   break;

       case 0x00A: // addc      (Add Carrying)
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
	   if (flag_Rc)	{ setFlags_CR0_Result( Rd ); }
	   mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	   if (flag_OE) { mk_ppc32g_set_xer_ov_so( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb ); }

	   DIP("addc%s%s %i,%i,%i\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   break;

       case 0x08A: // adde      (Add Extended)
	   // rD = rA + rB + XER[CA]
	   assign( tmp, IRExpr_Get(OFFB_XER_CA, Ity_I32) );
	   assign( Rd, binop(Iop_Add32,
			     binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)),
			     mkexpr(tmp)) );

	   if (flag_Rc)	{ setFlags_CR0_Result( Rd ); }
	   mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADDE, Rd, Ra, Rb );
	   if (flag_OE) { mk_ppc32g_set_xer_ov_so( PPC32G_FLAG_OP_ADDE, Rd, Ra, Rb ); }

	   DIP("adde%s%s %i,%i,%i\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   break;

       case 0x0EA: // addme      (Add to Minus One Extended)
	   // B=0
	   // rD = rA + XER[CA] - 1   (-1 == 0xFFFF_FFFF_FFFF_FFFF)
	   // if (Rc=1) { set guest_result }
	   // set XER[CA]
	   // if (OE=1) { XER[SO,OV] }

	   DIP("addme%s%s %i,%i,%i\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   return False;

       case 0x0CA: // addze      (Add to Zero Extended)
	   // B=0
	   // rD = rA + XER[CA]
	   // if (Rc=1) { set guest_result }
	   // set XER[CA]
	   // if (OE=1) { XER[SO,OV] }

	   DIP("addze%s%s %i,%i,%i\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   return False;

       default:
	   return False;
       }
       break;
    default:
	return False;
    }

    putIReg( Rd_addr, mkexpr(Rd) );

    return False; // True...
}



static Bool dis_int_cmp ( UInt theInstr )
{
    UChar opc1    = (theInstr) & 0x3F;            /* theInstr[0:5]   */
    UChar crfD    = (theInstr >> 6 ) & 0x1F;      /* theInstr[6:8]   */
    UChar b9      = (theInstr >> 9 ) & 0x1;       /* theInstr[9]     */
    UChar flag_L  = (theInstr >> 10) & 0x1;       /* theInstr[10]    */
    UChar Ra_addr = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */

    /* D-Form */
    UInt  SIMM_16 = (theInstr >> 16) & 0xFFFF;    /* theInstr[16:31] */
    UInt  UIMM_16 = (theInstr >> 16) & 0xFFFF;    /* theInstr[16:31] */

    /* X-Form */
    UChar Rb_addr = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UInt  opc2    = (theInstr >> 21) & 0x3F;      /* theInstr[21:30] */
    UChar b31     = (theInstr >> 31) & 1;         /* theInstr[31]    */

    UInt EXTS_SIMM = 0;
    IRTemp Ra = newTemp(Ity_I32);
    IRTemp Rb = newTemp(Ity_I32);
    IRTemp cr_flags = newTemp(Ity_I32);
    IRTemp tmp = newTemp(Ity_I32);
    IRTemp xer_so = newTemp(Ity_I32);
	
    assign( Ra, getIReg(Ra_addr) );
    assign( xer_so, unop(Iop_8Uto32, IRExpr_Get(OFFB_XER_SO, Ity_I8)) );

    if (flag_L==1) { return False; }  // L==1 invalid for 32 bit.

    if (b9 != 0) { return False; }

    switch (opc1) {
    case 0x0B: // cmpi (Compare Immediate, p398)
	EXTS_SIMM = extend_s_16to32(SIMM_16);

	// CAB: This right?  Don't need a 'mkS32(EXTS_SIMM)' ?

	assign( tmp, IRExpr_Mux0X(
		    binop(Iop_CmpEQ32, mkexpr(Ra), mkU32(EXTS_SIMM)),
		    IRExpr_Mux0X( binop(Iop_CmpLT32S, mkU32(EXTS_SIMM), mkexpr(Ra)),
				  mkU32(2), mkU32(4) ), mkU32(8) ));

	assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

	DIP("cmpi %i,%i,%i,%i\n", crfD, flag_L, Ra_addr, SIMM_16);
	break;

    case 0x0A: // cmpli (Compare Logical Immediate, p400)
	assign( tmp, IRExpr_Mux0X(
		    binop(Iop_CmpEQ32, mkexpr(Ra), mkU32(SIMM_16)),
		    IRExpr_Mux0X( binop(Iop_CmpLT32U, mkU32(SIMM_16), mkexpr(Ra)),
				  mkU32(2), mkU32(4) ), mkU32(8) ));

	assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

	DIP("cmpli %i,%i,%i,%i\n", crfD, flag_L, Ra_addr, SIMM_16);
	break;

    /* X Form */
    case 0x1F:
	if (b31 != 0) { return False; }

	switch (opc2) {
	case 0x000: // cmp (Compare, p397)
	    assign( Rb, getIReg(Rb_addr) );
	    assign( tmp, IRExpr_Mux0X(
			binop(Iop_CmpEQ32, mkexpr(Ra), mkexpr(Rb)),
			IRExpr_Mux0X( binop(Iop_CmpLT32S, mkexpr(Rb), mkexpr(Ra)),
				      mkU32(2), mkU32(4) ), mkU32(8) ));
	    assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

	    DIP("cmp %i,%i,%i,%i\n", crfD, flag_L, Ra_addr, Rb_addr);
	    break;

        case 0x020: // cmpl (Compare Logical, p399)
	    assign( Rb, getIReg(Rb_addr) );
	    assign( tmp, IRExpr_Mux0X(
			binop(Iop_CmpEQ32, mkexpr(Ra), mkexpr(Rb)),
			IRExpr_Mux0X( binop(Iop_CmpLT32U, mkexpr(Rb), mkexpr(Ra)),
				      mkU32(2), mkU32(4) ), mkU32(8) ));
	    assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

	    DIP("cmpl %i,%i,%i,%i\n", crfD, flag_L, Ra_addr, Rb_addr);
	    break;

	default:
	    return False;
	}
    default:
	return False;
    }

    if (crfD == 0) {
	setFlags_CR0_Flags( cr_flags );
    } else {
	stmt( IRStmt_Put( OFFB_CR1to7,
			  binop(Iop_Shl32, mkexpr(cr_flags), mkU32(crfD * 4)) ));
    }
    return True;
}



static Bool dis_int_logic ( UInt theInstr )
{
    UChar opc1    = (theInstr) & 0x3F;            /* theInstr[0:5]   */

    /* D-Form */
    UChar S       = (theInstr >> 6 ) & 0x1F;      /* theInstr[6:10]  */
    UChar A       = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  UIMM    = (theInstr >> 16) & 0xFFFF;    /* theInstr[16:31] */

    /* X-Form */
    UChar B       = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UInt  opc2    = (theInstr >> 21) & 0x3F;      /* theInstr[21:30] */
    UChar Rc      = (theInstr >> 31) & 1;         /* theInstr[31]    */

    switch (opc1) {
    case 0x1C: // andi.
	return False;

    case 0x1D: // andis.
	return False;

    case 0x18: // ori
	return False;

    case 0x19: // oris
	return False;

    case 0x1A: // xori
	return False;

    case 0x1B: // xoris
	return False;

    /* X Form */
    case 0x1F:
	switch (opc2) {
	case 0x01C: // and
	    return False;

	case 0x03C: // andc
	    return False;

	case 0x01A: // cntlzw, B=0
	    if (B!=0) { return False; }
	    return False;

	case 0x11C: // eqv
	    return False;

	case 0x3BA: // extsb, B=0
	    if (B!=0) { return False; }
	    return False;

	case 0x39A: // extsh, B=0
	    if (B!=0) { return False; }
	    return False;

	case 0x1DA: // nand
	    return False;

	case 0x07C: // nor
	    return False;

	case 0x1BC: // or
	    return False;

	case 0x19C: // orc
	    return False;

	case 0x13C: // xor
	    return False;

	default:
	    return False;
	}
    default:
	return False;
    }
    return True;
}



static Bool dis_branch ( theInstr )
{
    UChar opc1     = (theInstr) & 0x3F;            /* opcode1: theInstr[0:5]   */
    UChar BO       = (theInstr >> 6 ) & 0x1F;      /* BO:      theInstr[6:10]  */
    UChar BI       = (theInstr >> 11) & 0x1F;      /* BI:      theInstr[11:15] */
    UInt  BD       = (theInstr >> 16) & 0x3FFF;    /* BD:      theInstr[16:29] */
    UChar b16to20  = (theInstr >> 16) & 0x1F;      /* zeros:   theInstr[16:20] */
    UChar opc2     = (theInstr >> 21) & 0x3F;      /* opcode2: theInstr[21:30] */
    UInt  LI       = (theInstr >> 6 ) & 0xFFFFFF;  /* LI:      theInstr[6:29]  */
    UChar flag_AA  = (theInstr >> 30) & 1;         /* AA:      theInstr[30]    */
    UChar flag_LK  = (theInstr >> 31) & 1;         /* LK:      theInstr[31]    */

    IRTemp ctr = newTemp(Ity_I32);
    IRTemp cia = newTemp(Ity_I32);
    IRTemp lr = newTemp(Ity_I32);
    IRTemp nia = newTemp(Ity_I32);
    IRTemp ctr_ok  = newTemp(Ity_I32);
    IRTemp cond_ok = newTemp(Ity_I32);
    IRTemp cr_bit = newTemp(Ity_I32);
    IRTemp tmp = newTemp(Ity_I32);
//    IRTemp tmp2 = newTemp(Ity_I32);

    assign( ctr, IRExpr_Get(OFFB_CTR, Ity_I32) );
    assign( cia, IRExpr_Get(OFFB_CIA, Ity_I32) );

    assign( lr, binop(Iop_Add32, mkexpr(cia), mkU32(4)) );


    switch (opc1) {
    case 0x12: // b                  (Branch)
	assign( tmp, mkU32(extend_s_24to32(LI << 2)) );
	if (flag_AA) {
	    assign( nia, mkexpr(tmp) );
	} else {
	    assign( nia, binop( Iop_Add32, mkexpr(cia), mkexpr(tmp) ));
	}
	if (flag_LK) {
	    stmt( IRStmt_Put( OFFB_LR, mkexpr(lr) ));
	}

	irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
	irbb->next     = mkexpr(nia);

	DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", LI);
	break;

    case 0x10: // bc                 (Branch Conditional)

	// Need to assert any of the bits of B0 ?

	if (!((BO>>2)&1)) {
	    assign( ctr, binop( Iop_Sub32, mkexpr(ctr), mkU32(1) ) );
	    stmt( IRStmt_Put( OFFB_CTR, mkexpr(ctr)) );
	}

	// ctr_ok = BO[2] | ((CTR[31] != 0) ^ BO[3])
	assign( ctr_ok,
		binop( Iop_Or32, mkexpr((BO>>2)&1),
		       binop( Iop_Xor32, mkU32((BO>>3)&1),
			      binop( Iop_CmpNE32, mkU32(0),
				     binop( Iop_And32,
					    mkexpr(ctr),
					    mkU32(1<<31) )))));
	    
	// cond_ok = BO[0] | (CR[BI] == BO[1])
	if (BI < 4) { // Get from guest_CC_OP etc.
	    assign( cr_bit, binop(Iop_And32, mkU32(1),
				  binop(Iop_Shr32,
					mk_ppc32g_calculate_cr0_all(),
					mkU32(BI))) );
	} else {      // Get from guest_CR1to7
	    assign( cr_bit, binop(Iop_And32, mkU32(1),
				  binop(Iop_Shr32,
					IRExpr_Get(OFFB_CR1to7, Ity_I32),
					mkU32(BI))) );
	}
	assign( cond_ok, binop( Iop_Or32, mkexpr(BO & 1),
				binop( Iop_CmpEQ8, mkexpr(cr_bit),
				       mkU32((BO>>1)&1) )));


	// CAB: This is getting silly - Maybe use a helper function?

/*
	stmt( IRStmt_Exit( mk_x86g_calculate_condition(condPos),
			   Ijk_Boring,
			   IRConst_U32(d32_false) ) );
	irbb->next     = mkU32(d32_true);
	irbb->jumpkind = Ijk_Boring;
*/

/*
	assign( tmp, binop(Iop_And32, mkexpr(ctr_ok), mkexpr(cond_ok)) );
	if (tmp) {
	    assign( tmp2, mkU32(extend_s_24to32(BD << 2)) );
	    if (flag_AA) {
	        assign( nia, mkexpr(tmp2) );
	    } else {
	        assign( nia, binop(Iop_Add32, mkexpr(cia), mkexpr(tmp2)) );
	    }
	    if (flag_LK) {
		stmt( IRStmt_Put( OFFB_LR, mkexpr(lr) ));
	    }

	    irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
	    irbb->next     = mkexpr(nia);
	}
*/

	DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
	    flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, BD);
	return False;

    case 0x13:
	if (b16to20!=0) { return False; }

	switch (opc2) {
        case 0x210: // bcctr         (Branch Conditional to Count Register) 
/*
	    cond_ok = BO[0] | (CR[BI] == BO[1])
	    if (cond_ok) {
		NIA = CTR[0-61] || 0b00
		if (flag_LK) {
		    LR = CIA + 4
		}
	    }
*/
	    DIP("bcctr%s 0x%x, 0x%x,\n", flag_LK ? "l" : "", BO, BI);
	    return False;

        case 0x010: // bclr          (Branch Conditional to Link Register) 
/*
	    if (!BO[2]) {
		CTR -= 1
	    }
	    ctr_ok = BO[2] | ((CTR[31] == 0) ^ BO[3])
            cond_ok = BO[0] | (CR[BI] a BO[1])
            if (ctr_ok & cond_ok) {
		NIA = LR[0-61] || 0b00
		if (flag_LK) {
		    LR = CIA + 4
		}
*/
	    DIP("bclr%s 0x%x, 0x%x,\n", flag_LK ? "l" : "", BO, BI);
	    return False;

        default:
	    return False;
	}
	break;
    default:
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
                            /*OUT*/ UInt*   size,
                            /*OUT*/ Addr64* whereNext )
{
//   IRType    ty;
//   IRTemp    addr, t1, t2;
//   Int       alen;
   UChar opc1, opc2;
//   PPC32Condcode cond;
//   UInt      d32;
//   UChar     dis_buf[50];
//   Int       am_sz, d_sz;
   DisResult whatNext = Dis_Continue;
   UInt      theInstr;


   /* At least this is simple on PPC32: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */

   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_PPC32 abort if we fail the
      decode. */
   *size = 0;

   theInstr = *(UInt*)(&guest_code[delta]);

//   vex_printf("START: 0x%x, %,b\n", theInstr, theInstr );

   DIP("\t0x%x:  ", guest_pc_bbstart+delta);



   // TODO: fix the client-request stuff, else nothing will work

   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
//      UInt* code = (UInt*)(guest_code + delta);

      // CAB: easy way to rotate left?

      /* Spot this:                                       
         E1A00EE0                   mov  r0, r0, ror #29
	 E1A001E0                   mov  r0, r0, ror #3
	 E1A00DE0                   mov  r0, r0, ror #27
	 E1A002E0                   mov  r0, r0, ror #5
	 E1A006E0                   mov  r0, r0, ror #13
	 E1A009E0                   mov  r0, r0, ror #19
      */
      /* I suspect these will have to be turned the other way round to
	 work on little-endian ppc32. */
      if (0){
/*
	  code[0] == 0xE1A00EE0 &&
          code[1] == 0xE1A001E0 &&
          code[2] == 0xE1A00DE0 &&
          code[3] == 0xE1A002E0 &&
          code[4] == 0xE1A006E0 &&
	  code[5] == 0xE1A009E0) {
*/
         // uh ... I'll figure this out later.  possibly r0 = client_request(r0)
         DIP("?CAB? = client_request ( ?CAB? )\n");

	 *size = 24;

	 irbb->next     = mkU32(guest_pc_bbstart+delta);
	 irbb->jumpkind = Ijk_ClientReq;

         whatNext = Dis_StopHere;
         goto decode_success;
      }
   }


   opc1 = (theInstr) & 0x3F;           /* opcode1: [0:5] */
   opc2 = (theInstr >> 21) & 0x3FF;    /* opcode2: [21:30] */

//   vex_printf("disInstr(ppc): opcode1: 0x%2x, %,09b\n", opc1, opc1 );
//   vex_printf("disInstr(ppc): opcode2: 0x%2x, %,09b\n", opc2, opc2 );

   // Note: all 'reserved' bits must be cleared, else invalid
   switch (opc1) {

   /*
     Integer Arithmetic Instructions
   */
   case 0x0C: // addi
   case 0x0D: // addic
   case 0x0E: // addic.
   case 0x0F: // addis
   case 0x07: // mulli
   case 0x08: // subfic
       if (dis_int_arith(theInstr, 0)) break;
       goto decode_failure;

   /*
     Integer Compare Instructions
   */
   case 0x0B: // cmpi
   case 0x0A: // cmpli
       if (dis_int_cmp(theInstr)) break;
       goto decode_failure;

   /*
     Integer Logical Instructions
   */
   case 0x1C: // andi.
   case 0x1D: // andis.
   case 0x18: // ori
   case 0x19: // oris
   case 0x1A: // xori
   case 0x1B: // xoris
       if (dis_int_logic(theInstr)) break;
       goto decode_failure;

   /*
      Branch Instructions
   */
   case 0x12: // b
   case 0x10: // bc
       if (dis_branch(theInstr)) break;
       goto decode_failure;


   case 0x13:
       switch (opc2) {

       /*
	 Branch Instructions
       */
       case 0x210: // bcctr
       case 0x010: // bclr
	   if (dis_branch(theInstr)) break;
	   goto decode_failure;

       default:
	   goto decode_failure;
       }



    case 0x1F:

       opc2 = (theInstr >> 22) & 0x1FF;    /* [22:30] */
       switch (opc2) {

       /*
	 Integer Arithmetic Instructions
       */
       case 0x10A: // add
       case 0x00A: // addc
       case 0x08A: // adde
       case 0x0EA: // addme
       case 0x0CA: // addze
	   if (dis_int_arith(theInstr, 1)) goto decode_success;
	   goto decode_failure;

       default:
	   break;
       }

       opc2 = (theInstr >> 21) & 0x3FF;    /* [21:30] */	
       switch (opc2) {

       /*
	 Integer Compare Instructions
	 Rc=0
       */
       case 0x000: // cmp, b9=0
       case 0x020: // cmpl, b9=0
	   if (dis_int_cmp(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Logical Instructions
	 No OE, have Rc:
       */
       case 0x01C: // and
       case 0x03C: // andc
       case 0x01A: // cntlzw, B=0
       case 0x11C: // eqv
       case 0x3BA: // extsb, B=0
       case 0x39A: // extsh, B=0
       case 0x1DA: // nand
       case 0x07C: // nor
       case 0x1BC: // or
       case 0x19C: // orc
       case 0x13C: // xor
	   if (dis_int_logic(theInstr)) break;
	   goto decode_failure;

       default:
	   goto decode_failure;
       }



   default:
   decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(ppc32): unhandled instruction: "
              "0x%x\n", theInstr);
   vpanic("ppc32ToIR: unimplemented insn");

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
