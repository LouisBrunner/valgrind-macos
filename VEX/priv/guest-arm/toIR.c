
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-arm/toIR.c) is                              ---*/
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

/* Translates ARM(v4) code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_arm.h"

#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "guest-arm/gdefs.h"


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
/*--- Offsets of various parts of the arm guest state.     ---*/
/*------------------------------------------------------------*/

#define OFFB_R0       offsetof(VexGuestARMState,guest_R0)
// etc etc
#define OFFB_R15      offsetof(VexGuestARMState,guest_R15)


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
IRBB* bbToIR_ARM ( UChar* armCode, 
                   Addr64 guest_pc_start, 
                   Int*   guest_bytes_read, 
                   Bool   (*byte_accessible)(Addr64),
                   Bool   (*chase_into_ok)(Addr64),
                   Bool   host_bigendian )
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

   /* Set up globals. */
   host_is_bigendian = host_bigendian;
   guest_code        = armCode;
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
            R15, but for all the others we have to do it ourselves. */
         stmt( IRStmt_Put( OFFB_R15, mkU32(guest_pc_bbstart + delta)) );
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

/* Bomb out if we can't handle something. */
__attribute__ ((noreturn))
static void unimplemented ( Char* str )
{
   vex_printf("armToIR: unimplemented feature\n");
   vpanic(str);
}

/* Various simple conversions */

static UInt extend_s_8to32( UInt x )
{
   return (UInt)((((Int)x) << 24) >> 24);
}

static UInt extend_s_16to32 ( UInt x )
{
   return (UInt)((((Int)x) << 16) >> 16);
}

/* Fetch a byte from the guest insn stream. */
static UChar getIByte ( UInt delta )
{
   return guest_code[delta];
}

/* Get a 8/16/32-bit unsigned value out of the insn stream. */

static UInt getUChar ( UInt delta )
{
   UInt v = guest_code[delta+0];
   return v & 0xFF;
}

static UInt getUDisp16 ( UInt delta )
{
   UInt v = guest_code[delta+1]; v <<= 8;
   v |= guest_code[delta+0];
   return v & 0xFFFF;
}

static UInt getUDisp32 ( UInt delta )
{
   UInt v = guest_code[delta+3]; v <<= 8;
   v |= guest_code[delta+2]; v <<= 8;
   v |= guest_code[delta+1]; v <<= 8;
   v |= guest_code[delta+0];
   return v;
}

static UInt getUDisp ( Int size, UInt delta )
{
   switch (size) {
      case 4: return getUDisp32(delta);
      case 2: return getUDisp16(delta);
      case 1: return getUChar(delta);
      default: vpanic("getUDisp(x86)");
   }
   return 0; /*notreached*/
}


/* Get a byte value out of the insn stream and sign-extend to 32
   bits. */
static UInt getSDisp8 ( UInt delta )
{
   return extend_s_8to32( (UInt) (guest_code[delta]) );
}

static UInt getSDisp16 ( UInt delta0 )
{
   UChar* eip = (UChar*)(&guest_code[delta0]);
   UInt d = *eip++;
   d |= ((*eip++) << 8);
   return extend_s_16to32(d);
}

static UInt getSDisp ( Int size, UInt delta )
{
   switch (size) {
      case 4: return getUDisp32(delta);
      case 2: return getSDisp16(delta);
      case 1: return getSDisp8(delta);
      default: vpanic("getSDisp(x86)");
  }
  return 0; /*notreached*/
}


/*------------------------------------------------------------*/
/*--- Helpers for constructing IR.                         ---*/
/*------------------------------------------------------------*/

/* Create a 1/2/4 byte read of an x86 integer registers.  For 16/8 bit
   register references, we need to take the host endianness into
   account.  Supplied value is 0 .. 7 and in the Intel instruction
   encoding. */

static IRType szToITy ( Int n )
{
   switch (n) {
      case 1: return Ity_I8;
      case 2: return Ity_I16;
      case 4: return Ity_I32;
      default: vpanic("szToITy(x86)");
   }
}

static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 16);

   vassert(!host_is_bigendian);   //TODO: is this necessary?

   switch (archreg) {
      case  0: return offsetof(VexGuestARMState,guest_R0);
      //etc etc
      case 15: return offsetof(VexGuestARMState,guest_R15);
   }

   /* NOTREACHED */
   vpanic("integerGuestRegOffset(arm,le)");
}

static IRExpr* getIReg ( UInt archreg )
{
   vassert(archreg < 16);
   return IRExpr_Get( integerGuestRegOffset(archreg), Ity_I32 );
}

/* Ditto, but write to a reg instead. */
static void putIReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 16);
   stmt( IRStmt_Put(integerGuestRegOffset(archreg), e) );
}

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_Tmp(dst, e) );
}

static void storeLE ( IRExpr* addr, IRExpr* data )
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

static IRExpr* mkU8 ( UInt i )
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8(i));
}

static IRExpr* mkU16 ( UInt i )
{
   vassert(i < 65536);
   return IRExpr_Const(IRConst_U16(i));
}

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* mkU ( IRType ty, UInt i )
{
   if (ty == Ity_I8)  return mkU8(i);
   if (ty == Ity_I16) return mkU16(i);
   if (ty == Ity_I32) return mkU32(i);
   /* If this panics, it usually means you passed a size (1,2,4)
      value as the IRType, rather than a real IRType. */
   vpanic("mkU(x86)");
}

static IRExpr* loadLE ( IRType ty, IRExpr* data )
{
   return IRExpr_LDle(ty,data);
}

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
   vpanic("mkWidenOp(x86,guest)");
}


/*------------------------------------------------------------*/
/*--- Helpers for %eflags.                                 ---*/
/*------------------------------------------------------------*/
#if 0
// Ceri: you'll need to make simplified versions of these
// for arm
/* -------------- Evaluating the flags-thunk. -------------- */

/* Build IR to calculate all the eflags from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression ::
   Ity_I32. */
static IRExpr* mk_x86g_calculate_eflags_all ( void )
{
   IRExpr** args
      = mkIRExprVec_4( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                       IRExpr_Get(OFFB_CC_NDEP, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "x86g_calculate_eflags_all", &x86g_calculate_eflags_all,
           args
        );
   /* Exclude OP and NDEP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);
   return call;
}

/* Build IR to calculate some particular condition from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression ::
   Ity_Bit. */
static IRExpr* mk_x86g_calculate_condition ( X86Condcode cond )
{
   IRExpr** args
      = mkIRExprVec_5( mkU32(cond),
                       IRExpr_Get(OFFB_CC_OP,  Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                       IRExpr_Get(OFFB_CC_NDEP, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "x86g_calculate_condition", &x86g_calculate_condition,
           args
        );
   /* Exclude the requested condition, OP and NDEP from definedness
      checking.  We're only interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<1) | (1<<4);
   return unop(Iop_32to1, call);
}

/* Build IR to calculate just the carry flag from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression :: Ity_I32. */
static IRExpr* mk_x86g_calculate_eflags_c ( void )
{
   IRExpr** args
      = mkIRExprVec_4( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                       IRExpr_Get(OFFB_CC_NDEP, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "x86g_calculate_eflags_c", &x86g_calculate_eflags_c,
           args
        );
   /* Exclude OP and NDEP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);
   return call;
}


/* -------------- Building the flags-thunk. -------------- */

/* The machinery in this section builds the flag-thunk following a
   flag-setting operation.  Hence the various setFlags_* functions.
*/

static Bool isAddSub ( IROp op8 )
{
   return op8 == Iop_Add8 || op8 == Iop_Sub8;
}

static Bool isLogic ( IROp op8 )
{
   return op8 == Iop_And8 || op8 == Iop_Or8 || op8 == Iop_Xor8;
}

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

/* S-widen 8/16/32 bit int expr to 32. */
static IRExpr* widenSto32 ( IRExpr* e )
{
   switch (typeOfIRExpr(irbb->tyenv,e)) {
      case Ity_I32: return e;
      case Ity_I16: return unop(Iop_16Sto32,e);
      case Ity_I8:  return unop(Iop_8Sto32,e);
      default: vpanic("widenSto32");
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
   vpanic("narrowTo(x86)");
}


/* Set the flags thunk OP, DEP1 and DEP2 fields.  The supplied op is
   auto-sized up to the real op. */

static 
void setFlags_DEP1_DEP2 ( IROp op8, IRTemp dep1, IRTemp dep2, IRType ty )
{
   Int ccOp = ty==Ity_I8 ? 0 : (ty==Ity_I16 ? 1 : 2);

   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   switch (op8) {
      case Iop_Add8: ccOp += X86G_CC_OP_ADDB;   break;
      case Iop_Sub8: ccOp += X86G_CC_OP_SUBB;   break;
      default:       ppIROp(op8);
                     vpanic("setFlags_DEP1_DEP2(x86)");
   }
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(ccOp)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(dep1))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(mkexpr(dep2))) );
}


/* Set the OP and DEP1 fields only, and write zero to DEP2. */

static 
void setFlags_DEP1 ( IROp op8, IRTemp dep1, IRType ty )
{
   Int ccOp = ty==Ity_I8 ? 0 : (ty==Ity_I16 ? 1 : 2);

   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   switch (op8) {
      case Iop_Or8:
      case Iop_And8:
      case Iop_Xor8: ccOp += X86G_CC_OP_LOGICB; break;
      default:       ppIROp(op8);
                     vpanic("setFlags_DEP1(x86)");
   }
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(ccOp)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(dep1))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0)) );
}


/* For shift operations, we put in the result and the undershifted
   result.  Except if the shift amount is zero, the thunk is left
   unchanged. */

static void setFlags_DEP1_DEP2_shift ( IROp    op32,
                                       IRTemp  res,
                                       IRTemp  resUS,
                                       IRType  ty,
                                       IRTemp  guard )
{
   Int ccOp = ty==Ity_I8 ? 2 : (ty==Ity_I16 ? 1 : 0);

   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);
   vassert(guard);

   /* Both kinds of right shifts are handled by the same thunk
      operation. */
   switch (op32) {
      case Iop_Shr32:
      case Iop_Sar32: ccOp = X86G_CC_OP_SHRL - ccOp; break;
      case Iop_Shl32: ccOp = X86G_CC_OP_SHLL - ccOp; break;
      default:        ppIROp(op32);
                      vpanic("setFlags_DEP1_DEP2_shift(x86)");
   }

   /* DEP1 contains the result, DEP2 contains the undershifted value. */
   stmt( IRStmt_Put( OFFB_CC_OP,
                     IRExpr_Mux0X( mkexpr(guard),
                                   IRExpr_Get(OFFB_CC_OP,Ity_I32),
                                   mkU32(ccOp))) );
   stmt( IRStmt_Put( OFFB_CC_DEP1,
                     IRExpr_Mux0X( mkexpr(guard),
                                   IRExpr_Get(OFFB_CC_DEP1,Ity_I32),
                                   widenUto32(mkexpr(res)))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, 
                     IRExpr_Mux0X( mkexpr(guard),
                                   IRExpr_Get(OFFB_CC_DEP2,Ity_I32),
                                   widenUto32(mkexpr(resUS)))) );
}


/* For the inc/dec case, we store in DEP1 the result value and in NDEP
   the former value of the carry flag, which unfortunately we have to
   compute. */

static void setFlags_INC_DEC ( Bool inc, IRTemp res, IRType ty )
{
   Int ccOp = inc ? X86G_CC_OP_INCB : X86G_CC_OP_DECB;
   
   ccOp += ty==Ity_I8 ? 0 : (ty==Ity_I16 ? 1 : 2);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   /* This has to come first, because calculating the C flag 
      may require reading all four thunk fields. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mk_x86g_calculate_eflags_c()) );
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(ccOp)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(res)) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0)) );
}


/* Multiplies are pretty much like add and sub: DEP1 and DEP2 hold the
   two arguments. */

static
void setFlags_MUL ( IRType ty, IRTemp arg1, IRTemp arg2, UInt base_op )
{
   switch (ty) {
      case Ity_I8:
         stmt( IRStmt_Put( OFFB_CC_OP, mkU32(base_op+0) ) );
         break;
      case Ity_I16:
         stmt( IRStmt_Put( OFFB_CC_OP, mkU32(base_op+1) ) );
         break;
      case Ity_I32:
         stmt( IRStmt_Put( OFFB_CC_OP, mkU32(base_op+2) ) );
         break;
      default:
         vpanic("setFlags_MUL(x86)");
   }
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(arg1)) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(mkexpr(arg2)) ));
}
#endif


/* -------------- Condition codes. -------------- */

/* Condition codes, using the Intel encoding.  */

static Char* name_ARMCondcode ( ARMCondcode cond )
{
   switch (cond) {
// ToDo: rehash for ARM
#if 0
      case X86CondO:      return "o";
      case X86CondNO:     return "no";
      case X86CondB:      return "b";
      case X86CondNB:     return "nb";
      case X86CondZ:      return "z";
      case X86CondNZ:     return "nz";
      case X86CondBE:     return "be";
      case X86CondNBE:    return "nbe";
      case X86CondS:      return "s";
      case X86CondNS:     return "ns";
      case X86CondP:      return "p";
      case X86CondNP:     return "np";
      case X86CondL:      return "l";
      case X86CondNL:     return "nl";
      case X86CondLE:     return "le";
      case X86CondNLE:    return "nle";
#endif
      default: vpanic("name_ARMCondcode");
   }
}

#if 0
// TODO: probably useful for ARM too
static 
X86Condcode positiveIse_X86Condcode ( X86Condcode  cond,
                                      Bool*     needInvert )
{
   vassert(cond >= X86CondO && cond <= X86CondNLE);
   if (cond & 1) {
      *needInvert = True;
      return cond-1;
   } else {
      *needInvert = False;
      return cond;
   }
}
#endif


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
   IRType    ty;
   IRTemp    addr, t1, t2;
   Int       alen;
   UChar     opc, modrm, abyte;
   UInt      d32;
   UChar     dis_buf[50];
   Int       am_sz, d_sz;
   DisResult whatNext = Dis_Continue;
   UInt      theInstr;

   /* Holds pc at the start of the insn, so that we can print
      consistent error messages for unimplemented insns. */
   UInt delta_start = delta;

   /* At least this is simple on ARM: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */

   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_ARM abort if we fail the
      decode. */
   *size = 0;

   theInstr = *(UInt*)(&guest_code[delta]);

   DIP("\t0x%x:  ", guest_pc_bbstart+delta);

#if 0
   // TODO: fix the client-request stuff, else nothing will work
   /* Spot the client-request magic sequence. */
   {
      UChar* code = (UChar*)(guest_code + delta);
      /* Spot this:
         C1C01D                roll $29, %eax
         C1C003                roll $3,  %eax
         C1C81B                rorl $27, %eax
         C1C805                rorl $5,  %eax
         C1C00D                roll $13, %eax
         C1C013                roll $19, %eax      
      */
      if (code[ 0] == 0xC1 && code[ 1] == 0xC0 && code[ 2] == 0x1D &&
          code[ 3] == 0xC1 && code[ 4] == 0xC0 && code[ 5] == 0x03 &&
          code[ 6] == 0xC1 && code[ 7] == 0xC8 && code[ 8] == 0x1B &&
          code[ 9] == 0xC1 && code[10] == 0xC8 && code[11] == 0x05 &&
          code[12] == 0xC1 && code[13] == 0xC0 && code[14] == 0x0D &&
          code[15] == 0xC1 && code[16] == 0xC0 && code[17] == 0x13
         ) {
         DIP("%%edx = client_request ( %%eax )\n");         
         delta += 18;
         jmp_lit(Ijk_ClientReq, guest_eip_bbstart+delta);
         whatNext = Dis_StopHere;
         goto decode_success;
      }
   }
#endif

   /* As per ARM ARM v2 page A3-2, primary opcode appears to be in
      bits 27:21 of the instruction (roughly).  Hence ... */

   switch ((theInstr >> 20) & 0x7F) {

   /* ------------------------ ??? ------------------------ */
  
   default:
   decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(arm): unhandled instruction: "
              "0x%x\n", theInstr );
   vpanic("armToIR: unimplemented insn");

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   DIP("\n");

   *size = 4;
   return whatNext;
}

#undef DIP
#undef DIS

/*--------------------------------------------------------------------*/
/*--- end                                         guest-arm/toIR.c ---*/
/*--------------------------------------------------------------------*/
