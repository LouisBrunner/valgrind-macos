
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
#define OFFB_R1       offsetof(VexGuestARMState,guest_R1)
#define OFFB_R2       offsetof(VexGuestARMState,guest_R2)
#define OFFB_R3       offsetof(VexGuestARMState,guest_R3)
#define OFFB_R4       offsetof(VexGuestARMState,guest_R4)
#define OFFB_R5       offsetof(VexGuestARMState,guest_R5)
#define OFFB_R6       offsetof(VexGuestARMState,guest_R6)
#define OFFB_R7       offsetof(VexGuestARMState,guest_R7)
#define OFFB_R8       offsetof(VexGuestARMState,guest_R8)
#define OFFB_R9       offsetof(VexGuestARMState,guest_R9)
#define OFFB_R10      offsetof(VexGuestARMState,guest_R10)
#define OFFB_R11      offsetof(VexGuestARMState,guest_R11)
#define OFFB_R12      offsetof(VexGuestARMState,guest_R12)
#define OFFB_R13      offsetof(VexGuestARMState,guest_R13)
#define OFFB_R14      offsetof(VexGuestARMState,guest_R14)
#define OFFB_R15      offsetof(VexGuestARMState,guest_R15)

// CAB: ? guest_SYSCALLNO;

#define OFFB_CC_OP    offsetof(VexGuestARMState,guest_CC_OP)
#define OFFB_CC_DEP1  offsetof(VexGuestARMState,guest_CC_DEP1)
#define OFFB_CC_DEP2  offsetof(VexGuestARMState,guest_CC_DEP2)

// CAB: ? guest_EMWARN;


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
      default: vpanic("getUDisp(ARM)");
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
      default: vpanic("getSDisp(ARM)");
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
      default: vpanic("szToITy(ARM)");
   }
}

static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 16);

   vassert(!host_is_bigendian);   //TODO: is this necessary?
   // jrs: probably not; only matters if we reference sub-parts
   // of the arm registers, but that isn't the case
   switch (archreg) {
      case  0: return offsetof(VexGuestARMState, guest_R0);
      case  1: return offsetof(VexGuestARMState, guest_R1);
      case  2: return offsetof(VexGuestARMState, guest_R2);
      case  3: return offsetof(VexGuestARMState, guest_R3);
      case  4: return offsetof(VexGuestARMState, guest_R4);
      case  5: return offsetof(VexGuestARMState, guest_R5);
      case  6: return offsetof(VexGuestARMState, guest_R6);
      case  7: return offsetof(VexGuestARMState, guest_R7);
      case  8: return offsetof(VexGuestARMState, guest_R8);
      case  9: return offsetof(VexGuestARMState, guest_R9);
      case 10: return offsetof(VexGuestARMState,guest_R10);
      case 11: return offsetof(VexGuestARMState,guest_R11);
      case 12: return offsetof(VexGuestARMState,guest_R12);
      case 13: return offsetof(VexGuestARMState,guest_R13);
      case 14: return offsetof(VexGuestARMState,guest_R14);
      case 15: return offsetof(VexGuestARMState,guest_R15);
   }

   vpanic("integerGuestRegOffset(arm,le)"); /*notreached*/
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
   vpanic("mkU(ARM)");
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
   vpanic("mkWidenOp(ARM,guest)");
}















/*------------------------------------------------------------*/
/*--- Helpers for %eflags.                                 ---*/
/*------------------------------------------------------------*/

/* -------------- Evaluating the flags-thunk. -------------- */

/* Build IR to calculate all the eflags from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.
   Returns an expression :: Ity_I32. */
static IRExpr* mk_armg_calculate_flags_all ( void )
{
   IRExpr** args
      = mkIRExprVec_3( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_flags_all", &armg_calculate_flags_all,
           args
        );

   /* Exclude OP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = 1;
   return call;
}



/* Build IR to calculate some particular condition from stored
   CC_OP/CC_DEP1/CC_DEP2.  Returns an expression
   of type Ity_I1.
*/
static IRExpr* mk_armg_calculate_condition ( ARMCondcode cond )
{
   IRExpr** args
      = mkIRExprVec_4( mkU32(cond),
                       IRExpr_Get(OFFB_CC_OP,  Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_condition", &armg_calculate_condition,
           args
        );

   /* Exclude the requested condition and OP from definedness
      checking.  We're only interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<1);
   return unop(Iop_32to1, call);
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
   vpanic("narrowTo(ARM)");
}


/* Set the flags thunk OP, DEP1 and DEP2 fields.  The supplied op is
   auto-sized up to the real op. */

static 
void setFlags_DEP1_DEP2 ( IROp op, IRTemp dep1, IRTemp dep2 )
{
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(op)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(dep1))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(mkexpr(dep2))) );
}


/* Set the OP and DEP1 fields only, and write zero to DEP2. */

static 
void setFlags_DEP1 ( IROp op, IRTemp dep1 )
{
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(op)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(dep1))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0)) );
}


/* For shift operations, we put in the result and the undershifted
   result.  Except if the shift amount is zero, the thunk is left
   unchanged. */

static void setFlags_DEP1_DEP2_shift ( IROp    op,
                                       IRTemp  res,
                                       IRTemp  resUS,
                                       IRType  ty,
                                       IRTemp  guard )
{
    vassert(guard);

   /* DEP1 contains the result, DEP2 contains the undershifted value. */
   stmt( IRStmt_Put( OFFB_CC_OP,
                     IRExpr_Mux0X( mkexpr(guard),
                                   IRExpr_Get(OFFB_CC_OP,Ity_I32),
                                   mkU32(op))) );
   stmt( IRStmt_Put( OFFB_CC_DEP1,
                     IRExpr_Mux0X( mkexpr(guard),
                                   IRExpr_Get(OFFB_CC_DEP1,Ity_I32),
                                   widenUto32(mkexpr(res)))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, 
                     IRExpr_Mux0X( mkexpr(guard),
                                   IRExpr_Get(OFFB_CC_DEP2,Ity_I32),
                                   widenUto32(mkexpr(resUS)))) );
}







/* Multiplies are pretty much like add and sub: DEP1 and DEP2 hold the
   two arguments. */

static
void setFlags_MUL ( IRTemp arg1, IRTemp arg2, UInt op )
{
    stmt( IRStmt_Put( OFFB_CC_OP, mkU32(op) ) );
    stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(arg1)) ));
    stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(mkexpr(arg2)) ));
}










/* -------------- Condition codes. -------------- */

/* Condition codes, using the ARM encoding.  */

// CAB: Just used for debugging printouts ?
// yes, only for debugging
static HChar* name_ARMCondcode ( ARMCondcode cond )
{
   switch (cond) {
       case ARMCondEQ:    return "eq";
       case ARMCondNE:    return "ne";
       case ARMCondHS:    return "hs";
       case ARMCondLO:    return "no";
       case ARMCondMI:    return "mi";
       case ARMCondPL:    return "pl";
       case ARMCondVS:    return "vs";
       case ARMCondVC:    return "vc";
       case ARMCondHI:    return "hi";
       case ARMCondLS:    return "ls";
       case ARMCondGE:    return "ge";
       case ARMCondLT:    return "lt";
       case ARMCondGT:    return "gt";
       case ARMCondLE:    return "le";
       case ARMCondAL:    return "al";
       case ARMCondNV:    return "nv";
       default: vpanic("name_ARMCondcode");
   }
}


static 
ARMCondcode positiveIse_ARMCondcode ( ARMCondcode  cond,
                                      Bool*     needInvert )
{
   vassert(cond >= ARMCondEQ && cond <= ARMCondNV);
   if (cond & 1) {
      *needInvert = True;
      return cond-1;
   } else {
      *needInvert = False;
      return cond;
   }
}







// note, i wasn't clear which Rd/Rm/Rs are supposed to be
// IRTemps and which are Ints.

// ARMG_CC_OP_LSL
static
IRExpr* dis_shift_lsl ( UInt theInstr )
{
    UChar set_flags = (theInstr >> 20) & 1;    // instr[20]
    UChar is_reg = (theInstr >> 4) & 1;   // instr[4]
    UChar Rm = theInstr & 0xF;   
    IRTemp Rm_tmp = newTemp(Ity_I32);
    IRTemp Rs_tmp = newTemp(Ity_I32);
    IRTemp imm_tmp = newTemp(Ity_I32);
    IRTemp res = newTemp(Ity_I32);
    UInt imm;
    UInt Rs_0;

    assign( Rm_tmp, getIReg(Rm) );


    if (is_reg) {  // Register Shift
	vex_printf("dis_shift_lsl: reg\n");
	assign( Rs_tmp, getIReg((theInstr >> 8) & 0xF) );  // instr[11:8]

	Rs_0 = Rs_tmp & 0xFF;     // Rs[7:0]
	if ( Rs_0 == 0 ) {        // op = Rm, carry = C Flag;
	    vex_printf("dis_shift_lsl: 1\n");
	    // No LSL: cf not changed -> don't track
	    return getIReg(Rm);
	}
	else if ( Rs_0 < 32 ) {   // op = Rm LSL Rs, carry = Rm[32 - Rs]
	    vex_printf("dis_shift_lsl: 2\n");
	    assign( res, binop(Iop_Shl32, getIReg(Rm), mkexpr(Rs_tmp)) );
	    setFlags_DEP1_DEP2( ARMG_CC_OP_LSL, Rm_tmp, Rs_tmp );
	    return mkexpr(res);
	}
	else if ( Rs_0 == 32 ) {  // op = 0, carry = Rm[0];
	    vex_printf("dis_shift_lsl: 3\n");
	    setFlags_DEP1_DEP2( ARMG_CC_OP_LSL, Rm_tmp, Rs_tmp );
	    return mkexpr(0);
	}
	else {                    // op = 0, carry = 0;
	    vex_printf("dis_shift_lsl: 4\n");
	    setFlags_DEP1_DEP2( ARMG_CC_OP_LSL, Rm_tmp, Rs_tmp );
	    return mkexpr(0);
	}
    }
    else {  // Immediate shift
	vex_printf("dis_shift_lsl: imm\n");
	imm = (theInstr >> 7) & 0x1F;     // instr[11:7]

	assign( imm_tmp, mkU32(imm) );

	if ( imm == 0 ) {         // op = Rm, carry = C Flag;
	    vex_printf("dis_shift_lsl: 1\n");
	    // No LSL: cf not changed -> don't track
	    return getIReg(Rm);
	}
	else {                    // op = Rm LSL imm, carry = Rm[32 - imm];
	    vex_printf("dis_shift_lsl: 2\n");
	    assign( res, binop(Iop_Shl32, getIReg(Rm), mkU8(imm_tmp)) );
	    setFlags_DEP1_DEP2( ARMG_CC_OP_LSL, Rm_tmp, imm_tmp );
	    return mkexpr(res);
	}
    }




/*
  Q. How does the flag association work?

  Q. Even when not actually performing an LSL, flags still get changed, so need to register an LSL just so we can associate the flags with that instruction... ???
*/


    // carry dep1 = Rs, dep2 = Rm


//	    setFlags_DEP1_DEP2( op, dep1, dep2 )
//   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(thunkOp) ) );
//    putIReg(r0, getIReg(r1));
//    DIP("mov %i,%i\n", r0, r1);

    
    return 0;  /*notreached*/
}



/* Returns shifted result to a temp */
static
IRExpr* dis_shift ( UInt theInstr )
{
    UChar shift_op = (theInstr >> 5) & 0xF;  // second byte

    vex_printf("dis_shift\n");

    // CAB TODO: Check what can do with R15... strict limits apply (ARM A5-9)
   
    // We shouldn't have any 'op' with bits 4=1 and 7=1 : 1xx1
    switch (shift_op) {
    case 0x0:
    case 0x8:
    case 0x1: return dis_shift_lsl(theInstr);
/*
    case 0x2:
    case 0xA:
    case 0x3: return dis_shift_lsr(theInstr);

    case 0x4:
    case 0xC:
    case 0x5: return dis_shift_asr(theInstr);

    case 0x6:
    case 0xE:
    case 0x7: return dis_shift_ror(theInstr);  // Also RRX
*/
    default:
	// Error: Any other value shouldn't be here.
	vpanic("dis_shift(ARM)");
	return mkexpr(0); 
    }
}





/* -------------- Helper for MOV. -------------- */
/* MOV R2, R0            ; R2 = R0
   MOV R2, R0, LSL #2    ; shift R0 left by 2, write to R2  (R2 = R0 x 4)
   MOV R2, R4, ROR R3    ; R2 = R4 rotated right by val of R3

   <opcode>{<cond>}{S}  <Rd>, <shifter_op>
   where <shifter_op> => #imm | <Rm> | <Rm>, LSL <Rs> | ... See ARM ARM A5-2
   (<Rn> not used)
*/
/*
  if cond_passed(cond)
    Rd = shifter_op
    if S bit set
      if Rd is R15
        => Err.  This is undefined when executed in User/System mode.
      else
        N flag = Rd[31]                   (post shift if shift)
        Z flag = if Rd ==0 the 1 else 0   (post shift if shift)
        C flag = shifter_carry_out
	V flag = unaffected.

  => Flags dep on Rd, Rm, Rs!
  ... mind you, that's more than one IR instruction... lsl + move... ?
*/
static
void dis_mov_reg ( UInt theInstr )
{
    UChar S = (theInstr >> 20) & 1;
//    UChar Rn = (theInstr >> 16) & 0xF;  // Not used
    UChar Rd = (theInstr >> 12) & 0xF;
    UChar Rm = theInstr & 0xF;
    UChar Rs;
    UInt shift_imm;
    UChar reg_shft = (theInstr >> 4) & 1;
    IRTemp t1;

    vex_printf("dis_move_reg\n");
    
    putIReg(Rd, dis_shift( theInstr ));

    return;
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
   IRType    ty;
   IRTemp    addr, t1, t2;
   Int       alen;
   UChar     opc, modrm, abyte;
   ARMCondcode cond;
   UInt      d32;
   UChar     dis_buf[50];
   Int       am_sz, d_sz;
   DisResult whatNext = Dis_Continue;
   UInt      theInstr;

   /* At least this is simple on ARM: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */

   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_ARM abort if we fail the
      decode. */
   *size = 0;

   theInstr = *(UInt*)(&guest_code[delta]);

   vex_printf("START: 0x%x\n", theInstr );

   DIP("\t0x%x:  ", guest_pc_bbstart+delta);



   // TODO: fix the client-request stuff, else nothing will work

   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
      UInt* code = (UInt*)(guest_code + delta);
      /* Spot this:                                // CAB: easy way to rotate left?
         E1A00EE0                   mov  r0, r0, ror #29
	 E1A001E0                   mov  r0, r0, ror #3
	 E1A00DE0                   mov  r0, r0, ror #27
	 E1A002E0                   mov  r0, r0, ror #5
	 E1A006E0                   mov  r0, r0, ror #13
	 E1A009E0                   mov  r0, r0, ror #19
      */
      /* I suspect these will have to be turned the other way round to
	 work on little-endian arm. */
      if (code[0] == 0xE1A00EE0 &&
          code[1] == 0xE1A001E0 &&
          code[2] == 0xE1A00DE0 &&
          code[3] == 0xE1A002E0 &&
          code[4] == 0xE1A006E0 &&
	  code[5] == 0xE1A009E0) {

         // uh ... I'll figure this out later.  possibly r0 = client_request(r0) */
         DIP("?CAB? = client_request ( ?CAB? )\n");

	 *size = 24;

	 irbb->next     = mkU32(guest_pc_bbstart+delta);
	 irbb->jumpkind = Ijk_ClientReq;

         whatNext = Dis_StopHere;
         goto decode_success;
      }
   }




   /*
     Deal with condition first
    */
   cond = (theInstr >> 28) & 0xF;    /* opcode: bits 31:28 */
   vex_printf("\ndisInstr(arm): cond: 0x%x\n", cond );

   switch (cond) {
   case 0xF:   // => Illegal instruction prior to v5 (see ARM ARM A3-5)
       vex_printf("disInstr(arm): illegal condition\n");
       goto decode_failure;

   case 0xE:   // => Unconditional: go translate the instruction
       break;

   default:    // => Valid condition: translate the condition test first
       stmt( IRStmt_Exit( mk_armg_calculate_condition(cond),
			  Ijk_Boring,
			  IRConst_U32(guest_pc_bbstart+delta+4) ) );
//       irbb->next     = mkU32(guest_pc_bbstart+delta+4);
//       irbb->jumpkind = Ijk_Boring;
   }
   




   /*
     Deal with multiplies, load/store instructions
     ARM ARM A3-3
     ...
    */

   
   /*
     Deal with 'misc instructions'
     ARM ARM A3-4
     ...
    */





   /* As per ARM ARM v2 page A3-2, primary opcode appears to be in
      bits 27:21 of the instruction (roughly).  Hence ... */
   opc = (theInstr >> 21) & 0x7F;    /* opcode: bits 27:21 */
   vex_printf("disInstr(arm): opcode: 0x%x\n", opc );

   switch (opc) {

       /* DPI: xxxx 000a aaaS nnnn dddd ...
               cond [opcode ] op1  dest op2
	  24:21 => opcode
	  20    => (S)et flag (sets NZCV flags of CPSR: opcode dependant)
	  19:16 => Rn
	  15:12 => Rd
       */
       /* DPI, Register: operand2 => cccc cttt mmmm
	  11:7  => Rc (11:6, 7=0), or #c
	  6:4   => shift instr
	  3:0   => Rm
       */
       /* DPI, Immediate: operand2 => rrrr bbbb bbbb
	  19:16 => Rn
	  15:12 => Rd
	  11:8  => Rr
	  7:0   => Rb
       */

   case 0x00:            // AND   Boolean And           Rd = Rn AND Op2
       goto decode_failure;
   case 0x01:            // EOR   Boolean Eor           Rd = Rn EOR Op2
       goto decode_failure;

   case 0x02:            // SUB   Subtract              Rd = Rn  -  Op2
       /*      e24cb004        sub     fp, ip, #4      ; 0x4
	       1110 0010 0100 1100 1011 0000 0000 0100
	*/
       vex_printf("OPCODE: SUB\n");
       goto decode_failure;

   case 0x03:            // RSB   Reverse Subtract      Rd = Op2 -  Rn
       goto decode_failure;
   case 0x04:            // ADD   Addition              Rd = Rn  +  Op2
       goto decode_failure;
   case 0x05:            // ADC   Add with Carry        Rd = Rn  +  Op2 + C
       goto decode_failure;
   case 0x06:            // SBC   Subtract with carry   Rd = Rn  -  Op2 - (1-C)
       goto decode_failure;
   case 0x07:            // RSC   Reverse sub w/carry   Rd = Op2 -  Rn  - (1-C)
       goto decode_failure;
   case 0x08:            // TST   Test bit              Rn AND Op2
       goto decode_failure;
   case 0x09:            // TEQ   Test equality         Rn EOR Op2
       goto decode_failure;
   case 0x0A:            // CMP   Compare               Rn  -  Op2
       goto decode_failure;
   case 0x0B:            // CMN   Compare Negative      Rn  + Op2
       goto decode_failure;
   case 0x0C:            // ORR   Boolean Or            Rd = Rn OR  Op2
       goto decode_failure;



   case 0x0D:            // MOV (reg)
       vex_printf("OPCODE: MOV(reg)\n");
       dis_mov_reg(theInstr);
       break;


   case 0x0E:            // BIC   Bit clear             Rd = Rn AND NOT Op2
       goto decode_failure;
   case 0x0F:            // MVN   Move Not              Rd =    NOT Op2
       goto decode_failure;



   case 0x1D:            // MOV (imm)
       /*      e3a00014        mov     r0, #20 ; 0x14
	       1110 0011 1010 0000 0000 0000 0001 0100
	*/
       vex_printf("OPCODE: MOV(imm)\n");
       goto decode_failure;







   case 0x49:     // STMDB: STM(1), decrement before
	/* e92dd810        stmdb   sp!, {r4, fp, ip, lr, pc}
	   1110 1001 0010 1101 1101 1000 0001 0000
	   1110 100P U0W0 Rn   reg list
	   P=1 => Rn included in range of mem
	   U=0 => Rn lies at top of mem range
	   W=1 => Base reg updated after transfer (U=1 => incremented 4x num regs)
	   opc => 0100 1001 => 49
	 */
       vex_printf("OPCODE: STM-DB\n");
       goto decode_failure;


       
   case 0x58:
   case 0x59:
   case 0x5A:
   case 0x5B:
   case 0x5D:
   case 0x5E:
   case 0x5F:
       /* BL
	  ebfffffe        bl      0 <newHHW>
	  1110 1011 1111 1111 1111 1111 1111 1110
	  cond 101L signed_immediate_24
	  L=1 => return address stored in link register (R14)
	  opcode => 101 1xxx => 0x58 to 0x5F
       */
       vex_printf("OPCODE: BL\n");
       goto decode_failure;
       

   default:
   decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(arm): unhandled instruction: "
              "0x%x\n", theInstr);
   vpanic("armToIR: unimplemented insn");

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   vex_printf("disInstr(arm): success");
   DIP("\n");

   *size = 4;
   return whatNext;
}

#undef DIP
#undef DIS

/*--------------------------------------------------------------------*/
/*--- end                                         guest-arm/toIR.c ---*/
/*--------------------------------------------------------------------*/
