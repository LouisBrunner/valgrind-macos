
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

static UInt extend_s_24to32 ( UInt x )
{
   return (UInt)((((Int)x) << 8) >> 8);
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
/*--- Helpers for %flags.                                 ---*/
/*------------------------------------------------------------*/

/* -------------- Evaluating the flags-thunk. -------------- */

/* Build IR to calculate all the flags from stored
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


/* Build IR to calculate just the carry flag from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression :: Ity_I32. */
static IRExpr* mk_armg_calculate_flags_c ( void )
{
   IRExpr** args
      = mkIRExprVec_3( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_flags_c", &armg_calculate_flags_c,
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






/*
  Addressing mode 4 - LOAD/STORE multiple, LDM|STM
  ARM ARM A5-48
*/
static
Bool dis_loadstore_mult ( theInstr )
{
    UChar flags   = (theInstr >> 20) & 0x1F;   // theInstr[24:20]
    UChar Rn_addr = (theInstr >> 16) & 0xF;
    IRTemp Rn = newTemp(Ity_I32);
    IRTemp Rn_orig = newTemp(Ity_I32);
    UInt reg_list = theInstr & 0xFFFF;  // each bit addresses a register: R0 to R15
    UChar L  = (flags >> 0) & 1;  // Load(1) | Store(0)
    UChar W  = (flags >> 1) & 1;  // (W)riteback Rn (incr(U=1) | decr(U=0) by n_bytes)
//  UChar S  = (flags >> 2) & 1;  // Priviledged mode flag - *** CAB TODO ***
    UChar U  = (flags >> 3) & 1;  // Txfr ctl: Direction = upwards(1) | downwards(0)
    UChar PU = (flags >> 3) & 3;  // Txfr ctl: Rn within(P=1) | outside(P=0) accessed mem

    IRTemp start_addr = newTemp(Ity_I32);
    IRTemp end_addr   = newTemp(Ity_I32);
    IRTemp data=0;

    UInt n_bytes=0;
    UInt tmp_reg = reg_list;
    UInt reg_idx, offset;
    Bool decode_ok = True;

    while (tmp_reg > 0) {     // Count num bits in reg_list => num_bytes
	if (tmp_reg & 1) { n_bytes += 4; }
	tmp_reg = tmp_reg >> 1;
    }

    assign( Rn, getIReg(Rn_addr) );
    assign( Rn_orig, mkexpr(Rn) );

    switch (PU) {  // <addressing_mode>
    case 0x0:  // Decrement after  (DA)
	assign( start_addr, binop( Iop_Add32, mkexpr(Rn), mkU32(n_bytes + 4) ) );
	assign( end_addr,   mkexpr(Rn) );
	break;

    case 0x1:  // Increment after  (IA)
	assign( start_addr, mkexpr(Rn) );
	assign( end_addr,   binop( Iop_Add32, mkexpr(Rn), mkU32(n_bytes - 4) ) );
	break;

    case 0x2:  // Decrement before (DB)
	assign( start_addr, binop( Iop_Sub32, mkexpr(Rn), mkU32(n_bytes) ) );
	assign( end_addr,   binop( Iop_Sub32, mkexpr(Rn), mkU32(4) ) );
	break;

    case 0x3:  // Increment before (IB)
	assign( start_addr, binop( Iop_Add32, mkexpr(Rn), mkU32(4) ) );
	assign( end_addr,   binop( Iop_Add32, mkexpr(Rn), mkU32(n_bytes) ) );
	break;

    default:
	vex_printf("dis_loadstore_mult(ARM): No such case: 0x%x", PU);
	return False; 
    }

    if (W==1) {
	if (U==1) { // upwards
	    putIReg( Rn_addr, binop( Iop_Add32, mkexpr(Rn), mkU32(n_bytes) ) );
	} else { // downwards
	    putIReg( Rn_addr, binop( Iop_Sub32, mkexpr(Rn), mkU32(n_bytes) ) );
	}
    }


    /*
      Loop through register list, LOAD/STORE indicated registers

      lowest numbered reg -> lowest address
       -> so start with lowest register...
      reg_idx gives the guest register address
      offset gives current mem offset from start_addr
    */
    offset=0;
    for (reg_idx=0; reg_idx < 16; reg_idx++) {
	if (( reg_list >> reg_idx ) & 1) {  // reg_list[i] == 1?

	    if (L==1) { // LOAD Ri, (start_addr + offset)

		if (Rn_addr == reg_idx && W==1) { // Undefined - ARM ARM A4-31
		    decode_ok=False;
		    break;
		}

		assign( data, loadLE(Ity_I32, binop(Iop_Add32,
						    mkexpr(start_addr), mkU32(offset))) );
		if (reg_idx == 15) {
		    // assuming architecture < 5: See ARM ARM A4-31
		    putIReg( reg_idx, binop(Iop_And32, mkexpr(data), mkU32(0xFFFFFFFC)) );
		} else {
		    putIReg( reg_idx, mkexpr(data) );
		}
	    } else {    // STORE Ri, (start_addr + offset)

		// ARM ARM A4-85 (Operand restrictions)
		if (reg_idx == Rn_addr && W==1) {  // Rn in reg_list && writeback
		    if (offset != 0) {  // Undefined - See ARM ARM A4-85
			decode_ok=False;
			break;
		    }
		    // is lowest reg in reg_list: store Rn_orig
		    storeLE( mkexpr(start_addr), mkexpr(Rn_orig) );
		} else {
		    storeLE( binop(Iop_Add32, mkexpr(start_addr), mkU32(offset) ),
			     getIReg(reg_idx) );
		}
	    }
	    offset += 4;
	}
    }
    // CAB TODO:
    // IR assert( end_addr == (start_addr + offset) - 8 )

    return decode_ok;
}




/*
  Addressing mode 2 - LOAD/STORE word or unsigned byte
  ARM ARM A5-18
*/
static
Bool dis_loadstore_w_ub ( theInstr )
{
    UChar is_reg   = (theInstr >> 25) & 0x1;   // immediate | register offset/index
    UInt flags     = (theInstr >> 20) & 0x3F;  // theInstr[25:20]
    UChar Rn_addr  = (theInstr >> 16) & 0xF;
    UChar Rd_addr  = (theInstr >> 12) & 0xF;
    UChar Rm_addr  = (theInstr >> 00) & 0xF;
    UChar shift_op = (theInstr >> 04) & 0xFF;
    UInt offset_12 = (theInstr >> 00) & 0xFFF;
    IRTemp Rn = newTemp(Ity_I32);
    IRTemp Rm = newTemp(Ity_I32);
    UChar shift_imm, shift;

    UChar L  = (flags >> 0) & 1;   // Load(1) | Store(0)
    UChar W  = (flags >> 1) & 1;   // P==0: mem access = normal(W==0) | unprivileged(W==1)
                                   // P==1: Rn !updated(W==0) | updated(W==1)
    UChar B  = (flags >> 2) & 1;   // access = unsigned byte(1) | word(0)
    UChar U  = (flags >> 3) & 1;   // offset is added(1)|subtracted(0) from the base
    UChar P  = (flags >> 4) & 1;   // P==0: post-indexed addressing
                                   // P==1: W==0: offset addressing: Rn not updated
                                   //       W==1: pre-indexed addressing: addr -> Rn
    IRTemp addr = newTemp(Ity_I32);
    IRTemp indx = newTemp(Ity_I32);

    IRTemp tmp = newTemp(Ity_I32);
    IRTemp tmp1 = newTemp(Ity_I32);
    IRTemp tmp2 = newTemp(Ity_I32);
    IRTemp tmp3 = newTemp(Ity_I32);
    IRTemp tmp4 = newTemp(Ity_I32);
    IRExpr* expr;

    IRTemp oldFlagC = newTemp(Ity_I32);

    vassert(((theInstr >> 26) & 0x3) == 0x1);

    assign( Rn, getIReg(Rn_addr) );

    if (Rn_addr == 15) {
	if (P==1 && W==0) { // offset addressing: Rn not updated
	    // CAB TODO
	    // CAB: This right?
//	    assign( Rn, binop(Iop_And32, mkexpr(Rn), mkU32(8)) );
	} else { // Unpredictable: ARM ARM A5-29
	    return False;
	}
    }

    /*
      Post-indexed: Set addr to Rn
    */
    if (P==0) {
	assign( addr, mkexpr(Rn) );
    }

    /*
      Retrieve address to load/store
    */
    if (is_reg) {
	if (Rm_addr == 15 || Rm_addr == Rn_addr) { // Unpredictable: ARM ARM A5-27
	    return False;
	}

	assign( Rm, getIReg(Rm_addr) );

	if (shift_op == 0) {
	    assign( tmp, mkexpr(Rm) );
	} else {
	    shift_imm = (shift_op >> 3) & 0x1F;
	    shift = (shift_op >> 1) & 0x3;

	    switch (shift) {
	    case 0x0: // LSL
		assign( indx, binop(Iop_Shl32, mkexpr(Rm), mkU8(shift_imm)) );
		break;

	    case 0x1: // LSR
		if (shift_imm) {
		    assign( indx, binop(Iop_Shr32, mkexpr(Rm), mkU8(shift_imm)) );
		} else {
		    assign( indx, mkU32(0) );
		}
		break;

	    case 0x2: // ASR
		if (shift_imm) {
		    assign( indx, binop(Iop_Sar32, mkexpr(Rm), mkU32(shift_imm)) );
		} else {
		    assign( indx,     // Rm[31] ? 0xFFFFFFFF : 0x0
			    IRExpr_Mux0X( binop(Iop_And32, mkexpr(Rm), mkU32(0x8FFFFFFF)),
					  mkexpr(0x0), mkexpr(0xFFFFFFFF) ) );
		}
		break;

	    case 0x3: // ROR|RRX

		// CAB: These right?

		assign(oldFlagC, mk_armg_calculate_flags_c());
	
		if (shift_imm == 0) { // RRX (ARM ARM A5-17)
		    // 33 bit ROR using carry flag as the 33rd bit
		    // op = Rm >> 1, carry flag replacing vacated bit position.  
		    // indx = (c_flag lsl 31) OR (Rm LSR 1)
		    assign( tmp, mkexpr(oldFlagC) );
		    assign( indx, binop( Iop_Or32,
					 binop( Iop_Shl32, mkexpr(tmp), mkU32(31) ),
					 binop( Iop_Shr32, mkexpr(Rm),  mkU8(1)  ) ) );

		} else { // ROR
		    // indx = Rm ROR shift_imm
		    //      = (Rm >> shift_imm) | (Rm << (32-shift_imm))
		    assign( tmp, binop(Iop_Sub8, mkU8(32), mkU32(shift_imm)) );
		    assign( indx, binop( Iop_Or32,
					 binop( Iop_Shr32, mkexpr(Rm), mkU8(shift_imm) ),
					 binop( Iop_Shl32, mkexpr(Rm), mkexpr(tmp) ) ) );
		}
		break;

	    default: break;
	    }
	    assign( tmp, mkexpr(indx) );
	}
    } else { // immediate offset/index
	assign( tmp, mkU32(offset_12) );
    }
			
    /*
      Depending on P,U,W, set addr and write to Rn
    */
    if (P==1) {
	if (U == 1) { // increment
	    assign( addr, binop( Iop_Add32, mkexpr(Rn), mkexpr(tmp) ) );
	} else {   // decrement
	    assign( addr, binop( Iop_Sub32, mkexpr(Rn), mkexpr(tmp) ) );
	}
	if (W == 1) { // pre-indexed addressing
	    putIReg( Rn_addr, mkexpr(addr) );
	}
    } else {          // post-indexed addressing
	assign( addr, mkexpr(Rn) );
	if (U == 1) { // increment
	    putIReg( Rn_addr, binop( Iop_Add32, mkexpr(Rn), mkexpr(tmp) ) );
	} else {   // decrement
	    putIReg( Rn_addr, binop( Iop_Sub32, mkexpr(Rn), mkexpr(tmp) ) );
	}
    }



    /*
      LOAD/STORE Rd, address
    */
    if (L==1) { // LOAD
	if (B==1) {  // unsigned byte (LDRB): ARM ARM A4-40
	    putIReg( Rd_addr, loadLE( Ity_I8, mkexpr( addr ) ) );
	}
	else {       // word (LDR): ARM ARM A4-38
	    expr = binop(Iop_And32, mkexpr(addr), mkU32(0x3));

	    /* LOAD memory data (4 bytes) */
	    assign( tmp1, loadLE( Ity_I32, mkexpr( addr ) ) );

	    // data ROR 8
	    assign( tmp2, binop(Iop_Sub8, mkU8(32), mkU32(8)) ); 
	    assign( tmp2, binop( Iop_Or32,
				 binop( Iop_Shr32, mkexpr(tmp1), mkU8(8) ),
				 binop( Iop_Shl32, mkexpr(tmp1), mkexpr(tmp2) ) ) );
	    // data ROR 16
	    assign( tmp3, binop(Iop_Sub8, mkU8(32), mkU32(16)) );
	    assign( tmp3, binop( Iop_Or32,
				 binop( Iop_Shr32, mkexpr(tmp1), mkU8(16) ),
				 binop( Iop_Shl32, mkexpr(tmp1), mkexpr(tmp3) ) ) );
	    
	    // data ROR 24
	    assign( tmp4, binop(Iop_Sub8, mkU8(32), mkU32(24)) );
	    assign( tmp4, binop( Iop_Or32,
				 binop( Iop_Shr32, mkexpr(tmp1), mkU8(24) ),
				 binop( Iop_Shl32, mkexpr(tmp1), mkexpr(tmp4) ) ) );

	    /* switch (addr[1:0]) {
	       0x0:addr;
	       0x1:addr ROR 8;
	       0x2:addr ROR 16;
	       0x3:addr ROR 24  } */
	    assign( tmp, IRExpr_Mux0X(
			binop(Iop_CmpEQ32, expr, mkU32(0x0)),
			IRExpr_Mux0X(
			    binop(Iop_CmpEQ32, expr, mkU32(0x1)),
			    IRExpr_Mux0X(
				binop(Iop_CmpEQ32, expr, mkU32(0x2)),
				mkexpr(tmp4),
				mkexpr(tmp3) ),
			    mkexpr(tmp2) ),
			mkexpr(tmp1) ) );
	    
				  
	    if ( Rd_addr == 15 && !(P == 0 && W==1)) {  // R15 && not unprivileged...
		// CAB: TODO
		// assuming architecture < 5: See ARM ARM A4-28
//		putIReg( Rd_addr, binop(Iop_And32, mkexpr(tmp), mkU32(0xFFFFFFFC)) );
	    } else {
//		putIReg( Rd_addr, mkexpr(tmp) );
	    }

	}
    } else { // STORE: ARM ARM A4-88
	// CAB: these right?

	if (B==1) {  // unsigned byte
            storeLE( mkexpr(addr), unop(Iop_32to8, getIReg(Rd_addr)) );   // Rd[7:0]
	} else {     // word
	    storeLE( mkexpr(addr), getIReg(Rd_addr) );
	}
    }
    return True;
}






/*
  ARMG_CC_OP_LSL, ARMG_CC_OP_LSR, ARMG_CC_OP_ASR
  ARM ARM A5-9...

  carry = carry_out[0]
*/
static
IRExpr* dis_shift( Bool* decode_ok, UInt theInstr, IRTemp* carry_out )
{
    UChar Rn_addr  = (theInstr >> 16) & 0xF;
    UChar Rd_addr  = (theInstr >> 12) & 0xF;
    UChar Rs_addr  = (theInstr >> 8) & 0xF;
    UChar Rm_addr  = (theInstr >> 0) & 0xF;
    UChar by_reg   = (theInstr >> 4) & 0x1; // instr[4]
    UChar shift_imm = (theInstr >> 7) & 0x1F; // instr[11:7]
    UChar shift_op  = (theInstr >> 4) & 0xF;  // instr[7:4]
    IRTemp Rm          = newTemp(Ity_I32);
    IRTemp Rs          = newTemp(Ity_I32);
    IRTemp shift_amt   = newTemp(Ity_I8);
    IRTemp carry_shift = newTemp(Ity_I8);
    IRTemp oldFlagC    = newTemp(Ity_I32);
    IRTemp mux_false   = newTemp(Ity_I32);
    IRExpr* expr;
    IROp op;

    assign( Rm, getIReg(Rm_addr) );
    assign(oldFlagC, mk_armg_calculate_flags_c());
	
    switch (shift_op) {
    case 0x0: case 0x8: case 0x1: op = Iop_Shl32; break;
    case 0x2: case 0xA: case 0x3: op = Iop_Shr32; break;
    case 0x4: case 0xC: case 0x5: op = Iop_Sar32; break;
    default:
	vex_printf("dis_shift(arm): No such case: 0x%x\n", shift_op);
	*decode_ok = False;
	return mkU32(0);
    }


    if (by_reg) {  // Register Shift
	vex_printf("shift: reg\n");

	if (Rd_addr == 15 || Rm_addr == 15 || Rn_addr == 15 || Rs_addr == 15) {
	    // Unpredictable (ARM ARM A5-10)
	    vex_printf("dis_shift(arm): Unpredictable: R15 used in instr\n");
	    *decode_ok = False;
	    return mkU32(0);
	}

	assign( Rs, getIReg((theInstr >> 8) & 0xF) );

	// shift_amt = shift_expr & 31   => Rs[5:0]
	assign( shift_amt,
		narrowTo(Ity_I8, binop( Iop_And32, mkexpr(Rs), mkU32(0x1F)) ) );
	
	// CAB TODO: support for >31 shift ?  (Rs[7:0])

	switch (shift_op) {
	    case 0x1: // LSL(reg)
		assign( mux_false, mkU32(0) );
		assign( carry_shift, binop(Iop_Add8, mkU8(32), mkexpr(shift_amt)) );
		break;
	    case 0x3: // LSR(reg)
		assign( mux_false, mkU32(0) );
		assign( carry_shift, binop(Iop_Sub8, mkexpr(shift_amt), mkU8(1)) );
		break;
	    case 0x5: // ASR(reg)
		// Rs[31] == 0 ? 0x0 : 0xFFFFFFFF
		assign( mux_false,
			IRExpr_Mux0X(
			    binop(Iop_CmpLT32U, mkexpr(Rs), mkU32(0x80000000)),
			    mkU32(0xFFFFFFFF), mkU32(0) ) );
		assign( carry_shift,
			binop(Iop_Sub8, mkexpr(shift_amt), mkU8(1)) );
		break;
	    default:
		vex_printf("dis_shift(arm): Reg shift: No such case: 0x%x\n", shift_op);
		*decode_ok = False;
		return mkU32(0);
	}

	expr = IRExpr_Mux0X( 
	    binop(Iop_CmpLT32U, widenUto32(mkexpr(shift_amt)), mkU32(32)),
	    mkexpr(mux_false),
	    binop(op, mkexpr(Rm), mkexpr(shift_amt)) );

	// shift_amt == 0 ? old_flag_c : Rm >> x
	assign( *carry_out,
		IRExpr_Mux0X(
		    binop(Iop_CmpEQ8, mkexpr(shift_amt), mkU8(0)),
		    binop(Iop_Shr32, mkexpr(Rm), mkexpr(carry_shift)),
		    mkexpr(oldFlagC) ) );
    }
    else {  // Immediate shift
	vex_printf("shift: imm\n");
	
	// CAB: This right? Seems kinda strange... (ARM ARM A5-9)
	if (Rm_addr == 15 || Rn_addr == 15) {
	    assign( Rm, binop(Iop_Add32, getIReg(15), mkU32(8)) );
	}

	if (shift_imm == 0) {
	    switch (shift_op) {
		case 0x0: case 0x8: // LSL(imm)
		    expr = mkexpr(Rm);
		    assign( *carry_out, mkexpr(oldFlagC) );
//		    assign( *carry_out, binop(Iop_Shr32, mkexpr(oldFlagC),
//		                              mkU32(ARMG_CC_SHIFT_C)) );
		    break;
		case 0x2: case 0xA: // LSR(imm)
		    expr = mkexpr(0);
		    // Rm >> 31: carry = R[0]
		    assign( *carry_out, binop(Iop_Shr32, mkexpr(Rm), mkU8(31)) );
		    break;
		case 0x4: case 0xC: // ASR(imm)
		    // Rs[31] == 0 ? 0x0 : 0xFFFFFFFF
		    expr = IRExpr_Mux0X(
			binop(Iop_CmpLT32U, mkexpr(Rs), mkU32(0x80000000)),
			mkU32(0xFFFFFFFF), mkU32(0) );
		    // Rm >> 31: carry = R[0]
		    assign( *carry_out, binop(Iop_Shr32, mkexpr(Rm), mkU8(31)) );
		    break;
		default:
		    vex_printf("dis_shift(arm): Imm shift: No such case: 0x%x\n", shift_op);
		    *decode_ok = False;
		    return mkU32(0);
	    }
	} else {
	    expr = binop(op, mkexpr(Rm), mkU8(shift_imm));
	    assign( *carry_out, binop(op, mkexpr(Rm),
				      binop(Iop_Sub32, mkU32(shift_imm), mkU32(1)) ) );
	}
    }
    return expr;
}




/*
  ARMG_CC_OP_ROR
  ARM ARM A5-15,16,17
*/
static
IRExpr* dis_rotate ( Bool* decode_ok, UInt theInstr, IRTemp* carry_out )
{
    UChar Rn_addr  = (theInstr >> 16) & 0xF;
    UChar Rd_addr  = (theInstr >> 12) & 0xF;
    UChar Rs_addr  = (theInstr >> 8) & 0xF;
    UChar Rm_addr  = (theInstr >> 0) & 0xF;
    UChar by_reg   = (theInstr >> 4) & 0x1;  // instr[4]
    UInt rot_imm   = (theInstr >> 7) & 0x1F; // instr[11:7]
    IRTemp Rm       = newTemp(Ity_I32);
    IRTemp Rs       = newTemp(Ity_I32);
    IRTemp rot_amt  = newTemp(Ity_I8);      // Rs[7:0]
    IRTemp tmp_8    = newTemp(Ity_I8);
    IRTemp tmp_32   = newTemp(Ity_I32);
    IRTemp oldFlagC = newTemp(Ity_I32);
    IRExpr* expr=0;

    assign( Rm, getIReg(Rm_addr) );
    assign(oldFlagC, mk_armg_calculate_flags_c());

    if (by_reg) {  // Register rotate
	vex_printf("rotate: reg\n");

	if (Rd_addr == 15 || Rm_addr == 15 || Rn_addr == 15 || Rs_addr == 15) {
	    // Unpredictable (ARM ARM A5-10)
	    *decode_ok = False;
	    return mkU32(0);
	}

	assign( Rs, getIReg((theInstr >> 8) & 0xF) );  // instr[11:8]
	// Rs[4:0]
	assign( rot_amt, narrowTo(Ity_I8,
				  binop(Iop_And32, mkexpr(Rs), mkU32(0x1F))) );

	// CAB: This right?

	// Rs[7:0] == 0 ? oldFlagC : (Rs[4:0] == 0 ? Rm >> 31 : Rm >> rot-1 )
//	assign( tmp_32, binop(Iop_Shr32, mkexpr(oldFlagC), mkU32(ARMG_CC_SHIFT_C)) );
	assign( tmp_32, mkexpr(oldFlagC) );
	assign( *carry_out,
		IRExpr_Mux0X(
		    binop(Iop_CmpNE32, mkU32(0),
			  binop(Iop_And32, mkexpr(Rs), mkU32(0xFF))),
		    mkexpr(tmp_32),
		    IRExpr_Mux0X(
			binop(Iop_CmpEQ8, mkexpr(rot_amt), mkU8(0)),
			binop(Iop_Shr32, mkexpr(Rm),
			      binop(Iop_Sub8, mkexpr(rot_amt), mkU8(1))),
			binop(Iop_Shr32, mkexpr(Rm),
			      binop(Iop_Shr32, mkexpr(Rm), mkU8(31))) ) ) );
	

	/* expr = (dst0 >> rot_amt) | (dst0 << (wordsize-rot_amt)) */
	assign( tmp_8, binop(Iop_Sub8, mkU8(32), mkexpr(rot_amt)) );
	expr = binop( Iop_Or32,
		      binop( Iop_Shr32, mkexpr(Rm), mkexpr(rot_amt) ),
		      binop(Iop_Shl32, mkexpr(Rm), mkexpr(tmp_8)) );
    }
    else {  // Immediate rotate
	vex_printf("rotate: imm\n");

	// CAB: This right? Seems kinda strange... (ARM ARM A5-9)
	if (Rm_addr == 15 || Rn_addr == 15) {
//	    assign( Rm, binop(Iop_Add32, getIReg(15), mkU32(8)) );
	    // TODO : Can't re-assign a temp!
	}

	// Rm >> rot-1: carry = R[0]
	assign( *carry_out, binop(Iop_Shr32, mkexpr(Rm),
				  binop(Iop_Sub8, mkU8(rot_imm), mkU8(1)) ) );

	if (rot_imm == 0) { // RRX (ARM ARM A5-17)
	    // 33 bit ROR using carry flag as the 33rd bit
	    // op = Rm >> 1, carry flag replacing vacated bit position.  

	    // CAB: This right?
	    assign( tmp_32, mkexpr(oldFlagC) );
//	    assign( tmp_32, binop(Iop_Shr32, mkexpr(oldFlagC),
//				  mkU32(ARMG_CC_SHIFT_C)) );
	    expr = binop(Iop_Or32,
			 binop( Iop_Shl32, mkexpr(tmp_32), mkU8(31) ),
			 binop( Iop_Shr32, mkexpr(Rm), mkU8(1) ) );
	} else {
	    assign( tmp_8, binop(Iop_Sub8, mkU8(32), mkU8(rot_imm)) );
	    expr = binop(Iop_Or32,
			 binop( Iop_Shr32, mkexpr(Rm), mkU8(rot_imm) ),
			 binop( Iop_Shl32, mkexpr(Rm), mkexpr(tmp_8) ) );
	}
    }
    return expr;
}




/*
  CAB TODO:
   - Not all shifts by 0 leave c_flag unchanged, so guard_expr is more difficult...
  assign( flags_guard, binop( Iop_CmpEQ32, mkexpr(shift_amt), mkU32(0) ) );
  setFlags_DEP1_DEP2_shift( ARMG_CC_OP_LSL, Rm, shift_op, flags_guard );
*/




/* Addressing mode 1 - Data Processing ops
   General syntax: <opcode>{<cond>}{S} <Rd>, <Rn>, <shifter_operand>
   Returns <shifter_operand> expression
*/
static
IRExpr* dis_shifter_op ( Bool *decode_ok, UInt theInstr, IRTemp* carry_out)
{
    UChar is_immed  = (theInstr >> 25) & 1;    // immediate / register shift
    UChar shift_op = (theInstr >> 4) & 0xF;    // second byte
    UInt immed_8, rot_imm;
    UInt imm;
    IRTemp oldFlagC = newTemp(Ity_I32);

    if (is_immed) {  // ARM ARM A5-2
	vex_printf("shifter_op: imm\n");

	immed_8 = theInstr & 0xFF;
	rot_imm = (theInstr >> 8) & 0xF;
	imm = immed_8 << (rot_imm << 1);
	vex_printf("imm: %,b\n", imm);

	if (rot_imm == 0) {
	    assign(oldFlagC, mk_armg_calculate_flags_c());

	    assign( *carry_out, mkexpr(oldFlagC) );
	} else {
	    assign( *carry_out, binop(Iop_Shr32, mkU32(imm), mkU8(31)) );
	}
	return mkU32(imm);
    } else {
	vex_printf("shifter_op: shift\n");

	// We shouldn't have any 'op' with bits 4=1 and 7=1 : 1xx1
	switch (shift_op) {
	case 0x0: case 0x8: case 0x1:
	case 0x2: case 0xA: case 0x3: 
	case 0x4: case 0xC: case 0x5:
	    return dis_shift( decode_ok, theInstr, carry_out );

	case 0x6: case 0xE: case 0x7:
	    return dis_rotate( decode_ok, theInstr, carry_out );

	default: // Error: Any other value shouldn't be here.
	    *decode_ok = False;
	    vex_printf("dis_shifter_op(arm): shift: No such case: 0x%x\n", shift_op);
	    return mkU32(0);
	}
    }
}





/* -------------- Helper for DPI's. --------------
*/
static
Bool dis_dataproc ( UInt theInstr )
{
    UChar opc       = (theInstr >> 21) & 0xF;
    UChar set_flags = (theInstr >> 20) & 1;
    UChar Rn_addr   = (theInstr >> 16) & 0xF;
    UChar Rd_addr   = (theInstr >> 12) & 0xF;
    IRTemp Rn = newTemp(Ity_I32);
    IRTemp Rd = newTemp(Ity_I32);
    IRTemp shifter_op = newTemp(Ity_I32);
    IRTemp carry_out = newTemp(Ity_I32);
    IROp op = ARMG_CC_OP_LOGIC;
    Bool check_r15 = True;
    Bool decode_ok = True;

    assign( shifter_op, dis_shifter_op( &decode_ok, theInstr, &carry_out ) );
    if (!decode_ok) return False;

    assign( Rd, getIReg(Rd_addr) );
    assign( Rn, getIReg(Rn_addr) );

    switch (opc) {
    case 0x0: // AND
	vex_printf("OPCODE: AND\n");
	putIReg( Rd_addr, binop(Iop_And32, getIReg(Rn_addr), mkexpr(shifter_op)) );
	break;

    case 0x1: // EOR
	vex_printf("OPCODE: EOR\n");
	putIReg( Rd_addr, binop(Iop_Xor32, getIReg(Rn_addr), mkexpr(shifter_op)) );
	break;

    case 0x2: // SUB
	vex_printf("OPCODE: SUB\n");
	putIReg( Rd_addr, binop( Iop_Sub32, getIReg(Rn_addr), mkexpr(shifter_op) ) );
	op = ARMG_CC_OP_SUB;
	break;

    case 0x3:  // RSB
	vex_printf("OPCODE: RSB\n");
	putIReg( Rd_addr, binop( Iop_Sub32, mkexpr(shifter_op), getIReg(Rn_addr) ) );
	op = ARMG_CC_OP_SUB;
	break;

    case 0x4: // ADD
	vex_printf("OPCODE: ADD\n");
	putIReg( Rd_addr, binop( Iop_Add32, getIReg(Rn_addr), mkexpr(shifter_op) ) );
	op = ARMG_CC_OP_ADD;
	break;

    case 0x5:  // ADC - Unimplemented
    case 0x6:  // SBC - Unimplemented
    case 0x7:  // RSC - Unimplemented
	goto decode_failure;

    case 0x8: // TST
	vex_printf("OPCODE: TST\n");
	vassert(set_flags==1);
	assign( Rd, binop(Iop_And32, getIReg(Rn_addr), mkexpr(shifter_op)) );
	check_r15 = False;
	break;

    case 0x9: // TEQ
	vex_printf("OPCODE: TEQ\n");
	vassert(set_flags==1);
	assign( Rd, binop(Iop_Xor32, getIReg(Rn_addr), mkexpr(shifter_op)) );
	check_r15 = False;
	break;

    case 0xA: // CMP
	vex_printf("OPCODE: CMP\n");
	vassert(set_flags==1);
	op = ARMG_CC_OP_SUB;
	check_r15 = False;
	break;

    case 0xB: // CMN
	vex_printf("OPCODE: CMN\n");
	vassert(set_flags==1);
	op = ARMG_CC_OP_ADD;
	check_r15 = False;
	break;

    case 0xC: // ORR
	vex_printf("OPCODE: ORR\n");
	putIReg( Rd_addr, binop(Iop_Or32, getIReg(Rn_addr), mkexpr(shifter_op)) );
	break;

    case 0xD: // MOV
	vex_printf("OPCODE: MOV\n");
	putIReg( Rd_addr, mkexpr(shifter_op) );
	break;

    case 0xE: // BIC
	vex_printf("OPCODE: BIC\n");
	putIReg( Rd_addr, binop(Iop_And32, getIReg(Rn_addr),
				unop( Iop_Not32, mkexpr(shifter_op))) );
	break;

    case 0xF: // MVN
	vex_printf("OPCODE: MVN\n");
	putIReg( Rd_addr, unop(Iop_Not32, mkexpr(shifter_op)) );
	break;

    default:
    decode_failure:
	vex_printf("dis_dataproc(arm): unhandled opcode: 0x%x\n", opc);
	return False;
    }

    if (set_flags) {
	if ( check_r15 && Rd_addr == 15) { // dest reg == PC
	    // CPSR = SPSR: Unpredictable in User | System mode (no SPSR!)
	    // Unpredictable: We're only supporting user mode...
	    decode_ok = False;
	} else {
	    if (op == ARMG_CC_OP_LOGIC) {
		setFlags_DEP1_DEP2( op, Rd, carry_out );
	    } else {
		setFlags_DEP1_DEP2( op, Rn, shifter_op );
	    }
	}
    }
    return decode_ok;
}




/* -------------- Helper for Branch. --------------
*/
static
void dis_branch ( UInt theInstr )
{
    UChar link = (theInstr >> 24) & 1;
    UInt signed_immed_24 = theInstr & 0xFFFFFF;
    UInt branch_offset;
    IRTemp addr = newTemp(Ity_I32);

    if (link) { // LR (R14) = addr of instr after branch instr
	assign( addr, binop(Iop_Add32, getIReg(15), mkU32(4)) );
	putIReg( 14, mkexpr(addr) );
    }

    // PC = PC + (SignExtend(signed_immed_24) << 2)
    branch_offset = extend_s_24to32( signed_immed_24 ) << 2;
    putIReg( 15, binop(Iop_Add32, getIReg(15), mkU32(branch_offset)) );

    irbb->jumpkind = link ? Ijk_Call : Ijk_Boring;
    irbb->next     = mkU32(branch_offset);
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
  //  IRTemp    addr, t1, t2;
  //   Int       alen;
   UChar opc1, opc2, opc_tmp; //, modrm, abyte;
   ARMCondcode cond;
   //  UInt      d32;
   // UChar     dis_buf[50];
   // Int       am_sz, d_sz;
   DisResult whatNext = Dis_Continue;
   UInt      theInstr;
   Bool decode_OK = True;


   /* At least this is simple on ARM: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */

   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_ARM abort if we fail the
      decode. */
   *size = 0;

   theInstr = *(UInt*)(&guest_code[delta]);

   vex_printf("START: 0x%x, %,b\n", theInstr, theInstr );

   DIP("\t0x%x:  ", guest_pc_bbstart+delta);



   // TODO: fix the client-request stuff, else nothing will work

   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
      UInt* code = (UInt*)(guest_code + delta);

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
   vex_printf("\ndisInstr(arm): cond: 0x%x, %b\n", cond, cond );

   switch (cond) {
   case 0xF:   // => Illegal instruction prior to v5 (see ARM ARM A3-5)
       vex_printf("disInstr(arm): illegal condition\n");
       goto decode_failure;

   case 0xE:   // => Unconditional: go translate the instruction
       break;

   default:
       // => Valid condition: translate the condition test first
       stmt( IRStmt_Exit( mk_armg_calculate_condition(cond),
			  Ijk_Boring,
			  IRConst_U32(guest_pc_bbstart+delta+4) ) );
       //irbb->next     = mkU32(guest_pc_bbstart+delta+4);
       //irbb->jumpkind = Ijk_Boring;
   }
   


   /* Primary opcode is roughly bits 27:20 (ARM ARM(v2) A3-2)
      secondary opcode is bits 4:0 */
   opc1 = (theInstr >> 20) & 0xFF;    /* opcode1: bits 27:20 */
   opc2 = (theInstr >> 4 ) & 0xF;     /* opcode2: bits 7:4   */
   vex_printf("disInstr(arm): opcode1: 0x%2x, %,09b\n", opc1, opc1 );
   vex_printf("disInstr(arm): opcode2: 0x%02x, %,04b\n", opc2, opc2 );

   switch (opc1 >> 4) { // instr[27:24]
   case 0x0:
   case 0x1:
       /*
	 Multiplies, extra load/store instructions: ARM ARM A3-3
       */
       if ( (opc1 & 0xE0) == 0x0 && (opc2 & 0x9) == 0x9 ) {  // 000xxxxx && 1xx1
	   if (opc2 == 0x9) {
	       if ((opc1 & 0x1C) == 0x00) {  // multiply (accumulate)
		   goto decode_failure;
	       }
	       if ((opc1 & 0x18) == 0x08) {  // multiply (accumulate) long
		   goto decode_failure;
	       }
	       if ((opc1 & 0x1B) == 0x10) {  // swap/swap byte
		   goto decode_failure;
	       }
	   }
	   if ( opc2 == 0xB ) {
	       if ((opc1 & 0x04) == 0x00) {  // load/store 1/2word reg offset
		   goto decode_failure;
	       } else {                      // load/store 1/2word imm offset
		   goto decode_failure;	       
	       }
	   }
	   if ((opc2 & 0xD) == 0xD) {
	       if ((opc1 & 0x05) == 0x00) {  // load/store 2 words reg offset
		   goto decode_failure;
	       }
	       if ((opc1 & 0x05) == 0x04) {  // load/store 2 words imm offset
		   goto decode_failure;
	       }
	       if ((opc1 & 0x05) == 0x01) {  // load/store signed 1/2word/byte reg offset
		   goto decode_failure;
	       }
	       if ((opc1 & 0x05) == 0x05) {  // load/store signed 1/2word/byte imm offset
		   goto decode_failure;
	       }
	   }
       } /* endif: Multiplies, extra load/store... */

       /* 
	  'Misc' Instructions: ARM ARM A3-4
       */
       if ((opc1 & 0xF9) == 0x10) {  // 0001 0xx0
	   opc_tmp = (opc1 >> 1) & 0x3;
	   switch (opc2) {
	   case 0x0:
	       if ((opc_tmp & 0x1) == 0x0) { // move stat reg -> reg
		   goto decode_failure;
	       } else {                      // move reg -> stat reg
		   goto decode_failure;
	       }

	   case 0x1:
	       if (opc_tmp == 0x1) {       // branch/exchange instr set
		   goto decode_failure;
	       }
	       if (opc_tmp == 0x3) {       // count leading zeros
		   goto decode_failure;
	       }
	       break;
	       
	   case 0x3:
	       if (opc_tmp == 0x1) {       // branch & link/exchange instr set
		   goto decode_failure;
	       }
	       break;

	   case 0x5:                       // enhanced dsp add/subtracts
	       goto decode_failure;

	   case 0x7:
	       if (opc_tmp == 0x1) {       // software breakpoint
		   if (cond != 0xE) goto decode_failure; // (unpredictable ARM ARM A3-4)
		   goto decode_failure;
	       }
	       break;

	   case 0x8: case 0x9: case 0xA:   // enhanced dsp multiplies
	   case 0xB: case 0xC: case 0xD: case 0xE:
	       goto decode_failure;

	   default: break;
	   }
       } /* endif: 'Misc' Instructions... */
      // fall through...

   case 0x2:
   case 0x3:
       if ((opc1 & 0xFB) == 0x30) goto decode_failure; // Undefined - ARM ARM A3-2

       /*
	 A lonely 'MOV imm to status reg':
	*/
       if ((opc1 & 0xFB) == 0x32) { // 0011 0x10
	   goto decode_failure;
       }

       /*
	 Data Processing Instructions
	 (if we get here, it's a valid dpi)
       */
       vex_printf("OPCODE: DPI\n");
       if (!dis_dataproc( theInstr )) { goto decode_failure; }
       break;


       /*
	 Load/Store word | unsigned byte
	*/
   case 0x6: case 0x7:  // LOAD/STORE reg offset
       if ((opc2 & 0x1) == 0x1) goto decode_failure; // Undefined - ARM ARM A3-2

   case 0x4: case 0x5:  // LOAD/STORE imm offset
       if (!dis_loadstore_w_ub(theInstr)) { goto decode_failure; }
       break;

       /* 
	  Load/Store multiple 
	*/
   case 0x8: case 0x9:
       vex_printf("OPCODE: LOAD/STORE mult\n");
       if (!dis_loadstore_mult(theInstr)) { goto decode_failure; }
       break;
       

       /*
	 Branch, Branch and Link
       */
   case 0xA: case 0xB:  // B, BL
       // B(L): L=1 => return address stored in link register (R14)
       vex_printf("OPCODE: B(L)\n");
       dis_branch(theInstr);
       whatNext = Dis_StopHere;
       break;


       /*
	 Co-processor instructions
	*/
   case 0xC: case 0xD:  // co-pro load/store & double reg trxfrs
       goto decode_failure;

   case 0xE:
       if ((opc2 & 0x1) == 0x0) { // co-pro data processing
	   goto decode_failure;
       } else {                   // co-pro register transfers
	   goto decode_failure;
       }


       /*
	 Software Interrupt
	*/
   case 0xF: // swi
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
