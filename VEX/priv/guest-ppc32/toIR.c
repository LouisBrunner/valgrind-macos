
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

#define OFFB_RESULT    offsetof(VexGuestPPC32State,guest_Result)


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

#if 0
/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleType(ty));
   return newIRTemp( irbb->tyenv, ty );
}
#endif

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

static UInt extend_s_16to32 ( UInt x )
{
   return (UInt)((((Int)x) << 16) >> 16);
}
#endif

#if 0
static UInt extend_s_24to32 ( UInt x )
{
   return (UInt)((((Int)x) << 8) >> 8);
}
#endif

#if 0
/* Fetch a byte from the guest insn stream. */
static UChar getIByte ( UInt delta )
{
   return guest_code[delta];
}
#endif

/* Get a 8/16/32-bit unsigned value out of the insn stream. */

#if 0
static UInt getUChar ( UInt delta )
{
   UInt v = guest_code[delta+0];
   return v & 0xFF;
}
#endif

#if 0
static UInt getUDisp16 ( UInt delta )
{
   UInt v = guest_code[delta+1]; v <<= 8;
   v |= guest_code[delta+0];
   return v & 0xFFFF;
}
#endif

#if 0
static UInt getUDisp32 ( UInt delta )
{
   UInt v = guest_code[delta+3]; v <<= 8;
   v |= guest_code[delta+2]; v <<= 8;
   v |= guest_code[delta+1]; v <<= 8;
   v |= guest_code[delta+0];
   return v;
}
#endif

#if 0
static UInt getUDisp ( Int size, UInt delta )
{
   switch (size) {
      case 4: return getUDisp32(delta);
      case 2: return getUDisp16(delta);
      case 1: return getUChar(delta);
      default: vpanic("getUDisp(PPC32)");
   }
   return 0; /*notreached*/
}
#endif

#if 0
/* Get a byte value out of the insn stream and sign-extend to 32
   bits. */
static UInt getSDisp8 ( UInt delta )
{
   return extend_s_8to32( (UInt) (guest_code[delta]) );
}
#endif

#if 0
static UInt getSDisp16 ( UInt delta0 )
{
   UChar* eip = (UChar*)(&guest_code[delta0]);
   UInt d = *eip++;
   d |= ((*eip++) << 8);
   return extend_s_16to32(d);
}
#endif

#if 0
static UInt getSDisp ( Int size, UInt delta )
{
   switch (size) {
      case 4: return getUDisp32(delta);
      case 2: return getSDisp16(delta);
      case 1: return getSDisp8(delta);
      default: vpanic("getSDisp(PPC32)");
  }
  return 0; /*notreached*/
}
#endif


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

#if 0
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
#endif

#if 0
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
#endif

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
static IRExpr* loadLE ( IRType ty, IRExpr* data )
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

#if 0
static IRExpr* mk_ppc32g_calculate_flags_cr0 ( void )
{
   IRExpr** args
      = mkIRExprVec_1( IRExpr_Get(OFFB_RESULT,  Ity_I32) );

   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regpppc32*/, 
           "ppc32g_calculate_flags_all", &ppc32g_calculate_flags_all,
           args
        );

   /* Exclude OP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = 1;
   return call;
}
#endif







#if 0

/* -------------- Building the flags-thunk. -------------- */

/* The machinery in this section builds the flag-thunk following a
   flag-setting operation.  Hence the various setFlags_* functions.
*/

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


/* Set the flags thunk OP, DEP1 and DEP2 fields.  The supplied op is
   auto-sized up to the real op. */

static 
void setFlags_DEP1_DEP2 ( IRTemp result )
{
   stmt( IRStmt_Put( OFFB_RESULT, mkU32(result)) );
}

#endif








/* -------------- Condition codes. -------------- */

#if 0
/* Condition codes, using the PPC32 encoding.  */
static HChar* name_PPC32Condcode ( PPC32Condcode cond )
{
   switch (cond) {
       default: vpanic("name_PPC32Condcode");
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
  //   IRType    ty;
  //  IRTemp    addr, t1, t2;
  //   Int       alen;
   UChar opc;
//   PPC32Condcode cond;
   //  UInt      d32;
   // UChar     dis_buf[50];
   // Int       am_sz, d_sz;
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
	 work on little-endian ppc32. */
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



   

   opc = 0;//(theInstr >> 20) & 0xFF;    /* opcode1: bits 27:20 */
//   vex_printf("disInstr(ppc32): opcode: 0x%2x, %,09b\n", opc, opc );



   switch (opc) {
   case 0:
       goto decode_failure;

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
