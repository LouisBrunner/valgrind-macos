
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

/* References
   All page references, unless otherwise indicated, refer to IBM's
   "PowerPC Microprocessor Family:
    Programming Environments Manual for 64 and 32-Bit Microprocessors
    Version 2.0"
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

   vassert(subarch_guest == VexSubArchPPC32);

   /* Start a new, empty extent. */
   vge->n_used  = 1;
   vge->base[0] = guest_pc_start;
   vge->len[0]  = 0;

   /* Set up globals. */
   host_is_bigendian = host_bigendian;
   guest_code        = ppc32code;
   guest_pc_bbstart  = (Addr32)guest_pc_start;
   irbb              = emptyIRBB();

   vassert((guest_pc_start >> 32) == 0);

   /* Delta keeps track of how far along the x86code array we
      have so far gone. */
   delta             = 0;
   n_instrs          = 0;
//   *guest_bytes_read = 0;

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
      vge->len[vge->n_used-1] += size;
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

/* Do a big-endian load of a 32-bit word, regardless of the endianness
   of the underlying host. */
static UInt getUIntBigendianly ( HChar* hp )
{
   UChar* p = (UChar*)hp;
   UInt   w = 0;
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
   return w;
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
static Bool dis_int_arith ( UInt theInstr )
{
    UChar opc1    = (theInstr >> 26) & 0x3F;    /* theInstr[26:31] */
    UChar Rd_addr = (theInstr >> 21) & 0x1F;    /* theInstr[21:25] */
    UChar Ra_addr = (theInstr >> 16) & 0x1F;    /* theInstr[16:20] */

    UInt  SIMM_16 = (theInstr >>  0) & 0xFFFF;  /* theInstr[0:15]  */

    UChar Rb_addr = (theInstr >> 11) & 0x1F;    /* theInstr[11:15] */
    UChar flag_OE = (theInstr >> 10) & 1;       /* theInstr[10]    */
    UInt  opc2    = (theInstr >>  1) & 0x1FF;   /* theInstr[1:9]   */
    UChar flag_Rc = (theInstr >>  0) & 1;       /* theInstr[0]     */

    UInt EXTS_SIMM = 0;

    IRTemp Ra = newTemp(Ity_I32);
    IRTemp Rb = newTemp(Ity_I32);
    IRTemp Rd = newTemp(Ity_I32);
    IRTemp tmp = newTemp(Ity_I32);

    assign( Ra, getIReg(Ra_addr) );
    assign( Rb, getIReg(Rb_addr) );         // XO-Form: Rd, Ra, Rb
    EXTS_SIMM = extend_s_16to32(SIMM_16);   // D-Form:  Rd, Ra, EXTS(SIMM)

    switch (opc1) {

    /* D-Form */
    case 0x0C: // addi   (Add Immediate, p380)
	// li rD,val   == addi rD,0,val
	// la disp(rA) == addi rD,rA,disp
	DIP("addi %d,%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	if ( Ra_addr == 0 ) {
	    assign( Rd, mkU32(EXTS_SIMM) );
	} else {
	    assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	}
	break;

    case 0x0D: // addic  (Add Immediate Carrying, p381)
	DIP("addic %d,%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	break;
	
    case 0x0E: // addic. (Add Immediate Carrying and Record, p382)
	DIP("addic. %d,%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	setFlags_CR0_Result( Rd );
	break;

    case 0x0F: // addis  (Add Immediate Shifted, p383)
	// lis rD,val == addis rD,0,val
	DIP("addis %d,%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	if ( Ra_addr == 0 ) {
	    assign( Rd, mkU32(EXTS_SIMM << 16) );
	} else {
	    assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM << 16) ));
	}
	break;


    /* XO-Form */
    case 0x1F:
       switch (opc2) {
       case 0x10A: // add  (Add, p377)
	   DIP("add%s%s %d,%d,%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
	   if (flag_Rc)	{ setFlags_CR0_Result( Rd ); }
	   if (flag_OE) {
	       mk_ppc32g_set_xer_ov_so( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	   }
	   break;

       case 0x00A: // addc      (Add Carrying, p378)
	   DIP("addc%s%s %d,%d,%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
	   if (flag_Rc)	{ setFlags_CR0_Result( Rd ); }
	   mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	   if (flag_OE) {
	       mk_ppc32g_set_xer_ov_so( PPC32G_FLAG_OP_ADD, Rd, Ra, Rb );
	   }
	   break;

       case 0x08A: // adde      (Add Extended, p379)
	   DIP("adde%s%s %d,%d,%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = rA + rB + XER[CA]
	   assign( tmp, IRExpr_Get(OFFB_XER_CA, Ity_I32) );
	   assign( Rd, binop(Iop_Add32,
			     binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)),
			     mkexpr(tmp)) );

	   if (flag_Rc)	{ setFlags_CR0_Result( Rd ); }
	   mk_ppc32g_set_xer_ca( PPC32G_FLAG_OP_ADDE, Rd, Ra, Rb );
	   if (flag_OE) {
	       mk_ppc32g_set_xer_ov_so( PPC32G_FLAG_OP_ADDE, Rd, Ra, Rb );
	   }
	   break;

       case 0x0EA: // addme      (Add to Minus One Extended, p384)
	   DIP("addme%s%s %d,%d,%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // B=0
	   // rD = rA + XER[CA] - 1   (-1 == 0xFFFF_FFFF_FFFF_FFFF)
	   // if (Rc=1) { set guest_result }
	   // set XER[CA]
	   // if (OE=1) { XER[SO,OV] }
	   return False;

       case 0x0CA: // addze      (Add to Zero Extended, p385)
	   DIP("addze%s%s %d,%d,%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // B=0
	   // rD = rA + XER[CA]
	   // if (Rc=1) { set guest_result }
	   // set XER[CA]
	   // if (OE=1) { XER[SO,OV] }
	   return False;

       default:
	   return False;
       }
       break;
    default:
	return False;
    }

    putIReg( Rd_addr, mkexpr(Rd) );

    return True;
}



static Bool dis_int_cmp ( UInt theInstr )
{
    UChar opc1    = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar crfD    = (theInstr >> 23) & 0x1F;      /* theInstr[23:26] */
    UChar b9      = (theInstr >> 22) & 0x1;       /* theInstr[22]    */
    UChar flag_L  = (theInstr >> 21) & 0x1;       /* theInstr[21]    */
    UChar Ra_addr = (theInstr >> 14) & 0x1F;      /* theInstr[14:20] */

    /* D-Form */
    UInt  SIMM_16 = (theInstr >>  0) & 0xFFFF;    /* theInstr[0:15]  */
    UInt  UIMM_16 = (theInstr >>  0) & 0xFFFF;    /* theInstr[0:15]  */

    /* X-Form */
    UChar Rb_addr = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2    = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0      = (theInstr >>  0) & 1;         /* theInstr[0]     */

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
	DIP("cmpi %d,%d,%d,%d\n", crfD, flag_L, Ra_addr, SIMM_16);

	EXTS_SIMM = extend_s_16to32(SIMM_16);

	// CAB: This right?  Don't need a 'mkS32(EXTS_SIMM)' ?

	assign( tmp, IRExpr_Mux0X(
		    binop(Iop_CmpEQ32, mkexpr(Ra), mkU32(EXTS_SIMM)),
		    IRExpr_Mux0X( binop(Iop_CmpLT32S, mkU32(EXTS_SIMM), mkexpr(Ra)),
				  mkU32(2), mkU32(4) ), mkU32(8) ));

	assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );
	break;

    case 0x0A: // cmpli (Compare Logical Immediate, p400)
	DIP("cmpli %d,%d,%d,%d\n", crfD, flag_L, Ra_addr, UIMM_16);
	assign( tmp, IRExpr_Mux0X(
		    binop(Iop_CmpEQ32, mkexpr(Ra), mkU32(UIMM_16)),
		    IRExpr_Mux0X( binop(Iop_CmpLT32U, mkU32(UIMM_16), mkexpr(Ra)),
				  mkU32(2), mkU32(4) ), mkU32(8) ));

	assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

	break;

    /* X Form */
    case 0x1F:
	if (b0 != 0) { return False; }

	switch (opc2) {
	case 0x000: // cmp (Compare, p397)
	    DIP("cmp %d,%d,%d,%d\n", crfD, flag_L, Ra_addr, Rb_addr);
	    assign( Rb, getIReg(Rb_addr) );
	    assign( tmp, IRExpr_Mux0X(
			binop(Iop_CmpEQ32, mkexpr(Ra), mkexpr(Rb)),
			IRExpr_Mux0X( binop(Iop_CmpLT32S, mkexpr(Rb), mkexpr(Ra)),
				      mkU32(2), mkU32(4) ), mkU32(8) ));
	    assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

	    break;

        case 0x020: // cmpl (Compare Logical, p399)
	    DIP("cmpl %d,%d,%d,%d\n", crfD, flag_L, Ra_addr, Rb_addr);
	    assign( Rb, getIReg(Rb_addr) );
	    assign( tmp, IRExpr_Mux0X(
			binop(Iop_CmpEQ32, mkexpr(Ra), mkexpr(Rb)),
			IRExpr_Mux0X( binop(Iop_CmpLT32U, mkexpr(Rb), mkexpr(Ra)),
				      mkU32(2), mkU32(4) ), mkU32(8) ));
	    assign( cr_flags, binop(Iop_Or32, mkexpr(tmp), mkexpr(xer_so)) );

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
    UChar opc1    = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rs_addr = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */

    /* D-Form */
    UInt  UIMM_16 = (theInstr >>  0) & 0xFFFF;    /* theInstr[0:15]  */

    /* X-Form */
    UChar Rb_addr = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2    = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar flag_Rc = (theInstr >>  0) & 1;         /* theInstr[0]     */

    IRTemp Rs = newTemp(Ity_I32);
//    IRTemp Ra = newTemp(Ity_I32);
//    IRTemp Rb = newTemp(Ity_I32);

    assign( Rs, getIReg(Ra_addr) );

    switch (opc1) {
    case 0x1C: // andi. (AND Immediate, p388)
	DIP("andi %d,%d,%d\n", Ra_addr, Rs_addr, UIMM_16);
	return False;

    case 0x1D: // andis. (AND Immediate Shifted, p389)
	DIP("andis %d,%d,%d\n", Ra_addr, Rs_addr, UIMM_16);
	return False;

    case 0x18: // ori (OR Immediate, p551)
	DIP("ori %d,%d,%d\n", Ra_addr, Rs_addr, UIMM_16);
	putIReg( Ra_addr, binop(Iop_Or32, mkexpr(Rs), mkU32(UIMM_16)) );
	break;

    case 0x19: // oris (OR Immediate Shifted, p552)
	DIP("oris %d,%d,%d\n", Ra_addr, Rs_addr, UIMM_16);
	putIReg( Ra_addr, binop(Iop_Or32, mkexpr(Rs),
				binop(Iop_Shl32, mkU32(UIMM_16), mkU32(16))) );
	break;

    case 0x1A: // xori (XOR Immediate, p625)
	DIP("xori %d,%d,%d\n", Ra_addr, Rs_addr, UIMM_16);
	return False;

    case 0x1B: // xoris (XOR Immediate Shifted, p626)
	DIP("xoris %d,%d,%d\n", Ra_addr, Rs_addr, UIMM_16);
	return False;

    /* X Form */
    case 0x1F:
	switch (opc2) {
	case 0x01C: // and (AND, p386)
	    DIP("and%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    return False;

	case 0x03C: // andc (AND with Complement, p387)
	    DIP("andc%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    return False;

	case 0x01A: // cntlzw (Count Leading Zeros Word, p402)
	    if (Rb_addr!=0) { return False; }
	    DIP("cntlzw%s %d,%d\n", flag_Rc ? "." : "", Ra_addr, Rs_addr);
	    return False;

	case 0x11C: // eqv (Equivalent, p427)
	    DIP("eqv%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    return False;

	case 0x3BA: // extsb (Extend Sign Byte, p428)
	    if (Rb_addr!=0) { return False; }
	    DIP("extsb%s %d,%d\n", flag_Rc ? "." : "", Ra_addr, Rs_addr);
	    return False;

	case 0x39A: // extsh (Extend Sign Half Word, p429)
	    if (Rb_addr!=0) { return False; }
	    DIP("extsh%s %d,%d\n", flag_Rc ? "." : "", Ra_addr, Rs_addr);
	    return False;

	case 0x1DA: // nand (NAND, p546)
	    DIP("nand%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    return False;

	case 0x07C: // nor (NOR, p548)
	    DIP("nor%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    return False;

	case 0x1BC: // or (OR, p549)
	    DIP("or%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    putIReg( Ra_addr, binop(Iop_Or32, mkexpr(Rs), getIReg(Rb_addr)) );
  	    if (flag_Rc==1) {
		// CAB: CR0 affected how?
	    }
	    break;

	case 0x19C: // orc  (OR with Complement, p550)
	    DIP("orc%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    putIReg( Ra_addr, binop(Iop_Or32, mkexpr(Rs),
				    unop(Iop_Not32, getIReg(Rb_addr))) );
  	    if (flag_Rc==1) {
		// CAB: ?
	    }
	    break;

	case 0x13C: // xor (XOR, p624)
	    DIP("xor%s %d,%d,%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
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



static Bool dis_int_rot ( UInt theInstr )
{
    /* M-Form */
    UChar opc1      = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rs_addr   = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr   = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UChar Rb_addr   = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UChar Shift_Imm = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UChar MaskBegin = (theInstr >>  6) & 0x1F;      /* theInstr[6:10]  */
    UChar MaskEnd   = (theInstr >>  1) & 0x1F;      /* theInstr[1:5]   */
    UChar flag_Rc   = (theInstr >>  0) & 1;         /* theInstr[0]     */

    switch (opc1) {
    case 0x14: // rlwimi (Rotate Left Word Immediate then Mask Insert, p561)
	DIP("rlwimi%s %d,%d,%d,%d,%d\n", flag_Rc ? "." : "",
	    Ra_addr, Rs_addr, Shift_Imm, MaskBegin, MaskEnd);
	return False;

    case 0x15: // rlwinm (Rotate Left Word Immediate then AND with Mask, p562)
	DIP("rlwimi%s %d,%d,%d,%d,%d\n", flag_Rc ? "." : "",
	    Ra_addr, Rs_addr, Shift_Imm, MaskBegin, MaskEnd);
	return False;

    case 0x17: // rlwnm (Rotate Left Word then AND with Mask, p564)
	DIP("rlwimi%s %d,%d,%d,%d,%d\n", flag_Rc ? "." : "",
	    Ra_addr, Rs_addr, Rb_addr, MaskBegin, MaskEnd);
	return False;

    default:
	return False;
    }
    return True;
}



static Bool dis_int_load ( UInt theInstr )
{
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rd_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr  = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */

    /* D-Form */
    UInt  d_offset = (theInstr >>  0) & 0xFFFF;    /* theInstr[0:15] */

    /* X-Form */
    UChar Rb_addr = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2    = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0      = (theInstr >>  0) & 1;         /* theInstr[0]     */


    switch (opc1) {
    case 0x22: // lbz (Load B & Zero, p468)
	DIP("lbz %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x23: // lbzu (Load B & Zero with Update, p469)
	DIP("lbzu %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x2A: // lha (Load HW Algebraic, p485)
	DIP("lha %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x2B: // lhau (Load HW Algebraic with Update, p486)
	DIP("lhau %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x28: // lhz (Load HW & Zero, p490)
	DIP("lhz %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x29: // lhzu (Load HW & and Zero with Update, p491)
	DIP("lhzu %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x20: // lwz (Load W & Zero, p504)
	DIP("lwz %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x21: // lwzu (Load W & Zero with Update, p505))
	DIP("lwzu %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    /* X Form */
    case 0x1F:
	if (b0 != 0) { return False; }
	switch (opc2) {
        case 0x077: // lbzux (Load B & Zero with Update Indexed, p470)
	    DIP("lbzux %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x057: // lbzx (Load B & Zero Indexed, p471)
	    DIP("lbzx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x177: // lhaux (Load HW Algebraic with Update Indexed, p487)
	    DIP("lhaux %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x157: // lhax (Load HW Algebraic Indexed, p488)
	    DIP("lhax %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x137: // lhzux (Load HW & Zero with Update Indexed, p492)
	    DIP("lhzux %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x117: // lhzx (Load HW & Zero Indexed, p493)
	    DIP("lhzx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x037: // lwzux (Load W & Zero with Update Indexed, p506)
	    DIP("lwzux %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x017: // lwzx (Load W & Zero Indexed, p507)
	    DIP("lwzx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
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



static Bool dis_int_store ( UInt theInstr )
{
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rs_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr  = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */

    /* D-Form */
    UInt  d_offset = (theInstr >>  0) & 0xFFFF;    /* theInstr[0:15] */

    /* X-Form */
    UChar Rb_addr = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2    = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0      = (theInstr >>  0) & 1;         /* theInstr[0]     */

    IRTemp Rs   = newTemp(Ity_I32);
    IRTemp tmp1 = newTemp(Ity_I32);
    IRTemp tmp2 = newTemp(Ity_I32);

    switch (opc1) {
    case 0x26: // stb (Store B, p576)
	DIP("stb %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	return False;

    case 0x27: // stbu (Store B with Update, p577)
	DIP("stbu %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	return False;

    case 0x2C: // sth (Store HW, p595)
	DIP("sth %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	return False;

    case 0x2D: // sthu (Store HW with Update, p597)
	DIP("sthu %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	return False;

    case 0x24: // stw (Store W, p603)
	DIP("stw %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	assign( Rs, mkexpr(Rs_addr) );
	if (Ra_addr == 0) {
	    assign( tmp1, mkU32(0) );
	} else {
	    assign( tmp1, getIReg(Ra_addr) );
	}
	assign( tmp2, binop(Iop_Add32, mkexpr(tmp1),
			    mkU32(extend_s_16to32(d_offset))) );
	storeBE( mkexpr(tmp2), mkexpr(Rs_addr) );
	break;

    case 0x25: // stwu (Store W with Update, p607)
	DIP("stwu %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	return False;

    /* X Form */
    case 0x1F:
	if (b0 != 0) { return False; }
	switch (opc2) {
	case 0x0F7: // stbux (Store B with Update Indexed, p578)
	    DIP("stbux %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	    return False;

	case 0x0D7: // stbx (Store B Indexed, p579)
	    DIP("stbx %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	    return False;

	case 0x1B7: // sthux (Store HW with Update Indexed, p598)
	    DIP("sthux %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	    return False;

	case 0x197: // sthx (Store HW Indexed, p599)
	    DIP("sthx %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	    return False;

	case 0x0B7: // stwux (Store W with Update Indexed, p608)
	    DIP("stwux %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	    return False;

	case 0x097: // stwx (Store W Indexed, p609)
	    DIP("stwx %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
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



static Bool dis_int_ldst_mult ( UInt theInstr )
{
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rd_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Rs_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr  = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */

    /* D-Form */
    UInt  d_offset = (theInstr >>  0) & 0xFFFF;    /* theInstr[0:15] */

    switch (opc1) {
    /* D Form */
    case 0x2E: // lmw (Load Multiple Word, p494)
	DIP("lmw %d,%d(%d)\n", Rd_addr, d_offset, Ra_addr);
	return False;

    case 0x2F: // stmw (Store Multiple Word, p600)
	DIP("stmw %d,%d(%d)\n", Rs_addr, d_offset, Ra_addr);
	return False;

    default:
	return False;
    }
    return True;
}



static Bool dis_int_ldst_str ( UInt theInstr )
{
    /* X-Form */
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rd_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Rs_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr  = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UChar NumBytes = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UChar Rb_addr  = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2     = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0       = (theInstr >>  0) & 1;         /* theInstr[0]     */

    if (opc1 != 0x1F || b0 != 0) { return False; }
    switch (opc2) {
    case 0x255: // lswi (Load String Word Immediate, p495)
	DIP("lswi %d,%d,%d\n", Rd_addr, Ra_addr, NumBytes);
	return False;

    case 0x215: // lswx (Load String Word Indexed, p497)
	DIP("lswx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	return False;

    case 0x2D5: // stswi (Store String Word Immediate, p601)
	DIP("stswi %d,%d,%d\n", Rs_addr, Ra_addr, NumBytes);
	return False;

    case 0x295: // stswx (Store String Word Indexed, p602)
	DIP("stswx %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	return False;

    default:
	return False;
    }
    return True;
}



static Bool dis_branch ( theInstr )
{
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar BO       = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar BI       = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UInt  BD       = (theInstr >>  2) & 0x3FFF;    /* theInstr[2:15]  */
    UChar b11to15  = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2     = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UInt  LI_24    = (theInstr >>  2) & 0xFFFFFF;  /* theInstr[2:25]  */
    UChar flag_AA  = (theInstr >>  1) & 1;         /* theInstr[1]     */
    UChar flag_LK  = (theInstr >>  0) & 1;         /* theInstr[0]     */

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

//    vex_printf("disInstr(ppc32): In: 0x%8x, %,031b\n", theInstr, theInstr );
//    vex_printf("disInstr(ppc32): LI: %,039b\n", LI_24);
//    vex_printf("disInstr(ppc32): LI: %,039b\n", LI_24 << 2);
//    vex_printf("disInstr(ppc32): LI: %,031b\n", extend_s_24to32(LI_24 << 2));

    switch (opc1) {
    case 0x12: // b     (Branch, p390)
	DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", LI_24);
	assign( tmp, mkU32(extend_s_24to32(LI_24 << 2)) );
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
	break;

    case 0x10: // bc    (Branch Conditional, p391)
	DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
	    flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, BD);

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
	return False;

    case 0x13:
	if (b11to15!=0) { return False; }

	switch (opc2) {
        case 0x210: // bcctr (Branch Cond. to Count Register, p393) 
	    DIP("bcctr%s 0x%x, 0x%x,\n", flag_LK ? "l" : "", BO, BI);
/*
	    cond_ok = BO[0] | (CR[BI] == BO[1])
	    if (cond_ok) {
		NIA = CTR[0-61] || 0b00
		if (flag_LK) {
		    LR = CIA + 4
		}
	    }
*/
	    return False;

        case 0x010: // bclr (Branch Cond. to Link Register, p395) 
	    DIP("bclr%s 0x%x, 0x%x,\n", flag_LK ? "l" : "", BO, BI);
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



static Bool dis_syslink ( UInt theInstr )
{
    if (theInstr != 0x44000010) { return False; }
    // sc  (System Call, p565)
    DIP("sc\n");
    // ...
    return False;
}



static Bool dis_memsync ( UInt theInstr )
{
    /* X-Form, XL-Form */
    UChar opc1      = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar b11to25   = (theInstr >> 11) & 0x7FFF;    /* theInstr[11:25]  */
    UChar Rd_addr   = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Rs_addr   = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr   = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UChar Rb_addr   = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2      = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0        = (theInstr >>  0) & 1;         /* theInstr[0]     */

    switch (opc1) {
    /* XL-Form */
    case 0x13:	// isync (Instruction Synchronize, p467)
	if (opc2 != 0x096) { return False; }
	if (b11to25 != 0 || b0 != 0) { return False; }
	DIP("isync\n");
	return False;

    /* X-Form */
    case 0x1F:
	switch (opc2) {
        case 0x356: // eieio (Enforce In-Order Execution of I/O, p425)
	    if (b11to25 != 0 || b0 != 0) { return False; }
	    DIP("eieio\n");
	    return False;

        case 0x014: // lwarx (Load Word and Reserve Indexed, p500)
	    if (b0 != 0) { return False; }
	    DIP("lwarx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x096: // stwcx. (Store Word Conditional Indexed, p605)
	    if (b0 != 1) { return False; }
	    DIP("stwcx. %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	    return False;

        case 0x256: // sync (Synchronize, p616)
	    if (b11to25 != 0 || b0 != 0) { return False; }
	    DIP("sync\n");
	    return False;

        default:
	    return False;
	}
	return True;
    default:
	return False;
    }
    return True;
}



static Bool dis_int_shift ( UInt theInstr )
{
    /* X-Form */
    UChar opc1      = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rs_addr   = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr   = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UChar Rb_addr   = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UChar Shift_Imm = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2      = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar flag_Rc   = (theInstr >>  0) & 1;         /* theInstr[0]     */

    if (opc1 == 0x1F) {
	switch (opc2) {
        case 0x018: // slw (Shift Left Word, p569)
	    DIP("slw%s %d,%d,%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Rb_addr);
	    return False;

        case 0x318: // sraw (Shift Right Algebraic Word, p572)
	    DIP("sraw%s %d,%d,%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Rb_addr);
	    return False;

        case 0x338: // srawi (Shift Right Algebraic Word Immediate, p573)
	    DIP("srawi%s %d,%d,%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Shift_Imm);
	    return False;

        case 0x218: // srw (Shift Right Word, p575)
	    DIP("srw%s %d,%d,%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Rb_addr);
	    return False;

        default:
	    return False;
	}
    }
    return True;
}



static Bool dis_int_ldst_rev ( UInt theInstr )
{
    /* X-Form */
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar Rd_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Rs_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr  = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UChar Rb_addr  = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2     = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0       = (theInstr >>  0) & 1;         /* theInstr[0]     */

    if (opc1 != 0x1F || b0 != 0) {
	return False;
    }

    switch (opc2) {
    case 0x316: // lhbrx (Load Half Word Byte-Reverse Indexed, p489)
	DIP("lhbrx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	return False;

    case 0x216: // lwbrx (Load Word Byte-Reverse Indexed, p503)
	DIP("lwbrx %d,%d,%d\n", Rd_addr, Ra_addr, Rb_addr);
	return False;

    case 0x396: // sthbrx (Store Half Word Byte-Reverse Indexed, p596)
	DIP("sthbrx %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	return False;

    case 0x296: // stwbrx (Store Word Byte-Reverse Indexed, p604)
	DIP("stwbrx %d,%d,%d\n", Rs_addr, Ra_addr, Rb_addr);
	return False;

    default:
	return False;
    }
    return True;
}



static Bool dis_proc_ctl ( UInt theInstr )
{
    UChar opc1     = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */

    /* X-Form */
    UChar crfD     = (theInstr >> 23) & 0x7;       /* theInstr[23:25] */
    UChar b21to22  = (theInstr >> 21) & 0x3;       /* theInstr[21:22] */
    UChar Rd_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UInt  b11to20  = (theInstr >> 11) & 0x3FF;     /* theInstr[11:20] */

    /* XFX-Form */
    UChar Rs_addr  = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UInt  SPR      = (theInstr >> 11) & 0x3FF;     /* theInstr[11:20] */
    UInt  TBR      = (theInstr >> 11) & 0x3FF;     /* theInstr[11:20] */
    UChar b20      = (theInstr >> 11) & 0x1;       /* theInstr[11]    */
    UInt  CRM      = (theInstr >> 12) & 0xFF;      /* theInstr[12:19] */
    UChar b11      = (theInstr >> 11) & 0x1;       /* theInstr[20]    */
    UInt  opc2     = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0       = (theInstr >>  0) & 1;         /* theInstr[0]     */

    if (opc1 != 0x1F || b0 != 0) {
	return False;
    }

    switch (opc2) {
    /* X-Form */
    case 0x200: // mcrxr (Move to Condition Register from XER, p510)
	if (b21to22 != 0 || b11to20 != 0) { return False; }
	DIP("mcrxr %d\n", crfD);
	return False;

    case 0x013: // mfcr (Move from Condition Register, p511)
	if (b11to20 != 0) { return False; }
	DIP("mfcr %d\n", Rd_addr);
	return False;

    /* XFX-Form */
    case 0x153: // mfspr (Move from Special-Purpose Register, p514)
	DIP("mfspr %d,%d\n", Rd_addr, SPR);
	return False;

    case 0x173: // mftb (Move from Time Base, p521)
	DIP("mftb %d,%d\n", Rd_addr, TBR);
	return False;

    case 0x090: // mtcrf (Move to Condition Register Fields, p523)
	if (b11 != 0 || b20 != 0) { return False; }
	DIP("mtcrf %d,%d\n", CRM, Rs_addr);
	return False;

    case 0x1D3: // mtspr (Move to Special-Purpose Register, p530)
	DIP("mtspr %d,%d\n", SPR, Rs_addr);
	return False;

    default:
	return False;
    }
    return True;
}


static Bool dis_cache_manage ( UInt theInstr )
{
    /* X-Form */
    UChar opc1    = (theInstr >> 26) & 0x3F;      /* theInstr[26:31] */
    UChar b21to25 = (theInstr >> 21) & 0x1F;      /* theInstr[21:25] */
    UChar Ra_addr = (theInstr >> 16) & 0x1F;      /* theInstr[16:20] */
    UChar Rb_addr = (theInstr >> 11) & 0x1F;      /* theInstr[11:15] */
    UInt  opc2    = (theInstr >>  1) & 0x3FF;     /* theInstr[1:10]  */
    UChar b0      = (theInstr >>  0) & 1;         /* theInstr[0]     */

    if (opc1 != 0x1F || b21to25 != 0 || b0 != 0) {
	return False;
    }

    switch (opc2) {
    case 0x2F6: // dcba (Data Cache Block Allocate, p411)
	DIP("dcba %d,%d\n", Ra_addr, Rb_addr);
	return False;

    case 0x056: // dcbf (Data Cache Block Flush, p413)
	DIP("dcbf %d,%d\n", Ra_addr, Rb_addr);
	return False;

    case 0x036: // dcbst (Data Cache Block Store, p415)
	DIP("dcbst %d,%d\n", Ra_addr, Rb_addr);
	return False;

    case 0x116: // dcbt (Data Cache Block Touch, p416)
	DIP("dcbt %d,%d\n", Ra_addr, Rb_addr);
	return False;

    case 0x0F6: // dcbtst (Data Cache Block Touch for Store, p417)
	DIP("dcbtst %d,%d\n", Ra_addr, Rb_addr);
	return False;

    case 0x3F6: // dcbz (Data Cache Block Clear to Zero, p418)
	DIP("dcbz %d,%d\n", Ra_addr, Rb_addr);
	return False;

    case 0x3D6: // icbi (Instruction Cache Block Invalidate, p466)
	DIP("icbi %d,%d\n", Ra_addr, Rb_addr);
	return False;

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
   UChar opc1;
   UInt opc2;
//   PPC32Condcode cond;
   DisResult whatNext = Dis_Continue;
   UInt      theInstr;


   /* At least this is simple on PPC32: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */

   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_PPC32 abort if we fail the
      decode. */
   *size = 0;

   theInstr = getUIntBigendianly( (HChar*)(&guest_code[delta]) );

//   vex_printf("START: 0x%x, %,b\n", theInstr, theInstr );

   DIP("\t0x%x:  ", guest_pc_bbstart+delta);



   // TODO: fix the client-request stuff, else nothing will work

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

         // uh ... I'll figure this out later.  possibly r0 = client_request(r0)
         DIP("?CAB? = client_request ( ?CAB? )\n");

	 *size = 24;

	 irbb->next     = mkU32(guest_pc_bbstart+delta);
	 irbb->jumpkind = Ijk_ClientReq;

         whatNext = Dis_StopHere;
         goto decode_success;
      }
   }


   opc1 = (theInstr >> 26) & 0x3F;     /* theInstr[26:31] */
   opc2 = (theInstr >> 1 ) & 0x3FF;    /* theInstr[1:10]  */

//   vex_printf("\n");
//   vex_printf("disInstr(ppc32): instr:   0x%8x, %,039b\n", theInstr, theInstr );
//   vex_printf("disInstr(ppc32): opcode1: 0x%02x, %06b\n", opc1, opc1 );
//   vex_printf("disInstr(ppc32): opcode2: 0x%02x, %010b\n", opc2, opc2 );

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
       if (dis_int_arith(theInstr)) break;
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
     Integer Rotate Instructions
   */
   case 0x14: // rlwimi
   case 0x15: // rlwinm
   case 0x17: // rlwnm
       if (dis_int_rot(theInstr)) break;
       goto decode_failure;

   /*
     Integer Load Instructions
   */
   case 0x22: // lbz
   case 0x23: // lbzu
   case 0x2A: // lha
   case 0x2B: // lhau
   case 0x28: // lhz
   case 0x29: // lhzu
   case 0x20: // lwz
   case 0x21: // lwzu
       if (dis_int_load(theInstr)) break;
       goto decode_failure;

   /*
     Integer Store Instructions
   */
   case 0x26: // stb
   case 0x27: // stbu
   case 0x2C: // sth
   case 0x2D: // sthu
   case 0x24: // stw
   case 0x25: // stwu
       if (dis_int_store(theInstr)) break;
       goto decode_failure;

   /*
     Integer Load and Store Multiple Instructions
   */
   case 0x2E: // lmw
   case 0x2F: // stmw
       if (dis_int_ldst_mult(theInstr)) break;
       goto decode_failure;

   /*
      Branch Instructions
   */
   case 0x12: // b
   case 0x10: // bc
       if (dis_branch(theInstr)) break;
       goto decode_failure;

   /*
     System Linkage Instructions
   */
   case 0x11: // sc
       if (dis_syslink(theInstr)) break;
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

       /*
	 Memory Synchronization Instructions
       */
       case 0x096: // isync
	   if (dis_memsync(theInstr)) break;
	   goto decode_failure;

       default:
	   goto decode_failure;
       }
       break;


    case 0x1F:
       opc2 = (theInstr >> 1) & 0x1FF;    /* theInstr[1:9] */
       switch (opc2) {

       /*
	 Integer Arithmetic Instructions
       */
       case 0x10A: // add
       case 0x00A: // addc
       case 0x08A: // adde
       case 0x0EA: // addme
       case 0x0CA: // addze
	   if (dis_int_arith(theInstr)) goto decode_success;
	   goto decode_failure;

       default:
	   break;
       }


       opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
       switch (opc2) {

       /*
	 Integer Compare Instructions
       */
       case 0x000: // cmp
       case 0x020: // cmpl
	   if (dis_int_cmp(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Logical Instructions
       */
       case 0x01C: // and
       case 0x03C: // andc
       case 0x01A: // cntlzw
       case 0x11C: // eqv
       case 0x3BA: // extsb
       case 0x39A: // extsh
       case 0x1DA: // nand
       case 0x07C: // nor
       case 0x1BC: // or
       case 0x19C: // orc
       case 0x13C: // xor
	   if (dis_int_logic(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Shift Instructions
       */
       case 0x018: // slw
       case 0x318: // sraw
       case 0x338: // srawi
       case 0x218: // srw
	   if (dis_int_shift(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Load Instructions
       */
       case 0x057: // lbzx
       case 0x077: // lbzux
       case 0x157: // lhax
       case 0x177: // lhaux
       case 0x117: // lhzx
       case 0x137: // lhzux
       case 0x017: // lwzx
       case 0x037: // lwzux
	   if (dis_int_load(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Store Instructions
       */
       case 0x0F7: // stbux
       case 0x0D7: // stbx
       case 0x1B7: // sthux
       case 0x197: // sthx
       case 0x0B7: // stwux
       case 0x097: // stwx
	   if (dis_int_store(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Load and Store with Byte Reverse Instructions
       */
       case 0x316: // lhbrx
       case 0x216: // lwbrx
       case 0x396: // sthbrx
       case 0x296: // stwbrx
	   if (dis_int_ldst_rev(theInstr)) break;
	   goto decode_failure;

       /*
	 Integer Load and Store String Instructions
       */
       case 0x255: // lswi
       case 0x215: // lswx
       case 0x2D5: // stswi
       case 0x295: // stswx
	   if (dis_int_ldst_str(theInstr)) break;
	   goto decode_failure;

       /*
	 Memory Synchronization Instructions
       */
       case 0x356: // eieio
       case 0x014: // lwarx
       case 0x096: // stwcx.
       case 0x256: // sync
	   if (dis_memsync(theInstr)) break;
	   goto decode_failure;

       /*
	 Processor Control Instructions
       */
       case 0x200: // mcrxr
       case 0x013: // mfcr
       case 0x153: // mfspr
       case 0x173: // mftb
       case 0x090: // mtcrf
       case 0x1D3: // mtspr
	   if (dis_proc_ctl(theInstr)) break;
	   goto decode_failure;

       /*
	 Cache Management Instructions
       */
       case 0x2F6: // dcba
       case 0x056: // dcbf
       case 0x036: // dcbst
       case 0x116: // dcbt
       case 0x0F6: // dcbtst
       case 0x3F6: // dcbz
       case 0x3D6: // icbi
	   if (dis_cache_manage(theInstr)) break;
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
