
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
    06/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/F6153E213FDD912E87256D49006C6541

   Also see (but not for the page refs):
   "PowerPC Microprocessor Family:
    The Programming Environments for 32-Bit Microprocessors"
    02/21/2000
    http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600719DF2
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

/* The guest address for the instruction currently being
   translated. */
/* CONST for any specific insn, not for the entire BB */
static Addr32 guest_cia_curr_instr;

/* The IRBB* into which we're generating code. */
static IRBB* irbb;


// Non-gpr/fpr registers
typedef enum {
    REG_LR,     // Link Register
    REG_CTR,    // Count Register
    REG_CR,     // Condition Register
    REG_FPSCR,  // Floating Point Status and Control Register
    REG_XER,    // Summary Overflow
    REG_NUMBER
} PPC32Reg;


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

#define OFFB_XER_SO     offsetof(VexGuestPPC32State,guest_XER_SO)
#define OFFB_XER_OV     offsetof(VexGuestPPC32State,guest_XER_OV)
#define OFFB_XER_CA     offsetof(VexGuestPPC32State,guest_XER_CA)
#define OFFB_XER_BC     offsetof(VexGuestPPC32State,guest_XER_BC)


/*------------------------------------------------------------*/
/*--- Offsets of bitfields within various ppc32 registers. ---*/
/*------------------------------------------------------------*/
#define OFFBIT_XER_SO 31
#define OFFBIT_XER_OV 30
#define OFFBIT_XER_CA 29
#define OFFBIT_XER_BC 0



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
      resteerOK = toBool(n_instrs < vex_control.guest_chase_thresh);
      first_stmt_idx = irbb->stmts_used;

      guest_cia_curr_instr = guest_pc_bbstart + delta;

      if (n_instrs > 0) {
         /* for the first insn, the dispatch loop will have set
            CIA, but for all the others we have to do it ourselves. */
         stmt( IRStmt_Put( OFFB_CIA, mkU32(guest_cia_curr_instr)) );
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
            delta = (UInt)(guest_next - guest_pc_start);
            n_resteers++;
            d_resteers++;
            if (0 && (n_resteers & 0xFF) == 0)
            vex_printf("resteer[%d,%d] to %p (delta = %d)\n",
                       n_resteers, d_resteers,
                       ULong_to_Ptr(guest_next), (Int)delta);
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

//   vassert(!host_is_bigendian);   //TODO: is this necessary?
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

#if 0
static IRExpr* mkU ( IRType ty, UInt i )
{
   if (ty == Ity_I8)  return mkU8(i);
   if (ty == Ity_I16) return mkU16(i);
   if (ty == Ity_I32) return mkU32(i);
   /* If this panics, it usually means you passed a size (1,2,4)
      value as the IRType, rather than a real IRType. */
   vpanic("mkU(ppc32)");
}
#endif

static IRExpr* loadBE ( IRType ty, IRExpr* data )
{
   return IRExpr_LDle(ty,data);
}

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
   vpanic("mkWidenOp(ppc32,guest)");
}
#endif


// ROTL(src32, rot_amt5)
static IRExpr* ROTL32 ( IRExpr* src, IRExpr* rot_amt )
{
    vassert(typeOfIRExpr(irbb->tyenv,src) == Ity_I32);
    vassert(typeOfIRExpr(irbb->tyenv,rot_amt) == Ity_I8);

    /* By masking the rotate amount thusly, the IR-level Shl/Shr
       expressions never shift beyond the word size and thus remain
       well defined. */
    IRTemp rot_amt5 = newTemp(Ity_I8);
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

static IRExpr* mk_ppc32g_calculate_cr7_all ( void )
{
   IRExpr* op = unop(Iop_8Uto32, IRExpr_Get(OFFB_CC_OP,   Ity_I8));
   IRExpr* d1 = IRExpr_Get(OFFB_CC_DEP1, Ity_I32);
   IRExpr* d2 = unop(Iop_8Uto32, IRExpr_Get(OFFB_CC_DEP2, Ity_I8));

   IRExpr** args = mkIRExprVec_3( op, d1, d2 );

   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "ppc32g_calculate_cr7_all", &ppc32g_calculate_cr7_all,
           args
        );

   /* Exclude OP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
//   call->Iex.CCall.cee->mcx_mask = 1;

// CAB: Haven't looked at the whole 'definedness' stuff...
// 02/02/05 - leaving 'till get memcheck working well.

   return call;
}


// Calculate XER_OV flag
static IRExpr* mk_ppc32g_calculate_xer_ov ( UInt op, IRExpr* res,
					    IRExpr* arg1, IRExpr* arg2 )
{
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,arg1) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,arg2) == Ity_I32);

   IRExpr* xer_ov = unop(Iop_8Uto32, IRExpr_Get(OFFB_XER_OV, Ity_I8));

   IRExpr** args = mkIRExprVec_5( mkU32(op), res, arg1, arg2, xer_ov );

   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/,
           "ppc32g_calculate_xer_ov", &ppc32g_calculate_xer_ov,
           args
        );
   return unop(Iop_32to1, call);
}

// Calculate XER_CA flag
static IRExpr* mk_ppc32g_calculate_xer_ca ( UInt op, IRExpr* res,
					    IRExpr* arg1, IRExpr* arg2 )
{
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,arg1) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,arg2) == Ity_I32);

   IRExpr* xer_ca = unop(Iop_8Uto32, IRExpr_Get(OFFB_XER_CA, Ity_I8));

   IRExpr** args = mkIRExprVec_5( mkU32(op), res, arg1, arg2, xer_ca );

   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/,
           "ppc32g_calculate_xer_ca", &ppc32g_calculate_xer_ca,
           args
        );
   return unop(Iop_32to1, call );
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
      default: vpanic("widenUto32(ppc32)");
   }
}
#endif

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
   vpanic("narrowTo(ppc32)");
}


/* Set the flags thunk OP=0, DEP1, DEP2 fields. */
static  void setFlags_CR7 ( IRExpr* result )
{
   vassert(typeOfIRExpr(irbb->tyenv,result) == Ity_I32);

   // => Delaying calculating result until needed...
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU8(0) ));
   stmt( IRStmt_Put( OFFB_CC_DEP1, result ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, IRExpr_Get(OFFB_XER_SO, Ity_I8) ));
}

/* Write exactly the given flags to field CR7
   Set the flags thunk OP=1, DEP1 fields, DEP2=0. */
static void setFlags_CR7_Imm ( IRExpr* flags )
{
   vassert(typeOfIRExpr(irbb->tyenv,flags) == Ity_I32);

   // Just in case...
   IRExpr* flags_masked = binop(Iop_And32, flags, mkU32(0xF0000000));

   // => Delaying calculating result until needed...
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU8(1)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, flags_masked) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU8(0)) );
}

static void setFlags_XER_OV_SO( UInt op, IRExpr* res,
				IRExpr* arg1, IRExpr* arg2 )
{
    vassert(op < PPC32G_FLAG_OP_NUMBER);
    vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
    vassert(typeOfIRExpr(irbb->tyenv,arg1) == Ity_I32);
    vassert(typeOfIRExpr(irbb->tyenv,arg2) == Ity_I32);

    // => Calculate result immediately
    IRExpr* xer_ov = mk_ppc32g_calculate_xer_ov(op, res, arg1, arg2);
    stmt( IRStmt_Put( OFFB_XER_SO, unop(Iop_1Uto8, xer_ov) ));
    stmt( IRStmt_Put( OFFB_XER_OV, unop(Iop_1Uto8, xer_ov) ));
}

static void setFlags_XER_CA( UInt op, IRExpr* res,
			     IRExpr* arg1, IRExpr* arg2 )
{
    vassert(op < PPC32G_FLAG_OP_NUMBER);
    vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
    vassert(typeOfIRExpr(irbb->tyenv,arg1) == Ity_I32);
    vassert(typeOfIRExpr(irbb->tyenv,arg2) == Ity_I32);

    // => Calculate result immediately
    IRExpr* xer_ca = mk_ppc32g_calculate_xer_ca(op, res, arg1, arg2);
    stmt( IRStmt_Put( OFFB_XER_CA, unop(Iop_1Uto8, xer_ca) ));
}







/* ------------- Abstract register interface. ------------- */
/*
  Most registers are represented directly in the cpu_state
  Others are represented by the thunk
*/

/* Get single bits from given [register,idx] */
static IRExpr* getReg_bit ( PPC32Reg reg, UInt bit_idx )
{
    vassert( bit_idx <= 32 );
    vassert( reg < REG_NUMBER );

    IRExpr* val;

    switch (reg) {
    case REG_XER:
	switch (bit_idx) {
	case OFFBIT_XER_SO:
	    val = IRExpr_Get(OFFB_XER_SO, Ity_I8);
	    break;
	case OFFBIT_XER_OV:
	    val = IRExpr_Get(OFFB_XER_OV, Ity_I8);
	    break;
	case OFFBIT_XER_CA:
	    val = IRExpr_Get(OFFB_XER_CA, Ity_I8);
	    break;
	case OFFBIT_XER_BC:
	    val = IRExpr_Get(OFFB_XER_BC, Ity_I8);
	    break;
	default:
	    vpanic("getReg_bit(ppc32, bit_idx)");
	}
	break;

    default:
	vpanic("getReg_bit(ppc32, reg)");
    }
    return unop(Iop_8Uto32, binop(Iop_And8, val, mkU8(1)) );
}

/* Get single bits from given [register,idx],
   shifted to position bit_idx  */
static IRExpr* getReg_bit_shifted ( PPC32Reg reg, UInt bit_idx )
{
    vassert( bit_idx <= 32 );
    vassert( reg < REG_NUMBER );
    return binop(Iop_Shl32, getReg_bit( reg, bit_idx ), mkU8(bit_idx));
}


/* Get a word, controlled by a mask, from the given register */
static IRExpr* getReg_masked ( PPC32Reg reg, UInt mask )
{
    IRTemp val       = newTemp(Ity_I32);
    IRExpr* irx_tmp1 = mkU32(0);
    IRExpr* irx_tmp2 = mkU32(0);
    IRExpr* irx_tmp3 = mkU32(0);
    IRExpr* irx_tmp4 = mkU32(0);

    vassert( mask > 0 );
    vassert( reg < REG_NUMBER );

    switch (reg) {
    case REG_LR:
	assign( val, IRExpr_Get(OFFB_LR, Ity_I32) );
	break;

    case REG_CTR:
	assign( val, IRExpr_Get(OFFB_CTR, Ity_I32) );
	break;

    case REG_CR:	
	if (mask & 0xF0000000) {
	    // Call helper function to calculate latest CR7 from thunk:
	    irx_tmp1 = mk_ppc32g_calculate_cr7_all();
	}
	irx_tmp2 = IRExpr_Get(OFFB_CR0to6, Ity_I32);
	assign( val, binop(Iop_Or32, irx_tmp1, irx_tmp2) );
	break;

    case REG_XER:
	if (mask & 0x80000000) {
	    irx_tmp1 = getReg_bit_shifted( REG_XER, OFFBIT_XER_SO );
	}
	if (mask & 0x40000000) {
	    irx_tmp2 = getReg_bit_shifted( REG_XER, OFFBIT_XER_OV );
	}
	if (mask & 0x20000000) {
	    irx_tmp3 = getReg_bit_shifted( REG_XER, OFFBIT_XER_CA );
	}
	if (mask & 0x0000007F) {
	    irx_tmp4 = unop(Iop_8Uto32, 
			    binop(Iop_And8, 
				  IRExpr_Get(OFFB_XER_BC, Ity_I8),
				  mkU32(0x7F)));
	}
	assign( val, binop(Iop_Or32,
			   binop(Iop_Or32, irx_tmp1, irx_tmp2),
			   binop(Iop_Or32, irx_tmp3, irx_tmp4)) );
	break;

    case REG_FPSCR:
    default:
	vpanic("getReg(ppc32)");
    }

    return binop(Iop_And32, mkexpr(val), mkU32(mask));
}

/* Get a word, controlled by a mask, from the given register */
static IRExpr* getReg ( PPC32Reg reg )
{
    vassert( reg < REG_NUMBER );
    return getReg_masked( reg, 0x0FFFFFFF );
}


/* Write single bit to given [register,idx] */
static void putReg_bit ( PPC32Reg reg, UInt bit_idx, IRExpr* src )
{
    vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I1 );
    vassert( bit_idx <= 32 );
    vassert( reg < REG_NUMBER );

    switch (reg) {
    case REG_XER:
	switch (bit_idx) {
	case OFFBIT_XER_SO:
	    stmt( IRStmt_Put( OFFB_XER_SO, unop(Iop_1Uto8, src) ));
	    break;
	case OFFBIT_XER_OV:
	    stmt( IRStmt_Put( OFFB_XER_OV, unop(Iop_1Uto8, src) ));
	    break;
	case OFFBIT_XER_CA:
	    stmt( IRStmt_Put( OFFB_XER_CA, unop(Iop_1Uto8, src) ));
	    break;
	case OFFBIT_XER_BC:
	    stmt( IRStmt_Put( OFFB_XER_BC, unop(Iop_1Uto8, src) ));
	    break;
	default:
	    vpanic("putReg_bit(ppc32, bit_idx)");
	}
	break;

    default:
	vpanic("putReg_bit(ppc32, reg)");
    }
}

/* Write a word, controlled by a mask, to the given register */
static void putReg_masked ( PPC32Reg reg, IRExpr* src, UInt mask )
{
    vassert( reg < REG_NUMBER );
    vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
    vassert( mask > 0 );

    IRTemp src_mskd = newTemp(Ity_I32);
    IRTemp old      = newTemp(Ity_I32);
    IRTemp tmp      = newTemp(Ity_I32);

    assign( src_mskd, binop(Iop_And32, src, mkU32(mask)) );

    switch (reg) {
    case REG_LR:
	stmt( IRStmt_Put( OFFB_LR, src) );
	break;

    case REG_CTR:
	stmt( IRStmt_Put( OFFB_CTR, src) );
	break;

    case REG_CR:
	if (mask & 0xF0000000) { // CR 7:
	    setFlags_CR7_Imm( mkexpr(src_mskd) );
	}
	// CR 0 to 6:
	assign( old, binop(Iop_And32, getReg( REG_CR ), mkU32(~mask)) );
	assign( tmp, binop(Iop_And32, mkexpr(src_mskd), mkU32(0x0FFFFFFF)) );
	stmt( IRStmt_Put( OFFB_CR0to6, binop(Iop_Or32,
					     mkexpr(tmp),mkexpr(old)) ));
	break;

    case REG_XER:
	if (mask & (1 << OFFBIT_XER_SO)) {
	    putReg_bit( REG_XER, OFFBIT_XER_SO,
			unop(Iop_32to1,
			     binop(Iop_Shr32, src, mkU8(OFFBIT_XER_SO))) );
	}
	if (mask & (1 << OFFBIT_XER_OV)) {
	    putReg_bit( REG_XER, OFFBIT_XER_OV,
			unop(Iop_32to1,
			     binop(Iop_Shr32, src, mkU8(OFFBIT_XER_OV))) );
	}
	if (mask & (1 << OFFBIT_XER_CA)) {
	    putReg_bit( REG_XER, OFFBIT_XER_CA,
			unop(Iop_32to1,
			     binop(Iop_Shr32, src, mkU8(OFFBIT_XER_CA))) );
	}
	if (mask & (0x7F << OFFBIT_XER_BC)) {
	    stmt( IRStmt_Put( OFFB_XER_BC,
			      binop(Iop_And8, mkU8(0x7F),
				    unop(Iop_32to8, src)) ));
	}
	break;

    case REG_FPSCR:
    default:
	vpanic("putReg(ppc32)");
    }
}

/* Write a word to the given register */
static void putReg ( PPC32Reg reg, IRExpr* src )
{
    vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
    vassert( reg < REG_NUMBER );
    putReg_masked( reg, src, 0xFFFFFFFF );
}


/* Write a nibble (least significant) to the given register,field */
static void putReg_field ( PPC32Reg reg, UInt field_idx, IRExpr* src )
{
    vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
    vassert( field_idx <= 8 );
    vassert( reg < REG_NUMBER );

    UInt bit_idx = field_idx * 4;
    UInt field_mask = 0x0000000F << field_idx;

    IRTemp val = newTemp(Ity_I32);

    assign( val, binop(Iop_Shl32,
		       binop(Iop_And32, src, mkU32(0xF)),
		       mkU8(bit_idx)) );

    switch (reg) {
    case REG_CR:
        putReg_masked( REG_CR, mkexpr(val), field_mask );
	break;

    case REG_XER:
	putReg_masked( REG_XER, mkexpr(val), field_mask );
	break;

    default:
	vpanic("putReg_field(ppc32)");
    }
}


















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

    UInt op    = PPC32G_FLAG_OP_NUMBER;
    Bool do_ca = False;
    Bool do_ov = False;
    Bool do_rc = False;
    IRExpr* arg1;
    IRExpr* arg2;

    assign( Ra, getIReg(Ra_addr) );
    assign( Rb, getIReg(Rb_addr) );         // XO-Form: Rd, Ra, Rb
    EXTS_SIMM = extend_s_16to32(SIMM_16);   // D-Form:  Rd, Ra, EXTS(SIMM)

    arg1 = mkexpr(Ra);
    arg2 = mkU32(EXTS_SIMM);

    assign( xer_ca, getReg_bit(REG_XER, OFFBIT_XER_CA) );


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
	DIP("addic r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	op = PPC32G_FLAG_OP_ADD;
	do_ca = True;
	break;
	
    case 0x0E: // addic. (Add Immediate Carrying and Record, p382)
	DIP("addic. r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
	op = PPC32G_FLAG_OP_ADD;
	do_ca = True;
	do_rc = True;
	flag_Rc = 1;
	break;

    case 0x0F: // addis  (Add Immediate Shifted, p383)
	// lis rD,val == addis rD,0,val
	DIP("addis r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	if ( Ra_addr == 0 ) {
	    assign( Rd, mkU32(EXTS_SIMM << 16) );
	} else {
	    assign( Rd, binop(Iop_Add32, mkexpr(Ra),
			      mkU32(EXTS_SIMM << 16)) );
	}
	break;

    case 0x07: // mulli    (Multiply Low Immediate, p544)
	DIP("mulli r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	assign( res64, binop(Iop_MullS32, mkexpr(Ra), mkU32(EXTS_SIMM)) );
	assign( Rd, unop(Iop_64to32, mkexpr(res64)) );
	break;

    case 0x08: // subfic   (Subtract from Immediate Carrying, p613)
	DIP("subfic r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
	assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
			  mkU32(EXTS_SIMM)) );
	op = PPC32G_FLAG_OP_SUBFI;
	do_ca = True;
	break;


    /* XO-Form */
    case 0x1F:
       arg2 = mkexpr(Rb);
       do_rc = True;    // All below record to CR

       switch (opc2) {
       case 0x10A: // add  (Add, p377)
	   DIP("add%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
	   op = PPC32G_FLAG_OP_ADD;
	   do_ov = True;
	   break;

       case 0x00A: // addc      (Add Carrying, p378)
	   DIP("addc%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
	   op = PPC32G_FLAG_OP_ADD;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x08A: // adde      (Add Extended, p379)
	   DIP("adde%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = rA + rB + XER[CA]
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra),
			     binop(Iop_Add32, mkexpr(Rb), mkexpr(xer_ca))) );
	   op = PPC32G_FLAG_OP_ADDE;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x0EA: // addme      (Add to Minus One Extended, p384)
	   if (Rb_addr != 0) {
	       vex_printf("dis_int_arith(PPC32)(addme,Rb_addr)\n");
	       return False;
	   }
	   DIP("addme%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = rA + XER[CA] - 1
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra),
			     binop(Iop_Sub32, mkexpr(xer_ca), mkU32(1)) ));
	   op = PPC32G_FLAG_OP_ADDME;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x0CA: // addze      (Add to Zero Extended, p385)
	   if (Rb_addr != 0) {
	       vex_printf("dis_int_arith(PPC32)(addze,Rb_addr)\n");
	       return False;
	   }
	   DIP("addze%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = rA + XER[CA]
	   assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(xer_ca)) );
	   op = PPC32G_FLAG_OP_ADDZE;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x1EB: // divw       (Divide Word, p421)
	   DIP("divw%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
           assign( Rd, binop(Iop_DivS32, mkexpr(Ra), mkexpr(Rb)) );
	   op = PPC32G_FLAG_OP_DIVW;
	   do_ov = True;
	   /* Note:
	      if (0x8000_0000 / -1) or (x / 0)
	      => Rd=undef, if(flag_Rc) CR7=undef, if(flag_OE) XER_OV=1
	      => But _no_ exception raised. */
	   break;

       case 0x1CB: // divwu      (Divide Word Unsigned, p422)
	   DIP("divwu%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
           assign( Rd, binop(Iop_DivU32, mkexpr(Ra), mkexpr(Rb)) );
	   op = PPC32G_FLAG_OP_DIVWU;
	   do_ov = True;
	   /* Note: ditto comment divw, for (x / 0) */
	   break;

       case 0x04B: // mulhw      (Multiply High Word, p541)
	   if (flag_OE != 0) {
	       vex_printf("dis_int_arith(PPC32)(mulhw,flag_OE)\n");
	       return False;
	   }
	   DIP("mulhw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( res64, binop(Iop_MullS32, mkexpr(Ra), mkexpr(Rb)) );
	   assign( Rd, unop(Iop_64HIto32, mkexpr(res64)) );
	   break;

       case 0x00B: // mulhwu     (Multiply High Word Unsigned, p542)
	   if (flag_OE != 0) {
	       vex_printf("dis_int_arith(PPC32)(mulhwu,flag_OE)\n");
	       return False;
	   }
	   DIP("mulhwu%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( res64, binop(Iop_MullU32, mkexpr(Ra), mkexpr(Rb)) );
	   assign( Rd, unop(Iop_64HIto32, mkexpr(res64)) );
	   break;

       case 0x0EB: // mullw      (Multiply Low Word, p545)
	   DIP("mullw%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   assign( res64, binop(Iop_MullU32, mkexpr(Ra), mkexpr(Rb)) );
	   assign( Rd, unop(Iop_64to32, mkexpr(res64)) );
	   op = PPC32G_FLAG_OP_MULLW;
	   do_ov = True;
	   break;

       case 0x068: // neg        (Negate, p547)
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
	   op = PPC32G_FLAG_OP_NEG;
	   do_ov = True;
	   break;
		   
       case 0x028: // subf       (Subtract From, p610)
	   DIP("subf%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = (log not)rA + rB + 1
	   assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
			     binop(Iop_Add32, mkexpr(Rb), mkU32(1))) );
	   op = PPC32G_FLAG_OP_SUBF;
	   do_ov = True;
	   break;

       case 0x008: // subfc      (Subtract from Carrying, p611)
	   DIP("subfc%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = (log not)rA + rB + 1
	   assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
			     binop(Iop_Add32, mkexpr(Rb), mkU32(1))) );
	   op = PPC32G_FLAG_OP_SUBFC;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x088: // subfe      (Subtract from Extended, p612)
	   DIP("subfe%s%s r%d,r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr, Rb_addr);
	   // rD = (log not)rA + rB + XER[CA]
	   assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
			     binop(Iop_Add32, mkexpr(Rb), mkexpr(xer_ca))) );
	   op = PPC32G_FLAG_OP_SUBFE;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x0E8: // subfme     (Subtract from Minus One Extended, p614)
	   if (Rb_addr != 0) {
	       vex_printf("dis_int_arith(PPC32)(subfme,Rb_addr)\n");
	       return False;
	   }
	   DIP("subfme%s%s r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr);
	   // rD = (log not)rA + XER[CA] - 1
	   assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
			     binop(Iop_Sub32, mkexpr(xer_ca), mkU32(1))) );
	   op = PPC32G_FLAG_OP_SUBFME;
	   do_ca = True;
	   do_ov = True;
	   break;

       case 0x0C8: // subfze     (Subtract from Zero Extended, p615)
	   if (Rb_addr != 0) {
	       vex_printf("dis_int_arith(PPC32)(subfze,Rb_addr)\n");
	       return False;
	   }
	   DIP("subfze%s%s r%d,r%d\n",
	       flag_OE ? "o" : "", flag_Rc ? "." : "",
	       Rd_addr, Ra_addr);
	   // rD = (log not)rA + XER[CA]
	   assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
			     mkexpr(xer_ca)) );
	   op = PPC32G_FLAG_OP_SUBFZE;
	   do_ca = True;
	   do_ov = True;
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

    if (do_ov && flag_OE) {
	vassert(op < PPC32G_FLAG_OP_NUMBER);
	setFlags_XER_OV_SO( op, mkexpr(Rd), arg1, arg2 );
    }
    if (do_ca) {
	vassert(op < PPC32G_FLAG_OP_NUMBER);
	setFlags_XER_CA( op, mkexpr(Rd), arg1, arg2 );
    }
    if (do_rc && flag_Rc) {
	setFlags_CR7( mkexpr(Rd) );
    }
    return True;
}



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
    IRTemp Ra    = newTemp(Ity_I32);
    IRTemp Rb    = newTemp(Ity_I32);
    IRTemp tmp   = newTemp(Ity_I32);
    IRTemp cr_f7 = newTemp(Ity_I32);
    IRTemp tst1  = newTemp(Ity_I1);
    IRTemp tst2  = newTemp(Ity_I1);
	
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
    case 0x0B: // cmpi (Compare Immediate, p398)
	DIP("cmpi crf%d,%u,r%d,0x%x\n", crfD, flag_L, Ra_addr, SIMM_16);
	EXTS_SIMM = extend_s_16to32(SIMM_16);
	assign( tst1, binop(Iop_CmpEQ32, mkU32(EXTS_SIMM), mkexpr(Ra)) );
	assign( tst2, binop(Iop_CmpLT32S, mkU32(EXTS_SIMM), mkexpr(Ra)) );
	break;

    case 0x0A: // cmpli (Compare Logical Immediate, p400)
	DIP("cmpli crf%d,%u,r%d,0x%x\n", crfD, flag_L, Ra_addr, UIMM_16);
	assign( tst1, binop(Iop_CmpEQ32, mkU32(UIMM_16), mkexpr(Ra)) );
	assign( tst2, binop(Iop_CmpLT32U, mkU32(UIMM_16), mkexpr(Ra)) );
	break;

    /* X Form */
    case 0x1F:
	if (b0 != 0) {
	    vex_printf("dis_int_cmp(PPC32)(0x1F,b0)\n");
	    return False;
	}

	switch (opc2) {
	case 0x000: // cmp (Compare, p397)
	    DIP("cmp crf%d,%u,r%d,r%d\n", crfD, flag_L,
		Ra_addr, Rb_addr);
	    assign( Rb, getIReg(Rb_addr) );
	    assign( tst1, binop(Iop_CmpEQ32, mkexpr(Rb), mkexpr(Ra)) );
	    assign( tst2, binop(Iop_CmpLT32S, mkexpr(Rb), mkexpr(Ra)) );
	    break;

        case 0x020: // cmpl (Compare Logical, p399)
	    DIP("cmpl crf%d,%u,r%d,r%d\n", crfD, flag_L,
		Ra_addr, Rb_addr);
	    assign( Rb, getIReg(Rb_addr) );
	    assign( tst1, binop(Iop_CmpEQ32, mkexpr(Rb), mkexpr(Ra)) );
	    assign( tst2, binop(Iop_CmpLT32U, mkexpr(Rb), mkexpr(Ra)) );
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

    assign( tmp, IRExpr_Mux0X( unop(Iop_1Uto8, mkexpr(tst1)),
			       IRExpr_Mux0X( unop(Iop_1Uto8, mkexpr(tst2)),
					     mkU32(8), mkU32(4) ),
			       mkU32(2) ));

    assign( cr_f7, binop(Iop_Or32, mkexpr(tmp),
			 getReg_bit(REG_XER, OFFBIT_XER_SO)) );
    putReg_field( REG_CR, 7-crfD, mkexpr(cr_f7) );
    return True;
}



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

    assign( Rs, getIReg(Ra_addr) );
    assign( Rb, getIReg(Ra_addr) );

    switch (opc1) {
    case 0x1C: // andi. (AND Immediate, p388)
	DIP("andi r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
	assign( Ra, binop(Iop_And32, mkexpr(Rs), mkU32(UIMM_16)) );
	putIReg( Ra_addr, mkexpr(Ra) );
	do_rc = True;
	flag_Rc = 1;
	break;

    case 0x1D: // andis. (AND Immediate Shifted, p389)
	DIP("andis r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
	assign( Ra, binop(Iop_And32, mkexpr(Rs), mkU32(UIMM_16 << 16)) );
	putIReg( Ra_addr, mkexpr(Ra) );
	do_rc = True;
	flag_Rc = 1;
	break;

    case 0x18: // ori (OR Immediate, p551)
	DIP("ori r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
	putIReg( Ra_addr, binop(Iop_Or32, mkexpr(Rs), mkU32(UIMM_16)) );
	break;

    case 0x19: // oris (OR Immediate Shifted, p552)
	DIP("oris r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
	putIReg( Ra_addr, binop(Iop_Or32, mkexpr(Rs), mkU32(UIMM_16 << 16)) );
	break;

    case 0x1A: // xori (XOR Immediate, p625)
	DIP("xori r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
	putIReg( Ra_addr, binop(Iop_Xor32, mkexpr(Rs), mkU32(UIMM_16)) );
	break;

    case 0x1B: // xoris (XOR Immediate Shifted, p626)
	DIP("xoris r%d,r%d,0x%x\n", Ra_addr, Rs_addr, UIMM_16);
	putIReg( Ra_addr, binop(Iop_Xor32, mkexpr(Rs), mkU32(UIMM_16 << 16)) );
	break;

    /* X Form */
    case 0x1F:
	switch (opc2) {
	case 0x01C: // and (AND, p386)
	    DIP("and%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign(Ra, binop(Iop_And32, mkexpr(Rs), mkexpr(Rb)));
	    break;

	case 0x03C: // andc (AND with Complement, p387)
	    DIP("andc%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign(Ra, binop(Iop_And32, mkexpr(Rs),
			     unop(Iop_Not32, mkexpr(Rb))));
	    break;

	case 0x01A: // cntlzw (Count Leading Zeros Word, p402)
	    if (Rb_addr!=0) {
		vex_printf("dis_int_logic(PPC32)(cntlzw,Rb_addr)\n");
		return False;
	    }
	    DIP("cntlzw%s r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr);

	    // Iop_Clz32 undefined for arg==0, so deal with that case:
	    assign(Ra, IRExpr_Mux0X(
		       unop(Iop_1Uto8, binop(Iop_CmpNE32,
					     mkexpr(Rs), mkU32(0))),
		       mkU32(32),
		       unop(Iop_Clz32, mkexpr(Rs)) ));
	    break;

	case 0x11C: // eqv (Equivalent, p427)
	    DIP("eqv%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign( Ra, unop(Iop_Not32, binop(Iop_Xor32,
					      mkexpr(Rs), mkexpr(Rb))) );
	    break;

	case 0x3BA: // extsb (Extend Sign Byte, p428)
	    if (Rb_addr!=0) {
		vex_printf("dis_int_logic(PPC32)(extsb,Rb_addr)\n");
		return False;
	    }
	    DIP("extsb%s r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr);
	    assign( Sign, binop(Iop_And32, mkU32(0x80), mkexpr(Rs)) );
	    assign( Ra, IRExpr_Mux0X(
			 unop(Iop_1Uto8, binop(Iop_CmpEQ32,
					       mkexpr(Sign), mkU32(0))),
			 binop(Iop_Or32,  mkU32(0xFFFFFF00), mkexpr(Rs)),
			 binop(Iop_And32, mkU32(0x000000FF), mkexpr(Rs)) ));
	    break;

	case 0x39A: // extsh (Extend Sign Half Word, p429)
	    if (Rb_addr!=0) {
		vex_printf("dis_int_logic(PPC32)(extsh,Rb_addr)\n");
		return False;
	    }
	    DIP("extsh%s r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr);
	    assign( Sign, binop(Iop_And32, mkU32(0x8000), mkexpr(Rs)) );
	    assign( Ra, IRExpr_Mux0X(
			 unop(Iop_1Uto8, binop(Iop_CmpEQ32,
					       mkexpr(Sign), mkU32(0))),
			 binop(Iop_Or32,  mkU32(0xFFFF0000), mkexpr(Rs)),
			 binop(Iop_And32, mkU32(0x0000FFFF), mkexpr(Rs)) ));
	    break;

	case 0x1DC: // nand (NAND, p546)
	    DIP("nand%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign( Ra, unop(Iop_Not32,
			     binop(Iop_And32, mkexpr(Rs), mkexpr(Rb))) );
	    break;

	case 0x07C: // nor (NOR, p548)
	    DIP("nor%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign( Ra, unop(Iop_Not32,
			     binop(Iop_Or32, mkexpr(Rs), mkexpr(Rb))) );
	    break;

	case 0x1BC: // or (OR, p549)
	    DIP("or%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign( Ra, binop(Iop_Or32, mkexpr(Rs), mkexpr(Rb)) );
	    break;

	case 0x19C: // orc  (OR with Complement, p550)
	    DIP("orc%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign( Ra, binop(Iop_Or32, mkexpr(Rs),
			      unop(Iop_Not32, mkexpr(Rb))) );
	    break;

	case 0x13C: // xor (XOR, p624)
	    DIP("xor%s r%d,r%d,r%d\n",
		flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
	    assign( Ra, binop(Iop_Xor32, mkexpr(Rs), mkexpr(Rb)) );
	    break;

	default:
	    vex_printf("dis_int_logic(PPC32)(opc2)\n");
	    return False;
	}

	putIReg( Ra_addr, mkexpr(Ra) );
	do_rc = True;
	break;

    default:
	vex_printf("dis_int_logic(PPC32)(opc1)\n");
	return False;
    }

    if (do_rc && flag_Rc) {
	setFlags_CR7( mkexpr(Ra) );
    }
    return True;
}



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

    UInt mask = MASK(MaskBegin, MaskEnd);
    IRTemp rot_amt = newTemp(Ity_I8);
    IRTemp Rs = newTemp(Ity_I32);
    IRTemp Ra = newTemp(Ity_I32);
    IRTemp Rb = newTemp(Ity_I32);
    
    assign( Rs, getIReg(Rs_addr) );
    assign( Rb, getIReg(Rb_addr) );


    switch (opc1) {
    case 0x14: // rlwimi (Rotate Left Word Immediate then Mask Insert, p561)
	DIP("rlwimi%s r%d,r%d,%d,%u,%u\n", flag_Rc ? "." : "",
	    Ra_addr, Rs_addr, sh_imm, MaskBegin, MaskEnd);
	// Ra = (ROTL(Rs, Imm) & mask) | (Ra & ~mask);
	assign( Ra, binop(Iop_Or32,
			  binop(Iop_And32, mkU32(mask),
				ROTL32(mkexpr(Rs), mkU8(sh_imm))),
			  binop(Iop_And32, getIReg(Ra_addr), mkU32(~mask))) );
	break;

    case 0x15: // rlwinm (Rotate Left Word Immediate then AND with Mask, p562)
	DIP("rlwinm%s r%d,r%d,%d,%u,%u\n", flag_Rc ? "." : "",
	    Ra_addr, Rs_addr, sh_imm, MaskBegin, MaskEnd);
	// Ra = ROTL(Rs, Imm) & mask
	assign( Ra, binop(Iop_And32, ROTL32(mkexpr(Rs),
					    mkU8(sh_imm)), mkU32(mask)) );
	break;

    case 0x17: // rlwnm (Rotate Left Word then AND with Mask, p564)
	DIP("rlwnm%s r%d,r%d,r%d,%u,%u\n", flag_Rc ? "." : "",
	    Ra_addr, Rs_addr, Rb_addr, MaskBegin, MaskEnd);
	// Ra = ROTL(Rs, Rb[0-4]) & mask
	assign( rot_amt, narrowTo(Ity_I8, binop(Iop_And32,
						mkexpr(Rb), mkU32(0x1F))) );
	assign( Ra, binop(Iop_And32, ROTL32(mkexpr(Rs),
					    mkexpr(rot_amt)), mkU32(mask)) );
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
    
    if (Ra_addr == 0) {
	assign( Ra_or_0, mkU32(0) );
    } else {
	assign( Ra_or_0, mkexpr(Ra) );
    }
    assign( EA_imm, binop(Iop_And32, mkexpr(Ra_or_0), mkU32(exts_d_imm)) );

    switch (opc1) {
    case 0x22: // lbz (Load B & Zero, p468)
	DIP("lbz r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, unop(Iop_8Uto32,
			       loadBE(Ity_I8, mkexpr(EA_imm))) );
	break;

    case 0x23: // lbzu (Load B & Zero with Update, p469)
	if (Ra_addr == 0 || Ra_addr == Rd_addr) {
	    vex_printf("dis_int_load(PPC32)(lbzu,Ra_addr|Rd_addr)\n");
	    return False;
	}
	DIP("lbzu r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, unop(Iop_8Uto32,
			       loadBE(Ity_I8, mkexpr(EA_imm))) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    case 0x2A: // lha (Load HW Algebraic, p485)
	DIP("lha r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, unop(Iop_16Sto32,
			       loadBE(Ity_I16, mkexpr(EA_imm))) );
	break;

    case 0x2B: // lhau (Load HW Algebraic with Update, p486)
	if (Ra_addr == 0 || Ra_addr == Rd_addr) {
	    vex_printf("dis_int_load(PPC32)(lhau,Ra_addr|Rd_addr)\n");
	    return False;
	}
	DIP("lhau r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, unop(Iop_16Sto32,
			       loadBE(Ity_I16, mkexpr(EA_imm))) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    case 0x28: // lhz (Load HW & Zero, p490)
	DIP("lhz r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, unop(Iop_16Sto32,
			       loadBE(Ity_I16, mkexpr(EA_imm))) );
	break;

    case 0x29: // lhzu (Load HW & and Zero with Update, p491)
	if (Ra_addr == 0 || Ra_addr == Rd_addr) {
	    vex_printf("dis_int_load(PPC32)(lhzu,Ra_addr|Rd_addr)\n");
	    return False;
	}
	DIP("lhzu r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, loadBE(Ity_I16, mkexpr(EA_imm)) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    case 0x20: // lwz (Load W & Zero, p504)
	DIP("lwz r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
	break;

    case 0x21: // lwzu (Load W & Zero with Update, p505))
	if (Ra_addr == 0 || Ra_addr == Rd_addr) {
	    vex_printf("dis_int_load(PPC32)(lwzu,Ra_addr|Rd_addr)\n");
	    return False;
	}
	DIP("lwzu r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    /* X Form */
    case 0x1F:
	if (b0 != 0) {
	    vex_printf("dis_int_load(PPC32)(Ox1F,b0)\n");
	    return False;
	}
	assign( EA_reg, binop(Iop_And32, mkexpr(Ra_or_0), mkexpr(Rb)) );

	switch (opc2) {
        case 0x077: // lbzux (Load B & Zero with Update Indexed, p470)
	    DIP("lbzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    if (Ra_addr == 0 || Ra_addr == Rd_addr) {
		vex_printf("dis_int_load(PPC32)(lwzux,Ra_addr|Rd_addr)\n");
		return False;
	    }
	    putIReg( Rd_addr, unop(Iop_8Uto32,
				   loadBE(Ity_I8, mkexpr(EA_reg))) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

        case 0x057: // lbzx (Load B & Zero Indexed, p471)
	    DIP("lbzx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    putIReg( Rd_addr, unop(Iop_8Uto32,
				   loadBE(Ity_I8, mkexpr(EA_reg))) );
	    break;

        case 0x177: // lhaux (Load HW Algebraic with Update Indexed, p487)
	    if (Ra_addr == 0 || Ra_addr == Rd_addr) {
		vex_printf("dis_int_load(PPC32)(lhaux,Ra_addr|Rd_addr)\n");
		return False;
	    }
	    DIP("lhaux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    putIReg( Rd_addr, unop(Iop_16Sto32,
				   loadBE(Ity_I16, mkexpr(EA_reg))) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

        case 0x157: // lhax (Load HW Algebraic Indexed, p488)
	    DIP("lhax r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    putIReg( Rd_addr, unop(Iop_16Sto32,
				   loadBE(Ity_I16, mkexpr(EA_reg))) );
	    break;

        case 0x137: // lhzux (Load HW & Zero with Update Indexed, p492)
	    if (Ra_addr == 0 || Ra_addr == Rd_addr) {
		vex_printf("dis_int_load(PPC32)(lhzux,Ra_addr|Rd_addr)\n");
		return False;
	    }
	    DIP("lhzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    putIReg( Rd_addr, unop(Iop_16Sto32,
				   loadBE(Ity_I16, mkexpr(EA_reg))) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

        case 0x117: // lhzx (Load HW & Zero Indexed, p493)
	    DIP("lhzx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    putIReg( Rd_addr, unop(Iop_16Sto32,
				   loadBE(Ity_I16, mkexpr(EA_reg))) );
	    break;

        case 0x037: // lwzux (Load W & Zero with Update Indexed, p506)
	    if (Ra_addr == 0 || Ra_addr == Rd_addr) {
		vex_printf("dis_int_load(PPC32)(lwzux,Ra_addr|Rd_addr)\n");
		return False;
	    }
	    DIP("lwzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	    putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_reg)) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

        case 0x017: // lwzx (Load W & Zero Indexed, p507)
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
    assign( Rs_8, narrowTo(Ity_I8, mkexpr(Rs)) );
    assign( Rs_16, narrowTo(Ity_I16, mkexpr(Rs)) );

    if (Ra_addr == 0) {
	assign( Ra_or_0, mkU32(0) );
    } else {
	assign( Ra_or_0, mkexpr(Ra) );
    }
    assign( EA_imm, binop(Iop_And32, mkexpr(Ra_or_0), mkU32(exts_d_imm)) );

    switch (opc1) {
    case 0x26: // stb (Store B, p576)
	DIP("stb r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	storeBE( mkexpr(EA_imm), mkexpr(Rs_8) );
	break;

    case 0x27: // stbu (Store B with Update, p577)
	if (Ra_addr == 0 ) {
	    vex_printf("dis_int_store(PPC32)(stbu,Ra_addr)\n");
	    return False;
	}
	DIP("stbu r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	storeBE( mkexpr(EA_imm), mkexpr(Rs_8) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    case 0x2C: // sth (Store HW, p595)
	DIP("sth r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	storeBE( mkexpr(EA_imm), mkexpr(Rs_16) );
	break;

    case 0x2D: // sthu (Store HW with Update, p597)
	if (Ra_addr == 0) {
	    vex_printf("dis_int_store(PPC32)(sthu,Ra_addr)\n");
	    return False;
	}
	DIP("sthu r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	assign( Rs_16, binop(Iop_And16, mkexpr(Rs), mkU16(0xFFFF)) );
	storeBE( mkexpr(EA_imm), mkexpr(Rs_16) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    case 0x24: // stw (Store W, p603)
	DIP("stw r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	storeBE( mkexpr(EA_imm), mkexpr(Rs) );
	break;

    case 0x25: // stwu (Store W with Update, p607)
	if (Ra_addr == 0) {
	    vex_printf("dis_int_store(PPC32)(stwu,Ra_addr)\n");
	    return False;
	}
	DIP("stwu r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	storeBE( mkexpr(EA_imm), mkexpr(Rs) );
	putIReg( Ra_addr, mkexpr(EA_imm) );
	break;

    /* X Form */
    case 0x1F:
	if (b0 != 0) {
	    vex_printf("dis_int_store(PPC32)(0x1F,b0)\n");
	    return False;
	}
	assign( EA_reg, binop(Iop_And32, mkexpr(Ra_or_0), mkexpr(Rb)) );

	switch (opc2) {
	case 0x0F7: // stbux (Store B with Update Indexed, p578)
	    if (Ra_addr == 0) {
		vex_printf("dis_int_store(PPC32)(stbux,Ra_addr)\n");
		return False;
	    }
	    DIP("stbux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
	    storeBE( mkexpr(EA_reg), mkexpr(Rs_8) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

	case 0x0D7: // stbx (Store B Indexed, p579)
	    DIP("stbx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
	    storeBE( mkexpr(EA_reg), mkexpr(Rs_8) );
	    break;

	case 0x1B7: // sthux (Store HW with Update Indexed, p598)
	    if (Ra_addr == 0) {
		vex_printf("dis_int_store(PPC32)(sthux,Ra_addr)\n");
		return False;
	    }
	    DIP("sthux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
	    storeBE( mkexpr(EA_reg), mkexpr(Rs_16) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

	case 0x197: // sthx (Store HW Indexed, p599)
	    DIP("sthx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
	    storeBE( mkexpr(EA_reg), mkexpr(Rs_16) );
	    break;

	case 0x0B7: // stwux (Store W with Update Indexed, p608)
	    if (Ra_addr == 0) {
		vex_printf("dis_int_store(PPC32)(stwux,Ra_addr)\n");
		return False;
	    }
	    DIP("stwux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
	    storeBE( mkexpr(EA_reg), mkexpr(Rs) );
	    putIReg( Ra_addr, mkexpr(EA_reg) );
	    break;

	case 0x097: // stwx (Store W Indexed, p609)
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

    if (Ra_addr == 0) {
	assign( EA, binop(Iop_And32, mkU32(0), mkU32(exts_d_imm)) );
    } else {
	assign( Ra, getIReg(Ra_addr) );
	assign( EA, binop(Iop_And32, mkexpr(Ra), mkU32(exts_d_imm)) );
    }

    switch (opc1) {
    case 0x2E: // lmw (Load Multiple Word, p494)
	if (Ra_addr >= reg_idx) {
	    vex_printf("dis_int_ldst_mult(PPC32)(lmw,Ra_addr)\n");
	    return False;
	}
	DIP("lmw r%d,%u(r%d)\n", Rd_addr, d_imm, Ra_addr);
	for (reg_idx = Rd_addr; reg_idx<=31; reg_idx++) {
	    putIReg( reg_idx,
		     loadBE(Ity_I32, binop(Iop_Add32, mkexpr(EA),
					   mkU32(offset))) );
	    offset +=4;
	}
	break;

    case 0x2F: // stmw (Store Multiple Word, p600)
	DIP("stmw r%d,%u(r%d)\n", Rs_addr, d_imm, Ra_addr);
	for (reg_idx = Rs_addr; reg_idx<=31; reg_idx++) {
	    storeBE( binop(Iop_Add32, mkexpr(EA), mkU32(offset)),
		     getIReg(reg_idx) );
	    offset +=4;
	}
	break;

    default:
	vex_printf("dis_int_ldst_mult(PPC32)(opc1)\n");
	return False;
    }
    return True;
}



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
    case 0x255: // lswi (Load String Word Immediate, p495)
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
	DIP("lswi r%d,r%d,%u\n", Rd_addr, Ra_addr, NumBytes);

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

    case 0x215: // lswx (Load String Word Indexed, p497)
	DIP("lswx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	return False;

    case 0x2D5: // stswi (Store String Word Immediate, p601)
	DIP("stswi r%d,r%d,%u\n", Rs_addr, Ra_addr, NumBytes);
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

    case 0x295: // stswx (Store String Word Indexed, p602)
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
	assign( ctr_0, unop(Iop_32to1, IRExpr_Get(OFFB_CTR, Ity_I32)) );
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
    IRTemp tmp = newTemp(Ity_I1);
    IRTemp cr_bi = newTemp(Ity_I32);
    
    if (BO >> 4) {
	assign( ok, mkU1(1) );
    } else {
	// ok = (CR[31-BI] == BO[3])
	assign( cr_bi, getReg_masked( REG_CR, (0x80000000 >> BI)) );
	assign( tmp, binop(Iop_CmpNE32, mkU32(0), mkexpr(cr_bi)) );

	if ((BO >> 3) & 1) {
	    assign( ok, mkexpr(tmp) );
	} else {
	    assign( ok, unop(Iop_Not1, mkexpr(tmp)) );
	}
    }
    return mkexpr(ok);
}



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

    UInt exts_BD = extend_s_24to32(BD << 2);
    UInt exts_LI = extend_s_24to32(LI_24 << 2);

    Addr32 nia = 0;

    IRTemp ctr       = newTemp(Ity_I32);
    IRTemp lr        = newTemp(Ity_I32);
    IRTemp ir_nia    = newTemp(Ity_I32);
    IRTemp do_branch = newTemp(Ity_I32);
    IRTemp ctr_ok    = newTemp(Ity_I1);
    IRTemp cond_ok   = newTemp(Ity_I1);

    assign( ctr, IRExpr_Get(OFFB_CTR, Ity_I32) );

//    vex_printf("disInstr(ppc32): In: 0x%8x, %,031b\n", theInstr, theInstr );
//    vex_printf("disInstr(ppc32): LI: %,039b\n", LI_24);
//    vex_printf("disInstr(ppc32): LI: %,039b\n", LI_24 << 2);
//    vex_printf("disInstr(ppc32): LI: %,031b\n", extend_s_24to32(LI_24 << 2));

#if 1
    /* Hack to pass through code that just wants to read the PC */
    if (theInstr == 0x429F0005) {
	DIP("bcl 0x%x, 0x%x,\n", BO, BI);
	stmt( IRStmt_Put( OFFB_LR, mkU32(guest_cia_curr_instr + 4)) );
	return True;
    }
    // 
#endif
    
    switch (opc1) {
    case 0x12: // b     (Branch, p390)
//	DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", LI_24);
	nia = exts_LI;
	if (!flag_AA) {
	    nia += guest_cia_curr_instr;
	}
	if (flag_LK) {
	    stmt( IRStmt_Put( OFFB_LR, mkU32(guest_cia_curr_instr+4) ));
	}

	irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
	irbb->next     = mkU32(nia);
	DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", nia);
	break;

    case 0x10: // bc    (Branch Conditional, p391)
	DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
	    flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, BD);

	if (!(BO & 0x4)) {
	    stmt( IRStmt_Put(OFFB_CTR, binop(Iop_Sub32,
					     mkexpr(ctr), mkU32(1))) );
	}
	assign( ctr_ok, branch_ctr_ok( BO ) );
	assign( cond_ok, branch_cond_ok( BO, BI ) );

	assign( do_branch, binop(Iop_And32,
				 unop(Iop_1Uto32, mkexpr(ctr_ok)),
				 unop(Iop_1Uto32, mkexpr(cond_ok))) );
	nia = exts_BD;
	if (!flag_AA) {
	    nia += guest_cia_curr_instr;
	}

	if (flag_LK) {
	    assign( lr, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(do_branch)),
				      IRExpr_Get(OFFB_LR, Ity_I32),
				      mkU32(guest_cia_curr_instr + 4)));
	    stmt( IRStmt_Put( OFFB_LR, mkexpr(lr) ));
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
        case 0x210: // bcctr (Branch Cond. to Count Register, p393) 
	    if ((BO & 0x4) == 0) { // "decrement and test CTR" option invalid
		vex_printf("dis_int_branch(PPC32)(bcctr,BO)\n");
		return False;
	    }
	    DIP("bcctr%s 0x%x, 0x%x,\n", flag_LK ? "l" : "", BO, BI);

	    assign( cond_ok, branch_cond_ok( BO, BI ) );

	    assign( ir_nia, binop(Iop_And32, mkU32(0xFFFFFFFC), mkexpr(ctr)) );

	    if (flag_LK) {
		assign( lr, IRExpr_Mux0X( unop(Iop_1Uto8, mkexpr(cond_ok)),
					  IRExpr_Get(OFFB_LR, Ity_I32),
					  mkU32(guest_cia_curr_instr + 4)));
		stmt( IRStmt_Put( OFFB_LR, mkexpr(lr) ));
	    }

	    stmt( IRStmt_Exit( unop(Iop_Not1, mkexpr(cond_ok)),
			       Ijk_Boring,
			       IRConst_U32(guest_cia_curr_instr + 4) ));

	    irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
	    irbb->next     = mkexpr(ir_nia);
	    break;

        case 0x010: // bclr (Branch Cond. to Link Register, p395) 
	    DIP("bclr%s 0x%x, 0x%x,\n", flag_LK ? "l" : "", BO, BI);

	    if (!(BO & 0x4)) {
		stmt( IRStmt_Put(OFFB_CTR, binop(Iop_Sub32,
						 mkexpr(ctr), mkU32(1))) );
	    }

	    assign( ctr_ok, branch_ctr_ok(BO) );
	    assign( cond_ok, branch_cond_ok(BO, BI) );

	    assign( do_branch, binop(Iop_And32,
				     unop(Iop_1Uto32, mkexpr(ctr_ok)),
				     unop(Iop_1Uto32, mkexpr(cond_ok))) );

	    assign( ir_nia, binop(Iop_And32,
				  IRExpr_Get(OFFB_LR, Ity_I32),
				  mkU32(0xFFFFFFFC)) );
	    if (flag_LK) {
		assign( lr, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(do_branch)),
					  IRExpr_Get(OFFB_LR, Ity_I32),
					  mkU32(guest_cia_curr_instr + 4)) );
		stmt( IRStmt_Put( OFFB_LR, mkexpr(lr) ));
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



static Bool dis_syslink ( UInt theInstr, DisResult *whatNext )
{
    if (theInstr != 0x44000002) {
	vex_printf("dis_int_syslink(PPC32)(theInstr)\n");
	return False;
    }

    // sc  (System Call, p565)
    DIP("sc\n");

    /* It's important that all ArchRegs carry their up-to-date value
       at this point.  So we declare an end-of-block here, which
       forces any TempRegs caching ArchRegs to be flushed. */
    irbb->next     = mkU32( guest_cia_curr_instr + 4 );
    irbb->jumpkind = Ijk_Syscall;

    *whatNext = Dis_StopHere;
    return True;
}

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
    IRTemp cr_f7 = newTemp(Ity_I32);

    switch (opc1) {
    /* XL-Form */
    case 0x13:	// isync (Instruction Synchronize, p467)
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
        case 0x356: // eieio (Enforce In-Order Execution of I/O, p425)
	    if (b11to25 != 0 || b0 != 0) {
		vex_printf("dis_int_memsync(PPC32)(eiei0,b11to25|b0)\n");
		return False;
	    }
	    DIP("eieio\n");
	    return False;

        case 0x014: // lwarx (Load Word and Reserve Indexed, p500)
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

        case 0x096: // stwcx. (Store Word Conditional Indexed, p605)
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
	    assign( cr_f7, binop(Iop_Or32, mkU32(2), 
				 getReg_bit(REG_XER, OFFBIT_XER_SO)) );
	    putReg_field( REG_CR, 7, mkexpr(cr_f7) );
	    break;

        case 0x256: // sync (Synchronize, p616)
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

    UInt op    = PPC32G_FLAG_OP_NUMBER;
    Bool do_ca = False;

    IRTemp sh_amt = newTemp(Ity_I8);
    IRTemp sign   = newTemp(Ity_I32);
    IRTemp rb_b5  = newTemp(Ity_I32);
    IRTemp sext   = newTemp(Ity_I32);
    IRTemp Rs     = newTemp(Ity_I32);
    IRTemp Rs_sh  = newTemp(Ity_I32);
    IRTemp Ra     = newTemp(Ity_I32);
    IRTemp Rb     = newTemp(Ity_I32);
    IRTemp mask   = newTemp(Ity_I32);

    assign( Rs, getIReg(Rs_addr) );
    assign( Rb, getIReg(Rb_addr) );

    if (opc1 == 0x1F) {
	switch (opc2) {
        case 0x018: // slw (Shift Left Word, p569)
	    DIP("slw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Rb_addr);
	    assign( sh_amt, binop(Iop_And8, mkU8(0x1F),
				    unop(Iop_32to8, mkexpr(Rb))) );
	    assign( Ra, binop(Iop_Shl32, mkexpr(Rs), mkexpr(sh_amt)) );
	    break;

        case 0x318: // sraw (Shift Right Algebraic Word, p572)
	    DIP("sraw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Rb_addr);

	    assign( sh_amt, binop(Iop_And8, mkU8(0x1F),
				    unop(Iop_32to8, getIReg(Rb_addr))) );
	    // Rs_shift = Rs >> sh_amt
	    assign( Rs_sh, binop(Iop_Shr32, mkexpr(Rs), mkexpr(sh_amt)) );
	    // rb_b5 = Rb[5]
	    assign( rb_b5, binop(Iop_And32, mkexpr(Rb), mkU32(1<<5)) );
	    // sign = Rs[31]
	    assign( sign, binop(Iop_Shr32, mkexpr(Rs), mkU8(31)) );
	    // mask = rb_b5==0 ? (-1 >> sh_amt) : 0
	    assign( mask, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(rb_b5)),
					binop(Iop_Shr32, mkU32(-1), mkexpr(sh_amt)),
					mkU32(0) ));
	    // sign_ext = sign==0 ? 0 : ~mask
	    assign( sext, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(sign)),
					mkU32(0),
					unop(Iop_Not32, mkexpr(mask)) ));

	    // Ra = (rb_b5 == 0 ? Rs_sh : 0) | sext
	    assign( Ra, binop(Iop_Or32, mkexpr(sext),
			      IRExpr_Mux0X( unop(Iop_32to8, mkexpr(rb_b5)),
					    mkU32(0), mkexpr(Rs_sh))) );
	    putIReg( Ra_addr, mkexpr(Ra) );
	    op = PPC32G_FLAG_OP_SRAW;
	    do_ca = True;
	    break;

        case 0x338: // srawi (Shift Right Algebraic Word Immediate, p573)
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
	    putIReg( Ra_addr, mkexpr(Ra) );
	    op = PPC32G_FLAG_OP_SRAWI;
	    do_ca = True;
	    break;

        case 0x218: // srw (Shift Right Word, p575)
	    DIP("srw%s r%d,r%d,r%d\n", flag_Rc ? "." : "",
		Ra_addr, Rs_addr, Rb_addr);
	    assign( sh_amt, binop(Iop_And8, mkU8(0x1F),
				    unop(Iop_32to8, getIReg(Rb_addr))) );
	    assign( Rs_sh, binop(Iop_Shr32, mkexpr(Rs), mkexpr(sh_amt)) );
	    assign( rb_b5, binop(Iop_And32, mkexpr(Rb), mkU32(1<<5)) );
	    assign( Ra, IRExpr_Mux0X( unop(Iop_32to8, mkexpr(rb_b5)),
				      mkU32(0), mkexpr(Rs_sh) ));
	    putIReg( Ra_addr, mkexpr(Ra) );
	    break;

        default:
	    vex_printf("dis_int_shift(PPC32)(opc2)\n");
	    return False;
	}
    } else {
	vex_printf("dis_int_shift(PPC32)(opc1)\n");
	return False;
    }

    if (do_ca) {
	vassert(op < PPC32G_FLAG_OP_NUMBER);
	setFlags_XER_CA( op, mkexpr(Ra), mkexpr(Rs), mkexpr(Rb) );
    }
    if (flag_Rc) {
	setFlags_CR7( mkexpr(Ra) );
    }
    return True;
}



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
    case 0x316: // lhbrx (Load Half Word Byte-Reverse Indexed, p489)
	DIP("lhbrx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
	assign( byte0, loadBE(Ity_I8, mkexpr(EA)) );
	assign( byte1, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(1))) );
	assign( Rd, binop(Iop_Or32,
			  binop(Iop_Shl32, mkexpr(byte1), mkU8(8)),
			  mkexpr(byte0)) );
	putIReg( Rd_addr, mkexpr(Rd));
	break;

    case 0x216: // lwbrx (Load Word Byte-Reverse Indexed, p503)
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

    case 0x396: // sthbrx (Store Half Word Byte-Reverse Indexed, p596)
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

    case 0x296: // stwbrx (Store Word Byte-Reverse Indexed, p604)
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
    UInt  mask;
    UChar i;

    IRTemp xer_f7 = newTemp(Ity_I32);
    IRTemp Rs     = newTemp(Ity_I32);

    assign( Rs, getIReg(Rs_addr) );

    if (opc1 != 0x1F || b0 != 0) {
	vex_printf("dis_proc_ctl(PPC32)(opc1|b0)\n");
	return False;
    }

    switch (opc2) {
    /* X-Form */
    case 0x200: // mcrxr (Move to Condition Register from XER, p510)
	if (b21to22 != 0 || b11to20 != 0) {
	    vex_printf("dis_proc_ctl(PPC32)(mcrxr,b21to22|b11to20)\n");
	    return False;
	}
	DIP("mcrxr crf%d\n", crfD);

	// CR[7-crfD] = XER[28-31]
	assign( xer_f7, binop(Iop_Shr32,
			      getReg_masked( REG_XER, 0xF0000000 ),
			      mkU8(28)) );
	putReg_field( REG_CR, 7-crfD, mkexpr(xer_f7) );

	// Clear XER[28 - 31]
	putReg_field( REG_XER, 7, mkU32(0) );
	break;

    case 0x013: // mfcr (Move from Condition Register, p511)
	if (b11to20 != 0) {
	    vex_printf("dis_proc_ctl(PPC32)(mfcr,b11to20)\n");
	    return False;
	}
	DIP("mfcr crf%d\n", Rd_addr);
	putIReg( Rd_addr, getReg( REG_CR ) );
	break;

    /* XFX-Form */
    case 0x153: // mfspr (Move from Special-Purpose Register, p514)
	DIP("mfspr r%d,0x%x\n", Rd_addr, SPR_flipped);

	switch (SPR_flipped) {  // Choose a register...
	case 0x1:            // XER
	    putIReg( Rd_addr, getReg( REG_XER ) );
	    break;
	case 0x8:            // LR
	    putIReg( Rd_addr, getReg( REG_LR ) );
	    break;
	case 0x9:            // CTR
	    putIReg( Rd_addr, getReg( REG_CTR ) );
	    break;
	    
	case 0x012: case 0x013: case 0x016:
	case 0x019: case 0x01A: case 0x01B:
	case 0x110: case 0x111:	case 0x112: case 0x113:
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
	    vex_printf("dis_proc_ctl(PPC32)(mfspr,SPR)\n");
	    return False;
	}
	break;

    case 0x173: // mftb (Move from Time Base, p521)
	DIP("mftb r%d,0x%x\n", Rd_addr, TBR);
	return False;

    case 0x090: // mtcrf (Move to Condition Register Fields, p523)
	if (b11 != 0 || b20 != 0) {
	    vex_printf("dis_proc_ctl(PPC32)(mtcrf,b11|b20)\n");
	    return False;
	}
	DIP("mtcrf 0x%x,r%d\n", CRM, Rs_addr);
	mask=0;
	for (i=0; i<8; i++) {
	    if (CRM & (1<<i)) {
		mask = mask | (0xF << (7-i)*4);
	    }
	}
	putReg_masked( REG_CR, mkexpr(Rs), mask );
	break;

    case 0x1D3: // mtspr (Move to Special-Purpose Register, p530)
	DIP("mtspr 0x%x,r%d\n", SPR_flipped, Rs_addr);

	switch (SPR_flipped) {  // Choose a register...
	case 0x1:            // XER
	    putReg( REG_XER, mkexpr(Rs) );
	    break;
	case 0x8:            // LR
	    putReg( REG_LR, mkexpr(Rs) );
	    break;
	case 0x9:            // CTR
	    putReg( REG_CTR, mkexpr(Rs) );
	    break;

	case 0x012: case 0x013: case 0x016:
	case 0x019: case 0x01A: case 0x01B:
	case 0x110: case 0x111: case 0x112: case 0x113:
//	case 0x118: // 64bit only
	case 0x11A: case 0x11C: case 0x11D:
	case 0x210: case 0x211: case 0x212: case 0x213:
	case 0x214: case 0x215: case 0x216: case 0x217:
	case 0x218: case 0x219: case 0x21A: case 0x21B:
	case 0x21C: case 0x21D: case 0x21E: case 0x21F:
	case 0x3F5:
	    vex_printf("dis_proc_ctl(PPC32)(mtspr) - supervisor level op\n");
	    return False;

	default:
	    vex_printf("dis_proc_ctl(PPC32)(mtspr,SPR)\n");
	    return False;
	}
	break;

    default:
	vex_printf("dis_proc_ctl(PPC32)(opc2)\n");
	return False;
    }
    return True;
}


static Bool dis_cache_manage ( UInt theInstr )
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
    case 0x2F6: // dcba (Data Cache Block Allocate, p411)
	DIP("dcba r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged dcba\n");
	break;

    case 0x056: // dcbf (Data Cache Block Flush, p413)
	DIP("dcbf r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged dcbf\n");
	break;

    case 0x036: // dcbst (Data Cache Block Store, p415)
	DIP("dcbst r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged dcbst\n");
	break;

    case 0x116: // dcbt (Data Cache Block Touch, p416)
	DIP("dcbt r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged dcbt\n");
	break;

    case 0x0F6: // dcbtst (Data Cache Block Touch for Store, p417)
	DIP("dcbtst r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged dcbtst\n");
	break;

    case 0x3F6: // dcbz (Data Cache Block Clear to Zero, p418)
	DIP("dcbz r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged dcbz\n");
	break;

    case 0x3D6: // icbi (Instruction Cache Block Invalidate, p466)
	DIP("icbi r%d,r%d\n", Ra_addr, Rb_addr);
	if (1) vex_printf("vex ppc32->IR: kludged icbi\n");
	break;

    default:
	vex_printf("dis_cache_manage(PPC32)(opc2)\n");
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

   theInstr = getUIntBigendianly( (UChar*)(&guest_code[delta]) );

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


   opc1 = toUChar((theInstr >> 26) & 0x3F);   /* theInstr[26:31] */
   opc2 = (theInstr >> 1 ) & 0x3FF;           /* theInstr[1:10]  */

#if 0
   vex_printf("\ndisInstr(ppc32): instr:   0x%x\n", theInstr);
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

   if (theInstr == 0x7C0042A6) {
       // CAB: what's this?
       DIP("Invalid instruction! Would be 'mfspr 0,256'.  Passing through for now...\n");
       goto decode_success;
   }

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
       if (dis_int_arith(theInstr))
	   break;
       goto decode_failure;

   /*
     Integer Compare Instructions
   */
   case 0x0B: // cmpi
   case 0x0A: // cmpli
       if (dis_int_cmp(theInstr))
	   break;
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
       if (dis_int_logic(theInstr))
	   break;
       goto decode_failure;

   /*
     Integer Rotate Instructions
   */
   case 0x14: // rlwimi
   case 0x15: // rlwinm
   case 0x17: // rlwnm
       if (dis_int_rot(theInstr))
	   break;
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
       if (dis_int_load(theInstr))
	   break;
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
       if (dis_int_store(theInstr))
	   break;
       goto decode_failure;

   /*
     Integer Load and Store Multiple Instructions
   */
   case 0x2E: // lmw
   case 0x2F: // stmw
       if (dis_int_ldst_mult(theInstr))
	   break;
       goto decode_failure;

   /*
      Branch Instructions
   */
   case 0x12: // b
   case 0x10: // bc
       if (dis_branch(theInstr, &whatNext))
	   break;
       goto decode_failure;

   /*
     System Linkage Instructions
   */
   case 0x11: // sc
       if (dis_syslink(theInstr, &whatNext))
	   break;
       goto decode_failure;

   /*
     Trap Instructions
   */
   case 0x03: // twi
       DIP("trap op (twi) => not implemented\n");
       goto decode_failure;

   /*
     Floating Point Ops
   */
   case 0x30:
   case 0x31:
   case 0x32:
   case 0x33:
   case 0x34:
   case 0x35:
   case 0x36:
   case 0x37:
   case 0x3B:
   case 0x3F:
       DIP("Floating Point Op => not implemented\n");
       break;
//       goto decode_failure;


   case 0x13:
       switch (opc2) {

       /*
	 Condition Register Logical Instructions
       */
       case 0x101: // crand
       case 0x081: // crandc
       case 0x121: // creqv
       case 0x0E1: // crnand
       case 0x021: // crnor
       case 0x1C1: // cror
       case 0x1A1: // crorc
       case 0x0C1: // crxor
       case 0x000: // mcrf
	   DIP("condition register logical op => not implemented\n");
	   goto decode_failure;

       /*
	 Branch Instructions
       */
       case 0x210: // bcctr
       case 0x010: // bclr
	   if (dis_branch(theInstr, &whatNext))
	       break;
	   goto decode_failure;

       /*
	 Memory Synchronization Instructions
       */
       case 0x096: // isync
	   if (dis_memsync(theInstr))
	       break;
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
       case 0x1EB: // divw
       case 0x1CB: // divwu
       case 0x04B: // mulhw
       case 0x00B: // mulhwu
       case 0x0EB: // mullw
       case 0x068: // neg
       case 0x028: // subf
       case 0x008: // subfc
       case 0x088: // subfe
       case 0x0E8: // subfme
       case 0x0C8: // subfze
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
	   if (dis_int_cmp(theInstr))
	       break;
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
       case 0x1DC: // nand
       case 0x07C: // nor
       case 0x1BC: // or
       case 0x19C: // orc
       case 0x13C: // xor
	   if (dis_int_logic(theInstr))
	       break;
	   goto decode_failure;

       /*
	 Integer Shift Instructions
       */
       case 0x018: // slw
       case 0x318: // sraw
       case 0x338: // srawi
       case 0x218: // srw
	   if (dis_int_shift(theInstr))
	       break;
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
	   if (dis_int_load(theInstr))
	       break;
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
	   if (dis_int_store(theInstr))
	       break;
	   goto decode_failure;

       /*
	 Integer Load and Store with Byte Reverse Instructions
       */
       case 0x316: // lhbrx
       case 0x216: // lwbrx
       case 0x396: // sthbrx
       case 0x296: // stwbrx
	   if (dis_int_ldst_rev(theInstr))
	       break;
	   goto decode_failure;

       /*
	 Integer Load and Store String Instructions
       */
       case 0x255: // lswi
       case 0x215: // lswx
       case 0x2D5: // stswi
       case 0x295: // stswx
	   if (dis_int_ldst_str(theInstr))
	       break;
	   goto decode_failure;

       /*
	 Memory Synchronization Instructions
       */
       case 0x356: // eieio
       case 0x014: // lwarx
       case 0x096: // stwcx.
       case 0x256: // sync
	   if (dis_memsync(theInstr))
	       break;
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
	   if (dis_proc_ctl(theInstr))
	       break;
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
	   if (dis_cache_manage(theInstr))
	       break;
	   goto decode_failure;

       /*
	 External Control Instructions
	 Rc=0
       */
       case 0x136: // eciwx
       case 0x1B6: // ecowx
	   DIP("external control op => not implemented\n");
	   goto decode_failure;

       /*
	 Trap Instructions
       */
       case 0x004: // tw
	   DIP("trap op (tw) => not implemented\n");
	   goto decode_failure;

       /*
	 Floating Point Ops
       */
       case 0x217:
       case 0x237:
       case 0x257:
       case 0x277:
       case 0x297:
       case 0x2B7:
       case 0x2D7:
       case 0x2F7:
       case 0x3D7:
	   DIP("Floating Point Op => not implemented\n");
	   break;
//	   goto decode_failure;

       /*
	 AltiVec instructions
       */
       case 0x0E7: // stvx
	   DIP("Altivec op (stvx) => not implemented\n");
	   goto decode_success;

       default:
	   goto decode_failure;
       }
       break;

   default:
   decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(ppc32): unhandled instruction: "
              "0x%x\n", theInstr);

#if 1
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
   stmt( IRStmt_Put( OFFB_CIA, mkU32(guest_cia_curr_instr) ) );
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
