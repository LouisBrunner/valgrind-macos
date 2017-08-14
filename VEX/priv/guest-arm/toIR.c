
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-arm/toIR.c) is                              ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
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

/* The IRSB* into which we're generating code. */
static IRSB* irsb;


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
                            /*IN*/  Long    delta, 
                            /*OUT*/ Int*    size,
                            /*OUT*/ Addr64* whereNext );


/* This is the main (only, in fact) entry point for this module. */

/* Disassemble a complete basic block, starting at guest_pc_start, and
   dumping the IR into global irsb.  Returns the size, in bytes, of
   the basic block.  
*/
IRSB* bbToIR_ARM ( UChar*           armCode, 
                   Addr64           guest_pc_start, 
                   VexGuestExtents* vge,
                   Bool             (*byte_accessible)(Addr64),
                   Bool             (*chase_into_ok)(Addr64),
                   Bool             host_bigendian,
                   VexArchInfo*     archinfo_guest )
{
   Long       delta;
   Int        i, n_instrs, size, first_stmt_idx;
   Addr64     guest_next;
   Bool       resteerOK;
   DisResult  dres;
   static Int n_resteers = 0;
   Int        d_resteers = 0;

   /* check sanity .. */
   vassert(vex_control.guest_max_insns >= 1);
   vassert(vex_control.guest_max_insns < 500);
   vassert(vex_control.guest_chase_thresh >= 0);
   vassert(vex_control.guest_chase_thresh < vex_control.guest_max_insns);

   vassert(archinfo_guest->hwcaps == 0);

   /* Start a new, empty extent. */
   vge->n_used  = 1;
   vge->base[0] = guest_pc_start;
   vge->len[0]  = 0;

   /* Set up globals. */
   host_is_bigendian = host_bigendian;
   guest_code        = armCode;
   guest_pc_bbstart  = (Addr32)guest_pc_start;
   irsb              = emptyIRSB();

   vassert((guest_pc_start >> 32) == 0);

   /* Delta keeps track of how far along the armCode array we
      have so far gone. */
   delta             = 0;
   n_instrs          = 0;

   while (True) {
      vassert(n_instrs < vex_control.guest_max_insns);

      guest_next = 0;
      resteerOK = toBool(n_instrs < vex_control.guest_chase_thresh);
      first_stmt_idx = irsb->stmts_used;

      if (n_instrs > 0) {
         /* for the first insn, the dispatch loop will have set
            R15, but for all the others we have to do it ourselves. */
         stmt( IRStmt_Put( OFFB_R15, mkU32(toUInt(guest_pc_bbstart + delta))) );
      }

      dres = disInstr( resteerOK, chase_into_ok, 
                       delta, &size, &guest_next );

      /* Print the resulting IR, if needed. */
      if (vex_traceflags & VEX_TRACE_FE) {
         for (i = first_stmt_idx; i < irsb->stmts_used; i++) {
            vex_printf("              ");
            ppIRStmt(irsb->stmts[i]);
            vex_printf("\n");
         }
      }
   
      if (dres == Dis_StopHere) {
         vassert(irsb->next != NULL);
         if (vex_traceflags & VEX_TRACE_FE) {
            vex_printf("              ");
            vex_printf( "goto {");
            ppIRJumpKind(irsb->jumpkind);
            vex_printf( "} ");
            ppIRExpr( irsb->next );
            vex_printf( "\n");
         }
      }

      delta += size;
      vge->len[vge->n_used-1] = toUShort(vge->len[vge->n_used-1] + size);
      n_instrs++;
      DIP("\n");

      vassert(size > 0 && size <= 18);
      if (!resteerOK) 
         vassert(dres != Dis_Resteer);
      if (dres != Dis_Resteer) 
         vassert(guest_next == 0);

      switch (dres) {
      case Dis_Continue:
         vassert(irsb->next == NULL);
         if (n_instrs < vex_control.guest_max_insns) {
            /* keep going */
         } else {
            irsb->next = mkU32(toUInt(guest_pc_start+delta));
            return irsb;
         }
         break;
      case Dis_StopHere:
         vassert(irsb->next != NULL);
         return irsb;
      case Dis_Resteer:
         vpanic("bbToIR_ARM: Dis_Resteer: fixme");
         /* need to add code here to start a new extent ... */
         vassert(irsb->next == NULL);
         /* figure out a new delta to continue at. */
         vassert(chase_into_ok(guest_next));
         delta = guest_next - guest_pc_start;
         n_resteers++;
         d_resteers++;
         if (0 && (n_resteers & 0xFF) == 0)
            vex_printf("resteer[%d,%d] to %p (delta = %lld)\n",
                       n_resteers, d_resteers,
                       ULong_to_Ptr(guest_next), delta);
         break;
      }
   }
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- ARM insn stream.                                     ---*/
/*------------------------------------------------------------*/

/* Add a statement to the list held by "irsb". */
static void stmt ( IRStmt* st )
{
   addStmtToIRSB( irsb, st );
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp( irsb->tyenv, ty );
}

#if 0
/* Bomb out if we can't handle something. */
__attribute__ ((noreturn))
static void unimplemented ( Char* str )
{
   vex_printf("armToIR: unimplemented feature\n");
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

static UInt extend_s_24to32 ( UInt x )
{
   return (UInt)((((Int)x) << 8) >> 8);
}

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
   default: vpanic("getUDisp(ARM)");
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
   default: vpanic("getSDisp(ARM)");
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
   default: vpanic("szToITy(ARM)");
   }
}
#endif

static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 16);

   vassert(!host_is_bigendian);   //TODO: is this necessary?
   // jrs: probably not; only matters if we reference sub-parts
   // of the arm registers, but that isn't the case
   switch (archreg) {
   case  0: return offsetof(VexGuestARMState,guest_R0);
   case  1: return offsetof(VexGuestARMState,guest_R1);
   case  2: return offsetof(VexGuestARMState,guest_R2);
   case  3: return offsetof(VexGuestARMState,guest_R3);
   case  4: return offsetof(VexGuestARMState,guest_R4);
   case  5: return offsetof(VexGuestARMState,guest_R5);
   case  6: return offsetof(VexGuestARMState,guest_R6);
   case  7: return offsetof(VexGuestARMState,guest_R7);
   case  8: return offsetof(VexGuestARMState,guest_R8);
   case  9: return offsetof(VexGuestARMState,guest_R9);
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
   stmt( IRStmt_WrTmp(dst, e) );
}

static void storeLE ( IRExpr* addr, IRExpr* data )
{
   stmt( IRStmt_Store(Iend_LE,addr,data) );
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
   return IRExpr_RdTmp(tmp);
}

static IRExpr* mkU8 ( UChar i )
{
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
   vpanic("mkU(ARM)");
}
#endif

static IRExpr* loadLE ( IRType ty, IRExpr* data )
{
   return IRExpr_Load(Iend_LE,ty,data);
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
   vpanic("mkWidenOp(ARM,guest)");
}
#endif














/*------------------------------------------------------------*/
/*--- Helpers for %flags.                                 ---*/
/*------------------------------------------------------------*/

/* -------------- Evaluating the flags-thunk. -------------- */

#if 0
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
#endif

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

#if 0
static Bool isAddSub ( IROp op8 )
{
   return op8 == Iop_Add8 || op8 == Iop_Sub8;
}
#endif

#if 0
static Bool isLogic ( IROp op8 )
{
   return op8 == Iop_And8 || op8 == Iop_Or8 || op8 == Iop_Xor8;
}
#endif

/* U-widen 8/16/32 bit int expr to 32. */
static IRExpr* widenUto32 ( IRExpr* e )
{
   switch (typeOfIRExpr(irsb->tyenv,e)) {
   case Ity_I32: return e;
   case Ity_I16: return unop(Iop_16Uto32,e);
   case Ity_I8:  return unop(Iop_8Uto32,e);
   default: vpanic("widenUto32");
   }
}

#if 0
/* S-widen 8/16/32 bit int expr to 32. */
static IRExpr* widenSto32 ( IRExpr* e )
{
   switch (typeOfIRExpr(irsb->tyenv,e)) {
   case Ity_I32: return e;
   case Ity_I16: return unop(Iop_16Sto32,e);
   case Ity_I8:  return unop(Iop_8Sto32,e);
   default: vpanic("widenSto32");
   }
}
#endif

/* Narrow 8/16/32 bit int expr to 8/16/32.  Clearly only some
   of these combinations make sense. */
static IRExpr* narrowTo ( IRType dst_ty, IRExpr* e )
{
   IRType src_ty = typeOfIRExpr(irsb->tyenv,e);
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

#if 0
static 
void setFlags_DEP1 ( IROp op, IRTemp dep1 )
{
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(op)) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(dep1))) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0)) );
}
#endif

#if 0
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
#endif





#if 0
/* Multiplies are pretty much like add and sub: DEP1 and DEP2 hold the
   two arguments. */

static
void setFlags_MUL ( IRTemp arg1, IRTemp arg2, UInt op )
{
   stmt( IRStmt_Put( OFFB_CC_OP, mkU32(op) ) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(arg1)) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(mkexpr(arg2)) ));
}
#endif









/* -------------- Condition codes. -------------- */

/* Condition codes, using the ARM encoding.  */

static HChar* name_ARMCondcode ( ARMCondcode cond )
{
   switch (cond) {
   case ARMCondEQ:  return "{eq}";
   case ARMCondNE:  return "{ne}";
   case ARMCondHS:  return "{hs}";  // or 'cs'
   case ARMCondLO:  return "{lo}";  // or 'cc'
   case ARMCondMI:  return "{mi}";
   case ARMCondPL:  return "{pl}";
   case ARMCondVS:  return "{vs}";
   case ARMCondVC:  return "{vc}";
   case ARMCondHI:  return "{hi}";
   case ARMCondLS:  return "{ls}";
   case ARMCondGE:  return "{ge}";
   case ARMCondLT:  return "{lt}";
   case ARMCondGT:  return "{gt}";
   case ARMCondLE:  return "{le}";
   case ARMCondAL:  return "";      // {al}: default, doesn't need specifying
   case ARMCondNV:  return "{nv}";
   default: vpanic("name_ARMCondcode");
   }
}

#if 0
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
#endif


/* Addressing Mode 1 - DP ops
   Addressing Mode 2 - Load/Store word/ubyte (scaled)
 */
static HChar* name_ARMShiftOp ( UChar shift_op, UChar imm_val )
{
   switch (shift_op) {
   case 0x0: case 0x1: case 0x8: return "lsl";
   case 0x2: case 0x3: case 0xA: return "lsr";
   case 0x4: case 0x5: case 0xC: return "asr";
   case 0x6:                     return (imm_val==0) ? "rrx" : "ror";
   case 0x7: case 0xE:           return "ror";
   default: vpanic("name_ARMShiftcode");
   }
}


/* Addressing Mode 4 - Load/Store Multiple */
static HChar* name_ARMAddrMode4 ( UChar mode )
{
   /* See ARM ARM A5-55 for alternative names for stack operations
      ldmfa (full ascending), etc. */
   switch (mode) {
   case 0x0: return "da"; // Decrement after
   case 0x1: return "ia"; // Increment after
   case 0x2: return "db"; // Decrement before
   case 0x3: return "ib"; // Increment before
   default: vpanic("name_ARMAddrMode4");
   }
}

/* Data Processing ops */
static HChar* name_ARMDataProcOp ( UChar opc )
{
   switch (opc) {
   case 0x0: return "and";
   case 0x1: return "eor";
   case 0x2: return "sub";
   case 0x3: return "rsb";
   case 0x4: return "add";
   case 0x5: return "adc";
   case 0x6: return "sbc";
   case 0x7: return "rsc";
   case 0x8: return "tst";
   case 0x9: return "teq";
   case 0xA: return "cmp";
   case 0xB: return "cmn";
   case 0xC: return "orr";
   case 0xD: return "mov";
   case 0xE: return "bic";
   case 0xF: return "mvn";
   default: vpanic("name_ARMDataProcOp");
   }
}



/*
  Addressing mode 4 - LOAD/STORE multiple, LDM|STM
  ARM ARM A5-48
*/
static
Bool dis_loadstore_mult ( UInt theInstr )
{
   UChar flags   = toUChar((theInstr >> 20) & 0x1F); // theInstr[24:20]
   UChar Rn_addr = toUChar((theInstr >> 16) & 0xF);
   IRTemp Rn = newTemp(Ity_I32);
   IRTemp Rn_orig = newTemp(Ity_I32);
   UInt reg_list = theInstr & 0xFFFF;  // each bit addresses a register: R15 to R0
   
              // Load(1) | Store(0)
   UChar L  = toUChar((flags >> 0) & 1);
              // (W)riteback Rn (incr(U=1) | decr(U=0) by n_bytes)
   UChar W  = toUChar((flags >> 1) & 1);
              // Priviledged mode flag - *** CAB TODO ***
   UChar S  = toUChar((flags >> 2) & 1);  
              // Txfr ctl: Direction = upwards(1) | downwards(0)
   UChar U  = toUChar((flags >> 3) & 1);
              // Txfr ctl: Rn within(P=1) | outside(P=0) accessed mem
   UChar PU = toUChar((flags >> 3) & 3);  
   
   IRTemp start_addr = newTemp(Ity_I32);
   IRTemp end_addr   = newTemp(Ity_I32);
   IRTemp data=0;
   UInt n_bytes=0;
   UInt tmp_reg = reg_list;
   UInt reg_idx, offset;
   Bool decode_ok = True;
   
   HChar* cond_name = name_ARMCondcode( (theInstr >> 28) & 0xF );
   HChar reg_names[70];
   UInt buf_offset;
   
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
     Lowest numbered reg -> lowest address, so start with lowest register
     reg_idx: guest register address
     offset : current mem offset from start_addr
   */
   reg_names[0] = '\0';
   buf_offset=0;
   offset=0;
   for (reg_idx=0; reg_idx < 16; reg_idx++) {
      if (( reg_list >> reg_idx ) & 1) {  // reg_list[i] == 1?
         
         if (L==1) { // LOAD Ri, (start_addr + offset)
            
            if (Rn_addr == reg_idx && W==1) { // Undefined - ARM ARM A4-31
               decode_ok=False;
               break;
            }
            
            assign( data, loadLE(Ity_I32, binop(Iop_Add32,
                                                mkexpr(start_addr),
                                                mkU32(offset))) );
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
         
         reg_names[buf_offset++] = 'R';
         if (reg_idx > 9) {
            reg_names[buf_offset++] = '1';
            reg_names[buf_offset++] = (HChar)toUChar(38 + reg_idx);
         } else {
            reg_names[buf_offset++] = (HChar)toUChar(48 + reg_idx);
         }
         reg_names[buf_offset++] = ',';
         // CAB: Eugh!  Where's strcpy?!
      }
   }
   if (buf_offset > 0) {
      reg_names[buf_offset-1] = '\0';
   }
   DIP("%s%s%s R%d%s, {%s}%s\n", (L==1) ? "ldm":"stm", cond_name,
       name_ARMAddrMode4( PU ), Rn_addr, (W==1) ? "!" : "",
       reg_names, (S==1) ? "^" : "");
   
   // CAB TODO:
   // IR assert( end_addr == (start_addr + offset) - 8 )
   
   if (offset == 0) {   // Unpredictable - ARM ARM A5-21
      vex_printf("dis_loadstore_mult(arm): Unpredictable - offset==0\n");
      decode_ok = False;
   }
   
   return decode_ok;
}





static
Bool dis_loadstore_w_ub_address ( UInt theInstr, IRTemp* address, HChar* buf )
{
   UChar is_reg    = toUChar((theInstr >> 25) & 0x1); 
                     // immediate | register offset/index
   UInt  flags     =         (theInstr >> 20) & 0x3F; // theInstr[25:20]
   UChar Rn_addr   = toUChar((theInstr >> 16) & 0xF);
   UChar Rm_addr   = toUChar((theInstr >> 00) & 0xF);
   UChar shift_op  = toUChar((theInstr >> 04) & 0xFF);
   UInt  offset_12 =         (theInstr >> 00) & 0xFFF;
   IRTemp Rn = newTemp(Ity_I32);
   IRTemp Rm = newTemp(Ity_I32);
   UChar shift_imm, shift;
   
   UChar W = toUChar((flags >> 1) & 1); // base register writeback flag - See *Note
   UChar U = toUChar((flags >> 3) & 1); // offset is added(1)|subtracted(0) from the base
   UChar P = toUChar((flags >> 4) & 1); // addressing mode flag - See *Note
   /* *Note
      P==0: post-indexed addressing: addr -> Rn
      W==0: normal mem access
      W==1: unprivileged mem access
      P==1: W==0: offset addressing: Rn not updated  - ARM ARM A5-20
      W==1: pre-indexed addressing: addr -> Rn
   */
   
   IRTemp scaled_index = newTemp(Ity_I32);
   IRTemp reg_offset = newTemp(Ity_I32);
   
   IRTemp oldFlagC = newTemp(Ity_I32);
   
   HChar buf2[30];
   HChar buf3[20];
   buf3[0] = '\0';
   
   if (Rn_addr == 15) {
      if (P==1 && W==0) { // offset addressing
         // CAB: This right?
         assign( Rn, binop(Iop_And32, getIReg(15), mkU32(8)) );
      } else {                        // Unpredictable - ARM ARM A5-25,29...
         vex_printf("dis_loadstore_w_ub_address(arm): Unpredictable - Rn_addr==15\n");
         return False;
      }
   } else {
      assign( Rn, getIReg(Rn_addr) );
   }
   
   /*
     Retrieve / Calculate reg_offset
   */
   if (is_reg) {
      if (Rm_addr == 15) {               // Unpredictable - ARM ARM A5-21
         vex_printf("dis_loadstore_w_ub_address(arm): Unpredictable - Rm_addr==15\n");
         return False;
      }
      if (P==0 || W==1) {  // pre|post-indexed addressing
         if (Rm_addr == Rn_addr) {      // Unpredictable - ARM ARM A5-25
            vex_printf("dis_loadstore_w_ub_address(arm): Unpredictable - Rm_addr==Rn_addr\n");
            return False;
         }
      }
      assign( Rm, getIReg(Rm_addr) );
      
      if (shift_op == 0) {  // Register addressing
         assign( reg_offset, mkexpr(Rm) );
      } else {              // Scaled Register addressing
         shift_imm = toUChar((shift_op >> 3) & 0x1F);
         shift     = toUChar((shift_op >> 1) & 0x3);
         
         switch (shift) {
         case 0x0: // LSL
            assign( scaled_index, binop(Iop_Shl32, mkexpr(Rm), mkU8(shift_imm)) );
            break;
            
         case 0x1: // LSR
            if (shift_imm) {
               assign( scaled_index, binop(Iop_Shr32, mkexpr(Rm), mkU8(shift_imm)) );
            } else {
               assign( scaled_index, mkU32(0) );
            }
            break;
            
         case 0x2: // ASR
            if (shift_imm) {
               assign( scaled_index, binop(Iop_Sar32, mkexpr(Rm), mkU32(shift_imm)) );
            } else {
               assign( scaled_index,     // Rm[31] ? 0xFFFFFFFF : 0x0
                       IRExpr_Mux0X(binop(Iop_And32, mkexpr(Rm), mkU32(0x8FFFFFFF)),
                                    mkexpr(0x0), mkexpr(0xFFFFFFFF)) );
            }
            break;
            
         case 0x3: // ROR|RRX
            assign( oldFlagC, binop(Iop_Shr32,
                                    mk_armg_calculate_flags_c(),
                                    mkU8(ARMG_CC_SHIFT_C)) );
            
            if (shift_imm == 0) { // RRX (ARM ARM A5-17)
               // 33 bit ROR using carry flag as the 33rd bit
               // op = Rm >> 1, carry flag replacing vacated bit position.  
               // scaled_index = (c_flag << 31) | (Rm >> 1)
               assign( scaled_index, binop(Iop_Or32,
                                           binop(Iop_Shl32, mkexpr(oldFlagC), mkU32(31)),
                                           binop(Iop_Shr32, mkexpr(Rm),  mkU8(1))) );
               
            } else { // ROR
               // scaled_index = Rm ROR shift_imm
               //              = (Rm >> shift_imm) | (Rm << (32-shift_imm))
               assign( scaled_index,
                       binop(Iop_Or32,
                             binop(Iop_Shr32, mkexpr(Rm), mkU8(shift_imm)),
                             binop(Iop_Shl32, mkexpr(Rm),
                                   binop(Iop_Sub8, mkU8(32), mkU32(shift_imm)))) );
            }
            break;
            
         default:
            vex_printf("dis_loadstore_w_ub(ARM): No such case: 0x%x", shift);
            return False; 
         }
         assign( reg_offset, mkexpr(scaled_index) );
         
         if (shift == 0x3 && shift_imm == 0) {
            DIS(buf3, ", %s", name_ARMShiftOp(toUChar(shift_op * 2), shift_imm));
         } else {
            DIS(buf3, ", %s #%d", 
                      name_ARMShiftOp(toUChar(shift_op * 2), shift_imm), 
                      shift_imm);
         }
      }
      DIS(buf2, "%cR%d%s", (U==1) ? '+' : '-', Rm_addr, buf3);
   } else { // immediate
      assign( reg_offset, mkU32(offset_12) );
      
      DIS(buf2, "#%c%u", (U==1) ? '+' : '-', offset_12);
   }
   DIS(buf, "[R%d%s, %s%s", Rn_addr,
       (P==0) ? "]" : "", buf2,
       (P==1) ? ((W==1) ? "]!" : "]") : "");
   
   /*
     Depending on P,U,W, write to Rn and set address to load/store
   */
   if (P==1) {       // offset | pre-indexed addressing
      if (U == 1) { // - increment
         assign( *address, binop(Iop_Add32, mkexpr(Rn), mkexpr(reg_offset)) );
      } else {      // - decrement
         assign( *address, binop(Iop_Sub32, mkexpr(Rn), mkexpr(reg_offset)) );
      }
      if (W == 1) { // pre-indexed addressing, base register writeback
         putIReg( Rn_addr, mkexpr(*address) );
      }
   } else {          // post-indexed addressing
      assign( *address, mkexpr(Rn) );
      if (U == 1) { // - increment
         putIReg( Rn_addr, binop( Iop_Add32, mkexpr(Rn), mkexpr(reg_offset) ) );
      } else {      // - decrement
         putIReg( Rn_addr, binop( Iop_Sub32, mkexpr(Rn), mkexpr(reg_offset) ) );
      }
   }
   return True;
}




/*
  Addressing mode 2 - LOAD/STORE word or unsigned byte
  ARM ARM A5-18
*/
static
Bool dis_loadstore_w_ub ( UInt theInstr )
{
   UInt   flags   =         (theInstr >> 20) & 0x3F;  // theInstr[25:20]
   UChar  Rn_addr = toUChar((theInstr >> 16) & 0xF);
   UChar  Rd_addr = toUChar((theInstr >> 12) & 0xF);
   IRTemp address = newTemp(Ity_I32);
  
   UChar L  = toUChar((flags >> 0) & 1); // Load(1) | Store(0)
   UChar W  = toUChar((flags >> 1) & 1); // base register writeback
   UChar B  = toUChar((flags >> 2) & 1); // access = unsigned byte(1) | word(0)
   
   IRTemp value      = newTemp(Ity_I32);
   IRTemp data       = newTemp(Ity_I32);
   IRTemp data_ror8  = newTemp(Ity_I32);
   IRTemp data_ror16 = newTemp(Ity_I32);
   IRTemp data_ror24 = newTemp(Ity_I32);
   IRExpr* expr_addr_10;
   HChar* cond_name = name_ARMCondcode( (theInstr >> 28) & 0xF );
   HChar dis_buf[50];
   
   
   vassert(((theInstr >> 26) & 0x3) == 0x1);
   
   // Get the address to load/store
   if (!dis_loadstore_w_ub_address(theInstr, &address, dis_buf)) { return False; }
   
   DIP("%s%s%s R%d, %s\n", (L==1) ? "ldr" : "str", cond_name,
       (B==1) ? "b" : "", Rd_addr, dis_buf);
   
   if (Rd_addr == Rn_addr && W==1) {  // Unpredictable - ARM ARM A4-39,41,89,91
      vex_printf("dis_loadstore_w_ub(arm): Unpredictable - Rd_addr==Rn_addr\n");
      return False;
   }
   
   /*
     LOAD/STORE Rd, address
   */
   if (L==1) { // LOAD
      if (B==1) {  // unsigned byte (LDRB): ARM ARM A4-40
         if (Rd_addr == 15) {  // Unpredictable - ARM ARM A4-40
            vex_printf("dis_loadstore_w_ub(arm): Unpredictable - Rd_addr==15\n");
            return False;
         }
         putIReg( Rd_addr, loadLE( Ity_I8, mkexpr( address ) ) );
      }
      else {       // word (LDR): ARM ARM A4-38
         expr_addr_10 = binop(Iop_And32, mkexpr(address), mkU32(0x3));
         
         /*
           CAB TODO
           if (Rd_addr == 15 && address[1:0] == 0) => Unpredictable
           How to bomb out using IR?
         */
         
         /* LOAD memory data (4 bytes) */
         assign( data, loadLE( Ity_I32, mkexpr( address ) ) );
         
         // data ROR 8
         assign( data_ror8, binop(Iop_Sub8, mkU8(32), mkU32(8)) ); 
         assign( data_ror8,
                 binop( Iop_Or32,
                        binop( Iop_Shr32, mkexpr(data), mkU8(8) ),
                        binop( Iop_Shl32, mkexpr(data), mkexpr(data_ror8) )));
         // data ROR 16
         assign( data_ror16, binop(Iop_Sub8, mkU8(32), mkU32(16)) );
         assign( data_ror16,
                 binop( Iop_Or32,
                        binop( Iop_Shr32, mkexpr(data), mkU8(16) ),
                        binop( Iop_Shl32, mkexpr(data), mkexpr(data_ror16) )));
         
         // data ROR 24
         assign( data_ror24, binop(Iop_Sub8, mkU8(32), mkU32(24)) );
         assign( data_ror24,
                 binop( Iop_Or32,
                        binop( Iop_Shr32, mkexpr(data), mkU8(24) ),
                        binop( Iop_Shl32, mkexpr(data), mkexpr(data_ror24) )));
         
         /* switch (address[1:0]) {
            0x0: value = data;
            0x1: value = data ROR 8;
            0x2: value = data ROR 16;
            0x3: value = data ROR 24; } */
         assign( value, IRExpr_Mux0X(
                    binop(Iop_CmpEQ32, expr_addr_10, mkU32(0x0)),
                    IRExpr_Mux0X(
                       binop(Iop_CmpEQ32, expr_addr_10, mkU32(0x1)),
                       IRExpr_Mux0X(
                          binop(Iop_CmpEQ32, expr_addr_10, mkU32(0x2)),
                          mkexpr(data_ror24),
                          mkexpr(data_ror16) ),
                       mkexpr(data_ror8) ),
                    mkexpr(data) ) );
         
         if (Rd_addr == 15) {
            // assuming architecture < 5: See ARM ARM A4-28
            putIReg( Rd_addr, binop(Iop_And32, mkexpr(value), mkU32(0xFFFFFFFC)) );
            
            // CAB: Need to tell vex we're doing a jump here?
            // irsb->jumpkind = Ijk_Boring;
            // irsb->next     = mkexpr(value);
         } else {
            putIReg( Rd_addr, mkexpr(value) );
         }
         
      }
   } else { // STORE: ARM ARM A4-88
      if (B==1) {  // unsigned byte
         if (Rd_addr == 15) {  // Unpredictable - ARM ARM A4-90
            vex_printf("dis_loadstore_w_ub(arm): Unpredictable - Rd_addr==15\n");
            return False;
         }
         storeLE( mkexpr(address), unop(Iop_32to8, getIReg(Rd_addr)) );   // Rd[7:0]
      } else {     // word
         
         if (Rd_addr == 15) {  // Implementation Defined - ARM ARM A4-88
            vex_printf("dis_loadstore_w_ub(arm): Implementation Defined - Rd_addr==15\n");
            return False;
            // CAB TODO: What to do here?
         }
         storeLE( mkexpr(address), getIReg(Rd_addr) );
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
IRExpr* dis_shift( Bool* decode_ok, UInt theInstr, IRTemp* carry_out, HChar* buf )
{
   UChar   Rn_addr     = toUChar((theInstr >> 16) & 0xF);
   UChar   Rd_addr     = toUChar((theInstr >> 12) & 0xF);
   UChar   Rs_addr     = toUChar((theInstr >>  8) & 0xF);
   UChar   Rm_addr     = toUChar((theInstr >>  0) & 0xF);
   UChar   by_reg      = toUChar((theInstr >>  4) & 0x1);  // instr[4]
   UChar   shift_imm   = toUChar((theInstr >>  7) & 0x1F); // instr[11:7]
   UChar   shift_op    = toUChar((theInstr >>  4) & 0xF);  // instr[7:4]
   IRTemp  Rm          = newTemp(Ity_I32);
   IRTemp  Rs          = newTemp(Ity_I32);
   IRTemp  shift_amt   = newTemp(Ity_I8);
   IRTemp  carry_shift = newTemp(Ity_I8);
   IRTemp  oldFlagC    = newTemp(Ity_I32);
   IRTemp  mux_false   = newTemp(Ity_I32);
   IRExpr* expr;
   IROp    op;
   
   assign( oldFlagC, binop(Iop_Shr32,
                           mk_armg_calculate_flags_c(),
                           mkU8(ARMG_CC_SHIFT_C)) );
   
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
      assign( Rm, getIReg(Rm_addr) );
      
      if (Rd_addr == 15 || Rm_addr == 15 ||
          Rn_addr == 15 || Rs_addr == 15) {   // Unpredictable (ARM ARM A5-10)
         vex_printf("dis_shift(arm): Unpredictable - Rd|Rm|Rn|Rs == R15\n");
         *decode_ok = False;
         return mkU32(0);
      }
      
      assign( Rs, getIReg((theInstr >> 8) & 0xF) );  // instr[11:8]
      
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
      
      DIS(buf, "R%d, %s R%d", Rm_addr, name_ARMShiftOp(shift_op, 0), Rs_addr);
   }
   else {  // Immediate shift
      
      // CAB: This right?
      // "the value used is the address of the current intruction plus 8"
      if (Rm_addr == 15 || Rn_addr == 15) {        // ARM ARM A5-9
         assign( Rm, binop(Iop_Add32, getIReg(15), mkU32(8)) );
      } else {
         assign( Rm, getIReg(Rm_addr) );
      }
      
      if (shift_imm == 0) {
         switch (shift_op) {
         case 0x0: case 0x8: // LSL(imm)
            expr = mkexpr(Rm);
            assign( *carry_out, mkexpr(oldFlagC) );
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
         DIS(buf, "R%d", Rm_addr);
      } else {
         expr = binop(op, mkexpr(Rm), mkU8(shift_imm));
         assign( *carry_out, binop(op, mkexpr(Rm),
                                   binop(Iop_Sub32, mkU32(shift_imm), mkU32(1)) ) );

         DIS(buf, "R%d, %s #%d", Rm_addr, name_ARMShiftOp(shift_op, 0), shift_imm);
      }
   }
   return expr;
}




/*
  ARMG_CC_OP_ROR
  ARM ARM A5-15,16,17
*/
static
IRExpr* dis_rotate ( Bool* decode_ok, UInt theInstr, IRTemp* carry_out, HChar* buf )
{
   UChar  Rn_addr  = toUChar((theInstr >> 16) & 0xF);
   UChar  Rd_addr  = toUChar((theInstr >> 12) & 0xF);
   UChar  Rs_addr  = toUChar((theInstr >> 8) & 0xF);
   UChar  Rm_addr  = toUChar((theInstr >> 0) & 0xF);
   UChar  by_reg   = toUChar((theInstr >> 4) & 0x1);  // instr[4]
   UChar  rot_imm  = toUChar((theInstr >> 7) & 0x1F); // instr[11:7]
   IRTemp Rm       = newTemp(Ity_I32);
   IRTemp Rs       = newTemp(Ity_I32);
   IRTemp rot_amt  = newTemp(Ity_I8);      // Rs[7:0]
   IRTemp oldFlagC = newTemp(Ity_I32);
   IRExpr* expr=0;
   
   assign( oldFlagC, binop(Iop_Shr32,
                           mk_armg_calculate_flags_c(),
                           mkU8(ARMG_CC_SHIFT_C)) );
   
   if (by_reg) {  // Register rotate
      assign( Rm, getIReg(Rm_addr) );
      
      if (Rd_addr == 15 || Rm_addr == 15 ||
          Rn_addr == 15 || Rs_addr == 15) {    // Unpredictable (ARM ARM A5-10)
         vex_printf("dis_rotate(arm): Unpredictable - Rd|Rm|Rn|Rs == R15\n");
         *decode_ok = False;
         return mkU32(0);
      }
      
      assign( Rs, getIReg((theInstr >> 8) & 0xF) );  // instr[11:8]
      // Rs[4:0]
      assign( rot_amt, narrowTo(Ity_I8,
                                binop(Iop_And32, mkexpr(Rs), mkU32(0x1F))) );
      
      // CAB: This right?
      // Rs[7:0] == 0 ? oldFlagC : (Rs[4:0] == 0 ? Rm >> 31 : Rm >> rot-1 )
      assign( *carry_out,
              IRExpr_Mux0X(
                 binop(Iop_CmpNE32, mkU32(0),
                       binop(Iop_And32, mkexpr(Rs), mkU32(0xFF))),
                 mkexpr(oldFlagC),
                 IRExpr_Mux0X(
                    binop(Iop_CmpEQ8, mkexpr(rot_amt), mkU8(0)),
                    binop(Iop_Shr32, mkexpr(Rm),
                          binop(Iop_Sub8, mkexpr(rot_amt), mkU8(1))),
                    binop(Iop_Shr32, mkexpr(Rm),
                          binop(Iop_Shr32, mkexpr(Rm), mkU8(31))) ) ) );
      
      
      /* expr = (dst0 >> rot_amt) | (dst0 << (wordsize-rot_amt)) */
      expr = binop(Iop_Or32,
                   binop(Iop_Shr32, mkexpr(Rm), mkexpr(rot_amt)),
                   binop(Iop_Shl32, mkexpr(Rm),
                         binop(Iop_Sub8, mkU8(32), mkexpr(rot_amt))));

      DIS(buf, "R%d, ror R%d", Rm_addr, Rs_addr);
   }
   else {  // Immediate rotate

      // CAB: This right?
      // "the value used is the address of the current intruction plus 8"
      if (Rm_addr == 15 || Rn_addr == 15) {        // ARM ARM A5-9
         assign( Rm, binop(Iop_Add32, getIReg(15), mkU32(8)) );
      } else {
         assign( Rm, getIReg(Rm_addr) );
      }
      
      // Rm >> rot-1: carry = R[0]
      assign( *carry_out, binop(Iop_Shr32, mkexpr(Rm),
                                binop(Iop_Sub8, mkU8(rot_imm), mkU8(1)) ) );
      
      if (rot_imm == 0) {   // RRX (ARM ARM A5-17)
         // 33 bit ROR using carry flag as the 33rd bit
         // op = Rm >> 1, carry flag replacing vacated bit position.  
         
         // CAB: This right?
         expr = binop(Iop_Or32,
                      binop(Iop_Shl32, mkexpr(oldFlagC), mkU8(31)),
                      binop(Iop_Shr32, mkexpr(Rm), mkU8(1)));
         DIS(buf, "R%d, rrx", Rm_addr);
      } else {
         expr = binop(Iop_Or32,
                      binop(Iop_Shr32, mkexpr(Rm), mkU8(rot_imm)),
                      binop(Iop_Shl32, mkexpr(Rm),
                            binop(Iop_Sub8, mkU8(32), mkU8(rot_imm))));
         
         DIS(buf, "R%d, ror #%u", Rm_addr, (UInt)rot_imm);
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
IRExpr* dis_shifter_op ( Bool *decode_ok, UInt theInstr, IRTemp* carry_out, HChar* buf )
{
   UChar is_immed  = toUChar((theInstr >> 25) & 1);  // immediate / register shift
   UChar shift_op  = toUChar((theInstr >> 4) & 0xF); // second byte
   UInt immed_8, rot_imm;
   UInt imm;
   IRTemp oldFlagC = newTemp(Ity_I32);
   
   if (is_immed) {  // ARM ARM A5-2
      // dst = src ROR rot << 1
      //     = (src >> rot) | (src << (32-rot));
      immed_8 = theInstr & 0xFF;
      rot_imm = ((theInstr >> 8) & 0xF) << 1;  
      imm = (immed_8 >> rot_imm) | (immed_8 << (32-rot_imm));
      
      if (rot_imm == 0) {
         assign( oldFlagC, binop(Iop_Shr32,
                                 mk_armg_calculate_flags_c(),
                                 mkU8(ARMG_CC_SHIFT_C)) );
         assign( *carry_out, mkexpr(oldFlagC) );
      } else {
         assign( *carry_out, binop(Iop_Shr32, mkU32(imm), mkU8(31)) );
      }
      DIS(buf, "#%u", imm);
      return mkU32(imm);
   } else {
      
      // We shouldn't have any 'op' with bits 4=1 and 7=1 : 1xx1
      switch (shift_op) {
      case 0x0: case 0x8: case 0x1:
      case 0x2: case 0xA: case 0x3: 
      case 0x4: case 0xC: case 0x5:
         return dis_shift( decode_ok, theInstr, carry_out, buf );
         
      case 0x6: case 0xE: case 0x7:
         return dis_rotate( decode_ok, theInstr, carry_out, buf );
         
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
   UChar opc       = toUChar((theInstr >> 21) & 0xF);
   UChar set_flags = toUChar((theInstr >> 20) & 1);
   UChar Rn_addr   = toUChar((theInstr >> 16) & 0xF);
   UChar Rd_addr   = toUChar((theInstr >> 12) & 0xF);
   IRTemp Rn = newTemp(Ity_I32);
   IRTemp Rd = newTemp(Ity_I32);
   IRTemp alu_out = newTemp(Ity_I32);
   IRTemp shifter_op = newTemp(Ity_I32);
   IRTemp carry_out = newTemp(Ity_I32);
   IROp op_set_flags = ARMG_CC_OP_LOGIC;
   Bool testing_instr = False;
   Bool decode_ok = True;
   HChar* cond_name = name_ARMCondcode( (theInstr >> 28) & 0xF );
   HChar* ch_set_flags = (set_flags == 1) ? "S" : "";
   HChar dis_buf[50];
   
   assign( shifter_op, dis_shifter_op( &decode_ok, theInstr, &carry_out, dis_buf ) );
   if (!decode_ok) return False;
   
   assign( Rd, getIReg(Rd_addr) );
   assign( Rn, getIReg(Rn_addr) );
   
   
   switch (opc) {
   case 0x0: case 0x1: case 0x2: case 0x3: case 0x4:
   case 0xC: case 0xE:
      DIP("%s%s%s R%d, R%d, %s\n", name_ARMDataProcOp(opc),
          cond_name, ch_set_flags, Rd_addr, Rn_addr, dis_buf);
      break;
   case 0x5: case 0x6: case 0x7:
      // CAB: Unimplemented
      break;
   case 0x8: case 0x9: case 0xA: case 0xB:
      DIP("%s%s R%d, %s\n", name_ARMDataProcOp(opc),
          cond_name, Rn_addr, dis_buf);
      break;
   case 0xD: case 0xF:
      DIP("%s%s%s R%d, %s\n", name_ARMDataProcOp(opc),
          cond_name, ch_set_flags, Rd_addr, dis_buf);
      break;
   default:break;
   }


   switch (opc) {
   case 0x0: // AND
      assign( alu_out, binop(Iop_And32, getIReg(Rn_addr), mkexpr(shifter_op)) );
      break;
       
   case 0x1: // EOR
      assign( alu_out, binop(Iop_Xor32, getIReg(Rn_addr), mkexpr(shifter_op)) );
      break;

   case 0x2: // SUB
      assign( alu_out, binop( Iop_Sub32, getIReg(Rn_addr), mkexpr(shifter_op) ) );
      op_set_flags = ARMG_CC_OP_SUB;
      break;

   case 0x3:  // RSB
      assign( alu_out, binop( Iop_Sub32, mkexpr(shifter_op), getIReg(Rn_addr) ) );
      op_set_flags = ARMG_CC_OP_SUB;
      /* set_flags(), below, switches the args for this case */
      break;

   case 0x4: // ADD
      assign( alu_out, binop( Iop_Add32, getIReg(Rn_addr), mkexpr(shifter_op) ) );
      op_set_flags = ARMG_CC_OP_ADD;
      break;

   case 0x5:  // ADC  // CAB: Unimplemented
   case 0x6:  // SBC  // CAB: Unimplemented
   case 0x7:  // RSC  // CAB: Unimplemented
      goto decode_failure;

   case 0x8: // TST
      vassert(set_flags==1);
      assign( alu_out, binop(Iop_And32, getIReg(Rn_addr), mkexpr(shifter_op)) );
      testing_instr = True;
      break;
      
   case 0x9: // TEQ
      vassert(set_flags==1);
      assign( alu_out, binop(Iop_Xor32, getIReg(Rn_addr), mkexpr(shifter_op)) );
      testing_instr = True;
      break;
      
   case 0xA: // CMP
      vassert(set_flags==1);
      op_set_flags = ARMG_CC_OP_SUB;
      testing_instr = True;
      break;

   case 0xB: // CMN
      vassert(set_flags==1);
      op_set_flags = ARMG_CC_OP_ADD;
      testing_instr = True;
      break;

   case 0xC: // ORR
      assign( alu_out, binop(Iop_Or32, getIReg(Rn_addr), mkexpr(shifter_op)) );
      break;

   case 0xD: // MOV
      assign( alu_out, mkexpr(shifter_op) );
      break;

   case 0xE: // BIC
      assign( alu_out, binop(Iop_And32, getIReg(Rn_addr),
                             unop( Iop_Not32, mkexpr(shifter_op))) );
      break;

   case 0xF: // MVN
      assign( alu_out, unop(Iop_Not32, mkexpr(shifter_op)) );
      break;

   default:
   decode_failure:
      vex_printf("dis_dataproc(arm): unhandled opcode: 0x%x\n", opc);
      return False;
   }

   if (!testing_instr) {
      if ( Rd_addr == 15) { // dest reg == PC
         // CPSR = SPSR: Unpredictable in User | System mode (no SPSR!)
         // Unpredictable - We're only supporting user mode...
         vex_printf("dis_dataproc(arm): Unpredictable - Rd_addr==15\n");
         return False;
      }
      putIReg( Rd_addr, mkexpr(alu_out) );
   }
   
   if (set_flags) {
      if (op_set_flags == ARMG_CC_OP_LOGIC) {
         setFlags_DEP1_DEP2( op_set_flags, alu_out, carry_out );
      } else {
         if (opc == 0x3) {
            setFlags_DEP1_DEP2( op_set_flags, shifter_op, Rn );
         } else {
            setFlags_DEP1_DEP2( op_set_flags, Rn, shifter_op );
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
   UChar link = toUChar((theInstr >> 24) & 1);
   UInt signed_immed_24 = theInstr & 0xFFFFFF;
   UInt branch_offset;
   IRTemp addr = newTemp(Ity_I32);
   IRTemp dest = newTemp(Ity_I32);
   
   if (link) { // LR (R14) = addr of instr after branch instr
      assign( addr, binop(Iop_Add32, getIReg(15), mkU32(4)) );
      putIReg( 14, mkexpr(addr) );
   }
   
   // PC = PC + (SignExtend(signed_immed_24) << 2)
   branch_offset = extend_s_24to32( signed_immed_24 ) << 2;
   assign( dest, binop(Iop_Add32, getIReg(15), mkU32(branch_offset)) );
   
   irsb->jumpkind = link ? Ijk_Call : Ijk_Boring;
   irsb->next     = mkexpr(dest);
   
   // Note: Not actually writing to R15 - let the IR stuff do that.
   
   DIP("b%s%s 0x%x\n",
       link ? "l" : "",
       name_ARMCondcode( (theInstr >> 28) & 0xF ),
       branch_offset);
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
                            /*IN*/  Long    delta, 
                            /*OUT*/ Int*    size,
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
   

   /* At least this is simple on ARM: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   
   /* We will set *size to 4 if the insn is successfully decoded.
      Setting it to 0 by default makes bbToIR_ARM abort if we fail the
      decode. */
   *size = 0;

   theInstr = *(UInt*)(&guest_code[delta]);

//   vex_printf("START: 0x%x, %,b\n", theInstr, theInstr );

   DIP("\t0x%x:  ", toUInt(guest_pc_bbstart+delta));



   // TODO: fix the client-request stuff, else nothing will work

   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
      UInt* code = (UInt*)(guest_code + delta);
      
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
         
         irsb->next     = mkU32(toUInt(guest_pc_bbstart+delta));
         irsb->jumpkind = Ijk_ClientReq;
         
         whatNext = Dis_StopHere;
         goto decode_success;
      }
   }





   /*
     Deal with condition first
   */
   cond = (theInstr >> 28) & 0xF;    /* opcode: bits 31:28 */
//   vex_printf("\ndisInstr(arm): cond: 0x%x, %b\n", cond, cond );
   
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
                         IRConst_U32(toUInt(guest_pc_bbstart+delta+4)) ) );
      //irsb->next     = mkU32(guest_pc_bbstart+delta+4);
      //irsb->jumpkind = Ijk_Boring;
   }
   


   /* Primary opcode is roughly bits 27:20 (ARM ARM(v2) A3-2)
      secondary opcode is bits 4:0 */
   opc1 = toUChar((theInstr >> 20) & 0xFF);    /* opcode1: bits 27:20 */
   opc2 = toUChar((theInstr >> 4 ) & 0xF);     /* opcode2: bits 7:4   */
//   vex_printf("disInstr(arm): opcode1: 0x%2x, %,09b\n", opc1, opc1 );
//   vex_printf("disInstr(arm): opcode2: 0x%02x, %,04b\n", opc2, opc2 );
   
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
         opc_tmp = toUChar((opc1 >> 1) & 0x3);
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
               if (cond != 0xE) {            // Unpredictable - ARM ARM A3-4
                  vex_printf("disInstr(arm): Unpredictable instruction\n");
                  goto decode_failure;
               }
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
        (if we get here, it's a dpi)
      */
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
      if (!dis_loadstore_mult(theInstr)) { goto decode_failure; }
      break;
      
      
   /*
     Branch, Branch and Link
   */
   case 0xA: case 0xB:  // B, BL
      // B(L): L=1 => return address stored in link register (R14)
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
//   vex_printf("disInstr(arm): success");
   DIP("\n");
   
   *size = 4;
   return whatNext;
}

#undef DIP
#undef DIS

/*--------------------------------------------------------------------*/
/*--- end                                         guest-arm/toIR.c ---*/
/*--------------------------------------------------------------------*/
