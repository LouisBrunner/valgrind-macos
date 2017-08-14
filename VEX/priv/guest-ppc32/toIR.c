
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-ppc32/toIR.c) is                            ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.  All rights reserved.

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

/* TODO 2005 07 15:

   Get rid of all vestiges of flag helper fns.

   Spot rlwini cases which are simply left/right shifts and
   emit Shl32/Shr32 accordingly.

   Move mtxer/mfxer code into its own function.
*/


/* Translates PPC32 code to IR. */

/* References

#define PPC32
   "PowerPC Microprocessor Family:
    The Programming Environments for 32-Bit Microprocessors"
    02/21/2000
    http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600719DF2

#define AV
   "PowerPC Microprocessor Family:
    AltiVec(TM) Technology Programming Environments Manual"
    07/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/FBFA164F824370F987256D6A006F424D

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
#include "guest-generic/bb_to_IR.h"
#include "guest-ppc32/gdefs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of an insn, right
   down in disInstr_PPC32, so that we don't have to pass them around
   endlessly.  They are all constant during the translation of any
   given insn. */

/* We need to know this to do sub-register accesses correctly. */
static Bool host_is_bigendian;

/* Pointer to the guest code area. */
static UChar* guest_code;

/* The guest address corresponding to guest_code[0]. */
static Addr32 guest_CIA_bbstart;

/* The guest address for the instruction currently being
   translated. */
static Addr32 guest_CIA_curr_instr;

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

#define OFFB_CIA        offsetof(VexGuestPPC32State,guest_CIA)
#define OFFB_LR         offsetof(VexGuestPPC32State,guest_LR)
#define OFFB_CTR        offsetof(VexGuestPPC32State,guest_CTR)

#define OFFB_XER_SO     offsetof(VexGuestPPC32State,guest_XER_SO)
#define OFFB_XER_OV     offsetof(VexGuestPPC32State,guest_XER_OV)
#define OFFB_XER_CA     offsetof(VexGuestPPC32State,guest_XER_CA)
#define OFFB_XER_BC     offsetof(VexGuestPPC32State,guest_XER_BC)

#define OFFB_FPROUND    offsetof(VexGuestPPC32State,guest_FPROUND)

#define OFFB_VRSAVE     offsetof(VexGuestPPC32State,guest_VRSAVE)
#define OFFB_VSCR       offsetof(VexGuestPPC32State,guest_VSCR)

#define OFFB_EMWARN     offsetof(VexGuestPPC32State,guest_EMWARN)

#define OFFB_TISTART    offsetof(VexGuestPPC32State,guest_TISTART)
#define OFFB_TILEN      offsetof(VexGuestPPC32State,guest_TILEN)


/*------------------------------------------------------------*/
/*--- Extract instruction fields                          --- */
/*------------------------------------------------------------*/

/* Extract primary opcode, instr[31:26] */
static UInt ifieldOPC ( UInt instr ) {
   return (instr >> 26) & 0x3F;
}

/* Extract 10-bit secondary opcode, instr[11:1] */
static UInt ifieldOPClo10 ( UInt instr) {
   return (instr >> 1 ) & 0x3FF;
}

/* Extract RD (destination register) field, instr[25:21] */
static UInt ifieldRD ( UInt instr ) {
   return (instr >> 21) & 0x1F;
}

/* Extract RA (first source register) field, instr[20:16] */
static UInt ifieldRA ( UInt instr ) {
   return (instr >> 16) & 0x1F;
}

/* Extract RB (first source register) field, instr[15:11] */
static UInt ifieldRB ( UInt instr ) {
   return (instr >> 11) & 0x1F;
}

/* Extract bottom bit (Rc?) from instr */
static UInt ifieldBIT0 ( UInt instr ) {
   return instr & 1;
}

/* Extract lower half of instruction and sign extent to 32 bits */
static Int ifieldSIMM16 ( UInt instr ) {
   Int i = instr & 0xFFFF;
   return (i << 16) >> 16;
}


//zz /*------------------------------------------------------------*/
//zz /*--- Abstract register interface (non-gpr|fpr)           --- */
//zz /*------------------------------------------------------------*/
//zz 
//zz /* Offsets of bitfields within various ppc32 registers */
//zz #define SHIFT_XER_SO 31
//zz #define SHIFT_XER_OV 30
//zz #define SHIFT_XER_CA 29
//zz #define SHIFT_XER_BC  0
//zz 
//zz #define SHIFT_CR_LT 8
//zz #define SHIFT_CR_GT 4
//zz #define SHIFT_CR_EQ 2
//zz #define SHIFT_CR_SO 1

#define SHIFT_FPSCR_RN 0
#define MASK_FPSCR_RN  (3  << SHIFT_FPSCR_RN)

//zz #define SHIFT_VSCR_NJ  16
//zz #define SHIFT_VSCR_SAT  0


// Special purpose (i.e. non-gpr/fpr) registers
typedef enum {
    PPC32_SPR_CIA,    // Current Instruction Address
    PPC32_SPR_LR,     // Link Register
    PPC32_SPR_CTR,    // Count Register
//zz     PPC32_SPR_XER,    // Summary Overflow
//zz     PPC32_SPR_CR,     // Condition Register
    PPC32_SPR_FPSCR,  // Floating Point Status/Control Register
    PPC32_SPR_VRSAVE, // Vector Save/Restore Register
//zz     PPC32_SPR_VSCR,   // Vector Status and Control Register
    PPC32_SPR_MAX
} PPC32SPR;

//zz /*
//zz   Note on FPSCR: Floating Point Status and Control Register
//zz 
//zz   We're only keeping hold of fp rounding-mode bits, via guest_FPROUND
//zz   The rest of the FPSCR is set to 0x0, which corresponds to
//zz   'all exceptions masked'
//zz   
//zz   FPSCR[29:31] => Exception Summaries
//zz   FPSCR[17:28] => Exceptions
//zz   FPSCR[16]    => FPRF::Class Descriptor
//zz   FPSCR[12:15] => FPRF::Condition Code
//zz   FPSCR[11]    => Unused (0)
//zz   FPSCR[8:10]  => Exceptions
//zz   FPSCR[3:7]   => Exception Control
//zz   FPSCR[2]     => Non-IEEE mode
//zz   FPSCR[0:1]   => Rounding Mode
//zz 
//zz   CAB: Perhaps necessary to record writes to FPRF ?
//zz   Set by dis_fp_cmp() instrs, also some fp arith/round/conv instrs.
//zz   Tested using dis_fp_scr(): e.g fpscr->cr, branch conditional...
//zz */
//zz 
//zz 
//zz /* Gets from SPR (non-GPR|FPR) registers */
//zz static IRExpr* getReg_masked ( PPC32SPR reg, UInt mask );
static IRExpr* getSPR        ( PPC32SPR reg );
//zz static IRExpr* getReg_field  ( PPC32SPR reg, UInt field_idx );
//zz static IRExpr* getReg_bit    ( PPC32SPR reg, UInt bit_idx );
//zz 
//zz /* Puts to SPR (non-GPR|FPR) registers */
//zz static void putReg_masked ( PPC32SPR reg, IRExpr* src, UInt mask );
static void putSPR        ( PPC32SPR reg, IRExpr* src );
//zz static void putReg_field  ( PPC32SPR reg, IRExpr* src, UInt field_idx );
//zz static void putReg_bit    ( PPC32SPR reg, IRExpr* src, UInt bit_idx );

/* FP Helpers */
static void put_emwarn ( IRExpr* e /* :: Ity_I32 */ );




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

//zz #if PPC32_TOIR_DEBUG
//zz static void vex_printf_binary( UInt x, UInt len, Bool spaces )
//zz {
//zz    UInt i;
//zz    vassert(len > 0 && len <= 32);
//zz    
//zz    for (i=len; i>0; i--) {
//zz       vex_printf("%d", ((x & (1<<(len-1))) != 0) );
//zz       x = x << 1;
//zz       if (((i-1)%4)==0 && (i > 1) && spaces) {
//zz          vex_printf(" ");
//zz       }
//zz    }
//zz }
//zz #endif


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

//zz /* Various simple conversions */
//zz 
//zz static UChar extend_s_5to8 ( UChar x )
//zz {
//zz    return toUChar((((Int)x) << 27) >> 27);
//zz }
//zz 
//zz #if 0
//zz static UInt extend_s_8to32( UInt x )
//zz {
//zz    return (UInt)((((Int)x) << 24) >> 24);
//zz }
//zz #endif

static UInt extend_s_16to32 ( UInt x )
{
   return (UInt)((((Int)x) << 16) >> 16);
}

static UInt extend_s_26to32 ( UInt x )
{
   return (UInt)((((Int)x) << 6) >> 6);
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

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_Tmp(dst, e) );
}

static void storeBE ( IRExpr* addr, IRExpr* data )
{
   stmt( IRStmt_Store(Iend_BE,addr,data) );
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

//uu static IRExpr* mkU1 ( UInt i )
//uu {
//uu    vassert(i < 2);
//uu    return IRExpr_Const(IRConst_U1( toBool(i) ));
//uu }

static IRExpr* mkU8 ( UChar i )
{
   return IRExpr_Const(IRConst_U8(i));
}

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* loadBE ( IRType ty, IRExpr* data )
{
   return IRExpr_Load(Iend_BE,ty,data);
}

static IRExpr* mkOR1 ( IRExpr* arg1, IRExpr* arg2 )
{
   vassert(typeOfIRExpr(irbb->tyenv, arg1) == Ity_I1);
   vassert(typeOfIRExpr(irbb->tyenv, arg2) == Ity_I1);
   return
      unop(Iop_32to1, binop(Iop_Or32, unop(Iop_1Uto32, arg1), 
                                      unop(Iop_1Uto32, arg2)));
}

static IRExpr* mkAND1 ( IRExpr* arg1, IRExpr* arg2 )
{
   vassert(typeOfIRExpr(irbb->tyenv, arg1) == Ity_I1);
   vassert(typeOfIRExpr(irbb->tyenv, arg2) == Ity_I1);
   return
      unop(Iop_32to1, binop(Iop_And32, unop(Iop_1Uto32, arg1), 
                                       unop(Iop_1Uto32, arg2)));
}

static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   // jrs: probably not necessary; only matters if we reference sub-parts
   // of the ppc32 registers, but that isn't the case
   // later: this might affect Altivec though?
   vassert(host_is_bigendian);

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
      default: break;
   }
   vpanic("integerGuestRegOffset(ppc32,be)"); /*notreached*/
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


static Int floatGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   switch (archreg) {
      case  0: return offsetof(VexGuestPPC32State, guest_FPR0);
      case  1: return offsetof(VexGuestPPC32State, guest_FPR1);
      case  2: return offsetof(VexGuestPPC32State, guest_FPR2);
      case  3: return offsetof(VexGuestPPC32State, guest_FPR3);
      case  4: return offsetof(VexGuestPPC32State, guest_FPR4);
      case  5: return offsetof(VexGuestPPC32State, guest_FPR5);
      case  6: return offsetof(VexGuestPPC32State, guest_FPR6);
      case  7: return offsetof(VexGuestPPC32State, guest_FPR7);
      case  8: return offsetof(VexGuestPPC32State, guest_FPR8);
      case  9: return offsetof(VexGuestPPC32State, guest_FPR9);
      case 10: return offsetof(VexGuestPPC32State, guest_FPR10);
      case 11: return offsetof(VexGuestPPC32State, guest_FPR11);
      case 12: return offsetof(VexGuestPPC32State, guest_FPR12);
      case 13: return offsetof(VexGuestPPC32State, guest_FPR13);
      case 14: return offsetof(VexGuestPPC32State, guest_FPR14);
      case 15: return offsetof(VexGuestPPC32State, guest_FPR15);
      case 16: return offsetof(VexGuestPPC32State, guest_FPR16);
      case 17: return offsetof(VexGuestPPC32State, guest_FPR17);
      case 18: return offsetof(VexGuestPPC32State, guest_FPR18);
      case 19: return offsetof(VexGuestPPC32State, guest_FPR19);
      case 20: return offsetof(VexGuestPPC32State, guest_FPR20);
      case 21: return offsetof(VexGuestPPC32State, guest_FPR21);
      case 22: return offsetof(VexGuestPPC32State, guest_FPR22);
      case 23: return offsetof(VexGuestPPC32State, guest_FPR23);
      case 24: return offsetof(VexGuestPPC32State, guest_FPR24);
      case 25: return offsetof(VexGuestPPC32State, guest_FPR25);
      case 26: return offsetof(VexGuestPPC32State, guest_FPR26);
      case 27: return offsetof(VexGuestPPC32State, guest_FPR27);
      case 28: return offsetof(VexGuestPPC32State, guest_FPR28);
      case 29: return offsetof(VexGuestPPC32State, guest_FPR29);
      case 30: return offsetof(VexGuestPPC32State, guest_FPR30);
      case 31: return offsetof(VexGuestPPC32State, guest_FPR31);
      default: break;
   }
   vpanic("floatGuestRegOffset(ppc32)"); /*notreached*/
}

static IRExpr* getFReg ( UInt archreg )
{
   vassert(archreg < 32);
   return IRExpr_Get( floatGuestRegOffset(archreg), Ity_F64 );
}

/* Ditto, but write to a reg instead. */
static void putFReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_F64);
   stmt( IRStmt_Put(floatGuestRegOffset(archreg), e) );
}


static Int vectorGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   switch (archreg) {
      case  0: return offsetof(VexGuestPPC32State, guest_VR0);
      case  1: return offsetof(VexGuestPPC32State, guest_VR1);
      case  2: return offsetof(VexGuestPPC32State, guest_VR2);
      case  3: return offsetof(VexGuestPPC32State, guest_VR3);
      case  4: return offsetof(VexGuestPPC32State, guest_VR4);
      case  5: return offsetof(VexGuestPPC32State, guest_VR5);
      case  6: return offsetof(VexGuestPPC32State, guest_VR6);
      case  7: return offsetof(VexGuestPPC32State, guest_VR7);
      case  8: return offsetof(VexGuestPPC32State, guest_VR8);
      case  9: return offsetof(VexGuestPPC32State, guest_VR9);
      case 10: return offsetof(VexGuestPPC32State, guest_VR10);
      case 11: return offsetof(VexGuestPPC32State, guest_VR11);
      case 12: return offsetof(VexGuestPPC32State, guest_VR12);
      case 13: return offsetof(VexGuestPPC32State, guest_VR13);
      case 14: return offsetof(VexGuestPPC32State, guest_VR14);
      case 15: return offsetof(VexGuestPPC32State, guest_VR15);
      case 16: return offsetof(VexGuestPPC32State, guest_VR16);
      case 17: return offsetof(VexGuestPPC32State, guest_VR17);
      case 18: return offsetof(VexGuestPPC32State, guest_VR18);
      case 19: return offsetof(VexGuestPPC32State, guest_VR19);
      case 20: return offsetof(VexGuestPPC32State, guest_VR20);
      case 21: return offsetof(VexGuestPPC32State, guest_VR21);
      case 22: return offsetof(VexGuestPPC32State, guest_VR22);
      case 23: return offsetof(VexGuestPPC32State, guest_VR23);
      case 24: return offsetof(VexGuestPPC32State, guest_VR24);
      case 25: return offsetof(VexGuestPPC32State, guest_VR25);
      case 26: return offsetof(VexGuestPPC32State, guest_VR26);
      case 27: return offsetof(VexGuestPPC32State, guest_VR27);
      case 28: return offsetof(VexGuestPPC32State, guest_VR28);
      case 29: return offsetof(VexGuestPPC32State, guest_VR29);
      case 30: return offsetof(VexGuestPPC32State, guest_VR30);
      case 31: return offsetof(VexGuestPPC32State, guest_VR31);
      default: break;
   }
   vpanic("vextorGuestRegOffset(ppc32)"); /*notreached*/
}

static IRExpr* getVReg ( UInt archreg )
{
   vassert(archreg < 32);
   return IRExpr_Get( vectorGuestRegOffset(archreg), Ity_V128 );
}

/* Ditto, but write to a reg instead. */
static void putVReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_V128);
   stmt( IRStmt_Put(vectorGuestRegOffset(archreg), e) );
}

static Int guestCR321offset ( UInt cr )
{
   switch (cr) {
      case 0: return offsetof(VexGuestPPC32State, guest_CR0_321 );
      case 1: return offsetof(VexGuestPPC32State, guest_CR1_321 );
      case 2: return offsetof(VexGuestPPC32State, guest_CR2_321 );
      case 3: return offsetof(VexGuestPPC32State, guest_CR3_321 );
      case 4: return offsetof(VexGuestPPC32State, guest_CR4_321 );
      case 5: return offsetof(VexGuestPPC32State, guest_CR5_321 );
      case 6: return offsetof(VexGuestPPC32State, guest_CR6_321 );
      case 7: return offsetof(VexGuestPPC32State, guest_CR7_321 );
      default: vpanic("guestCR321offset(ppc32)");
   }
} 

static Int guestCR0offset ( UInt cr )
{
   switch (cr) {
      case 0: return offsetof(VexGuestPPC32State, guest_CR0_0 );
      case 1: return offsetof(VexGuestPPC32State, guest_CR1_0 );
      case 2: return offsetof(VexGuestPPC32State, guest_CR2_0 );
      case 3: return offsetof(VexGuestPPC32State, guest_CR3_0 );
      case 4: return offsetof(VexGuestPPC32State, guest_CR4_0 );
      case 5: return offsetof(VexGuestPPC32State, guest_CR5_0 );
      case 6: return offsetof(VexGuestPPC32State, guest_CR6_0 );
      case 7: return offsetof(VexGuestPPC32State, guest_CR7_0 );
      default: vpanic("guestCR3offset(ppc32)");
   }
} 

static void putCR321 ( UInt cr, IRExpr* e )
{
   vassert(cr < 8);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(guestCR321offset(cr), e) );
}

static void putCR0 ( UInt cr, IRExpr* e )
{
   vassert(cr < 8);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(guestCR0offset(cr), e) );
}

static IRExpr* /* :: Ity_I8 */ getCR0 ( UInt cr )
{
   vassert(cr < 8);
   return IRExpr_Get(guestCR0offset(cr), Ity_I8);
}

static IRExpr* /* :: Ity_I8 */ getCR321 ( UInt cr )
{
   vassert(cr < 8);
   return IRExpr_Get(guestCR321offset(cr), Ity_I8);
}

static IRExpr* /* :: Ity_I8 */ getXER_SO ( void )
{
   return IRExpr_Get( OFFB_XER_SO, Ity_I8 );
}

// ROTL(src32, rot_amt5)
static IRExpr* ROTL32 ( IRExpr* src, IRExpr* rot_amt )
{
   IRExpr* masked;
   vassert(typeOfIRExpr(irbb->tyenv,src) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,rot_amt) == Ity_I32);

   masked 
      = unop(Iop_32to8, binop(Iop_And32, rot_amt, mkU32(31)));

   // (src << rot_amt) | (src >> (32-rot_amt))
   /* Note: the MuxOX is not merely an optimisation; it's needed
      because otherwise the Shr32 is a shift by the word size when
      masked denotes zero.  For rotates by immediates, a lot of
      this junk gets folded out. */
   return 
      IRExpr_Mux0X( 
         masked,
         /* zero rotate. */
         src,
         /* non-zero rotate */
         binop( Iop_Or32,
                binop(Iop_Shl32, src, masked),
                binop(Iop_Shr32, src, binop(Iop_Sub8, mkU8(32), masked))
         )
      );
}


/*------------------------------------------------------------*/
/*--- Helpers for condition codes.                         ---*/
/*------------------------------------------------------------*/

/* Condition register layout. 

   In the hardware, CR is laid out like this.  The leftmost end is the
   most significant bit in the register; however the IBM documentation
   numbers the bits backwards for some reason.

   CR0      CR1    ..........   CR6       CR7
   0 .. 3   .......................  28 .. 31    (IBM bit numbering)
   31  28                             3    0     (normal bit numbering)

   Each CR field is 4 bits:

       <  >  ==  SO

   Hence in IBM's notation, BI=0 indicates CR7.SO, BI=1 is CR7.==,
   etc.

   Indexing from BI to guest state:

     let    n = BI / 4
          off = BI % 4
     this references CR n:

        off==3   ->  guest_CRn_SO
        off==2   ->  guest_CRn_123 >> 1
        off==1   ->  guest_CRn_123 >> 2
        off==0   ->  guest_CRn_123 >> 3

   Bear in mind the only significant bit in guest_CRn_SO is bit 0
   (normal notation) and in guest_CRn_123 the significant bits are
   3, 2 and 1 (normal notation).
*/
/* Fetch the specified CR bit (as per IBM/hardware notation) and
   return it at the bottom of an I32; the top 31 bits are guaranteed
   to be zero. */
static IRExpr* /* :: Ity_I32 */ getCRbit ( UInt bi )
{
   UInt n   = bi / 4;
   UInt off = bi % 4;
   vassert(bi < 32);
   if (off == 3) {
      /* Fetch the SO bit for this CR field */
      /* Note: And32 is redundant paranoia iff guest state only has 0
         or 1 in that slot. */
      return binop(Iop_And32, unop(Iop_8Uto32, getCR0(n)), mkU32(1));
   } else {
      /* Fetch the <, > or == bit for this CR field */
      return binop( Iop_And32, 
                    binop( Iop_Shr32, 
                           unop(Iop_8Uto32, getCR321(n)),
                           mkU8(3-off) ),
                    mkU32(1) );
   }
}

/* Dually, write the least significant bit of BIT to the specified CR
   bit.  Indexing as per getCRbit. */
static void putCRbit ( UInt bi, IRExpr* bit )
{
   IRExpr* safe;
   vassert(typeOfIRExpr(irbb->tyenv,bit) == Ity_I32);
   safe = binop(Iop_And32, bit, mkU32(1));
   UInt n   = bi / 4;
   UInt off = bi % 4;
   vassert(bi < 32);
   if (off == 3) {
      /* This is the SO bit for this CR field */
      putCR0(n, unop(Iop_32to8, safe));
   } else {
      off = 3 - off;
      vassert(off == 1 || off == 2 || off == 3);
      putCR321(
         n,
         unop( Iop_32to8,
               binop( Iop_Or32,
                      /* old value with field masked out */
                      binop(Iop_And32, unop(Iop_8Uto32, getCR321(n)),
                                       mkU32(~(1 << off))),
                      /* new value in the right place */
                      binop(Iop_Shl32, safe, mkU8(off))
               )
         )
      );
   }
}


/* Fetch the specified CR bit (as per IBM/hardware notation) and
   return it somewhere in an I32; it does not matter where, but
   whichever bit it is, all other bits are guaranteed to be zero.  In
   other words, the I32-typed expression will be zero if the bit is
   zero and nonzero if the bit is 1.  Write into *where the index
   of where the bit will be. */

static IRExpr* /* :: Ity_I32 */ getCRbit_anywhere ( UInt bi, Int* where )
{
   UInt n   = bi / 4;
   UInt off = bi % 4;
   vassert(bi < 32);
   if (off == 3) {
      /* Fetch the SO bit for this CR field */
      /* Note: And32 is redundant paranoia iff guest state only has 0
         or 1 in that slot. */
      *where = 0;
      return binop(Iop_And32, unop(Iop_8Uto32, getCR0(n)), mkU32(1));
   } else {
      /* Fetch the <, > or == bit for this CR field */
      *where = 3-off;
      return binop( Iop_And32, 
                    unop(Iop_8Uto32, getCR321(n)),
                    mkU32(1 << (3-off)) );
   }
}


/* Synthesise the entire CR into a single word.  Expensive. */

static IRExpr* /* :: Ity_I32 */ getEntireCR ( void )
{
#  define FIELD(_n)                                               \
      binop(Iop_Shl32,                                            \
            unop(Iop_8Uto32,                                      \
                 binop(Iop_Or8,                                   \
                       binop(Iop_And8, getCR321(_n), mkU8(7<<1)), \
                       binop(Iop_And8, getCR0(_n), mkU8(1))       \
                 )                                                \
            ),                                                    \
            mkU8(4 * (7-(_n)))                                    \
      )

   return binop(Iop_Or32,
                binop(Iop_Or32,
                      binop(Iop_Or32, FIELD(0), FIELD(1)),
                      binop(Iop_Or32, FIELD(2), FIELD(3))
                ),
                binop(Iop_Or32,
                      binop(Iop_Or32, FIELD(4), FIELD(5)),
                      binop(Iop_Or32, FIELD(6), FIELD(7))
                )
          );
#  undef FIELD
}

static void putCRfields ( IRExpr* w32, UInt mask )
{
   IRTemp t;
   Int    cr;
   vassert(typeOfIRExpr(irbb->tyenv,w32) == Ity_I32);
   vassert(mask < 256);
   for (cr = 0; cr < 8; cr++) {
      if ((mask & (1 << (7-cr))) == 0)
         continue;
      t = newTemp(Ity_I32);
      assign( t, binop(Iop_Shr32, w32, mkU8(4*(7-cr))) );
      putCR0( cr, unop(Iop_32to8, 
                       binop(Iop_And32, mkexpr(t), mkU32(1))) );
      putCR321( cr, unop(Iop_32to8,
                         binop(Iop_And32, mkexpr(t), mkU32(7<<1))) );
   }
}


//zz /* -------------- Evaluating the flags-thunk. -------------- */
//zz 
//zz /* Calculate CR7 (IBM CR0) conditional flags */
//zz static IRExpr* mk_ppc32g_calculate_cr7 ( void )
//zz {
//zz    IRExpr** args =
//zz       mkIRExprVec_3( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
//zz                      IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
//zz                      IRExpr_Get(OFFB_CC_DEP2, Ity_I32) );
//zz    IRExpr* call
//zz       = mkIRExprCCall(
//zz            Ity_I32,
//zz            0/*regparm*/, 
//zz            "ppc32g_calculate_cr7", &ppc32g_calculate_cr7,
//zz            args
//zz         );
//zz 
//zz // TODO
//zz // 02/02/05 - leaving definedness stuff 'till get memcheck working well.
//zz 
//zz    /* Exclude OP from definedness checking.  We're only
//zz       interested in DEP1 and DEP2. */
//zz //   call->Iex.CCall.cee->mcx_mask = 1;
//zz 
//zz    return call;
//zz }
//zz 
//zz /* Calculate XER_OV flag */
//zz static IRExpr* mk_ppc32g_calculate_xer_ov ( UInt op, IRExpr* res,
//zz                                             IRExpr* argL, IRExpr* argR )
//zz {
//zz    IRExpr** args;
//zz    IRExpr*  call;
//zz    vassert(op < PPC32G_FLAG_OP_NUMBER);
//zz    vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
//zz    vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
//zz    vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);
//zz 
//zz    args = mkIRExprVec_4( mkU32(op), res, argL, argR );
//zz 
//zz    call
//zz       = mkIRExprCCall(
//zz            Ity_I32,
//zz            0/*regparm*/,
//zz            "ppc32g_calculate_xer_ov", &ppc32g_calculate_xer_ov,
//zz            args
//zz         );
//zz    return binop(Iop_And32, mkU32(1), call);
//zz }

//uu /* Calculate XER_CA flag.  RES is the result of applying OP to ARGL
//uu    and ARGR, and OLDCA is the old carry flag.  The latter may be zero
//uu    if it is known that OP does not need to consult it. */
//uu 
//uu static IRExpr* mk_ppc32g_calculate_xer_ca ( UInt op, 
//uu                                             IRExpr* res,
//uu                                             IRExpr* argL, 
//uu                                             IRExpr* argR,
//uu                                             IRExpr* oldca )
//uu {
//uu    IRExpr** args;
//uu    IRExpr*  call;
//uu    vassert(op < PPC32G_FLAG_OP_NUMBER);
//uu    vassert(typeOfIRExpr(irbb->tyenv,res)   == Ity_I32);
//uu    vassert(typeOfIRExpr(irbb->tyenv,argL)  == Ity_I32);
//uu    vassert(typeOfIRExpr(irbb->tyenv,argR)  == Ity_I32);
//uu    vassert(typeOfIRExpr(irbb->tyenv,oldca) == Ity_I32);
//uu 
//uu    args = mkIRExprVec_5( mkU32(op), res, argL, argR, oldca );
//uu 
//uu    call
//uu       = mkIRExprCCall(
//uu            Ity_I32,
//uu            0/*regparm*/,
//uu            "ppc32g_calculate_xer_ca", &ppc32g_calculate_xer_ca,
//uu            args
//uu         );
//uu    return binop(Iop_And32, mkU32(1), call);
//uu }


/* Set the CR0 flags following an arithmetic operation.
   (Condition Register CR0 Field Definition, PPC32 p60)
*/
static void set_CR0 ( IRExpr* result )
{
   vassert(typeOfIRExpr(irbb->tyenv,result) == Ity_I32);
   putCR321( 0, unop(Iop_32to8,
                        binop(Iop_CmpORD32S, result, mkU32(0))) );
   putCR0( 0, getXER_SO() );
}


/* RES is the result of doing OP on ARGL and ARGR.  Set %XER.OV and
   %XER.SO accordingly. */

static void set_XER_OV( UInt op, IRExpr* res,
                                 IRExpr* argL, IRExpr* argR )
{
   IRTemp  t64;
   IRExpr* xer_ov;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);

#  define INT32_MIN 0x80000000

#  define XOR2(_aa,_bb) \
      binop(Iop_Xor32,(_aa),(_bb))

#  define XOR3(_cc,_dd,_ee) \
      binop(Iop_Xor32,binop(Iop_Xor32,(_cc),(_dd)),(_ee))

#  define AND3(_ff,_gg,_hh) \
      binop(Iop_And32,binop(Iop_And32,(_ff),(_gg)),(_hh))

#define NOT(_jj) \
      unop(Iop_Not32, (_jj))

   switch (op) {

      case /* 0  */ PPC32G_FLAG_OP_ADD:
      case /* 1  */ PPC32G_FLAG_OP_ADDE:
         /* (argL^argR^-1) & (argL^res) & (1<<31)  ?1:0 */
         // i.e. ((both_same_sign) & (sign_changed) & (sign_mask))
         xer_ov 
            = AND3( XOR3(argL,argR,mkU32(-1)),
                    XOR2(argL,res),
                    mkU32(INT32_MIN) );
	 /* xer_ov can only be 0 or 1<<31 */
         xer_ov 
            = binop(Iop_Shr32, xer_ov, mkU8(31) );
         break;

      case /* 2  */ PPC32G_FLAG_OP_DIVW:
         /* (argL == INT32_MIN && argR == -1) || argR == 0 */
         xer_ov
            = mkOR1(
                 mkAND1( 
                    binop(Iop_CmpEQ32, argL, mkU32(INT32_MIN)),
                    binop(Iop_CmpEQ32, argR, mkU32(-1)) 
                 ),
                 binop(Iop_CmpEQ32, argR, mkU32(0) ) 
              );
         xer_ov 
            = unop(Iop_1Uto32, xer_ov);
         break;

      case /* 3  */ PPC32G_FLAG_OP_DIVWU:
         /* argR == 0 */
         xer_ov 
            = unop(Iop_1Uto32, binop(Iop_CmpEQ32, argR, mkU32(0)));
         break;

      case /* 4  */ PPC32G_FLAG_OP_MULLW:
         /* OV true if result can't be represented in 32 bits
            i.e sHi != sign extension of sLo */
         t64 = newTemp(Ity_I64);
         assign( t64, binop(Iop_MullS32, argL, argR) );
         xer_ov 
            = binop( Iop_CmpNE32,
                     unop(Iop_64HIto32, mkexpr(t64)),
                     binop( Iop_Sar32, 
                            unop(Iop_64to32, mkexpr(t64)), 
                            mkU8(31))
              );
         xer_ov
            = unop(Iop_1Uto32, xer_ov);
         break;

      case /* 5  */ PPC32G_FLAG_OP_NEG:
         /* argL == INT32_MIN */
         xer_ov
            = unop( Iop_1Uto32, 
                    binop(Iop_CmpEQ32, argL, mkU32(INT32_MIN)) );
         break;

      case /* 6  */ PPC32G_FLAG_OP_SUBF:
      case /* 7  */ PPC32G_FLAG_OP_SUBFC:
      case /* 8  */ PPC32G_FLAG_OP_SUBFE:
         /* ((~argL)^argR^-1) & ((~argL)^res) & (1<<31) ?1:0; */
         xer_ov 
            = AND3( XOR3(NOT(argL),argR,mkU32(-1)),
                    XOR2(NOT(argL),res),
                    mkU32(INT32_MIN) );
	 /* xer_ov can only be 0 or 1<<31 */
         xer_ov 
            = binop(Iop_Shr32, xer_ov, mkU8(31) );
         break;

      default: 
         vex_printf("set_XER_OV: op = %d\n", op);
         vpanic("set_XER_OV(ppc32)");
   }
 
   /* xer_ov MUST denote either 0 or 1, no other value allowed */
   stmt( IRStmt_Put( OFFB_XER_OV, unop(Iop_32to8, xer_ov) ) );

   /* Update the summary overflow */
   stmt( IRStmt_Put( 
            OFFB_XER_SO,
            binop(Iop_Or8, IRExpr_Get( OFFB_XER_SO, Ity_I8 ),
                           IRExpr_Get( OFFB_XER_OV, Ity_I8 ) )
       ));

#  undef INT32_MIN
#  undef AND3
#  undef XOR3
#  undef XOR2
#  undef NOT
}


/* RES is the result of doing OP on ARGL and ARGR with the old %XER.CA
   value being OLDCA.  Set %XER.CA accordingly. */

static void set_XER_CA( UInt op, 
                        IRExpr* res,
                        IRExpr* argL, 
                        IRExpr* argR,
                        IRExpr* oldca )
{
   IRExpr* xer_ca;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res)   == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL)  == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR)  == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,oldca) == Ity_I32);

   /* Incoming oldca is assumed to hold the values 0 or 1 only.  This
      seems reasonable given that it's always generated by
      get_XER_CA(), which masks it accordingly.  In any case it being
      0 or 1 is an invariant of the ppc32 guest state representation;
      if it has any other value, that invariant has been violated. */

   switch (op) {

      case /* 0 */ PPC32G_FLAG_OP_ADD:
         /* res <u argL */
         xer_ca
            = unop(Iop_1Uto32, binop(Iop_CmpLT32U, res, argL));
         break;

      case /* 1 */ PPC32G_FLAG_OP_ADDE:
         /* res <u argL || (old_ca==1 && res==argL) */
         xer_ca 
            = mkOR1( 
                 binop(Iop_CmpLT32U, res, argL),
                 mkAND1( 
                    binop(Iop_CmpEQ32, oldca, mkU32(1)),
                    binop(Iop_CmpEQ32, res, argL) 
                 ) 
              );
         xer_ca 
            = unop(Iop_1Uto32, xer_ca);
         break;

      case /* 8 */ PPC32G_FLAG_OP_SUBFE:
         /* res <u argR || (old_ca==1 && res==argR) */
         xer_ca 
            = mkOR1( 
                 binop(Iop_CmpLT32U, res, argR),
                 mkAND1( 
                    binop(Iop_CmpEQ32, oldca, mkU32(1)),
                    binop(Iop_CmpEQ32, res, argR) 
                 ) 
              );
         xer_ca 
            = unop(Iop_1Uto32, xer_ca);
         break;

      case /* 7 */ PPC32G_FLAG_OP_SUBFC:
      case /* 9 */ PPC32G_FLAG_OP_SUBFI:
         /* res <=u argR */
         xer_ca
            = unop(Iop_1Uto32, binop(Iop_CmpLE32U, res, argR));
         break;

      case /* 10 */ PPC32G_FLAG_OP_SRAW:
         /* The shift amount is guaranteed to be in 0 .. 63 inclusive.
            If it is <= 31, behave like SRAWI; else XER.CA is the sign
            bit of argL. */
	/* This term valid for shift amount < 32 only */
         xer_ca
            = binop(
                 Iop_And32,
                 binop(Iop_Sar32, argL, mkU8(31)),
                 binop( Iop_And32,
                        argL,
                        binop( Iop_Sub32,
                               binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,argR)),
                               mkU32(1) )
                 )
              );
         xer_ca 
            = IRExpr_Mux0X(
                 /* shift amt > 31 ? */
                 unop(Iop_1Uto8, binop(Iop_CmpLT32U, mkU32(31), argR)),
                 /* no -- be like srawi */
                 unop(Iop_1Uto32, binop(Iop_CmpNE32, xer_ca, mkU32(0))),
                 /* yes -- get sign bit of argL */
                 binop(Iop_Shr32, argL, mkU8(31))
              );
         break;

      case /* 11 */ PPC32G_FLAG_OP_SRAWI:
         /* xer_ca is 1 iff src was negative and bits_shifted_out != 
            0.  Since the shift amount is known to be in the range
            0 .. 31 inclusive the following seems viable:
            xer.ca == 1 iff the following is nonzero:
               (argL >>s 31)           -- either all 0s or all 1s
               & (argL & (1<<argR)-1)  -- the stuff shifted out */
         xer_ca
            = binop(
                 Iop_And32,
                 binop(Iop_Sar32, argL, mkU8(31)),
                 binop( Iop_And32,
                        argL,
                        binop( Iop_Sub32,
                               binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,argR)),
                               mkU32(1) )
                 )
              );
         xer_ca 
            = unop(Iop_1Uto32, binop(Iop_CmpNE32, xer_ca, mkU32(0)));
         break;

      default: 
         vex_printf("set_XER_CA: op = %d\n", op);
         vpanic("set_XER_CA(ppc32)");
   }

   /* xer_ca MUST denote either 0 or 1, no other value allowed */
   stmt( IRStmt_Put( OFFB_XER_CA, unop(Iop_32to8, xer_ca) ) );
}

static IRExpr* /* :: Ity_I32 */ get_XER_CA ( void )
{
   return binop( Iop_And32,
                 unop( Iop_8Uto32, 
                       IRExpr_Get(OFFB_XER_CA, Ity_I8) ),
                 mkU32(1) );
}



/*------------------------------------------------------------*/
/*--- Abstract register interface                         --- */
/*------------------------------------------------------------*/

/* Get a masked word from the given reg */
static IRExpr* getReg_masked ( PPC32SPR reg, UInt mask )
{
   IRTemp val = newTemp(Ity_I32);
   vassert( reg < PPC32_SPR_MAX );
    
   switch (reg) {

   case PPC32_SPR_FPSCR: {
      vassert((mask & 0x3)    == 0x3    || (mask & 0x3)    == 0x0);
      vassert((mask & 0xF000) == 0xF000 || (mask & 0xF000) == 0x0);
      /* all masks now refer to valid fields */
      
      /* Vex-generated code expects to run with the FPSCR set as follows:
	 all exceptions masked, round-to-nearest.
	 This corresponds to a FPSCR value of 0x0. */

      /* We're only keeping track of the rounding mode,
	 so if the mask isn't asking for this, just return 0x0 */
      if (mask & 0x3) {
         assign( val, IRExpr_Get(OFFB_FPROUND, Ity_I32) );
      } else {
         assign( val, mkU32(0x0) );
      }
      break;      
   }

//zz    case PPC32_SPR_VRSAVE:
//zz       assign( val, IRExpr_Get(OFFB_VRSAVE, Ity_I32) );
//zz       break;
//zz 
//zz    case PPC32_SPR_VSCR:
//zz       // All other bits are 'Reserved'. Returning zero for these bits.
//zz       mask = mask & 0x00010001;
//zz       assign( val, IRExpr_Get(OFFB_VSCR, Ity_I32) );
//zz       break;

   default:
      vpanic("getReg(ppc32)");
   }

   if (mask != 0xFFFFFFFF) {
      return binop(Iop_And32, mkexpr(val), mkU32(mask));
   } else {
      return mkexpr(val);
   }
}

//zz /* Get word from the given reg */
//zz static IRExpr* getReg ( PPC32SPR reg )
//zz {
//zz    vassert( reg < PPC32_SPR_MAX );
//zz    return getReg_masked( reg, 0xFFFFFFFF );
//zz }
//zz 
//zz /* Get a right-shifted nibble from given reg[field_idx]
//zz    returns zero padded word */
//zz static IRExpr* getReg_field ( PPC32SPR reg, UInt field_idx )
//zz {
//zz    IRExpr* fld;
//zz    vassert( field_idx < 8 );
//zz    vassert( reg < PPC32_SPR_MAX );
//zz    
//zz    fld = getReg_masked( reg, (0xF << (field_idx*4)) );
//zz    
//zz    if (field_idx != 0) {
//zz       fld = binop(Iop_Shr32, fld, mkU8(toUChar(field_idx * 4)));
//zz    }
//zz    return fld;
//zz }
//zz 
//zz /* Get a right-shifted bit from given reg[bit_idx]
//zz    returns zero padded word */
//zz static IRExpr* getReg_bit ( PPC32SPR reg, UInt bit_idx )
//zz {
//zz    IRExpr* val;
//zz    vassert( bit_idx < 32 );
//zz    vassert( reg < PPC32_SPR_MAX );
//zz    
//zz    val = getReg_masked( reg, 1<<bit_idx );
//zz 
//zz    if (bit_idx != 0) {
//zz       val = binop(Iop_Shr32, val, mkU8(toUChar(bit_idx)));
//zz    }
//zz    return val;
//zz }



/* Write masked src to the given reg */
static void putReg_masked ( PPC32SPR reg, IRExpr* src, UInt mask )
{
   vassert( reg < PPC32_SPR_MAX );
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   
   switch (reg) {
//zz    case PPC32_SPR_CIA:
//zz       vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
//zz       stmt( IRStmt_Put( OFFB_CIA, src ) );
//zz       break;

   case PPC32_SPR_FPSCR:
      vassert((mask & 0x3)    == 0x3    || (mask & 0x3)    == 0x0);
      vassert((mask & 0xF000) == 0xF000 || (mask & 0xF000) == 0x0);
      /* all masks now refer to valid fields */

      /* Allow writes to Rounding Mode */
      if (mask & 0x3) {
         stmt( IRStmt_Put( OFFB_FPROUND,
                           binop(Iop_And32, src, mkU32(0x3)) ));
      }

      /*
        Give EmWarn for attempted writes to:
         - Exception Controls
         - Non-IEEE Mode
      */
      if (mask & 0xFC) {  // Exception Control, Non-IEE mode
         VexEmWarn ew = EmWarn_PPC32exns;

         /* If any of the src::exception_control bits are actually set,
            side-exit to the next insn, reporting the warning,
            so that Valgrind's dispatcher sees the warning. */
         put_emwarn( mkU32(ew) );
         stmt( 
            IRStmt_Exit(
               binop(Iop_CmpNE32, mkU32(ew), mkU32(EmWarn_NONE)),
               Ijk_EmWarn,
               IRConst_U32(guest_CIA_curr_instr + 4)
               )
            );
      }

      /*
        Ignore all other writes
      */
      break;

//zz    case PPC32_SPR_VRSAVE:
//zz       vassert(mask == 0xFFFFFFFF);    // Only ever need whole reg
//zz       stmt( IRStmt_Put( OFFB_VRSAVE, src ) );
//zz       break;
//zz 
//zz    case PPC32_SPR_VSCR:
//zz //CAB: There are only 2 valid bits in VSCR - maybe split into two vars...
//zz 
//zz       // All other bits are 'Reserved'. Ignoring writes to these bits.
//zz       stmt( IRStmt_Put( OFFB_VSCR,
//zz                binop(Iop_Or32,
//zz                      binop(Iop_And32, src, mkU32(mask & 0x00010001)),
//zz                      getReg_masked( PPC32_SPR_VSCR, (~mask & 0x00010001) ))));
//zz       break;
//zz    }

   default:
      vpanic("putReg(ppc32)");
   }
}

//zz /* Write src to the given reg */
//zz static void putReg ( PPC32SPR reg, IRExpr* src )
//zz {
//zz    vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
//zz    vassert( reg < PPC32_SPR_MAX );
//zz    putReg_masked( reg, src, 0xFFFFFFFF );
//zz }

static void putSPR ( PPC32SPR reg, IRExpr* src )
{
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   switch (reg) {
      case PPC32_SPR_CIA: 
         stmt( IRStmt_Put( OFFB_CIA, src ) );
         break;
      case PPC32_SPR_LR: 
         stmt( IRStmt_Put( OFFB_LR, src ) );
         break;
      case PPC32_SPR_CTR: 
         stmt( IRStmt_Put( OFFB_CTR, src ) );
         break;
      case PPC32_SPR_VRSAVE: 
         stmt( IRStmt_Put( OFFB_VRSAVE, src ) );
         break;
      default:
         vpanic("putSPR(ppc32)");
   }
}

static IRExpr* /* :: Ity_I32 */ getSPR ( PPC32SPR reg )
{
   switch (reg) {
      case PPC32_SPR_LR: 
         return IRExpr_Get( OFFB_LR, Ity_I32 );
      case PPC32_SPR_CTR: 
         return IRExpr_Get( OFFB_CTR, Ity_I32 );
      case PPC32_SPR_VRSAVE: 
         return IRExpr_Get( OFFB_VRSAVE, Ity_I32 );
      default:
         vpanic("getSPR(ppc32)");
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

//zz    UInt flag_op = PPC32G_FLAG_OP_NUMBER;
   Bool do_rc = False;

   assign( Ra, getIReg(Ra_addr) );
   assign( Rb, getIReg(Rb_addr) );         // XO-Form: Rd, Ra, Rb
   EXTS_SIMM = extend_s_16to32(SIMM_16);   // D-Form:  Rd, Ra, EXTS(SIMM)

//zz    assign( xer_ca, getReg_bit( PPC32_SPR_XER, SHIFT_XER_CA ) );

   switch (opc1) {
   /* D-Form */
   case 0x0C: // addic  (Add Immediate Carrying, PPC32 p351
      DIP("addic r%d,r%d,0x%x\n", Rd_addr, Ra_addr, EXTS_SIMM);
      assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
      set_XER_CA( PPC32G_FLAG_OP_ADD, 
                  mkexpr(Rd), mkexpr(Ra), mkU32(EXTS_SIMM),
                  mkU32(0)/*old xer.ca, which is ignored*/ );
      break;
    
   case 0x0D: // addic. (Add Immediate Carrying and Record, PPC32 p352)
      DIP("addic. r%d,r%d,0x%x\n", Rd_addr, Ra_addr, EXTS_SIMM);
      assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
      set_XER_CA( PPC32G_FLAG_OP_ADD, 
                  mkexpr(Rd), mkexpr(Ra), mkU32(EXTS_SIMM), 
                  mkU32(0)/*old xer.ca, which is ignored*/ );
      do_rc = True;  // Always record to CR
      flag_Rc = 1;
      break;

   case 0x0E: // addi   (Add Immediate, PPC32 p350)
      // li rD,val   == addi rD,0,val
      // la disp(rA) == addi rD,rA,disp
      if ( Ra_addr == 0 ) {
         DIP("li r%d,%d\n", Rd_addr, EXTS_SIMM);
         assign( Rd, mkU32(EXTS_SIMM) );
      } else {
         DIP("addi r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
         assign( Rd, binop( Iop_Add32, mkexpr(Ra), mkU32(EXTS_SIMM) ) );
      }
      break;

   case 0x0F: // addis  (Add Immediate Shifted, PPC32 p353)
      // lis rD,val == addis rD,0,val
      if ( Ra_addr == 0 ) {
         DIP("lis r%d,%d\n", Rd_addr, SIMM_16);
         assign( Rd, mkU32(SIMM_16 << 16) );
      } else {
         DIP("addis r%d,r%d,0x%x\n", Rd_addr, Ra_addr, SIMM_16);
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
      set_XER_CA( PPC32G_FLAG_OP_SUBFI, 
                  mkexpr(Rd), mkexpr(Ra), mkU32(EXTS_SIMM),
                  mkU32(0)/*old xer.ca, which is ignored*/ );
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
            set_XER_OV( PPC32G_FLAG_OP_ADD,
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x00A: // addc      (Add Carrying, PPC32 p348)
         DIP("addc%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
         set_XER_CA( PPC32G_FLAG_OP_ADD, 
                     mkexpr(Rd), mkexpr(Ra), mkexpr(Rb),
                     mkU32(0)/*old xer.ca, which is ignored*/ );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADD, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
         
      case 0x08A: { // adde      (Add Extended, PPC32 p349)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         DIP("adde%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rA + rB + XER[CA]
         assign( old_xer_ca, get_XER_CA() );
         assign( Rd, binop(Iop_Add32, mkexpr(Ra),
                           binop(Iop_Add32, mkexpr(Rb), mkexpr(old_xer_ca))) );
         set_XER_CA( PPC32G_FLAG_OP_ADDE, 
                     mkexpr(Rd), mkexpr(Ra), mkexpr(Rb),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADDE, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
      }

      case 0x0EA: { // addme      (Add to Minus One Extended, PPC32 p354)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(addme,Rb_addr)\n");
            return False;
         }
         DIP("addme%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rA + (-1) + XER[CA]
         // => Just another form of adde
         assign( old_xer_ca, get_XER_CA() );
         assign( Rd, binop(Iop_Add32, mkexpr(Ra),
                           binop(Iop_Add32, mkU32(-1), mkexpr(old_xer_ca)) ));
         set_XER_CA( PPC32G_FLAG_OP_ADDE,
                     mkexpr(Rd), mkexpr(Ra), mkU32(-1),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADDE, 
                        mkexpr(Rd), mkexpr(Ra), mkU32(-1) );
         }
         break;
      }

      case 0x0CA: { // addze      (Add to Zero Extended, PPC32 p355)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(addze,Rb_addr)\n");
            return False;
         }
         DIP("addze%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rA + (0) + XER[CA]
         // => Just another form of adde
         assign( old_xer_ca, get_XER_CA() );
         assign( Rd, binop(Iop_Add32, mkexpr(Ra), mkexpr(old_xer_ca)) );
         set_XER_CA( PPC32G_FLAG_OP_ADDE, 
                     mkexpr(Rd), mkexpr(Ra), mkU32(0), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADDE, 
                        mkexpr(Rd), mkexpr(Ra), mkU32(0) );
         }
         break;
      }

      case 0x1EB: // divw       (Divide Word, PPC32 p388)
         DIP("divw%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         assign( Rd, binop(Iop_DivS32, mkexpr(Ra), mkexpr(Rb)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_DIVW, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
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
            set_XER_OV( PPC32G_FLAG_OP_DIVWU, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
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
            set_XER_OV( PPC32G_FLAG_OP_MULLW, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
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
            set_XER_OV( PPC32G_FLAG_OP_NEG, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x028: // subf       (Subtract From, PPC32 p537)
         DIP("subf%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rB - rA
         assign( Rd, binop(Iop_Sub32, mkexpr(Rb), mkexpr(Ra)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBF, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;

      case 0x008: // subfc      (Subtract from Carrying, PPC32 p538)
         DIP("subfc%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = rB - rA
         assign( Rd, binop(Iop_Sub32, mkexpr(Rb), mkexpr(Ra)) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFC, 
                     mkexpr(Rd), mkexpr(Ra), mkexpr(Rb),
                     mkU32(0)/*old xer.ca, which is ignored*/ );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFC, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
         
      case 0x088: {// subfe      (Subtract from Extended, PPC32 p539)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         DIP("subfe%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr, Rb_addr);
         // rD = (log not)rA + rB + XER[CA]
         assign( old_xer_ca, get_XER_CA() );
         assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
                           binop(Iop_Add32, mkexpr(Rb), mkexpr(old_xer_ca))) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFE, 
                     mkexpr(Rd), mkexpr(Ra), mkexpr(Rb), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFE, 
                        mkexpr(Rd), mkexpr(Ra), mkexpr(Rb) );
         }
         break;
      }

      case 0x0E8: { // subfme     (Subtract from Minus One Extended, PPC32 p541)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(subfme,Rb_addr)\n");
            return False;
         }
         DIP("subfme%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr);
         // rD = (log not)rA + (-1) + XER[CA]
         // => Just another form of subfe
         assign( old_xer_ca, get_XER_CA() );
         assign( Rd, binop(Iop_Add32, unop(Iop_Not32, mkexpr(Ra)),
                           binop(Iop_Add32, mkU32(-1), mkexpr(old_xer_ca))) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFE,
                     mkexpr(Rd), mkexpr(Ra), mkU32(-1),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFE, 
                        mkexpr(Rd), mkexpr(Ra), mkU32(-1) );
         }
         break;
      }

      case 0x0C8: { // subfze     (Subtract from Zero Extended, PPC32 p542)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (Rb_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(subfze,Rb_addr)\n");
            return False;
         }
         DIP("subfze%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_Rc ? "." : "",
             Rd_addr, Ra_addr);
         // rD = (log not)rA + (0) + XER[CA]
         // => Just another form of subfe
         assign( old_xer_ca, get_XER_CA() );
         assign( Rd, binop(Iop_Add32,
                           unop(Iop_Not32, mkexpr(Ra)), mkexpr(old_xer_ca)) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFE,
                     mkexpr(Rd), mkexpr(Ra), mkU32(0), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFE,
                        mkexpr(Rd), mkexpr(Ra), mkU32(0) );
         }
         break;
      }

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
      set_CR0( mkexpr(Rd) );
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
   UInt  UIMM_16 =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
   
   /* X-Form */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF;  /* theInstr[1:10]  */
   UChar b0      = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */
   
   UInt EXTS_SIMM = 0;
   IRTemp Ra      = newTemp(Ity_I32);
   IRTemp Rb      = newTemp(Ity_I32);
//uu   IRTemp xer_so  = newTemp(Ity_I32);
//uu   IRTemp cr7     = newTemp(Ity_I32);
//uu   IRTemp mux1    = newTemp(Ity_I32);
//uu   IRTemp mux2    = newTemp(Ity_I32);
//uu   IRExpr* irx_cmp_lt;
//uu   IRExpr* irx_cmp_eq;

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
         EXTS_SIMM = extend_s_16to32(UIMM_16);
         DIP("cmp cr%d,r%d,%d\n", crfD, Ra_addr, EXTS_SIMM);
         putCR321( crfD, unop(Iop_32to8,
                              binop(Iop_CmpORD32S, mkexpr(Ra), 
                                                   mkU32(EXTS_SIMM))) );
         putCR0( crfD, getXER_SO() );
         break;

      case 0x0A: // cmpli (Compare Logical Immediate, PPC32 p370)
         DIP("cmpli cr%d,r%d,0x%x\n", crfD, Ra_addr, UIMM_16);
         putCR321( crfD, unop(Iop_32to8,
                              binop(Iop_CmpORD32U, mkexpr(Ra), 
                                                   mkU32(UIMM_16))) );
         putCR0( crfD, getXER_SO() );
         break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_cmp(PPC32)(0x1F,b0)\n");
         return False;
      }
      assign( Rb, getIReg(Rb_addr) );
//zz       irx_cmp_eq = binop(Iop_CmpEQ32, mkexpr(Ra), mkexpr(Rb));

      switch (opc2) {
         case 0x000: // cmp (Compare, PPC32 p367)
            DIP("cmp cr%d,r%d,r%d\n", crfD, Ra_addr, Rb_addr);
            putCR321( crfD, unop(Iop_32to8,
                                 binop(Iop_CmpORD32S, mkexpr(Ra), mkexpr(Rb))) );
            putCR0( crfD, getXER_SO() );
            break;
         
         case 0x020: // cmpl (Compare Logical, PPC32 p369)
            DIP("cmpl cr%d,r%d,r%d\n", crfD, Ra_addr, Rb_addr);
            putCR321( crfD, unop(Iop_32to8,
                                 binop(Iop_CmpORD32U, mkexpr(Ra), mkexpr(Rb))) );
            putCR0( crfD, getXER_SO() );
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
   
//zz    irx_cmp_lt = unop(Iop_1Uto8, irx_cmp_lt);
//zz    irx_cmp_eq = unop(Iop_1Uto8, irx_cmp_eq);
//zz 
//zz    // mux_shift_bit = (argL < argR) ? LT : GT (or EQ...)
//zz    assign( mux1, IRExpr_Mux0X( irx_cmp_lt, mkU32(SHIFT_CR_GT), mkU32(SHIFT_CR_LT) ));
//zz 
//zz    // mux_shift_bit = (argL == argR) ? EQ : GT|LT
//zz    assign( mux2, IRExpr_Mux0X( irx_cmp_eq, mkexpr(mux1), mkU32(SHIFT_CR_EQ) ));
//zz    
//zz    assign( xer_so, getReg_bit( PPC32_SPR_XER, SHIFT_XER_SO ) );
//zz    assign( cr7, binop(Iop_Or32, mkexpr(mux2), mkexpr(xer_so)) );
//zz    putReg_field( PPC32_SPR_CR, mkexpr(cr7), 7-crfD );
//zz    return True;
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
//uu   IRTemp Sign = newTemp(Ity_I32);
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
         assign( Ra, unop(Iop_8Sto32, unop(Iop_32to8, mkexpr(Rs))) );
         break;

      case 0x39A: // extsh (Extend Sign Half Word, PPC32 p398)
         if (Rb_addr!=0) {
            vex_printf("dis_int_logic(PPC32)(extsh,Rb_addr)\n");
            return False;
         }
         DIP("extsh%s r%d,r%d\n",
             flag_Rc ? "." : "", Ra_addr, Rs_addr);
         assign( Ra, unop(Iop_16Sto32, unop(Iop_32to16, mkexpr(Rs))) );
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
         if ((!flag_Rc) && Rs_addr == Rb_addr) {
            DIP("mr r%d,r%d\n", Ra_addr, Rs_addr);
            assign( Ra, mkexpr(Rs) );
         } else {
            DIP("or%s r%d,r%d,r%d\n",
                flag_Rc ? "." : "", Ra_addr, Rs_addr, Rb_addr);
            assign( Ra, binop(Iop_Or32, mkexpr(Rs), mkexpr(Rb)) );
         }
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
      set_CR0( mkexpr(Ra) );
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
   IRTemp Rs = newTemp(Ity_I32);
   IRTemp Ra = newTemp(Ity_I32);
   IRTemp Rb = newTemp(Ity_I32);
   
   assign( Rs, getIReg(Rs_addr) );
   assign( Rb, getIReg(Rb_addr) );
      
   switch (opc1) {
   case 0x14: 
      // rlwimi (Rotate Left Word Immediate then Mask Insert, PPC32 p500)
      DIP("rlwimi%s r%d,r%d,%d,%d,%d\n", flag_Rc ? "." : "",
          Ra_addr, Rs_addr, sh_imm, MaskBegin, MaskEnd);
      // Ra = (ROTL(Rs, Imm) & mask) | (Ra & ~mask);
      assign( Ra, binop(Iop_Or32,
                        binop(Iop_And32, mkU32(mask),
                              ROTL32(mkexpr(Rs), mkU32(sh_imm))),
                        binop(Iop_And32, getIReg(Ra_addr), mkU32(~mask))) );
      break;

   case 0x15: 
      // rlwinm (Rotate Left Word Immediate then AND with Mask, PPC32 p501)
      DIP("rlwinm%s r%d,r%d,%d,%d,%d\n", flag_Rc ? "." : "",
          Ra_addr, Rs_addr, sh_imm, MaskBegin, MaskEnd);
      // Ra = ROTL(Rs, Imm) & mask
      assign( Ra, binop(Iop_And32, ROTL32(mkexpr(Rs), mkU32(sh_imm)), 
                                   mkU32(mask)) );
      break;

   case 0x17: 
      // rlwnm (Rotate Left Word then AND with Mask, PPC32 p503
      DIP("rlwnm%s r%d,r%d,r%d,%d,%d\n", flag_Rc ? "." : "",
          Ra_addr, Rs_addr, Rb_addr, MaskBegin, MaskEnd);
      // Ra = ROTL(Rs, Rb[0-4]) & mask
      // note, ROTL32 does the masking, so we don't do it here
      assign( Ra, binop(Iop_And32, ROTL32(mkexpr(Rs), mkexpr(Rb)), 
                                   mkU32(mask)) );
      break;

   default:
      vex_printf("dis_int_rot(PPC32)(opc1)\n");
      return False;
   }

   putIReg( Ra_addr, mkexpr(Ra) );
   if (flag_Rc) {
      set_CR0( mkexpr(Ra) );
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
      DIP("lbz r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_8Uto32,
                             loadBE(Ity_I8, mkexpr(EA_imm))) );
      break;
      
   case 0x23: // lbzu (Load B & Zero with Update, PPC32 p434)
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lbzu,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lbzu r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_8Uto32,
                             loadBE(Ity_I8, mkexpr(EA_imm))) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;
      
   case 0x2A: // lha (Load HW Algebraic, PPC32 p445)
      DIP("lha r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Sto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      break;

//zz    case 0x2B: // lhau (Load HW Algebraic with Update, PPC32 p446)
//zz       if (Ra_addr == 0 || Ra_addr == Rd_addr) {
//zz          vex_printf("dis_int_load(PPC32)(lhau,Ra_addr|Rd_addr)\n");
//zz          return False;
//zz       }
//zz       DIP("lhau r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
//zz       putIReg( Rd_addr, unop(Iop_16Sto32,
//zz                              loadBE(Ity_I16, mkexpr(EA_imm))) );
//zz       putIReg( Ra_addr, mkexpr(EA_imm) );
//zz       break;
      
   case 0x28: // lhz (Load HW & Zero, PPC32 p450)
      DIP("lhz r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Uto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      break;
      
   case 0x29: // lhzu (Load HW & and Zero with Update, PPC32 p451)
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lhzu,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lhzu r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
      putIReg( Rd_addr, unop(Iop_16Uto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      putIReg( Ra_addr, mkexpr(EA_imm) );
      break;

   case 0x20: // lwz (Load W & Zero, PPC32 p460)
      DIP("lwz r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
      putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
      break;
      
   case 0x21: // lwzu (Load W & Zero with Update, PPC32 p461))
      if (Ra_addr == 0 || Ra_addr == Rd_addr) {
         vex_printf("dis_int_load(PPC32)(lwzu,Ra_addr|Rd_addr)\n");
         return False;
      }
      DIP("lwzu r%d,%d(r%d)\n", Rd_addr, exts_d_imm, Ra_addr);
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
         
//zz       case 0x177: // lhaux (Load HW Algebraic with Update Indexed, PPC32 p447)
//zz          if (Ra_addr == 0 || Ra_addr == Rd_addr) {
//zz             vex_printf("dis_int_load(PPC32)(lhaux,Ra_addr|Rd_addr)\n");
//zz             return False;
//zz          }
//zz          DIP("lhaux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
//zz          putIReg( Rd_addr, unop(Iop_16Sto32,
//zz                                 loadBE(Ity_I16, mkexpr(EA_reg))) );
//zz          putIReg( Ra_addr, mkexpr(EA_reg) );
//zz          break;
         
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

//zz       case 0x037: // lwzux (Load W & Zero with Update Indexed, PPC32 p462)
//zz          if (Ra_addr == 0 || Ra_addr == Rd_addr) {
//zz             vex_printf("dis_int_load(PPC32)(lwzux,Ra_addr|Rd_addr)\n");
//zz             return False;
//zz          }
//zz          DIP("lwzux r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
//zz          putIReg( Rd_addr, loadBE(Ity_I32, mkexpr(EA_reg)) );
//zz          putIReg( Ra_addr, mkexpr(EA_reg) );
//zz          break;
         
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
   UInt opc1    = ifieldOPC(theInstr);     /* theInstr[26:31] */
   UInt Rs_addr = ifieldRD(theInstr);      /* theInstr[21:25] */
   UInt Ra_addr = ifieldRA(theInstr);      /* theInstr[16:20] */
   
   /* D-Form */
   Int  simm16  = ifieldSIMM16(theInstr);  /* theInstr[0:15] */
   
   /* X-Form */
   UInt Rb_addr = ifieldRB(theInstr);      /* theInstr[11:15] */
   UInt opc2    = ifieldOPClo10(theInstr); /* theInstr[1:10]  */
   UInt b0      = ifieldBIT0(theInstr);    /* theInstr[0]     */
      
   IRTemp Ra      = newTemp(Ity_I32);
   IRTemp Ra_or_0 = newTemp(Ity_I32);
   IRTemp Rb      = newTemp(Ity_I32);
   IRTemp Rs      = newTemp(Ity_I32);
//   IRTemp Rs_8    = newTemp(Ity_I8);
//IRTemp Rs_16   = newTemp(Ity_I16);
   IRTemp EA_imm  = newTemp(Ity_I32);
   IRTemp EA_reg  = newTemp(Ity_I32);
   
   assign( Ra, getIReg(Ra_addr) );
   assign( Rb, getIReg(Rb_addr) );
   assign( Rs, getIReg(Rs_addr) );
   //assign( Rs_8, unop(Iop_32to8, mkexpr(Rs)) );
   //assign( Rs_16, unop(Iop_32to16, mkexpr(Rs)) );
   
   if (Ra_addr == 0) {
      assign( Ra_or_0, mkU32(0) );
   } else {
      assign( Ra_or_0, mkexpr(Ra) );
   }
   assign( EA_imm, binop(Iop_Add32, mkexpr(Ra_or_0), mkU32(simm16)) );
   
   switch (opc1) {
      case 0x26: // stb (Store B, PPC32 p509)
         DIP("stb r%d,%d(r%d)\n", Rs_addr, simm16, Ra_addr);
         storeBE( mkexpr(EA_imm), unop(Iop_32to8, mkexpr(Rs)) );
         break;
       
   case 0x27: // stbu (Store B with Update, PPC32 p510)
      if (Ra_addr == 0 ) {
         vex_printf("dis_int_store(PPC32)(stbu,Ra_addr)\n");
         return False;
      }
      DIP("stbu r%d,%d(r%d)\n", Rs_addr, simm16, Ra_addr);
      putIReg( Ra_addr, mkexpr(EA_imm) );
      storeBE( mkexpr(EA_imm), unop(Iop_32to8, mkexpr(Rs)) );
      break;

   case 0x2C: // sth (Store HW, PPC32 p522)
      DIP("sth r%d,%d(r%d)\n", Rs_addr, simm16, Ra_addr);
      storeBE( mkexpr(EA_imm), unop(Iop_32to16, mkexpr(Rs)) );
      break;
      
   case 0x2D: // sthu (Store HW with Update, PPC32 p524)
      if (Ra_addr == 0) {
         vex_printf("dis_int_store(PPC32)(sthu,Ra_addr)\n");
         return False;
      }
      DIP("sthu r%d,%d(r%d)\n", Rs_addr, simm16, Ra_addr);
      putIReg( Ra_addr, mkexpr(EA_imm) );
      storeBE( mkexpr(EA_imm), unop(Iop_32to16, mkexpr(Rs)) );
      break;

   case 0x24: // stw (Store W, PPC32 p530)
      DIP("stw r%d,%d(r%d)\n", Rs_addr, simm16, Ra_addr);
      storeBE( mkexpr(EA_imm), mkexpr(Rs) );
      break;

   case 0x25: // stwu (Store W with Update, PPC32 p534)
      if (Ra_addr == 0) {
         vex_printf("dis_int_store(PPC32)(stwu,Ra_addr)\n");
         return False;
      }
      DIP("stwu r%d,%d(r%d)\n", Rs_addr, simm16, Ra_addr);
      putIReg( Ra_addr, mkexpr(EA_imm) );
      storeBE( mkexpr(EA_imm), mkexpr(Rs) );
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
         putIReg( Ra_addr, mkexpr(EA_reg) );
         storeBE( mkexpr(EA_reg), unop(Iop_32to8, mkexpr(Rs)) );
         break;
         
      case 0x0D7: // stbx (Store B Indexed, PPC32 p512)
         DIP("stbx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), unop(Iop_32to8, mkexpr(Rs)) );
         break;
         
//zz       case 0x1B7: // sthux (Store HW with Update Indexed, PPC32 p525)
//zz          if (Ra_addr == 0) {
//zz             vex_printf("dis_int_store(PPC32)(sthux,Ra_addr)\n");
//zz             return False;
//zz          }
//zz          DIP("sthux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
//zz          putIReg( Ra_addr, mkexpr(EA_reg) );
//zz          storeBE( mkexpr(EA_reg), mkexpr(Rs_16) );
//zz          break;
         
      case 0x197: // sthx (Store HW Indexed, PPC32 p526)
         DIP("sthx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         storeBE( mkexpr(EA_reg), unop(Iop_32to16, mkexpr(Rs)) );
         break;
         
      case 0x0B7: // stwux (Store W with Update Indexed, PPC32 p535)
         if (Ra_addr == 0) {
            vex_printf("dis_int_store(PPC32)(stwux,Ra_addr)\n");
            return False;
         }
         DIP("stwux r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
         putIReg( Ra_addr, mkexpr(EA_reg) );
         storeBE( mkexpr(EA_reg), mkexpr(Rs) );
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



//zz /*
//zz   Integer Load/Store Multiple Instructions
//zz */
//zz static Bool dis_int_ldst_mult ( UInt theInstr )
//zz {
//zz    /* D-Form */
//zz    UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
//zz    UChar Rd_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
//zz    UChar Rs_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
//zz    UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
//zz    UInt  d_imm   =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */
//zz    
//zz    UInt exts_d_imm = extend_s_16to32(d_imm);
//zz    UInt reg_idx    = 0;
//zz    UInt offset     = 0;
//zz    
//zz    IRTemp Ra = newTemp(Ity_I32);
//zz    IRTemp EA = newTemp(Ity_I32);
//zz    
//zz    IRExpr* irx_addr;
//zz    
//zz    if (Ra_addr == 0) {
//zz       assign( EA, binop(Iop_Add32, mkU32(0), mkU32(exts_d_imm)) );
//zz    } else {
//zz       assign( Ra, getIReg(Ra_addr) );
//zz       assign( EA, binop(Iop_Add32, mkexpr(Ra), mkU32(exts_d_imm)) );
//zz    }
//zz    
//zz    switch (opc1) {
//zz    case 0x2E: // lmw (Load Multiple Word, PPC32 p454)
//zz vassert(1);
//zz 
//zz       if (Ra_addr >= Rd_addr) {
//zz          vex_printf("dis_int_ldst_mult(PPC32)(lmw,Ra_addr)\n");
//zz          return False;
//zz       }
//zz       DIP("lmw r%d,%d(r%d)\n", Rd_addr, (Int)d_imm, Ra_addr);
//zz       for (reg_idx = Rd_addr; reg_idx<=31; reg_idx++) {
//zz          irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(offset));
//zz          putIReg( reg_idx, loadBE(Ity_I32, irx_addr ) );
//zz          offset +=4;
//zz       }
//zz       break;
//zz       
//zz    case 0x2F: // stmw (Store Multiple Word, PPC32 p527)
//zz vassert(1);
//zz 
//zz       DIP("stmw r%d,%d(r%d)\n", Rs_addr, (Int)d_imm, Ra_addr);
//zz       for (reg_idx = Rs_addr; reg_idx<=31; reg_idx++) {
//zz          irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(offset));
//zz          storeBE( irx_addr, getIReg(reg_idx) );
//zz          offset +=4;
//zz       }
//zz       break;
//zz       
//zz    default:
//zz       vex_printf("dis_int_ldst_mult(PPC32)(opc1)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz 
//zz 
//zz /*
//zz   Integer Load/Store String Instructions
//zz */
//zz static Bool dis_int_ldst_str ( UInt theInstr )
//zz {
//zz    /* X-Form */
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar Ra_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar NumBytes = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UChar Rb_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
//zz    UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
//zz    
//zz    UInt reg_idx, bit_idx, n_byte;
//zz    UInt EA_offset = 0;
//zz    UInt n_regs, reg_first, reg_last;
//zz    
//zz    IRTemp Ra = newTemp(Ity_I32);
//zz //    IRTemp Rb = newTemp(Ity_I32);
//zz    IRTemp EA = newTemp(Ity_I32);
//zz    IRTemp b_EA = newTemp(Ity_I32);
//zz    IRExpr* irx_byte;
//zz    IRExpr* irx_shl;
//zz    
//zz    if (Ra_addr == 0) {
//zz       assign( b_EA, mkU32(0) );
//zz    } else {
//zz       assign( Ra, getIReg(Ra_addr) );
//zz       assign( b_EA, mkexpr(Ra) );
//zz    }    
//zz    
//zz    if (opc1 != 0x1F || b0 != 0) {
//zz       vex_printf("dis_int_ldst_str(PPC32)(opc1)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x255: // lswi (Load String Word Immediate, PPC32 p455)
//zz 
//zz       if (NumBytes == 8) {
//zz          /* Special case hack */
//zz          /* Rd = Mem[EA]; (Rd+1)%32 = Mem[EA+4] */
//zz          DIP("lswi r%d,r%d,%d\n", Rd_addr, Ra_addr, NumBytes);
//zz          putIReg( Rd_addr,          
//zz                   loadBE(Ity_I32, mkexpr(b_EA)) );
//zz 
//zz          putIReg( (Rd_addr+1) % 32, 
//zz                   loadBE(Ity_I32, binop(Iop_Add32, mkexpr(b_EA), mkU32(4))) );
//zz          return True;
//zz       }
//zz 
//zz       /* else too difficult */
//zz       return False;
//zz vassert(0);
//zz 
//zz       n_regs = (NumBytes / 4) + (NumBytes%4 == 0 ? 0:1); // ceil(nb/4)
//zz       reg_first = Rd_addr;
//zz       reg_last = Rd_addr + n_regs - 1;
//zz       
//zz       if (reg_last < reg_first) {
//zz          if (Ra_addr >= reg_first || Ra_addr <= reg_last) {
//zz             vex_printf("dis_int_ldst_str(PPC32)(lswi,Ra_addr,1)\n");
//zz             return False;
//zz          }
//zz       } else {
//zz          if (Ra_addr >= reg_first && Ra_addr <= reg_last) {
//zz             vex_printf("dis_int_ldst_str(PPC32)(lswi,Ra_addr,2)\n");
//zz             return False;
//zz          }
//zz       }
//zz       DIP("lswi r%d,r%d,%d\n", Rd_addr, Ra_addr, NumBytes);
//zz       
//zz       assign( EA, mkexpr(b_EA) );
//zz       
//zz       bit_idx = 0;
//zz       reg_idx = Rd_addr - 1;
//zz       n_byte = NumBytes;
//zz       if (n_byte == 0) { n_byte = 32; }
//zz       
//zz       for (; n_byte>0; n_byte--) {
//zz          if (bit_idx == 0) {
//zz             reg_idx++;
//zz             if (reg_idx == 32) reg_idx = 0;
//zz             putIReg( reg_idx, mkU32(0) );
//zz          }
//zz          irx_byte = loadBE(Ity_I8, binop(Iop_Add32,
//zz                                          mkexpr(EA),
//zz                                          mkU32(EA_offset)));
//zz          irx_shl = binop(Iop_Shl32, irx_byte, 
//zz                          mkU8(toUChar(24 - bit_idx)));
//zz          putIReg( reg_idx, binop(Iop_Or32, getIReg(reg_idx), irx_shl) );
//zz          bit_idx += 8;
//zz          if (bit_idx == 32) { bit_idx = 0; }
//zz          EA_offset++;
//zz       }
//zz       break;      
//zz 
//zz    case 0x215: // lswx (Load String Word Indexed, PPC32 p456)
//zz vassert(0);
//zz 
//zz       DIP("lswx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
//zz       return False;
//zz 
//zz    case 0x2D5: // stswi (Store String Word Immediate, PPC32 p528)
//zz 
//zz       if (NumBytes == 8) {
//zz          /* Special case hack */
//zz          /* Mem[EA] = Rd; Mem[EA+4] = (Rd+1)%32 */
//zz          DIP("stswi r%d,r%d,%d\n", Rs_addr, Ra_addr, NumBytes);
//zz 	 storeBE( mkexpr(b_EA), 
//zz                   getIReg(Rd_addr) );
//zz 	 storeBE( binop(Iop_Add32, mkexpr(b_EA), mkU32(4)), 
//zz                   getIReg((Rd_addr+1) % 32) );
//zz          return True;
//zz       }
//zz 
//zz       /* else too difficult */
//zz       return False;
//zz 
//zz vassert(0);
//zz 
//zz       DIP("stswi r%d,r%d,%d\n", Rs_addr, Ra_addr, NumBytes);
//zz       if (Ra_addr == 0) {
//zz          assign( EA, mkU32(0) );
//zz       } else {
//zz          assign( EA, mkexpr(b_EA) );
//zz       }
//zz       
//zz       n_byte = NumBytes;
//zz       if (n_byte == 0) { n_byte = 32; }
//zz       reg_idx = Rs_addr - 1;
//zz       bit_idx = 0;
//zz       
//zz       for (; n_byte>0; n_byte--) {
//zz          if (bit_idx == 0) {
//zz             reg_idx++;
//zz             if (reg_idx==32) reg_idx = 0;
//zz          }
//zz          irx_byte = unop(Iop_32to8,
//zz                          binop(Iop_Shr32,
//zz                                getIReg(reg_idx), 
//zz                                mkU8(toUChar(24 - bit_idx))));
//zz          storeBE( binop(Iop_Add32, mkexpr(EA), mkU32(EA_offset)),
//zz                   irx_byte );
//zz          
//zz          bit_idx += 8;
//zz          if (bit_idx == 32) { bit_idx = 0; }
//zz          EA_offset++;
//zz       }
//zz       break;
//zz 
//zz    case 0x295: // stswx (Store String Word Indexed, PPC32 p529)
//zz vassert(0);
//zz 
//zz       DIP("stswx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
//zz       return False;
//zz #if 0
//zz // CAB: Might something like this work ?
//zz // won't produce very nice code (ir_ctr will get _rather_ long...), but hey.
//zz // or perhaps arrays of IRTemp...
//zz    assign( NumBytes, AND(get(xer_bc), 0x1F) );
//zz    IRExpr* irx_ea;
//zz    IRExpr* irx_orig_byte;
//zz    IRExpr* irx_tostore;
//zz    IRExpr* ir_ctr = mkU8(0);
//zz    Uint EA_offset = 0;
//zz    UInt start = Rs_addr;
//zz    UInt reg_idx;
//zz    UInt i;
//zz    for (i=0; i<128; i++) {
//zz       bit_idx = (i % 4) * 8;
//zz       reg_idx = (i / 4) + start;
//zz       reg_idx = reg_idx % 32;
//zz       word = getIReg(reg_idx);
//zz       byte = get_byte(word, bit_idx);
//zz       
//zz       irx_ea = (EA + EA_offset);
//zz       irx_orig_byte = loadBE(Ity_I8, irx_ea);
//zz       irx_tostore = IRExpr_Mux0X( (ir_ctr <= NumBytes),
//zz                                   irx_orig_byte,
//zz                                   mkexpr(byte0) );
//zz       storeBE( irx_ea, irx_tostore );
//zz       
//zz       ir_ctr = binop(Iop_And8, ir_ctr, mkU8(1));
//zz       EA_offset++;
//zz    }
//zz    break;
//zz #endif
//zz 
//zz    default:
//zz       vex_printf("dis_int_ldst_str(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz 

/* ------------------------------------------------------------------
   Integer Branch Instructions
   ------------------------------------------------------------------ */

/*
  Branch helper function
  ok = BO[2] | ((CTR[0] != 0) ^ BO[1])
  Returns an I32 which is 0x00000000 if the ctr condition failed
  and 0xFFFFFFFF otherwise.
*/
static IRExpr* /* :: Ity_I32 */ branch_ctr_ok( UInt BO )
{
   IRTemp ok = newTemp(Ity_I32);

   if ((BO >> 2) & 1) {
      assign( ok, mkU32(0xFFFFFFFF) );
   } else {
      if ((BO >> 1) & 1) {
         assign( ok, unop( Iop_1Sto32, 
                           binop( Iop_CmpEQ32, 
                                  getSPR( PPC32_SPR_CTR ), mkU32(0))) );
      } else {
         assign( ok, unop( Iop_1Sto32,
                           binop( Iop_CmpNE32, 
                                  getSPR( PPC32_SPR_CTR ), mkU32(0))) );
      }
   }
   return mkexpr(ok);
}


/*
  Branch helper function cond_ok = BO[4] | (CR[BI] == BO[3])
  Returns an I32 which is either 0 if the condition failed or 
  some arbitrary nonzero value otherwise. */

static IRExpr* /* :: Ity_I32 */ branch_cond_ok( UInt BO, UInt BI )
{
   Int where;
   IRTemp res   = newTemp(Ity_I32);
   IRTemp cr_bi = newTemp(Ity_I32);
   
   if ((BO >> 4) & 1) {
      assign( res, mkU32(1) );
   } else {
      // ok = (CR[BI] == BO[3]) Note, the following relies on
      // getCRbit_anywhere returning a value which
      // is either zero or has exactly 1 bit set.  
      assign( cr_bi, getCRbit_anywhere( BI, &where ) );

      if ((BO >> 3) & 1) {
         /* We can use cr_bi as-is. */
         assign( res, mkexpr(cr_bi) );
      } else {
         /* We have to invert the sense of the information held in
            cr_bi.  For that we need to know which bit
            getCRbit_somewhere regards as significant. */
         assign( res, binop(Iop_Xor32, mkexpr(cr_bi), mkU32(1<<where)) );
      }
   }
   return mkexpr(res);
}


/*
  Integer Branch Instructions
*/
static Bool dis_branch ( UInt theInstr, DisResult* dres )
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
   Int exts_LI = (Int)extend_s_26to32(LI_24 << 2);
   
   Addr32 nia = 0;
   
   //   IRTemp ctr       = newTemp(Ity_I32);
   //   IRTemp lr        = newTemp(Ity_I32);
   IRTemp ir_nia    = newTemp(Ity_I32);
   IRTemp do_branch = newTemp(Ity_I32);
   IRTemp ctr_ok    = newTemp(Ity_I32);
   IRTemp cond_ok   = newTemp(Ity_I32);
   
//   assign( ctr, getSPR( PPC32_SPR_CTR ) );

   /* Hack to pass through code that just wants to read the PC */
   if (theInstr == 0x429F0005) {
      DIP("bcl 0x%x, 0x%x (a.k.a mr lr,cia+4)\n", BO, BI);
      putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
      return True;
   }
    
   switch (opc1) {
   case 0x12: // b     (Branch, PPC32 p360)
      if (flag_AA) {
         nia = (UInt)exts_LI;
      } else {
         nia = (UInt)((Int)guest_CIA_curr_instr + exts_LI);
      }
      DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", nia);

      if (flag_LK) {
         putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
      }      
      irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
      irbb->next     = mkU32(nia);
      break;
      
   case 0x10: // bc    (Branch Conditional, PPC32 p361)
      DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
          flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, exts_BD);
      
      if (!(BO & 0x4)) {
         putSPR( PPC32_SPR_CTR,
                 binop(Iop_Sub32, getSPR( PPC32_SPR_CTR ), mkU32(1)) );
      }

      /* This is a bit subtle.  ctr_ok is either all 0s or all 1s.
	 cond_ok is either zero or nonzero, since that's the cheapest
	 way to compute it.  Anding them together gives a value which
	 is either zero or non zero and so that's what we must test
	 for in the IRStmt_Exit. */
      assign( ctr_ok,  branch_ctr_ok( BO ) );
      assign( cond_ok, branch_cond_ok( BO, BI ) );
      assign( do_branch,
              binop(Iop_And32, mkexpr(cond_ok), mkexpr(ctr_ok)) );

      if (flag_AA) {
         nia = (UInt)exts_BD;
      } else {
         nia = (UInt)((Int)guest_CIA_curr_instr + exts_BD);
      }
      if (flag_LK) {
         putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
      }
      
      stmt( IRStmt_Exit( binop(Iop_CmpNE32, mkexpr(do_branch), mkU32(0)),
                         flag_LK ? Ijk_Call : Ijk_Boring,
                         IRConst_U32(nia) ));
      
      irbb->jumpkind = Ijk_Boring;
      irbb->next     = mkU32(guest_CIA_curr_instr + 4);
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
         
         assign( ir_nia,
                 binop(Iop_And32, mkU32(0xFFFFFFFC), 
                                  getSPR( PPC32_SPR_CTR ) ));
         
         if (flag_LK) {
            putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
         }
         
         stmt( IRStmt_Exit(
                  binop(Iop_CmpEQ32, mkexpr(cond_ok), mkU32(0)),
                  Ijk_Boring,
                  IRConst_U32(guest_CIA_curr_instr + 4) 
             ));
         
         irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irbb->next     = mkexpr(ir_nia);
         break;
         
      case 0x010: // bclr (Branch Cond. to Link Register, PPC32 p365) 

         if ((BO & 0x14 /* 1z1zz */) == 0x14 && flag_LK == 0) {
            DIP("blr");
         } else {
            DIP("bclr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);
         }

         if (!(BO & 0x4)) {
            putSPR( PPC32_SPR_CTR, 
                    binop(Iop_Sub32, getSPR( PPC32_SPR_CTR ), mkU32(1)) );
         }
         
         /* See comments above for 'bc' about this */
         assign( ctr_ok,  branch_ctr_ok( BO ) );
         assign( cond_ok, branch_cond_ok( BO, BI ) );
         assign( do_branch,
                 binop(Iop_And32, mkexpr(cond_ok), mkexpr(ctr_ok)) );
         
         assign( ir_nia, binop(Iop_And32,
                               getSPR( PPC32_SPR_LR ),
                               mkU32(0xFFFFFFFC)) );
         if (flag_LK) {
            putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
         }

         stmt( IRStmt_Exit(
                  binop(Iop_CmpEQ32, mkexpr(do_branch), mkU32(0)),
                  Ijk_Boring,
                  IRConst_U32(guest_CIA_curr_instr + 4)
             ));

         irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Ret;
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
    
   dres->whatNext = Dis_StopHere;
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
//uu   IRTemp tmp  = newTemp(Ity_I32);

   if (opc1 != 19 || b0 != 0) {
      vex_printf("dis_cond_logic(PPC32)(opc1)\n");
      return False;
   }

   if (opc2 == 0) {  // mcrf    (Move Cond Reg Field, PPC32 p464)
      if (((crbD_addr & 0x3) != 0) ||
          ((crbA_addr & 0x3) != 0) || (crbB_addr != 0))
         return False;
      DIP("mcrf cr%d,cr%d\n", crfD_addr, crfS_addr);
      putCR0(   crfD_addr, getCR0(  crfS_addr) );
      putCR321( crfD_addr, getCR321(crfS_addr) );
   } else {
      assign( crbA, getCRbit(crbA_addr) );
      if (crbA_addr == crbB_addr)
         crbB = crbA;
      else
         assign( crbB, getCRbit(crbB_addr) );

      switch (opc2) {
//zz       case 0x101: // crand   (Cond Reg AND, PPC32 p372)
//zz          DIP("crand crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
//zz          assign( crbD, binop(Iop_And32, mkexpr(crbA), mkexpr(crbB)) );
//zz          break;
//zz       case 0x081: // crandc  (Cond Reg AND w. Complement, PPC32 p373)
//zz          DIP("crandc crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
//zz          assign( crbD, binop(Iop_And32, mkexpr(crbA),
//zz                              unop(Iop_Not32, mkexpr(crbB))) );
//zz          break;
      case 0x121: // creqv   (Cond Reg Equivalent, PPC32 p374)
         DIP("creqv crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_Xor32, mkexpr(crbA), mkexpr(crbB))) );
         break;
//zz       case 0x0E1: // crnand  (Cond Reg NAND, PPC32 p375)
//zz          DIP("crnand crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
//zz          assign( crbD, unop(Iop_Not32,
//zz                             binop(Iop_And32, mkexpr(crbA), mkexpr(crbB))) );
//zz          break;
      case 0x021: // crnor   (Cond Reg NOR, PPC32 p376)
         DIP("crnor crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_Or32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x1C1: // cror    (Cond Reg OR, PPC32 p377)
         DIP("cror crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Or32, mkexpr(crbA), mkexpr(crbB)) );
         break;
//zz       case 0x1A1: // crorc   (Cond Reg OR w. Complement, PPC32 p378)
//zz          DIP("crorc crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
//zz          assign( crbD, binop(Iop_Or32, mkexpr(crbA),
//zz                              unop(Iop_Not32, mkexpr(crbB))) );
//zz          break;
      case 0x0C1: // crxor   (Cond Reg XOR, PPC32 p379)
         DIP("crxor crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Xor32, mkexpr(crbA), mkexpr(crbB)) );
         break;

      default:
         vex_printf("dis_cond_logic(PPC32)(opc2)\n");
         return False;
      }

      putCRbit( crbD_addr, mkexpr(crbD) );
   }
   return True;
}


/*
  System Linkage Instructions
*/
static Bool dis_syslink ( UInt theInstr, DisResult* dres )
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
   irbb->next     = mkU32( guest_CIA_curr_instr + 4 );
   irbb->jumpkind = Ijk_Syscall;
   
   dres->whatNext = Dis_StopHere;
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
//uu   IRTemp xer_so = newTemp(Ity_I32);
//uu   IRTemp cr_f7  = newTemp(Ity_I32);
   
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
//zz       case 0x356: // eieio (Enforce In-Order Execution of I/O, PPC32 p394)
//zz vassert(0);
//zz 
//zz          if (b11to25 != 0 || b0 != 0) {
//zz             vex_printf("dis_int_memsync(PPC32)(eiei0,b11to25|b0)\n");
//zz             return False;
//zz          }
//zz          DIP("eieio\n");
//zz          return False;

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
            assign( EA, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
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
            assign( EA, binop(Iop_Add32, mkexpr(Ra), mkexpr(Rb)) );
         }
         storeBE( mkexpr(EA), mkexpr(Rs) );
         
         // Set CR0[LT GT EQ S0] = 0b001 || XER[SO]
         putCR321(0, mkU8(1<<1));
	 putCR0(0, getXER_SO());
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
   
//uu   UInt flag_op  = PPC32G_FLAG_OP_NUMBER;
   
   IRTemp sh_amt = newTemp(Ity_I8);
//uu   IRTemp sign   = newTemp(Ity_I32);
   IRTemp rb_b5  = newTemp(Ity_I32);
//uu   IRTemp sext   = newTemp(Ity_I32);
   IRTemp Rs     = newTemp(Ity_I32);
   IRTemp Rs_sh  = newTemp(Ity_I32);
//uu   IRTemp Rs_msk = newTemp(Ity_I32);
   IRTemp Ra     = newTemp(Ity_I32);
   IRTemp Rb     = newTemp(Ity_I32);
//uu   IRTemp mask   = newTemp(Ity_I32);
   IRTemp sh_amt32   = newTemp(Ity_I32);
   IRTemp outofrange = newTemp(Ity_I8);
   
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
         
	 /* JRS: my reading of the (poorly worded) PPC32 doc p506 is:
               amt = Rb & 63
               Ra = Sar32( Rs, amt > 31 ? 31 : amt )
               XER.CA = amt > 31 ? sign-of-Rs : (computation as per srawi)
	  */
        assign( sh_amt32, binop(Iop_And32, mkU32(0x3F), mkexpr(Rb)) );
        assign( outofrange,
                 unop( Iop_1Uto8, 
                       binop(Iop_CmpLT32U, mkU32(31), mkexpr(sh_amt32)) ));
         assign( Ra,
                 binop( Iop_Sar32, 
                        mkexpr(Rs), 
                        unop( Iop_32to8, 
                              IRExpr_Mux0X( mkexpr(outofrange), 
                                            mkexpr(sh_amt32), 
                                            mkU32(31)) ))
               );
         set_XER_CA( PPC32G_FLAG_OP_SRAW,
                     mkexpr(Ra), mkexpr(Rs), mkexpr(sh_amt32),
                     get_XER_CA() );
         break;
         
      case 0x338: // srawi (Shift Right Algebraic Word Immediate, PPC32 p507)
         DIP("srawi%s r%d,r%d,%d\n", flag_Rc ? "." : "",
             Ra_addr, Rs_addr, sh_imm);
      	 vassert(sh_imm < 32);
         assign( sh_amt, mkU8(sh_imm) );
         assign( Ra, binop(Iop_Sar32, mkexpr(Rs), mkexpr(sh_amt)) );
         set_XER_CA( PPC32G_FLAG_OP_SRAWI, 
                     mkexpr(Ra), mkexpr(Rs), mkU32(sh_imm), 
                     get_XER_CA() );
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
      set_CR0( mkexpr(Ra) );
   }
   return True;
}



//zz /*
//zz   Integer Load/Store Reverse Instructions
//zz */
//zz static Bool dis_int_ldst_rev ( UInt theInstr )
//zz {
//zz    /* X-Form */
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar Ra_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar Rb_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
//zz    UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
//zz    
//zz    IRTemp EA    = newTemp(Ity_I32);
//zz    IRTemp Rd    = newTemp(Ity_I32);
//zz    IRTemp Rs    = newTemp(Ity_I32);
//zz    IRTemp byte0 = newTemp(Ity_I32);
//zz    IRTemp byte1 = newTemp(Ity_I32);
//zz    IRTemp byte2 = newTemp(Ity_I32);
//zz    IRTemp byte3 = newTemp(Ity_I32);
//zz    IRTemp tmp16 = newTemp(Ity_I16);
//zz    IRTemp tmp32 = newTemp(Ity_I32);
//zz 
//zz    if (opc1 != 0x1F || b0 != 0) {
//zz       vex_printf("dis_int_ldst_rev(PPC32)(opc1|b0)\n");
//zz       return False;
//zz    }
//zz    
//zz    if (Ra_addr == 0) {
//zz       assign( EA, getIReg(Rb_addr));
//zz    } else {
//zz       assign( EA, binop(Iop_Add32, getIReg(Ra_addr), getIReg(Rb_addr)) );
//zz    }
//zz    
//zz    switch (opc2) {
//zz    case 0x316: // lhbrx (Load Half Word Byte-Reverse Indexed, PPC32 p449)
//zz vassert(0);
//zz 
//zz       DIP("lhbrx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
//zz       assign( byte0, loadBE(Ity_I8, mkexpr(EA)) );
//zz       assign( byte1, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(1))) );
//zz       assign( Rd, binop(Iop_Or32,
//zz                         binop(Iop_Shl32, mkexpr(byte1), mkU8(8)),
//zz                         mkexpr(byte0)) );
//zz       putIReg( Rd_addr, mkexpr(Rd));
//zz       break;
//zz        
//zz    case 0x216: // lwbrx (Load Word Byte-Reverse Indexed, PPC32 p459)
//zz vassert(0);
//zz 
//zz       DIP("lwbrx r%d,r%d,r%d\n", Rd_addr, Ra_addr, Rb_addr);
//zz       assign( byte0, loadBE(Ity_I8, mkexpr(EA)) );
//zz       assign( byte1, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(1))) );
//zz       assign( byte2, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(2))) );
//zz       assign( byte3, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(3))) );
//zz       assign( Rd, binop(Iop_Or32,
//zz                         binop(Iop_Or32,
//zz                               binop(Iop_Shl32, mkexpr(byte3), mkU8(24)),
//zz                               binop(Iop_Shl32, mkexpr(byte2), mkU8(16))),
//zz                         binop(Iop_Or32,
//zz                               binop(Iop_Shl32, mkexpr(byte1), mkU8(8)),
//zz                               mkexpr(byte0))) );
//zz       putIReg( Rd_addr, mkexpr(Rd));
//zz       break;
//zz       
//zz    case 0x396: // sthbrx (Store Half Word Byte-Reverse Indexed, PPC32 p523)
//zz vassert(0);
//zz 
//zz       DIP("sthbrx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
//zz       assign( Rs, getIReg(Rs_addr) );
//zz       assign( byte0, binop(Iop_And32, mkexpr(Rs), mkU32(0x00FF)) );
//zz       assign( byte1, binop(Iop_And32, mkexpr(Rs), mkU32(0xFF00)) );
//zz       
//zz       assign( tmp16,
//zz               unop(Iop_32to16,
//zz                    binop(Iop_Or32,
//zz                          binop(Iop_Shl32, mkexpr(byte0), mkU8(8)),
//zz                          binop(Iop_Shr32, mkexpr(byte1), mkU8(8)))) );
//zz       storeBE( mkexpr(EA), getIReg(tmp16) );
//zz       break;
//zz       
//zz    case 0x296: // stwbrx (Store Word Byte-Reverse Indexed, PPC32 p531)
//zz vassert(0);
//zz 
//zz       DIP("stwbrx r%d,r%d,r%d\n", Rs_addr, Ra_addr, Rb_addr);
//zz       assign( Rs, getIReg(Rs_addr) );
//zz       assign( byte0, binop(Iop_And32, mkexpr(Rs), mkU32(0x000000FF)) );
//zz       assign( byte1, binop(Iop_And32, mkexpr(Rs), mkU32(0x0000FF00)) );
//zz       assign( byte2, binop(Iop_And32, mkexpr(Rs), mkU32(0x00FF0000)) );
//zz       assign( byte3, binop(Iop_And32, mkexpr(Rs), mkU32(0xFF000000)) );
//zz       
//zz       assign( tmp32,
//zz               binop(Iop_Or32,
//zz                     binop(Iop_Or32,
//zz                           binop(Iop_Shl32, mkexpr(byte0), mkU8(24)),
//zz                           binop(Iop_Shl32, mkexpr(byte1), mkU8(8))),
//zz                     binop(Iop_Or32,
//zz                           binop(Iop_Shr32, mkexpr(byte2), mkU8(8)),
//zz                           binop(Iop_Shr32, mkexpr(byte3), mkU8(24)))) );
//zz       storeBE( mkexpr(EA), mkexpr(tmp32) );
//zz       break;
//zz       
//zz    default:
//zz       vex_printf("dis_int_ldst_rev(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }



/*
  Processor Control Instructions
*/
static Bool dis_proc_ctl ( UInt theInstr )
{
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   
   /* X-Form */
//uu   UChar crfD     = toUChar((theInstr >> 23) & 0x7);  /* theInstr[23:25] */
//uu   UChar b21to22  = toUChar((theInstr >> 21) & 0x3);  /* theInstr[21:22] */
   UChar Rd_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UInt  b11to20  =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
   
   /* XFX-Form */
   UChar Rs_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UInt  SPR      =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
//uu   UInt  TBR      =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
   UChar b20      = toUChar((theInstr >> 11) & 0x1);  /* theInstr[11]    */
   UInt  CRM      =         (theInstr >> 12) & 0xFF;  /* theInstr[12:19] */
   UChar b11      = toUChar((theInstr >> 11) & 0x1);  /* theInstr[20]    */
   
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   
   UInt  SPR_flipped = ((SPR & 0x1F) << 5) | ((SPR >> 5) & 0x1F);

   IRTemp Rs  = newTemp(Ity_I32);
//uu   IRTemp tmp = newTemp(Ity_I32);

   assign( Rs, getIReg(Rs_addr) );
   
   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_proc_ctl(PPC32)(opc1|b0)\n");
      return False;
   }
   
   switch (opc2) {
//zz    /* X-Form */
//zz    case 0x200: // mcrxr (Move to Condition Register from XER, PPC32 p466)
//zz       if (b21to22 != 0 || b11to20 != 0) {
//zz          vex_printf("dis_proc_ctl(PPC32)(mcrxr,b21to22|b11to20)\n");
//zz          return False;
//zz       }
//zz       DIP("mcrxr crf%d\n", crfD);
//zz       
//zz       // CR[7-crfD] = XER[28-31]
//zz       assign( tmp, getReg_field( PPC32_SPR_XER, 7 ) );
//zz       putReg_field( PPC32_SPR_CR, mkexpr(tmp), 7-crfD );
//zz       
//zz       // Clear XER[28 - 31]
//zz       putReg_field( PPC32_SPR_XER, mkU32(0), 7 );
//zz       break;
      
   case 0x013: // mfcr (Move from Condition Register, PPC32 p467)
      if (b11to20 != 0) {
         vex_printf("dis_proc_ctl(PPC32)(mfcr,b11to20)\n");
         return False;
      }
      DIP("mfcr r%d\n", Rd_addr);
      putIReg( Rd_addr, getEntireCR() );
      break;
      
   /* XFX-Form */
   case 0x153: // mfspr (Move from Special-Purpose Register, PPC32 p470)
      
      switch (SPR_flipped) {  // Choose a register...

         case 0x1:
            DIP("mfxer r%d\n", Rd_addr);
            /* sheesh [kebab] */
            putIReg( 
               Rd_addr,
               binop(
                  Iop_Or32,
                  binop(
                     Iop_Or32,
                     binop( Iop_Shl32, 
                            binop( Iop_And32, 
                                   unop( Iop_8Uto32, 
                                         IRExpr_Get( OFFB_XER_SO, Ity_I8 )), 
                                   mkU32(1)), 
                            mkU8(31)),
                     binop( Iop_Shl32, 
                            binop( Iop_And32, 
                                   unop( Iop_8Uto32, 
                                         IRExpr_Get( OFFB_XER_OV, Ity_I8 )), 
                                   mkU32(1)), 
                            mkU8(30))
                  ),
                  binop(
                     Iop_Or32,
                     binop( Iop_Shl32, 
                            binop( Iop_And32, 
                                   unop( Iop_8Uto32, 
                                         IRExpr_Get( OFFB_XER_CA, Ity_I8 )), 
                                   mkU32(1)), 
                            mkU8(29)),
                     binop( Iop_And32, 
                            unop( Iop_8Uto32, 
                                  IRExpr_Get( OFFB_XER_BC, Ity_I8 )), 
                            mkU32(0xFF))
                  )
               )
            );
            break;

         case 0x8:
            DIP("mflr r%d\n", Rd_addr);
            putIReg( Rd_addr, getSPR( PPC32_SPR_LR ) ); 
            break;
         case 0x9:
            DIP("mfctr r%d\n", Rd_addr);
            putIReg( Rd_addr, getSPR( PPC32_SPR_CTR ) ); 
            break;
         case 0x100: 
            DIP("mfvrsave r%d\n", Rd_addr);
            putIReg( Rd_addr, getSPR( PPC32_SPR_VRSAVE ) ); 
            break;

         case 0x012: case 0x013: case 0x016:
         case 0x019: case 0x01A: case 0x01B:
         case 0x110: case 0x111: case 0x112: case 0x113:
         // case 0x118: // 64bit only
         case 0x11A: case 0x11F:
         case 0x210: case 0x211: case 0x212: case 0x213:
         case 0x214: case 0x215: case 0x216: case 0x217:
         case 0x218: case 0x219: case 0x21A: case 0x21B:
         case 0x21C: case 0x21D: case 0x21E: case 0x21F:
         case 0x3F5:
            vex_printf("dis_proc_ctl(PPC32)(mfspr) - supervisor level op\n");
            return False;

         default:
            vex_printf("dis_proc_ctl(PPC32)(mfspr,SPR_flipped)(0x%x)\n",
                       SPR_flipped);
            return False;
      }
      break;
      
//zz    case 0x173: // mftb (Move from Time Base, PPC32 p475)
//zz vassert(0);
//zz 
//zz       DIP("mftb r%d,0x%x\n", Rd_addr, TBR);
//zz       return False;
      
   case 0x090: // mtcrf (Move to Condition Register Fields, PPC32 p477)
      if (b11 != 0 || b20 != 0) {
         vex_printf("dis_proc_ctl(PPC32)(mtcrf,b11|b20)\n");
         return False;
      }
      DIP("mtcrf 0x%x,r%d\n", CRM, Rs_addr);
      putCRfields ( mkexpr(Rs), CRM );
      break;

   case 0x1D3: // mtspr (Move to Special-Purpose Register, PPC32 p483)
      
      switch (SPR_flipped) {  // Choose a register...
         case 0x1:
            DIP("mtxer r%d\n", Rs_addr);
            stmt(IRStmt_Put( 
               OFFB_XER_SO, 
               unop( Iop_32to8, 
                     binop( Iop_And32, 
                            binop(Iop_Shr32, mkexpr(Rs), mkU8(31)), 
                            mkU32(1)) ) 
            ));
            stmt(IRStmt_Put( 
               OFFB_XER_OV, 
               unop( Iop_32to8, 
                     binop( Iop_And32, 
                            binop(Iop_Shr32, mkexpr(Rs), mkU8(30)), 
                            mkU32(1)) ) 
            ));
            stmt(IRStmt_Put( 
               OFFB_XER_CA, 
               unop( Iop_32to8, 
                     binop( Iop_And32, 
                            binop(Iop_Shr32, mkexpr(Rs), mkU8(29)), 
                            mkU32(1)) ) 
            ));
            stmt(IRStmt_Put( 
               OFFB_XER_BC, 
               unop( Iop_32to8, 
                     binop( Iop_And32, mkexpr(Rs), mkU32(0xFF)) )
            ));
            break;
         case 0x8:
            DIP("mtlr r%d\n", Rs_addr);
            putSPR( PPC32_SPR_LR, mkexpr(Rs) ); 
            break;
         case 0x9:
            DIP("mtctr r%d\n", Rs_addr);
            putSPR( PPC32_SPR_CTR, mkexpr(Rs) ); 
            break;
         case 0x100:
            DIP("mtvrsave r%d\n", Rs_addr);
            putSPR( PPC32_SPR_VRSAVE, mkexpr(Rs) ); 
            break;
//zz 
//zz       case 0x012: case 0x013: case 0x016:
//zz       case 0x019: case 0x01A: case 0x01B:
//zz       case 0x110: case 0x111: case 0x112: case 0x113:
//zz //      case 0x118: // 64bit only
//zz       case 0x11A: case 0x11C: case 0x11D:
//zz       case 0x210: case 0x211: case 0x212: case 0x213:
//zz       case 0x214: case 0x215: case 0x216: case 0x217:
//zz       case 0x218: case 0x219: case 0x21A: case 0x21B:
//zz       case 0x21C: case 0x21D: case 0x21E: case 0x21F:
//zz       case 0x3F5:
//zz          vex_printf("dis_proc_ctl(PPC32)(mtspr) - supervisor level op\n");
//zz          return False;

         default:
            vex_printf("dis_proc_ctl(PPC32)(mtspr,SPR_flipped)(%d)\n",
                       SPR_flipped);
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
static Bool dis_cache_manage ( UInt         theInstr, 
                               DisResult*   dres,
                               VexArchInfo* guest_archinfo )
{
   /* X-Form */
   UChar opc1    = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar b21to25 = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar Ra_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar Rb_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2    =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0      = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
   Int   lineszB = guest_archinfo->ppc32_cache_line_szB;

   if (opc1 != 0x1F || b21to25 != 0 || b0 != 0) {
      vex_printf("dis_cache_manage(PPC32)(opc1|b21to25|b0)\n");
      return False;
   }

   /* stay sane .. */
   vassert(lineszB == 32 || lineszB == 128);
   
   switch (opc2) {
//zz    case 0x2F6: // dcba (Data Cache Block Allocate, PPC32 p380)
//zz       vassert(0); /* AWAITING TEST CASE */
//zz       DIP("dcba r%d,r%d\n", Ra_addr, Rb_addr);
//zz       if (0) vex_printf("vex ppc32->IR: kludged dcba\n");
//zz       break;
      
   case 0x056: // dcbf (Data Cache Block Flush, PPC32 p382)
      DIP("dcbf r%d,r%d\n", Ra_addr, Rb_addr);
      /* nop as far as vex is concerned */
      if (0) vex_printf("vex ppc32->IR: kludged dcbf\n");
      break;
      
   case 0x036: // dcbst (Data Cache Block Store, PPC32 p384)
      DIP("dcbst r%d,r%d\n", Ra_addr, Rb_addr);
      /* nop as far as vex is concerned */
      break;

   case 0x116: // dcbt (Data Cache Block Touch, PPC32 p385)
      DIP("dcbt r%d,r%d\n", Ra_addr, Rb_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x0F6: // dcbtst (Data Cache Block Touch for Store, PPC32 p386)
      DIP("dcbtst r%d,r%d\n", Ra_addr, Rb_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x3F6: { // dcbz (Data Cache Block Clear to Zero, PPC32 p387)
      /* Clear all bytes in cache block at (rA|0) + rB. */
      IRTemp  EA   = newTemp(Ity_I32);
      IRTemp  addr = newTemp(Ity_I32);
      IRExpr* irx_addr;
      UInt    i;
      DIP("dcbz r%d,r%d\n", Ra_addr, Rb_addr);
      assign( EA,
	      binop( Iop_Add32,
		     getIReg(Rb_addr), 
		     Ra_addr==0 ? mkU32(0) : getIReg(Ra_addr)) );

      /* Round EA down to the start of the containing block. */
      assign( addr,
	      binop( Iop_And32,
		     mkexpr(EA),
		     mkU32( ~(lineszB-1) )) );

      for (i = 0; i < lineszB / 4; i++) {
         irx_addr = binop( Iop_Add32, mkexpr(addr), mkU32(i*4) );
         storeBE( irx_addr, mkU32(0) );
      }
      break;
   }

   case 0x3D6: { 
      // icbi (Instruction Cache Block Invalidate, PPC32 p431)
      /* Invalidate all translations containing code from the cache
         block at (rA|0) + rB. */
      IRTemp addr = newTemp(Ity_I32);
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
                      mkU32( ~(lineszB-1) ))) );

      stmt( IRStmt_Put(OFFB_TILEN, mkU32(lineszB) ) );

      /* be paranoid ... */
      stmt( IRStmt_MFence() );

      irbb->jumpkind = Ijk_TInval;
      irbb->next     = mkU32(guest_CIA_curr_instr + 4);
      dres->whatNext = Dis_StopHere;
      break;
   }

   default:
      vex_printf("dis_cache_manage(PPC32)(opc2)\n");
      return False;
   }
   return True;
}


/*------------------------------------------------------------*/
/*--- Floating Point Helpers                               ---*/
/*------------------------------------------------------------*/

/* --- Set the emulation-warning pseudo-register. --- */

static void put_emwarn ( IRExpr* e /* :: Ity_I32 */ )
{
   vassert(typeOfIRExpr(irbb->tyenv,e) == Ity_I32);
   stmt( IRStmt_Put( OFFB_EMWARN, e ) );
}

/* --------- Synthesise a 2-bit FPU rounding mode. --------- */
/* Produces a value in 0 .. 3, which is encoded as per the type
   IRRoundingMode.  PPC32RoundingMode encoding is different to
   IRRoundingMode, so need to map it.
*/
static IRExpr* /* :: Ity_I32 */ get_roundingmode ( void )
{
/* 
   rounding mode | PPC | IR
   ------------------------
   to nearest    | 00  | 00
   to zero       | 01  | 11
   to +infinity  | 10  | 10
   to -infinity  | 11  | 01
*/
   IRTemp rm_PPC32 = newTemp(Ity_I32);
   assign( rm_PPC32, getReg_masked( PPC32_SPR_FPSCR, MASK_FPSCR_RN ) );

   // rm_IR = XOR( rm_PPC32, (rm_PPC32 << 1) & 2)
   return binop(Iop_Xor32, mkexpr(rm_PPC32),
                binop(Iop_And32, mkU32(2),
                      binop(Iop_Shl32, mkexpr(rm_PPC32), mkU8(1))));
}

/* Round float to single precision
 - returns type Ity_F64 */
static IRExpr* roundToSgl ( IRExpr* src )
{
   return unop(Iop_F32toF64, binop(Iop_F64toF32, get_roundingmode(), src));
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
   UChar opc1      = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar frD_addr  = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar rA_addr   = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   UChar rB_addr   = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UInt  opc2      =         (theInstr >>  1) & 0x3FF;  /* theInstr[1:10]  */
   UChar b0        = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */

   /* D-Form */
   UInt  d_imm     =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */

   Int exts_d_imm  = extend_s_16to32(d_imm);

   IRTemp EA       = newTemp(Ity_I32);
   IRTemp rA       = newTemp(Ity_I32);
   IRTemp rB       = newTemp(Ity_I32);
   IRTemp rA_or_0  = newTemp(Ity_I32);

   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );
   assign( rA_or_0, (rA_addr == 0) ? mkU32(0) : mkexpr(rA) );

   switch(opc1) {
   case 0x30: // lfs (Load Float Single, PPC32 p441)
      DIP("lfs fr%d,%d(r%d)\n", frD_addr, exts_d_imm, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA_or_0)) );
      putFReg( frD_addr, unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
      break;

//zz    case 0x31: // lfsu (Load Float Single with Update, PPC32 p442)
//zz       if (rA_addr == 0) {
//zz          vex_printf("dis_fp_load(PPC32)(instr,lfsu)\n");
//zz          return False;
//zz       }
//zz       DIP("lfsu fr%d,%d(r%d)\n", frD_addr, exts_d_imm, rA_addr);
//zz       assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA)) );
//zz       putFReg( frD_addr, unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
//zz       putIReg( rA_addr, mkexpr(EA) );
//zz       break;
      
   case 0x32: // lfd (Load Float Double, PPC32 p437)
      DIP("lfd fr%d,%d(r%d)\n", frD_addr, exts_d_imm, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA_or_0)) );
      putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
      break;

   case 0x33: // lfdu (Load Float Double with Update, PPC32 p438)
      if (rA_addr == 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfdu)\n");
         return False;
      }
      DIP("lfdu fr%d,%d(r%d)\n", frD_addr, exts_d_imm, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA)) );
      putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,b0)\n");
         return False;
      }

      switch(opc2) {
         case 0x217: // lfsx (Load Float Single Indexed, PPC32 p444)
            DIP("lfsx fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            putFReg( frD_addr, unop( Iop_F32toF64, 
                                     loadBE(Ity_F32, mkexpr(EA))) );
            break;

         case 0x237: // lfsux (Load Float Single with Update Indexed, PPC32 p443)
            if (rA_addr == 0) {
               vex_printf("dis_fp_load(PPC32)(instr,lfsux)\n");
               return False;
            }
            DIP("lfsux fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
            putFReg( frD_addr, unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
            putIReg( rA_addr, mkexpr(EA) );
            break;

         case 0x257: // lfdx (Load Float Double Indexed, PPC32 p440)
            DIP("lfdx fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
            break;

         case 0x277: // lfdux (Load Float Double with Update Indexed, PPC32 p439)
            if (rA_addr == 0) {
               vex_printf("dis_fp_load(PPC32)(instr,lfdux)\n");
               return False;
            }
            DIP("lfdux fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
            putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
            putIReg( rA_addr, mkexpr(EA) );
            break;

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
   UChar opc1      = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
   UChar frS_addr  = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
   UChar rA_addr   = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
   UChar rB_addr   = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
   UInt  opc2      =         (theInstr >>  1) & 0x3FF;  /* theInstr[1:10]  */
   UChar b0        = toUChar((theInstr >>  0) & 1);     /* theInstr[0]     */

   /* D-Form */
   UInt  d_imm     =         (theInstr >>  0) & 0xFFFF; /* theInstr[0:15]  */

   Int exts_d_imm  = extend_s_16to32(d_imm);

   IRTemp EA       = newTemp(Ity_I32);
   IRTemp frS      = newTemp(Ity_F64);
   IRTemp rA       = newTemp(Ity_I32);
   IRTemp rB       = newTemp(Ity_I32);
   IRTemp rA_or_0  = newTemp(Ity_I32);

   assign( frS, getFReg(frS_addr) );
   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );
   assign( rA_or_0, (rA_addr == 0) ? mkU32(0) : mkexpr(rA) );

   switch(opc1) {

   case 0x34: // stfs (Store Float Single, PPC32 p518)
      DIP("stfs fr%d,%d(r%d)\n", frS_addr, exts_d_imm, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA_or_0)) );
      storeBE( mkexpr(EA),
               binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
      break;

//zz    case 0x35: // stfsu (Store Float Single with Update, PPC32 p519)
//zz       if (rA_addr == 0) {
//zz          vex_printf("dis_fp_store(PPC32)(instr,stfsu)\n");
//zz          return False;
//zz       }
//zz       DIP("stfsu fr%d,%d(r%d)\n", frS_addr, exts_d_imm, rA_addr);
//zz       assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA)) );
//zz       storeBE( mkexpr(EA),
//zz                binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
//zz       putIReg( rA_addr, mkexpr(EA) );
//zz       break;

   case 0x36: // stfd (Store Float Double, PPC32 p513)
      DIP("stfd fr%d,%d(r%d)\n", frS_addr, exts_d_imm, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA_or_0)) );
      storeBE( mkexpr(EA), mkexpr(frS) );
      break;

   case 0x37: // stfdu (Store Float Double with Update, PPC32 p514)
      if (rA_addr == 0) {
         vex_printf("dis_fp_store(PPC32)(instr,stfdu)\n");
         return False;
      }
      DIP("stfdu fr%d,%d(r%d)\n", frS_addr, exts_d_imm, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(exts_d_imm), mkexpr(rA)) );
      storeBE( mkexpr(EA), mkexpr(frS) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_fp_store(PPC32)(instr,b0)\n");
         return False;
      }

      switch(opc2) {
         case 0x297: // stfsx (Store Float Single Indexed, PPC32 p521)
            DIP("stfsx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            storeBE( mkexpr(EA),
                     binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
            break;

//zz       case 0x2B7: // stfsux (Store Float Single with Update Indexed, PPC32 p520)
//zz          if (rA_addr == 0) {
//zz             vex_printf("dis_fp_store(PPC32)(instr,stfsux)\n");
//zz             return False;
//zz          }
//zz          DIP("stfsux fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
//zz          assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
//zz          storeBE( mkexpr(EA),
//zz                   binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
//zz          putIReg( rA_addr, mkexpr(EA) );
//zz          break;

         case 0x2D7: // stfdx (Store Float Double Indexed, PPC32 p516)
            DIP("stfdx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            storeBE( mkexpr(EA), mkexpr(frS) );
            break;
         
//zz       case 0x2F7: // stfdux (Store Float Double with Update Indexed, PPC32 p515)
//zz          if (rA_addr == 0) {
//zz             vex_printf("dis_fp_store(PPC32)(instr,stfdux)\n");
//zz             return False;
//zz          }
//zz          DIP("stfdux fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
//zz          assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
//zz          storeBE( mkexpr(EA), mkexpr(frS) );
//zz          putIReg( rA_addr, mkexpr(EA) );
//zz          break;
//zz 
//zz       case 0x3D7: // stfiwx (Store Float as Int, Indexed, PPC32 p517)
//zz          DIP("stfiwx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
//zz          assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
//zz          storeBE( mkexpr(EA),
//zz                   unop(Iop_64to32, unop(Iop_ReinterpF64asI64, mkexpr(frS))) );
//zz          break;

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
   // Note: flag_Rc ignored as fp exceptions not supported.

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frA = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp frC = newTemp(Ity_F64);

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));
   assign( frC, getFReg(frC_addr));

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x12: // fdivs (Floating Divide Single, PPC32 p407)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fdivs)\n");
            return False;
         }
         DIP("fdivs%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, roundToSgl( binop(Iop_DivF64, mkexpr(frA), mkexpr(frB)) ));
         break;

      case 0x14: // fsubs (Floating Subtract Single, PPC32 p430)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fsubs)\n");
            return False;
         }
         DIP("fsubs%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop(Iop_SubF64, mkexpr(frA), mkexpr(frB)) ));
         break;

      case 0x15: // fadds (Floating Add Single, PPC32 p401)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fadds)\n");
            return False;
         }
         DIP("fadds%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop(Iop_AddF64, mkexpr(frA), mkexpr(frB)) ));
         break;

//zz       case 0x16: // fsqrts (Floating SqRt (Single-Precision), PPC32 p428)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,fsqrts)\n");
//zz             return False;
//zz          }
//zz          DIP("fsqrts%s fr%d,fr%d\n", flag_Rc ? "." : "",
//zz              frD_addr, frB_addr);
//zz          assign( frD, roundToSgl( unop(Iop_SqrtF64, mkexpr(frB)) ));
//zz          break;

//zz       case 0x18: // fres (Floating Reciprocal Estimate Single, PPC32 p421)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,fres)\n");
//zz             return False;
//zz          }
//zz          DIP("fres%s fr%d,fr%d\n", flag_Rc ? "." : "",
//zz              frD_addr, frB_addr);
//zz          DIP(" => not implemented\n");        
//zz          // CAB: Can we use one of the 128 bit SIMD Iop_Recip32F ops?
//zz          return False;

      case 0x19: // fmuls (Floating Multiply Single, PPC32 p414)
         if (frB_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fmuls)\n");
            return False;
         }
         DIP("fmuls%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr);
         assign( frD, roundToSgl( binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)) ));
         break;

      default:
         vex_printf("dis_fp_arith(PPC32)(3B: opc2)\n");
         return False;
      }
      break;

   case 0x3F:
      switch (opc2) {           
      case 0x12: // fdiv (Floating Divide (Double-Precision), PPC32 p406)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fdiv)\n");
            return False;
         }
         DIP("fdiv%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, binop( Iop_DivF64, mkexpr(frA), mkexpr(frB) ) );
         break;

      case 0x14: // fsub (Floating Subtract (Double-Precision), PPC32 p429)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fsub)\n");
            return False;
         }
         DIP("fsub%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, binop( Iop_SubF64, mkexpr(frA), mkexpr(frB) ) );
         break;

      case 0x15: // fadd (Floating Add (Double-Precision), PPC32 p400)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fadd)\n");
            return False;
         }
         DIP("fadd%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, binop( Iop_AddF64, mkexpr(frA), mkexpr(frB) ) );
         break;

//zz       case 0x16: // fsqrt (Floating SqRt (Double-Precision), PPC32 p427)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,fsqrt)\n");
//zz             return False;
//zz          }
//zz          DIP("fsqrt%s fr%d,fr%d\n", flag_Rc ? "." : "",
//zz              frD_addr, frB_addr);
//zz          assign( frD, unop( Iop_SqrtF64, mkexpr(frB) ) );
//zz          break;

      case 0x17: { // fsel (Floating Select, PPC32 p426)
         IRTemp cc    = newTemp(Ity_I32);
         IRTemp cc_b0 = newTemp(Ity_I32);

         DIP("fsel%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);

         // cc: UN == 0x41, LT == 0x01, GT == 0x00, EQ == 0x40
         // => GT|EQ == (cc & 0x1 == 0)
         assign( cc, binop(Iop_CmpF64, mkexpr(frA), IRExpr_Const(IRConst_F64(0))) );
         assign( cc_b0, binop(Iop_And32, mkexpr(cc), mkU32(1)) );

         // frD = (frA >= 0.0) ? frC : frB
         //     = (cc_b0 == 0) ? frC : frB
         assign( frD,
                 IRExpr_Mux0X(
                    unop(Iop_1Uto8,
                         binop(Iop_CmpEQ32, mkexpr(cc_b0), mkU32(0))),
                    mkexpr(frB),
                    mkexpr(frC) ));
         break;
      }

      case 0x19: // fmul (Floating Multiply (Double Precision), PPC32 p413)
         if (frB_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fmul)\n");
            return False;
         }
         DIP("fmul%s fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr);
         assign( frD, binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ) );
         break;

//zz       case 0x1A: // frsqrte (Floating Reciprocal SqRt Estimate, PPC32 p424)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,frsqrte)\n");
//zz             return False;
//zz          }
//zz          DIP("frsqrte%s fr%d,fr%d\n", flag_Rc ? "." : "",
//zz              frD_addr, frB_addr);
//zz          DIP(" => not implemented\n");
//zz          // CAB: Iop_SqrtF64, then one of the 128 bit SIMD Iop_Recip32F ops?
//zz          return False;

      default:
         vex_printf("dis_fp_arith(PPC32)(3F: opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_arith(PPC32)(opc1)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frA = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp frC = newTemp(Ity_F64);

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));
   assign( frC, getFReg(frC_addr));

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x1C: // fmsubs (Floating Mult-Subtr Single, PPC32 p412)
         DIP("fmsubs%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop( Iop_SubF64,
                                binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                                mkexpr(frB)) ));
          break;

      case 0x1D: // fmadds (Floating Mult-Add Single, PPC32 p409)
         DIP("fmadds%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop( Iop_AddF64,
                                binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                                mkexpr(frB)) ));
         break;

      case 0x1E: // fnmsubs (Float Neg Mult-Subtr Single, PPC32 p420)
         DIP("fnmsubs%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl(
                    unop(Iop_NegF64,
                         binop(Iop_SubF64,
                               binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                               mkexpr(frB))) ));
         break;

      case 0x1F: // fnmadds (Floating Negative Multiply-Add Single, PPC32 p418)
         DIP("fnmadds%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl(
                    unop(Iop_NegF64,
                         binop(Iop_AddF64,
                               binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                               mkexpr(frB))) ));
         break;

      default:
         vex_printf("dis_fp_multadd(PPC32)(3B: opc2)\n");
         return False;
      }
      break;

   case 0x3F:
      switch (opc2) {           
      case 0x1C: // fmsub (Float Mult-Subtr (Double Precision), PPC32 p411)
         DIP("fmsub%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, binop( Iop_SubF64,
                             binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                             mkexpr(frB) ));
         break;

      case 0x1D: // fmadd (Float Mult-Add (Double Precision), PPC32 p408)
         DIP("fmadd%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, binop( Iop_AddF64,
                             binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                             mkexpr(frB) ));
         break;

      case 0x1E: // fnmsub (Float Neg Mult-Subtr (Double Precision), PPC32 p419)
         DIP("fnmsub%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                            binop( Iop_SubF64,
                                   binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                                   mkexpr(frB) )));
         break;

      case 0x1F: // fnmadd (Float Neg Mult-Add (Double Precision), PPC32 p417)
         DIP("fnmadd%s fr%d,fr%d,fr%d,fr%d\n", flag_Rc ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                            binop( Iop_AddF64,
                                   binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                                   mkexpr(frB) )));
         break;

      default:
         vex_printf("dis_fp_multadd(PPC32)(3F: opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_multadd(PPC32)(opc1)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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

   IRTemp ccIR    = newTemp(Ity_I32);
   IRTemp ccPPC32 = newTemp(Ity_I32);

#if 0
   IRTemp cc_lt   = newTemp(Ity_I32);
   IRTemp cc_gt   = newTemp(Ity_I32);
   IRTemp cc_eq   = newTemp(Ity_I32);
   IRTemp cc_un   = newTemp(Ity_I32);
#endif

   IRTemp frA     = newTemp(Ity_F64);
   IRTemp frB     = newTemp(Ity_F64);
//   IRExpr* irx;

   if (opc1 != 0x3F || b21to22 != 0 || b0 != 0) {
      vex_printf("dis_fp_cmp(PPC32)(instr)\n");
      return False;
   }

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));

   assign( ccIR, binop(Iop_CmpF64, mkexpr(frA), mkexpr(frB)) );
   
   /* Map compare result from IR to PPC32 */
   /*
     FP cmp result | PPC | IR
     --------------------------
     UN            | 0x1 | 0x45
     EQ            | 0x2 | 0x40
     GT            | 0x4 | 0x00
     LT            | 0x8 | 0x01
   */

   // ccPPC32 = Shl(1, (0x2 & ~(ccIR>>5)) || (0x1 & (XOR(ccIR, ccIR>>6))))
   assign(
      ccPPC32,
      binop(Iop_Shl32, mkU32(1),
            unop(Iop_32to8, 
                 binop(Iop_Or32,
                       binop(Iop_And32, mkU32(2),
                             unop(Iop_Not32,
                                  binop(Iop_Shr32, mkexpr(ccIR), mkU8(5)))),
                       binop(Iop_And32, mkU32(1),
                             binop(Iop_Xor32, mkexpr(ccIR),
                                   binop(Iop_Shr32, mkexpr(ccIR), mkU8(6)))))))
   );

   putCR0( crfD, unop( Iop_32to8, 
                       binop(Iop_And32, mkexpr(ccPPC32), mkU32(1))) );
   putCR321( crfD, unop( Iop_32to8, 
                         binop(Iop_And32, mkexpr(ccPPC32), mkU32(7<<1))) );

   // CAB: Useful to support writing cc to FPSCR->FPCC ?
   // putReg_field( PPC32_SPR_FPSCR, mkexpr(ccPPC32), 3 );

   // Note: Differences between fcmpu and fcmpo are only
   // in exception flag settings, which aren't supported anyway...
   opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */
   switch (opc2) {
      case 0x000: // fcmpu (Floating Compare Unordered, PPC32 p403)
         DIP("fcmpu crf%d,fr%d,fr%d\n", crfD, frA_addr, frB_addr);
         break;

      case 0x020: // fcmpo (Floating Compare Ordered, PPC32 p402)
         DIP("fcmpo crf%d,fr%d,fr%d\n", crfD, frA_addr, frB_addr);
         break;
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

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp r_tmp = newTemp(Ity_I32);

   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_round(PPC32)(instr)\n");
      return False;
   }

   assign( frB, getFReg(frB_addr));

   opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */

   switch (opc2) {

      case 0x00C: // frsp (Floating Round to Single, PPC32 p423)
         DIP("frsp%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
         assign( frD, roundToSgl( mkexpr(frB) ));
         break;

//zz    case 0x00E: // fctiw (Floating Conv to Int, PPC32 p404)
//zz       DIP("fctiw%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
//zz       assign( r_tmp, binop(Iop_F64toI32, get_roundingmode(), mkexpr(frB)) );
//zz       assign( frD, unop( Iop_ReinterpI64asF64,
//zz                          unop( Iop_32Uto64, mkexpr(r_tmp))));
//zz       break;

      case 0x00F: // fctiwz (Floating Conv to Int, Round to Zero, PPC32 p405)
         DIP("fctiwz%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
         assign( r_tmp, binop(Iop_F64toI32, mkU32(0x3), mkexpr(frB)) );
         assign( frD, unop( Iop_ReinterpI64asF64,
                            unop( Iop_32Uto64, mkexpr(r_tmp))));
         break;

      default:
         vex_printf("dis_fp_round(PPC32)(opc2)\n");
         return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);

   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_move(PPC32)(instr)\n");
      return False;
   }

   assign( frB, getFReg(frB_addr));

   opc2 = (theInstr >> 1) & 0x3FF;    /* theInstr[1:10] */

   switch (opc2) {

      case 0x028: // fneg (Floating Negate, PPC32 p416)
         DIP("fneg%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
         assign( frD, unop( Iop_NegF64, mkexpr(frB) ));
         break;

      case 0x048: // fmr (Floating Move Register, PPC32 p410)
         DIP("fmr%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
         assign( frD, mkexpr(frB) );
         break;

      case 0x088: // fnabs (Floating Negative Absolute Value, PPC32 p415)
         DIP("fnabs%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
         assign( frD, unop( Iop_NegF64, unop( Iop_AbsF64, mkexpr(frB) )));
         break;

      case 0x108: // fabs (Floating Absolute Value, PPC32 p399)
         DIP("fabs%s fr%d,fr%d\n", flag_Rc ? "." : "", frD_addr, frB_addr);
         assign( frD, unop( Iop_AbsF64, mkexpr(frB) ));
         break;

      default:
         vex_printf("dis_fp_move(PPC32)(opc2)\n");
         return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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
//zz    case 0x026: { // mtfsb1 (Move to FPSCR Bit 1, PPC32 p479)
//zz       // Bit crbD of the FPSCR is set.
//zz       UChar crbD    = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz       UInt  b11to20 =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */
//zz 
//zz       if (b11to20 != 0) {
//zz          vex_printf("dis_fp_scr(PPC32)(instr,mtfsb1)\n");
//zz          return False;
//zz       }
//zz       DIP("mtfsb1%s crb%d \n", flag_Rc ? "." : "", crbD);
//zz       putReg_bit( PPC32_SPR_FPSCR, mkU32(1), 31-crbD );
//zz       break;
//zz    }
//zz 
//zz    case 0x040: { // mcrfs (Move to Condition Register from FPSCR, PPC32 p465)
//zz       UChar crfD    = toUChar((theInstr >> 23) & 0x7);  /* theInstr[23:25] */
//zz       UChar b21to22 = toUChar((theInstr >> 21) & 0x3);  /* theInstr[21:22] */
//zz       UChar crfS    = toUChar((theInstr >> 18) & 0x7);  /* theInstr[18:20] */
//zz       UChar b11to17 = toUChar((theInstr >> 11) & 0x7F); /* theInstr[11:17] */
//zz 
//zz       IRTemp tmp = newTemp(Ity_I32);
//zz 
//zz       if (b21to22 != 0 || b11to17 != 0 || flag_Rc != 0) {
//zz          vex_printf("dis_fp_scr(PPC32)(instr,mcrfs)\n");
//zz          return False;
//zz       }
//zz       DIP("mcrfs crf%d,crf%d\n", crfD, crfS);
//zz       assign( tmp, getReg_field( PPC32_SPR_FPSCR, 7-crfS ) );
//zz       putReg_field( PPC32_SPR_CR, mkexpr(tmp), 7-crfD );
//zz       break;
//zz    }

   case 0x046: { // mtfsb0 (Move to FPSCR Bit 0, PPC32 p478)
      // Bit crbD of the FPSCR is cleared.
      UChar crbD    = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
      UInt  b11to20 =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsb0)\n");
         return False;
      }      
      DIP("mtfsb0%s crb%d\n", flag_Rc ? "." : "", crbD);
      putReg_bit( PPC32_SPR_FPSCR, mkU32(0), 31-crbD );
      break;
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
      putReg_field( PPC32_SPR_FPSCR, mkU32(IMM), 7-crfD );
      break;
   }

   case 0x247: { // mffs (Move from FPSCR, PPC32 p468)
      UChar frD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
      UInt  b11to20  =         (theInstr >> 11) & 0x3FF; /* theInstr[11:20] */

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mffs)\n");
         return False;
      }
      DIP("mffs%s fr%d\n", flag_Rc ? "." : "", frD_addr);
      putFReg( frD_addr, unop( Iop_ReinterpI64asF64,
                               unop( Iop_32Uto64, 
                                     getReg_masked( PPC32_SPR_FPSCR, 0x3 ) )));
      break;
   }

   case 0x2C7: { // mtfsf (Move to FPSCR Fields, PPC32 p480)
      UChar b25      = toUChar((theInstr >> 25) & 0x1);  /* theInstr[25]    */
      UChar FM       = toUChar((theInstr >> 17) & 0xFF); /* theInstr[17:24] */
      UChar b16      = toUChar((theInstr >> 16) & 0x1);  /* theInstr[16]    */
      UChar frB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
      IRTemp frB = newTemp(Ity_F64);
      IRTemp rB_32 = newTemp(Ity_I32);
      Int mask = 0;
      Int i = 0;

      if (b25 != 0 || b16 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsf)\n");
         return False;
      }      
      DIP("mtfsf%s %d,fr%d\n", flag_Rc ? "." : "", FM, frB_addr);
      assign( frB, getFReg(frB_addr));
      assign( rB_32, unop( Iop_64to32,
                           unop( Iop_ReinterpF64asI64, mkexpr(frB) )));
      // Build 32bit mask from FM:
      for (i=0; i<8; i++) {
         if ((FM & (1<<(7-i))) == 1) {
            mask |= 0xF << (7-i);
         }
      }
      putReg_masked( PPC32_SPR_FPSCR, mkexpr(rB_32), mask );
      break;
   }

   default:
      vex_printf("dis_fp_scr(PPC32)(opc2)\n");
      return False;
   }
   return True;
}



//zz /*------------------------------------------------------------*/
//zz /*--- AltiVec Instruction Translation                      ---*/
//zz /*------------------------------------------------------------*/
//zz 
//zz /*
//zz   Altivec Cache Control Instructions (Data Streams)
//zz */
//zz static Bool dis_av_datastream ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar flag_T   = toUChar((theInstr >> 25) & 0x1);  /* theInstr[25]    */
//zz    UChar flag_A   = toUChar((theInstr >> 25) & 0x1);  /* theInstr[25]    */
//zz    UChar b23to24  = toUChar((theInstr >> 23) & 0x3);  /* theInstr[23:24] */
//zz    UChar STRM     = toUChar((theInstr >> 21) & 0x3);  /* theInstr[21:22] */
//zz    UChar rA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar rB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
//zz    UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */
//zz 
//zz    if (opc1 != 0x1F || b23to24 != 0 || b0 != 0) {
//zz       vex_printf("dis_av_datastream(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x156: // dst (Data Stream Touch, AV p115)
//zz       DIP("dst%s r%d,r%d,%d\n", flag_T ? "t" : "", rA_addr, rB_addr, STRM);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x176: // dstst (Data Stream Touch for Store, AV p117)
//zz       DIP("dstst%s r%d,r%d,%d\n", flag_T ? "t" : "", rA_addr, rB_addr, STRM);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x336: // dss (Data Stream Stop, AV p114)
//zz       if (rA_addr != 0 || rB_addr != 0) {
//zz          vex_printf("dis_av_datastream(PPC32)(opc2,dst)\n");
//zz          return False;
//zz       }
//zz       if (flag_A == 0) {
//zz 	DIP("dss %d\n", STRM);
//zz 	DIP(" => not implemented\n");
//zz       } else {
//zz 	DIP("dssall\n");
//zz 	DIP(" => not implemented\n");
//zz       }
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_datastream(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Processor Control Instructions
//zz */
//zz static Bool dis_av_procctl ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  0) & 0x7FF; /* theInstr[0:10]  */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_procctl(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x604: // mfvscr (Move from VSCR, AV p129)
//zz       if (vA_addr != 0 || vB_addr != 0) {
//zz          vex_printf("dis_av_procctl(PPC32)(opc2,dst)\n");
//zz          return False;
//zz       }
//zz       DIP("mfvscr v%d\n", vD_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x644: // mtvscr (Move to VSCR, AV p130)
//zz       if (vD_addr != 0 || vA_addr != 0) {
//zz          vex_printf("dis_av_procctl(PPC32)(opc2,dst)\n");
//zz          return False;
//zz       }
//zz       DIP("mtvscr v%d\n", vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_procctl(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }

/*
  AltiVec Load Instructions
*/
static Bool dis_av_load ( UInt theInstr )
{
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar rA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar rB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   IRTemp EA          = newTemp(Ity_I32);
   IRTemp EA_aligned  = newTemp(Ity_I32);

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_av_load(PPC32)(instr)\n");
      return False;
   }

   assign( EA, binop(Iop_Add32,
                     ((rA_addr == 0) ? mkU32(0) : getIReg(rA_addr)),
                     getIReg(rB_addr) ));

   switch (opc2) {

//zz    case 0x006: // lvsl (Load Vector for Shift Left, AV p123)
//zz       DIP("lvsl v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x026: // lvsr (Load Vector for Shift Right, AV p125)
//zz       DIP("lvsr v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x007: // lvebx (Load Vector Element Byte Indexed, AV p119)
//zz       DIP("lvebx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x027: // lvehx (Load Vector Element Half Word Indexed, AV p121)
//zz       DIP("lvehx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x047: // lvewx (Load Vector Element Word Indexed, AV p122)
//zz       DIP("lvewx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;

   case 0x067: // lvx (Load Vector Indexed, AV p127)
      DIP("lvx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_aligned)) );
      break;

//zz    case 0x167: // lvxl (Load Vector Indexed LRU, AV p128)
//zz      // XXX: lvxl gives explicit control over cache block replacement
//zz       DIP("lvxl v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;

   default:
      vex_printf("dis_av_load(PPC32)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Store Instructions
*/
static Bool dis_av_store ( UInt theInstr )
{
   UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
   UChar vS_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
   UChar rA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
   UChar rB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
   UInt  opc2     =         (theInstr >>  1) & 0x3FF; /* theInstr[1:10]  */
   UChar b0       = toUChar((theInstr >>  0) & 1);    /* theInstr[0]     */

   IRTemp rA = newTemp(Ity_I32);
   IRTemp rB = newTemp(Ity_I32);
   IRTemp vS = newTemp(Ity_V128);
   IRTemp EA = newTemp(Ity_I32);
   IRTemp EA_aligned = newTemp(Ity_I32);

   assign( rA, getIReg(rA_addr));
   assign( rB, getIReg(rB_addr));
   assign( vS, getVReg(vS_addr));

   if (rA_addr == 0) {
      assign( EA, mkexpr(rB) );
   } else {
      assign( EA, binop(Iop_Add32, mkexpr(rA), mkexpr(rB)) );
   }

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_av_store(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
//zz    case 0x087: // stvebx (Store Vector Byte Indexed, AV p131)
//zz       DIP("stvebx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz //      eb = EA & 0xF;
//zz //      STORE(vS[eb*8:eb*8+7], 1, EA);
//zz //      storeBE( mkexpr(EA), mkexpr(vS) );
//zz 
//zz    case 0x0A7: // stvehx (Store Vector Half Word Indexed, AV p132)
//zz       DIP("stvehx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz //      EA_aligned = EA & 0xFFFF_FFFE
//zz //      eb = EA_aligned & 0xF;
//zz //      STORE(vS[eb*8:eb*8+15], 2, EA_aligned);
//zz 
//zz    case 0x0C7: // stvewx (Store Vector Word Indexed, AV p133)
//zz       DIP("stvewx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz //      EA_aligned = EA & 0xFFFF_FFFC
//zz //      eb = EA_aligned & 0xF;
//zz //      STORE(vS[eb*8:eb*8+31], 4, EA_aligned);

   case 0x0E7: // stvx (Store Vector Indexed, AV p134)
      DIP("stvx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      storeBE( mkexpr(EA_aligned), mkexpr(vS) );
      break;

//zz    case 0x1E7: // stvxl (Store Vector Indexed LRU, AV p135)
//zz      // XXX: stvxl can give explicit control over cache block replacement
//zz       DIP("stvxl v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz    
//zz //      EA_aligned = EA & 0xFFFF_FFF0;
//zz //      STORE(vS, 16, EA);

   default:
      vex_printf("dis_av_store(PPC32)(opc2)\n");
      return False;
   }
   return True;
}

//zz /*
//zz   AltiVec Arithmetic Instructions
//zz */
//zz static Bool dis_av_arith ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  0) & 0x7FF; /* theInstr[0:10]  */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_arith(PPC32)(opc1 != 0x4)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    /* Add */
//zz    case 0x180: // vaddcuw (Add Carryout Unsigned Word, AV p136)
//zz       DIP("vaddcuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x000: // vaddubm (Add Unsigned Byte Modulo, AV p141)
//zz       DIP("vaddubm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x040: // vadduhm (Add Unsigned Half Word Modulo, AV p143)
//zz       DIP("vadduhm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x080: // vadduwm (Add Unsigned Word Modulo, AV p145)
//zz       DIP("vadduwm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x200: // vaddubs (Add Unsigned Byte Saturate, AV p142)
//zz       DIP("vaddubs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x240: // vadduhs (Add Unsigned Half Word Saturate, AV p144)
//zz       DIP("vadduhs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x280: // vadduws (Add Unsigned Word Saturate, AV p146)
//zz       DIP("vadduws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x300: // vaddsbs (Add Signed Byte Saturate, AV p138)
//zz       DIP("vaddsbs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x340: // vaddshs (Add Signed Half Word Saturate, AV p139)
//zz       DIP("vaddshs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x380: // vaddsws (Add Signed Word Saturate, AV p140)
//zz       DIP("vaddsws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    /* Subtract */
//zz    case 0x580: // vsubcuw (Subtract Carryout Unsigned Word, AV p260)
//zz       DIP("vsubcuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x400: // vsububm (Subtract Unsigned Byte Modulo, AV p265)
//zz       DIP("vsububm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x440: // vsubuhm (Subtract Unsigned Half Word Modulo, AV p267)
//zz       DIP("vsubuhm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x480: // vsubuwm (Subtract Unsigned Word Modulo, AV p269)
//zz       DIP("vsubuwm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x600: // vsububs (Subtract Unsigned Byte Saturate, AV p266)
//zz       DIP("vsububs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x640: // vsubuhs (Subtract Unsigned Half Word Saturate, AV p268)
//zz       DIP("vsubuhs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x680: // vsubuws (Subtract Unsigned Word Saturate, AV p270)
//zz       DIP("vsubuws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x700: // vsubsbs (Subtract Signed Byte Saturate, AV p262)
//zz       DIP("vsubsbs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x740: // vsubshs (Subtract Signed Half Word Saturate, AV p263)
//zz       DIP("vsubshs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x780: // vsubsws (Subtract Signed Word Saturate, AV p264)
//zz       DIP("vsubsws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz 
//zz    /* Maximum */
//zz    case 0x002: // vmaxub (Maximum Unsigned Byte, AV p182)
//zz       DIP("vmaxub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x042: // vmaxuh (Maximum Unsigned Half Word, AV p183)
//zz       DIP("vmaxuh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x082: // vmaxuw (Maximum Unsigned Word, AV p184)
//zz       DIP("vmaxuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x102: // vmaxsb (Maximum Signed Byte, AV p179)
//zz       DIP("vmaxsb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x142: // vmaxsh (Maximum Signed Half Word, AV p180)
//zz       DIP("vmaxsh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x182: // vmaxsw (Maximum Signed Word, AV p181)
//zz       DIP("vmaxsw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz 
//zz    /* Minimum */
//zz    case 0x202: // vminub (Minimum Unsigned Byte, AV p191)
//zz       DIP("vminub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x242: // vminuh (Minimum Unsigned Half Word, AV p192)
//zz       DIP("vminuh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x282: // vminuw (Minimum Unsigned Word, AV p193)
//zz       DIP("vminuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x302: // vminsb (Minimum Signed Byte, AV p188)
//zz       DIP("vminsb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x342: // vminsh (Minimum Signed Half Word, AV p189)
//zz       DIP("vminsh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x382: // vminsw (Minimum Signed Word, AV p190)
//zz       DIP("vminsw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz 
//zz    /* Average */
//zz    case 0x402: // vavgub (Average Unsigned Byte, AV p152)
//zz       DIP("vavgub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x442: // vavguh (Average Unsigned Half Word, AV p153)
//zz       DIP("vavguh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x482: // vavguw (Average Unsigned Word, AV p154)
//zz       DIP("vavguw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x502: // vavgsb (Average Signed Byte, AV p149)
//zz       DIP("vavgsb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x542: // vavgsh (Average Signed Half Word, AV p150)
//zz       DIP("vavgsh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x582: // vavgsw (Average Signed Word, AV p151)
//zz       DIP("vavgsw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz 
//zz    /* Multiply */
//zz    case 0x008: // vmuloub (Multiply Odd Unsigned Byte, AV p213)
//zz       DIP("vmuloub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x048: // vmulouh (Multiply Odd Unsigned Half Word, AV p214)
//zz       DIP("vmulouh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x108: // vmulosb (Multiply Odd Signed Byte, AV p211)
//zz       DIP("vmulosb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x148: // vmulosh (Multiply Odd Signed Half Word, AV p212)
//zz       DIP("vmulosh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x208: // vmuleub (Multiply Even Unsigned Byte, AV p209)
//zz       DIP("vmuleub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x248: // vmuleuh (Multiply Even Unsigned Half Word, AV p210)
//zz       DIP("vmuleuh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x308: // vmulesb (Multiply Even Signed Byte, AV p207)
//zz       DIP("vmulesb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x348: // vmulesh (Multiply Even Signed Half Word, AV p208)
//zz       DIP("vmulesh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz 
//zz    /* Sum Across Partial */
//zz    case 0x608: // vsum4ubs (Sum Partial (1/4) UB Saturate, AV p275)
//zz       DIP("vsum4ubs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x708: // vsum4sbs (Sum Partial (1/4) SB Saturate, AV p273)
//zz       DIP("vsum4sbs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x648: // vsum4shs (Sum Partial (1/4) SHW Saturate, AV p274)
//zz       DIP("vsum4shs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x688: // vsum2sws (Sum Partial (1/2) SW Saturate, AV p272)
//zz       DIP("vsum2sws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x788: // vsumsws  (Sum SW Saturate, AV p271)
//zz       DIP("vsumsws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_arith(PPC32)(opc2=0x%x)\n", opc2);
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Logic Instructions
//zz */
//zz static Bool dis_av_logic ( UInt theInstr )
//zz {
//zz    UChar opc1    = toUChar((theInstr >> 26) & 0x3F);  /* theInstr[26:31] */
//zz    UChar vD_addr = toUChar((theInstr >> 21) & 0x1F);  /* theInstr[21:25] */
//zz    UChar vA_addr = toUChar((theInstr >> 16) & 0x1F);  /* theInstr[16:20] */
//zz    UChar vB_addr = toUChar((theInstr >> 11) & 0x1F);  /* theInstr[11:15] */
//zz    UInt  opc2    =         (theInstr >>  0) & 0x7FF;  /* theInstr[0:10]  */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_logic(PPC32)(opc1 != 0x4)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x404: // vand (And, AV p147)
//zz       DIP("vand v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x444: // vandc (And, AV p148)
//zz       DIP("vandc v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x484: // vor (Or, AV p217)
//zz       DIP("vor v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x4C4: // vxor (Xor, AV p282)
//zz       DIP("vxor v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x504: // vnor (Nor, AV p216)
//zz       DIP("vnor v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_logic(PPC32)(opc2=0x%x)\n", opc2);
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Compare Instructions
//zz */
//zz static Bool dis_av_cmp ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UChar flag_Rc  = toUChar((theInstr >> 10) & 0x1);  /* theInstr[10]    */
//zz    UInt  opc2     =         (theInstr >>  0) & 0x3FF; /* theInstr[0:9]   */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_cmp(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x006: // vcmpequb (Compare Equal-to Unsigned B, AV p160)
//zz       DIP("vcmpequb%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x046: // vcmpequh (Compare Equal-to Unsigned HW, AV p161)
//zz       DIP("vcmpequh%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x086: // vcmpequw (Compare Equal-to Unsigned W, AV p162)
//zz       DIP("vcmpequw%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x206: // vcmpgtub (Compare Greater-than Unsigned B, AV p168)
//zz       DIP("vcmpgtub%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x246: // vcmpgtuh (Compare Greater-than Unsigned HW, AV p169)
//zz       DIP("vcmpgtuh%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x286: // vcmpgtuw (Compare Greater-than Unsigned W, AV p170)
//zz       DIP("vcmpgtuw%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x306: // vcmpgtsb (Compare Greater-than Signed B, AV p165)
//zz       DIP("vcmpgtsb%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x346: // vcmpgtsh (Compare Greater-than Signed HW, AV p166)
//zz       DIP("vcmpgtsh%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x386: // vcmpgtsw (Compare Greater-than Signed W, AV p167)
//zz       DIP("vcmpgtsw%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_cmp(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Multiply-Sum Instructions
//zz */
//zz static Bool dis_av_multarith ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UChar vC_addr  = toUChar((theInstr >>  6) & 0x1F); /* theInstr[6:10]  */
//zz    UChar opc2     = toUChar((theInstr >>  0) & 0x3F); /* theInstr[0:5]   */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_multarith(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz 
//zz    /* Multiply-Add */
//zz    case 0x20: // vmhaddshs (Multiply High, Add Signed HW Saturate, AV p185)
//zz       DIP("vmhaddshs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x21: // vmhraddshs (Multiply High Round, Add Signed HW Saturate, AV p186)
//zz       DIP("vmhraddshs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x22: // vmladduhm (Multiply Low, Add Unsigned HW Modulo, AV p194)
//zz       DIP("vmladduhm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz 
//zz    /* Multiply-Sum */
//zz    case 0x24: // vmsumubm (Multiply Sum Unsigned B Modulo, AV p204)
//zz       DIP("vmsumubm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x25: // vmsummbm (Multiply Sum Mixed-Sign B Modulo, AV p201)
//zz       DIP("vmsummbm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x26: // vmsumuhm (Multiply Sum Unsigned HW Modulo, AV p205)
//zz       DIP("vmsumuhm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x27: // vmsumuhs (Multiply Sum Unsigned HW Saturate, AV p206)
//zz       DIP("vmsumuhs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x28: // vmsumshm (Multiply Sum Signed HW Modulo, AV p202)
//zz       DIP("vmsumshm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x29: // vmsumshs (Multiply Sum Signed HW Saturate, AV p203)
//zz       DIP("vmsumshs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_multarith(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Shift/Rotate Instructions
//zz */
//zz static Bool dis_av_shift ( UInt theInstr )
//zz {
//zz    UChar opc1    = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2    =         (theInstr >>  0) & 0x7FF; /* theInstr[0:10]  */
//zz 
//zz    if (opc1 != 0x4){
//zz       vex_printf("dis_av_shift(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    /* Rotate */
//zz    case 0x004: // vrlb (Rotate Left Integer B, AV p234)
//zz       DIP("vrlb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x044: // vrlh (Rotate Left Integer HW, AV p235)
//zz       DIP("vrlh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x084: // vrlw (Rotate Left Integer W, AV p236)
//zz       DIP("vrlw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz 
//zz    /* Shift Left */
//zz    case 0x104: // vslb (Shift Left Integer B, AV p240)
//zz       DIP("vslb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x144: // vslh (Shift Left Integer HW, AV p242)
//zz       DIP("vslh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x184: // vslw (Shift Left Integer W, AV p244)
//zz       DIP("vslw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x1C4: // vsl (Shift Left, AV p239)
//zz       DIP("vsl v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x40C: // vslo (Shift Left by Octet, AV p243)
//zz       DIP("vslo v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    /* Shift Right */
//zz    case 0x204: // vsrb (Shift Right B, AV p256)
//zz       DIP("vsrb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x244: // vsrh (Shift Right HW, AV p257)
//zz       DIP("vsrh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x284: // vsrw (Shift Right W, AV p259)
//zz       DIP("vsrw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x2C4: // vsr (Shift Right, AV p252)
//zz       DIP("vsr v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x304: // vsrab (Shift Right Algebraic B, AV p253)
//zz       DIP("vsrab v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x344: // vsrah (Shift Right Algebraic HW, AV p254)
//zz       DIP("vsrah v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x384: // vsraw (Shift Right Algebraic W, AV p255)
//zz       DIP("vsraw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x44C: // vsro (Shift Right by Octet, AV p258)
//zz       DIP("vsro v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_shift(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Permute Instructions
//zz */
//zz static Bool dis_av_permute ( UInt theInstr )
//zz {
//zz    UChar opc1      = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr   = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr   = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar UIMM_5    = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar SIMM_5    = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr   = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UChar vC_addr   = toUChar((theInstr >>  6) & 0x1F); /* theInstr[6:10]  */
//zz    UChar b10       = toUChar((theInstr >> 10) & 0x1);  /* theInstr[10]    */
//zz    UChar SHB_uimm4 = toUChar((theInstr >>  6) & 0xF);  /* theInstr[6:9]   */
//zz    UInt  opc2      =         (theInstr >>  0) & 0x3F;  /* theInstr[0:5]   */
//zz 
//zz    UChar SIMM_8 = extend_s_5to8(SIMM_5);
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_permute(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x2A: // vsel (Conditional Select, AV p238)
//zz       DIP("vsel v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz      
//zz    case 0x2B: // vperm (Permute, AV p218)
//zz       DIP("vperm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x2C: // vsldoi (Shift Left Double by Octet Imm, AV p241)
//zz       if (b10 != 0) {
//zz          vex_printf("dis_av_permute(PPC32)(vsldoi)\n");
//zz          return False;
//zz       }
//zz       DIP("vsldoi v%d,v%d,v%d,%d\n", vD_addr, vA_addr, vB_addr, SHB_uimm4);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz      break; // Fall through...
//zz    }
//zz 
//zz    opc2 = (theInstr) & 0x7FF; /* theInstr[0:10]  */
//zz    switch (opc2) {
//zz 
//zz    /* Merge */
//zz    case 0x00C: // vmrghb (Merge High B, AV p195)
//zz       DIP("vmrghb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x04C: // vmrghh (Merge High HW, AV p196)
//zz       DIP("vmrghh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x08C: // vmrghw (Merge High W, AV p197)
//zz       DIP("vmrghw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x10C: // vmrglb (Merge Low B, AV p198)
//zz       DIP("vmrglb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x14C: // vmrglh (Merge Low HW, AV p199)
//zz       DIP("vmrglh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x18C: // vmrglw (Merge Low W, AV p200)
//zz       DIP("vmrglw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    /* Splat */
//zz    case 0x20C: // vspltb (Splat Byte, AV p245)
//zz       DIP("vspltb v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x24C: // vsplth (Splat Half Word, AV p246)
//zz       DIP("vsplth v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x28C: // vspltw (Splat Word, AV p250)
//zz       DIP("vspltw v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x30C: // vspltisb (Splat Immediate Signed B, AV p247)
//zz       DIP("vspltisb v%d,%d\n", vD_addr, (Char)SIMM_8);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x34C: // vspltish (Splat Immediate Signed HW, AV p248)
//zz       DIP("vspltish v%d,%d\n", vD_addr, (Char)SIMM_8);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x38C: // vspltisw (Splat Immediate Signed W, AV p249)
//zz       DIP("vspltisw v%d,%d\n", vD_addr, (Char)SIMM_8);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_permute(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Pack/Unpack Instructions
//zz */
//zz static Bool dis_av_pack ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  0) & 0x7FF; /* theInstr[0:10]  */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_pack(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    /* Packing */
//zz    case 0x00E: // vpkuhum (Pack Unsigned HW Unsigned Modulo, AV p224)
//zz       DIP("vpkuhum v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x04E: // vpkuwum (Pack Unsigned W Unsigned Modulo, AV p226)
//zz       DIP("vpkuwum v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x08E: // vpkuhus (Pack Unsigned HW Unsigned Saturate, AV p225)
//zz       DIP("vpkuhus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x0CE: // vpkuwus (Pack Unsigned W Unsigned Saturate, AV p227)
//zz       DIP("vpkuwus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x10E: // vpkshus (Pack Signed HW Unsigned Saturate, AV p221)
//zz       DIP("vpkshus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x14E: // vpkswus (Pack Signed W Unsigned Saturate, AV p223)
//zz       DIP("vpkswus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x18E: // vpkshss (Pack Signed HW Signed Saturate, AV p220)
//zz       DIP("vpkshss v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x1CE: // vpkswss (Pack Signed W Signed Saturate, AV p222)
//zz       DIP("vpkswss v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x30E: // vpkpx (Pack Pixel, AV p219)
//zz       DIP("vpkpx v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       break; // Fall through...
//zz    }
//zz 
//zz 
//zz    if (vA_addr != 0) {
//zz       vex_printf("dis_av_pack(PPC32)(vA_addr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    /* Unpacking */
//zz    case 0x20E: // vupkhsb (Unpack High Signed B, AV p277)
//zz       DIP("vupkhsb v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x24E: // vupkhsh (Unpack High Signed HW, AV p278)
//zz       DIP("vupkhsh v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x28E: // vupklsb (Unpack Low Signed B, AV p280)
//zz       DIP("vupklsb v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x2CE: // vupklsh (Unpack Low Signed HW, AV p281)
//zz       DIP("vupklsh v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x34E: // vupkhpx (Unpack High Pixel16, AV p276)
//zz       DIP("vupkhpx v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x3CE: // vupklpx (Unpack Low Pixel16, AV p279)
//zz       DIP("vupklpx v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_pack(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz 
//zz /*
//zz   AltiVec Floating Point Arithmetic Instructions
//zz */
//zz static Bool dis_av_fp_arith ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UChar vC_addr  = toUChar((theInstr >>  6) & 0x1F); /* theInstr[6:10] */
//zz    UInt  opc2=0;
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_fp_arith(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    opc2 = (theInstr) & 0x3F;  /* theInstr[0:5]   */
//zz    switch (opc2) {
//zz    case 0x2E: // vmaddfp (Multiply Add FP, AV p177)
//zz       DIP("vmaddfp v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vC_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x2F: // vnmsubfp (Negative Multiply-Subtract FP, AV p215)
//zz       DIP("vnmsubfp v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vC_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz      break; // Fall through...
//zz    }
//zz 
//zz    opc2 = (theInstr) & 0x7FF; /* theInstr[0:10]  */
//zz    switch (opc2) {
//zz    case 0x00A: // vaddfp (Add FP, AV p137)
//zz       DIP("vaddfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz   case 0x04A: // vsubfp (Subtract FP, AV p261)
//zz       DIP("vsubfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x40A: // vmaxfp (Maximum FP, AV p178)
//zz       DIP("vmaxfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x44A: // vminfp (Minimum FP, AV p187)
//zz       DIP("vminfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       break; // Fall through...
//zz    }
//zz 
//zz 
//zz    if (vA_addr != 0) {
//zz       vex_printf("dis_av_fp_arith(PPC32)(vA_addr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x10A: // vrefp (Reciprocal Esimate FP, AV p228)
//zz       DIP("vrefp v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x14A: // vrsqrtefp (Reciprocal Square Root Estimate FP, AV p237)
//zz       DIP("vrsqrtefp v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x18A: // vexptefp (2 Raised to the Exp Est FP, AV p173)
//zz       DIP("vexptefp v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x1CA: // vlogefp (Log2 Estimate FP, AV p175)
//zz       DIP("vlogefp v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_fp_arith(PPC32)(opc2=0x%x)\n",opc2);
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Floating Point Compare Instructions
//zz */
//zz static Bool dis_av_fp_cmp ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar vA_addr  = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UChar flag_Rc  = toUChar((theInstr >> 10) & 0x1);  /* theInstr[10]    */
//zz    UInt  opc2     =         (theInstr >>  0) & 0x3FF; /* theInstr[0:9]   */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_fp_cmp(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x0C6: // vcmpeqfp (Compare Equal-to FP, AV p159)
//zz       DIP("vcmpeqfp%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x1C6: // vcmpgefp (Compare Greater-than-or-Equal-to FP, AV p163)
//zz       DIP("vcmpgefp%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x2C6: // vcmpgtfp (Compare Greater-than FP, AV p164)
//zz       DIP("vcmpgtfp%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x3C6: // vcmpbfp (Compare Bounds FP, AV p157)
//zz       DIP("vcmpbfp%s v%d,v%d,v%d\n", (flag_Rc ? ".":""), vD_addr, vA_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_fp_cmp(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }
//zz 
//zz /*
//zz   AltiVec Floating Point Convert/Round Instructions
//zz */
//zz static Bool dis_av_fp_convert ( UInt theInstr )
//zz {
//zz    UChar opc1     = toUChar((theInstr >> 26) & 0x3F); /* theInstr[26:31] */
//zz    UChar vD_addr  = toUChar((theInstr >> 21) & 0x1F); /* theInstr[21:25] */
//zz    UChar UIMM_5   = toUChar((theInstr >> 16) & 0x1F); /* theInstr[16:20] */
//zz    UChar vB_addr  = toUChar((theInstr >> 11) & 0x1F); /* theInstr[11:15] */
//zz    UInt  opc2     =         (theInstr >>  0) & 0x7FF; /* theInstr[0:10]  */
//zz 
//zz    if (opc1 != 0x4) {
//zz       vex_printf("dis_av_fp_convert(PPC32)(instr)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x30A: // vcfux (Convert from Unsigned Fixed-Point W, AV p156)
//zz       DIP("vcfux v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x34A: // vcfsx (Convert from Signed Fixed-Point W, AV p155)
//zz       DIP("vcfsx v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x38A: // vctuxs (Convert to Unsigned Fixed-Point W Saturate, AV p172)
//zz       DIP("vctuxs v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x3CA: // vctsxs (Convert to Signed Fixed-Point W Saturate, AV p171)
//zz       DIP("vctsxs v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz      break;    // Fall through...
//zz    }
//zz 
//zz    if (UIMM_5 != 0) {
//zz       vex_printf("dis_av_fp_convert(PPC32)(UIMM_5)\n");
//zz       return False;
//zz    }
//zz 
//zz    switch (opc2) {
//zz    case 0x20A: // vrfin (Round to FP Integer Nearest, AV p231)
//zz       DIP("vrfin v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x24A: // vrfiz (Round to FP Integer toward zero, AV p233)
//zz       DIP("vrfiz v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x28A: // vrfip (Round to FP Integer toward +inf, AV p232)
//zz       DIP("vrfip v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    case 0x2CA: // vrfim (Round to FP Integer toward -inf, AV p230)
//zz       DIP("vrfim v%d,v%d\n", vD_addr, vB_addr);
//zz       DIP(" => not implemented\n");
//zz       return False;
//zz 
//zz    default:
//zz       vex_printf("dis_av_fp_convert(PPC32)(opc2)\n");
//zz       return False;
//zz    }
//zz    return True;
//zz }






/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

static   
DisResult disInstr_PPC32_WRK ( 
             Bool         put_IP,
             Bool         (*resteerOkFn) ( Addr64 ),
             Long         delta64,
             VexArchInfo* archinfo 
          )
{
   UChar     opc1;
   UInt      opc2;
   DisResult dres;
   UInt      theInstr;

   /* The running delta */
   Int delta = (Int)delta64;

   /* Set result defaults. */
   dres.whatNext   = Dis_Continue;
   dres.len        = 0;
   dres.continueAt = 0;

   /* At least this is simple on PPC32: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   theInstr = getUIntBigendianly( (UChar*)(&guest_code[delta]) );

   DIP("\t0x%x:  ", guest_CIA_curr_instr);

   /* We may be asked to update the guest CIA before going further. */
   if (put_IP)
      putSPR( PPC32_SPR_CIA, mkU32(guest_CIA_curr_instr) );

   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
      UChar* code = (UChar*)(&guest_code[delta]);

      /* Spot this:                                       
         0x7C03D808   tw 0,3,27            => trap word if (0) => nop
         0x5400E800   rlwinm 0,0,29,0,0    => r0 = rotl(r0,29)
         0x54001800   rlwinm 0,0,3,0,0     => r0 = rotl(r0,3)
         0x54006800   rlwinm 0,0,13,0,0    => r0 = rotl(r0,13)
         0x54009800   rlwinm 0,0,19,0,0    => r0 = rotl(r0,19)
         0x60000000   nop
      */
      if (getUIntBigendianly(code+ 0) == 0x7C03D808 &&
          getUIntBigendianly(code+ 4) == 0x5400E800 &&
          getUIntBigendianly(code+ 8) == 0x54001800 &&
          getUIntBigendianly(code+12) == 0x54006800 &&
          getUIntBigendianly(code+16) == 0x54009800 &&
          getUIntBigendianly(code+20) == 0x60000000) {
         DIP("%%r3 = client_request ( %%r31 )\n");
         dres.len = 24;
         delta += 24;

         irbb->next     = mkU32(guest_CIA_bbstart+delta);
         irbb->jumpkind = Ijk_ClientReq;
         dres.whatNext  = Dis_StopHere;
         goto decode_success;
      }
   }


   opc1 = ifieldOPC(theInstr);
   opc2 = ifieldOPClo10(theInstr);

#if PPC32_TOIR_DEBUG
   vex_printf("\ndisInstr(ppc32): instr:   0x%x\n", theInstr);
   vex_printf("disInstr(ppc32): instr:   ");
   vex_printf_binary( theInstr, 32, True );
   vex_printf("\n");
#endif


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

//zz    /* Integer Load and Store Multiple Instructions */
//zz    case 0x2E: case 0x2F: // lmw, stmw
//zz       if (dis_int_ldst_mult( theInstr )) goto decode_success;
//zz       goto decode_failure;

   /* Branch Instructions */
   case 0x12: case 0x10: // b, bc
      if (dis_branch(theInstr, &dres)) goto decode_success;
      goto decode_failure;

   /* System Linkage Instructions */
   case 0x11: // sc
      if (dis_syslink(theInstr, &dres)) goto decode_success;
      goto decode_failure;

//zz    /* Trap Instructions */
//zz    case 0x03: // twi
//zz       DIP("trap op (twi) => not implemented\n");
//zz       goto decode_failure;

   /* Floating Point Load Instructions */
   case 0x30: case 0x31: case 0x32: // lfs, lfsu, lfd
   case 0x33:                       // lfdu
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

//zz       /* Floating Point Status/Control Register Instructions */         
//zz       case 0x026: // mtfsb1
//zz       case 0x040: // mcrfs
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
            if (dis_branch(theInstr, &dres)) goto decode_success;
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

//zz       /* Integer Load and Store with Byte Reverse Instructions */
//zz       case 0x316: case 0x216: case 0x396: // lhbrx, lwbrx, sthbrx
//zz       case 0x296:                         // stwbrx
//zz          if (dis_int_ldst_rev( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz          
//zz       /* Integer Load and Store String Instructions */
//zz       case 0x255: case 0x215: case 0x2D5: // lswi, lswx, stswi
//zz       case 0x295:                         // stswx
//zz          if (dis_int_ldst_str( theInstr )) goto decode_success;
//zz          goto decode_failure;

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
         if (dis_cache_manage( theInstr, &dres, archinfo )) 
            goto decode_success;
         goto decode_failure;

//zz       /* External Control Instructions */
//zz       case 0x136: case 0x1B6: // eciwx, ecowx
//zz          DIP("external control op => not implemented\n");
//zz          goto decode_failure;
//zz 
//zz       /* Trap Instructions */
//zz       case 0x004:                         // tw
//zz          DIP("trap op (tw) => not implemented\n");
//zz          goto decode_failure;

      /* Floating Point Load Instructions */
      case 0x217: case 0x237: case 0x257: // lfsx, lfsux, lfdx
      case 0x277:                         // lfdux
         if (dis_fp_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* Floating Point Store Instructions */
      case 0x297: case 0x2B7: case 0x2D7: // stfs,  stfsu, stfd
      case 0x2F7: case 0x3D7:             // stfdu, stfiwx
         if (dis_fp_store( theInstr )) goto decode_success;
         goto decode_failure;


//zz       /* AltiVec instructions */
//zz 
//zz       /* AV Cache Control - Data streams */
//zz       case 0x156: case 0x176: case 0x336: // dst, dstst, dss
//zz          if (dis_av_datastream( theInstr )) goto decode_success;
//zz          goto decode_failure;

      /* AV Load */
      case 0x006: case 0x026:             // lvsl, lvsr
      case 0x007: case 0x027: case 0x047: // lvebx, lvehx, lvewx
      case 0x067: case 0x167:             // lvx, lvxl
         if (dis_av_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Store */
      case 0x087: case 0x0A7: case 0x0C7: // stvebx, stvehx, stvewx
      case 0x0E7: case 0x1E7:             // stvx, stvxl
         if (dis_av_store( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;


//zz    case 0x04:
//zz       /* AltiVec instructions */
//zz 
//zz       opc2 = (theInstr) & 0x3F;    /* theInstr[0:5] */
//zz       switch (opc2) {
//zz       /* AV Mult-Add, Mult-Sum */
//zz       case 0x20: case 0x21: case 0x22: // vmhaddshs, vmhraddshs, vmladduhm
//zz       case 0x24: case 0x25: case 0x26: // vmsumubm, vmsummbm, vmsumuhm
//zz       case 0x27: case 0x28: case 0x29: // vmsumuhs, vmsumshm, vmsumshs
//zz          if (dis_av_multarith( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Permutations */
//zz       case 0x2A:                       // vsel
//zz       case 0x2B:                       // vperm
//zz          if (dis_av_permute( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Shift */
//zz       case 0x2C:                       // vsldoi
//zz          if (dis_av_shift( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Floating Point Mult-Add/Sub */
//zz       case 0x2E: case 0x2F:            // vmaddfp, vnmsubfp
//zz          if (dis_av_fp_arith( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       default:
//zz          break;  // Fall through...
//zz       }
//zz 
//zz       opc2 = (theInstr) & 0x7FF;    /* theInstr[0:10] */
//zz       switch (opc2) {
//zz       /* AV Arithmetic */
//zz       case 0x180:                         // vaddcuw
//zz       case 0x000: case 0x040: case 0x080: // vaddubm, vadduhm, vadduwm
//zz       case 0x200: case 0x240: case 0x280: // vaddubs, vadduhs, vadduws
//zz       case 0x300: case 0x340: case 0x380: // vaddsbs, vaddshs, vaddsws
//zz       case 0x580:                         // vsubcuw
//zz       case 0x400: case 0x440: case 0x480: // vsububm, vsubuhm, vsubuwm
//zz       case 0x600: case 0x640: case 0x680: // vsububs, vsubuhs, vsubuws
//zz       case 0x700: case 0x740: case 0x780: // vsubsbs, vsubshs, vsubsws
//zz       case 0x402: case 0x442: case 0x482: // vavgub, vavguh, vavguw
//zz       case 0x502: case 0x542: case 0x582: // vavgsb, vavgsh, vavgsw
//zz       case 0x002: case 0x042: case 0x082: // vmaxub, vmaxuh, vmaxuw
//zz       case 0x102: case 0x142: case 0x182: // vmaxsb, vmaxsh, vmaxsw
//zz       case 0x202: case 0x242: case 0x282: // vminub, vminuh, vminuw
//zz       case 0x302: case 0x342: case 0x382: // vminsb, vminsh, vminsw
//zz       case 0x008: case 0x048:             // vmuloub, vmulouh
//zz       case 0x108: case 0x148:             // vmulosb, vmulosh
//zz       case 0x208: case 0x248:             // vmuleub, vmuleuh
//zz       case 0x308: case 0x348:             // vmulesb, vmulesh
//zz       case 0x608: case 0x708: case 0x648: // vsum4ubs, vsum4sbs, vsum4shs
//zz       case 0x688: case 0x788:             // vsum2sws, vsumsws
//zz          if (dis_av_arith( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Rotate, Shift */
//zz       case 0x004: case 0x044: case 0x084: // vrlb, vrlh, vrlw
//zz       case 0x104: case 0x144: case 0x184: // vslb, vslh, vslw
//zz       case 0x204: case 0x244: case 0x284: // vsrb, vsrh, vsrw
//zz       case 0x304: case 0x344: case 0x384: // vsrab, vsrah, vsraw
//zz       case 0x1C4: case 0x2C4:             // vsl, vsr
//zz       case 0x40C: case 0x44C:             // vslo, vsro
//zz          if (dis_av_shift( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Logic */
//zz       case 0x404: case 0x444: case 0x484: // vand, vandc, vor
//zz       case 0x4C4: case 0x504:             // vxor, vnor
//zz          if (dis_av_logic( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Processor Control */
//zz       case 0x604: case 0x644:             // mfvscr, mtvscr
//zz          if (dis_av_procctl( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Floating Point Arithmetic */
//zz       case 0x00A: case 0x04A:             // vaddfp, vsubfp
//zz       case 0x10A: case 0x14A: case 0x18A: // vrefp, vrsqrtefp, vexptefp
//zz       case 0x1CA:                         // vlogefp
//zz       case 0x40A: case 0x44A:             // vmaxfp, vminfp
//zz          if (dis_av_fp_arith( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Floating Point Round/Convert */
//zz       case 0x20A: case 0x24A: case 0x28A: // vrfin, vrfiz, vrfip
//zz       case 0x2CA:                         // vrfim
//zz       case 0x30A: case 0x34A: case 0x38A: // vcfux, vcfsx, vctuxs
//zz       case 0x3CA:                         // vctsxs
//zz          if (dis_av_fp_convert( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Merge, Splat */
//zz       case 0x00C: case 0x04C: case 0x08C: // vmrghb, vmrghh, vmrghw
//zz       case 0x10C: case 0x14C: case 0x18C: // vmrglb, vmrglh, vmrglw
//zz       case 0x20C: case 0x24C: case 0x28C: // vspltb, vsplth, vspltw
//zz       case 0x30C: case 0x34C: case 0x38C: // vspltisb, vspltish, vspltisw
//zz          if (dis_av_permute( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Pack, Unpack */
//zz       case 0x00E: case 0x04E: case 0x08E: // vpkuhum, vpkuwum, vpkuhus
//zz       case 0x0CE:                         // vpkuwus
//zz       case 0x10E: case 0x14E: case 0x18E: // vpkshus, vpkswus, vpkshss
//zz       case 0x1CE:                         // vpkswss
//zz       case 0x20E: case 0x24E: case 0x28E: // vupkhsb, vupkhsh, vupklsb
//zz       case 0x2CE:                         // vupklsh
//zz       case 0x30E: case 0x34E: case 0x3CE: // vpkpx, vupkhpx, vupklpx
//zz          if (dis_av_pack( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       default:
//zz          break;  // Fall through...
//zz       }
//zz 
//zz       opc2 = (theInstr) & 0x3FF;    /* theInstr[0:9] (Bit 10 = Rc)*/
//zz       switch (opc2) {
//zz 
//zz       /* AV Compare */
//zz       case 0x006: case 0x046: case 0x086: // vcmpequb, vcmpequh, vcmpequw
//zz       case 0x206: case 0x246: case 0x286: // vcmpgtub, vcmpgtuh, vcmpgtuw
//zz       case 0x306: case 0x346: case 0x386: // vcmpgtsb, vcmpgtsh, vcmpgtsw
//zz          if (dis_av_cmp( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       /* AV Floating Point Compare */
//zz       case 0x0C6: case 0x1C6: case 0x2C6: // vcmpeqfp, vcmpgefp, vcmpgtfp
//zz       case 0x3C6:                         // vcmpbfp
//zz          if (dis_av_fp_cmp( theInstr )) goto decode_success;
//zz          goto decode_failure;
//zz 
//zz       default:
//zz          goto decode_failure;
//zz       }
//zz       break;

   default:
   decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(ppc32): unhandled instruction: "
              "0x%x\n", theInstr);
   vex_printf("                 primary %d(0x%x), secondary %d(0x%x)\n", 
              opc1, opc1, opc2, opc2);
   
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
   putSPR( PPC32_SPR_CIA, mkU32(guest_CIA_curr_instr) );
   irbb->next = mkU32(guest_CIA_curr_instr);
   irbb->jumpkind = Ijk_NoDecode;
   dres.whatNext = Dis_StopHere;
   dres.len = 0;
   return dres;

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   DIP("\n");

   dres.len = 4;
   return dres;
}

#undef DIP
#undef DIS


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_PPC32 ( IRBB*        irbb_IN,
                           Bool         put_IP,
                           Bool         (*resteerOkFn) ( Addr64 ),
                           UChar*       guest_code_IN,
                           Long         delta,
                           Addr64       guest_IP,
                           VexArchInfo* archinfo,
                           Bool         host_bigendian_IN )
{
   DisResult dres;

   /* Set globals (see top of this file) */
   guest_code           = guest_code_IN;
   irbb                 = irbb_IN;
   host_is_bigendian    = host_bigendian_IN;
   guest_CIA_curr_instr = (Addr32)guest_IP;
   guest_CIA_bbstart    = (Addr32)toUInt(guest_IP - delta);

   dres = disInstr_PPC32_WRK ( put_IP, resteerOkFn,
                               delta, archinfo );

   return dres;
}


/*--------------------------------------------------------------------*/
/*--- end                                       guest-ppc32/toIR.c ---*/
/*--------------------------------------------------------------------*/
