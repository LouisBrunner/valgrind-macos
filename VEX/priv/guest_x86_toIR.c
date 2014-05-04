
/*--------------------------------------------------------------------*/
/*--- begin                                       guest_x86_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2013 OpenWorks LLP
      info@open-works.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Translates x86 code to IR. */

/* TODO:

   All Puts to CC_OP/CC_DEP1/CC_DEP2/CC_NDEP should really be checked
   to ensure a 32-bit value is being written.

   FUCOMI(P): what happens to A and S flags?  Currently are forced
      to zero.

   x87 FP Limitations:

   * all arithmetic done at 64 bits

   * no FP exceptions, except for handling stack over/underflow

   * FP rounding mode observed only for float->int conversions
     and int->float conversions which could lose accuracy, and
     for float-to-float rounding.  For all other operations, 
     round-to-nearest is used, regardless.

   * some of the FCOM cases could do with testing -- not convinced
     that the args are the right way round.

   * FSAVE does not re-initialise the FPU; it should do

   * FINIT not only initialises the FPU environment, it also
     zeroes all the FP registers.  It should leave the registers
     unchanged.

   SAHF should cause eflags[1] == 1, and in fact it produces 0.  As
   per Intel docs this bit has no meaning anyway.  Since PUSHF is the
   only way to observe eflags[1], a proper fix would be to make that
   bit be set by PUSHF.

   The state of %eflags.AC (alignment check, bit 18) is recorded by
   the simulation (viz, if you set it with popf then a pushf produces
   the value you set it to), but it is otherwise ignored.  In
   particular, setting it to 1 does NOT cause alignment checking to
   happen.  Programs that set it to 1 and then rely on the resulting
   SIGBUSs to inform them of misaligned accesses will not work.

   Implementation of sysenter is necessarily partial.  sysenter is a
   kind of system call entry.  When doing a sysenter, the return
   address is not known -- that is something that is beyond Vex's
   knowledge.  So the generated IR forces a return to the scheduler,
   which can do what it likes to simulate the systenter, but it MUST
   set this thread's guest_EIP field with the continuation address
   before resuming execution.  If that doesn't happen, the thread will
   jump to address zero, which is probably fatal.

   This module uses global variables and so is not MT-safe (if that
   should ever become relevant).

   The delta values are 32-bit ints, not 64-bit ints.  That means
   this module may not work right if run on a 64-bit host.  That should
   be fixed properly, really -- if anyone ever wants to use Vex to
   translate x86 code for execution on a 64-bit host.

   casLE (implementation of lock-prefixed insns) and rep-prefixed
   insns: the side-exit back to the start of the insn is done with
   Ijk_Boring.  This is quite wrong, it should be done with
   Ijk_NoRedir, since otherwise the side exit, which is intended to
   restart the instruction for whatever reason, could go somewhere
   entirely else.  Doing it right (with Ijk_NoRedir jumps) would make
   no-redir jumps performance critical, at least for rep-prefixed
   instructions, since all iterations thereof would involve such a
   jump.  It's not such a big deal with casLE since the side exit is
   only taken if the CAS fails, that is, the location is contended,
   which is relatively unlikely.

   XXXX: Nov 2009: handling of SWP on ARM suffers from the same
   problem.

   Note also, the test for CAS success vs failure is done using
   Iop_CasCmp{EQ,NE}{8,16,32,64} rather than the ordinary
   Iop_Cmp{EQ,NE} equivalents.  This is so as to tell Memcheck that it
   shouldn't definedness-check these comparisons.  See
   COMMENT_ON_CasCmpEQ in memcheck/mc_translate.c for
   background/rationale.
*/

/* Performance holes:

   - fcom ; fstsw %ax ; sahf
     sahf does not update the O flag (sigh) and so O needs to
     be computed.  This is done expensively; it would be better
     to have a calculate_eflags_o helper.

   - emwarns; some FP codes can generate huge numbers of these
     if the fpucw is changed in an inner loop.  It would be
     better for the guest state to have an emwarn-enable reg
     which can be set zero or nonzero.  If it is zero, emwarns
     are not flagged, and instead control just flows all the
     way through bbs as usual.
*/

/* "Special" instructions.

   This instruction decoder can decode three special instructions
   which mean nothing natively (are no-ops as far as regs/mem are
   concerned) but have meaning for supporting Valgrind.  A special
   instruction is flagged by the 12-byte preamble C1C703 C1C70D C1C71D
   C1C713 (in the standard interpretation, that means: roll $3, %edi;
   roll $13, %edi; roll $29, %edi; roll $19, %edi).  Following that,
   one of the following 3 are allowed (standard interpretation in
   parentheses):

      87DB (xchgl %ebx,%ebx)   %EDX = client_request ( %EAX )
      87C9 (xchgl %ecx,%ecx)   %EAX = guest_NRADDR
      87D2 (xchgl %edx,%edx)   call-noredir *%EAX
      87FF (xchgl %edi,%edi)   IR injection

   Any other bytes following the 12-byte preamble are illegal and
   constitute a failure in instruction decoding.  This all assumes
   that the preamble will never occur except in specific code
   fragments designed for Valgrind to catch.

   No prefixes may precede a "Special" instruction.
*/

/* LOCK prefixed instructions.  These are translated using IR-level
   CAS statements (IRCAS) and are believed to preserve atomicity, even
   from the point of view of some other process racing against a
   simulated one (presumably they communicate via a shared memory
   segment).

   Handlers which are aware of LOCK prefixes are:
      dis_op2_G_E      (add, or, adc, sbb, and, sub, xor)
      dis_cmpxchg_G_E  (cmpxchg)
      dis_Grp1         (add, or, adc, sbb, and, sub, xor)
      dis_Grp3         (not, neg)
      dis_Grp4         (inc, dec)
      dis_Grp5         (inc, dec)
      dis_Grp8_Imm     (bts, btc, btr)
      dis_bt_G_E       (bts, btc, btr)
      dis_xadd_G_E     (xadd)
*/


#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_x86.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_generic_x87.h"
#include "guest_x86_defs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of an insn, right
   down in disInstr_X86, so that we don't have to pass them around
   endlessly.  They are all constant during the translation of any
   given insn. */

/* We need to know this to do sub-register accesses correctly. */
static Bool host_is_bigendian;

/* Pointer to the guest code area (points to start of BB, not to the
   insn being processed). */
static UChar* guest_code;

/* The guest address corresponding to guest_code[0]. */
static Addr32 guest_EIP_bbstart;

/* The guest address for the instruction currently being
   translated. */
static Addr32 guest_EIP_curr_instr;

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
/*--- Offsets of various parts of the x86 guest state.     ---*/
/*------------------------------------------------------------*/

#define OFFB_EAX       offsetof(VexGuestX86State,guest_EAX)
#define OFFB_EBX       offsetof(VexGuestX86State,guest_EBX)
#define OFFB_ECX       offsetof(VexGuestX86State,guest_ECX)
#define OFFB_EDX       offsetof(VexGuestX86State,guest_EDX)
#define OFFB_ESP       offsetof(VexGuestX86State,guest_ESP)
#define OFFB_EBP       offsetof(VexGuestX86State,guest_EBP)
#define OFFB_ESI       offsetof(VexGuestX86State,guest_ESI)
#define OFFB_EDI       offsetof(VexGuestX86State,guest_EDI)

#define OFFB_EIP       offsetof(VexGuestX86State,guest_EIP)

#define OFFB_CC_OP     offsetof(VexGuestX86State,guest_CC_OP)
#define OFFB_CC_DEP1   offsetof(VexGuestX86State,guest_CC_DEP1)
#define OFFB_CC_DEP2   offsetof(VexGuestX86State,guest_CC_DEP2)
#define OFFB_CC_NDEP   offsetof(VexGuestX86State,guest_CC_NDEP)

#define OFFB_FPREGS    offsetof(VexGuestX86State,guest_FPREG[0])
#define OFFB_FPTAGS    offsetof(VexGuestX86State,guest_FPTAG[0])
#define OFFB_DFLAG     offsetof(VexGuestX86State,guest_DFLAG)
#define OFFB_IDFLAG    offsetof(VexGuestX86State,guest_IDFLAG)
#define OFFB_ACFLAG    offsetof(VexGuestX86State,guest_ACFLAG)
#define OFFB_FTOP      offsetof(VexGuestX86State,guest_FTOP)
#define OFFB_FC3210    offsetof(VexGuestX86State,guest_FC3210)
#define OFFB_FPROUND   offsetof(VexGuestX86State,guest_FPROUND)

#define OFFB_CS        offsetof(VexGuestX86State,guest_CS)
#define OFFB_DS        offsetof(VexGuestX86State,guest_DS)
#define OFFB_ES        offsetof(VexGuestX86State,guest_ES)
#define OFFB_FS        offsetof(VexGuestX86State,guest_FS)
#define OFFB_GS        offsetof(VexGuestX86State,guest_GS)
#define OFFB_SS        offsetof(VexGuestX86State,guest_SS)
#define OFFB_LDT       offsetof(VexGuestX86State,guest_LDT)
#define OFFB_GDT       offsetof(VexGuestX86State,guest_GDT)

#define OFFB_SSEROUND  offsetof(VexGuestX86State,guest_SSEROUND)
#define OFFB_XMM0      offsetof(VexGuestX86State,guest_XMM0)
#define OFFB_XMM1      offsetof(VexGuestX86State,guest_XMM1)
#define OFFB_XMM2      offsetof(VexGuestX86State,guest_XMM2)
#define OFFB_XMM3      offsetof(VexGuestX86State,guest_XMM3)
#define OFFB_XMM4      offsetof(VexGuestX86State,guest_XMM4)
#define OFFB_XMM5      offsetof(VexGuestX86State,guest_XMM5)
#define OFFB_XMM6      offsetof(VexGuestX86State,guest_XMM6)
#define OFFB_XMM7      offsetof(VexGuestX86State,guest_XMM7)

#define OFFB_EMNOTE    offsetof(VexGuestX86State,guest_EMNOTE)

#define OFFB_CMSTART   offsetof(VexGuestX86State,guest_CMSTART)
#define OFFB_CMLEN     offsetof(VexGuestX86State,guest_CMLEN)
#define OFFB_NRADDR    offsetof(VexGuestX86State,guest_NRADDR)

#define OFFB_IP_AT_SYSCALL offsetof(VexGuestX86State,guest_IP_AT_SYSCALL)


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- x86 insn stream.                                     ---*/
/*------------------------------------------------------------*/

/* This is the Intel register encoding -- integer regs. */
#define R_EAX 0
#define R_ECX 1
#define R_EDX 2
#define R_EBX 3
#define R_ESP 4
#define R_EBP 5
#define R_ESI 6
#define R_EDI 7

#define R_AL (0+R_EAX)
#define R_AH (4+R_EAX)

/* This is the Intel register encoding -- segment regs. */
#define R_ES 0
#define R_CS 1
#define R_SS 2
#define R_DS 3
#define R_FS 4
#define R_GS 5


/* Add a statement to the list held by "irbb". */
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
static UChar getIByte ( Int delta )
{
   return guest_code[delta];
}

/* Extract the reg field from a modRM byte. */
static Int gregOfRM ( UChar mod_reg_rm )
{
   return (Int)( (mod_reg_rm >> 3) & 7 );
}

/* Figure out whether the mod and rm parts of a modRM byte refer to a
   register or memory.  If so, the byte will have the form 11XXXYYY,
   where YYY is the register number. */
static Bool epartIsReg ( UChar mod_reg_rm )
{
   return toBool(0xC0 == (mod_reg_rm & 0xC0));
}

/* ... and extract the register number ... */
static Int eregOfRM ( UChar mod_reg_rm )
{
   return (Int)(mod_reg_rm & 0x7);
}

/* Get a 8/16/32-bit unsigned value out of the insn stream. */

static UChar getUChar ( Int delta )
{
   UChar v = guest_code[delta+0];
   return toUChar(v);
}

static UInt getUDisp16 ( Int delta )
{
   UInt v = guest_code[delta+1]; v <<= 8;
   v |= guest_code[delta+0];
   return v & 0xFFFF;
}

static UInt getUDisp32 ( Int delta )
{
   UInt v = guest_code[delta+3]; v <<= 8;
   v |= guest_code[delta+2]; v <<= 8;
   v |= guest_code[delta+1]; v <<= 8;
   v |= guest_code[delta+0];
   return v;
}

static UInt getUDisp ( Int size, Int delta )
{
   switch (size) {
      case 4: return getUDisp32(delta);
      case 2: return getUDisp16(delta);
      case 1: return (UInt)getUChar(delta);
      default: vpanic("getUDisp(x86)");
   }
   return 0; /*notreached*/
}


/* Get a byte value out of the insn stream and sign-extend to 32
   bits. */
static UInt getSDisp8 ( Int delta )
{
   return extend_s_8to32( (UInt) (guest_code[delta]) );
}

static UInt getSDisp16 ( Int delta0 )
{
   UChar* eip = (UChar*)(&guest_code[delta0]);
   UInt d = *eip++;
   d |= ((*eip++) << 8);
   return extend_s_16to32(d);
}

static UInt getSDisp ( Int size, Int delta )
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

/* On a little-endian host, less significant bits of the guest
   registers are at lower addresses.  Therefore, if a reference to a
   register low half has the safe guest state offset as a reference to
   the full register.
*/
static Int integerGuestRegOffset ( Int sz, UInt archreg )
{
   vassert(archreg < 8);

   /* Correct for little-endian host only. */
   vassert(!host_is_bigendian);

   if (sz == 4 || sz == 2 || (sz == 1 && archreg < 4)) {
      switch (archreg) {
         case R_EAX: return OFFB_EAX;
         case R_EBX: return OFFB_EBX;
         case R_ECX: return OFFB_ECX;
         case R_EDX: return OFFB_EDX;
         case R_ESI: return OFFB_ESI;
         case R_EDI: return OFFB_EDI;
         case R_ESP: return OFFB_ESP;
         case R_EBP: return OFFB_EBP;
         default: vpanic("integerGuestRegOffset(x86,le)(4,2)");
      }
   }

   vassert(archreg >= 4 && archreg < 8 && sz == 1);
   switch (archreg-4) {
      case R_EAX: return 1+ OFFB_EAX;
      case R_EBX: return 1+ OFFB_EBX;
      case R_ECX: return 1+ OFFB_ECX;
      case R_EDX: return 1+ OFFB_EDX;
      default: vpanic("integerGuestRegOffset(x86,le)(1h)");
   }

   /* NOTREACHED */
   vpanic("integerGuestRegOffset(x86,le)");
}

static Int segmentGuestRegOffset ( UInt sreg )
{
   switch (sreg) {
      case R_ES: return OFFB_ES;
      case R_CS: return OFFB_CS;
      case R_SS: return OFFB_SS;
      case R_DS: return OFFB_DS;
      case R_FS: return OFFB_FS;
      case R_GS: return OFFB_GS;
      default: vpanic("segmentGuestRegOffset(x86)");
   }
}

static Int xmmGuestRegOffset ( UInt xmmreg )
{
   switch (xmmreg) {
      case 0: return OFFB_XMM0;
      case 1: return OFFB_XMM1;
      case 2: return OFFB_XMM2;
      case 3: return OFFB_XMM3;
      case 4: return OFFB_XMM4;
      case 5: return OFFB_XMM5;
      case 6: return OFFB_XMM6;
      case 7: return OFFB_XMM7;
      default: vpanic("xmmGuestRegOffset");
   }
}

/* Lanes of vector registers are always numbered from zero being the
   least significant lane (rightmost in the register).  */

static Int xmmGuestRegLane16offset ( UInt xmmreg, Int laneno )
{
   /* Correct for little-endian host only. */
   vassert(!host_is_bigendian);
   vassert(laneno >= 0 && laneno < 8);
   return xmmGuestRegOffset( xmmreg ) + 2 * laneno;
}

static Int xmmGuestRegLane32offset ( UInt xmmreg, Int laneno )
{
   /* Correct for little-endian host only. */
   vassert(!host_is_bigendian);
   vassert(laneno >= 0 && laneno < 4);
   return xmmGuestRegOffset( xmmreg ) + 4 * laneno;
}

static Int xmmGuestRegLane64offset ( UInt xmmreg, Int laneno )
{
   /* Correct for little-endian host only. */
   vassert(!host_is_bigendian);
   vassert(laneno >= 0 && laneno < 2);
   return xmmGuestRegOffset( xmmreg ) + 8 * laneno;
}

static IRExpr* getIReg ( Int sz, UInt archreg )
{
   vassert(sz == 1 || sz == 2 || sz == 4);
   vassert(archreg < 8);
   return IRExpr_Get( integerGuestRegOffset(sz,archreg),
                      szToITy(sz) );
}

/* Ditto, but write to a reg instead. */
static void putIReg ( Int sz, UInt archreg, IRExpr* e )
{
   IRType ty = typeOfIRExpr(irsb->tyenv, e);
   switch (sz) {
      case 1: vassert(ty == Ity_I8); break;
      case 2: vassert(ty == Ity_I16); break;
      case 4: vassert(ty == Ity_I32); break;
      default: vpanic("putIReg(x86)");
   }
   vassert(archreg < 8);
   stmt( IRStmt_Put(integerGuestRegOffset(sz,archreg), e) );
}

static IRExpr* getSReg ( UInt sreg )
{
   return IRExpr_Get( segmentGuestRegOffset(sreg), Ity_I16 );
}

static void putSReg ( UInt sreg, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_I16);
   stmt( IRStmt_Put( segmentGuestRegOffset(sreg), e ) );
}

static IRExpr* getXMMReg ( UInt xmmreg )
{
   return IRExpr_Get( xmmGuestRegOffset(xmmreg), Ity_V128 );
}

static IRExpr* getXMMRegLane64 ( UInt xmmreg, Int laneno )
{
   return IRExpr_Get( xmmGuestRegLane64offset(xmmreg,laneno), Ity_I64 );
}

static IRExpr* getXMMRegLane64F ( UInt xmmreg, Int laneno )
{
   return IRExpr_Get( xmmGuestRegLane64offset(xmmreg,laneno), Ity_F64 );
}

static IRExpr* getXMMRegLane32 ( UInt xmmreg, Int laneno )
{
   return IRExpr_Get( xmmGuestRegLane32offset(xmmreg,laneno), Ity_I32 );
}

static IRExpr* getXMMRegLane32F ( UInt xmmreg, Int laneno )
{
   return IRExpr_Get( xmmGuestRegLane32offset(xmmreg,laneno), Ity_F32 );
}

static void putXMMReg ( UInt xmmreg, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_V128);
   stmt( IRStmt_Put( xmmGuestRegOffset(xmmreg), e ) );
}

static void putXMMRegLane64 ( UInt xmmreg, Int laneno, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_I64);
   stmt( IRStmt_Put( xmmGuestRegLane64offset(xmmreg,laneno), e ) );
}

static void putXMMRegLane64F ( UInt xmmreg, Int laneno, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_F64);
   stmt( IRStmt_Put( xmmGuestRegLane64offset(xmmreg,laneno), e ) );
}

static void putXMMRegLane32F ( UInt xmmreg, Int laneno, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_F32);
   stmt( IRStmt_Put( xmmGuestRegLane32offset(xmmreg,laneno), e ) );
}

static void putXMMRegLane32 ( UInt xmmreg, Int laneno, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_I32);
   stmt( IRStmt_Put( xmmGuestRegLane32offset(xmmreg,laneno), e ) );
}

static void putXMMRegLane16 ( UInt xmmreg, Int laneno, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_I16);
   stmt( IRStmt_Put( xmmGuestRegLane16offset(xmmreg,laneno), e ) );
}

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_WrTmp(dst, e) );
}

static void storeLE ( IRExpr* addr, IRExpr* data )
{
   stmt( IRStmt_Store(Iend_LE, addr, data) );
}

static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static IRExpr* triop ( IROp op, IRExpr* a1, IRExpr* a2, IRExpr* a3 )
{
   return IRExpr_Triop(op, a1, a2, a3);
}

static IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_RdTmp(tmp);
}

static IRExpr* mkU8 ( UInt i )
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8( (UChar)i ));
}

static IRExpr* mkU16 ( UInt i )
{
   vassert(i < 65536);
   return IRExpr_Const(IRConst_U16( (UShort)i ));
}

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* mkU64 ( ULong i )
{
   return IRExpr_Const(IRConst_U64(i));
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

static IRExpr* mkV128 ( UShort mask )
{
   return IRExpr_Const(IRConst_V128(mask));
}

static IRExpr* loadLE ( IRType ty, IRExpr* addr )
{
   return IRExpr_Load(Iend_LE, ty, addr);
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
           || op8 == Iop_CasCmpNE8
           || op8 == Iop_ExpCmpNE8
           || op8 == Iop_Not8);
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

static IRExpr* mkAnd1 ( IRExpr* x, IRExpr* y )
{
   vassert(typeOfIRExpr(irsb->tyenv,x) == Ity_I1);
   vassert(typeOfIRExpr(irsb->tyenv,y) == Ity_I1);
   return unop(Iop_32to1, 
               binop(Iop_And32, 
                     unop(Iop_1Uto32,x), 
                     unop(Iop_1Uto32,y)));
}

/* Generate a compare-and-swap operation, operating on memory at
   'addr'.  The expected value is 'expVal' and the new value is
   'newVal'.  If the operation fails, then transfer control (with a
   no-redir jump (XXX no -- see comment at top of this file)) to
   'restart_point', which is presumably the address of the guest
   instruction again -- retrying, essentially. */
static void casLE ( IRExpr* addr, IRExpr* expVal, IRExpr* newVal,
                    Addr32 restart_point )
{
   IRCAS* cas;
   IRType tyE    = typeOfIRExpr(irsb->tyenv, expVal);
   IRType tyN    = typeOfIRExpr(irsb->tyenv, newVal);
   IRTemp oldTmp = newTemp(tyE);
   IRTemp expTmp = newTemp(tyE);
   vassert(tyE == tyN);
   vassert(tyE == Ity_I32 || tyE == Ity_I16 || tyE == Ity_I8);
   assign(expTmp, expVal);
   cas = mkIRCAS( IRTemp_INVALID, oldTmp, Iend_LE, addr, 
                  NULL, mkexpr(expTmp), NULL, newVal );
   stmt( IRStmt_CAS(cas) );
   stmt( IRStmt_Exit(
            binop( mkSizedOp(tyE,Iop_CasCmpNE8),
                   mkexpr(oldTmp), mkexpr(expTmp) ),
            Ijk_Boring, /*Ijk_NoRedir*/
            IRConst_U32( restart_point ),
            OFFB_EIP
         ));
}


/*------------------------------------------------------------*/
/*--- Helpers for %eflags.                                 ---*/
/*------------------------------------------------------------*/

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
           3/*regparm*/, 
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
   return toBool(op8 == Iop_Add8 || op8 == Iop_Sub8);
}

static Bool isLogic ( IROp op8 )
{
   return toBool(op8 == Iop_And8 || op8 == Iop_Or8 || op8 == Iop_Xor8);
}

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
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
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
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
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

   /* guard :: Ity_I8.  We need to convert it to I1. */
   IRTemp guardB = newTemp(Ity_I1);
   assign( guardB, binop(Iop_CmpNE8, mkexpr(guard), mkU8(0)) );

   /* DEP1 contains the result, DEP2 contains the undershifted value. */
   stmt( IRStmt_Put( OFFB_CC_OP,
                     IRExpr_ITE( mkexpr(guardB),
                                 mkU32(ccOp),
                                 IRExpr_Get(OFFB_CC_OP,Ity_I32) ) ));
   stmt( IRStmt_Put( OFFB_CC_DEP1,
                     IRExpr_ITE( mkexpr(guardB),
                                 widenUto32(mkexpr(res)),
                                 IRExpr_Get(OFFB_CC_DEP1,Ity_I32) ) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, 
                     IRExpr_ITE( mkexpr(guardB),
                                 widenUto32(mkexpr(resUS)),
                                 IRExpr_Get(OFFB_CC_DEP2,Ity_I32) ) ));
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP,
                     IRExpr_ITE( mkexpr(guardB),
                                 mkU32(0),
                                 IRExpr_Get(OFFB_CC_NDEP,Ity_I32) ) ));
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
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(res))) );
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
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
}


/* -------------- Condition codes. -------------- */

/* Condition codes, using the Intel encoding.  */

static const HChar* name_X86Condcode ( X86Condcode cond )
{
   switch (cond) {
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
      case X86CondAlways: return "ALWAYS";
      default: vpanic("name_X86Condcode");
   }
}

static 
X86Condcode positiveIse_X86Condcode ( X86Condcode  cond,
                                      Bool*        needInvert )
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


/* -------------- Helpers for ADD/SUB with carry. -------------- */

/* Given ta1, ta2 and tres, compute tres = ADC(ta1,ta2) and set flags
   appropriately.

   Optionally, generate a store for the 'tres' value.  This can either
   be a normal store, or it can be a cas-with-possible-failure style
   store:

   if taddr is IRTemp_INVALID, then no store is generated.

   if taddr is not IRTemp_INVALID, then a store (using taddr as
   the address) is generated:

     if texpVal is IRTemp_INVALID then a normal store is
     generated, and restart_point must be zero (it is irrelevant).

     if texpVal is not IRTemp_INVALID then a cas-style store is
     generated.  texpVal is the expected value, restart_point
     is the restart point if the store fails, and texpVal must
     have the same type as tres.   
*/
static void helper_ADC ( Int sz,
                         IRTemp tres, IRTemp ta1, IRTemp ta2,
                         /* info about optional store: */
                         IRTemp taddr, IRTemp texpVal, Addr32 restart_point )
{
   UInt    thunkOp;
   IRType  ty    = szToITy(sz);
   IRTemp  oldc  = newTemp(Ity_I32);
   IRTemp  oldcn = newTemp(ty);
   IROp    plus  = mkSizedOp(ty, Iop_Add8);
   IROp    xor   = mkSizedOp(ty, Iop_Xor8);

   vassert(typeOfIRTemp(irsb->tyenv, tres) == ty);
   vassert(sz == 1 || sz == 2 || sz == 4);
   thunkOp = sz==4 ? X86G_CC_OP_ADCL 
                   : (sz==2 ? X86G_CC_OP_ADCW : X86G_CC_OP_ADCB);

   /* oldc = old carry flag, 0 or 1 */
   assign( oldc,  binop(Iop_And32,
                        mk_x86g_calculate_eflags_c(),
                        mkU32(1)) );

   assign( oldcn, narrowTo(ty, mkexpr(oldc)) );

   assign( tres, binop(plus,
                       binop(plus,mkexpr(ta1),mkexpr(ta2)),
                       mkexpr(oldcn)) );

   /* Possibly generate a store of 'tres' to 'taddr'.  See comment at
      start of this function. */
   if (taddr != IRTemp_INVALID) {
      if (texpVal == IRTemp_INVALID) {
         vassert(restart_point == 0);
         storeLE( mkexpr(taddr), mkexpr(tres) );
      } else {
         vassert(typeOfIRTemp(irsb->tyenv, texpVal) == ty);
         /* .. and hence 'texpVal' has the same type as 'tres'. */
         casLE( mkexpr(taddr),
                mkexpr(texpVal), mkexpr(tres), restart_point );
      }
   }

   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(thunkOp) ) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(ta1)) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(binop(xor, mkexpr(ta2), 
                                                         mkexpr(oldcn)) )) );
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkexpr(oldc) ) );
}


/* Given ta1, ta2 and tres, compute tres = SBB(ta1,ta2) and set flags
   appropriately.  As with helper_ADC, possibly generate a store of
   the result -- see comments on helper_ADC for details.
*/
static void helper_SBB ( Int sz,
                         IRTemp tres, IRTemp ta1, IRTemp ta2,
                         /* info about optional store: */
                         IRTemp taddr, IRTemp texpVal, Addr32 restart_point )
{
   UInt    thunkOp;
   IRType  ty    = szToITy(sz);
   IRTemp  oldc  = newTemp(Ity_I32);
   IRTemp  oldcn = newTemp(ty);
   IROp    minus = mkSizedOp(ty, Iop_Sub8);
   IROp    xor   = mkSizedOp(ty, Iop_Xor8);

   vassert(typeOfIRTemp(irsb->tyenv, tres) == ty);
   vassert(sz == 1 || sz == 2 || sz == 4);
   thunkOp = sz==4 ? X86G_CC_OP_SBBL 
                   : (sz==2 ? X86G_CC_OP_SBBW : X86G_CC_OP_SBBB);

   /* oldc = old carry flag, 0 or 1 */
   assign( oldc, binop(Iop_And32,
                       mk_x86g_calculate_eflags_c(),
                       mkU32(1)) );

   assign( oldcn, narrowTo(ty, mkexpr(oldc)) );

   assign( tres, binop(minus,
                       binop(minus,mkexpr(ta1),mkexpr(ta2)),
                       mkexpr(oldcn)) );

   /* Possibly generate a store of 'tres' to 'taddr'.  See comment at
      start of this function. */
   if (taddr != IRTemp_INVALID) {
      if (texpVal == IRTemp_INVALID) {
         vassert(restart_point == 0);
         storeLE( mkexpr(taddr), mkexpr(tres) );
      } else {
         vassert(typeOfIRTemp(irsb->tyenv, texpVal) == ty);
         /* .. and hence 'texpVal' has the same type as 'tres'. */
         casLE( mkexpr(taddr),
                mkexpr(texpVal), mkexpr(tres), restart_point );
      }
   }

   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(thunkOp) ) );
   stmt( IRStmt_Put( OFFB_CC_DEP1, widenUto32(mkexpr(ta1) )) );
   stmt( IRStmt_Put( OFFB_CC_DEP2, widenUto32(binop(xor, mkexpr(ta2), 
                                                         mkexpr(oldcn)) )) );
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkexpr(oldc) ) );
}


/* -------------- Helpers for disassembly printing. -------------- */

static const HChar* nameGrp1 ( Int opc_aux )
{
   static const HChar* grp1_names[8] 
     = { "add", "or", "adc", "sbb", "and", "sub", "xor", "cmp" };
   if (opc_aux < 0 || opc_aux > 7) vpanic("nameGrp1(x86)");
   return grp1_names[opc_aux];
}

static const HChar* nameGrp2 ( Int opc_aux )
{
   static const HChar* grp2_names[8] 
     = { "rol", "ror", "rcl", "rcr", "shl", "shr", "shl", "sar" };
   if (opc_aux < 0 || opc_aux > 7) vpanic("nameGrp2(x86)");
   return grp2_names[opc_aux];
}

static const HChar* nameGrp4 ( Int opc_aux )
{
   static const HChar* grp4_names[8] 
     = { "inc", "dec", "???", "???", "???", "???", "???", "???" };
   if (opc_aux < 0 || opc_aux > 1) vpanic("nameGrp4(x86)");
   return grp4_names[opc_aux];
}

static const HChar* nameGrp5 ( Int opc_aux )
{
   static const HChar* grp5_names[8] 
     = { "inc", "dec", "call*", "call*", "jmp*", "jmp*", "push", "???" };
   if (opc_aux < 0 || opc_aux > 6) vpanic("nameGrp5(x86)");
   return grp5_names[opc_aux];
}

static const HChar* nameGrp8 ( Int opc_aux )
{
   static const HChar* grp8_names[8] 
     = { "???", "???", "???", "???", "bt", "bts", "btr", "btc" };
   if (opc_aux < 4 || opc_aux > 7) vpanic("nameGrp8(x86)");
   return grp8_names[opc_aux];
}

static const HChar* nameIReg ( Int size, Int reg )
{
   static const HChar* ireg32_names[8] 
     = { "%eax", "%ecx", "%edx", "%ebx", 
         "%esp", "%ebp", "%esi", "%edi" };
   static const HChar* ireg16_names[8] 
     = { "%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di" };
   static const HChar* ireg8_names[8] 
     = { "%al", "%cl", "%dl", "%bl", 
         "%ah{sp}", "%ch{bp}", "%dh{si}", "%bh{di}" };
   if (reg < 0 || reg > 7) goto bad;
   switch (size) {
      case 4: return ireg32_names[reg];
      case 2: return ireg16_names[reg];
      case 1: return ireg8_names[reg];
   }
  bad:
   vpanic("nameIReg(X86)");
   return NULL; /*notreached*/
}

static const HChar* nameSReg ( UInt sreg )
{
   switch (sreg) {
      case R_ES: return "%es";
      case R_CS: return "%cs";
      case R_SS: return "%ss";
      case R_DS: return "%ds";
      case R_FS: return "%fs";
      case R_GS: return "%gs";
      default: vpanic("nameSReg(x86)");
   }
}

static const HChar* nameMMXReg ( Int mmxreg )
{
   static const HChar* mmx_names[8] 
     = { "%mm0", "%mm1", "%mm2", "%mm3", "%mm4", "%mm5", "%mm6", "%mm7" };
   if (mmxreg < 0 || mmxreg > 7) vpanic("nameMMXReg(x86,guest)");
   return mmx_names[mmxreg];
}

static const HChar* nameXMMReg ( Int xmmreg )
{
   static const HChar* xmm_names[8] 
     = { "%xmm0", "%xmm1", "%xmm2", "%xmm3", 
         "%xmm4", "%xmm5", "%xmm6", "%xmm7" };
   if (xmmreg < 0 || xmmreg > 7) vpanic("name_of_xmm_reg");
   return xmm_names[xmmreg];
}
 
static const HChar* nameMMXGran ( Int gran )
{
   switch (gran) {
      case 0: return "b";
      case 1: return "w";
      case 2: return "d";
      case 3: return "q";
      default: vpanic("nameMMXGran(x86,guest)");
   }
}

static HChar nameISize ( Int size )
{
   switch (size) {
      case 4: return 'l';
      case 2: return 'w';
      case 1: return 'b';
      default: vpanic("nameISize(x86)");
   }
}


/*------------------------------------------------------------*/
/*--- JMP helpers                                          ---*/
/*------------------------------------------------------------*/

static void jmp_lit( /*MOD*/DisResult* dres,
                     IRJumpKind kind, Addr32 d32 )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->continueAt  == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt( IRStmt_Put( OFFB_EIP, mkU32(d32) ) );
}

static void jmp_treg( /*MOD*/DisResult* dres,
                      IRJumpKind kind, IRTemp t )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->continueAt  == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt( IRStmt_Put( OFFB_EIP, mkexpr(t) ) );
}

static 
void jcc_01( /*MOD*/DisResult* dres,
             X86Condcode cond, Addr32 d32_false, Addr32 d32_true )
{
   Bool        invert;
   X86Condcode condPos;
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->continueAt  == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = Ijk_Boring;
   condPos = positiveIse_X86Condcode ( cond, &invert );
   if (invert) {
      stmt( IRStmt_Exit( mk_x86g_calculate_condition(condPos),
                         Ijk_Boring,
                         IRConst_U32(d32_false),
                         OFFB_EIP ) );
      stmt( IRStmt_Put( OFFB_EIP, mkU32(d32_true) ) );
   } else {
      stmt( IRStmt_Exit( mk_x86g_calculate_condition(condPos),
                         Ijk_Boring,
                         IRConst_U32(d32_true),
                         OFFB_EIP ) );
      stmt( IRStmt_Put( OFFB_EIP, mkU32(d32_false) ) );
   }
}


/*------------------------------------------------------------*/
/*--- Disassembling addressing modes                       ---*/
/*------------------------------------------------------------*/

static 
const HChar* sorbTxt ( UChar sorb )
{
   switch (sorb) {
      case 0:    return ""; /* no override */
      case 0x3E: return "%ds";
      case 0x26: return "%es:";
      case 0x64: return "%fs:";
      case 0x65: return "%gs:";
      default: vpanic("sorbTxt(x86,guest)");
   }
}


/* 'virtual' is an IRExpr* holding a virtual address.  Convert it to a
   linear address by adding any required segment override as indicated
   by sorb. */
static
IRExpr* handleSegOverride ( UChar sorb, IRExpr* virtual )
{
   Int    sreg;
   IRType hWordTy;
   IRTemp ldt_ptr, gdt_ptr, seg_selector, r64;

   if (sorb == 0)
      /* the common case - no override */
      return virtual;

   switch (sorb) {
      case 0x3E: sreg = R_DS; break;
      case 0x26: sreg = R_ES; break;
      case 0x64: sreg = R_FS; break;
      case 0x65: sreg = R_GS; break;
      default: vpanic("handleSegOverride(x86,guest)");
   }

   hWordTy = sizeof(HWord)==4 ? Ity_I32 : Ity_I64;

   seg_selector = newTemp(Ity_I32);
   ldt_ptr      = newTemp(hWordTy);
   gdt_ptr      = newTemp(hWordTy);
   r64          = newTemp(Ity_I64);

   assign( seg_selector, unop(Iop_16Uto32, getSReg(sreg)) );
   assign( ldt_ptr, IRExpr_Get( OFFB_LDT, hWordTy ));
   assign( gdt_ptr, IRExpr_Get( OFFB_GDT, hWordTy ));

   /*
   Call this to do the translation and limit checks: 
   ULong x86g_use_seg_selector ( HWord ldt, HWord gdt,
                                 UInt seg_selector, UInt virtual_addr )
   */
   assign( 
      r64, 
      mkIRExprCCall( 
         Ity_I64, 
         0/*regparms*/, 
         "x86g_use_seg_selector", 
         &x86g_use_seg_selector, 
         mkIRExprVec_4( mkexpr(ldt_ptr), mkexpr(gdt_ptr), 
                        mkexpr(seg_selector), virtual)
      )
   );

   /* If the high 32 of the result are non-zero, there was a 
      failure in address translation.  In which case, make a
      quick exit.
   */
   stmt( 
      IRStmt_Exit(
         binop(Iop_CmpNE32, unop(Iop_64HIto32, mkexpr(r64)), mkU32(0)),
         Ijk_MapFail,
         IRConst_U32( guest_EIP_curr_instr ),
         OFFB_EIP
      )
   );

   /* otherwise, here's the translated result. */
   return unop(Iop_64to32, mkexpr(r64));
}


/* Generate IR to calculate an address indicated by a ModRM and
   following SIB bytes.  The expression, and the number of bytes in
   the address mode, are returned.  Note that this fn should not be
   called if the R/M part of the address denotes a register instead of
   memory.  If print_codegen is true, text of the addressing mode is
   placed in buf. 

   The computed address is stored in a new tempreg, and the
   identity of the tempreg is returned.  */

static IRTemp disAMode_copy2tmp ( IRExpr* addr32 )
{
   IRTemp tmp = newTemp(Ity_I32);
   assign( tmp, addr32 );
   return tmp;
}

static 
IRTemp disAMode ( Int* len, UChar sorb, Int delta, HChar* buf )
{
   UChar mod_reg_rm = getIByte(delta);
   delta++;

   buf[0] = (UChar)0;

   /* squeeze out the reg field from mod_reg_rm, since a 256-entry
      jump table seems a bit excessive. 
   */
   mod_reg_rm &= 0xC7;                      /* is now XX000YYY */
   mod_reg_rm  = toUChar(mod_reg_rm | (mod_reg_rm >> 3));  
                                            /* is now XX0XXYYY */
   mod_reg_rm &= 0x1F;                      /* is now 000XXYYY */
   switch (mod_reg_rm) {

      /* (%eax) .. (%edi), not including (%esp) or (%ebp).
         --> GET %reg, t 
      */
      case 0x00: case 0x01: case 0x02: case 0x03: 
      /* ! 04 */ /* ! 05 */ case 0x06: case 0x07:
         { UChar rm = mod_reg_rm;
           DIS(buf, "%s(%s)", sorbTxt(sorb), nameIReg(4,rm));
           *len = 1;
           return disAMode_copy2tmp(
                  handleSegOverride(sorb, getIReg(4,rm)));
         }

      /* d8(%eax) ... d8(%edi), not including d8(%esp) 
         --> GET %reg, t ; ADDL d8, t
      */
      case 0x08: case 0x09: case 0x0A: case 0x0B: 
      /* ! 0C */ case 0x0D: case 0x0E: case 0x0F:
         { UChar rm = toUChar(mod_reg_rm & 7);
           UInt  d  = getSDisp8(delta);
           DIS(buf, "%s%d(%s)", sorbTxt(sorb), (Int)d, nameIReg(4,rm));
           *len = 2;
           return disAMode_copy2tmp(
                  handleSegOverride(sorb,
                     binop(Iop_Add32,getIReg(4,rm),mkU32(d))));
         }

      /* d32(%eax) ... d32(%edi), not including d32(%esp)
         --> GET %reg, t ; ADDL d8, t
      */
      case 0x10: case 0x11: case 0x12: case 0x13: 
      /* ! 14 */ case 0x15: case 0x16: case 0x17:
         { UChar rm = toUChar(mod_reg_rm & 7);
           UInt  d  = getUDisp32(delta);
           DIS(buf, "%s0x%x(%s)", sorbTxt(sorb), (Int)d, nameIReg(4,rm));
           *len = 5;
           return disAMode_copy2tmp(
                  handleSegOverride(sorb,
                     binop(Iop_Add32,getIReg(4,rm),mkU32(d))));
         }

      /* a register, %eax .. %edi.  This shouldn't happen. */
      case 0x18: case 0x19: case 0x1A: case 0x1B:
      case 0x1C: case 0x1D: case 0x1E: case 0x1F:
         vpanic("disAMode(x86): not an addr!");

      /* a 32-bit literal address
         --> MOV d32, tmp 
      */
      case 0x05: 
         { UInt d = getUDisp32(delta);
           *len = 5;
           DIS(buf, "%s(0x%x)", sorbTxt(sorb), d);
           return disAMode_copy2tmp( 
                     handleSegOverride(sorb, mkU32(d)));
         }

      case 0x04: {
         /* SIB, with no displacement.  Special cases:
            -- %esp cannot act as an index value.  
               If index_r indicates %esp, zero is used for the index.
            -- when mod is zero and base indicates EBP, base is instead
               a 32-bit literal.
            It's all madness, I tell you.  Extract %index, %base and 
            scale from the SIB byte.  The value denoted is then:
               | %index == %ESP && %base == %EBP
               = d32 following SIB byte
               | %index == %ESP && %base != %EBP
               = %base
               | %index != %ESP && %base == %EBP
               = d32 following SIB byte + (%index << scale)
               | %index != %ESP && %base != %ESP
               = %base + (%index << scale)

            What happens to the souls of CPU architects who dream up such
            horrendous schemes, do you suppose?  
         */
         UChar sib     = getIByte(delta);
         UChar scale   = toUChar((sib >> 6) & 3);
         UChar index_r = toUChar((sib >> 3) & 7);
         UChar base_r  = toUChar(sib & 7);
         delta++;

         if (index_r != R_ESP && base_r != R_EBP) {
            DIS(buf, "%s(%s,%s,%d)", sorbTxt(sorb), 
                      nameIReg(4,base_r), nameIReg(4,index_r), 1<<scale);
            *len = 2;
            return
               disAMode_copy2tmp( 
               handleSegOverride(sorb,
                  binop(Iop_Add32, 
                        getIReg(4,base_r),
                        binop(Iop_Shl32, getIReg(4,index_r),
                              mkU8(scale)))));
         }

         if (index_r != R_ESP && base_r == R_EBP) {
            UInt d = getUDisp32(delta);
            DIS(buf, "%s0x%x(,%s,%d)", sorbTxt(sorb), d, 
                      nameIReg(4,index_r), 1<<scale);
            *len = 6;
            return
               disAMode_copy2tmp(
               handleSegOverride(sorb, 
                  binop(Iop_Add32,
                        binop(Iop_Shl32, getIReg(4,index_r), mkU8(scale)),
                        mkU32(d))));
         }

         if (index_r == R_ESP && base_r != R_EBP) {
            DIS(buf, "%s(%s,,)", sorbTxt(sorb), nameIReg(4,base_r));
            *len = 2;
            return disAMode_copy2tmp(
                   handleSegOverride(sorb, getIReg(4,base_r)));
         }

         if (index_r == R_ESP && base_r == R_EBP) {
            UInt d = getUDisp32(delta);
            DIS(buf, "%s0x%x(,,)", sorbTxt(sorb), d);
            *len = 6;
            return disAMode_copy2tmp(
                   handleSegOverride(sorb, mkU32(d)));
         }
         /*NOTREACHED*/
         vassert(0);
      }

      /* SIB, with 8-bit displacement.  Special cases:
         -- %esp cannot act as an index value.  
            If index_r indicates %esp, zero is used for the index.
         Denoted value is:
            | %index == %ESP
            = d8 + %base
            | %index != %ESP
            = d8 + %base + (%index << scale)
      */
      case 0x0C: {
         UChar sib     = getIByte(delta);
         UChar scale   = toUChar((sib >> 6) & 3);
         UChar index_r = toUChar((sib >> 3) & 7);
         UChar base_r  = toUChar(sib & 7);
         UInt  d       = getSDisp8(delta+1);

         if (index_r == R_ESP) {
            DIS(buf, "%s%d(%s,,)", sorbTxt(sorb), 
                                   (Int)d, nameIReg(4,base_r));
            *len = 3;
            return disAMode_copy2tmp(
                   handleSegOverride(sorb, 
                      binop(Iop_Add32, getIReg(4,base_r), mkU32(d)) ));
         } else {
            DIS(buf, "%s%d(%s,%s,%d)", sorbTxt(sorb), (Int)d, 
                     nameIReg(4,base_r), nameIReg(4,index_r), 1<<scale);
            *len = 3;
            return 
                disAMode_copy2tmp(
                handleSegOverride(sorb,
                  binop(Iop_Add32,
                        binop(Iop_Add32, 
                              getIReg(4,base_r), 
                              binop(Iop_Shl32, 
                                    getIReg(4,index_r), mkU8(scale))),
                        mkU32(d))));
         }
	 /*NOTREACHED*/
         vassert(0);
      }

      /* SIB, with 32-bit displacement.  Special cases:
         -- %esp cannot act as an index value.  
            If index_r indicates %esp, zero is used for the index.
         Denoted value is:
            | %index == %ESP
            = d32 + %base
            | %index != %ESP
            = d32 + %base + (%index << scale)
      */
      case 0x14: {
         UChar sib     = getIByte(delta);
         UChar scale   = toUChar((sib >> 6) & 3);
         UChar index_r = toUChar((sib >> 3) & 7);
         UChar base_r  = toUChar(sib & 7);
         UInt d        = getUDisp32(delta+1);

         if (index_r == R_ESP) {
            DIS(buf, "%s%d(%s,,)", sorbTxt(sorb), 
                                   (Int)d, nameIReg(4,base_r));
            *len = 6;
            return disAMode_copy2tmp(
                   handleSegOverride(sorb, 
                      binop(Iop_Add32, getIReg(4,base_r), mkU32(d)) ));
         } else {
            DIS(buf, "%s%d(%s,%s,%d)", sorbTxt(sorb), (Int)d, 
                     nameIReg(4,base_r), nameIReg(4,index_r), 1<<scale);
            *len = 6;
            return 
                disAMode_copy2tmp(
                handleSegOverride(sorb,
                  binop(Iop_Add32,
                        binop(Iop_Add32, 
                              getIReg(4,base_r), 
                              binop(Iop_Shl32, 
                                    getIReg(4,index_r), mkU8(scale))),
                        mkU32(d))));
         }
	 /*NOTREACHED*/
         vassert(0);
      }

      default:
         vpanic("disAMode(x86)");
         return 0; /*notreached*/
   }
}


/* Figure out the number of (insn-stream) bytes constituting the amode
   beginning at delta.  Is useful for getting hold of literals beyond
   the end of the amode before it has been disassembled.  */

static UInt lengthAMode ( Int delta )
{
   UChar mod_reg_rm = getIByte(delta); delta++;

   /* squeeze out the reg field from mod_reg_rm, since a 256-entry
      jump table seems a bit excessive. 
   */
   mod_reg_rm &= 0xC7;               /* is now XX000YYY */
   mod_reg_rm  = toUChar(mod_reg_rm | (mod_reg_rm >> 3));  
                                     /* is now XX0XXYYY */
   mod_reg_rm &= 0x1F;               /* is now 000XXYYY */
   switch (mod_reg_rm) {

      /* (%eax) .. (%edi), not including (%esp) or (%ebp). */
      case 0x00: case 0x01: case 0x02: case 0x03: 
      /* ! 04 */ /* ! 05 */ case 0x06: case 0x07:
         return 1;

      /* d8(%eax) ... d8(%edi), not including d8(%esp). */ 
      case 0x08: case 0x09: case 0x0A: case 0x0B: 
      /* ! 0C */ case 0x0D: case 0x0E: case 0x0F:
         return 2;

      /* d32(%eax) ... d32(%edi), not including d32(%esp). */
      case 0x10: case 0x11: case 0x12: case 0x13: 
      /* ! 14 */ case 0x15: case 0x16: case 0x17:
         return 5;

      /* a register, %eax .. %edi.  (Not an addr, but still handled.) */
      case 0x18: case 0x19: case 0x1A: case 0x1B:
      case 0x1C: case 0x1D: case 0x1E: case 0x1F:
         return 1;

      /* a 32-bit literal address. */
      case 0x05: return 5;

      /* SIB, no displacement.  */
      case 0x04: {
         UChar sib    = getIByte(delta);
         UChar base_r = toUChar(sib & 7);
         if (base_r == R_EBP) return 6; else return 2;
      }
      /* SIB, with 8-bit displacement.  */
      case 0x0C: return 3;

      /* SIB, with 32-bit displacement.  */
      case 0x14: return 6;

      default:
         vpanic("lengthAMode");
         return 0; /*notreached*/
   }
}

/*------------------------------------------------------------*/
/*--- Disassembling common idioms                          ---*/
/*------------------------------------------------------------*/

/* Handle binary integer instructions of the form
      op E, G  meaning
      op reg-or-mem, reg
   Is passed the a ptr to the modRM byte, the actual operation, and the
   data size.  Returns the address advanced completely over this
   instruction.

   E(src) is reg-or-mem
   G(dst) is reg.

   If E is reg, -->    GET %G,  tmp
                       OP %E,   tmp
                       PUT tmp, %G
 
   If E is mem and OP is not reversible, 
                -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpa
                       GET %G, tmp2
                       OP tmpa, tmp2
                       PUT tmp2, %G

   If E is mem and OP is reversible
                -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpa
                       OP %G, tmpa
                       PUT tmpa, %G
*/
static
UInt dis_op2_E_G ( UChar       sorb,
                   Bool        addSubCarry,
                   IROp        op8, 
                   Bool        keep,
                   Int         size, 
                   Int         delta0,
                   const HChar* t_x86opc )
{
   HChar   dis_buf[50];
   Int     len;
   IRType  ty   = szToITy(size);
   IRTemp  dst1 = newTemp(ty);
   IRTemp  src  = newTemp(ty);
   IRTemp  dst0 = newTemp(ty);
   UChar   rm   = getUChar(delta0);
   IRTemp  addr = IRTemp_INVALID;

   /* addSubCarry == True indicates the intended operation is
      add-with-carry or subtract-with-borrow. */
   if (addSubCarry) {
      vassert(op8 == Iop_Add8 || op8 == Iop_Sub8);
      vassert(keep);
   }

   if (epartIsReg(rm)) {
      /* Specially handle XOR reg,reg, because that doesn't really
         depend on reg, and doing the obvious thing potentially
         generates a spurious value check failure due to the bogus
         dependency.  Ditto SBB reg,reg. */
      if ((op8 == Iop_Xor8 || (op8 == Iop_Sub8 && addSubCarry))
          && gregOfRM(rm) == eregOfRM(rm)) {
         putIReg(size, gregOfRM(rm), mkU(ty,0));
      }
      assign( dst0, getIReg(size,gregOfRM(rm)) );
      assign( src,  getIReg(size,eregOfRM(rm)) );

      if (addSubCarry && op8 == Iop_Add8) {
         helper_ADC( size, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
         putIReg(size, gregOfRM(rm), mkexpr(dst1));
      } else
      if (addSubCarry && op8 == Iop_Sub8) {
         helper_SBB( size, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
         putIReg(size, gregOfRM(rm), mkexpr(dst1));
      } else {
         assign( dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)) );
         if (isAddSub(op8))
            setFlags_DEP1_DEP2(op8, dst0, src, ty);
         else
            setFlags_DEP1(op8, dst1, ty);
         if (keep)
            putIReg(size, gregOfRM(rm), mkexpr(dst1));
      }

      DIP("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                          nameIReg(size,eregOfRM(rm)),
                          nameIReg(size,gregOfRM(rm)));
      return 1+delta0;
   } else {
      /* E refers to memory */
      addr = disAMode ( &len, sorb, delta0, dis_buf);
      assign( dst0, getIReg(size,gregOfRM(rm)) );
      assign( src,  loadLE(szToITy(size), mkexpr(addr)) );

      if (addSubCarry && op8 == Iop_Add8) {
         helper_ADC( size, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
         putIReg(size, gregOfRM(rm), mkexpr(dst1));
      } else
      if (addSubCarry && op8 == Iop_Sub8) {
         helper_SBB( size, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
         putIReg(size, gregOfRM(rm), mkexpr(dst1));
      } else {
         assign( dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)) );
         if (isAddSub(op8))
            setFlags_DEP1_DEP2(op8, dst0, src, ty);
         else
            setFlags_DEP1(op8, dst1, ty);
         if (keep)
            putIReg(size, gregOfRM(rm), mkexpr(dst1));
      }

      DIP("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                          dis_buf,nameIReg(size,gregOfRM(rm)));
      return len+delta0;
   }
}



/* Handle binary integer instructions of the form
      op G, E  meaning
      op reg, reg-or-mem
   Is passed the a ptr to the modRM byte, the actual operation, and the
   data size.  Returns the address advanced completely over this
   instruction.

   G(src) is reg.
   E(dst) is reg-or-mem

   If E is reg, -->    GET %E,  tmp
                       OP %G,   tmp
                       PUT tmp, %E
 
   If E is mem, -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpv
                       OP %G, tmpv
                       ST tmpv, (tmpa)
*/
static
UInt dis_op2_G_E ( UChar       sorb,
                   Bool        locked,
                   Bool        addSubCarry,
                   IROp        op8, 
                   Bool        keep,
                   Int         size, 
                   Int         delta0,
                   const HChar* t_x86opc )
{
   HChar   dis_buf[50];
   Int     len;
   IRType  ty   = szToITy(size);
   IRTemp  dst1 = newTemp(ty);
   IRTemp  src  = newTemp(ty);
   IRTemp  dst0 = newTemp(ty);
   UChar   rm   = getIByte(delta0);
   IRTemp  addr = IRTemp_INVALID;

   /* addSubCarry == True indicates the intended operation is
      add-with-carry or subtract-with-borrow. */
   if (addSubCarry) {
      vassert(op8 == Iop_Add8 || op8 == Iop_Sub8);
      vassert(keep);
   }

   if (epartIsReg(rm)) {
      /* Specially handle XOR reg,reg, because that doesn't really
         depend on reg, and doing the obvious thing potentially
         generates a spurious value check failure due to the bogus
         dependency.  Ditto SBB reg,reg.*/
      if ((op8 == Iop_Xor8 || (op8 == Iop_Sub8 && addSubCarry))
          && gregOfRM(rm) == eregOfRM(rm)) {
         putIReg(size, eregOfRM(rm), mkU(ty,0));
      }
      assign(dst0, getIReg(size,eregOfRM(rm)));
      assign(src,  getIReg(size,gregOfRM(rm)));

      if (addSubCarry && op8 == Iop_Add8) {
         helper_ADC( size, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
         putIReg(size, eregOfRM(rm), mkexpr(dst1));
      } else
      if (addSubCarry && op8 == Iop_Sub8) {
         helper_SBB( size, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
         putIReg(size, eregOfRM(rm), mkexpr(dst1));
      } else {
         assign(dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)));
         if (isAddSub(op8))
            setFlags_DEP1_DEP2(op8, dst0, src, ty);
         else
            setFlags_DEP1(op8, dst1, ty);
         if (keep)
            putIReg(size, eregOfRM(rm), mkexpr(dst1));
      }

      DIP("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                          nameIReg(size,gregOfRM(rm)),
                          nameIReg(size,eregOfRM(rm)));
      return 1+delta0;
   }

   /* E refers to memory */    
   {
      addr = disAMode ( &len, sorb, delta0, dis_buf);
      assign(dst0, loadLE(ty,mkexpr(addr)));
      assign(src,  getIReg(size,gregOfRM(rm)));

      if (addSubCarry && op8 == Iop_Add8) {
         if (locked) {
            /* cas-style store */
            helper_ADC( size, dst1, dst0, src,
                        /*store*/addr, dst0/*expVal*/, guest_EIP_curr_instr );
         } else {
            /* normal store */
            helper_ADC( size, dst1, dst0, src,
                        /*store*/addr, IRTemp_INVALID, 0 );
         }
      } else
      if (addSubCarry && op8 == Iop_Sub8) {
         if (locked) {
            /* cas-style store */
            helper_SBB( size, dst1, dst0, src,
                        /*store*/addr, dst0/*expVal*/, guest_EIP_curr_instr );
         } else {
            /* normal store */
            helper_SBB( size, dst1, dst0, src,
                        /*store*/addr, IRTemp_INVALID, 0 );
         }
      } else {
         assign(dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)));
         if (keep) {
            if (locked) {
               if (0) vex_printf("locked case\n" );
               casLE( mkexpr(addr),
                      mkexpr(dst0)/*expval*/, 
                      mkexpr(dst1)/*newval*/, guest_EIP_curr_instr );
            } else {
               if (0) vex_printf("nonlocked case\n");
               storeLE(mkexpr(addr), mkexpr(dst1));
            }
         }
         if (isAddSub(op8))
            setFlags_DEP1_DEP2(op8, dst0, src, ty);
         else
            setFlags_DEP1(op8, dst1, ty);
      }

      DIP("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                          nameIReg(size,gregOfRM(rm)), dis_buf);
      return len+delta0;
   }
}


/* Handle move instructions of the form
      mov E, G  meaning
      mov reg-or-mem, reg
   Is passed the a ptr to the modRM byte, and the data size.  Returns
   the address advanced completely over this instruction.

   E(src) is reg-or-mem
   G(dst) is reg.

   If E is reg, -->    GET %E,  tmpv
                       PUT tmpv, %G
 
   If E is mem  -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpb
                       PUT tmpb, %G
*/
static
UInt dis_mov_E_G ( UChar       sorb,
                   Int         size, 
                   Int         delta0 )
{
   Int len;
   UChar rm = getIByte(delta0);
   HChar dis_buf[50];

   if (epartIsReg(rm)) {
      putIReg(size, gregOfRM(rm), getIReg(size, eregOfRM(rm)));
      DIP("mov%c %s,%s\n", nameISize(size), 
                           nameIReg(size,eregOfRM(rm)),
                           nameIReg(size,gregOfRM(rm)));
      return 1+delta0;
   }

   /* E refers to memory */    
   {
      IRTemp addr = disAMode ( &len, sorb, delta0, dis_buf );
      putIReg(size, gregOfRM(rm), loadLE(szToITy(size), mkexpr(addr)));
      DIP("mov%c %s,%s\n", nameISize(size), 
                           dis_buf,nameIReg(size,gregOfRM(rm)));
      return delta0+len;
   }
}


/* Handle move instructions of the form
      mov G, E  meaning
      mov reg, reg-or-mem
   Is passed the a ptr to the modRM byte, and the data size.  Returns
   the address advanced completely over this instruction.

   G(src) is reg.
   E(dst) is reg-or-mem

   If E is reg, -->    GET %G,  tmp
                       PUT tmp, %E
 
   If E is mem, -->    (getAddr E) -> tmpa
                       GET %G, tmpv
                       ST tmpv, (tmpa) 
*/
static
UInt dis_mov_G_E ( UChar       sorb,
                   Int         size, 
                   Int         delta0 )
{
   Int len;
   UChar rm = getIByte(delta0);
   HChar dis_buf[50];

   if (epartIsReg(rm)) {
      putIReg(size, eregOfRM(rm), getIReg(size, gregOfRM(rm)));
      DIP("mov%c %s,%s\n", nameISize(size), 
                           nameIReg(size,gregOfRM(rm)),
                           nameIReg(size,eregOfRM(rm)));
      return 1+delta0;
   }

   /* E refers to memory */    
   {
      IRTemp addr = disAMode ( &len, sorb, delta0, dis_buf);
      storeLE( mkexpr(addr), getIReg(size, gregOfRM(rm)) );
      DIP("mov%c %s,%s\n", nameISize(size), 
                           nameIReg(size,gregOfRM(rm)), dis_buf);
      return len+delta0;
   }
}


/* op $immediate, AL/AX/EAX. */
static
UInt dis_op_imm_A ( Int    size,
                    Bool   carrying,
                    IROp   op8,
                    Bool   keep,
                    Int    delta,
                    const HChar* t_x86opc )
{
   IRType ty   = szToITy(size);
   IRTemp dst0 = newTemp(ty);
   IRTemp src  = newTemp(ty);
   IRTemp dst1 = newTemp(ty);
   UInt lit    = getUDisp(size,delta);
   assign(dst0, getIReg(size,R_EAX));
   assign(src,  mkU(ty,lit));

   if (isAddSub(op8) && !carrying) {
      assign(dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)) );
      setFlags_DEP1_DEP2(op8, dst0, src, ty);
   } 
   else
   if (isLogic(op8)) {
      vassert(!carrying);
      assign(dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)) );
      setFlags_DEP1(op8, dst1, ty);
   } 
   else
   if (op8 == Iop_Add8 && carrying) {
      helper_ADC( size, dst1, dst0, src,
                  /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
   }
   else
   if (op8 == Iop_Sub8 && carrying) {
      helper_SBB( size, dst1, dst0, src,
                  /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
   }
   else
      vpanic("dis_op_imm_A(x86,guest)");

   if (keep)
      putIReg(size, R_EAX, mkexpr(dst1));

   DIP("%s%c $0x%x, %s\n", t_x86opc, nameISize(size), 
                           lit, nameIReg(size,R_EAX));
   return delta+size;
}


/* Sign- and Zero-extending moves. */
static
UInt dis_movx_E_G ( UChar      sorb,
                    Int delta, Int szs, Int szd, Bool sign_extend )
{
   UChar rm = getIByte(delta);
   if (epartIsReg(rm)) {
      if (szd == szs) {
         // mutant case.  See #250799
         putIReg(szd, gregOfRM(rm),
                           getIReg(szs,eregOfRM(rm)));
      } else {
         // normal case
         putIReg(szd, gregOfRM(rm),
                      unop(mkWidenOp(szs,szd,sign_extend), 
                           getIReg(szs,eregOfRM(rm))));
      }
      DIP("mov%c%c%c %s,%s\n", sign_extend ? 's' : 'z',
                               nameISize(szs), nameISize(szd),
                               nameIReg(szs,eregOfRM(rm)),
                               nameIReg(szd,gregOfRM(rm)));
      return 1+delta;
   }

   /* E refers to memory */    
   {
      Int    len;
      HChar  dis_buf[50];
      IRTemp addr = disAMode ( &len, sorb, delta, dis_buf );
      if (szd == szs) {
         // mutant case.  See #250799
         putIReg(szd, gregOfRM(rm),
                           loadLE(szToITy(szs),mkexpr(addr)));
      } else {
         // normal case
         putIReg(szd, gregOfRM(rm),
                      unop(mkWidenOp(szs,szd,sign_extend), 
                           loadLE(szToITy(szs),mkexpr(addr))));
      }
      DIP("mov%c%c%c %s,%s\n", sign_extend ? 's' : 'z',
                               nameISize(szs), nameISize(szd),
                               dis_buf, nameIReg(szd,gregOfRM(rm)));
      return len+delta;
   }
}


/* Generate code to divide ArchRegs EDX:EAX / DX:AX / AX by the 32 /
   16 / 8 bit quantity in the given IRTemp.  */
static
void codegen_div ( Int sz, IRTemp t, Bool signed_divide )
{
   IROp   op    = signed_divide ? Iop_DivModS64to32 : Iop_DivModU64to32;
   IRTemp src64 = newTemp(Ity_I64);
   IRTemp dst64 = newTemp(Ity_I64);
   switch (sz) {
      case 4:
         assign( src64, binop(Iop_32HLto64, 
                              getIReg(4,R_EDX), getIReg(4,R_EAX)) );
         assign( dst64, binop(op, mkexpr(src64), mkexpr(t)) );
         putIReg( 4, R_EAX, unop(Iop_64to32,mkexpr(dst64)) );
         putIReg( 4, R_EDX, unop(Iop_64HIto32,mkexpr(dst64)) );
         break;
      case 2: {
         IROp widen3264 = signed_divide ? Iop_32Sto64 : Iop_32Uto64;
         IROp widen1632 = signed_divide ? Iop_16Sto32 : Iop_16Uto32;
         assign( src64, unop(widen3264,
                             binop(Iop_16HLto32, 
                                   getIReg(2,R_EDX), getIReg(2,R_EAX))) );
         assign( dst64, binop(op, mkexpr(src64), unop(widen1632,mkexpr(t))) );
         putIReg( 2, R_EAX, unop(Iop_32to16,unop(Iop_64to32,mkexpr(dst64))) );
         putIReg( 2, R_EDX, unop(Iop_32to16,unop(Iop_64HIto32,mkexpr(dst64))) );
         break;
      }
      case 1: {
         IROp widen3264 = signed_divide ? Iop_32Sto64 : Iop_32Uto64;
         IROp widen1632 = signed_divide ? Iop_16Sto32 : Iop_16Uto32;
         IROp widen816  = signed_divide ? Iop_8Sto16  : Iop_8Uto16;
         assign( src64, unop(widen3264, unop(widen1632, getIReg(2,R_EAX))) );
         assign( dst64, 
                 binop(op, mkexpr(src64), 
                           unop(widen1632, unop(widen816, mkexpr(t)))) );
         putIReg( 1, R_AL, unop(Iop_16to8, unop(Iop_32to16,
                           unop(Iop_64to32,mkexpr(dst64)))) );
         putIReg( 1, R_AH, unop(Iop_16to8, unop(Iop_32to16,
                           unop(Iop_64HIto32,mkexpr(dst64)))) );
         break;
      }
      default: vpanic("codegen_div(x86)");
   }
}


static 
UInt dis_Grp1 ( UChar sorb, Bool locked,
                Int delta, UChar modrm, 
                Int am_sz, Int d_sz, Int sz, UInt d32 )
{
   Int     len;
   HChar   dis_buf[50];
   IRType  ty   = szToITy(sz);
   IRTemp  dst1 = newTemp(ty);
   IRTemp  src  = newTemp(ty);
   IRTemp  dst0 = newTemp(ty);
   IRTemp  addr = IRTemp_INVALID;
   IROp    op8  = Iop_INVALID;
   UInt    mask = sz==1 ? 0xFF : (sz==2 ? 0xFFFF : 0xFFFFFFFF);

   switch (gregOfRM(modrm)) {
      case 0: op8 = Iop_Add8; break;  case 1: op8 = Iop_Or8;  break;
      case 2: break;  // ADC
      case 3: break;  // SBB
      case 4: op8 = Iop_And8; break;  case 5: op8 = Iop_Sub8; break;
      case 6: op8 = Iop_Xor8; break;  case 7: op8 = Iop_Sub8; break;
      /*NOTREACHED*/
      default: vpanic("dis_Grp1: unhandled case");
   }

   if (epartIsReg(modrm)) {
      vassert(am_sz == 1);

      assign(dst0, getIReg(sz,eregOfRM(modrm)));
      assign(src,  mkU(ty,d32 & mask));

      if (gregOfRM(modrm) == 2 /* ADC */) {
         helper_ADC( sz, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
      } else 
      if (gregOfRM(modrm) == 3 /* SBB */) {
         helper_SBB( sz, dst1, dst0, src,
                     /*no store*/IRTemp_INVALID, IRTemp_INVALID, 0 );
      } else {
         assign(dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)));
         if (isAddSub(op8))
            setFlags_DEP1_DEP2(op8, dst0, src, ty);
         else
            setFlags_DEP1(op8, dst1, ty);
      }

      if (gregOfRM(modrm) < 7)
         putIReg(sz, eregOfRM(modrm), mkexpr(dst1));

      delta += (am_sz + d_sz);
      DIP("%s%c $0x%x, %s\n", nameGrp1(gregOfRM(modrm)), nameISize(sz), d32, 
                              nameIReg(sz,eregOfRM(modrm)));
   } else {
      addr = disAMode ( &len, sorb, delta, dis_buf);

      assign(dst0, loadLE(ty,mkexpr(addr)));
      assign(src, mkU(ty,d32 & mask));

      if (gregOfRM(modrm) == 2 /* ADC */) {
         if (locked) {
            /* cas-style store */
            helper_ADC( sz, dst1, dst0, src,
                       /*store*/addr, dst0/*expVal*/, guest_EIP_curr_instr );
         } else {
            /* normal store */
            helper_ADC( sz, dst1, dst0, src,
                        /*store*/addr, IRTemp_INVALID, 0 );
         }
      } else 
      if (gregOfRM(modrm) == 3 /* SBB */) {
         if (locked) {
            /* cas-style store */
            helper_SBB( sz, dst1, dst0, src,
                       /*store*/addr, dst0/*expVal*/, guest_EIP_curr_instr );
         } else {
            /* normal store */
            helper_SBB( sz, dst1, dst0, src,
                        /*store*/addr, IRTemp_INVALID, 0 );
         }
      } else {
         assign(dst1, binop(mkSizedOp(ty,op8), mkexpr(dst0), mkexpr(src)));
         if (gregOfRM(modrm) < 7) {
            if (locked) {
               casLE( mkexpr(addr), mkexpr(dst0)/*expVal*/, 
                                    mkexpr(dst1)/*newVal*/,
                                    guest_EIP_curr_instr );
            } else {
               storeLE(mkexpr(addr), mkexpr(dst1));
            }
         }
         if (isAddSub(op8))
            setFlags_DEP1_DEP2(op8, dst0, src, ty);
         else
            setFlags_DEP1(op8, dst1, ty);
      }

      delta += (len+d_sz);
      DIP("%s%c $0x%x, %s\n", nameGrp1(gregOfRM(modrm)), nameISize(sz),
                              d32, dis_buf);
   }
   return delta;
}


/* Group 2 extended opcodes.  shift_expr must be an 8-bit typed
   expression. */

static
UInt dis_Grp2 ( UChar sorb,
                Int delta, UChar modrm,
                Int am_sz, Int d_sz, Int sz, IRExpr* shift_expr,
                const HChar* shift_expr_txt, Bool* decode_OK )
{
   /* delta on entry points at the modrm byte. */
   HChar  dis_buf[50];
   Int    len;
   Bool   isShift, isRotate, isRotateC;
   IRType ty    = szToITy(sz);
   IRTemp dst0  = newTemp(ty);
   IRTemp dst1  = newTemp(ty);
   IRTemp addr  = IRTemp_INVALID;

   *decode_OK = True;

   vassert(sz == 1 || sz == 2 || sz == 4);

   /* Put value to shift/rotate in dst0. */
   if (epartIsReg(modrm)) {
      assign(dst0, getIReg(sz, eregOfRM(modrm)));
      delta += (am_sz + d_sz);
   } else {
      addr = disAMode ( &len, sorb, delta, dis_buf);
      assign(dst0, loadLE(ty,mkexpr(addr)));
      delta += len + d_sz;
   }

   isShift = False;
   switch (gregOfRM(modrm)) { case 4: case 5: case 6: case 7: isShift = True; }

   isRotate = False;
   switch (gregOfRM(modrm)) { case 0: case 1: isRotate = True; }

   isRotateC = False;
   switch (gregOfRM(modrm)) { case 2: case 3: isRotateC = True; }

   if (!isShift && !isRotate && !isRotateC) {
      /*NOTREACHED*/
      vpanic("dis_Grp2(Reg): unhandled case(x86)");
   }

   if (isRotateC) {
      /* call a helper; these insns are so ridiculous they do not
         deserve better */
      Bool     left = toBool(gregOfRM(modrm) == 2);
      IRTemp   r64  = newTemp(Ity_I64);
      IRExpr** args 
         = mkIRExprVec_4( widenUto32(mkexpr(dst0)), /* thing to rotate */
                          widenUto32(shift_expr),   /* rotate amount */
                          widenUto32(mk_x86g_calculate_eflags_all()),
                          mkU32(sz) );
      assign( r64, mkIRExprCCall(
                      Ity_I64, 
                      0/*regparm*/, 
                      left ? "x86g_calculate_RCL" : "x86g_calculate_RCR", 
                      left ? &x86g_calculate_RCL  : &x86g_calculate_RCR,
                      args
                   )
            );
      /* new eflags in hi half r64; new value in lo half r64 */
      assign( dst1, narrowTo(ty, unop(Iop_64to32, mkexpr(r64))) );
      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
      stmt( IRStmt_Put( OFFB_CC_DEP1, unop(Iop_64HIto32, mkexpr(r64)) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
      /* Set NDEP even though it isn't used.  This makes redundant-PUT
         elimination of previous stores to this field work better. */
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
   }

   if (isShift) {

      IRTemp pre32     = newTemp(Ity_I32);
      IRTemp res32     = newTemp(Ity_I32);
      IRTemp res32ss   = newTemp(Ity_I32);
      IRTemp shift_amt = newTemp(Ity_I8);
      IROp   op32;

      switch (gregOfRM(modrm)) { 
         case 4: op32 = Iop_Shl32; break;
         case 5: op32 = Iop_Shr32; break;
         case 6: op32 = Iop_Shl32; break;
         case 7: op32 = Iop_Sar32; break;
         /*NOTREACHED*/
         default: vpanic("dis_Grp2:shift"); break;
      }

      /* Widen the value to be shifted to 32 bits, do the shift, and
         narrow back down.  This seems surprisingly long-winded, but
         unfortunately the Intel semantics requires that 8/16-bit
         shifts give defined results for shift values all the way up
         to 31, and this seems the simplest way to do it.  It has the
         advantage that the only IR level shifts generated are of 32
         bit values, and the shift amount is guaranteed to be in the
         range 0 .. 31, thereby observing the IR semantics requiring
         all shift values to be in the range 0 .. 2^word_size-1. */

      /* shift_amt = shift_expr & 31, regardless of operation size */
      assign( shift_amt, binop(Iop_And8, shift_expr, mkU8(31)) );

      /* suitably widen the value to be shifted to 32 bits. */
      assign( pre32, op32==Iop_Sar32 ? widenSto32(mkexpr(dst0))
                                     : widenUto32(mkexpr(dst0)) );

      /* res32 = pre32 `shift` shift_amt */
      assign( res32, binop(op32, mkexpr(pre32), mkexpr(shift_amt)) );

      /* res32ss = pre32 `shift` ((shift_amt - 1) & 31) */
      assign( res32ss,
              binop(op32,
                    mkexpr(pre32), 
                    binop(Iop_And8,
                          binop(Iop_Sub8,
                                mkexpr(shift_amt), mkU8(1)),
                          mkU8(31))) );

      /* Build the flags thunk. */
      setFlags_DEP1_DEP2_shift(op32, res32, res32ss, ty, shift_amt);

      /* Narrow the result back down. */
      assign( dst1, narrowTo(ty, mkexpr(res32)) );

   } /* if (isShift) */

   else 
   if (isRotate) {
      Int    ccOp      = ty==Ity_I8 ? 0 : (ty==Ity_I16 ? 1 : 2);
      Bool   left      = toBool(gregOfRM(modrm) == 0);
      IRTemp rot_amt   = newTemp(Ity_I8);
      IRTemp rot_amt32 = newTemp(Ity_I8);
      IRTemp oldFlags  = newTemp(Ity_I32);

      /* rot_amt = shift_expr & mask */
      /* By masking the rotate amount thusly, the IR-level Shl/Shr
         expressions never shift beyond the word size and thus remain
         well defined. */
      assign(rot_amt32, binop(Iop_And8, shift_expr, mkU8(31)));

      if (ty == Ity_I32)
         assign(rot_amt, mkexpr(rot_amt32));
      else
         assign(rot_amt, binop(Iop_And8, mkexpr(rot_amt32), mkU8(8*sz-1)));

      if (left) {

         /* dst1 = (dst0 << rot_amt) | (dst0 >>u (wordsize-rot_amt)) */
         assign(dst1, 
            binop( mkSizedOp(ty,Iop_Or8),
                   binop( mkSizedOp(ty,Iop_Shl8), 
                          mkexpr(dst0),
                          mkexpr(rot_amt)
                   ),
                   binop( mkSizedOp(ty,Iop_Shr8), 
                          mkexpr(dst0), 
                          binop(Iop_Sub8,mkU8(8*sz), mkexpr(rot_amt))
                   )
            )
         );
         ccOp += X86G_CC_OP_ROLB;

      } else { /* right */

         /* dst1 = (dst0 >>u rot_amt) | (dst0 << (wordsize-rot_amt)) */
         assign(dst1, 
            binop( mkSizedOp(ty,Iop_Or8),
                   binop( mkSizedOp(ty,Iop_Shr8), 
                          mkexpr(dst0),
                          mkexpr(rot_amt)
                   ),
                   binop( mkSizedOp(ty,Iop_Shl8), 
                          mkexpr(dst0), 
                          binop(Iop_Sub8,mkU8(8*sz), mkexpr(rot_amt))
                   )
            )
         );
         ccOp += X86G_CC_OP_RORB;

      }

      /* dst1 now holds the rotated value.  Build flag thunk.  We
         need the resulting value for this, and the previous flags.
         Except don't set it if the rotate count is zero. */

      assign(oldFlags, mk_x86g_calculate_eflags_all());

      /* rot_amt32 :: Ity_I8.  We need to convert it to I1. */
      IRTemp rot_amt32b = newTemp(Ity_I1);
      assign(rot_amt32b, binop(Iop_CmpNE8, mkexpr(rot_amt32), mkU8(0)) );

      /* CC_DEP1 is the rotated value.  CC_NDEP is flags before. */
      stmt( IRStmt_Put( OFFB_CC_OP,
                        IRExpr_ITE( mkexpr(rot_amt32b),
                                    mkU32(ccOp),
                                    IRExpr_Get(OFFB_CC_OP,Ity_I32) ) ));
      stmt( IRStmt_Put( OFFB_CC_DEP1, 
                        IRExpr_ITE( mkexpr(rot_amt32b),
                                    widenUto32(mkexpr(dst1)),
                                    IRExpr_Get(OFFB_CC_DEP1,Ity_I32) ) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, 
                        IRExpr_ITE( mkexpr(rot_amt32b),
                                    mkU32(0),
                                    IRExpr_Get(OFFB_CC_DEP2,Ity_I32) ) ));
      stmt( IRStmt_Put( OFFB_CC_NDEP, 
                        IRExpr_ITE( mkexpr(rot_amt32b),
                                    mkexpr(oldFlags),
                                    IRExpr_Get(OFFB_CC_NDEP,Ity_I32) ) ));
   } /* if (isRotate) */

   /* Save result, and finish up. */
   if (epartIsReg(modrm)) {
      putIReg(sz, eregOfRM(modrm), mkexpr(dst1));
      if (vex_traceflags & VEX_TRACE_FE) {
         vex_printf("%s%c ",
                    nameGrp2(gregOfRM(modrm)), nameISize(sz) );
         if (shift_expr_txt)
            vex_printf("%s", shift_expr_txt);
         else
            ppIRExpr(shift_expr);
         vex_printf(", %s\n", nameIReg(sz,eregOfRM(modrm)));
      }
   } else {
      storeLE(mkexpr(addr), mkexpr(dst1));
      if (vex_traceflags & VEX_TRACE_FE) {
         vex_printf("%s%c ",
                    nameGrp2(gregOfRM(modrm)), nameISize(sz) );
         if (shift_expr_txt)
            vex_printf("%s", shift_expr_txt);
         else
            ppIRExpr(shift_expr);
         vex_printf(", %s\n", dis_buf);
      }
   }
   return delta;
}


/* Group 8 extended opcodes (but BT/BTS/BTC/BTR only). */
static
UInt dis_Grp8_Imm ( UChar sorb,
                    Bool locked,
                    Int delta, UChar modrm,
                    Int am_sz, Int sz, UInt src_val,
                    Bool* decode_OK )
{
   /* src_val denotes a d8.
      And delta on entry points at the modrm byte. */

   IRType ty     = szToITy(sz);
   IRTemp t2     = newTemp(Ity_I32);
   IRTemp t2m    = newTemp(Ity_I32);
   IRTemp t_addr = IRTemp_INVALID;
   HChar  dis_buf[50];
   UInt   mask;

   /* we're optimists :-) */
   *decode_OK = True;

   /* Limit src_val -- the bit offset -- to something within a word.
      The Intel docs say that literal offsets larger than a word are
      masked in this way. */
   switch (sz) {
      case 2:  src_val &= 15; break;
      case 4:  src_val &= 31; break;
      default: *decode_OK = False; return delta;
   }

   /* Invent a mask suitable for the operation. */
   switch (gregOfRM(modrm)) {
      case 4: /* BT */  mask = 0;               break;
      case 5: /* BTS */ mask = 1 << src_val;    break;
      case 6: /* BTR */ mask = ~(1 << src_val); break;
      case 7: /* BTC */ mask = 1 << src_val;    break;
         /* If this needs to be extended, probably simplest to make a
            new function to handle the other cases (0 .. 3).  The
            Intel docs do however not indicate any use for 0 .. 3, so
            we don't expect this to happen. */
      default: *decode_OK = False; return delta;
   }

   /* Fetch the value to be tested and modified into t2, which is
      32-bits wide regardless of sz. */
   if (epartIsReg(modrm)) {
      vassert(am_sz == 1);
      assign( t2, widenUto32(getIReg(sz, eregOfRM(modrm))) );
      delta += (am_sz + 1);
      DIP("%s%c $0x%x, %s\n", nameGrp8(gregOfRM(modrm)), nameISize(sz),
                              src_val, nameIReg(sz,eregOfRM(modrm)));
   } else {
      Int len;
      t_addr = disAMode ( &len, sorb, delta, dis_buf);
      delta  += (len+1);
      assign( t2, widenUto32(loadLE(ty, mkexpr(t_addr))) );
      DIP("%s%c $0x%x, %s\n", nameGrp8(gregOfRM(modrm)), nameISize(sz),
                              src_val, dis_buf);
   }

   /* Compute the new value into t2m, if non-BT. */
   switch (gregOfRM(modrm)) {
      case 4: /* BT */
         break;
      case 5: /* BTS */
         assign( t2m, binop(Iop_Or32, mkU32(mask), mkexpr(t2)) );
         break;
      case 6: /* BTR */
         assign( t2m, binop(Iop_And32, mkU32(mask), mkexpr(t2)) );
         break;
      case 7: /* BTC */
         assign( t2m, binop(Iop_Xor32, mkU32(mask), mkexpr(t2)) );
         break;
      default: 
         /*NOTREACHED*/ /*the previous switch guards this*/
         vassert(0);
   }

   /* Write the result back, if non-BT.  If the CAS fails then we
      side-exit from the trace at this point, and so the flag state is
      not affected.  This is of course as required. */
   if (gregOfRM(modrm) != 4 /* BT */) {
      if (epartIsReg(modrm)) {
         putIReg(sz, eregOfRM(modrm), narrowTo(ty, mkexpr(t2m)));
      } else {
         if (locked) {
            casLE( mkexpr(t_addr),
                   narrowTo(ty, mkexpr(t2))/*expd*/,
                   narrowTo(ty, mkexpr(t2m))/*new*/,
                   guest_EIP_curr_instr );
         } else {
            storeLE(mkexpr(t_addr), narrowTo(ty, mkexpr(t2m)));
         }
      }
   }

   /* Copy relevant bit from t2 into the carry flag. */
   /* Flags: C=selected bit, O,S,Z,A,P undefined, so are set to zero. */
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
   stmt( IRStmt_Put( 
            OFFB_CC_DEP1,
            binop(Iop_And32,
                  binop(Iop_Shr32, mkexpr(t2), mkU8(src_val)),
                  mkU32(1))
       ));
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));

   return delta;
}


/* Signed/unsigned widening multiply.  Generate IR to multiply the
   value in EAX/AX/AL by the given IRTemp, and park the result in
   EDX:EAX/DX:AX/AX.
*/
static void codegen_mulL_A_D ( Int sz, Bool syned, 
                               IRTemp tmp, const HChar* tmp_txt )
{
   IRType ty = szToITy(sz);
   IRTemp t1 = newTemp(ty);

   assign( t1, getIReg(sz, R_EAX) );

   switch (ty) {
      case Ity_I32: {
         IRTemp res64   = newTemp(Ity_I64);
         IRTemp resHi   = newTemp(Ity_I32);
         IRTemp resLo   = newTemp(Ity_I32);
         IROp   mulOp   = syned ? Iop_MullS32 : Iop_MullU32;
         UInt   tBaseOp = syned ? X86G_CC_OP_SMULB : X86G_CC_OP_UMULB;
         setFlags_MUL ( Ity_I32, t1, tmp, tBaseOp );
         assign( res64, binop(mulOp, mkexpr(t1), mkexpr(tmp)) );
         assign( resHi, unop(Iop_64HIto32,mkexpr(res64)));
         assign( resLo, unop(Iop_64to32,mkexpr(res64)));
         putIReg(4, R_EDX, mkexpr(resHi));
         putIReg(4, R_EAX, mkexpr(resLo));
         break;
      }
      case Ity_I16: {
         IRTemp res32   = newTemp(Ity_I32);
         IRTemp resHi   = newTemp(Ity_I16);
         IRTemp resLo   = newTemp(Ity_I16);
         IROp   mulOp   = syned ? Iop_MullS16 : Iop_MullU16;
         UInt   tBaseOp = syned ? X86G_CC_OP_SMULB : X86G_CC_OP_UMULB;
         setFlags_MUL ( Ity_I16, t1, tmp, tBaseOp );
         assign( res32, binop(mulOp, mkexpr(t1), mkexpr(tmp)) );
         assign( resHi, unop(Iop_32HIto16,mkexpr(res32)));
         assign( resLo, unop(Iop_32to16,mkexpr(res32)));
         putIReg(2, R_EDX, mkexpr(resHi));
         putIReg(2, R_EAX, mkexpr(resLo));
         break;
      }
      case Ity_I8: {
         IRTemp res16   = newTemp(Ity_I16);
         IRTemp resHi   = newTemp(Ity_I8);
         IRTemp resLo   = newTemp(Ity_I8);
         IROp   mulOp   = syned ? Iop_MullS8 : Iop_MullU8;
         UInt   tBaseOp = syned ? X86G_CC_OP_SMULB : X86G_CC_OP_UMULB;
         setFlags_MUL ( Ity_I8, t1, tmp, tBaseOp );
         assign( res16, binop(mulOp, mkexpr(t1), mkexpr(tmp)) );
         assign( resHi, unop(Iop_16HIto8,mkexpr(res16)));
         assign( resLo, unop(Iop_16to8,mkexpr(res16)));
         putIReg(2, R_EAX, mkexpr(res16));
         break;
      }
      default:
         vpanic("codegen_mulL_A_D(x86)");
   }
   DIP("%s%c %s\n", syned ? "imul" : "mul", nameISize(sz), tmp_txt);
}


/* Group 3 extended opcodes. */
static 
UInt dis_Grp3 ( UChar sorb, Bool locked, Int sz, Int delta, Bool* decode_OK )
{
   UInt    d32;
   UChar   modrm;
   HChar   dis_buf[50];
   Int     len;
   IRTemp  addr;
   IRType  ty = szToITy(sz);
   IRTemp  t1 = newTemp(ty);
   IRTemp dst1, src, dst0;

   *decode_OK = True; /* may change this later */

   modrm = getIByte(delta);

   if (locked && (gregOfRM(modrm) != 2 && gregOfRM(modrm) != 3)) {
      /* LOCK prefix only allowed with not and neg subopcodes */
      *decode_OK = False;
      return delta;
   }

   if (epartIsReg(modrm)) {
      switch (gregOfRM(modrm)) {
         case 0: { /* TEST */
            delta++; d32 = getUDisp(sz, delta); delta += sz;
            dst1 = newTemp(ty);
            assign(dst1, binop(mkSizedOp(ty,Iop_And8),
                               getIReg(sz,eregOfRM(modrm)),
                               mkU(ty,d32)));
            setFlags_DEP1( Iop_And8, dst1, ty );
            DIP("test%c $0x%x, %s\n", nameISize(sz), d32, 
                                      nameIReg(sz, eregOfRM(modrm)));
            break;
         }
         case 1: /* UNDEFINED */
           /* The Intel docs imply this insn is undefined and binutils
              agrees.  Unfortunately Core 2 will run it (with who
              knows what result?)  sandpile.org reckons it's an alias
              for case 0.  We play safe. */
           *decode_OK = False;
           break;
         case 2: /* NOT */
            delta++;
            putIReg(sz, eregOfRM(modrm),
                        unop(mkSizedOp(ty,Iop_Not8),
                             getIReg(sz, eregOfRM(modrm))));
            DIP("not%c %s\n", nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
            break;
         case 3: /* NEG */
            delta++;
            dst0 = newTemp(ty);
            src  = newTemp(ty);
            dst1 = newTemp(ty);
            assign(dst0, mkU(ty,0));
            assign(src,  getIReg(sz,eregOfRM(modrm)));
            assign(dst1, binop(mkSizedOp(ty,Iop_Sub8), mkexpr(dst0), mkexpr(src)));
            setFlags_DEP1_DEP2(Iop_Sub8, dst0, src, ty);
            putIReg(sz, eregOfRM(modrm), mkexpr(dst1));
            DIP("neg%c %s\n", nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
            break;
         case 4: /* MUL (unsigned widening) */
            delta++;
            src = newTemp(ty);
            assign(src, getIReg(sz,eregOfRM(modrm)));
            codegen_mulL_A_D ( sz, False, src, nameIReg(sz,eregOfRM(modrm)) );
            break;
         case 5: /* IMUL (signed widening) */
            delta++;
            src = newTemp(ty);
            assign(src, getIReg(sz,eregOfRM(modrm)));
            codegen_mulL_A_D ( sz, True, src, nameIReg(sz,eregOfRM(modrm)) );
            break;
         case 6: /* DIV */
            delta++;
            assign( t1, getIReg(sz, eregOfRM(modrm)) );
            codegen_div ( sz, t1, False );
            DIP("div%c %s\n", nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
            break;
         case 7: /* IDIV */
            delta++;
            assign( t1, getIReg(sz, eregOfRM(modrm)) );
            codegen_div ( sz, t1, True );
            DIP("idiv%c %s\n", nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
            break;
         default: 
            /* This can't happen - gregOfRM should return 0 .. 7 only */
            vpanic("Grp3(x86)");
      }
   } else {
      addr = disAMode ( &len, sorb, delta, dis_buf );
      t1   = newTemp(ty);
      delta += len;
      assign(t1, loadLE(ty,mkexpr(addr)));
      switch (gregOfRM(modrm)) {
         case 0: { /* TEST */
            d32 = getUDisp(sz, delta); delta += sz;
            dst1 = newTemp(ty);
            assign(dst1, binop(mkSizedOp(ty,Iop_And8),
                               mkexpr(t1), mkU(ty,d32)));
            setFlags_DEP1( Iop_And8, dst1, ty );
            DIP("test%c $0x%x, %s\n", nameISize(sz), d32, dis_buf);
            break;
         }
         case 1: /* UNDEFINED */
           /* See comment above on R case */
           *decode_OK = False;
           break;
         case 2: /* NOT */
            dst1 = newTemp(ty);
            assign(dst1, unop(mkSizedOp(ty,Iop_Not8), mkexpr(t1)));
            if (locked) {
               casLE( mkexpr(addr), mkexpr(t1)/*expd*/, mkexpr(dst1)/*new*/,
                                    guest_EIP_curr_instr );
            } else {
               storeLE( mkexpr(addr), mkexpr(dst1) );
            }
            DIP("not%c %s\n", nameISize(sz), dis_buf);
            break;
         case 3: /* NEG */
            dst0 = newTemp(ty);
            src  = newTemp(ty);
            dst1 = newTemp(ty);
            assign(dst0, mkU(ty,0));
            assign(src,  mkexpr(t1));
            assign(dst1, binop(mkSizedOp(ty,Iop_Sub8),
                               mkexpr(dst0), mkexpr(src)));
            if (locked) {
               casLE( mkexpr(addr), mkexpr(t1)/*expd*/, mkexpr(dst1)/*new*/,
                                    guest_EIP_curr_instr );
            } else {
               storeLE( mkexpr(addr), mkexpr(dst1) );
            }
            setFlags_DEP1_DEP2(Iop_Sub8, dst0, src, ty);
            DIP("neg%c %s\n", nameISize(sz), dis_buf);
            break;
         case 4: /* MUL */
            codegen_mulL_A_D ( sz, False, t1, dis_buf );
            break;
         case 5: /* IMUL */
            codegen_mulL_A_D ( sz, True, t1, dis_buf );
            break;
         case 6: /* DIV */
            codegen_div ( sz, t1, False );
            DIP("div%c %s\n", nameISize(sz), dis_buf);
            break;
         case 7: /* IDIV */
            codegen_div ( sz, t1, True );
            DIP("idiv%c %s\n", nameISize(sz), dis_buf);
            break;
         default: 
            /* This can't happen - gregOfRM should return 0 .. 7 only */
            vpanic("Grp3(x86)");
      }
   }
   return delta;
}


/* Group 4 extended opcodes. */
static
UInt dis_Grp4 ( UChar sorb, Bool locked, Int delta, Bool* decode_OK )
{
   Int   alen;
   UChar modrm;
   HChar dis_buf[50];
   IRType ty = Ity_I8;
   IRTemp t1 = newTemp(ty);
   IRTemp t2 = newTemp(ty);

   *decode_OK = True;

   modrm = getIByte(delta);

   if (locked && (gregOfRM(modrm) != 0 && gregOfRM(modrm) != 1)) {
      /* LOCK prefix only allowed with inc and dec subopcodes */
      *decode_OK = False;
      return delta;
   }

   if (epartIsReg(modrm)) {
      assign(t1, getIReg(1, eregOfRM(modrm)));
      switch (gregOfRM(modrm)) {
         case 0: /* INC */
            assign(t2, binop(Iop_Add8, mkexpr(t1), mkU8(1)));
            putIReg(1, eregOfRM(modrm), mkexpr(t2));
            setFlags_INC_DEC( True, t2, ty );
            break;
         case 1: /* DEC */
            assign(t2, binop(Iop_Sub8, mkexpr(t1), mkU8(1)));
            putIReg(1, eregOfRM(modrm), mkexpr(t2));
            setFlags_INC_DEC( False, t2, ty );
            break;
         default: 
            *decode_OK = False;
            return delta;
      }
      delta++;
      DIP("%sb %s\n", nameGrp4(gregOfRM(modrm)),
                      nameIReg(1, eregOfRM(modrm)));
   } else {
      IRTemp addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( t1, loadLE(ty, mkexpr(addr)) );
      switch (gregOfRM(modrm)) {
         case 0: /* INC */
            assign(t2, binop(Iop_Add8, mkexpr(t1), mkU8(1)));
            if (locked) {
               casLE( mkexpr(addr), mkexpr(t1)/*expd*/, mkexpr(t2)/*new*/, 
                      guest_EIP_curr_instr );
            } else {
               storeLE( mkexpr(addr), mkexpr(t2) );
            }
            setFlags_INC_DEC( True, t2, ty );
            break;
         case 1: /* DEC */
            assign(t2, binop(Iop_Sub8, mkexpr(t1), mkU8(1)));
            if (locked) {
               casLE( mkexpr(addr), mkexpr(t1)/*expd*/, mkexpr(t2)/*new*/, 
                      guest_EIP_curr_instr );
            } else {
               storeLE( mkexpr(addr), mkexpr(t2) );
            }
            setFlags_INC_DEC( False, t2, ty );
            break;
         default: 
            *decode_OK = False;
            return delta;
      }
      delta += alen;
      DIP("%sb %s\n", nameGrp4(gregOfRM(modrm)), dis_buf);
   }
   return delta;
}


/* Group 5 extended opcodes. */
static
UInt dis_Grp5 ( UChar sorb, Bool locked, Int sz, Int delta, 
                /*MOD*/DisResult* dres, /*OUT*/Bool* decode_OK )
{
   Int     len;
   UChar   modrm;
   HChar   dis_buf[50];
   IRTemp  addr = IRTemp_INVALID;
   IRType  ty = szToITy(sz);
   IRTemp  t1 = newTemp(ty);
   IRTemp  t2 = IRTemp_INVALID;

   *decode_OK = True;

   modrm = getIByte(delta);

   if (locked && (gregOfRM(modrm) != 0 && gregOfRM(modrm) != 1)) {
      /* LOCK prefix only allowed with inc and dec subopcodes */
      *decode_OK = False;
      return delta;
   }

   if (epartIsReg(modrm)) {
      assign(t1, getIReg(sz,eregOfRM(modrm)));
      switch (gregOfRM(modrm)) {
         case 0: /* INC */ 
            vassert(sz == 2 || sz == 4);
            t2 = newTemp(ty);
            assign(t2, binop(mkSizedOp(ty,Iop_Add8),
                             mkexpr(t1), mkU(ty,1)));
            setFlags_INC_DEC( True, t2, ty );
            putIReg(sz,eregOfRM(modrm),mkexpr(t2));
            break;
         case 1: /* DEC */ 
            vassert(sz == 2 || sz == 4);
            t2 = newTemp(ty);
            assign(t2, binop(mkSizedOp(ty,Iop_Sub8),
                             mkexpr(t1), mkU(ty,1)));
            setFlags_INC_DEC( False, t2, ty );
            putIReg(sz,eregOfRM(modrm),mkexpr(t2));
            break;
         case 2: /* call Ev */
            vassert(sz == 4);
            t2 = newTemp(Ity_I32);
            assign(t2, binop(Iop_Sub32, getIReg(4,R_ESP), mkU32(4)));
            putIReg(4, R_ESP, mkexpr(t2));
            storeLE( mkexpr(t2), mkU32(guest_EIP_bbstart+delta+1));
            jmp_treg(dres, Ijk_Call, t1);
            vassert(dres->whatNext == Dis_StopHere);
            break;
         case 4: /* jmp Ev */
            vassert(sz == 4);
            jmp_treg(dres, Ijk_Boring, t1);
            vassert(dres->whatNext == Dis_StopHere);
            break;
         case 6: /* PUSH Ev */
            vassert(sz == 4 || sz == 2);
            t2 = newTemp(Ity_I32);
            assign( t2, binop(Iop_Sub32,getIReg(4,R_ESP),mkU32(sz)) );
            putIReg(4, R_ESP, mkexpr(t2) );
            storeLE( mkexpr(t2), mkexpr(t1) );
            break;
         default: 
            *decode_OK = False;
            return delta;
      }
      delta++;
      DIP("%s%c %s\n", nameGrp5(gregOfRM(modrm)),
                       nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
   } else {
      addr = disAMode ( &len, sorb, delta, dis_buf );
      assign(t1, loadLE(ty,mkexpr(addr)));
      switch (gregOfRM(modrm)) {
         case 0: /* INC */ 
            t2 = newTemp(ty);
            assign(t2, binop(mkSizedOp(ty,Iop_Add8),
                             mkexpr(t1), mkU(ty,1)));
            if (locked) {
               casLE( mkexpr(addr),
                      mkexpr(t1), mkexpr(t2), guest_EIP_curr_instr );
            } else {
               storeLE(mkexpr(addr),mkexpr(t2));
            }
            setFlags_INC_DEC( True, t2, ty );
            break;
         case 1: /* DEC */ 
            t2 = newTemp(ty);
            assign(t2, binop(mkSizedOp(ty,Iop_Sub8),
                             mkexpr(t1), mkU(ty,1)));
            if (locked) {
               casLE( mkexpr(addr),
                      mkexpr(t1), mkexpr(t2), guest_EIP_curr_instr );
            } else {
               storeLE(mkexpr(addr),mkexpr(t2));
            }
            setFlags_INC_DEC( False, t2, ty );
            break;
         case 2: /* call Ev */
            vassert(sz == 4);
            t2 = newTemp(Ity_I32);
            assign(t2, binop(Iop_Sub32, getIReg(4,R_ESP), mkU32(4)));
            putIReg(4, R_ESP, mkexpr(t2));
            storeLE( mkexpr(t2), mkU32(guest_EIP_bbstart+delta+len));
            jmp_treg(dres, Ijk_Call, t1);
            vassert(dres->whatNext == Dis_StopHere);
            break;
         case 4: /* JMP Ev */
            vassert(sz == 4);
            jmp_treg(dres, Ijk_Boring, t1);
            vassert(dres->whatNext == Dis_StopHere);
            break;
         case 6: /* PUSH Ev */
            vassert(sz == 4 || sz == 2);
            t2 = newTemp(Ity_I32);
            assign( t2, binop(Iop_Sub32,getIReg(4,R_ESP),mkU32(sz)) );
            putIReg(4, R_ESP, mkexpr(t2) );
            storeLE( mkexpr(t2), mkexpr(t1) );
            break;
         default: 
            *decode_OK = False;
            return delta;
      }
      delta += len;
      DIP("%s%c %s\n", nameGrp5(gregOfRM(modrm)),
                       nameISize(sz), dis_buf);
   }
   return delta;
}


/*------------------------------------------------------------*/
/*--- Disassembling string ops (including REP prefixes)    ---*/
/*------------------------------------------------------------*/

/* Code shared by all the string ops */
static
void dis_string_op_increment(Int sz, Int t_inc)
{
   if (sz == 4 || sz == 2) {
      assign( t_inc, 
              binop(Iop_Shl32, IRExpr_Get( OFFB_DFLAG, Ity_I32 ),
                               mkU8(sz/2) ) );
   } else {
      assign( t_inc, 
              IRExpr_Get( OFFB_DFLAG, Ity_I32 ) );
   }
}

static
void dis_string_op( void (*dis_OP)( Int, IRTemp ), 
                    Int sz, const HChar* name, UChar sorb )
{
   IRTemp t_inc = newTemp(Ity_I32);
   vassert(sorb == 0); /* hmm.  so what was the point of passing it in? */
   dis_string_op_increment(sz, t_inc);
   dis_OP( sz, t_inc );
   DIP("%s%c\n", name, nameISize(sz));
}

static 
void dis_MOVS ( Int sz, IRTemp t_inc )
{
   IRType ty = szToITy(sz);
   IRTemp td = newTemp(Ity_I32);   /* EDI */
   IRTemp ts = newTemp(Ity_I32);   /* ESI */

   assign( td, getIReg(4, R_EDI) );
   assign( ts, getIReg(4, R_ESI) );

   storeLE( mkexpr(td), loadLE(ty,mkexpr(ts)) );

   putIReg( 4, R_EDI, binop(Iop_Add32, mkexpr(td), mkexpr(t_inc)) );
   putIReg( 4, R_ESI, binop(Iop_Add32, mkexpr(ts), mkexpr(t_inc)) );
}

static 
void dis_LODS ( Int sz, IRTemp t_inc )
{
   IRType ty = szToITy(sz);
   IRTemp ts = newTemp(Ity_I32);   /* ESI */

   assign( ts, getIReg(4, R_ESI) );

   putIReg( sz, R_EAX, loadLE(ty, mkexpr(ts)) );

   putIReg( 4, R_ESI, binop(Iop_Add32, mkexpr(ts), mkexpr(t_inc)) );
}

static 
void dis_STOS ( Int sz, IRTemp t_inc )
{
   IRType ty = szToITy(sz);
   IRTemp ta = newTemp(ty);        /* EAX */
   IRTemp td = newTemp(Ity_I32);   /* EDI */

   assign( ta, getIReg(sz, R_EAX) );
   assign( td, getIReg(4, R_EDI) );

   storeLE( mkexpr(td), mkexpr(ta) );

   putIReg( 4, R_EDI, binop(Iop_Add32, mkexpr(td), mkexpr(t_inc)) );
}

static 
void dis_CMPS ( Int sz, IRTemp t_inc )
{
   IRType ty  = szToITy(sz);
   IRTemp tdv = newTemp(ty);      /* (EDI) */
   IRTemp tsv = newTemp(ty);      /* (ESI) */
   IRTemp td  = newTemp(Ity_I32); /*  EDI  */
   IRTemp ts  = newTemp(Ity_I32); /*  ESI  */

   assign( td, getIReg(4, R_EDI) );
   assign( ts, getIReg(4, R_ESI) );

   assign( tdv, loadLE(ty,mkexpr(td)) );
   assign( tsv, loadLE(ty,mkexpr(ts)) );

   setFlags_DEP1_DEP2 ( Iop_Sub8, tsv, tdv, ty );

   putIReg(4, R_EDI, binop(Iop_Add32, mkexpr(td), mkexpr(t_inc)) );
   putIReg(4, R_ESI, binop(Iop_Add32, mkexpr(ts), mkexpr(t_inc)) );
}

static 
void dis_SCAS ( Int sz, IRTemp t_inc )
{
   IRType ty  = szToITy(sz);
   IRTemp ta  = newTemp(ty);       /*  EAX  */
   IRTemp td  = newTemp(Ity_I32);  /*  EDI  */
   IRTemp tdv = newTemp(ty);       /* (EDI) */

   assign( ta, getIReg(sz, R_EAX) );
   assign( td, getIReg(4, R_EDI) );

   assign( tdv, loadLE(ty,mkexpr(td)) );
   setFlags_DEP1_DEP2 ( Iop_Sub8, ta, tdv, ty );

   putIReg(4, R_EDI, binop(Iop_Add32, mkexpr(td), mkexpr(t_inc)) );
}


/* Wrap the appropriate string op inside a REP/REPE/REPNE.
   We assume the insn is the last one in the basic block, and so emit a jump
   to the next insn, rather than just falling through. */
static 
void dis_REP_op ( /*MOD*/DisResult* dres,
                  X86Condcode cond,
                  void (*dis_OP)(Int, IRTemp),
                  Int sz, Addr32 eip, Addr32 eip_next, const HChar* name )
{
   IRTemp t_inc = newTemp(Ity_I32);
   IRTemp tc    = newTemp(Ity_I32);  /*  ECX  */

   assign( tc, getIReg(4,R_ECX) );

   stmt( IRStmt_Exit( binop(Iop_CmpEQ32,mkexpr(tc),mkU32(0)),
                      Ijk_Boring,
                      IRConst_U32(eip_next), OFFB_EIP ) );

   putIReg(4, R_ECX, binop(Iop_Sub32, mkexpr(tc), mkU32(1)) );

   dis_string_op_increment(sz, t_inc);
   dis_OP (sz, t_inc);

   if (cond == X86CondAlways) {
      jmp_lit(dres, Ijk_Boring, eip);
      vassert(dres->whatNext == Dis_StopHere);
   } else {
      stmt( IRStmt_Exit( mk_x86g_calculate_condition(cond),
                         Ijk_Boring,
                         IRConst_U32(eip), OFFB_EIP ) );
      jmp_lit(dres, Ijk_Boring, eip_next);
      vassert(dres->whatNext == Dis_StopHere);
   }
   DIP("%s%c\n", name, nameISize(sz));
}


/*------------------------------------------------------------*/
/*--- Arithmetic, etc.                                     ---*/
/*------------------------------------------------------------*/

/* IMUL E, G.  Supplied eip points to the modR/M byte. */
static
UInt dis_mul_E_G ( UChar       sorb,
                   Int         size, 
                   Int         delta0 )
{
   Int    alen;
   HChar  dis_buf[50];
   UChar  rm = getIByte(delta0);
   IRType ty = szToITy(size);
   IRTemp te = newTemp(ty);
   IRTemp tg = newTemp(ty);
   IRTemp resLo = newTemp(ty);

   assign( tg, getIReg(size, gregOfRM(rm)) );
   if (epartIsReg(rm)) {
      assign( te, getIReg(size, eregOfRM(rm)) );
   } else {
      IRTemp addr = disAMode( &alen, sorb, delta0, dis_buf );
      assign( te, loadLE(ty,mkexpr(addr)) );
   }

   setFlags_MUL ( ty, te, tg, X86G_CC_OP_SMULB );

   assign( resLo, binop( mkSizedOp(ty, Iop_Mul8), mkexpr(te), mkexpr(tg) ) );

   putIReg(size, gregOfRM(rm), mkexpr(resLo) );

   if (epartIsReg(rm)) {
      DIP("imul%c %s, %s\n", nameISize(size), 
                             nameIReg(size,eregOfRM(rm)),
                             nameIReg(size,gregOfRM(rm)));
      return 1+delta0;
   } else {
      DIP("imul%c %s, %s\n", nameISize(size), 
                             dis_buf, nameIReg(size,gregOfRM(rm)));
      return alen+delta0;
   }
}


/* IMUL I * E -> G.  Supplied eip points to the modR/M byte. */
static
UInt dis_imul_I_E_G ( UChar       sorb,
                      Int         size, 
                      Int         delta,
                      Int         litsize )
{
   Int    d32, alen;
   HChar  dis_buf[50];
   UChar  rm = getIByte(delta);
   IRType ty = szToITy(size);
   IRTemp te = newTemp(ty);
   IRTemp tl = newTemp(ty);
   IRTemp resLo = newTemp(ty);

   vassert(size == 1 || size == 2 || size == 4);

   if (epartIsReg(rm)) {
      assign(te, getIReg(size, eregOfRM(rm)));
      delta++;
   } else {
      IRTemp addr = disAMode( &alen, sorb, delta, dis_buf );
      assign(te, loadLE(ty, mkexpr(addr)));
      delta += alen;
   }
   d32 = getSDisp(litsize,delta);
   delta += litsize;

   if (size == 1) d32 &= 0xFF;
   if (size == 2) d32 &= 0xFFFF;

   assign(tl, mkU(ty,d32));

   assign( resLo, binop( mkSizedOp(ty, Iop_Mul8), mkexpr(te), mkexpr(tl) ));

   setFlags_MUL ( ty, te, tl, X86G_CC_OP_SMULB );

   putIReg(size, gregOfRM(rm), mkexpr(resLo));

   DIP("imul %d, %s, %s\n", d32, 
       ( epartIsReg(rm) ? nameIReg(size,eregOfRM(rm)) : dis_buf ),
       nameIReg(size,gregOfRM(rm)) );
   return delta;
}


/* Generate an IR sequence to do a count-leading-zeroes operation on
   the supplied IRTemp, and return a new IRTemp holding the result.
   'ty' may be Ity_I16 or Ity_I32 only.  In the case where the
   argument is zero, return the number of bits in the word (the
   natural semantics). */
static IRTemp gen_LZCNT ( IRType ty, IRTemp src )
{
   vassert(ty == Ity_I32 || ty == Ity_I16);

   IRTemp src32 = newTemp(Ity_I32);
   assign(src32, widenUto32( mkexpr(src) ));

   IRTemp src32x = newTemp(Ity_I32);
   assign(src32x, 
          binop(Iop_Shl32, mkexpr(src32),
                           mkU8(32 - 8 * sizeofIRType(ty))));

   // Clz32 has undefined semantics when its input is zero, so
   // special-case around that.
   IRTemp res32 = newTemp(Ity_I32);
   assign(res32,
          IRExpr_ITE(
             binop(Iop_CmpEQ32, mkexpr(src32x), mkU32(0)),
             mkU32(8 * sizeofIRType(ty)),
             unop(Iop_Clz32, mkexpr(src32x))
   ));

   IRTemp res = newTemp(ty);
   assign(res, narrowTo(ty, mkexpr(res32)));
   return res;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- x87 FLOATING POINT INSTRUCTIONS                      ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- Helper functions for dealing with the register stack. --- */

/* --- Set the emulation-warning pseudo-register. --- */

static void put_emwarn ( IRExpr* e /* :: Ity_I32 */ )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I32);
   stmt( IRStmt_Put( OFFB_EMNOTE, e ) );
}

/* --- Produce an IRExpr* denoting a 64-bit QNaN. --- */

static IRExpr* mkQNaN64 ( void )
{
  /* QNaN is 0 2047 1 0(51times) 
     == 0b 11111111111b 1 0(51times)
     == 0x7FF8 0000 0000 0000
   */
   return IRExpr_Const(IRConst_F64i(0x7FF8000000000000ULL));
}

/* --------- Get/put the top-of-stack pointer. --------- */

static IRExpr* get_ftop ( void )
{
   return IRExpr_Get( OFFB_FTOP, Ity_I32 );
}

static void put_ftop ( IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I32);
   stmt( IRStmt_Put( OFFB_FTOP, e ) );
}

/* --------- Get/put the C3210 bits. --------- */

static IRExpr* get_C3210 ( void )
{
   return IRExpr_Get( OFFB_FC3210, Ity_I32 );
}

static void put_C3210 ( IRExpr* e )
{
   stmt( IRStmt_Put( OFFB_FC3210, e ) );
}

/* --------- Get/put the FPU rounding mode. --------- */
static IRExpr* /* :: Ity_I32 */ get_fpround ( void )
{
   return IRExpr_Get( OFFB_FPROUND, Ity_I32 );
}

static void put_fpround ( IRExpr* /* :: Ity_I32 */ e )
{
   stmt( IRStmt_Put( OFFB_FPROUND, e ) );
}


/* --------- Synthesise a 2-bit FPU rounding mode. --------- */
/* Produces a value in 0 .. 3, which is encoded as per the type
   IRRoundingMode.  Since the guest_FPROUND value is also encoded as
   per IRRoundingMode, we merely need to get it and mask it for
   safety.
*/
static IRExpr* /* :: Ity_I32 */ get_roundingmode ( void )
{
   return binop( Iop_And32, get_fpround(), mkU32(3) );
}

static IRExpr* /* :: Ity_I32 */ get_FAKE_roundingmode ( void )
{
   return mkU32(Irrm_NEAREST);
}


/* --------- Get/set FP register tag bytes. --------- */

/* Given i, and some expression e, generate 'ST_TAG(i) = e'. */

static void put_ST_TAG ( Int i, IRExpr* value )
{
   IRRegArray* descr;
   vassert(typeOfIRExpr(irsb->tyenv, value) == Ity_I8);
   descr = mkIRRegArray( OFFB_FPTAGS, Ity_I8, 8 );
   stmt( IRStmt_PutI( mkIRPutI(descr, get_ftop(), i, value) ) );
}

/* Given i, generate an expression yielding 'ST_TAG(i)'.  This will be
   zero to indicate "Empty" and nonzero to indicate "NonEmpty".  */

static IRExpr* get_ST_TAG ( Int i )
{
   IRRegArray* descr = mkIRRegArray( OFFB_FPTAGS, Ity_I8, 8 );
   return IRExpr_GetI( descr, get_ftop(), i );
}


/* --------- Get/set FP registers. --------- */

/* Given i, and some expression e, emit 'ST(i) = e' and set the
   register's tag to indicate the register is full.  The previous
   state of the register is not checked. */

static void put_ST_UNCHECKED ( Int i, IRExpr* value )
{
   IRRegArray* descr;
   vassert(typeOfIRExpr(irsb->tyenv, value) == Ity_F64);
   descr = mkIRRegArray( OFFB_FPREGS, Ity_F64, 8 );
   stmt( IRStmt_PutI( mkIRPutI(descr, get_ftop(), i, value) ) );
   /* Mark the register as in-use. */
   put_ST_TAG(i, mkU8(1));
}

/* Given i, and some expression e, emit
      ST(i) = is_full(i) ? NaN : e
   and set the tag accordingly.
*/

static void put_ST ( Int i, IRExpr* value )
{
   put_ST_UNCHECKED(
      i,
      IRExpr_ITE( binop(Iop_CmpNE8, get_ST_TAG(i), mkU8(0)),
                  /* non-0 means full */
                  mkQNaN64(),
                  /* 0 means empty */
                  value
      )
   );
}


/* Given i, generate an expression yielding 'ST(i)'. */

static IRExpr* get_ST_UNCHECKED ( Int i )
{
   IRRegArray* descr = mkIRRegArray( OFFB_FPREGS, Ity_F64, 8 );
   return IRExpr_GetI( descr, get_ftop(), i );
}


/* Given i, generate an expression yielding 
  is_full(i) ? ST(i) : NaN
*/

static IRExpr* get_ST ( Int i )
{
   return
      IRExpr_ITE( binop(Iop_CmpNE8, get_ST_TAG(i), mkU8(0)),
                  /* non-0 means full */
                  get_ST_UNCHECKED(i),
                  /* 0 means empty */
                  mkQNaN64());
}


/* Given i, and some expression e, and a condition cond, generate IR
   which has the same effect as put_ST(i,e) when cond is true and has
   no effect when cond is false.  Given the lack of proper
   if-then-else in the IR, this is pretty tricky.
*/

static void maybe_put_ST ( IRTemp cond, Int i, IRExpr* value )
{
   // new_tag = if cond then FULL else old_tag
   // new_val = if cond then (if old_tag==FULL then NaN else val)
   //                   else old_val

   IRTemp old_tag = newTemp(Ity_I8);
   assign(old_tag, get_ST_TAG(i));
   IRTemp new_tag = newTemp(Ity_I8);
   assign(new_tag,
          IRExpr_ITE(mkexpr(cond), mkU8(1)/*FULL*/, mkexpr(old_tag)));

   IRTemp old_val = newTemp(Ity_F64);
   assign(old_val, get_ST_UNCHECKED(i));
   IRTemp new_val = newTemp(Ity_F64);
   assign(new_val,
          IRExpr_ITE(mkexpr(cond),
                     IRExpr_ITE(binop(Iop_CmpNE8, mkexpr(old_tag), mkU8(0)),
                                /* non-0 means full */
                                mkQNaN64(),
                                /* 0 means empty */
                                value),
                     mkexpr(old_val)));

   put_ST_UNCHECKED(i, mkexpr(new_val));
   // put_ST_UNCHECKED incorrectly sets tag(i) to always be FULL.  So 
   // now set it to new_tag instead.
   put_ST_TAG(i, mkexpr(new_tag));
}

/* Adjust FTOP downwards by one register. */

static void fp_push ( void )
{
   put_ftop( binop(Iop_Sub32, get_ftop(), mkU32(1)) );
}

/* Adjust FTOP downwards by one register when COND is 1:I1.  Else
   don't change it. */

static void maybe_fp_push ( IRTemp cond )
{
   put_ftop( binop(Iop_Sub32, get_ftop(), unop(Iop_1Uto32,mkexpr(cond))) );
}

/* Adjust FTOP upwards by one register, and mark the vacated register
   as empty.  */

static void fp_pop ( void )
{
   put_ST_TAG(0, mkU8(0));
   put_ftop( binop(Iop_Add32, get_ftop(), mkU32(1)) );
}

/* Set the C2 bit of the FPU status register to e[0].  Assumes that
   e[31:1] == 0. 
*/
static void set_C2 ( IRExpr* e )
{
   IRExpr* cleared = binop(Iop_And32, get_C3210(), mkU32(~X86G_FC_MASK_C2));
   put_C3210( binop(Iop_Or32,
                    cleared,
                    binop(Iop_Shl32, e, mkU8(X86G_FC_SHIFT_C2))) );
}

/* Generate code to check that abs(d64) < 2^63 and is finite.  This is
   used to do the range checks for FSIN, FCOS, FSINCOS and FPTAN.  The
   test is simple, but the derivation of it is not so simple.

   The exponent field for an IEEE754 double is 11 bits.  That means it
   can take values 0 through 0x7FF.  If the exponent has value 0x7FF,
   the number is either a NaN or an Infinity and so is not finite.
   Furthermore, a finite value of exactly 2^63 is the smallest value
   that has exponent value 0x43E.  Hence, what we need to do is
   extract the exponent, ignoring the sign bit and mantissa, and check
   it is < 0x43E, or <= 0x43D.

   To make this easily applicable to 32- and 64-bit targets, a
   roundabout approach is used.  First the number is converted to I64,
   then the top 32 bits are taken.  Shifting them right by 20 bits
   places the sign bit and exponent in the bottom 12 bits.  Anding
   with 0x7FF gets rid of the sign bit, leaving just the exponent
   available for comparison.
*/
static IRTemp math_IS_TRIG_ARG_FINITE_AND_IN_RANGE ( IRTemp d64 )
{
   IRTemp i64 = newTemp(Ity_I64);
   assign(i64, unop(Iop_ReinterpF64asI64, mkexpr(d64)) );
   IRTemp exponent = newTemp(Ity_I32);
   assign(exponent,
          binop(Iop_And32,
                binop(Iop_Shr32, unop(Iop_64HIto32, mkexpr(i64)), mkU8(20)),
                mkU32(0x7FF)));
   IRTemp in_range_and_finite = newTemp(Ity_I1);
   assign(in_range_and_finite,
          binop(Iop_CmpLE32U, mkexpr(exponent), mkU32(0x43D)));
   return in_range_and_finite;
}

/* Invent a plausible-looking FPU status word value:
      ((ftop & 7) << 11) | (c3210 & 0x4700)
 */
static IRExpr* get_FPU_sw ( void )
{
   return
      unop(Iop_32to16,
           binop(Iop_Or32,
                 binop(Iop_Shl32, 
                       binop(Iop_And32, get_ftop(), mkU32(7)), 
                             mkU8(11)),
                       binop(Iop_And32, get_C3210(), mkU32(0x4700))
      ));
}


/* ------------------------------------------------------- */
/* Given all that stack-mangling junk, we can now go ahead
   and describe FP instructions. 
*/

/* ST(0) = ST(0) `op` mem64/32(addr)
   Need to check ST(0)'s tag on read, but not on write.
*/
static
void fp_do_op_mem_ST_0 ( IRTemp addr, const HChar* op_txt, HChar* dis_buf, 
                         IROp op, Bool dbl )
{
   DIP("f%s%c %s\n", op_txt, dbl?'l':'s', dis_buf);
   if (dbl) {
      put_ST_UNCHECKED(0, 
         triop( op, 
                get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                get_ST(0), 
                loadLE(Ity_F64,mkexpr(addr))
         ));
   } else {
      put_ST_UNCHECKED(0, 
         triop( op, 
                get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                get_ST(0), 
                unop(Iop_F32toF64, loadLE(Ity_F32,mkexpr(addr)))
         ));
   }
}


/* ST(0) = mem64/32(addr) `op` ST(0)
   Need to check ST(0)'s tag on read, but not on write.
*/
static
void fp_do_oprev_mem_ST_0 ( IRTemp addr, const HChar* op_txt, HChar* dis_buf,
                            IROp op, Bool dbl )
{
   DIP("f%s%c %s\n", op_txt, dbl?'l':'s', dis_buf);
   if (dbl) {
      put_ST_UNCHECKED(0, 
         triop( op, 
                get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                loadLE(Ity_F64,mkexpr(addr)),
                get_ST(0)
         ));
   } else {
      put_ST_UNCHECKED(0, 
         triop( op, 
                get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                unop(Iop_F32toF64, loadLE(Ity_F32,mkexpr(addr))),
                get_ST(0)
         ));
   }
}


/* ST(dst) = ST(dst) `op` ST(src).
   Check dst and src tags when reading but not on write.
*/
static
void fp_do_op_ST_ST ( const HChar* op_txt, IROp op, UInt st_src, UInt st_dst,
                      Bool pop_after )
{
   DIP("f%s%s st(%d), st(%d)\n", op_txt, pop_after?"p":"", 
                                 (Int)st_src, (Int)st_dst );
   put_ST_UNCHECKED( 
      st_dst, 
      triop( op, 
             get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
             get_ST(st_dst), 
             get_ST(st_src) ) 
   );
   if (pop_after)
      fp_pop();
}

/* ST(dst) = ST(src) `op` ST(dst).
   Check dst and src tags when reading but not on write.
*/
static
void fp_do_oprev_ST_ST ( const HChar* op_txt, IROp op, UInt st_src,
                         UInt st_dst, Bool pop_after )
{
   DIP("f%s%s st(%d), st(%d)\n", op_txt, pop_after?"p":"",
                                 (Int)st_src, (Int)st_dst );
   put_ST_UNCHECKED( 
      st_dst, 
      triop( op, 
             get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
             get_ST(st_src), 
             get_ST(st_dst) ) 
   );
   if (pop_after)
      fp_pop();
}

/* %eflags(Z,P,C) = UCOMI( st(0), st(i) ) */
static void fp_do_ucomi_ST0_STi ( UInt i, Bool pop_after )
{
   DIP("fucomi%s %%st(0),%%st(%d)\n", pop_after ? "p" : "", (Int)i );
   /* This is a bit of a hack (and isn't really right).  It sets
      Z,P,C,O correctly, but forces A and S to zero, whereas the Intel
      documentation implies A and S are unchanged. 
   */
   /* It's also fishy in that it is used both for COMIP and
      UCOMIP, and they aren't the same (although similar). */
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
   stmt( IRStmt_Put( OFFB_CC_DEP1,
                     binop( Iop_And32,
                            binop(Iop_CmpF64, get_ST(0), get_ST(i)),
                            mkU32(0x45)
       )));
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
   if (pop_after)
      fp_pop();
}


static
UInt dis_FPU ( Bool* decode_ok, UChar sorb, Int delta )
{
   Int    len;
   UInt   r_src, r_dst;
   HChar  dis_buf[50];
   IRTemp t1, t2;

   /* On entry, delta points at the second byte of the insn (the modrm
      byte).*/
   UChar first_opcode = getIByte(delta-1);
   UChar modrm        = getIByte(delta+0);

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xD8 opcodes +-+-+-+-+-+-+-+ */

   if (first_opcode == 0xD8) {
      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
           specifies an address. */
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FADD single-real */
               fp_do_op_mem_ST_0 ( addr, "add", dis_buf, Iop_AddF64, False );
               break;

            case 1: /* FMUL single-real */
               fp_do_op_mem_ST_0 ( addr, "mul", dis_buf, Iop_MulF64, False );
               break;

            case 2: /* FCOM single-real */
               DIP("fcoms %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      unop(Iop_F32toF64, 
                                           loadLE(Ity_F32,mkexpr(addr)))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;  

            case 3: /* FCOMP single-real */
               DIP("fcomps %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      unop(Iop_F32toF64, 
                                           loadLE(Ity_F32,mkexpr(addr)))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               break;  

            case 4: /* FSUB single-real */
               fp_do_op_mem_ST_0 ( addr, "sub", dis_buf, Iop_SubF64, False );
               break;

            case 5: /* FSUBR single-real */
               fp_do_oprev_mem_ST_0 ( addr, "subr", dis_buf, Iop_SubF64, False );
               break;

            case 6: /* FDIV single-real */
               fp_do_op_mem_ST_0 ( addr, "div", dis_buf, Iop_DivF64, False );
               break;

            case 7: /* FDIVR single-real */
               fp_do_oprev_mem_ST_0 ( addr, "divr", dis_buf, Iop_DivF64, False );
               break;

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xD8\n");
               goto decode_fail;
         }
      } else {
         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FADD %st(?),%st(0) */
               fp_do_op_ST_ST ( "add", Iop_AddF64, modrm - 0xC0, 0, False );
               break;

            case 0xC8 ... 0xCF: /* FMUL %st(?),%st(0) */
               fp_do_op_ST_ST ( "mul", Iop_MulF64, modrm - 0xC8, 0, False );
               break;

            /* Dunno if this is right */
            case 0xD0 ... 0xD7: /* FCOM %st(?),%st(0) */
               r_dst = (UInt)modrm - 0xD0;
               DIP("fcom %%st(0),%%st(%d)\n", (Int)r_dst);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, get_ST(0), get_ST(r_dst)),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;

            /* Dunno if this is right */
            case 0xD8 ... 0xDF: /* FCOMP %st(?),%st(0) */
               r_dst = (UInt)modrm - 0xD8;
               DIP("fcomp %%st(0),%%st(%d)\n", (Int)r_dst);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, get_ST(0), get_ST(r_dst)),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               break;

            case 0xE0 ... 0xE7: /* FSUB %st(?),%st(0) */
               fp_do_op_ST_ST ( "sub", Iop_SubF64, modrm - 0xE0, 0, False );
               break;

            case 0xE8 ... 0xEF: /* FSUBR %st(?),%st(0) */
               fp_do_oprev_ST_ST ( "subr", Iop_SubF64, modrm - 0xE8, 0, False );
               break;

            case 0xF0 ... 0xF7: /* FDIV %st(?),%st(0) */
               fp_do_op_ST_ST ( "div", Iop_DivF64, modrm - 0xF0, 0, False );
               break;

            case 0xF8 ... 0xFF: /* FDIVR %st(?),%st(0) */
               fp_do_oprev_ST_ST ( "divr", Iop_DivF64, modrm - 0xF8, 0, False );
               break;

            default:
               goto decode_fail;
         }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xD9 opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xD9) {
      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FLD single-real */
               DIP("flds %s\n", dis_buf);
               fp_push();
               put_ST(0, unop(Iop_F32toF64,
                              loadLE(Ity_F32, mkexpr(addr))));
               break;

            case 2: /* FST single-real */
               DIP("fsts %s\n", dis_buf);
               storeLE(mkexpr(addr),
                       binop(Iop_F64toF32, get_roundingmode(), get_ST(0)));
               break;

            case 3: /* FSTP single-real */
               DIP("fstps %s\n", dis_buf);
               storeLE(mkexpr(addr), 
                       binop(Iop_F64toF32, get_roundingmode(), get_ST(0)));
               fp_pop();
               break;

            case 4: { /* FLDENV m28 */
               /* Uses dirty helper: 
                     VexEmNote x86g_do_FLDENV ( VexGuestX86State*, HWord ) */
               IRTemp   ew = newTemp(Ity_I32);
               IRDirty* d  = unsafeIRDirty_0_N ( 
                                0/*regparms*/, 
                                "x86g_dirtyhelper_FLDENV", 
                                &x86g_dirtyhelper_FLDENV,
                                mkIRExprVec_2( IRExpr_BBPTR(), mkexpr(addr) )
                             );
               d->tmp   = ew;
               /* declare we're reading memory */
               d->mFx   = Ifx_Read;
               d->mAddr = mkexpr(addr);
               d->mSize = 28;

               /* declare we're writing guest state */
               d->nFxState = 4;
               vex_bzero(&d->fxState, sizeof(d->fxState));

               d->fxState[0].fx     = Ifx_Write;
               d->fxState[0].offset = OFFB_FTOP;
               d->fxState[0].size   = sizeof(UInt);

               d->fxState[1].fx     = Ifx_Write;
               d->fxState[1].offset = OFFB_FPTAGS;
               d->fxState[1].size   = 8 * sizeof(UChar);

               d->fxState[2].fx     = Ifx_Write;
               d->fxState[2].offset = OFFB_FPROUND;
               d->fxState[2].size   = sizeof(UInt);

               d->fxState[3].fx     = Ifx_Write;
               d->fxState[3].offset = OFFB_FC3210;
               d->fxState[3].size   = sizeof(UInt);

               stmt( IRStmt_Dirty(d) );

               /* ew contains any emulation warning we may need to
                  issue.  If needed, side-exit to the next insn,
                  reporting the warning, so that Valgrind's dispatcher
                  sees the warning. */
               put_emwarn( mkexpr(ew) );
               stmt( 
                  IRStmt_Exit(
                     binop(Iop_CmpNE32, mkexpr(ew), mkU32(0)),
                     Ijk_EmWarn,
                     IRConst_U32( ((Addr32)guest_EIP_bbstart)+delta),
                     OFFB_EIP
                  )
               );

               DIP("fldenv %s\n", dis_buf);
               break;
            }

            case 5: {/* FLDCW */
               /* The only thing we observe in the control word is the
                  rounding mode.  Therefore, pass the 16-bit value
                  (x87 native-format control word) to a clean helper,
                  getting back a 64-bit value, the lower half of which
                  is the FPROUND value to store, and the upper half of
                  which is the emulation-warning token which may be
                  generated.
               */
               /* ULong x86h_check_fldcw ( UInt ); */
               IRTemp t64 = newTemp(Ity_I64);
               IRTemp ew = newTemp(Ity_I32);
               DIP("fldcw %s\n", dis_buf);
               assign( t64, mkIRExprCCall(
                               Ity_I64, 0/*regparms*/, 
                               "x86g_check_fldcw",
                               &x86g_check_fldcw, 
                               mkIRExprVec_1( 
                                  unop( Iop_16Uto32, 
                                        loadLE(Ity_I16, mkexpr(addr)))
                               )
                            )
                     );

               put_fpround( unop(Iop_64to32, mkexpr(t64)) );
               assign( ew, unop(Iop_64HIto32, mkexpr(t64) ) );
               put_emwarn( mkexpr(ew) );
               /* Finally, if an emulation warning was reported,
                  side-exit to the next insn, reporting the warning,
                  so that Valgrind's dispatcher sees the warning. */
               stmt( 
                  IRStmt_Exit(
                     binop(Iop_CmpNE32, mkexpr(ew), mkU32(0)),
                     Ijk_EmWarn,
                     IRConst_U32( ((Addr32)guest_EIP_bbstart)+delta),
                     OFFB_EIP
                  )
               );
               break;
            }

            case 6: { /* FNSTENV m28 */
               /* Uses dirty helper: 
                     void x86g_do_FSTENV ( VexGuestX86State*, HWord ) */
               IRDirty* d = unsafeIRDirty_0_N ( 
                               0/*regparms*/, 
                               "x86g_dirtyhelper_FSTENV", 
                               &x86g_dirtyhelper_FSTENV,
                               mkIRExprVec_2( IRExpr_BBPTR(), mkexpr(addr) )
                            );
               /* declare we're writing memory */
               d->mFx   = Ifx_Write;
               d->mAddr = mkexpr(addr);
               d->mSize = 28;

               /* declare we're reading guest state */
               d->nFxState = 4;
               vex_bzero(&d->fxState, sizeof(d->fxState));

               d->fxState[0].fx     = Ifx_Read;
               d->fxState[0].offset = OFFB_FTOP;
               d->fxState[0].size   = sizeof(UInt);

               d->fxState[1].fx     = Ifx_Read;
               d->fxState[1].offset = OFFB_FPTAGS;
               d->fxState[1].size   = 8 * sizeof(UChar);

               d->fxState[2].fx     = Ifx_Read;
               d->fxState[2].offset = OFFB_FPROUND;
               d->fxState[2].size   = sizeof(UInt);

               d->fxState[3].fx     = Ifx_Read;
               d->fxState[3].offset = OFFB_FC3210;
               d->fxState[3].size   = sizeof(UInt);

               stmt( IRStmt_Dirty(d) );

               DIP("fnstenv %s\n", dis_buf);
               break;
            }

            case 7: /* FNSTCW */
              /* Fake up a native x87 FPU control word.  The only
                 thing it depends on is FPROUND[1:0], so call a clean
                 helper to cook it up. */
               /* UInt x86h_create_fpucw ( UInt fpround ) */
               DIP("fnstcw %s\n", dis_buf);
               storeLE(
                  mkexpr(addr), 
                  unop( Iop_32to16, 
                        mkIRExprCCall(
                           Ity_I32, 0/*regp*/,
                           "x86g_create_fpucw", &x86g_create_fpucw, 
                           mkIRExprVec_1( get_fpround() ) 
                        ) 
                  ) 
               );
               break;

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xD9\n");
               goto decode_fail;
         }

      } else {
         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FLD %st(?) */
               r_src = (UInt)modrm - 0xC0;
               DIP("fld %%st(%d)\n", (Int)r_src);
               t1 = newTemp(Ity_F64);
               assign(t1, get_ST(r_src));
               fp_push();
               put_ST(0, mkexpr(t1));
               break;

            case 0xC8 ... 0xCF: /* FXCH %st(?) */
               r_src = (UInt)modrm - 0xC8;
               DIP("fxch %%st(%d)\n", (Int)r_src);
               t1 = newTemp(Ity_F64);
               t2 = newTemp(Ity_F64);
               assign(t1, get_ST(0));
               assign(t2, get_ST(r_src));
               put_ST_UNCHECKED(0, mkexpr(t2));
               put_ST_UNCHECKED(r_src, mkexpr(t1));
               break;

            case 0xE0: /* FCHS */
               DIP("fchs\n");
               put_ST_UNCHECKED(0, unop(Iop_NegF64, get_ST(0)));
               break;

            case 0xE1: /* FABS */
               DIP("fabs\n");
               put_ST_UNCHECKED(0, unop(Iop_AbsF64, get_ST(0)));
               break;

            case 0xE4: /* FTST */
               DIP("ftst\n");
               /* This forces C1 to zero, which isn't right. */
               /* Well, in fact the Intel docs say (bizarrely): "C1 is
                  set to 0 if stack underflow occurred; otherwise, set
                  to 0" which is pretty nonsensical.  I guess it's a
                   typo. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      IRExpr_Const(IRConst_F64i(0x0ULL))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;

            case 0xE5: { /* FXAM */
               /* This is an interesting one.  It examines %st(0),
                  regardless of whether the tag says it's empty or not.
                  Here, just pass both the tag (in our format) and the
                  value (as a double, actually a ULong) to a helper
                  function. */
               IRExpr** args
                  = mkIRExprVec_2( unop(Iop_8Uto32, get_ST_TAG(0)),
                                   unop(Iop_ReinterpF64asI64, 
                                        get_ST_UNCHECKED(0)) );
               put_C3210(mkIRExprCCall(
                            Ity_I32, 
                            0/*regparm*/, 
                            "x86g_calculate_FXAM", &x86g_calculate_FXAM,
                            args
                        ));
               DIP("fxam\n");
               break;
            }

            case 0xE8: /* FLD1 */
               DIP("fld1\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(1.0))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x3ff0000000000000ULL)));
               break;

            case 0xE9: /* FLDL2T */
               DIP("fldl2t\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(3.32192809488736234781))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x400a934f0979a371ULL)));
               break;

            case 0xEA: /* FLDL2E */
               DIP("fldl2e\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(1.44269504088896340739))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x3ff71547652b82feULL)));
               break;

            case 0xEB: /* FLDPI */
               DIP("fldpi\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(3.14159265358979323851))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x400921fb54442d18ULL)));
               break;

            case 0xEC: /* FLDLG2 */
               DIP("fldlg2\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(0.301029995663981143))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x3fd34413509f79ffULL)));
               break;

            case 0xED: /* FLDLN2 */
               DIP("fldln2\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(0.69314718055994530942))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x3fe62e42fefa39efULL)));
               break;

            case 0xEE: /* FLDZ */
               DIP("fldz\n");
               fp_push();
               /* put_ST(0, IRExpr_Const(IRConst_F64(0.0))); */
               put_ST(0, IRExpr_Const(IRConst_F64i(0x0000000000000000ULL)));
               break;

            case 0xF0: /* F2XM1 */
               DIP("f2xm1\n");
               put_ST_UNCHECKED(0, 
                  binop(Iop_2xm1F64, 
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(0)));
               break;

            case 0xF1: /* FYL2X */
               DIP("fyl2x\n");
               put_ST_UNCHECKED(1, 
                  triop(Iop_Yl2xF64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(1), 
                        get_ST(0)));
               fp_pop();
               break;

            case 0xF2: { /* FPTAN */
               DIP("fptan\n");
               IRTemp argD = newTemp(Ity_F64);
               assign(argD, get_ST(0));
               IRTemp argOK = math_IS_TRIG_ARG_FINITE_AND_IN_RANGE(argD);
               IRTemp resD = newTemp(Ity_F64);
               assign(resD,
                  IRExpr_ITE(
                     mkexpr(argOK), 
                     binop(Iop_TanF64,
                           get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                           mkexpr(argD)),
                     mkexpr(argD))
               );
               put_ST_UNCHECKED(0, mkexpr(resD));
               /* Conditionally push 1.0 on the stack, if the arg is
                  in range */
               maybe_fp_push(argOK);
               maybe_put_ST(argOK, 0,
                            IRExpr_Const(IRConst_F64(1.0)));
               set_C2( binop(Iop_Xor32,
                             unop(Iop_1Uto32, mkexpr(argOK)), 
                             mkU32(1)) );
               break;
            }

            case 0xF3: /* FPATAN */
               DIP("fpatan\n");
               put_ST_UNCHECKED(1, 
                  triop(Iop_AtanF64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(1), 
                        get_ST(0)));
               fp_pop();
               break;

            case 0xF4: { /* FXTRACT */
               IRTemp argF = newTemp(Ity_F64);
               IRTemp sigF = newTemp(Ity_F64);
               IRTemp expF = newTemp(Ity_F64);
               IRTemp argI = newTemp(Ity_I64);
               IRTemp sigI = newTemp(Ity_I64);
               IRTemp expI = newTemp(Ity_I64);
               DIP("fxtract\n");
               assign( argF, get_ST(0) );
               assign( argI, unop(Iop_ReinterpF64asI64, mkexpr(argF)));
               assign( sigI, 
                       mkIRExprCCall(
                          Ity_I64, 0/*regparms*/, 
                          "x86amd64g_calculate_FXTRACT", 
                          &x86amd64g_calculate_FXTRACT, 
                          mkIRExprVec_2( mkexpr(argI), 
                                         mkIRExpr_HWord(0)/*sig*/ )) 
               );
               assign( expI, 
                       mkIRExprCCall(
                          Ity_I64, 0/*regparms*/, 
                          "x86amd64g_calculate_FXTRACT", 
                          &x86amd64g_calculate_FXTRACT, 
                          mkIRExprVec_2( mkexpr(argI), 
                                         mkIRExpr_HWord(1)/*exp*/ )) 
               );
               assign( sigF, unop(Iop_ReinterpI64asF64, mkexpr(sigI)) );
               assign( expF, unop(Iop_ReinterpI64asF64, mkexpr(expI)) );
               /* exponent */
               put_ST_UNCHECKED(0, mkexpr(expF) );
               fp_push();
               /* significand */
               put_ST(0, mkexpr(sigF) );
               break;
            }

            case 0xF5: { /* FPREM1 -- IEEE compliant */
               IRTemp a1 = newTemp(Ity_F64);
               IRTemp a2 = newTemp(Ity_F64);
               DIP("fprem1\n");
               /* Do FPREM1 twice, once to get the remainder, and once
                  to get the C3210 flag values. */
               assign( a1, get_ST(0) );
               assign( a2, get_ST(1) );
               put_ST_UNCHECKED(0, 
                  triop(Iop_PRem1F64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        mkexpr(a1), 
                        mkexpr(a2)));
               put_C3210(
                  triop(Iop_PRem1C3210F64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        mkexpr(a1), 
                        mkexpr(a2)) );
               break;
            }

            case 0xF7: /* FINCSTP */
               DIP("fprem\n");
               put_ftop( binop(Iop_Add32, get_ftop(), mkU32(1)) );
               break;

            case 0xF8: { /* FPREM -- not IEEE compliant */
               IRTemp a1 = newTemp(Ity_F64);
               IRTemp a2 = newTemp(Ity_F64);
               DIP("fprem\n");
               /* Do FPREM twice, once to get the remainder, and once
                  to get the C3210 flag values. */
               assign( a1, get_ST(0) );
               assign( a2, get_ST(1) );
               put_ST_UNCHECKED(0, 
                  triop(Iop_PRemF64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        mkexpr(a1), 
                        mkexpr(a2)));
               put_C3210( 
                  triop(Iop_PRemC3210F64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        mkexpr(a1), 
                        mkexpr(a2)) );
               break;
            }

            case 0xF9: /* FYL2XP1 */
               DIP("fyl2xp1\n");
               put_ST_UNCHECKED(1, 
                  triop(Iop_Yl2xp1F64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(1), 
                        get_ST(0)));
               fp_pop();
               break;

            case 0xFA: /* FSQRT */
               DIP("fsqrt\n");
               put_ST_UNCHECKED(0, 
                  binop(Iop_SqrtF64, 
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(0)));
               break;

            case 0xFB: { /* FSINCOS */
               DIP("fsincos\n");
               IRTemp argD = newTemp(Ity_F64);
               assign(argD, get_ST(0));
               IRTemp argOK = math_IS_TRIG_ARG_FINITE_AND_IN_RANGE(argD);
               IRTemp resD = newTemp(Ity_F64);
               assign(resD,
                  IRExpr_ITE(
                     mkexpr(argOK), 
                     binop(Iop_SinF64,
                           get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                           mkexpr(argD)),
                     mkexpr(argD))
               );
               put_ST_UNCHECKED(0, mkexpr(resD));
               /* Conditionally push the cos value on the stack, if
                  the arg is in range */
               maybe_fp_push(argOK);
               maybe_put_ST(argOK, 0,
                  binop(Iop_CosF64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        mkexpr(argD)));
               set_C2( binop(Iop_Xor32,
                             unop(Iop_1Uto32, mkexpr(argOK)), 
                             mkU32(1)) );
               break;
            }

            case 0xFC: /* FRNDINT */
               DIP("frndint\n");
               put_ST_UNCHECKED(0,
                  binop(Iop_RoundF64toInt, get_roundingmode(), get_ST(0)) );
               break;

            case 0xFD: /* FSCALE */
               DIP("fscale\n");
               put_ST_UNCHECKED(0, 
                  triop(Iop_ScaleF64,
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(0), 
                        get_ST(1)));
               break;

            case 0xFE:   /* FSIN */
            case 0xFF: { /* FCOS */
               Bool isSIN = modrm == 0xFE;
               DIP("%s\n", isSIN ? "fsin" : "fcos");
               IRTemp argD = newTemp(Ity_F64);
               assign(argD, get_ST(0));
               IRTemp argOK = math_IS_TRIG_ARG_FINITE_AND_IN_RANGE(argD);
               IRTemp resD = newTemp(Ity_F64);
               assign(resD,
                  IRExpr_ITE(
                     mkexpr(argOK), 
                     binop(isSIN ? Iop_SinF64 : Iop_CosF64,
                           get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                           mkexpr(argD)),
                     mkexpr(argD))
               );
               put_ST_UNCHECKED(0, mkexpr(resD));
               set_C2( binop(Iop_Xor32,
                             unop(Iop_1Uto32, mkexpr(argOK)), 
                             mkU32(1)) );
               break;
            }

            default:
               goto decode_fail;
         }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDA opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDA) {

      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IROp   fop;
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;
         switch (gregOfRM(modrm)) {

            case 0: /* FIADD m32int */ /* ST(0) += m32int */
               DIP("fiaddl %s\n", dis_buf);
               fop = Iop_AddF64;
               goto do_fop_m32;

            case 1: /* FIMUL m32int */ /* ST(0) *= m32int */
               DIP("fimull %s\n", dis_buf);
               fop = Iop_MulF64;
               goto do_fop_m32;

            case 2: /* FICOM m32int */
               DIP("ficoml %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      unop(Iop_I32StoF64, 
                                           loadLE(Ity_I32,mkexpr(addr)))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;

            case 3: /* FICOMP m32int */
               DIP("ficompl %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      unop(Iop_I32StoF64, 
                                           loadLE(Ity_I32,mkexpr(addr)))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               break;

            case 4: /* FISUB m32int */ /* ST(0) -= m32int */
               DIP("fisubl %s\n", dis_buf);
               fop = Iop_SubF64;
               goto do_fop_m32;

            case 5: /* FISUBR m32int */ /* ST(0) = m32int - ST(0) */
               DIP("fisubrl %s\n", dis_buf);
               fop = Iop_SubF64;
               goto do_foprev_m32;

            case 6: /* FIDIV m32int */ /* ST(0) /= m32int */
               DIP("fidivl %s\n", dis_buf);
               fop = Iop_DivF64;
               goto do_fop_m32;

            case 7: /* FIDIVR m32int */ /* ST(0) = m32int / ST(0) */
               DIP("fidivrl %s\n", dis_buf);
               fop = Iop_DivF64;
               goto do_foprev_m32;

            do_fop_m32:
               put_ST_UNCHECKED(0, 
                  triop(fop, 
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(0),
                        unop(Iop_I32StoF64,
                             loadLE(Ity_I32, mkexpr(addr)))));
               break;

            do_foprev_m32:
               put_ST_UNCHECKED(0, 
                  triop(fop, 
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        unop(Iop_I32StoF64,
                             loadLE(Ity_I32, mkexpr(addr))),
                        get_ST(0)));
               break;

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xDA\n");
               goto decode_fail;
         }

      } else {

         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FCMOVB ST(i), ST(0) */
               r_src = (UInt)modrm - 0xC0;
               DIP("fcmovb %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondB),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xC8 ... 0xCF: /* FCMOVE(Z) ST(i), ST(0) */
               r_src = (UInt)modrm - 0xC8;
               DIP("fcmovz %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondZ),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xD0 ... 0xD7: /* FCMOVBE ST(i), ST(0) */
               r_src = (UInt)modrm - 0xD0;
               DIP("fcmovbe %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondBE),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xD8 ... 0xDF: /* FCMOVU ST(i), ST(0) */
               r_src = (UInt)modrm - 0xD8;
               DIP("fcmovu %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondP),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xE9: /* FUCOMPP %st(0),%st(1) */
               DIP("fucompp %%st(0),%%st(1)\n");
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, get_ST(0), get_ST(1)),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               fp_pop();
               break;

            default:
               goto decode_fail;
         }

      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDB opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDB) {
      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FILD m32int */
               DIP("fildl %s\n", dis_buf);
               fp_push();
               put_ST(0, unop(Iop_I32StoF64,
                              loadLE(Ity_I32, mkexpr(addr))));
               break;

            case 1: /* FISTTPL m32 (SSE3) */
               DIP("fisttpl %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI32S, mkU32(Irrm_ZERO), get_ST(0)) );
               fp_pop();
               break;

            case 2: /* FIST m32 */
               DIP("fistl %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI32S, get_roundingmode(), get_ST(0)) );
               break;

            case 3: /* FISTP m32 */
               DIP("fistpl %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI32S, get_roundingmode(), get_ST(0)) );
               fp_pop();
               break;

            case 5: { /* FLD extended-real */
               /* Uses dirty helper: 
                     ULong x86g_loadF80le ( UInt )
                  addr holds the address.  First, do a dirty call to
                  get hold of the data. */
               IRTemp   val  = newTemp(Ity_I64);
               IRExpr** args = mkIRExprVec_1 ( mkexpr(addr) );

               IRDirty* d = unsafeIRDirty_1_N ( 
                               val, 
                               0/*regparms*/, 
                               "x86g_dirtyhelper_loadF80le", 
                               &x86g_dirtyhelper_loadF80le, 
                               args 
                            );
               /* declare that we're reading memory */
               d->mFx   = Ifx_Read;
               d->mAddr = mkexpr(addr);
               d->mSize = 10;

               /* execute the dirty call, dumping the result in val. */
               stmt( IRStmt_Dirty(d) );
               fp_push();
               put_ST(0, unop(Iop_ReinterpI64asF64, mkexpr(val)));

               DIP("fldt %s\n", dis_buf);
               break;
            }

            case 7: { /* FSTP extended-real */
               /* Uses dirty helper: void x86g_storeF80le ( UInt, ULong ) */
               IRExpr** args 
                  = mkIRExprVec_2( mkexpr(addr), 
                                   unop(Iop_ReinterpF64asI64, get_ST(0)) );

               IRDirty* d = unsafeIRDirty_0_N ( 
                               0/*regparms*/, 
                               "x86g_dirtyhelper_storeF80le", 
                               &x86g_dirtyhelper_storeF80le,
                               args 
                            );
               /* declare we're writing memory */
               d->mFx   = Ifx_Write;
               d->mAddr = mkexpr(addr);
               d->mSize = 10;

               /* execute the dirty call. */
               stmt( IRStmt_Dirty(d) );
               fp_pop();

               DIP("fstpt\n %s", dis_buf);
               break;
            }

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xDB\n");
               goto decode_fail;
         }

      } else {

         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FCMOVNB ST(i), ST(0) */
               r_src = (UInt)modrm - 0xC0;
               DIP("fcmovnb %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondNB),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xC8 ... 0xCF: /* FCMOVNE(NZ) ST(i), ST(0) */
               r_src = (UInt)modrm - 0xC8;
               DIP("fcmovnz %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondNZ),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xD0 ... 0xD7: /* FCMOVNBE ST(i), ST(0) */
               r_src = (UInt)modrm - 0xD0;
               DIP("fcmovnbe %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondNBE),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xD8 ... 0xDF: /* FCMOVNU ST(i), ST(0) */
               r_src = (UInt)modrm - 0xD8;
               DIP("fcmovnu %%st(%d), %%st(0)\n", (Int)r_src);
               put_ST_UNCHECKED(0, 
                                IRExpr_ITE( 
                                    mk_x86g_calculate_condition(X86CondNP),
                                    get_ST(r_src), get_ST(0)) );
               break;

            case 0xE2:
               DIP("fnclex\n");
               break;

            case 0xE3: {
               /* Uses dirty helper: 
                     void x86g_do_FINIT ( VexGuestX86State* ) */
               IRDirty* d  = unsafeIRDirty_0_N ( 
                                0/*regparms*/, 
                                "x86g_dirtyhelper_FINIT", 
                                &x86g_dirtyhelper_FINIT,
                                mkIRExprVec_1(IRExpr_BBPTR())
                             );

               /* declare we're writing guest state */
               d->nFxState = 5;
               vex_bzero(&d->fxState, sizeof(d->fxState));

               d->fxState[0].fx     = Ifx_Write;
               d->fxState[0].offset = OFFB_FTOP;
               d->fxState[0].size   = sizeof(UInt);

               d->fxState[1].fx     = Ifx_Write;
               d->fxState[1].offset = OFFB_FPREGS;
               d->fxState[1].size   = 8 * sizeof(ULong);

               d->fxState[2].fx     = Ifx_Write;
               d->fxState[2].offset = OFFB_FPTAGS;
               d->fxState[2].size   = 8 * sizeof(UChar);

               d->fxState[3].fx     = Ifx_Write;
               d->fxState[3].offset = OFFB_FPROUND;
               d->fxState[3].size   = sizeof(UInt);

               d->fxState[4].fx     = Ifx_Write;
               d->fxState[4].offset = OFFB_FC3210;
               d->fxState[4].size   = sizeof(UInt);

               stmt( IRStmt_Dirty(d) );

               DIP("fninit\n");
               break;
            }

            case 0xE8 ... 0xEF: /* FUCOMI %st(0),%st(?) */
               fp_do_ucomi_ST0_STi( (UInt)modrm - 0xE8, False );
               break;

            case 0xF0 ... 0xF7: /* FCOMI %st(0),%st(?) */
               fp_do_ucomi_ST0_STi( (UInt)modrm - 0xF0, False );
               break;

            default:
               goto decode_fail;
         }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDC opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDC) {
      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FADD double-real */
               fp_do_op_mem_ST_0 ( addr, "add", dis_buf, Iop_AddF64, True );
               break;

            case 1: /* FMUL double-real */
               fp_do_op_mem_ST_0 ( addr, "mul", dis_buf, Iop_MulF64, True );
               break;

            case 2: /* FCOM double-real */
               DIP("fcoml %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      loadLE(Ity_F64,mkexpr(addr))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;  

            case 3: /* FCOMP double-real */
               DIP("fcompl %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      loadLE(Ity_F64,mkexpr(addr))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               break;  

            case 4: /* FSUB double-real */
               fp_do_op_mem_ST_0 ( addr, "sub", dis_buf, Iop_SubF64, True );
               break;

            case 5: /* FSUBR double-real */
               fp_do_oprev_mem_ST_0 ( addr, "subr", dis_buf, Iop_SubF64, True );
               break;

            case 6: /* FDIV double-real */
               fp_do_op_mem_ST_0 ( addr, "div", dis_buf, Iop_DivF64, True );
               break;

            case 7: /* FDIVR double-real */
               fp_do_oprev_mem_ST_0 ( addr, "divr", dis_buf, Iop_DivF64, True );
               break;

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xDC\n");
               goto decode_fail;
         }

      } else {

         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FADD %st(0),%st(?) */
               fp_do_op_ST_ST ( "add", Iop_AddF64, 0, modrm - 0xC0, False );
               break;

            case 0xC8 ... 0xCF: /* FMUL %st(0),%st(?) */
               fp_do_op_ST_ST ( "mul", Iop_MulF64, 0, modrm - 0xC8, False );
               break;

            case 0xE0 ... 0xE7: /* FSUBR %st(0),%st(?) */
               fp_do_oprev_ST_ST ( "subr", Iop_SubF64, 0, modrm - 0xE0, False );
               break;

            case 0xE8 ... 0xEF: /* FSUB %st(0),%st(?) */
               fp_do_op_ST_ST ( "sub", Iop_SubF64, 0, modrm - 0xE8, False );
               break;

            case 0xF0 ... 0xF7: /* FDIVR %st(0),%st(?) */
               fp_do_oprev_ST_ST ( "divr", Iop_DivF64, 0, modrm - 0xF0, False );
               break;

            case 0xF8 ... 0xFF: /* FDIV %st(0),%st(?) */
               fp_do_op_ST_ST ( "div", Iop_DivF64, 0, modrm - 0xF8, False );
               break;

            default:
               goto decode_fail;
         }

      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDD opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDD) {

      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FLD double-real */
               DIP("fldl %s\n", dis_buf);
               fp_push();
               put_ST(0, loadLE(Ity_F64, mkexpr(addr)));
               break;

            case 1: /* FISTTPQ m64 (SSE3) */
               DIP("fistppll %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI64S, mkU32(Irrm_ZERO), get_ST(0)) );
               fp_pop();
               break;

            case 2: /* FST double-real */
               DIP("fstl %s\n", dis_buf);
               storeLE(mkexpr(addr), get_ST(0));
               break;

            case 3: /* FSTP double-real */
               DIP("fstpl %s\n", dis_buf);
               storeLE(mkexpr(addr), get_ST(0));
               fp_pop();
               break;

            case 4: { /* FRSTOR m108 */
               /* Uses dirty helper: 
                     VexEmNote x86g_do_FRSTOR ( VexGuestX86State*, Addr32 ) */
               IRTemp   ew = newTemp(Ity_I32);
               IRDirty* d  = unsafeIRDirty_0_N ( 
                                0/*regparms*/, 
                                "x86g_dirtyhelper_FRSTOR", 
                                &x86g_dirtyhelper_FRSTOR,
                                mkIRExprVec_2( IRExpr_BBPTR(), mkexpr(addr) )
                             );
               d->tmp   = ew;
               /* declare we're reading memory */
               d->mFx   = Ifx_Read;
               d->mAddr = mkexpr(addr);
               d->mSize = 108;

               /* declare we're writing guest state */
               d->nFxState = 5;
               vex_bzero(&d->fxState, sizeof(d->fxState));

               d->fxState[0].fx     = Ifx_Write;
               d->fxState[0].offset = OFFB_FTOP;
               d->fxState[0].size   = sizeof(UInt);

               d->fxState[1].fx     = Ifx_Write;
               d->fxState[1].offset = OFFB_FPREGS;
               d->fxState[1].size   = 8 * sizeof(ULong);

               d->fxState[2].fx     = Ifx_Write;
               d->fxState[2].offset = OFFB_FPTAGS;
               d->fxState[2].size   = 8 * sizeof(UChar);

               d->fxState[3].fx     = Ifx_Write;
               d->fxState[3].offset = OFFB_FPROUND;
               d->fxState[3].size   = sizeof(UInt);

               d->fxState[4].fx     = Ifx_Write;
               d->fxState[4].offset = OFFB_FC3210;
               d->fxState[4].size   = sizeof(UInt);

               stmt( IRStmt_Dirty(d) );

               /* ew contains any emulation warning we may need to
                  issue.  If needed, side-exit to the next insn,
                  reporting the warning, so that Valgrind's dispatcher
                  sees the warning. */
               put_emwarn( mkexpr(ew) );
               stmt( 
                  IRStmt_Exit(
                     binop(Iop_CmpNE32, mkexpr(ew), mkU32(0)),
                     Ijk_EmWarn,
                     IRConst_U32( ((Addr32)guest_EIP_bbstart)+delta),
                     OFFB_EIP
                  )
               );

               DIP("frstor %s\n", dis_buf);
               break;
            }

            case 6: { /* FNSAVE m108 */
               /* Uses dirty helper: 
                     void x86g_do_FSAVE ( VexGuestX86State*, UInt ) */
               IRDirty* d = unsafeIRDirty_0_N ( 
                               0/*regparms*/, 
                               "x86g_dirtyhelper_FSAVE", 
                               &x86g_dirtyhelper_FSAVE,
                               mkIRExprVec_2( IRExpr_BBPTR(), mkexpr(addr) )
                            );
               /* declare we're writing memory */
               d->mFx   = Ifx_Write;
               d->mAddr = mkexpr(addr);
               d->mSize = 108;

               /* declare we're reading guest state */
               d->nFxState = 5;
               vex_bzero(&d->fxState, sizeof(d->fxState));

               d->fxState[0].fx     = Ifx_Read;
               d->fxState[0].offset = OFFB_FTOP;
               d->fxState[0].size   = sizeof(UInt);

               d->fxState[1].fx     = Ifx_Read;
               d->fxState[1].offset = OFFB_FPREGS;
               d->fxState[1].size   = 8 * sizeof(ULong);

               d->fxState[2].fx     = Ifx_Read;
               d->fxState[2].offset = OFFB_FPTAGS;
               d->fxState[2].size   = 8 * sizeof(UChar);

               d->fxState[3].fx     = Ifx_Read;
               d->fxState[3].offset = OFFB_FPROUND;
               d->fxState[3].size   = sizeof(UInt);

               d->fxState[4].fx     = Ifx_Read;
               d->fxState[4].offset = OFFB_FC3210;
               d->fxState[4].size   = sizeof(UInt);

               stmt( IRStmt_Dirty(d) );

               DIP("fnsave %s\n", dis_buf);
               break;
            }

            case 7: { /* FNSTSW m16 */
               IRExpr* sw = get_FPU_sw();
               vassert(typeOfIRExpr(irsb->tyenv, sw) == Ity_I16);
               storeLE( mkexpr(addr), sw );
               DIP("fnstsw %s\n", dis_buf);
               break;
            }

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xDD\n");
               goto decode_fail;
         }
      } else {
         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FFREE %st(?) */
               r_dst = (UInt)modrm - 0xC0;
               DIP("ffree %%st(%d)\n", (Int)r_dst);
               put_ST_TAG ( r_dst, mkU8(0) );
               break;

            case 0xD0 ... 0xD7: /* FST %st(0),%st(?) */
               r_dst = (UInt)modrm - 0xD0;
               DIP("fst %%st(0),%%st(%d)\n", (Int)r_dst);
               /* P4 manual says: "If the destination operand is a
                  non-empty register, the invalid-operation exception
                  is not generated.  Hence put_ST_UNCHECKED. */
               put_ST_UNCHECKED(r_dst, get_ST(0));
               break;

            case 0xD8 ... 0xDF: /* FSTP %st(0),%st(?) */
               r_dst = (UInt)modrm - 0xD8;
               DIP("fstp %%st(0),%%st(%d)\n", (Int)r_dst);
               /* P4 manual says: "If the destination operand is a
                  non-empty register, the invalid-operation exception
                  is not generated.  Hence put_ST_UNCHECKED. */
               put_ST_UNCHECKED(r_dst, get_ST(0));
               fp_pop();
               break;

            case 0xE0 ... 0xE7: /* FUCOM %st(0),%st(?) */
               r_dst = (UInt)modrm - 0xE0;
               DIP("fucom %%st(0),%%st(%d)\n", (Int)r_dst);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, get_ST(0), get_ST(r_dst)),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;

            case 0xE8 ... 0xEF: /* FUCOMP %st(0),%st(?) */
               r_dst = (UInt)modrm - 0xE8;
               DIP("fucomp %%st(0),%%st(%d)\n", (Int)r_dst);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, get_ST(0), get_ST(r_dst)),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               break;

            default:
               goto decode_fail;
         }
      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDE opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDE) {

      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IROp   fop;
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FIADD m16int */ /* ST(0) += m16int */
               DIP("fiaddw %s\n", dis_buf);
               fop = Iop_AddF64;
               goto do_fop_m16;

            case 1: /* FIMUL m16int */ /* ST(0) *= m16int */
               DIP("fimulw %s\n", dis_buf);
               fop = Iop_MulF64;
               goto do_fop_m16;

            case 2: /* FICOM m16int */
               DIP("ficomw %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      unop(Iop_I32StoF64, 
                                         unop(Iop_16Sto32,
                                           loadLE(Ity_I16,mkexpr(addr))))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               break;

            case 3: /* FICOMP m16int */
               DIP("ficompw %s\n", dis_buf);
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, 
                                      get_ST(0),
                                      unop(Iop_I32StoF64, 
                                         unop(Iop_16Sto32,
                                              loadLE(Ity_I16,mkexpr(addr))))),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               break;

            case 4: /* FISUB m16int */ /* ST(0) -= m16int */
               DIP("fisubw %s\n", dis_buf);
               fop = Iop_SubF64;
               goto do_fop_m16;

            case 5: /* FISUBR m16int */ /* ST(0) = m16int - ST(0) */
               DIP("fisubrw %s\n", dis_buf);
               fop = Iop_SubF64;
               goto do_foprev_m16;

            case 6: /* FIDIV m16int */ /* ST(0) /= m16int */
               DIP("fisubw %s\n", dis_buf);
               fop = Iop_DivF64;
               goto do_fop_m16;

            case 7: /* FIDIVR m16int */ /* ST(0) = m16int / ST(0) */
               DIP("fidivrw %s\n", dis_buf);
               fop = Iop_DivF64;
               goto do_foprev_m16;

            do_fop_m16:
               put_ST_UNCHECKED(0, 
                  triop(fop, 
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        get_ST(0),
                        unop(Iop_I32StoF64,
                             unop(Iop_16Sto32, 
                                  loadLE(Ity_I16, mkexpr(addr))))));
               break;

            do_foprev_m16:
               put_ST_UNCHECKED(0, 
                  triop(fop, 
                        get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        unop(Iop_I32StoF64,
                             unop(Iop_16Sto32, 
                                  loadLE(Ity_I16, mkexpr(addr)))),
                        get_ST(0)));
               break;

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xDE\n");
               goto decode_fail;
         }

      } else {

         delta++;
         switch (modrm) {

            case 0xC0 ... 0xC7: /* FADDP %st(0),%st(?) */
               fp_do_op_ST_ST ( "add", Iop_AddF64, 0, modrm - 0xC0, True );
               break;

            case 0xC8 ... 0xCF: /* FMULP %st(0),%st(?) */
               fp_do_op_ST_ST ( "mul", Iop_MulF64, 0, modrm - 0xC8, True );
               break;

            case 0xD9: /* FCOMPP %st(0),%st(1) */
               DIP("fuompp %%st(0),%%st(1)\n");
               /* This forces C1 to zero, which isn't right. */
               put_C3210( 
                   binop( Iop_And32,
                          binop(Iop_Shl32, 
                                binop(Iop_CmpF64, get_ST(0), get_ST(1)),
                                mkU8(8)),
                          mkU32(0x4500)
                   ));
               fp_pop();
               fp_pop();
               break;

            case 0xE0 ... 0xE7: /* FSUBRP %st(0),%st(?) */
               fp_do_oprev_ST_ST ( "subr", Iop_SubF64, 0,  modrm - 0xE0, True );
               break;

            case 0xE8 ... 0xEF: /* FSUBP %st(0),%st(?) */
               fp_do_op_ST_ST ( "sub", Iop_SubF64, 0,  modrm - 0xE8, True );
               break;

            case 0xF0 ... 0xF7: /* FDIVRP %st(0),%st(?) */
               fp_do_oprev_ST_ST ( "divr", Iop_DivF64, 0, modrm - 0xF0, True );
               break;

            case 0xF8 ... 0xFF: /* FDIVP %st(0),%st(?) */
               fp_do_op_ST_ST ( "div", Iop_DivF64, 0, modrm - 0xF8, True );
               break;

            default: 
               goto decode_fail;
         }

      }
   }

   /* -+-+-+-+-+-+-+-+-+-+-+-+ 0xDF opcodes +-+-+-+-+-+-+-+ */
   else
   if (first_opcode == 0xDF) {

      if (modrm < 0xC0) {

         /* bits 5,4,3 are an opcode extension, and the modRM also
            specifies an address. */
         IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
         delta += len;

         switch (gregOfRM(modrm)) {

            case 0: /* FILD m16int */
               DIP("fildw %s\n", dis_buf);
               fp_push();
               put_ST(0, unop(Iop_I32StoF64,
                              unop(Iop_16Sto32,
                                   loadLE(Ity_I16, mkexpr(addr)))));
               break;

            case 1: /* FISTTPS m16 (SSE3) */
               DIP("fisttps %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI16S, mkU32(Irrm_ZERO), get_ST(0)) );
               fp_pop();
               break;

            case 2: /* FIST m16 */
               DIP("fistp %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI16S, get_roundingmode(), get_ST(0)) );
               break;

            case 3: /* FISTP m16 */
               DIP("fistps %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI16S, get_roundingmode(), get_ST(0)) );
               fp_pop();
               break;

            case 5: /* FILD m64 */
               DIP("fildll %s\n", dis_buf);
               fp_push();
               put_ST(0, binop(Iop_I64StoF64,
                               get_roundingmode(),
                               loadLE(Ity_I64, mkexpr(addr))));
               break;

            case 7: /* FISTP m64 */
               DIP("fistpll %s\n", dis_buf);
               storeLE( mkexpr(addr), 
                        binop(Iop_F64toI64S, get_roundingmode(), get_ST(0)) );
               fp_pop();
               break;

            default:
               vex_printf("unhandled opc_aux = 0x%2x\n", gregOfRM(modrm));
               vex_printf("first_opcode == 0xDF\n");
               goto decode_fail;
         }

      } else {

         delta++;
         switch (modrm) {

            case 0xC0: /* FFREEP %st(0) */
               DIP("ffreep %%st(%d)\n", 0);
               put_ST_TAG ( 0, mkU8(0) );
               fp_pop();
               break;

            case 0xE0: /* FNSTSW %ax */
               DIP("fnstsw %%ax\n");
               /* Get the FPU status word value and dump it in %AX. */
               if (0) {
                  /* The obvious thing to do is simply dump the 16-bit
                     status word value in %AX.  However, due to a
                     limitation in Memcheck's origin tracking
                     machinery, this causes Memcheck not to track the
                     origin of any undefinedness into %AH (only into
                     %AL/%AX/%EAX), which means origins are lost in
                     the sequence "fnstsw %ax; test $M,%ah; jcond .." */
                  putIReg(2, R_EAX, get_FPU_sw());
               } else {
                  /* So a somewhat lame kludge is to make it very
                     clear to Memcheck that the value is written to
                     both %AH and %AL.  This generates marginally
                     worse code, but I don't think it matters much. */
                  IRTemp t16 = newTemp(Ity_I16);
                  assign(t16, get_FPU_sw());
                  putIReg( 1, R_AL, unop(Iop_16to8, mkexpr(t16)) );
                  putIReg( 1, R_AH, unop(Iop_16HIto8, mkexpr(t16)) );
               }
               break;

            case 0xE8 ... 0xEF: /* FUCOMIP %st(0),%st(?) */
               fp_do_ucomi_ST0_STi( (UInt)modrm - 0xE8, True );
               break;

            case 0xF0 ... 0xF7: /* FCOMIP %st(0),%st(?) */
               /* not really right since COMIP != UCOMIP */
               fp_do_ucomi_ST0_STi( (UInt)modrm - 0xF0, True );
               break;

            default: 
               goto decode_fail;
         }
      }

   }

   else
   vpanic("dis_FPU(x86): invalid primary opcode");

   *decode_ok = True;
   return delta;

  decode_fail:
   *decode_ok = False;
   return delta;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- MMX INSTRUCTIONS                                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Effect of MMX insns on x87 FPU state (table 11-2 of 
   IA32 arch manual, volume 3):

   Read from, or write to MMX register (viz, any insn except EMMS):
   * All tags set to Valid (non-empty) -- FPTAGS[i] := nonzero
   * FP stack pointer set to zero

   EMMS:
   * All tags set to Invalid (empty) -- FPTAGS[i] := zero
   * FP stack pointer set to zero
*/

static void do_MMX_preamble ( void )
{
   Int         i;
   IRRegArray* descr = mkIRRegArray( OFFB_FPTAGS, Ity_I8, 8 );
   IRExpr*     zero  = mkU32(0);
   IRExpr*     tag1  = mkU8(1);
   put_ftop(zero);
   for (i = 0; i < 8; i++)
      stmt( IRStmt_PutI( mkIRPutI(descr, zero, i, tag1) ) );
}

static void do_EMMS_preamble ( void )
{
   Int         i;
   IRRegArray* descr = mkIRRegArray( OFFB_FPTAGS, Ity_I8, 8 );
   IRExpr*     zero  = mkU32(0);
   IRExpr*     tag0  = mkU8(0);
   put_ftop(zero);
   for (i = 0; i < 8; i++)
      stmt( IRStmt_PutI( mkIRPutI(descr, zero, i, tag0) ) );
}


static IRExpr* getMMXReg ( UInt archreg )
{
   vassert(archreg < 8);
   return IRExpr_Get( OFFB_FPREGS + 8 * archreg, Ity_I64 );
}


static void putMMXReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 8);
   vassert(typeOfIRExpr(irsb->tyenv,e) == Ity_I64);
   stmt( IRStmt_Put( OFFB_FPREGS + 8 * archreg, e ) );
}


/* Helper for non-shift MMX insns.  Note this is incomplete in the
   sense that it does not first call do_MMX_preamble() -- that is the
   responsibility of its caller. */

static 
UInt dis_MMXop_regmem_to_reg ( UChar  sorb,
                               Int    delta,
                               UChar  opc,
                               const HChar* name,
                               Bool   show_granularity )
{
   HChar   dis_buf[50];
   UChar   modrm = getIByte(delta);
   Bool    isReg = epartIsReg(modrm);
   IRExpr* argL  = NULL;
   IRExpr* argR  = NULL;
   IRExpr* argG  = NULL;
   IRExpr* argE  = NULL;
   IRTemp  res   = newTemp(Ity_I64);

   Bool    invG  = False;
   IROp    op    = Iop_INVALID;
   void*   hAddr = NULL;
   Bool    eLeft = False;
   const HChar*  hName = NULL;

#  define XXX(_name) do { hAddr = &_name; hName = #_name; } while (0)

   switch (opc) {
      /* Original MMX ones */
      case 0xFC: op = Iop_Add8x8; break;
      case 0xFD: op = Iop_Add16x4; break;
      case 0xFE: op = Iop_Add32x2; break;

      case 0xEC: op = Iop_QAdd8Sx8; break;
      case 0xED: op = Iop_QAdd16Sx4; break;

      case 0xDC: op = Iop_QAdd8Ux8; break;
      case 0xDD: op = Iop_QAdd16Ux4; break;

      case 0xF8: op = Iop_Sub8x8;  break;
      case 0xF9: op = Iop_Sub16x4; break;
      case 0xFA: op = Iop_Sub32x2; break;

      case 0xE8: op = Iop_QSub8Sx8; break;
      case 0xE9: op = Iop_QSub16Sx4; break;

      case 0xD8: op = Iop_QSub8Ux8; break;
      case 0xD9: op = Iop_QSub16Ux4; break;

      case 0xE5: op = Iop_MulHi16Sx4; break;
      case 0xD5: op = Iop_Mul16x4; break;
      case 0xF5: XXX(x86g_calculate_mmx_pmaddwd); break;

      case 0x74: op = Iop_CmpEQ8x8; break;
      case 0x75: op = Iop_CmpEQ16x4; break;
      case 0x76: op = Iop_CmpEQ32x2; break;

      case 0x64: op = Iop_CmpGT8Sx8; break;
      case 0x65: op = Iop_CmpGT16Sx4; break;
      case 0x66: op = Iop_CmpGT32Sx2; break;

      case 0x6B: op = Iop_QNarrowBin32Sto16Sx4; eLeft = True; break;
      case 0x63: op = Iop_QNarrowBin16Sto8Sx8;  eLeft = True; break;
      case 0x67: op = Iop_QNarrowBin16Sto8Ux8;  eLeft = True; break;

      case 0x68: op = Iop_InterleaveHI8x8;  eLeft = True; break;
      case 0x69: op = Iop_InterleaveHI16x4; eLeft = True; break;
      case 0x6A: op = Iop_InterleaveHI32x2; eLeft = True; break;

      case 0x60: op = Iop_InterleaveLO8x8;  eLeft = True; break;
      case 0x61: op = Iop_InterleaveLO16x4; eLeft = True; break;
      case 0x62: op = Iop_InterleaveLO32x2; eLeft = True; break;

      case 0xDB: op = Iop_And64; break;
      case 0xDF: op = Iop_And64; invG = True; break;
      case 0xEB: op = Iop_Or64; break;
      case 0xEF: /* Possibly do better here if argL and argR are the
                    same reg */
                 op = Iop_Xor64; break;

      /* Introduced in SSE1 */
      case 0xE0: op = Iop_Avg8Ux8;    break;
      case 0xE3: op = Iop_Avg16Ux4;   break;
      case 0xEE: op = Iop_Max16Sx4;   break;
      case 0xDE: op = Iop_Max8Ux8;    break;
      case 0xEA: op = Iop_Min16Sx4;   break;
      case 0xDA: op = Iop_Min8Ux8;    break;
      case 0xE4: op = Iop_MulHi16Ux4; break;
      case 0xF6: XXX(x86g_calculate_mmx_psadbw); break;

      /* Introduced in SSE2 */
      case 0xD4: op = Iop_Add64; break;
      case 0xFB: op = Iop_Sub64; break;

      default: 
         vex_printf("\n0x%x\n", (Int)opc);
         vpanic("dis_MMXop_regmem_to_reg");
   }

#  undef XXX

   argG = getMMXReg(gregOfRM(modrm));
   if (invG)
      argG = unop(Iop_Not64, argG);

   if (isReg) {
      delta++;
      argE = getMMXReg(eregOfRM(modrm));
   } else {
      Int    len;
      IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
      delta += len;
      argE = loadLE(Ity_I64, mkexpr(addr));
   }

   if (eLeft) {
      argL = argE;
      argR = argG;
   } else {
      argL = argG;
      argR = argE;
   }

   if (op != Iop_INVALID) {
      vassert(hName == NULL);
      vassert(hAddr == NULL);
      assign(res, binop(op, argL, argR));
   } else {
      vassert(hName != NULL);
      vassert(hAddr != NULL);
      assign( res, 
              mkIRExprCCall(
                 Ity_I64, 
                 0/*regparms*/, hName, hAddr,
                 mkIRExprVec_2( argL, argR )
              ) 
            );
   }

   putMMXReg( gregOfRM(modrm), mkexpr(res) );

   DIP("%s%s %s, %s\n", 
       name, show_granularity ? nameMMXGran(opc & 3) : "",
       ( isReg ? nameMMXReg(eregOfRM(modrm)) : dis_buf ),
       nameMMXReg(gregOfRM(modrm)) );

   return delta;
}


/* Vector by scalar shift of G by the amount specified at the bottom
   of E.  This is a straight copy of dis_SSE_shiftG_byE. */

static UInt dis_MMX_shiftG_byE ( UChar sorb, Int delta, 
                                 const HChar* opname, IROp op )
{
   HChar   dis_buf[50];
   Int     alen, size;
   IRTemp  addr;
   Bool    shl, shr, sar;
   UChar   rm   = getIByte(delta);
   IRTemp  g0   = newTemp(Ity_I64);
   IRTemp  g1   = newTemp(Ity_I64);
   IRTemp  amt  = newTemp(Ity_I32);
   IRTemp  amt8 = newTemp(Ity_I8);

   if (epartIsReg(rm)) {
      assign( amt, unop(Iop_64to32, getMMXReg(eregOfRM(rm))) );
      DIP("%s %s,%s\n", opname,
                        nameMMXReg(eregOfRM(rm)),
                        nameMMXReg(gregOfRM(rm)) );
      delta++;
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( amt, loadLE(Ity_I32, mkexpr(addr)) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameMMXReg(gregOfRM(rm)) );
      delta += alen;
   }
   assign( g0,   getMMXReg(gregOfRM(rm)) );
   assign( amt8, unop(Iop_32to8, mkexpr(amt)) );

   shl = shr = sar = False;
   size = 0;
   switch (op) {
      case Iop_ShlN16x4: shl = True; size = 32; break;
      case Iop_ShlN32x2: shl = True; size = 32; break;
      case Iop_Shl64:    shl = True; size = 64; break;
      case Iop_ShrN16x4: shr = True; size = 16; break;
      case Iop_ShrN32x2: shr = True; size = 32; break;
      case Iop_Shr64:    shr = True; size = 64; break;
      case Iop_SarN16x4: sar = True; size = 16; break;
      case Iop_SarN32x2: sar = True; size = 32; break;
      default: vassert(0);
   }

   if (shl || shr) {
     assign( 
        g1,
        IRExpr_ITE(
           binop(Iop_CmpLT32U,mkexpr(amt),mkU32(size)),
           binop(op, mkexpr(g0), mkexpr(amt8)),
           mkU64(0)
        )
     );
   } else 
   if (sar) {
     assign( 
        g1,
        IRExpr_ITE(
           binop(Iop_CmpLT32U,mkexpr(amt),mkU32(size)),
           binop(op, mkexpr(g0), mkexpr(amt8)),
           binop(op, mkexpr(g0), mkU8(size-1))
        )
     );
   } else {
      /*NOTREACHED*/
      vassert(0);
   }

   putMMXReg( gregOfRM(rm), mkexpr(g1) );
   return delta;
}


/* Vector by scalar shift of E by an immediate byte.  This is a
   straight copy of dis_SSE_shiftE_imm. */

static 
UInt dis_MMX_shiftE_imm ( Int delta, const HChar* opname, IROp op )
{
   Bool    shl, shr, sar;
   UChar   rm   = getIByte(delta);
   IRTemp  e0   = newTemp(Ity_I64);
   IRTemp  e1   = newTemp(Ity_I64);
   UChar   amt, size;
   vassert(epartIsReg(rm));
   vassert(gregOfRM(rm) == 2 
           || gregOfRM(rm) == 4 || gregOfRM(rm) == 6);
   amt = getIByte(delta+1);
   delta += 2;
   DIP("%s $%d,%s\n", opname,
                      (Int)amt,
                      nameMMXReg(eregOfRM(rm)) );

   assign( e0, getMMXReg(eregOfRM(rm)) );

   shl = shr = sar = False;
   size = 0;
   switch (op) {
      case Iop_ShlN16x4: shl = True; size = 16; break;
      case Iop_ShlN32x2: shl = True; size = 32; break;
      case Iop_Shl64:    shl = True; size = 64; break;
      case Iop_SarN16x4: sar = True; size = 16; break;
      case Iop_SarN32x2: sar = True; size = 32; break;
      case Iop_ShrN16x4: shr = True; size = 16; break;
      case Iop_ShrN32x2: shr = True; size = 32; break;
      case Iop_Shr64:    shr = True; size = 64; break;
      default: vassert(0);
   }

   if (shl || shr) {
      assign( e1, amt >= size 
                     ? mkU64(0)
                     : binop(op, mkexpr(e0), mkU8(amt))
      );
   } else 
   if (sar) {
      assign( e1, amt >= size 
                     ? binop(op, mkexpr(e0), mkU8(size-1))
                     : binop(op, mkexpr(e0), mkU8(amt))
      );
   } else {
      /*NOTREACHED*/
      vassert(0);
   }

   putMMXReg( eregOfRM(rm), mkexpr(e1) );
   return delta;
}


/* Completely handle all MMX instructions except emms. */

static
UInt dis_MMX ( Bool* decode_ok, UChar sorb, Int sz, Int delta )
{
   Int   len;
   UChar modrm;
   HChar dis_buf[50];
   UChar opc = getIByte(delta);
   delta++;

   /* dis_MMX handles all insns except emms. */
   do_MMX_preamble();

   switch (opc) {

      case 0x6E: 
         /* MOVD (src)ireg-or-mem (E), (dst)mmxreg (G)*/
         if (sz != 4) 
            goto mmx_decode_failure;
         modrm = getIByte(delta);
         if (epartIsReg(modrm)) {
            delta++;
            putMMXReg(
               gregOfRM(modrm),
               binop( Iop_32HLto64,
                      mkU32(0),
                      getIReg(4, eregOfRM(modrm)) ) );
            DIP("movd %s, %s\n", 
                nameIReg(4,eregOfRM(modrm)), nameMMXReg(gregOfRM(modrm)));
         } else {
            IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
            delta += len;
            putMMXReg(
               gregOfRM(modrm),
               binop( Iop_32HLto64,
                      mkU32(0),
                      loadLE(Ity_I32, mkexpr(addr)) ) );
            DIP("movd %s, %s\n", dis_buf, nameMMXReg(gregOfRM(modrm)));
         }
         break;

      case 0x7E: /* MOVD (src)mmxreg (G), (dst)ireg-or-mem (E) */
         if (sz != 4) 
            goto mmx_decode_failure;
         modrm = getIByte(delta);
         if (epartIsReg(modrm)) {
            delta++;
            putIReg( 4, eregOfRM(modrm),
                     unop(Iop_64to32, getMMXReg(gregOfRM(modrm)) ) );
            DIP("movd %s, %s\n", 
                nameMMXReg(gregOfRM(modrm)), nameIReg(4,eregOfRM(modrm)));
         } else {
            IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
            delta += len;
            storeLE( mkexpr(addr),
                     unop(Iop_64to32, getMMXReg(gregOfRM(modrm)) ) );
            DIP("movd %s, %s\n", nameMMXReg(gregOfRM(modrm)), dis_buf);
         }
         break;

      case 0x6F:
         /* MOVQ (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         modrm = getIByte(delta);
         if (epartIsReg(modrm)) {
            delta++;
            putMMXReg( gregOfRM(modrm), getMMXReg(eregOfRM(modrm)) );
            DIP("movq %s, %s\n", 
                nameMMXReg(eregOfRM(modrm)), nameMMXReg(gregOfRM(modrm)));
         } else {
            IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
            delta += len;
            putMMXReg( gregOfRM(modrm), loadLE(Ity_I64, mkexpr(addr)) );
            DIP("movq %s, %s\n", 
                dis_buf, nameMMXReg(gregOfRM(modrm)));
         }
         break;

      case 0x7F:
         /* MOVQ (src)mmxreg, (dst)mmxreg-or-mem */
         if (sz != 4) 
            goto mmx_decode_failure;
         modrm = getIByte(delta);
         if (epartIsReg(modrm)) {
            delta++;
            putMMXReg( eregOfRM(modrm), getMMXReg(gregOfRM(modrm)) );
            DIP("movq %s, %s\n", 
                nameMMXReg(gregOfRM(modrm)), nameMMXReg(eregOfRM(modrm)));
         } else {
            IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
            delta += len;
            storeLE( mkexpr(addr), getMMXReg(gregOfRM(modrm)) );
            DIP("mov(nt)q %s, %s\n", 
                nameMMXReg(gregOfRM(modrm)), dis_buf);
         }
         break;

      case 0xFC: 
      case 0xFD: 
      case 0xFE: /* PADDgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "padd", True );
         break;

      case 0xEC: 
      case 0xED: /* PADDSgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "padds", True );
         break;

      case 0xDC: 
      case 0xDD: /* PADDUSgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "paddus", True );
         break;

      case 0xF8: 
      case 0xF9: 
      case 0xFA: /* PSUBgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "psub", True );
         break;

      case 0xE8: 
      case 0xE9: /* PSUBSgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "psubs", True );
         break;

      case 0xD8: 
      case 0xD9: /* PSUBUSgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "psubus", True );
         break;

      case 0xE5: /* PMULHW (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pmulhw", False );
         break;

      case 0xD5: /* PMULLW (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pmullw", False );
         break;

      case 0xF5: /* PMADDWD (src)mmxreg-or-mem, (dst)mmxreg */
         vassert(sz == 4);
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pmaddwd", False );
         break;

      case 0x74: 
      case 0x75: 
      case 0x76: /* PCMPEQgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pcmpeq", True );
         break;

      case 0x64: 
      case 0x65: 
      case 0x66: /* PCMPGTgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pcmpgt", True );
         break;

      case 0x6B: /* PACKSSDW (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "packssdw", False );
         break;

      case 0x63: /* PACKSSWB (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "packsswb", False );
         break;

      case 0x67: /* PACKUSWB (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "packuswb", False );
         break;

      case 0x68: 
      case 0x69: 
      case 0x6A: /* PUNPCKHgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "punpckh", True );
         break;

      case 0x60: 
      case 0x61: 
      case 0x62: /* PUNPCKLgg (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "punpckl", True );
         break;

      case 0xDB: /* PAND (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pand", False );
         break;

      case 0xDF: /* PANDN (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pandn", False );
         break;

      case 0xEB: /* POR (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "por", False );
         break;

      case 0xEF: /* PXOR (src)mmxreg-or-mem, (dst)mmxreg */
         if (sz != 4) 
            goto mmx_decode_failure;
         delta = dis_MMXop_regmem_to_reg ( sorb, delta, opc, "pxor", False );
         break; 

#     define SHIFT_BY_REG(_name,_op)                                 \
                delta = dis_MMX_shiftG_byE(sorb, delta, _name, _op); \
                break;

      /* PSLLgg (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xF1: SHIFT_BY_REG("psllw", Iop_ShlN16x4);
      case 0xF2: SHIFT_BY_REG("pslld", Iop_ShlN32x2);
      case 0xF3: SHIFT_BY_REG("psllq", Iop_Shl64);

      /* PSRLgg (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xD1: SHIFT_BY_REG("psrlw", Iop_ShrN16x4);
      case 0xD2: SHIFT_BY_REG("psrld", Iop_ShrN32x2);
      case 0xD3: SHIFT_BY_REG("psrlq", Iop_Shr64);

      /* PSRAgg (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xE1: SHIFT_BY_REG("psraw", Iop_SarN16x4);
      case 0xE2: SHIFT_BY_REG("psrad", Iop_SarN32x2);

#     undef SHIFT_BY_REG

      case 0x71: 
      case 0x72: 
      case 0x73: {
         /* (sz==4): PSLLgg/PSRAgg/PSRLgg mmxreg by imm8 */
         UChar byte2, subopc;
         if (sz != 4) 
            goto mmx_decode_failure;
         byte2  = getIByte(delta);           /* amode / sub-opcode */
         subopc = toUChar( (byte2 >> 3) & 7 );

#        define SHIFT_BY_IMM(_name,_op)                         \
             do { delta = dis_MMX_shiftE_imm(delta,_name,_op);  \
             } while (0)

              if (subopc == 2 /*SRL*/ && opc == 0x71) 
                 SHIFT_BY_IMM("psrlw", Iop_ShrN16x4);
         else if (subopc == 2 /*SRL*/ && opc == 0x72) 
                 SHIFT_BY_IMM("psrld", Iop_ShrN32x2);
         else if (subopc == 2 /*SRL*/ && opc == 0x73) 
                 SHIFT_BY_IMM("psrlq", Iop_Shr64);

         else if (subopc == 4 /*SAR*/ && opc == 0x71) 
                 SHIFT_BY_IMM("psraw", Iop_SarN16x4);
         else if (subopc == 4 /*SAR*/ && opc == 0x72) 
                 SHIFT_BY_IMM("psrad", Iop_SarN32x2);

         else if (subopc == 6 /*SHL*/ && opc == 0x71) 
                 SHIFT_BY_IMM("psllw", Iop_ShlN16x4);
         else if (subopc == 6 /*SHL*/ && opc == 0x72) 
                 SHIFT_BY_IMM("pslld", Iop_ShlN32x2);
         else if (subopc == 6 /*SHL*/ && opc == 0x73) 
                 SHIFT_BY_IMM("psllq", Iop_Shl64);

         else goto mmx_decode_failure;

#        undef SHIFT_BY_IMM
         break;
      }

      case 0xF7: {
         IRTemp addr    = newTemp(Ity_I32);
         IRTemp regD    = newTemp(Ity_I64);
         IRTemp regM    = newTemp(Ity_I64);
         IRTemp mask    = newTemp(Ity_I64);
         IRTemp olddata = newTemp(Ity_I64);
         IRTemp newdata = newTemp(Ity_I64);

         modrm = getIByte(delta);
         if (sz != 4 || (!epartIsReg(modrm)))
            goto mmx_decode_failure;
         delta++;

         assign( addr, handleSegOverride( sorb, getIReg(4, R_EDI) ));
         assign( regM, getMMXReg( eregOfRM(modrm) ));
         assign( regD, getMMXReg( gregOfRM(modrm) ));
         assign( mask, binop(Iop_SarN8x8, mkexpr(regM), mkU8(7)) );
         assign( olddata, loadLE( Ity_I64, mkexpr(addr) ));
         assign( newdata, 
                 binop(Iop_Or64, 
                       binop(Iop_And64, 
                             mkexpr(regD), 
                             mkexpr(mask) ),
                       binop(Iop_And64, 
                             mkexpr(olddata),
                             unop(Iop_Not64, mkexpr(mask)))) );
         storeLE( mkexpr(addr), mkexpr(newdata) );
         DIP("maskmovq %s,%s\n", nameMMXReg( eregOfRM(modrm) ),
                                 nameMMXReg( gregOfRM(modrm) ) );
         break;
      }

      /* --- MMX decode failure --- */
      default:
      mmx_decode_failure:
         *decode_ok = False;
         return delta; /* ignored */

   }

   *decode_ok = True;
   return delta;
}


/*------------------------------------------------------------*/
/*--- More misc arithmetic and other obscure insns.        ---*/
/*------------------------------------------------------------*/

/* Double length left and right shifts.  Apparently only required in
   v-size (no b- variant). */
static
UInt dis_SHLRD_Gv_Ev ( UChar sorb,
                       Int delta, UChar modrm,
                       Int sz,
                       IRExpr* shift_amt,
                       Bool amt_is_literal,
                       const HChar* shift_amt_txt,
                       Bool left_shift )
{
   /* shift_amt :: Ity_I8 is the amount to shift.  shift_amt_txt is used
      for printing it.   And eip on entry points at the modrm byte. */
   Int len;
   HChar dis_buf[50];

   IRType ty       = szToITy(sz);
   IRTemp gsrc     = newTemp(ty);
   IRTemp esrc     = newTemp(ty);
   IRTemp addr     = IRTemp_INVALID;
   IRTemp tmpSH    = newTemp(Ity_I8);
   IRTemp tmpL     = IRTemp_INVALID;
   IRTemp tmpRes   = IRTemp_INVALID;
   IRTemp tmpSubSh = IRTemp_INVALID;
   IROp   mkpair;
   IROp   getres;
   IROp   shift;
   IRExpr* mask = NULL;

   vassert(sz == 2 || sz == 4);

   /* The E-part is the destination; this is shifted.  The G-part
      supplies bits to be shifted into the E-part, but is not
      changed.  

      If shifting left, form a double-length word with E at the top
      and G at the bottom, and shift this left.  The result is then in
      the high part.

      If shifting right, form a double-length word with G at the top
      and E at the bottom, and shift this right.  The result is then
      at the bottom.  */

   /* Fetch the operands. */

   assign( gsrc, getIReg(sz, gregOfRM(modrm)) );

   if (epartIsReg(modrm)) {
      delta++;
      assign( esrc, getIReg(sz, eregOfRM(modrm)) );
      DIP("sh%cd%c %s, %s, %s\n",
          ( left_shift ? 'l' : 'r' ), nameISize(sz), 
          shift_amt_txt,
          nameIReg(sz, gregOfRM(modrm)), nameIReg(sz, eregOfRM(modrm)));
   } else {
      addr = disAMode ( &len, sorb, delta, dis_buf );
      delta += len;
      assign( esrc, loadLE(ty, mkexpr(addr)) );
      DIP("sh%cd%c %s, %s, %s\n", 
          ( left_shift ? 'l' : 'r' ), nameISize(sz), 
          shift_amt_txt,
          nameIReg(sz, gregOfRM(modrm)), dis_buf);
   }

   /* Round up the relevant primops. */

   if (sz == 4) {
      tmpL     = newTemp(Ity_I64);
      tmpRes   = newTemp(Ity_I32);
      tmpSubSh = newTemp(Ity_I32);
      mkpair   = Iop_32HLto64;
      getres   = left_shift ? Iop_64HIto32 : Iop_64to32;
      shift    = left_shift ? Iop_Shl64 : Iop_Shr64;
      mask     = mkU8(31);
   } else {
      /* sz == 2 */
      tmpL     = newTemp(Ity_I32);
      tmpRes   = newTemp(Ity_I16);
      tmpSubSh = newTemp(Ity_I16);
      mkpair   = Iop_16HLto32;
      getres   = left_shift ? Iop_32HIto16 : Iop_32to16;
      shift    = left_shift ? Iop_Shl32 : Iop_Shr32;
      mask     = mkU8(15);
   }

   /* Do the shift, calculate the subshift value, and set 
      the flag thunk. */

   assign( tmpSH, binop(Iop_And8, shift_amt, mask) );

   if (left_shift)
      assign( tmpL, binop(mkpair, mkexpr(esrc), mkexpr(gsrc)) );
   else
      assign( tmpL, binop(mkpair, mkexpr(gsrc), mkexpr(esrc)) );

   assign( tmpRes, unop(getres, binop(shift, mkexpr(tmpL), mkexpr(tmpSH)) ) );
   assign( tmpSubSh, 
           unop(getres, 
                binop(shift, 
                      mkexpr(tmpL), 
                      binop(Iop_And8, 
                            binop(Iop_Sub8, mkexpr(tmpSH), mkU8(1) ),
                            mask))) );

   setFlags_DEP1_DEP2_shift ( left_shift ? Iop_Shl32 : Iop_Sar32,
                              tmpRes, tmpSubSh, ty, tmpSH );

   /* Put result back. */

   if (epartIsReg(modrm)) {
      putIReg(sz, eregOfRM(modrm), mkexpr(tmpRes));
   } else {
      storeLE( mkexpr(addr), mkexpr(tmpRes) );
   }

   if (amt_is_literal) delta++;
   return delta;
}


/* Handle BT/BTS/BTR/BTC Gv, Ev.  Apparently b-size is not
   required. */

typedef enum { BtOpNone, BtOpSet, BtOpReset, BtOpComp } BtOp;

static const HChar* nameBtOp ( BtOp op )
{
   switch (op) {
      case BtOpNone:  return "";
      case BtOpSet:   return "s";
      case BtOpReset: return "r";
      case BtOpComp:  return "c";
      default: vpanic("nameBtOp(x86)");
   }
}


static
UInt dis_bt_G_E ( VexAbiInfo* vbi,
                  UChar sorb, Bool locked, Int sz, Int delta, BtOp op )
{
   HChar  dis_buf[50];
   UChar  modrm;
   Int    len;
   IRTemp t_fetched, t_bitno0, t_bitno1, t_bitno2, t_addr0, 
          t_addr1, t_esp, t_mask, t_new;

   vassert(sz == 2 || sz == 4);

   t_fetched = t_bitno0 = t_bitno1 = t_bitno2 
             = t_addr0 = t_addr1 = t_esp 
             = t_mask = t_new = IRTemp_INVALID;

   t_fetched = newTemp(Ity_I8);
   t_new     = newTemp(Ity_I8);
   t_bitno0  = newTemp(Ity_I32);
   t_bitno1  = newTemp(Ity_I32);
   t_bitno2  = newTemp(Ity_I8);
   t_addr1   = newTemp(Ity_I32);
   modrm     = getIByte(delta);

   assign( t_bitno0, widenSto32(getIReg(sz, gregOfRM(modrm))) );
   
   if (epartIsReg(modrm)) {
      delta++;
      /* Get it onto the client's stack. */
      t_esp = newTemp(Ity_I32);
      t_addr0 = newTemp(Ity_I32);

      /* For the choice of the value 128, see comment in dis_bt_G_E in
         guest_amd64_toIR.c.  We point out here only that 128 is
         fast-cased in Memcheck and is > 0, so seems like a good
         choice. */
      vassert(vbi->guest_stack_redzone_size == 0);
      assign( t_esp, binop(Iop_Sub32, getIReg(4, R_ESP), mkU32(128)) );
      putIReg(4, R_ESP, mkexpr(t_esp));

      storeLE( mkexpr(t_esp), getIReg(sz, eregOfRM(modrm)) );

      /* Make t_addr0 point at it. */
      assign( t_addr0, mkexpr(t_esp) );

      /* Mask out upper bits of the shift amount, since we're doing a
         reg. */
      assign( t_bitno1, binop(Iop_And32, 
                              mkexpr(t_bitno0), 
                              mkU32(sz == 4 ? 31 : 15)) );

   } else {
      t_addr0 = disAMode ( &len, sorb, delta, dis_buf );
      delta += len;
      assign( t_bitno1, mkexpr(t_bitno0) );
   }
  
   /* At this point: t_addr0 is the address being operated on.  If it
      was a reg, we will have pushed it onto the client's stack.
      t_bitno1 is the bit number, suitably masked in the case of a
      reg.  */
  
   /* Now the main sequence. */
   assign( t_addr1, 
           binop(Iop_Add32, 
                 mkexpr(t_addr0), 
                 binop(Iop_Sar32, mkexpr(t_bitno1), mkU8(3))) );

   /* t_addr1 now holds effective address */

   assign( t_bitno2, 
           unop(Iop_32to8, 
                binop(Iop_And32, mkexpr(t_bitno1), mkU32(7))) );

   /* t_bitno2 contains offset of bit within byte */

   if (op != BtOpNone) {
      t_mask = newTemp(Ity_I8);
      assign( t_mask, binop(Iop_Shl8, mkU8(1), mkexpr(t_bitno2)) );
   }

   /* t_mask is now a suitable byte mask */

   assign( t_fetched, loadLE(Ity_I8, mkexpr(t_addr1)) );

   if (op != BtOpNone) {
      switch (op) {
         case BtOpSet:
            assign( t_new,
                    binop(Iop_Or8, mkexpr(t_fetched), mkexpr(t_mask)) );
            break;
         case BtOpComp:
            assign( t_new,
                    binop(Iop_Xor8, mkexpr(t_fetched), mkexpr(t_mask)) );
            break;
         case BtOpReset:
            assign( t_new,
                    binop(Iop_And8, mkexpr(t_fetched), 
                                    unop(Iop_Not8, mkexpr(t_mask))) );
            break;
         default: 
            vpanic("dis_bt_G_E(x86)");
      }
      if (locked && !epartIsReg(modrm)) {
         casLE( mkexpr(t_addr1), mkexpr(t_fetched)/*expd*/,
                                 mkexpr(t_new)/*new*/,
                                 guest_EIP_curr_instr );
      } else {
         storeLE( mkexpr(t_addr1), mkexpr(t_new) );
      }
   }
 
   /* Side effect done; now get selected bit into Carry flag */
   /* Flags: C=selected bit, O,S,Z,A,P undefined, so are set to zero. */
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
   stmt( IRStmt_Put( 
            OFFB_CC_DEP1,
            binop(Iop_And32,
                  binop(Iop_Shr32, 
                        unop(Iop_8Uto32, mkexpr(t_fetched)),
                        mkexpr(t_bitno2)),
                  mkU32(1)))
       );
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));

   /* Move reg operand from stack back to reg */
   if (epartIsReg(modrm)) {
      /* t_esp still points at it. */
      putIReg(sz, eregOfRM(modrm), loadLE(szToITy(sz), mkexpr(t_esp)) );
      putIReg(4, R_ESP, binop(Iop_Add32, mkexpr(t_esp), mkU32(128)) );
   }

   DIP("bt%s%c %s, %s\n",
       nameBtOp(op), nameISize(sz), nameIReg(sz, gregOfRM(modrm)), 
       ( epartIsReg(modrm) ? nameIReg(sz, eregOfRM(modrm)) : dis_buf ) );
 
   return delta;
}



/* Handle BSF/BSR.  Only v-size seems necessary. */
static
UInt dis_bs_E_G ( UChar sorb, Int sz, Int delta, Bool fwds )
{
   Bool   isReg;
   UChar  modrm;
   HChar  dis_buf[50];
   
   IRType ty  = szToITy(sz);
   IRTemp src = newTemp(ty);
   IRTemp dst = newTemp(ty);

   IRTemp src32 = newTemp(Ity_I32);
   IRTemp dst32 = newTemp(Ity_I32);
   IRTemp srcB  = newTemp(Ity_I1);

   vassert(sz == 4 || sz == 2);

   modrm = getIByte(delta);

   isReg = epartIsReg(modrm);
   if (isReg) {
      delta++;
      assign( src, getIReg(sz, eregOfRM(modrm)) );
   } else {
      Int    len;
      IRTemp addr = disAMode( &len, sorb, delta, dis_buf );
      delta += len;
      assign( src, loadLE(ty, mkexpr(addr)) );
   }

   DIP("bs%c%c %s, %s\n",
       fwds ? 'f' : 'r', nameISize(sz), 
       ( isReg ? nameIReg(sz, eregOfRM(modrm)) : dis_buf ), 
       nameIReg(sz, gregOfRM(modrm)));

   /* Generate a bool expression which is zero iff the original is
      zero, and nonzero otherwise.  Ask for a CmpNE version which, if
      instrumented by Memcheck, is instrumented expensively, since
      this may be used on the output of a preceding movmskb insn,
      which has been known to be partially defined, and in need of
      careful handling. */
   assign( srcB, binop(mkSizedOp(ty,Iop_ExpCmpNE8),
                       mkexpr(src), mkU(ty,0)) );

   /* Flags: Z is 1 iff source value is zero.  All others 
      are undefined -- we force them to zero. */
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
   stmt( IRStmt_Put( 
            OFFB_CC_DEP1,
            IRExpr_ITE( mkexpr(srcB),
                        /* src!=0 */
                        mkU32(0),
                        /* src==0 */
                        mkU32(X86G_CC_MASK_Z)
                        )
       ));
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));

   /* Result: iff source value is zero, we can't use
      Iop_Clz32/Iop_Ctz32 as they have no defined result in that case.
      But anyway, Intel x86 semantics say the result is undefined in
      such situations.  Hence handle the zero case specially. */

   /* Bleh.  What we compute:

          bsf32:  if src == 0 then 0 else  Ctz32(src)
          bsr32:  if src == 0 then 0 else  31 - Clz32(src)

          bsf16:  if src == 0 then 0 else  Ctz32(16Uto32(src))
          bsr16:  if src == 0 then 0 else  31 - Clz32(16Uto32(src))

      First, widen src to 32 bits if it is not already.

      Postscript 15 Oct 04: it seems that at least VIA Nehemiah leaves the
      dst register unchanged when src == 0.  Hence change accordingly.
   */
   if (sz == 2)
      assign( src32, unop(Iop_16Uto32, mkexpr(src)) );
   else
      assign( src32, mkexpr(src) );

   /* The main computation, guarding against zero. */
   assign( dst32,   
           IRExpr_ITE( 
              mkexpr(srcB),
              /* src != 0 */
              fwds ? unop(Iop_Ctz32, mkexpr(src32))
                   : binop(Iop_Sub32, 
                           mkU32(31), 
                           unop(Iop_Clz32, mkexpr(src32))),
              /* src == 0 -- leave dst unchanged */
              widenUto32( getIReg( sz, gregOfRM(modrm) ) )
           )
         );

   if (sz == 2)
      assign( dst, unop(Iop_32to16, mkexpr(dst32)) );
   else
      assign( dst, mkexpr(dst32) );

   /* dump result back */
   putIReg( sz, gregOfRM(modrm), mkexpr(dst) );

   return delta;
}


static 
void codegen_xchg_eAX_Reg ( Int sz, Int reg )
{
   IRType ty = szToITy(sz);
   IRTemp t1 = newTemp(ty);
   IRTemp t2 = newTemp(ty);
   vassert(sz == 2 || sz == 4);
   assign( t1, getIReg(sz, R_EAX) );
   assign( t2, getIReg(sz, reg) );
   putIReg( sz, R_EAX, mkexpr(t2) );
   putIReg( sz, reg, mkexpr(t1) );
   DIP("xchg%c %s, %s\n", 
       nameISize(sz), nameIReg(sz, R_EAX), nameIReg(sz, reg));
}


static 
void codegen_SAHF ( void )
{
   /* Set the flags to:
      (x86g_calculate_flags_all() & X86G_CC_MASK_O)  -- retain the old O flag
      | (%AH & (X86G_CC_MASK_S|X86G_CC_MASK_Z|X86G_CC_MASK_A
                |X86G_CC_MASK_P|X86G_CC_MASK_C)
   */
   UInt   mask_SZACP = X86G_CC_MASK_S|X86G_CC_MASK_Z|X86G_CC_MASK_A
                       |X86G_CC_MASK_C|X86G_CC_MASK_P;
   IRTemp oldflags   = newTemp(Ity_I32);
   assign( oldflags, mk_x86g_calculate_eflags_all() );
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
   stmt( IRStmt_Put( OFFB_CC_DEP1,
         binop(Iop_Or32,
               binop(Iop_And32, mkexpr(oldflags), mkU32(X86G_CC_MASK_O)),
               binop(Iop_And32, 
                     binop(Iop_Shr32, getIReg(4, R_EAX), mkU8(8)),
                     mkU32(mask_SZACP))
              )
   ));
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
}


static 
void codegen_LAHF ( void  )
{
   /* AH <- EFLAGS(SF:ZF:0:AF:0:PF:1:CF) */
   IRExpr* eax_with_hole;
   IRExpr* new_byte;
   IRExpr* new_eax;
   UInt    mask_SZACP = X86G_CC_MASK_S|X86G_CC_MASK_Z|X86G_CC_MASK_A
                        |X86G_CC_MASK_C|X86G_CC_MASK_P;

   IRTemp  flags = newTemp(Ity_I32);
   assign( flags, mk_x86g_calculate_eflags_all() );

   eax_with_hole 
      = binop(Iop_And32, getIReg(4, R_EAX), mkU32(0xFFFF00FF));
   new_byte 
      = binop(Iop_Or32, binop(Iop_And32, mkexpr(flags), mkU32(mask_SZACP)),
                        mkU32(1<<1));
   new_eax 
      = binop(Iop_Or32, eax_with_hole,
                        binop(Iop_Shl32, new_byte, mkU8(8)));
   putIReg(4, R_EAX, new_eax);
}


static
UInt dis_cmpxchg_G_E ( UChar       sorb,
                       Bool        locked,
                       Int         size, 
                       Int         delta0 )
{
   HChar dis_buf[50];
   Int   len;

   IRType ty    = szToITy(size);
   IRTemp acc   = newTemp(ty);
   IRTemp src   = newTemp(ty);
   IRTemp dest  = newTemp(ty);
   IRTemp dest2 = newTemp(ty);
   IRTemp acc2  = newTemp(ty);
   IRTemp cond  = newTemp(Ity_I1);
   IRTemp addr  = IRTemp_INVALID;
   UChar  rm    = getUChar(delta0);

   /* There are 3 cases to consider:

      reg-reg: ignore any lock prefix, generate sequence based
               on ITE

      reg-mem, not locked: ignore any lock prefix, generate sequence
                           based on ITE

      reg-mem, locked: use IRCAS
   */
   if (epartIsReg(rm)) {
      /* case 1 */
      assign( dest, getIReg(size, eregOfRM(rm)) );
      delta0++;
      assign( src, getIReg(size, gregOfRM(rm)) );
      assign( acc, getIReg(size, R_EAX) );
      setFlags_DEP1_DEP2(Iop_Sub8, acc, dest, ty);
      assign( cond, mk_x86g_calculate_condition(X86CondZ) );
      assign( dest2, IRExpr_ITE(mkexpr(cond), mkexpr(src), mkexpr(dest)) );
      assign( acc2,  IRExpr_ITE(mkexpr(cond), mkexpr(acc), mkexpr(dest)) );
      putIReg(size, R_EAX, mkexpr(acc2));
      putIReg(size, eregOfRM(rm), mkexpr(dest2));
      DIP("cmpxchg%c %s,%s\n", nameISize(size),
                               nameIReg(size,gregOfRM(rm)),
                               nameIReg(size,eregOfRM(rm)) );
   } 
   else if (!epartIsReg(rm) && !locked) {
      /* case 2 */
      addr = disAMode ( &len, sorb, delta0, dis_buf );
      assign( dest, loadLE(ty, mkexpr(addr)) );
      delta0 += len;
      assign( src, getIReg(size, gregOfRM(rm)) );
      assign( acc, getIReg(size, R_EAX) );
      setFlags_DEP1_DEP2(Iop_Sub8, acc, dest, ty);
      assign( cond, mk_x86g_calculate_condition(X86CondZ) );
      assign( dest2, IRExpr_ITE(mkexpr(cond), mkexpr(src), mkexpr(dest)) );
      assign( acc2,  IRExpr_ITE(mkexpr(cond), mkexpr(acc), mkexpr(dest)) );
      putIReg(size, R_EAX, mkexpr(acc2));
      storeLE( mkexpr(addr), mkexpr(dest2) );
      DIP("cmpxchg%c %s,%s\n", nameISize(size), 
                               nameIReg(size,gregOfRM(rm)), dis_buf);
   }
   else if (!epartIsReg(rm) && locked) {
      /* case 3 */
      /* src is new value.  acc is expected value.  dest is old value.
         Compute success from the output of the IRCAS, and steer the
         new value for EAX accordingly: in case of success, EAX is
         unchanged. */
      addr = disAMode ( &len, sorb, delta0, dis_buf );
      delta0 += len;
      assign( src, getIReg(size, gregOfRM(rm)) );
      assign( acc, getIReg(size, R_EAX) );
      stmt( IRStmt_CAS( 
         mkIRCAS( IRTemp_INVALID, dest, Iend_LE, mkexpr(addr), 
                  NULL, mkexpr(acc), NULL, mkexpr(src) )
      ));
      setFlags_DEP1_DEP2(Iop_Sub8, acc, dest, ty);
      assign( cond, mk_x86g_calculate_condition(X86CondZ) );
      assign( acc2,  IRExpr_ITE(mkexpr(cond), mkexpr(acc), mkexpr(dest)) );
      putIReg(size, R_EAX, mkexpr(acc2));
      DIP("cmpxchg%c %s,%s\n", nameISize(size), 
                               nameIReg(size,gregOfRM(rm)), dis_buf);
   }
   else vassert(0);

   return delta0;
}


/* Handle conditional move instructions of the form
      cmovcc E(reg-or-mem), G(reg)

   E(src) is reg-or-mem
   G(dst) is reg.

   If E is reg, -->    GET %E, tmps
                       GET %G, tmpd
                       CMOVcc tmps, tmpd
                       PUT tmpd, %G
 
   If E is mem  -->    (getAddr E) -> tmpa
                       LD (tmpa), tmps
                       GET %G, tmpd
                       CMOVcc tmps, tmpd
                       PUT tmpd, %G
*/
static
UInt dis_cmov_E_G ( UChar       sorb,
                    Int         sz, 
                    X86Condcode cond,
                    Int         delta0 )
{
   UChar rm  = getIByte(delta0);
   HChar dis_buf[50];
   Int   len;

   IRType ty   = szToITy(sz);
   IRTemp tmps = newTemp(ty);
   IRTemp tmpd = newTemp(ty);

   if (epartIsReg(rm)) {
      assign( tmps, getIReg(sz, eregOfRM(rm)) );
      assign( tmpd, getIReg(sz, gregOfRM(rm)) );

      putIReg(sz, gregOfRM(rm),
                  IRExpr_ITE( mk_x86g_calculate_condition(cond),
                              mkexpr(tmps),
                              mkexpr(tmpd) )
             );
      DIP("cmov%c%s %s,%s\n", nameISize(sz), 
                              name_X86Condcode(cond),
                              nameIReg(sz,eregOfRM(rm)),
                              nameIReg(sz,gregOfRM(rm)));
      return 1+delta0;
   }

   /* E refers to memory */    
   {
      IRTemp addr = disAMode ( &len, sorb, delta0, dis_buf );
      assign( tmps, loadLE(ty, mkexpr(addr)) );
      assign( tmpd, getIReg(sz, gregOfRM(rm)) );

      putIReg(sz, gregOfRM(rm),
                  IRExpr_ITE( mk_x86g_calculate_condition(cond),
                              mkexpr(tmps),
                              mkexpr(tmpd) )
             );

      DIP("cmov%c%s %s,%s\n", nameISize(sz), 
                              name_X86Condcode(cond),
                              dis_buf,
                              nameIReg(sz,gregOfRM(rm)));
      return len+delta0;
   }
}


static
UInt dis_xadd_G_E ( UChar sorb, Bool locked, Int sz, Int delta0,
                    Bool* decodeOK )
{
   Int   len;
   UChar rm = getIByte(delta0);
   HChar dis_buf[50];

   IRType ty    = szToITy(sz);
   IRTemp tmpd  = newTemp(ty);
   IRTemp tmpt0 = newTemp(ty);
   IRTemp tmpt1 = newTemp(ty);

   /* There are 3 cases to consider:

      reg-reg: ignore any lock prefix,
               generate 'naive' (non-atomic) sequence

      reg-mem, not locked: ignore any lock prefix, generate 'naive'
                           (non-atomic) sequence

      reg-mem, locked: use IRCAS
   */

   if (epartIsReg(rm)) {
      /* case 1 */
      assign( tmpd,  getIReg(sz, eregOfRM(rm)));
      assign( tmpt0, getIReg(sz, gregOfRM(rm)) );
      assign( tmpt1, binop(mkSizedOp(ty,Iop_Add8),
                           mkexpr(tmpd), mkexpr(tmpt0)) );
      setFlags_DEP1_DEP2( Iop_Add8, tmpd, tmpt0, ty );
      putIReg(sz, eregOfRM(rm), mkexpr(tmpt1));
      putIReg(sz, gregOfRM(rm), mkexpr(tmpd));
      DIP("xadd%c %s, %s\n",
          nameISize(sz), nameIReg(sz,gregOfRM(rm)), 
          				 nameIReg(sz,eregOfRM(rm)));
      *decodeOK = True;
      return 1+delta0;
   }
   else if (!epartIsReg(rm) && !locked) {
      /* case 2 */
      IRTemp addr = disAMode ( &len, sorb, delta0, dis_buf );
      assign( tmpd,  loadLE(ty, mkexpr(addr)) );
      assign( tmpt0, getIReg(sz, gregOfRM(rm)) );
      assign( tmpt1, binop(mkSizedOp(ty,Iop_Add8),
                           mkexpr(tmpd), mkexpr(tmpt0)) );
      storeLE( mkexpr(addr), mkexpr(tmpt1) );
      setFlags_DEP1_DEP2( Iop_Add8, tmpd, tmpt0, ty );
      putIReg(sz, gregOfRM(rm), mkexpr(tmpd));
      DIP("xadd%c %s, %s\n",
          nameISize(sz), nameIReg(sz,gregOfRM(rm)), dis_buf);
      *decodeOK = True;
      return len+delta0;
   }
   else if (!epartIsReg(rm) && locked) {
      /* case 3 */
      IRTemp addr = disAMode ( &len, sorb, delta0, dis_buf );
      assign( tmpd,  loadLE(ty, mkexpr(addr)) );
      assign( tmpt0, getIReg(sz, gregOfRM(rm)) );
      assign( tmpt1, binop(mkSizedOp(ty,Iop_Add8), 
                           mkexpr(tmpd), mkexpr(tmpt0)) );
      casLE( mkexpr(addr), mkexpr(tmpd)/*expVal*/,
                           mkexpr(tmpt1)/*newVal*/, guest_EIP_curr_instr );
      setFlags_DEP1_DEP2( Iop_Add8, tmpd, tmpt0, ty );
      putIReg(sz, gregOfRM(rm), mkexpr(tmpd));
      DIP("xadd%c %s, %s\n",
          nameISize(sz), nameIReg(sz,gregOfRM(rm)), dis_buf);
      *decodeOK = True;
      return len+delta0;
   }
   /*UNREACHED*/
   vassert(0);
}

/* Move 16 bits from Ew (ireg or mem) to G (a segment register). */

static
UInt dis_mov_Ew_Sw ( UChar sorb, Int delta0 )
{
   Int    len;
   IRTemp addr;
   UChar  rm  = getIByte(delta0);
   HChar  dis_buf[50];

   if (epartIsReg(rm)) {
      putSReg( gregOfRM(rm), getIReg(2, eregOfRM(rm)) );
      DIP("movw %s,%s\n", nameIReg(2,eregOfRM(rm)), nameSReg(gregOfRM(rm)));
      return 1+delta0;
   } else {
      addr = disAMode ( &len, sorb, delta0, dis_buf );
      putSReg( gregOfRM(rm), loadLE(Ity_I16, mkexpr(addr)) );
      DIP("movw %s,%s\n", dis_buf, nameSReg(gregOfRM(rm)));
      return len+delta0;
   }
}

/* Move 16 bits from G (a segment register) to Ew (ireg or mem).  If
   dst is ireg and sz==4, zero out top half of it.  */

static
UInt dis_mov_Sw_Ew ( UChar sorb,
                     Int   sz,
                     Int   delta0 )
{
   Int    len;
   IRTemp addr;
   UChar  rm  = getIByte(delta0);
   HChar  dis_buf[50];

   vassert(sz == 2 || sz == 4);

   if (epartIsReg(rm)) {
      if (sz == 4)
         putIReg(4, eregOfRM(rm), unop(Iop_16Uto32, getSReg(gregOfRM(rm))));
      else
         putIReg(2, eregOfRM(rm), getSReg(gregOfRM(rm)));

      DIP("mov %s,%s\n", nameSReg(gregOfRM(rm)), nameIReg(sz,eregOfRM(rm)));
      return 1+delta0;
   } else {
      addr = disAMode ( &len, sorb, delta0, dis_buf );
      storeLE( mkexpr(addr), getSReg(gregOfRM(rm)) );
      DIP("mov %s,%s\n", nameSReg(gregOfRM(rm)), dis_buf);
      return len+delta0;
   }
}


static 
void dis_push_segreg ( UInt sreg, Int sz )
{
    IRTemp t1 = newTemp(Ity_I16);
    IRTemp ta = newTemp(Ity_I32);
    vassert(sz == 2 || sz == 4);

    assign( t1, getSReg(sreg) );
    assign( ta, binop(Iop_Sub32, getIReg(4, R_ESP), mkU32(sz)) );
    putIReg(4, R_ESP, mkexpr(ta));
    storeLE( mkexpr(ta), mkexpr(t1) );

    DIP("push%c %s\n", sz==2 ? 'w' : 'l', nameSReg(sreg));
}

static
void dis_pop_segreg ( UInt sreg, Int sz )
{
    IRTemp t1 = newTemp(Ity_I16);
    IRTemp ta = newTemp(Ity_I32);
    vassert(sz == 2 || sz == 4);

    assign( ta, getIReg(4, R_ESP) );
    assign( t1, loadLE(Ity_I16, mkexpr(ta)) );

    putIReg(4, R_ESP, binop(Iop_Add32, mkexpr(ta), mkU32(sz)) );
    putSReg( sreg, mkexpr(t1) );
    DIP("pop%c %s\n", sz==2 ? 'w' : 'l', nameSReg(sreg));
}

static
void dis_ret ( /*MOD*/DisResult* dres, UInt d32 )
{
   IRTemp t1 = newTemp(Ity_I32);
   IRTemp t2 = newTemp(Ity_I32);
   assign(t1, getIReg(4,R_ESP));
   assign(t2, loadLE(Ity_I32,mkexpr(t1)));
   putIReg(4, R_ESP,binop(Iop_Add32, mkexpr(t1), mkU32(4+d32)));
   jmp_treg(dres, Ijk_Ret, t2);
   vassert(dres->whatNext == Dis_StopHere);
}

/*------------------------------------------------------------*/
/*--- SSE/SSE2/SSE3 helpers                                ---*/
/*------------------------------------------------------------*/

/* Indicates whether the op requires a rounding-mode argument.  Note
   that this covers only vector floating point arithmetic ops, and
   omits the scalar ones that need rounding modes.  Note also that
   inconsistencies here will get picked up later by the IR sanity
   checker, so this isn't correctness-critical. */
static Bool requiresRMode ( IROp op )
{
   switch (op) {
      /* 128 bit ops */
      case Iop_Add32Fx4: case Iop_Sub32Fx4:
      case Iop_Mul32Fx4: case Iop_Div32Fx4:
      case Iop_Add64Fx2: case Iop_Sub64Fx2:
      case Iop_Mul64Fx2: case Iop_Div64Fx2:
         return True;
      default:
         break;
   }
   return False;
}


/* Worker function; do not call directly. 
   Handles full width G = G `op` E   and   G = (not G) `op` E.
*/

static UInt dis_SSE_E_to_G_all_wrk ( 
               UChar sorb, Int delta, 
               const HChar* opname, IROp op,
               Bool   invertG
            )
{
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   IRExpr* gpart
      = invertG ? unop(Iop_NotV128, getXMMReg(gregOfRM(rm)))
                : getXMMReg(gregOfRM(rm));
   if (epartIsReg(rm)) {
      putXMMReg(
         gregOfRM(rm),
         requiresRMode(op)
            ? triop(op, get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        gpart,
                        getXMMReg(eregOfRM(rm)))
            : binop(op, gpart,
                        getXMMReg(eregOfRM(rm)))
      );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      return delta+1;
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      putXMMReg(
         gregOfRM(rm), 
         requiresRMode(op)
            ? triop(op, get_FAKE_roundingmode(), /* XXXROUNDINGFIXME */
                        gpart,
                        loadLE(Ity_V128, mkexpr(addr)))
            : binop(op, gpart,
                        loadLE(Ity_V128, mkexpr(addr)))
      );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      return delta+alen;
   }
}


/* All lanes SSE binary operation, G = G `op` E. */

static
UInt dis_SSE_E_to_G_all ( UChar sorb, Int delta, const HChar* opname, IROp op )
{
   return dis_SSE_E_to_G_all_wrk( sorb, delta, opname, op, False );
}

/* All lanes SSE binary operation, G = (not G) `op` E. */

static
UInt dis_SSE_E_to_G_all_invG ( UChar sorb, Int delta, 
                               const HChar* opname, IROp op )
{
   return dis_SSE_E_to_G_all_wrk( sorb, delta, opname, op, True );
}


/* Lowest 32-bit lane only SSE binary operation, G = G `op` E. */

static UInt dis_SSE_E_to_G_lo32 ( UChar sorb, Int delta, 
                                  const HChar* opname, IROp op )
{
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   IRExpr* gpart = getXMMReg(gregOfRM(rm));
   if (epartIsReg(rm)) {
      putXMMReg( gregOfRM(rm), 
                 binop(op, gpart,
                           getXMMReg(eregOfRM(rm))) );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      return delta+1;
   } else {
      /* We can only do a 32-bit memory read, so the upper 3/4 of the
         E operand needs to be made simply of zeroes. */
      IRTemp epart = newTemp(Ity_V128);
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( epart, unop( Iop_32UtoV128,
                           loadLE(Ity_I32, mkexpr(addr))) );
      putXMMReg( gregOfRM(rm), 
                 binop(op, gpart, mkexpr(epart)) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      return delta+alen;
   }
}


/* Lower 64-bit lane only SSE binary operation, G = G `op` E. */

static UInt dis_SSE_E_to_G_lo64 ( UChar sorb, Int delta, 
                                  const HChar* opname, IROp op )
{
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   IRExpr* gpart = getXMMReg(gregOfRM(rm));
   if (epartIsReg(rm)) {
      putXMMReg( gregOfRM(rm), 
                 binop(op, gpart,
                           getXMMReg(eregOfRM(rm))) );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      return delta+1;
   } else {
      /* We can only do a 64-bit memory read, so the upper half of the
         E operand needs to be made simply of zeroes. */
      IRTemp epart = newTemp(Ity_V128);
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( epart, unop( Iop_64UtoV128,
                           loadLE(Ity_I64, mkexpr(addr))) );
      putXMMReg( gregOfRM(rm), 
                 binop(op, gpart, mkexpr(epart)) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      return delta+alen;
   }
}


/* All lanes unary SSE operation, G = op(E). */

static UInt dis_SSE_E_to_G_unary_all ( 
               UChar sorb, Int delta, 
               const HChar* opname, IROp op
            )
{
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   if (epartIsReg(rm)) {
      putXMMReg( gregOfRM(rm), 
                 unop(op, getXMMReg(eregOfRM(rm))) );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      return delta+1;
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      putXMMReg( gregOfRM(rm), 
                 unop(op, loadLE(Ity_V128, mkexpr(addr))) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      return delta+alen;
   }
}


/* Lowest 32-bit lane only unary SSE operation, G = op(E). */

static UInt dis_SSE_E_to_G_unary_lo32 ( 
               UChar sorb, Int delta, 
               const HChar* opname, IROp op
            )
{
   /* First we need to get the old G value and patch the low 32 bits
      of the E operand into it.  Then apply op and write back to G. */
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   IRTemp  oldG0 = newTemp(Ity_V128);
   IRTemp  oldG1 = newTemp(Ity_V128);

   assign( oldG0, getXMMReg(gregOfRM(rm)) );

   if (epartIsReg(rm)) {
      assign( oldG1, 
              binop( Iop_SetV128lo32,
                     mkexpr(oldG0),
                     getXMMRegLane32(eregOfRM(rm), 0)) );
      putXMMReg( gregOfRM(rm), unop(op, mkexpr(oldG1)) );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      return delta+1;
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( oldG1, 
              binop( Iop_SetV128lo32,
                     mkexpr(oldG0),
                     loadLE(Ity_I32, mkexpr(addr)) ));
      putXMMReg( gregOfRM(rm), unop(op, mkexpr(oldG1)) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      return delta+alen;
   }
}


/* Lowest 64-bit lane only unary SSE operation, G = op(E). */

static UInt dis_SSE_E_to_G_unary_lo64 ( 
               UChar sorb, Int delta, 
               const HChar* opname, IROp op
            )
{
   /* First we need to get the old G value and patch the low 64 bits
      of the E operand into it.  Then apply op and write back to G. */
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   IRTemp  oldG0 = newTemp(Ity_V128);
   IRTemp  oldG1 = newTemp(Ity_V128);

   assign( oldG0, getXMMReg(gregOfRM(rm)) );

   if (epartIsReg(rm)) {
      assign( oldG1, 
              binop( Iop_SetV128lo64,
                     mkexpr(oldG0),
                     getXMMRegLane64(eregOfRM(rm), 0)) );
      putXMMReg( gregOfRM(rm), unop(op, mkexpr(oldG1)) );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      return delta+1;
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( oldG1, 
              binop( Iop_SetV128lo64,
                     mkexpr(oldG0),
                     loadLE(Ity_I64, mkexpr(addr)) ));
      putXMMReg( gregOfRM(rm), unop(op, mkexpr(oldG1)) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      return delta+alen;
   }
}


/* SSE integer binary operation:
      G = G `op` E   (eLeft == False)
      G = E `op` G   (eLeft == True)
*/
static UInt dis_SSEint_E_to_G( 
               UChar sorb, Int delta, 
               const HChar* opname, IROp op,
               Bool   eLeft
            )
{
   HChar   dis_buf[50];
   Int     alen;
   IRTemp  addr;
   UChar   rm = getIByte(delta);
   IRExpr* gpart = getXMMReg(gregOfRM(rm));
   IRExpr* epart = NULL;
   if (epartIsReg(rm)) {
      epart = getXMMReg(eregOfRM(rm));
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      delta += 1;
   } else {
      addr  = disAMode ( &alen, sorb, delta, dis_buf );
      epart = loadLE(Ity_V128, mkexpr(addr));
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      delta += alen;
   }
   putXMMReg( gregOfRM(rm), 
              eLeft ? binop(op, epart, gpart)
	            : binop(op, gpart, epart) );
   return delta;
}


/* Helper for doing SSE FP comparisons. */

static void findSSECmpOp ( Bool* needNot, IROp* op, 
                           Int imm8, Bool all_lanes, Int sz )
{
   imm8 &= 7;
   *needNot = False;
   *op      = Iop_INVALID;
   if (imm8 >= 4) {
      *needNot = True;
      imm8 -= 4;
   }

   if (sz == 4 && all_lanes) {
      switch (imm8) {
         case 0: *op = Iop_CmpEQ32Fx4; return;
         case 1: *op = Iop_CmpLT32Fx4; return;
         case 2: *op = Iop_CmpLE32Fx4; return;
         case 3: *op = Iop_CmpUN32Fx4; return;
         default: break;
      }
   }
   if (sz == 4 && !all_lanes) {
      switch (imm8) {
         case 0: *op = Iop_CmpEQ32F0x4; return;
         case 1: *op = Iop_CmpLT32F0x4; return;
         case 2: *op = Iop_CmpLE32F0x4; return;
         case 3: *op = Iop_CmpUN32F0x4; return;
         default: break;
      }
   }
   if (sz == 8 && all_lanes) {
      switch (imm8) {
         case 0: *op = Iop_CmpEQ64Fx2; return;
         case 1: *op = Iop_CmpLT64Fx2; return;
         case 2: *op = Iop_CmpLE64Fx2; return;
         case 3: *op = Iop_CmpUN64Fx2; return;
         default: break;
      }
   }
   if (sz == 8 && !all_lanes) {
      switch (imm8) {
         case 0: *op = Iop_CmpEQ64F0x2; return;
         case 1: *op = Iop_CmpLT64F0x2; return;
         case 2: *op = Iop_CmpLE64F0x2; return;
         case 3: *op = Iop_CmpUN64F0x2; return;
         default: break;
      }
   }
   vpanic("findSSECmpOp(x86,guest)");
}

/* Handles SSE 32F/64F comparisons. */

static UInt dis_SSEcmp_E_to_G ( UChar sorb, Int delta, 
				const HChar* opname, Bool all_lanes, Int sz )
{
   HChar   dis_buf[50];
   Int     alen, imm8;
   IRTemp  addr;
   Bool    needNot = False;
   IROp    op      = Iop_INVALID;
   IRTemp  plain   = newTemp(Ity_V128);
   UChar   rm      = getIByte(delta);
   UShort  mask    = 0;
   vassert(sz == 4 || sz == 8);
   if (epartIsReg(rm)) {
      imm8 = getIByte(delta+1);
      findSSECmpOp(&needNot, &op, imm8, all_lanes, sz);
      assign( plain, binop(op, getXMMReg(gregOfRM(rm)), 
                               getXMMReg(eregOfRM(rm))) );
      delta += 2;
      DIP("%s $%d,%s,%s\n", opname,
                            (Int)imm8,
                            nameXMMReg(eregOfRM(rm)),
                            nameXMMReg(gregOfRM(rm)) );
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      imm8 = getIByte(delta+alen);
      findSSECmpOp(&needNot, &op, imm8, all_lanes, sz);
      assign( plain, 
              binop(
                 op,
                 getXMMReg(gregOfRM(rm)), 
                   all_lanes  ? loadLE(Ity_V128, mkexpr(addr))
                 : sz == 8    ? unop( Iop_64UtoV128, loadLE(Ity_I64, mkexpr(addr)))
                 : /*sz==4*/    unop( Iop_32UtoV128, loadLE(Ity_I32, mkexpr(addr)))
             ) 
      );
      delta += alen+1;
      DIP("%s $%d,%s,%s\n", opname,
                            (Int)imm8,
                            dis_buf,
                            nameXMMReg(gregOfRM(rm)) );
   }

   if (needNot && all_lanes) {
      putXMMReg( gregOfRM(rm), 
                 unop(Iop_NotV128, mkexpr(plain)) );
   }
   else
   if (needNot && !all_lanes) {
      mask = toUShort( sz==4 ? 0x000F : 0x00FF );
      putXMMReg( gregOfRM(rm), 
                 binop(Iop_XorV128, mkexpr(plain), mkV128(mask)) );
   }
   else {
      putXMMReg( gregOfRM(rm), mkexpr(plain) );
   }

   return delta;
}


/* Vector by scalar shift of G by the amount specified at the bottom
   of E. */

static UInt dis_SSE_shiftG_byE ( UChar sorb, Int delta, 
                                 const HChar* opname, IROp op )
{
   HChar   dis_buf[50];
   Int     alen, size;
   IRTemp  addr;
   Bool    shl, shr, sar;
   UChar   rm   = getIByte(delta);
   IRTemp  g0   = newTemp(Ity_V128);
   IRTemp  g1   = newTemp(Ity_V128);
   IRTemp  amt  = newTemp(Ity_I32);
   IRTemp  amt8 = newTemp(Ity_I8);
   if (epartIsReg(rm)) {
      assign( amt, getXMMRegLane32(eregOfRM(rm), 0) );
      DIP("%s %s,%s\n", opname,
                        nameXMMReg(eregOfRM(rm)),
                        nameXMMReg(gregOfRM(rm)) );
      delta++;
   } else {
      addr = disAMode ( &alen, sorb, delta, dis_buf );
      assign( amt, loadLE(Ity_I32, mkexpr(addr)) );
      DIP("%s %s,%s\n", opname,
                        dis_buf,
                        nameXMMReg(gregOfRM(rm)) );
      delta += alen;
   }
   assign( g0,   getXMMReg(gregOfRM(rm)) );
   assign( amt8, unop(Iop_32to8, mkexpr(amt)) );

   shl = shr = sar = False;
   size = 0;
   switch (op) {
      case Iop_ShlN16x8: shl = True; size = 32; break;
      case Iop_ShlN32x4: shl = True; size = 32; break;
      case Iop_ShlN64x2: shl = True; size = 64; break;
      case Iop_SarN16x8: sar = True; size = 16; break;
      case Iop_SarN32x4: sar = True; size = 32; break;
      case Iop_ShrN16x8: shr = True; size = 16; break;
      case Iop_ShrN32x4: shr = True; size = 32; break;
      case Iop_ShrN64x2: shr = True; size = 64; break;
      default: vassert(0);
   }

   if (shl || shr) {
     assign( 
        g1,
        IRExpr_ITE(
           binop(Iop_CmpLT32U,mkexpr(amt),mkU32(size)),
           binop(op, mkexpr(g0), mkexpr(amt8)),
           mkV128(0x0000)
        )
     );
   } else 
   if (sar) {
     assign( 
        g1,
        IRExpr_ITE(
           binop(Iop_CmpLT32U,mkexpr(amt),mkU32(size)),
           binop(op, mkexpr(g0), mkexpr(amt8)),
           binop(op, mkexpr(g0), mkU8(size-1))
        )
     );
   } else {
      /*NOTREACHED*/
      vassert(0);
   }

   putXMMReg( gregOfRM(rm), mkexpr(g1) );
   return delta;
}


/* Vector by scalar shift of E by an immediate byte. */

static 
UInt dis_SSE_shiftE_imm ( Int delta, const HChar* opname, IROp op )
{
   Bool    shl, shr, sar;
   UChar   rm   = getIByte(delta);
   IRTemp  e0   = newTemp(Ity_V128);
   IRTemp  e1   = newTemp(Ity_V128);
   UChar   amt, size;
   vassert(epartIsReg(rm));
   vassert(gregOfRM(rm) == 2 
           || gregOfRM(rm) == 4 || gregOfRM(rm) == 6);
   amt = getIByte(delta+1);
   delta += 2;
   DIP("%s $%d,%s\n", opname,
                      (Int)amt,
                      nameXMMReg(eregOfRM(rm)) );
   assign( e0, getXMMReg(eregOfRM(rm)) );

   shl = shr = sar = False;
   size = 0;
   switch (op) {
      case Iop_ShlN16x8: shl = True; size = 16; break;
      case Iop_ShlN32x4: shl = True; size = 32; break;
      case Iop_ShlN64x2: shl = True; size = 64; break;
      case Iop_SarN16x8: sar = True; size = 16; break;
      case Iop_SarN32x4: sar = True; size = 32; break;
      case Iop_ShrN16x8: shr = True; size = 16; break;
      case Iop_ShrN32x4: shr = True; size = 32; break;
      case Iop_ShrN64x2: shr = True; size = 64; break;
      default: vassert(0);
   }

   if (shl || shr) {
      assign( e1, amt >= size 
                     ? mkV128(0x0000)
                     : binop(op, mkexpr(e0), mkU8(amt))
      );
   } else 
   if (sar) {
      assign( e1, amt >= size 
                     ? binop(op, mkexpr(e0), mkU8(size-1))
                     : binop(op, mkexpr(e0), mkU8(amt))
      );
   } else {
      /*NOTREACHED*/
      vassert(0);
   }

   putXMMReg( eregOfRM(rm), mkexpr(e1) );
   return delta;
}


/* Get the current SSE rounding mode. */

static IRExpr* /* :: Ity_I32 */ get_sse_roundingmode ( void )
{
   return binop( Iop_And32, 
                 IRExpr_Get( OFFB_SSEROUND, Ity_I32 ), 
                 mkU32(3) );
}

static void put_sse_roundingmode ( IRExpr* sseround )
{
   vassert(typeOfIRExpr(irsb->tyenv, sseround) == Ity_I32);
   stmt( IRStmt_Put( OFFB_SSEROUND, sseround ) );
}

/* Break a 128-bit value up into four 32-bit ints. */

static void breakup128to32s ( IRTemp t128,
			      /*OUTs*/
                              IRTemp* t3, IRTemp* t2,
                              IRTemp* t1, IRTemp* t0 )
{
   IRTemp hi64 = newTemp(Ity_I64);
   IRTemp lo64 = newTemp(Ity_I64);
   assign( hi64, unop(Iop_V128HIto64, mkexpr(t128)) );
   assign( lo64, unop(Iop_V128to64,   mkexpr(t128)) );

   vassert(t0 && *t0 == IRTemp_INVALID);
   vassert(t1 && *t1 == IRTemp_INVALID);
   vassert(t2 && *t2 == IRTemp_INVALID);
   vassert(t3 && *t3 == IRTemp_INVALID);

   *t0 = newTemp(Ity_I32);
   *t1 = newTemp(Ity_I32);
   *t2 = newTemp(Ity_I32);
   *t3 = newTemp(Ity_I32);
   assign( *t0, unop(Iop_64to32,   mkexpr(lo64)) );
   assign( *t1, unop(Iop_64HIto32, mkexpr(lo64)) );
   assign( *t2, unop(Iop_64to32,   mkexpr(hi64)) );
   assign( *t3, unop(Iop_64HIto32, mkexpr(hi64)) );
}

/* Construct a 128-bit value from four 32-bit ints. */

static IRExpr* mk128from32s ( IRTemp t3, IRTemp t2,
                              IRTemp t1, IRTemp t0 )
{
   return
      binop( Iop_64HLtoV128,
             binop(Iop_32HLto64, mkexpr(t3), mkexpr(t2)),
             binop(Iop_32HLto64, mkexpr(t1), mkexpr(t0))
   );
}

/* Break a 64-bit value up into four 16-bit ints. */

static void breakup64to16s ( IRTemp t64,
                             /*OUTs*/
                             IRTemp* t3, IRTemp* t2,
                             IRTemp* t1, IRTemp* t0 )
{
   IRTemp hi32 = newTemp(Ity_I32);
   IRTemp lo32 = newTemp(Ity_I32);
   assign( hi32, unop(Iop_64HIto32, mkexpr(t64)) );
   assign( lo32, unop(Iop_64to32,   mkexpr(t64)) );

   vassert(t0 && *t0 == IRTemp_INVALID);
   vassert(t1 && *t1 == IRTemp_INVALID);
   vassert(t2 && *t2 == IRTemp_INVALID);
   vassert(t3 && *t3 == IRTemp_INVALID);

   *t0 = newTemp(Ity_I16);
   *t1 = newTemp(Ity_I16);
   *t2 = newTemp(Ity_I16);
   *t3 = newTemp(Ity_I16);
   assign( *t0, unop(Iop_32to16,   mkexpr(lo32)) );
   assign( *t1, unop(Iop_32HIto16, mkexpr(lo32)) );
   assign( *t2, unop(Iop_32to16,   mkexpr(hi32)) );
   assign( *t3, unop(Iop_32HIto16, mkexpr(hi32)) );
}

/* Construct a 64-bit value from four 16-bit ints. */

static IRExpr* mk64from16s ( IRTemp t3, IRTemp t2,
                             IRTemp t1, IRTemp t0 )
{
   return
      binop( Iop_32HLto64,
             binop(Iop_16HLto32, mkexpr(t3), mkexpr(t2)),
             binop(Iop_16HLto32, mkexpr(t1), mkexpr(t0))
   );
}

/* Generate IR to set the guest %EFLAGS from the pushfl-format image
   in the given 32-bit temporary.  The flags that are set are: O S Z A
   C P D ID AC.

   In all cases, code to set AC is generated.  However, VEX actually
   ignores the AC value and so can optionally emit an emulation
   warning when it is enabled.  In this routine, an emulation warning
   is only emitted if emit_AC_emwarn is True, in which case
   next_insn_EIP must be correct (this allows for correct code
   generation for popfl/popfw).  If emit_AC_emwarn is False,
   next_insn_EIP is unimportant (this allows for easy if kludgey code
   generation for IRET.) */

static 
void set_EFLAGS_from_value ( IRTemp t1, 
                             Bool   emit_AC_emwarn,
                             Addr32 next_insn_EIP )
{
   vassert(typeOfIRTemp(irsb->tyenv,t1) == Ity_I32);

   /* t1 is the flag word.  Mask out everything except OSZACP and set
      the flags thunk to X86G_CC_OP_COPY. */
   stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
   stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
   stmt( IRStmt_Put( OFFB_CC_DEP1, 
                     binop(Iop_And32,
                           mkexpr(t1), 
                           mkU32( X86G_CC_MASK_C | X86G_CC_MASK_P 
                                  | X86G_CC_MASK_A | X86G_CC_MASK_Z 
                                  | X86G_CC_MASK_S| X86G_CC_MASK_O )
                          )
                    )
       );
   /* Set NDEP even though it isn't used.  This makes redundant-PUT
      elimination of previous stores to this field work better. */
   stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));

   /* Also need to set the D flag, which is held in bit 10 of t1.
      If zero, put 1 in OFFB_DFLAG, else -1 in OFFB_DFLAG. */
   stmt( IRStmt_Put( 
            OFFB_DFLAG,
            IRExpr_ITE( 
               unop(Iop_32to1,
                    binop(Iop_And32, 
                          binop(Iop_Shr32, mkexpr(t1), mkU8(10)), 
                          mkU32(1))),
               mkU32(0xFFFFFFFF),
               mkU32(1)))
       );

   /* Set the ID flag */
   stmt( IRStmt_Put( 
            OFFB_IDFLAG,
            IRExpr_ITE( 
               unop(Iop_32to1,
                    binop(Iop_And32, 
                          binop(Iop_Shr32, mkexpr(t1), mkU8(21)), 
                          mkU32(1))),
               mkU32(1),
               mkU32(0)))
       );

   /* And set the AC flag.  If setting it 1 to, possibly emit an
      emulation warning. */
   stmt( IRStmt_Put( 
            OFFB_ACFLAG,
            IRExpr_ITE( 
               unop(Iop_32to1,
                    binop(Iop_And32, 
                          binop(Iop_Shr32, mkexpr(t1), mkU8(18)), 
                          mkU32(1))),
               mkU32(1),
               mkU32(0)))
       );

   if (emit_AC_emwarn) {
      put_emwarn( mkU32(EmWarn_X86_acFlag) );
      stmt( 
         IRStmt_Exit(
            binop( Iop_CmpNE32, 
                   binop(Iop_And32, mkexpr(t1), mkU32(1<<18)), 
                   mkU32(0) ),
            Ijk_EmWarn,
            IRConst_U32( next_insn_EIP ),
            OFFB_EIP
         )
      );
   }
}


/* Helper for the SSSE3 (not SSE3) PMULHRSW insns.  Given two 64-bit
   values (aa,bb), computes, for each of the 4 16-bit lanes:

   (((aa_lane *s32 bb_lane) >>u 14) + 1) >>u 1
*/
static IRExpr* dis_PMULHRSW_helper ( IRExpr* aax, IRExpr* bbx )
{
   IRTemp aa      = newTemp(Ity_I64);
   IRTemp bb      = newTemp(Ity_I64);
   IRTemp aahi32s = newTemp(Ity_I64);
   IRTemp aalo32s = newTemp(Ity_I64);
   IRTemp bbhi32s = newTemp(Ity_I64);
   IRTemp bblo32s = newTemp(Ity_I64);
   IRTemp rHi     = newTemp(Ity_I64);
   IRTemp rLo     = newTemp(Ity_I64);
   IRTemp one32x2 = newTemp(Ity_I64);
   assign(aa, aax);
   assign(bb, bbx);
   assign( aahi32s,
           binop(Iop_SarN32x2,
                 binop(Iop_InterleaveHI16x4, mkexpr(aa), mkexpr(aa)),
                 mkU8(16) ));
   assign( aalo32s,
           binop(Iop_SarN32x2,
                 binop(Iop_InterleaveLO16x4, mkexpr(aa), mkexpr(aa)),
                 mkU8(16) ));
   assign( bbhi32s,
           binop(Iop_SarN32x2,
                 binop(Iop_InterleaveHI16x4, mkexpr(bb), mkexpr(bb)),
                 mkU8(16) ));
   assign( bblo32s,
           binop(Iop_SarN32x2,
                 binop(Iop_InterleaveLO16x4, mkexpr(bb), mkexpr(bb)),
                 mkU8(16) ));
   assign(one32x2, mkU64( (1ULL << 32) + 1 ));
   assign(
      rHi,
      binop(
         Iop_ShrN32x2,
         binop(
            Iop_Add32x2, 
            binop(
               Iop_ShrN32x2,
               binop(Iop_Mul32x2, mkexpr(aahi32s), mkexpr(bbhi32s)),
               mkU8(14)
            ),
            mkexpr(one32x2)
         ),
         mkU8(1)
      )
   );
   assign(
      rLo,
      binop(
         Iop_ShrN32x2,
         binop(
            Iop_Add32x2, 
            binop(
               Iop_ShrN32x2,
               binop(Iop_Mul32x2, mkexpr(aalo32s), mkexpr(bblo32s)),
               mkU8(14)
            ),
            mkexpr(one32x2)
         ),
         mkU8(1)
      )
   );
   return
      binop(Iop_CatEvenLanes16x4, mkexpr(rHi), mkexpr(rLo));
}

/* Helper for the SSSE3 (not SSE3) PSIGN{B,W,D} insns.  Given two 64-bit
   values (aa,bb), computes, for each lane:

          if aa_lane < 0 then - bb_lane
     else if aa_lane > 0 then bb_lane
     else 0
*/
static IRExpr* dis_PSIGN_helper ( IRExpr* aax, IRExpr* bbx, Int laneszB )
{
   IRTemp aa       = newTemp(Ity_I64);
   IRTemp bb       = newTemp(Ity_I64);
   IRTemp zero     = newTemp(Ity_I64);
   IRTemp bbNeg    = newTemp(Ity_I64);
   IRTemp negMask  = newTemp(Ity_I64);
   IRTemp posMask  = newTemp(Ity_I64);
   IROp   opSub    = Iop_INVALID;
   IROp   opCmpGTS = Iop_INVALID;

   switch (laneszB) {
      case 1: opSub = Iop_Sub8x8;  opCmpGTS = Iop_CmpGT8Sx8;  break;
      case 2: opSub = Iop_Sub16x4; opCmpGTS = Iop_CmpGT16Sx4; break;
      case 4: opSub = Iop_Sub32x2; opCmpGTS = Iop_CmpGT32Sx2; break;
      default: vassert(0);
   }

   assign( aa,      aax );
   assign( bb,      bbx );
   assign( zero,    mkU64(0) );
   assign( bbNeg,   binop(opSub,    mkexpr(zero), mkexpr(bb)) );
   assign( negMask, binop(opCmpGTS, mkexpr(zero), mkexpr(aa)) );
   assign( posMask, binop(opCmpGTS, mkexpr(aa),   mkexpr(zero)) );

   return
      binop(Iop_Or64,
            binop(Iop_And64, mkexpr(bb),    mkexpr(posMask)),
            binop(Iop_And64, mkexpr(bbNeg), mkexpr(negMask)) );

}

/* Helper for the SSSE3 (not SSE3) PABS{B,W,D} insns.  Given a 64-bit
   value aa, computes, for each lane

   if aa < 0 then -aa else aa

   Note that the result is interpreted as unsigned, so that the
   absolute value of the most negative signed input can be
   represented.
*/
static IRExpr* dis_PABS_helper ( IRExpr* aax, Int laneszB )
{
   IRTemp aa      = newTemp(Ity_I64);
   IRTemp zero    = newTemp(Ity_I64);
   IRTemp aaNeg   = newTemp(Ity_I64);
   IRTemp negMask = newTemp(Ity_I64);
   IRTemp posMask = newTemp(Ity_I64);
   IROp   opSub   = Iop_INVALID;
   IROp   opSarN  = Iop_INVALID;

   switch (laneszB) {
      case 1: opSub = Iop_Sub8x8;  opSarN = Iop_SarN8x8;  break;
      case 2: opSub = Iop_Sub16x4; opSarN = Iop_SarN16x4; break;
      case 4: opSub = Iop_Sub32x2; opSarN = Iop_SarN32x2; break;
      default: vassert(0);
   }

   assign( aa,      aax );
   assign( negMask, binop(opSarN, mkexpr(aa), mkU8(8*laneszB-1)) );
   assign( posMask, unop(Iop_Not64, mkexpr(negMask)) );
   assign( zero,    mkU64(0) );
   assign( aaNeg,   binop(opSub, mkexpr(zero), mkexpr(aa)) );
   return
      binop(Iop_Or64,
            binop(Iop_And64, mkexpr(aa),    mkexpr(posMask)),
            binop(Iop_And64, mkexpr(aaNeg), mkexpr(negMask)) );
}

static IRExpr* dis_PALIGNR_XMM_helper ( IRTemp hi64,
                                        IRTemp lo64, Int byteShift )
{
   vassert(byteShift >= 1 && byteShift <= 7);
   return
      binop(Iop_Or64,
            binop(Iop_Shl64, mkexpr(hi64), mkU8(8*(8-byteShift))),
            binop(Iop_Shr64, mkexpr(lo64), mkU8(8*byteShift))
      );
}

/* Generate a SIGSEGV followed by a restart of the current instruction
   if effective_addr is not 16-aligned.  This is required behaviour
   for some SSE3 instructions and all 128-bit SSSE3 instructions.
   This assumes that guest_RIP_curr_instr is set correctly! */
static void gen_SEGV_if_not_16_aligned ( IRTemp effective_addr )
{
   stmt(
      IRStmt_Exit(
         binop(Iop_CmpNE32,
               binop(Iop_And32,mkexpr(effective_addr),mkU32(0xF)),
               mkU32(0)),
         Ijk_SigSEGV,
         IRConst_U32(guest_EIP_curr_instr),
         OFFB_EIP
      )
   );
}


/* Helper for deciding whether a given insn (starting at the opcode
   byte) may validly be used with a LOCK prefix.  The following insns
   may be used with LOCK when their destination operand is in memory.
   AFAICS this is exactly the same for both 32-bit and 64-bit mode.

   ADD        80 /0,  81 /0,  82 /0,  83 /0,  00,  01
   OR         80 /1,  81 /1,  82 /x,  83 /1,  08,  09
   ADC        80 /2,  81 /2,  82 /2,  83 /2,  10,  11
   SBB        81 /3,  81 /3,  82 /x,  83 /3,  18,  19
   AND        80 /4,  81 /4,  82 /x,  83 /4,  20,  21
   SUB        80 /5,  81 /5,  82 /x,  83 /5,  28,  29
   XOR        80 /6,  81 /6,  82 /x,  83 /6,  30,  31

   DEC        FE /1,  FF /1
   INC        FE /0,  FF /0

   NEG        F6 /3,  F7 /3
   NOT        F6 /2,  F7 /2

   XCHG       86, 87

   BTC        0F BB,  0F BA /7
   BTR        0F B3,  0F BA /6
   BTS        0F AB,  0F BA /5

   CMPXCHG    0F B0,  0F B1
   CMPXCHG8B  0F C7 /1

   XADD       0F C0,  0F C1

   ------------------------------

   80 /0  =  addb $imm8,  rm8
   81 /0  =  addl $imm32, rm32  and  addw $imm16, rm16
   82 /0  =  addb $imm8,  rm8
   83 /0  =  addl $simm8, rm32  and  addw $simm8, rm16

   00     =  addb r8,  rm8
   01     =  addl r32, rm32  and  addw r16, rm16

   Same for ADD OR ADC SBB AND SUB XOR

   FE /1  = dec rm8
   FF /1  = dec rm32  and  dec rm16

   FE /0  = inc rm8
   FF /0  = inc rm32  and  inc rm16

   F6 /3  = neg rm8
   F7 /3  = neg rm32  and  neg rm16

   F6 /2  = not rm8
   F7 /2  = not rm32  and  not rm16

   0F BB     = btcw r16, rm16    and  btcl r32, rm32
   OF BA /7  = btcw $imm8, rm16  and  btcw $imm8, rm32

   Same for BTS, BTR
*/
static Bool can_be_used_with_LOCK_prefix ( UChar* opc )
{
   switch (opc[0]) {
      case 0x00: case 0x01: case 0x08: case 0x09:
      case 0x10: case 0x11: case 0x18: case 0x19:
      case 0x20: case 0x21: case 0x28: case 0x29:
      case 0x30: case 0x31:
         if (!epartIsReg(opc[1]))
            return True;
         break;

      case 0x80: case 0x81: case 0x82: case 0x83:
         if (gregOfRM(opc[1]) >= 0 && gregOfRM(opc[1]) <= 6
             && !epartIsReg(opc[1]))
            return True;
         break;

      case 0xFE: case 0xFF:
         if (gregOfRM(opc[1]) >= 0 && gregOfRM(opc[1]) <= 1
             && !epartIsReg(opc[1]))
            return True;
         break;

      case 0xF6: case 0xF7:
         if (gregOfRM(opc[1]) >= 2 && gregOfRM(opc[1]) <= 3
             && !epartIsReg(opc[1]))
            return True;
         break;

      case 0x86: case 0x87:
         if (!epartIsReg(opc[1]))
            return True;
         break;

      case 0x0F: {
         switch (opc[1]) {
            case 0xBB: case 0xB3: case 0xAB:
               if (!epartIsReg(opc[2]))
                  return True;
               break;
            case 0xBA: 
               if (gregOfRM(opc[2]) >= 5 && gregOfRM(opc[2]) <= 7
                   && !epartIsReg(opc[2]))
                  return True;
               break;
            case 0xB0: case 0xB1:
               if (!epartIsReg(opc[2]))
                  return True;
               break;
            case 0xC7: 
               if (gregOfRM(opc[2]) == 1 && !epartIsReg(opc[2]) )
                  return True;
               break;
            case 0xC0: case 0xC1:
               if (!epartIsReg(opc[2]))
                  return True;
               break;
            default:
               break;
         } /* switch (opc[1]) */
         break;
      }

      default:
         break;
   } /* switch (opc[0]) */

   return False;
}

static IRTemp math_BSWAP ( IRTemp t1, IRType ty )
{
   IRTemp t2 = newTemp(ty);
   if (ty == Ity_I32) {
      assign( t2,
         binop(
            Iop_Or32,
            binop(Iop_Shl32, mkexpr(t1), mkU8(24)),
            binop(
               Iop_Or32,
               binop(Iop_And32, binop(Iop_Shl32, mkexpr(t1), mkU8(8)),
                                mkU32(0x00FF0000)),
               binop(Iop_Or32,
                     binop(Iop_And32, binop(Iop_Shr32, mkexpr(t1), mkU8(8)),
                                      mkU32(0x0000FF00)),
                     binop(Iop_And32, binop(Iop_Shr32, mkexpr(t1), mkU8(24)),
                                      mkU32(0x000000FF) )
            )))
      );
      return t2;
   }
   if (ty == Ity_I16) {
      assign(t2, 
             binop(Iop_Or16,
                   binop(Iop_Shl16, mkexpr(t1), mkU8(8)),
                   binop(Iop_Shr16, mkexpr(t1), mkU8(8)) ));
      return t2;
   }
   vassert(0);
   /*NOTREACHED*/
   return IRTemp_INVALID;
}

/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction is
   located in host memory at &guest_code[delta].  *expect_CAS is set
   to True if the resulting IR is expected to contain an IRCAS
   statement, and False if it's not expected to.  This makes it
   possible for the caller of disInstr_X86_WRK to check that
   LOCK-prefixed instructions are at least plausibly translated, in
   that it becomes possible to check that a (validly) LOCK-prefixed
   instruction generates a translation containing an IRCAS, and
   instructions without LOCK prefixes don't generate translations
   containing an IRCAS.
*/
static
DisResult disInstr_X86_WRK (
             /*OUT*/Bool* expect_CAS,
             Bool         (*resteerOkFn) ( /*opaque*/void*, Addr64 ),
             Bool         resteerCisOk,
             void*        callback_opaque,
             Long         delta64,
             VexArchInfo* archinfo,
             VexAbiInfo*  vbi,
             Bool         sigill_diag
          )
{
   IRType    ty;
   IRTemp    addr, t0, t1, t2, t3, t4, t5, t6;
   Int       alen;
   UChar     opc, modrm, abyte, pre;
   UInt      d32;
   HChar     dis_buf[50];
   Int       am_sz, d_sz, n_prefixes;
   DisResult dres;
   UChar*    insn; /* used in SSE decoders */

   /* The running delta */
   Int delta = (Int)delta64;

   /* Holds eip at the start of the insn, so that we can print
      consistent error messages for unimplemented insns. */
   Int delta_start = delta;

   /* sz denotes the nominal data-op size of the insn; we change it to
      2 if an 0x66 prefix is seen */
   Int sz = 4;

   /* sorb holds the segment-override-prefix byte, if any.  Zero if no
      prefix has been seen, else one of {0x26, 0x3E, 0x64, 0x65}
      indicating the prefix.  */
   UChar sorb = 0;

   /* Gets set to True if a LOCK prefix is seen. */
   Bool pfx_lock = False;

   /* Set result defaults. */
   dres.whatNext    = Dis_Continue;
   dres.len         = 0;
   dres.continueAt  = 0;
   dres.jk_StopHere = Ijk_INVALID;

   *expect_CAS = False;

   addr = t0 = t1 = t2 = t3 = t4 = t5 = t6 = IRTemp_INVALID; 

   vassert(guest_EIP_bbstart + delta == guest_EIP_curr_instr);
   DIP("\t0x%x:  ", guest_EIP_bbstart+delta);

   /* Spot "Special" instructions (see comment at top of file). */
   {
      UChar* code = (UChar*)(guest_code + delta);
      /* Spot the 12-byte preamble:
         C1C703   roll $3,  %edi
         C1C70D   roll $13, %edi
         C1C71D   roll $29, %edi
         C1C713   roll $19, %edi
      */
      if (code[ 0] == 0xC1 && code[ 1] == 0xC7 && code[ 2] == 0x03 &&
          code[ 3] == 0xC1 && code[ 4] == 0xC7 && code[ 5] == 0x0D &&
          code[ 6] == 0xC1 && code[ 7] == 0xC7 && code[ 8] == 0x1D &&
          code[ 9] == 0xC1 && code[10] == 0xC7 && code[11] == 0x13) {
         /* Got a "Special" instruction preamble.  Which one is it? */
         if (code[12] == 0x87 && code[13] == 0xDB /* xchgl %ebx,%ebx */) {
            /* %EDX = client_request ( %EAX ) */
            DIP("%%edx = client_request ( %%eax )\n");
            delta += 14;
            jmp_lit(&dres, Ijk_ClientReq, guest_EIP_bbstart+delta);
            vassert(dres.whatNext == Dis_StopHere);
            goto decode_success;
         }
         else
         if (code[12] == 0x87 && code[13] == 0xC9 /* xchgl %ecx,%ecx */) {
            /* %EAX = guest_NRADDR */
            DIP("%%eax = guest_NRADDR\n");
            delta += 14;
            putIReg(4, R_EAX, IRExpr_Get( OFFB_NRADDR, Ity_I32 ));
            goto decode_success;
         }
         else
         if (code[12] == 0x87 && code[13] == 0xD2 /* xchgl %edx,%edx */) {
            /* call-noredir *%EAX */
            DIP("call-noredir *%%eax\n");
            delta += 14;
            t1 = newTemp(Ity_I32);
            assign(t1, getIReg(4,R_EAX));
            t2 = newTemp(Ity_I32);
            assign(t2, binop(Iop_Sub32, getIReg(4,R_ESP), mkU32(4)));
            putIReg(4, R_ESP, mkexpr(t2));
            storeLE( mkexpr(t2), mkU32(guest_EIP_bbstart+delta));
            jmp_treg(&dres, Ijk_NoRedir, t1);
            vassert(dres.whatNext == Dis_StopHere);
            goto decode_success;
         }
         else
         if (code[12] == 0x87 && code[13] == 0xFF /* xchgl %edi,%edi */) {
            /* IR injection */
            DIP("IR injection\n");
            vex_inject_ir(irsb, Iend_LE);

            // Invalidate the current insn. The reason is that the IRop we're
            // injecting here can change. In which case the translation has to
            // be redone. For ease of handling, we simply invalidate all the
            // time.
            stmt(IRStmt_Put(OFFB_CMSTART, mkU32(guest_EIP_curr_instr)));
            stmt(IRStmt_Put(OFFB_CMLEN,   mkU32(14)));
   
            delta += 14;

            stmt( IRStmt_Put( OFFB_EIP, mkU32(guest_EIP_bbstart + delta) ) );
            dres.whatNext    = Dis_StopHere;
            dres.jk_StopHere = Ijk_InvalICache;
            goto decode_success;
         }
         /* We don't know what it is. */
         goto decode_failure;
         /*NOTREACHED*/
      }
   }

   /* Handle a couple of weird-ass NOPs that have been observed in the
      wild. */
   {
      UChar* code = (UChar*)(guest_code + delta);
      /* Sun's JVM 1.5.0 uses the following as a NOP:
         26 2E 64 65 90  %es:%cs:%fs:%gs:nop */
      if (code[0] == 0x26 && code[1] == 0x2E && code[2] == 0x64 
          && code[3] == 0x65 && code[4] == 0x90) {
         DIP("%%es:%%cs:%%fs:%%gs:nop\n");
         delta += 5;
         goto decode_success;
      }
      /* Don't barf on recent binutils padding,
         all variants of which are: nopw %cs:0x0(%eax,%eax,1)
         66 2e 0f 1f 84 00 00 00 00 00
         66 66 2e 0f 1f 84 00 00 00 00 00
         66 66 66 2e 0f 1f 84 00 00 00 00 00
         66 66 66 66 2e 0f 1f 84 00 00 00 00 00
         66 66 66 66 66 2e 0f 1f 84 00 00 00 00 00
         66 66 66 66 66 66 2e 0f 1f 84 00 00 00 00 00
      */
      if (code[0] == 0x66) {
         Int data16_cnt;
         for (data16_cnt = 1; data16_cnt < 6; data16_cnt++)
            if (code[data16_cnt] != 0x66)
               break;
         if (code[data16_cnt] == 0x2E && code[data16_cnt + 1] == 0x0F
             && code[data16_cnt + 2] == 0x1F && code[data16_cnt + 3] == 0x84
             && code[data16_cnt + 4] == 0x00 && code[data16_cnt + 5] == 0x00
             && code[data16_cnt + 6] == 0x00 && code[data16_cnt + 7] == 0x00
             && code[data16_cnt + 8] == 0x00 ) {
            DIP("nopw %%cs:0x0(%%eax,%%eax,1)\n");
            delta += 9 + data16_cnt;
            goto decode_success;
         }
      }
   }       

   /* Normal instruction handling starts here. */

   /* Deal with some but not all prefixes: 
         66(oso)
         F0(lock)
         2E(cs:) 3E(ds:) 26(es:) 64(fs:) 65(gs:) 36(ss:)
      Not dealt with (left in place):
         F2 F3
   */
   n_prefixes = 0;
   while (True) {
      if (n_prefixes > 7) goto decode_failure;
      pre = getUChar(delta);
      switch (pre) {
         case 0x66: 
            sz = 2;
            break;
         case 0xF0: 
            pfx_lock = True; 
            *expect_CAS = True;
            break;
         case 0x3E: /* %DS: */
         case 0x26: /* %ES: */
         case 0x64: /* %FS: */
         case 0x65: /* %GS: */
            if (sorb != 0) 
               goto decode_failure; /* only one seg override allowed */
            sorb = pre;
            break;
         case 0x2E: { /* %CS: */
            /* 2E prefix on a conditional branch instruction is a
               branch-prediction hint, which can safely be ignored.  */
            UChar op1 = getIByte(delta+1);
            UChar op2 = getIByte(delta+2);
            if ((op1 >= 0x70 && op1 <= 0x7F)
                || (op1 == 0xE3)
                || (op1 == 0x0F && op2 >= 0x80 && op2 <= 0x8F)) {
               if (0) vex_printf("vex x86->IR: ignoring branch hint\n");
            } else {
               /* All other CS override cases are not handled */
               goto decode_failure;
            }
            break;
         }
         case 0x36: /* %SS: */
            /* SS override cases are not handled */
            goto decode_failure;
         default: 
            goto not_a_prefix;
      }
      n_prefixes++;
      delta++;
   }

   not_a_prefix:

   /* Now we should be looking at the primary opcode byte or the
      leading F2 or F3.  Check that any LOCK prefix is actually
      allowed. */

   if (pfx_lock) {
      if (can_be_used_with_LOCK_prefix( (UChar*)&guest_code[delta] )) {
         DIP("lock ");
      } else {
         *expect_CAS = False;
         goto decode_failure;
      }
   }


   /* ---------------------------------------------------- */
   /* --- The SSE decoder.                             --- */
   /* ---------------------------------------------------- */

   /* What did I do to deserve SSE ?  Perhaps I was really bad in a
      previous life? */

   /* Note, this doesn't handle SSE2 or SSE3.  That is handled in a
      later section, further on. */

   insn = (UChar*)&guest_code[delta];

   /* Treat fxsave specially.  It should be doable even on an SSE0
      (Pentium-II class) CPU.  Hence be prepared to handle it on
      any subarchitecture variant.
   */

   /* 0F AE /0 = FXSAVE m512 -- write x87 and SSE state to memory */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xAE
       && !epartIsReg(insn[2]) && gregOfRM(insn[2]) == 0) {
      IRDirty* d;
      modrm = getIByte(delta+2);
      vassert(sz == 4);
      vassert(!epartIsReg(modrm));

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;
      gen_SEGV_if_not_16_aligned(addr);

      DIP("fxsave %s\n", dis_buf);

      /* Uses dirty helper: 
            void x86g_do_FXSAVE ( VexGuestX86State*, UInt ) */
      d = unsafeIRDirty_0_N ( 
             0/*regparms*/, 
             "x86g_dirtyhelper_FXSAVE", 
             &x86g_dirtyhelper_FXSAVE,
             mkIRExprVec_2( IRExpr_BBPTR(), mkexpr(addr) )
          );

      /* declare we're writing memory */
      d->mFx   = Ifx_Write;
      d->mAddr = mkexpr(addr);
      d->mSize = 464; /* according to recent Intel docs */

      /* declare we're reading guest state */
      d->nFxState = 7;
      vex_bzero(&d->fxState, sizeof(d->fxState));

      d->fxState[0].fx     = Ifx_Read;
      d->fxState[0].offset = OFFB_FTOP;
      d->fxState[0].size   = sizeof(UInt);

      d->fxState[1].fx     = Ifx_Read;
      d->fxState[1].offset = OFFB_FPREGS;
      d->fxState[1].size   = 8 * sizeof(ULong);

      d->fxState[2].fx     = Ifx_Read;
      d->fxState[2].offset = OFFB_FPTAGS;
      d->fxState[2].size   = 8 * sizeof(UChar);

      d->fxState[3].fx     = Ifx_Read;
      d->fxState[3].offset = OFFB_FPROUND;
      d->fxState[3].size   = sizeof(UInt);

      d->fxState[4].fx     = Ifx_Read;
      d->fxState[4].offset = OFFB_FC3210;
      d->fxState[4].size   = sizeof(UInt);

      d->fxState[5].fx     = Ifx_Read;
      d->fxState[5].offset = OFFB_XMM0;
      d->fxState[5].size   = 8 * sizeof(U128);

      d->fxState[6].fx     = Ifx_Read;
      d->fxState[6].offset = OFFB_SSEROUND;
      d->fxState[6].size   = sizeof(UInt);

      /* Be paranoid ... this assertion tries to ensure the 8 %xmm
	 images are packed back-to-back.  If not, the value of
	 d->fxState[5].size is wrong. */
      vassert(16 == sizeof(U128));
      vassert(OFFB_XMM7 == (OFFB_XMM0 + 7 * 16));

      stmt( IRStmt_Dirty(d) );

      goto decode_success;
   }

   /* 0F AE /1 = FXRSTOR m512 -- read x87 and SSE state from memory */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xAE
       && !epartIsReg(insn[2]) && gregOfRM(insn[2]) == 1) {
      IRDirty* d;
      modrm = getIByte(delta+2);
      vassert(sz == 4);
      vassert(!epartIsReg(modrm));

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;
      gen_SEGV_if_not_16_aligned(addr);

      DIP("fxrstor %s\n", dis_buf);

      /* Uses dirty helper: 
            VexEmNote x86g_do_FXRSTOR ( VexGuestX86State*, UInt )
         NOTE:
            the VexEmNote value is simply ignored (unlike for FRSTOR)
      */
      d = unsafeIRDirty_0_N ( 
             0/*regparms*/, 
             "x86g_dirtyhelper_FXRSTOR", 
             &x86g_dirtyhelper_FXRSTOR,
             mkIRExprVec_2( IRExpr_BBPTR(), mkexpr(addr) )
          );

      /* declare we're reading memory */
      d->mFx   = Ifx_Read;
      d->mAddr = mkexpr(addr);
      d->mSize = 464; /* according to recent Intel docs */

      /* declare we're writing guest state */
      d->nFxState = 7;
      vex_bzero(&d->fxState, sizeof(d->fxState));

      d->fxState[0].fx     = Ifx_Write;
      d->fxState[0].offset = OFFB_FTOP;
      d->fxState[0].size   = sizeof(UInt);

      d->fxState[1].fx     = Ifx_Write;
      d->fxState[1].offset = OFFB_FPREGS;
      d->fxState[1].size   = 8 * sizeof(ULong);

      d->fxState[2].fx     = Ifx_Write;
      d->fxState[2].offset = OFFB_FPTAGS;
      d->fxState[2].size   = 8 * sizeof(UChar);

      d->fxState[3].fx     = Ifx_Write;
      d->fxState[3].offset = OFFB_FPROUND;
      d->fxState[3].size   = sizeof(UInt);

      d->fxState[4].fx     = Ifx_Write;
      d->fxState[4].offset = OFFB_FC3210;
      d->fxState[4].size   = sizeof(UInt);

      d->fxState[5].fx     = Ifx_Write;
      d->fxState[5].offset = OFFB_XMM0;
      d->fxState[5].size   = 8 * sizeof(U128);

      d->fxState[6].fx     = Ifx_Write;
      d->fxState[6].offset = OFFB_SSEROUND;
      d->fxState[6].size   = sizeof(UInt);

      /* Be paranoid ... this assertion tries to ensure the 8 %xmm
	 images are packed back-to-back.  If not, the value of
	 d->fxState[5].size is wrong. */
      vassert(16 == sizeof(U128));
      vassert(OFFB_XMM7 == (OFFB_XMM0 + 7 * 16));

      stmt( IRStmt_Dirty(d) );

      goto decode_success;
   }

   /* ------ SSE decoder main ------ */

   /* Skip parts of the decoder which don't apply given the stated
      guest subarchitecture. */
   if (archinfo->hwcaps == 0/*baseline, no sse at all*/)
      goto after_sse_decoders;

   /* With mmxext only some extended MMX instructions are recognized.
      The mmxext instructions are MASKMOVQ MOVNTQ PAVGB PAVGW PMAXSW
      PMAXUB PMINSW PMINUB PMULHUW PSADBW PSHUFW PEXTRW PINSRW PMOVMSKB
      PREFETCHNTA PREFETCHT0 PREFETCHT1 PREFETCHT2 SFENCE

      http://support.amd.com/us/Embedded_TechDocs/22466.pdf
      https://en.wikipedia.org/wiki/3DNow!#3DNow.21_extensions */

   if (archinfo->hwcaps == VEX_HWCAPS_X86_MMXEXT/*integer only sse1 subset*/)
      goto mmxext;

   /* Otherwise we must be doing sse1 or sse2, so we can at least try
      for SSE1 here. */

   /* 0F 58 = ADDPS -- add 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x58) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "addps", Iop_Add32Fx4 );
      goto decode_success;
   }

   /* F3 0F 58 = ADDSS -- add 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x58) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo32( sorb, delta+3, "addss", Iop_Add32F0x4 );
      goto decode_success;
   }

   /* 0F 55 = ANDNPS -- G = (not G) and E */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x55) {
      delta = dis_SSE_E_to_G_all_invG( sorb, delta+2, "andnps", Iop_AndV128 );
      goto decode_success;
   }

   /* 0F 54 = ANDPS -- G = G and E */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x54) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "andps", Iop_AndV128 );
      goto decode_success;
   }

   /* 0F C2 = CMPPS -- 32Fx4 comparison from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xC2) {
      delta = dis_SSEcmp_E_to_G( sorb, delta+2, "cmpps", True, 4 );
      goto decode_success;
   }

   /* F3 0F C2 = CMPSS -- 32F0x4 comparison from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0xC2) {
      vassert(sz == 4);
      delta = dis_SSEcmp_E_to_G( sorb, delta+3, "cmpss", False, 4 );
      goto decode_success;
   }

   /* 0F 2F = COMISS  -- 32F0x4 comparison G,E, and set ZCP */
   /* 0F 2E = UCOMISS -- 32F0x4 comparison G,E, and set ZCP */
   if (sz == 4 && insn[0] == 0x0F && (insn[1] == 0x2F || insn[1] == 0x2E)) {
      IRTemp argL = newTemp(Ity_F32);
      IRTemp argR = newTemp(Ity_F32);
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( argR, getXMMRegLane32F( eregOfRM(modrm), 0/*lowest lane*/ ) );
         delta += 2+1;
         DIP("[u]comiss %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)) );
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( argR, loadLE(Ity_F32, mkexpr(addr)) );
         delta += 2+alen;
         DIP("[u]comiss %s,%s\n", dis_buf,
                                  nameXMMReg(gregOfRM(modrm)) );
      }
      assign( argL, getXMMRegLane32F( gregOfRM(modrm), 0/*lowest lane*/ ) );

      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
      stmt( IRStmt_Put( 
               OFFB_CC_DEP1,
               binop( Iop_And32,
                      binop(Iop_CmpF64, 
                            unop(Iop_F32toF64,mkexpr(argL)),
                            unop(Iop_F32toF64,mkexpr(argR))),
                      mkU32(0x45)
          )));
      /* Set NDEP even though it isn't used.  This makes redundant-PUT
         elimination of previous stores to this field work better. */
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
      goto decode_success;
   }

   /* 0F 2A = CVTPI2PS -- convert 2 x I32 in mem/mmx to 2 x F32 in low
      half xmm */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x2A) {
      IRTemp arg64 = newTemp(Ity_I64);
      IRTemp rmode = newTemp(Ity_I32);
      vassert(sz == 4);

      modrm = getIByte(delta+2);
      do_MMX_preamble();
      if (epartIsReg(modrm)) {
         assign( arg64, getMMXReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("cvtpi2ps %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( arg64, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 2+alen;
         DIP("cvtpi2ps %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }

      assign( rmode, get_sse_roundingmode() );

      putXMMRegLane32F( 
         gregOfRM(modrm), 0,
         binop(Iop_F64toF32, 
               mkexpr(rmode),
               unop(Iop_I32StoF64, 
                    unop(Iop_64to32, mkexpr(arg64)) )) );

      putXMMRegLane32F(
         gregOfRM(modrm), 1, 
         binop(Iop_F64toF32, 
               mkexpr(rmode),
               unop(Iop_I32StoF64,
                    unop(Iop_64HIto32, mkexpr(arg64)) )) );

      goto decode_success;
   }

   /* F3 0F 2A = CVTSI2SS -- convert I32 in mem/ireg to F32 in low
      quarter xmm */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x2A) {
      IRTemp arg32 = newTemp(Ity_I32);
      IRTemp rmode = newTemp(Ity_I32);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         assign( arg32, getIReg(4, eregOfRM(modrm)) );
         delta += 3+1;
         DIP("cvtsi2ss %s,%s\n", nameIReg(4, eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign( arg32, loadLE(Ity_I32, mkexpr(addr)) );
         delta += 3+alen;
         DIP("cvtsi2ss %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }

      assign( rmode, get_sse_roundingmode() );

      putXMMRegLane32F( 
         gregOfRM(modrm), 0,
         binop(Iop_F64toF32,
               mkexpr(rmode),
               unop(Iop_I32StoF64, mkexpr(arg32)) ) );

      goto decode_success;
   }

   /* 0F 2D = CVTPS2PI -- convert 2 x F32 in mem/low half xmm to 2 x
      I32 in mmx, according to prevailing SSE rounding mode */
   /* 0F 2C = CVTTPS2PI -- convert 2 x F32 in mem/low half xmm to 2 x
      I32 in mmx, rounding towards zero */
   if (sz == 4 && insn[0] == 0x0F && (insn[1] == 0x2D || insn[1] == 0x2C)) {
      IRTemp dst64  = newTemp(Ity_I64);
      IRTemp rmode  = newTemp(Ity_I32);
      IRTemp f32lo  = newTemp(Ity_F32);
      IRTemp f32hi  = newTemp(Ity_F32);
      Bool   r2zero = toBool(insn[1] == 0x2C);

      do_MMX_preamble();
      modrm = getIByte(delta+2);

      if (epartIsReg(modrm)) {
         delta += 2+1;
	 assign(f32lo, getXMMRegLane32F(eregOfRM(modrm), 0));
	 assign(f32hi, getXMMRegLane32F(eregOfRM(modrm), 1));
         DIP("cvt%sps2pi %s,%s\n", r2zero ? "t" : "",
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign(f32lo, loadLE(Ity_F32, mkexpr(addr)));
	 assign(f32hi, loadLE(Ity_F32, binop( Iop_Add32, 
                                              mkexpr(addr), 
                                              mkU32(4) )));
         delta += 2+alen;
         DIP("cvt%sps2pi %s,%s\n", r2zero ? "t" : "",
                                   dis_buf,
                                   nameMMXReg(gregOfRM(modrm)));
      }

      if (r2zero) {
         assign(rmode, mkU32((UInt)Irrm_ZERO) );
      } else {
         assign( rmode, get_sse_roundingmode() );
      }

      assign( 
         dst64,
         binop( Iop_32HLto64,
                binop( Iop_F64toI32S, 
                       mkexpr(rmode), 
                       unop( Iop_F32toF64, mkexpr(f32hi) ) ),
                binop( Iop_F64toI32S, 
                       mkexpr(rmode), 
                       unop( Iop_F32toF64, mkexpr(f32lo) ) )
              )
      );

      putMMXReg(gregOfRM(modrm), mkexpr(dst64));
      goto decode_success;
   }

   /* F3 0F 2D = CVTSS2SI -- convert F32 in mem/low quarter xmm to
      I32 in ireg, according to prevailing SSE rounding mode */
   /* F3 0F 2C = CVTTSS2SI -- convert F32 in mem/low quarter xmm to
      I32 in ireg, rounding towards zero */
   if (insn[0] == 0xF3 && insn[1] == 0x0F 
       && (insn[2] == 0x2D || insn[2] == 0x2C)) {
      IRTemp rmode = newTemp(Ity_I32);
      IRTemp f32lo = newTemp(Ity_F32);
      Bool   r2zero = toBool(insn[2] == 0x2C);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         delta += 3+1;
	 assign(f32lo, getXMMRegLane32F(eregOfRM(modrm), 0));
         DIP("cvt%sss2si %s,%s\n", r2zero ? "t" : "",
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameIReg(4, gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign(f32lo, loadLE(Ity_F32, mkexpr(addr)));
         delta += 3+alen;
         DIP("cvt%sss2si %s,%s\n", r2zero ? "t" : "",
                                   dis_buf,
                                   nameIReg(4, gregOfRM(modrm)));
      }

      if (r2zero) {
         assign( rmode, mkU32((UInt)Irrm_ZERO) );
      } else {
         assign( rmode, get_sse_roundingmode() );
      }

      putIReg(4, gregOfRM(modrm),
                 binop( Iop_F64toI32S, 
                        mkexpr(rmode), 
                        unop( Iop_F32toF64, mkexpr(f32lo) ) )
      );

      goto decode_success;
   }

   /* 0F 5E = DIVPS -- div 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x5E) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "divps", Iop_Div32Fx4 );
      goto decode_success;
   }

   /* F3 0F 5E = DIVSS -- div 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5E) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo32( sorb, delta+3, "divss", Iop_Div32F0x4 );
      goto decode_success;
   }

   /* 0F AE /2 = LDMXCSR m32 -- load %mxcsr */
   if (insn[0] == 0x0F && insn[1] == 0xAE
       && !epartIsReg(insn[2]) && gregOfRM(insn[2]) == 2) {

      IRTemp t64 = newTemp(Ity_I64);
      IRTemp ew = newTemp(Ity_I32);

      modrm = getIByte(delta+2);
      vassert(!epartIsReg(modrm));
      vassert(sz == 4);

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;
      DIP("ldmxcsr %s\n", dis_buf);

      /* The only thing we observe in %mxcsr is the rounding mode.
         Therefore, pass the 32-bit value (SSE native-format control
         word) to a clean helper, getting back a 64-bit value, the
         lower half of which is the SSEROUND value to store, and the
         upper half of which is the emulation-warning token which may
         be generated.  
      */
      /* ULong x86h_check_ldmxcsr ( UInt ); */
      assign( t64, mkIRExprCCall(
                      Ity_I64, 0/*regparms*/, 
                      "x86g_check_ldmxcsr",
                      &x86g_check_ldmxcsr, 
                      mkIRExprVec_1( loadLE(Ity_I32, mkexpr(addr)) )
                   )
            );

      put_sse_roundingmode( unop(Iop_64to32, mkexpr(t64)) );
      assign( ew, unop(Iop_64HIto32, mkexpr(t64) ) );
      put_emwarn( mkexpr(ew) );
      /* Finally, if an emulation warning was reported, side-exit to
         the next insn, reporting the warning, so that Valgrind's
         dispatcher sees the warning. */
      stmt( 
         IRStmt_Exit(
            binop(Iop_CmpNE32, mkexpr(ew), mkU32(0)),
            Ijk_EmWarn,
            IRConst_U32( ((Addr32)guest_EIP_bbstart)+delta),
            OFFB_EIP
         )
      );
      goto decode_success;
   }


   /* mmxext sse1 subset starts here. mmxext only arches will parse
      only this subset of the sse1 instructions. */
  mmxext:

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F F7 = MASKMOVQ -- 8x8 masked store */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xF7) {
      Bool ok = False;
      delta = dis_MMX( &ok, sorb, sz, delta+1 );
      if (!ok)
         goto decode_failure;
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F E7 = MOVNTQ -- for us, just a plain MMX store.  Note, the
      Intel manual does not say anything about the usual business of
      the FP reg tags getting trashed whenever an MMX insn happens.
      So we just leave them alone. 
   */
   if (insn[0] == 0x0F && insn[1] == 0xE7) {
      modrm = getIByte(delta+2);
      if (sz == 4 && !epartIsReg(modrm)) {
         /* do_MMX_preamble(); Intel docs don't specify this */
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         storeLE( mkexpr(addr), getMMXReg(gregOfRM(modrm)) );
         DIP("movntq %s,%s\n", dis_buf,
                               nameMMXReg(gregOfRM(modrm)));
         delta += 2+alen;
         goto decode_success;
      }
      /* else fall through */
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F E0 = PAVGB -- 8x8 unsigned Packed Average, with rounding */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xE0) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pavgb", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F E3 = PAVGW -- 16x4 unsigned Packed Average, with rounding */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xE3) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pavgw", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F C5 = PEXTRW -- extract 16-bit field from mmx(E) and put 
      zero-extend of it in ireg(G). */
   if (insn[0] == 0x0F && insn[1] == 0xC5) {
      modrm = insn[2];
      if (sz == 4 && epartIsReg(modrm)) {
         IRTemp sV = newTemp(Ity_I64);
         t5 = newTemp(Ity_I16);
         do_MMX_preamble();
         assign(sV, getMMXReg(eregOfRM(modrm)));
         breakup64to16s( sV, &t3, &t2, &t1, &t0 );
         switch (insn[3] & 3) {
            case 0:  assign(t5, mkexpr(t0)); break;
            case 1:  assign(t5, mkexpr(t1)); break;
            case 2:  assign(t5, mkexpr(t2)); break;
            case 3:  assign(t5, mkexpr(t3)); break;
            default: vassert(0); /*NOTREACHED*/
         }
         putIReg(4, gregOfRM(modrm), unop(Iop_16Uto32, mkexpr(t5)));
         DIP("pextrw $%d,%s,%s\n",
             (Int)insn[3], nameMMXReg(eregOfRM(modrm)),
                           nameIReg(4,gregOfRM(modrm)));
         delta += 4;
         goto decode_success;
      } 
      /* else fall through */
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F C4 = PINSRW -- get 16 bits from E(mem or low half ireg) and
      put it into the specified lane of mmx(G). */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xC4) {
      /* Use t0 .. t3 to hold the 4 original 16-bit lanes of the
         mmx reg.  t4 is the new lane value.  t5 is the original
         mmx value. t6 is the new mmx value. */
      Int lane;
      t4 = newTemp(Ity_I16);
      t5 = newTemp(Ity_I64);
      t6 = newTemp(Ity_I64);
      modrm = insn[2];
      do_MMX_preamble();

      assign(t5, getMMXReg(gregOfRM(modrm)));
      breakup64to16s( t5, &t3, &t2, &t1, &t0 );

      if (epartIsReg(modrm)) {
         assign(t4, getIReg(2, eregOfRM(modrm)));
         delta += 3+1;
         lane = insn[3+1-1];
         DIP("pinsrw $%d,%s,%s\n", (Int)lane, 
                                   nameIReg(2,eregOfRM(modrm)),
                                   nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         delta += 3+alen;
         lane = insn[3+alen-1];
         assign(t4, loadLE(Ity_I16, mkexpr(addr)));
         DIP("pinsrw $%d,%s,%s\n", (Int)lane, 
                                   dis_buf,
                                   nameMMXReg(gregOfRM(modrm)));
      }

      switch (lane & 3) {
         case 0:  assign(t6, mk64from16s(t3,t2,t1,t4)); break;
         case 1:  assign(t6, mk64from16s(t3,t2,t4,t0)); break;
         case 2:  assign(t6, mk64from16s(t3,t4,t1,t0)); break;
         case 3:  assign(t6, mk64from16s(t4,t2,t1,t0)); break;
         default: vassert(0); /*NOTREACHED*/
      }
      putMMXReg(gregOfRM(modrm), mkexpr(t6));
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F EE = PMAXSW -- 16x4 signed max */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xEE) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pmaxsw", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F DE = PMAXUB -- 8x8 unsigned max */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xDE) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pmaxub", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F EA = PMINSW -- 16x4 signed min */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xEA) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pminsw", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F DA = PMINUB -- 8x8 unsigned min */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xDA) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pminub", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F D7 = PMOVMSKB -- extract sign bits from each of 8 lanes in
      mmx(E), turn them into a byte, and put zero-extend of it in
      ireg(G). */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xD7) {
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         do_MMX_preamble();
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I32);
         assign(t0, getMMXReg(eregOfRM(modrm)));
         assign(t1, unop(Iop_8Uto32, unop(Iop_GetMSBs8x8, mkexpr(t0))));
         putIReg(4, gregOfRM(modrm), mkexpr(t1));
         DIP("pmovmskb %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                 nameIReg(4,gregOfRM(modrm)));
         delta += 3;
         goto decode_success;
      } 
      /* else fall through */
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F E4 = PMULUH -- 16x4 hi-half of unsigned widening multiply */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xE4) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "pmuluh", False );
      goto decode_success;
   }

   /* 0F 18 /0 = PREFETCHNTA -- prefetch into caches, */
   /* 0F 18 /1 = PREFETCH0   -- with various different hints */
   /* 0F 18 /2 = PREFETCH1 */
   /* 0F 18 /3 = PREFETCH2 */
   if (insn[0] == 0x0F && insn[1] == 0x18
       && !epartIsReg(insn[2]) 
       && gregOfRM(insn[2]) >= 0 && gregOfRM(insn[2]) <= 3) {
      const HChar* hintstr = "??";

      modrm = getIByte(delta+2);
      vassert(!epartIsReg(modrm));

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;

      switch (gregOfRM(modrm)) {
         case 0: hintstr = "nta"; break;
         case 1: hintstr = "t0"; break;
         case 2: hintstr = "t1"; break;
         case 3: hintstr = "t2"; break;
         default: vassert(0); /*NOTREACHED*/
      }

      DIP("prefetch%s %s\n", hintstr, dis_buf);
      goto decode_success;
   }

   /* 0F 0D /0 = PREFETCH  m8 -- 3DNow! prefetch */
   /* 0F 0D /1 = PREFETCHW m8 -- ditto, with some other hint */
   if (insn[0] == 0x0F && insn[1] == 0x0D
       && !epartIsReg(insn[2]) 
       && gregOfRM(insn[2]) >= 0 && gregOfRM(insn[2]) <= 1) {
      const HChar* hintstr = "??";

      modrm = getIByte(delta+2);
      vassert(!epartIsReg(modrm));

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;

      switch (gregOfRM(modrm)) {
         case 0: hintstr = ""; break;
         case 1: hintstr = "w"; break;
         default: vassert(0); /*NOTREACHED*/
      }

      DIP("prefetch%s %s\n", hintstr, dis_buf);
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F F6 = PSADBW -- sum of 8Ux8 absolute differences */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xF6) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                 sorb, delta+2, insn[1], "psadbw", False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE1 ---*** */
   /* 0F 70 = PSHUFW -- rearrange 4x16 from E(mmx or mem) to G(mmx) */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x70) {
      Int order;
      IRTemp sV, dV, s3, s2, s1, s0;
      s3 = s2 = s1 = s0 = IRTemp_INVALID;
      sV = newTemp(Ity_I64);
      dV = newTemp(Ity_I64);
      do_MMX_preamble();
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         order = (Int)insn[3];
         delta += 2+2;
         DIP("pshufw $%d,%s,%s\n", order, 
                                   nameMMXReg(eregOfRM(modrm)),
                                   nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
	 order = (Int)insn[2+alen];
         delta += 3+alen;
         DIP("pshufw $%d,%s,%s\n", order, 
                                   dis_buf,
                                   nameMMXReg(gregOfRM(modrm)));
      }
      breakup64to16s( sV, &s3, &s2, &s1, &s0 );

#     define SEL(n) \
                ((n)==0 ? s0 : ((n)==1 ? s1 : ((n)==2 ? s2 : s3)))
      assign(dV,
	     mk64from16s( SEL((order>>6)&3), SEL((order>>4)&3),
                          SEL((order>>2)&3), SEL((order>>0)&3) )
      );
      putMMXReg(gregOfRM(modrm), mkexpr(dV));
#     undef SEL
      goto decode_success;
   }

   /* 0F AE /7 = SFENCE -- flush pending operations to memory */
   if (insn[0] == 0x0F && insn[1] == 0xAE
       && epartIsReg(insn[2]) && gregOfRM(insn[2]) == 7) {
      vassert(sz == 4);
      delta += 3;
      /* Insert a memory fence.  It's sometimes important that these
         are carried through to the generated code. */
      stmt( IRStmt_MBE(Imbe_Fence) );
      DIP("sfence\n");
      goto decode_success;
   }

   /* End of mmxext sse1 subset. No more sse parsing for mmxext only arches. */
   if (archinfo->hwcaps == VEX_HWCAPS_X86_MMXEXT/*integer only sse1 subset*/)
      goto after_sse_decoders;


   /* 0F 5F = MAXPS -- max 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x5F) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "maxps", Iop_Max32Fx4 );
      goto decode_success;
   }

   /* F3 0F 5F = MAXSS -- max 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5F) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo32( sorb, delta+3, "maxss", Iop_Max32F0x4 );
      goto decode_success;
   }

   /* 0F 5D = MINPS -- min 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x5D) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "minps", Iop_Min32Fx4 );
      goto decode_success;
   }

   /* F3 0F 5D = MINSS -- min 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5D) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo32( sorb, delta+3, "minss", Iop_Min32F0x4 );
      goto decode_success;
   }

   /* 0F 28 = MOVAPS -- move from E (mem or xmm) to G (xmm). */
   /* 0F 10 = MOVUPS -- move from E (mem or xmm) to G (xmm). */
   if (sz == 4 && insn[0] == 0x0F && (insn[1] == 0x28 || insn[1] == 0x10)) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         putXMMReg( gregOfRM(modrm), 
                    getXMMReg( eregOfRM(modrm) ));
         DIP("mov[ua]ps %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
         delta += 2+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         if (insn[1] == 0x28/*movaps*/)
            gen_SEGV_if_not_16_aligned( addr );
         putXMMReg( gregOfRM(modrm), 
                    loadLE(Ity_V128, mkexpr(addr)) );
         DIP("mov[ua]ps %s,%s\n", dis_buf,
                                  nameXMMReg(gregOfRM(modrm)));
         delta += 2+alen;
      }
      goto decode_success;
   }

   /* 0F 29 = MOVAPS -- move from G (xmm) to E (mem or xmm). */
   /* 0F 11 = MOVUPS -- move from G (xmm) to E (mem or xmm). */
   if (sz == 4 && insn[0] == 0x0F 
       && (insn[1] == 0x29 || insn[1] == 0x11)) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         /* fall through; awaiting test case */
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         if (insn[1] == 0x29/*movaps*/)
            gen_SEGV_if_not_16_aligned( addr );
         storeLE( mkexpr(addr), getXMMReg(gregOfRM(modrm)) );
         DIP("mov[ua]ps %s,%s\n", nameXMMReg(gregOfRM(modrm)),
                                  dis_buf );
         delta += 2+alen;
         goto decode_success;
      }
   }

   /* 0F 16 = MOVHPS -- move from mem to high half of XMM. */
   /* 0F 16 = MOVLHPS -- move from lo half to hi half of XMM. */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x16) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         delta += 2+1;
         putXMMRegLane64( gregOfRM(modrm), 1/*upper lane*/,
                          getXMMRegLane64( eregOfRM(modrm), 0 ) );
         DIP("movhps %s,%s\n", nameXMMReg(eregOfRM(modrm)), 
                               nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         putXMMRegLane64( gregOfRM(modrm), 1/*upper lane*/,
                          loadLE(Ity_I64, mkexpr(addr)) );
         DIP("movhps %s,%s\n", dis_buf, 
                               nameXMMReg( gregOfRM(modrm) ));
      }
      goto decode_success;
   }

   /* 0F 17 = MOVHPS -- move from high half of XMM to mem. */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x17) {
      if (!epartIsReg(insn[2])) {
         delta += 2;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;
         storeLE( mkexpr(addr), 
                  getXMMRegLane64( gregOfRM(insn[2]),
                                   1/*upper lane*/ ) );
         DIP("movhps %s,%s\n", nameXMMReg( gregOfRM(insn[2]) ),
                               dis_buf);
         goto decode_success;
      }
      /* else fall through */
   }

   /* 0F 12 = MOVLPS -- move from mem to low half of XMM. */
   /* OF 12 = MOVHLPS -- from from hi half to lo half of XMM. */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x12) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         delta += 2+1;
         putXMMRegLane64( gregOfRM(modrm),  
                          0/*lower lane*/,
                          getXMMRegLane64( eregOfRM(modrm), 1 ));
         DIP("movhlps %s, %s\n", nameXMMReg(eregOfRM(modrm)), 
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         putXMMRegLane64( gregOfRM(modrm),  0/*lower lane*/,
                          loadLE(Ity_I64, mkexpr(addr)) );
         DIP("movlps %s, %s\n", 
             dis_buf, nameXMMReg( gregOfRM(modrm) ));
      }
      goto decode_success;
   }

   /* 0F 13 = MOVLPS -- move from low half of XMM to mem. */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x13) {
      if (!epartIsReg(insn[2])) {
         delta += 2;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;
         storeLE( mkexpr(addr), 
                  getXMMRegLane64( gregOfRM(insn[2]), 
                                   0/*lower lane*/ ) );
         DIP("movlps %s, %s\n", nameXMMReg( gregOfRM(insn[2]) ),
                                dis_buf);
         goto decode_success;
      }
      /* else fall through */
   }

   /* 0F 50 = MOVMSKPS - move 4 sign bits from 4 x F32 in xmm(E)
      to 4 lowest bits of ireg(G) */
   if (insn[0] == 0x0F && insn[1] == 0x50) {
      modrm = getIByte(delta+2);
      if (sz == 4 && epartIsReg(modrm)) {
         Int src;
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         delta += 2+1;
         src = eregOfRM(modrm);
         assign( t0, binop( Iop_And32,
                            binop(Iop_Shr32, getXMMRegLane32(src,0), mkU8(31)),
                            mkU32(1) ));
         assign( t1, binop( Iop_And32,
                            binop(Iop_Shr32, getXMMRegLane32(src,1), mkU8(30)),
                            mkU32(2) ));
         assign( t2, binop( Iop_And32,
                            binop(Iop_Shr32, getXMMRegLane32(src,2), mkU8(29)),
                            mkU32(4) ));
         assign( t3, binop( Iop_And32,
                            binop(Iop_Shr32, getXMMRegLane32(src,3), mkU8(28)),
                            mkU32(8) ));
         putIReg(4, gregOfRM(modrm),
                    binop(Iop_Or32,
                          binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                          binop(Iop_Or32, mkexpr(t2), mkexpr(t3))
                         )
                 );
         DIP("movmskps %s,%s\n", nameXMMReg(src), 
                                 nameIReg(4, gregOfRM(modrm)));
         goto decode_success;
      }
      /* else fall through */
   }

   /* 0F 2B = MOVNTPS -- for us, just a plain SSE store. */
   /* 66 0F 2B = MOVNTPD -- for us, just a plain SSE store. */
   if (insn[0] == 0x0F && insn[1] == 0x2B) {
      modrm = getIByte(delta+2);
      if (!epartIsReg(modrm)) {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         storeLE( mkexpr(addr), getXMMReg(gregOfRM(modrm)) );
         DIP("movntp%s %s,%s\n", sz==2 ? "d" : "s",
                                 dis_buf,
                                 nameXMMReg(gregOfRM(modrm)));
         delta += 2+alen;
         goto decode_success;
      }
      /* else fall through */
   }

   /* F3 0F 10 = MOVSS -- move 32 bits from E (mem or lo 1/4 xmm) to G
      (lo 1/4 xmm).  If E is mem, upper 3/4 of G is zeroed out. */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x10) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         putXMMRegLane32( gregOfRM(modrm), 0,
                          getXMMRegLane32( eregOfRM(modrm), 0 ));
         DIP("movss %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                              nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         /* zero bits 127:64 */
         putXMMRegLane64( gregOfRM(modrm), 1, mkU64(0) ); 
         /* zero bits 63:32 */
         putXMMRegLane32( gregOfRM(modrm), 1, mkU32(0) ); 
         /* write bits 31:0 */
         putXMMRegLane32( gregOfRM(modrm), 0,
                          loadLE(Ity_I32, mkexpr(addr)) );
         DIP("movss %s,%s\n", dis_buf,
                              nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }
      goto decode_success;
   }

   /* F3 0F 11 = MOVSS -- move 32 bits from G (lo 1/4 xmm) to E (mem
      or lo 1/4 xmm). */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x11) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         /* fall through, we don't yet have a test case */
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         storeLE( mkexpr(addr),
                  getXMMRegLane32(gregOfRM(modrm), 0) );
         DIP("movss %s,%s\n", nameXMMReg(gregOfRM(modrm)),
                              dis_buf);
         delta += 3+alen;
         goto decode_success;
      }
   }

   /* 0F 59 = MULPS -- mul 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x59) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "mulps", Iop_Mul32Fx4 );
      goto decode_success;
   }

   /* F3 0F 59 = MULSS -- mul 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x59) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo32( sorb, delta+3, "mulss", Iop_Mul32F0x4 );
      goto decode_success;
   }

   /* 0F 56 = ORPS -- G = G and E */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x56) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "orps", Iop_OrV128 );
      goto decode_success;
   }

   /* 0F 53 = RCPPS -- approx reciprocal 32Fx4 from R/M to R */
   if (insn[0] == 0x0F && insn[1] == 0x53) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_unary_all( sorb, delta+2, 
                                        "rcpps", Iop_Recip32Fx4 );
      goto decode_success;
   }

   /* F3 0F 53 = RCPSS -- approx reciprocal 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x53) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_unary_lo32( sorb, delta+3, 
                                         "rcpss", Iop_Recip32F0x4 );
      goto decode_success;
   }

   /* 0F 52 = RSQRTPS -- approx reciprocal sqrt 32Fx4 from R/M to R */
   if (insn[0] == 0x0F && insn[1] == 0x52) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_unary_all( sorb, delta+2, 
                                        "rsqrtps", Iop_RSqrt32Fx4 );
      goto decode_success;
   }

   /* F3 0F 52 = RSQRTSS -- approx reciprocal sqrt 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x52) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_unary_lo32( sorb, delta+3, 
                                         "rsqrtss", Iop_RSqrt32F0x4 );
      goto decode_success;
   }

   /* 0F C6 /r ib = SHUFPS -- shuffle packed F32s */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xC6) {
      Int    select;
      IRTemp sV, dV;
      IRTemp s3, s2, s1, s0, d3, d2, d1, d0;
      sV = newTemp(Ity_V128);
      dV = newTemp(Ity_V128);
      s3 = s2 = s1 = s0 = d3 = d2 = d1 = d0 = IRTemp_INVALID;
      modrm = insn[2];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         select = (Int)insn[3];
         delta += 2+2;
         DIP("shufps $%d,%s,%s\n", select, 
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         select = (Int)insn[2+alen];
         delta += 3+alen;
         DIP("shufps $%d,%s,%s\n", select, 
                                   dis_buf,
                                   nameXMMReg(gregOfRM(modrm)));
      }

      breakup128to32s( dV, &d3, &d2, &d1, &d0 );
      breakup128to32s( sV, &s3, &s2, &s1, &s0 );

#     define SELD(n) ((n)==0 ? d0 : ((n)==1 ? d1 : ((n)==2 ? d2 : d3)))
#     define SELS(n) ((n)==0 ? s0 : ((n)==1 ? s1 : ((n)==2 ? s2 : s3)))

      putXMMReg(
         gregOfRM(modrm), 
         mk128from32s( SELS((select>>6)&3), SELS((select>>4)&3), 
                       SELD((select>>2)&3), SELD((select>>0)&3) )
      );

#     undef SELD
#     undef SELS

      goto decode_success;
   }

   /* 0F 51 = SQRTPS -- approx sqrt 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x51) {
      delta = dis_SSE_E_to_G_unary_all( sorb, delta+2, 
                                        "sqrtps", Iop_Sqrt32Fx4 );
      goto decode_success;
   }

   /* F3 0F 51 = SQRTSS -- approx sqrt 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x51) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_unary_lo32( sorb, delta+3, 
                                         "sqrtss", Iop_Sqrt32F0x4 );
      goto decode_success;
   }

   /* 0F AE /3 = STMXCSR m32 -- store %mxcsr */
   if (insn[0] == 0x0F && insn[1] == 0xAE
       && !epartIsReg(insn[2]) && gregOfRM(insn[2]) == 3) {
      modrm = getIByte(delta+2);
      vassert(sz == 4);
      vassert(!epartIsReg(modrm));

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;

      /* Fake up a native SSE mxcsr word.  The only thing it depends
         on is SSEROUND[1:0], so call a clean helper to cook it up. 
      */
      /* UInt x86h_create_mxcsr ( UInt sseround ) */
      DIP("stmxcsr %s\n", dis_buf);
      storeLE( mkexpr(addr), 
               mkIRExprCCall(
                  Ity_I32, 0/*regp*/,
                  "x86g_create_mxcsr", &x86g_create_mxcsr, 
                  mkIRExprVec_1( get_sse_roundingmode() ) 
               ) 
             );
      goto decode_success;
   }

   /* 0F 5C = SUBPS -- sub 32Fx4 from R/M to R */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x5C) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "subps", Iop_Sub32Fx4 );
      goto decode_success;
   }

   /* F3 0F 5C = SUBSS -- sub 32F0x4 from R/M to R */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5C) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo32( sorb, delta+3, "subss", Iop_Sub32F0x4 );
      goto decode_success;
   }

   /* 0F 15 = UNPCKHPS -- unpack and interleave high part F32s */
   /* 0F 14 = UNPCKLPS -- unpack and interleave low part F32s */
   /* These just appear to be special cases of SHUFPS */
   if (sz == 4 && insn[0] == 0x0F && (insn[1] == 0x15 || insn[1] == 0x14)) {
      IRTemp sV, dV;
      IRTemp s3, s2, s1, s0, d3, d2, d1, d0;
      Bool hi = toBool(insn[1] == 0x15);
      sV = newTemp(Ity_V128);
      dV = newTemp(Ity_V128);
      s3 = s2 = s1 = s0 = d3 = d2 = d1 = d0 = IRTemp_INVALID;
      modrm = insn[2];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("unpck%sps %s,%s\n", hi ? "h" : "l",
                                  nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("unpck%sps %s,%s\n", hi ? "h" : "l",
                                  dis_buf,
                                  nameXMMReg(gregOfRM(modrm)));
      }

      breakup128to32s( dV, &d3, &d2, &d1, &d0 );
      breakup128to32s( sV, &s3, &s2, &s1, &s0 );

      if (hi) {
         putXMMReg( gregOfRM(modrm), mk128from32s( s3, d3, s2, d2 ) );
      } else {
         putXMMReg( gregOfRM(modrm), mk128from32s( s1, d1, s0, d0 ) );
      }

      goto decode_success;
   }

   /* 0F 57 = XORPS -- G = G and E */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x57) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "xorps", Iop_XorV128 );
      goto decode_success;
   }

   /* ---------------------------------------------------- */
   /* --- end of the SSE decoder.                      --- */
   /* ---------------------------------------------------- */

   /* ---------------------------------------------------- */
   /* --- start of the SSE2 decoder.                   --- */
   /* ---------------------------------------------------- */

   /* Skip parts of the decoder which don't apply given the stated
      guest subarchitecture. */
   if (0 == (archinfo->hwcaps & VEX_HWCAPS_X86_SSE2))
      goto after_sse_decoders; /* no SSE2 capabilities */

   insn = (UChar*)&guest_code[delta];

   /* 66 0F 58 = ADDPD -- add 32Fx4 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x58) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "addpd", Iop_Add64Fx2 );
      goto decode_success;
   }
 
   /* F2 0F 58 = ADDSD -- add 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x58) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo64( sorb, delta+3, "addsd", Iop_Add64F0x2 );
      goto decode_success;
   }

   /* 66 0F 55 = ANDNPD -- G = (not G) and E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x55) {
      delta = dis_SSE_E_to_G_all_invG( sorb, delta+2, "andnpd", Iop_AndV128 );
      goto decode_success;
   }

   /* 66 0F 54 = ANDPD -- G = G and E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x54) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "andpd", Iop_AndV128 );
      goto decode_success;
   }

   /* 66 0F C2 = CMPPD -- 64Fx2 comparison from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xC2) {
      delta = dis_SSEcmp_E_to_G( sorb, delta+2, "cmppd", True, 8 );
      goto decode_success;
   }

   /* F2 0F C2 = CMPSD -- 64F0x2 comparison from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0xC2) {
      vassert(sz == 4);
      delta = dis_SSEcmp_E_to_G( sorb, delta+3, "cmpsd", False, 8 );
      goto decode_success;
   }

   /* 66 0F 2F = COMISD  -- 64F0x2 comparison G,E, and set ZCP */
   /* 66 0F 2E = UCOMISD -- 64F0x2 comparison G,E, and set ZCP */
   if (sz == 2 && insn[0] == 0x0F && (insn[1] == 0x2F || insn[1] == 0x2E)) {
      IRTemp argL = newTemp(Ity_F64);
      IRTemp argR = newTemp(Ity_F64);
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( argR, getXMMRegLane64F( eregOfRM(modrm), 0/*lowest lane*/ ) );
         delta += 2+1;
         DIP("[u]comisd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)) );
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( argR, loadLE(Ity_F64, mkexpr(addr)) );
         delta += 2+alen;
         DIP("[u]comisd %s,%s\n", dis_buf,
                                  nameXMMReg(gregOfRM(modrm)) );
      }
      assign( argL, getXMMRegLane64F( gregOfRM(modrm), 0/*lowest lane*/ ) );

      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
      stmt( IRStmt_Put( 
               OFFB_CC_DEP1,
               binop( Iop_And32,
                      binop(Iop_CmpF64, mkexpr(argL), mkexpr(argR)),
                      mkU32(0x45)
          )));
      /* Set NDEP even though it isn't used.  This makes redundant-PUT
         elimination of previous stores to this field work better. */
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
      goto decode_success;
   }

   /* F3 0F E6 = CVTDQ2PD -- convert 2 x I32 in mem/lo half xmm to 2 x
      F64 in xmm(G) */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0xE6) {
      IRTemp arg64 = newTemp(Ity_I64);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         assign( arg64, getXMMRegLane64(eregOfRM(modrm), 0) );
         delta += 3+1;
         DIP("cvtdq2pd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign( arg64, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("cvtdq2pd %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }

      putXMMRegLane64F( 
         gregOfRM(modrm), 0,
         unop(Iop_I32StoF64, unop(Iop_64to32, mkexpr(arg64)))
      );

      putXMMRegLane64F(
         gregOfRM(modrm), 1, 
         unop(Iop_I32StoF64, unop(Iop_64HIto32, mkexpr(arg64)))
      );

      goto decode_success;
   }

   /* 0F 5B = CVTDQ2PS -- convert 4 x I32 in mem/xmm to 4 x F32 in
      xmm(G) */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x5B) {
      IRTemp argV  = newTemp(Ity_V128);
      IRTemp rmode = newTemp(Ity_I32);

      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( argV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("cvtdq2ps %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( argV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("cvtdq2ps %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }
         
      assign( rmode, get_sse_roundingmode() );
      breakup128to32s( argV, &t3, &t2, &t1, &t0 );

#     define CVT(_t)  binop( Iop_F64toF32,                    \
                             mkexpr(rmode),                   \
                             unop(Iop_I32StoF64,mkexpr(_t)))
      
      putXMMRegLane32F( gregOfRM(modrm), 3, CVT(t3) );
      putXMMRegLane32F( gregOfRM(modrm), 2, CVT(t2) );
      putXMMRegLane32F( gregOfRM(modrm), 1, CVT(t1) );
      putXMMRegLane32F( gregOfRM(modrm), 0, CVT(t0) );

#     undef CVT

      goto decode_success;
   }

   /* F2 0F E6 = CVTPD2DQ -- convert 2 x F64 in mem/xmm to 2 x I32 in
      lo half xmm(G), and zero upper half */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0xE6) {
      IRTemp argV  = newTemp(Ity_V128);
      IRTemp rmode = newTemp(Ity_I32);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         assign( argV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("cvtpd2dq %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign( argV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("cvtpd2dq %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }
         
      assign( rmode, get_sse_roundingmode() );
      t0 = newTemp(Ity_F64);
      t1 = newTemp(Ity_F64);
      assign( t0, unop(Iop_ReinterpI64asF64, 
                       unop(Iop_V128to64, mkexpr(argV))) );
      assign( t1, unop(Iop_ReinterpI64asF64, 
                       unop(Iop_V128HIto64, mkexpr(argV))) );
      
#     define CVT(_t)  binop( Iop_F64toI32S,                   \
                             mkexpr(rmode),                   \
                             mkexpr(_t) )
      
      putXMMRegLane32( gregOfRM(modrm), 3, mkU32(0) );
      putXMMRegLane32( gregOfRM(modrm), 2, mkU32(0) );
      putXMMRegLane32( gregOfRM(modrm), 1, CVT(t1) );
      putXMMRegLane32( gregOfRM(modrm), 0, CVT(t0) );

#     undef CVT

      goto decode_success;
   }

   /* 66 0F 2D = CVTPD2PI -- convert 2 x F64 in mem/xmm to 2 x
      I32 in mmx, according to prevailing SSE rounding mode */
   /* 66 0F 2C = CVTTPD2PI -- convert 2 x F64 in mem/xmm to 2 x
      I32 in mmx, rounding towards zero */
   if (sz == 2 && insn[0] == 0x0F && (insn[1] == 0x2D || insn[1] == 0x2C)) {
      IRTemp dst64  = newTemp(Ity_I64);
      IRTemp rmode  = newTemp(Ity_I32);
      IRTemp f64lo  = newTemp(Ity_F64);
      IRTemp f64hi  = newTemp(Ity_F64);
      Bool   r2zero = toBool(insn[1] == 0x2C);

      do_MMX_preamble();
      modrm = getIByte(delta+2);

      if (epartIsReg(modrm)) {
         delta += 2+1;
	 assign(f64lo, getXMMRegLane64F(eregOfRM(modrm), 0));
	 assign(f64hi, getXMMRegLane64F(eregOfRM(modrm), 1));
         DIP("cvt%spd2pi %s,%s\n", r2zero ? "t" : "",
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign(f64lo, loadLE(Ity_F64, mkexpr(addr)));
	 assign(f64hi, loadLE(Ity_F64, binop( Iop_Add32, 
                                              mkexpr(addr), 
                                              mkU32(8) )));
         delta += 2+alen;
         DIP("cvt%spf2pi %s,%s\n", r2zero ? "t" : "",
                                   dis_buf,
                                   nameMMXReg(gregOfRM(modrm)));
      }

      if (r2zero) {
         assign(rmode, mkU32((UInt)Irrm_ZERO) );
      } else {
         assign( rmode, get_sse_roundingmode() );
      }

      assign( 
         dst64,
         binop( Iop_32HLto64,
                binop( Iop_F64toI32S, mkexpr(rmode), mkexpr(f64hi) ),
                binop( Iop_F64toI32S, mkexpr(rmode), mkexpr(f64lo) )
              )
      );

      putMMXReg(gregOfRM(modrm), mkexpr(dst64));
      goto decode_success;
   }

   /* 66 0F 5A = CVTPD2PS -- convert 2 x F64 in mem/xmm to 2 x F32 in
      lo half xmm(G), and zero upper half */
   /* Note, this is practically identical to CVTPD2DQ.  It would have
      been nicer to merge them together, but the insn[] offsets differ
      by one. */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x5A) {
      IRTemp argV  = newTemp(Ity_V128);
      IRTemp rmode = newTemp(Ity_I32);

      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( argV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("cvtpd2ps %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( argV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("cvtpd2ps %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }
         
      assign( rmode, get_sse_roundingmode() );
      t0 = newTemp(Ity_F64);
      t1 = newTemp(Ity_F64);
      assign( t0, unop(Iop_ReinterpI64asF64, 
                       unop(Iop_V128to64, mkexpr(argV))) );
      assign( t1, unop(Iop_ReinterpI64asF64, 
                       unop(Iop_V128HIto64, mkexpr(argV))) );
      
#     define CVT(_t)  binop( Iop_F64toF32,                    \
                             mkexpr(rmode),                   \
                             mkexpr(_t) )
      
      putXMMRegLane32(  gregOfRM(modrm), 3, mkU32(0) );
      putXMMRegLane32(  gregOfRM(modrm), 2, mkU32(0) );
      putXMMRegLane32F( gregOfRM(modrm), 1, CVT(t1) );
      putXMMRegLane32F( gregOfRM(modrm), 0, CVT(t0) );

#     undef CVT

      goto decode_success;
   }

   /* 66 0F 2A = CVTPI2PD -- convert 2 x I32 in mem/mmx to 2 x F64 in
      xmm(G) */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x2A) {
      IRTemp arg64 = newTemp(Ity_I64);

      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         /* Only switch to MMX mode if the source is a MMX register.
            This is inconsistent with all other instructions which
            convert between XMM and (M64 or MMX), which always switch
            to MMX mode even if 64-bit operand is M64 and not MMX.  At
            least, that's what the Intel docs seem to me to say.
            Fixes #210264. */
         do_MMX_preamble();
         assign( arg64, getMMXReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("cvtpi2pd %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( arg64, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 2+alen;
         DIP("cvtpi2pd %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }

      putXMMRegLane64F( 
         gregOfRM(modrm), 0,
         unop(Iop_I32StoF64, unop(Iop_64to32, mkexpr(arg64)) )
      );

      putXMMRegLane64F( 
         gregOfRM(modrm), 1,
         unop(Iop_I32StoF64, unop(Iop_64HIto32, mkexpr(arg64)) )
      );

      goto decode_success;
   }

   /* 66 0F 5B = CVTPS2DQ -- convert 4 x F32 in mem/xmm to 4 x I32 in
      xmm(G) */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x5B) {
      IRTemp argV  = newTemp(Ity_V128);
      IRTemp rmode = newTemp(Ity_I32);

      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( argV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("cvtps2dq %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( argV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("cvtps2dq %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }
         
      assign( rmode, get_sse_roundingmode() );
      breakup128to32s( argV, &t3, &t2, &t1, &t0 );

      /* This is less than ideal.  If it turns out to be a performance
	 bottleneck it can be improved. */
#     define CVT(_t)                            \
        binop( Iop_F64toI32S,                   \
               mkexpr(rmode),                   \
               unop( Iop_F32toF64,              \
                     unop( Iop_ReinterpI32asF32, mkexpr(_t))) )
      
      putXMMRegLane32( gregOfRM(modrm), 3, CVT(t3) );
      putXMMRegLane32( gregOfRM(modrm), 2, CVT(t2) );
      putXMMRegLane32( gregOfRM(modrm), 1, CVT(t1) );
      putXMMRegLane32( gregOfRM(modrm), 0, CVT(t0) );

#     undef CVT

      goto decode_success;
   }

   /* 0F 5A = CVTPS2PD -- convert 2 x F32 in low half mem/xmm to 2 x
      F64 in xmm(G). */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0x5A) {
      IRTemp f32lo = newTemp(Ity_F32);
      IRTemp f32hi = newTemp(Ity_F32);

      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( f32lo, getXMMRegLane32F(eregOfRM(modrm), 0) );
         assign( f32hi, getXMMRegLane32F(eregOfRM(modrm), 1) );
         delta += 2+1;
         DIP("cvtps2pd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( f32lo, loadLE(Ity_F32, mkexpr(addr)) );
	 assign( f32hi, loadLE(Ity_F32, 
                               binop(Iop_Add32,mkexpr(addr),mkU32(4))) );
         delta += 2+alen;
         DIP("cvtps2pd %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }

      putXMMRegLane64F( gregOfRM(modrm), 1,
                        unop(Iop_F32toF64, mkexpr(f32hi)) );
      putXMMRegLane64F( gregOfRM(modrm), 0,
                        unop(Iop_F32toF64, mkexpr(f32lo)) );

      goto decode_success;
   }

   /* F2 0F 2D = CVTSD2SI -- convert F64 in mem/low half xmm to
      I32 in ireg, according to prevailing SSE rounding mode */
   /* F2 0F 2C = CVTTSD2SI -- convert F64 in mem/low half xmm to
      I32 in ireg, rounding towards zero */
   if (insn[0] == 0xF2 && insn[1] == 0x0F 
       && (insn[2] == 0x2D || insn[2] == 0x2C)) {
      IRTemp rmode = newTemp(Ity_I32);
      IRTemp f64lo = newTemp(Ity_F64);
      Bool   r2zero = toBool(insn[2] == 0x2C);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         delta += 3+1;
	 assign(f64lo, getXMMRegLane64F(eregOfRM(modrm), 0));
         DIP("cvt%ssd2si %s,%s\n", r2zero ? "t" : "",
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameIReg(4, gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign(f64lo, loadLE(Ity_F64, mkexpr(addr)));
         delta += 3+alen;
         DIP("cvt%ssd2si %s,%s\n", r2zero ? "t" : "",
                                   dis_buf,
                                   nameIReg(4, gregOfRM(modrm)));
      }

      if (r2zero) {
         assign( rmode, mkU32((UInt)Irrm_ZERO) );
      } else {
         assign( rmode, get_sse_roundingmode() );
      }

      putIReg(4, gregOfRM(modrm),
                 binop( Iop_F64toI32S, mkexpr(rmode), mkexpr(f64lo)) );

      goto decode_success;
   }

   /* F2 0F 5A = CVTSD2SS -- convert F64 in mem/low half xmm to F32 in
      low 1/4 xmm(G), according to prevailing SSE rounding mode */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5A) {
      IRTemp rmode = newTemp(Ity_I32);
      IRTemp f64lo = newTemp(Ity_F64);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         delta += 3+1;
	 assign(f64lo, getXMMRegLane64F(eregOfRM(modrm), 0));
         DIP("cvtsd2ss %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign(f64lo, loadLE(Ity_F64, mkexpr(addr)));
         delta += 3+alen;
         DIP("cvtsd2ss %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)));
      }

      assign( rmode, get_sse_roundingmode() );
      putXMMRegLane32F( 
         gregOfRM(modrm), 0, 
         binop( Iop_F64toF32, mkexpr(rmode), mkexpr(f64lo) )
      );

      goto decode_success;
   }

   /* F2 0F 2A = CVTSI2SD -- convert I32 in mem/ireg to F64 in low
      half xmm */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x2A) {
      IRTemp arg32 = newTemp(Ity_I32);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         assign( arg32, getIReg(4, eregOfRM(modrm)) );
         delta += 3+1;
         DIP("cvtsi2sd %s,%s\n", nameIReg(4, eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign( arg32, loadLE(Ity_I32, mkexpr(addr)) );
         delta += 3+alen;
         DIP("cvtsi2sd %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)) );
      }

      putXMMRegLane64F( 
         gregOfRM(modrm), 0,
         unop(Iop_I32StoF64, mkexpr(arg32)) );

      goto decode_success;
   }

   /* F3 0F 5A = CVTSS2SD -- convert F32 in mem/low 1/4 xmm to F64 in
      low half xmm(G) */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5A) {
      IRTemp f32lo = newTemp(Ity_F32);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         delta += 3+1;
	 assign(f32lo, getXMMRegLane32F(eregOfRM(modrm), 0));
         DIP("cvtss2sd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign(f32lo, loadLE(Ity_F32, mkexpr(addr)));
         delta += 3+alen;
         DIP("cvtss2sd %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)));
      }

      putXMMRegLane64F( gregOfRM(modrm), 0, 
                        unop( Iop_F32toF64, mkexpr(f32lo) ) );

      goto decode_success;
   }

   /* 66 0F E6 = CVTTPD2DQ -- convert 2 x F64 in mem/xmm to 2 x I32 in
      lo half xmm(G), and zero upper half, rounding towards zero */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE6) {
      IRTemp argV  = newTemp(Ity_V128);
      IRTemp rmode = newTemp(Ity_I32);

      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         assign( argV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("cvttpd2dq %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
	 assign( argV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("cvttpd2dq %s,%s\n", dis_buf,
                                  nameXMMReg(gregOfRM(modrm)) );
      }

      assign( rmode, mkU32((UInt)Irrm_ZERO) );

      t0 = newTemp(Ity_F64);
      t1 = newTemp(Ity_F64);
      assign( t0, unop(Iop_ReinterpI64asF64, 
                       unop(Iop_V128to64, mkexpr(argV))) );
      assign( t1, unop(Iop_ReinterpI64asF64, 
                       unop(Iop_V128HIto64, mkexpr(argV))) );
      
#     define CVT(_t)  binop( Iop_F64toI32S,                   \
                             mkexpr(rmode),                   \
                             mkexpr(_t) )
      
      putXMMRegLane32( gregOfRM(modrm), 3, mkU32(0) );
      putXMMRegLane32( gregOfRM(modrm), 2, mkU32(0) );
      putXMMRegLane32( gregOfRM(modrm), 1, CVT(t1) );
      putXMMRegLane32( gregOfRM(modrm), 0, CVT(t0) );

#     undef CVT

      goto decode_success;
   }

   /* F3 0F 5B = CVTTPS2DQ -- convert 4 x F32 in mem/xmm to 4 x I32 in
      xmm(G), rounding towards zero */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5B) {
      IRTemp argV  = newTemp(Ity_V128);
      IRTemp rmode = newTemp(Ity_I32);
      vassert(sz == 4);

      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         assign( argV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("cvttps2dq %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
	 assign( argV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("cvttps2dq %s,%s\n", dis_buf,
                                  nameXMMReg(gregOfRM(modrm)) );
      }
         
      assign( rmode, mkU32((UInt)Irrm_ZERO) );
      breakup128to32s( argV, &t3, &t2, &t1, &t0 );

      /* This is less than ideal.  If it turns out to be a performance
	 bottleneck it can be improved. */
#     define CVT(_t)                            \
        binop( Iop_F64toI32S,                   \
               mkexpr(rmode),                   \
               unop( Iop_F32toF64,              \
                     unop( Iop_ReinterpI32asF32, mkexpr(_t))) )
      
      putXMMRegLane32( gregOfRM(modrm), 3, CVT(t3) );
      putXMMRegLane32( gregOfRM(modrm), 2, CVT(t2) );
      putXMMRegLane32( gregOfRM(modrm), 1, CVT(t1) );
      putXMMRegLane32( gregOfRM(modrm), 0, CVT(t0) );

#     undef CVT

      goto decode_success;
   }

   /* 66 0F 5E = DIVPD -- div 64Fx2 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x5E) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "divpd", Iop_Div64Fx2 );
      goto decode_success;
   }

   /* F2 0F 5E = DIVSD -- div 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5E) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo64( sorb, delta+3, "divsd", Iop_Div64F0x2 );
      goto decode_success;
   }

   /* 0F AE /5 = LFENCE -- flush pending operations to memory */
   /* 0F AE /6 = MFENCE -- flush pending operations to memory */
   if (insn[0] == 0x0F && insn[1] == 0xAE
       && epartIsReg(insn[2]) 
       && (gregOfRM(insn[2]) == 5 || gregOfRM(insn[2]) == 6)) {
      vassert(sz == 4);
      delta += 3;
      /* Insert a memory fence.  It's sometimes important that these
         are carried through to the generated code. */
      stmt( IRStmt_MBE(Imbe_Fence) );
      DIP("%sfence\n", gregOfRM(insn[2])==5 ? "l" : "m");
      goto decode_success;
   }

   /* 66 0F 5F = MAXPD -- max 64Fx2 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x5F) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "maxpd", Iop_Max64Fx2 );
      goto decode_success;
   }

   /* F2 0F 5F = MAXSD -- max 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5F) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo64( sorb, delta+3, "maxsd", Iop_Max64F0x2 );
      goto decode_success;
   }

   /* 66 0F 5D = MINPD -- min 64Fx2 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x5D) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "minpd", Iop_Min64Fx2 );
      goto decode_success;
   }

   /* F2 0F 5D = MINSD -- min 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5D) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo64( sorb, delta+3, "minsd", Iop_Min64F0x2 );
      goto decode_success;
   }

   /* 66 0F 28 = MOVAPD -- move from E (mem or xmm) to G (xmm). */
   /* 66 0F 10 = MOVUPD -- move from E (mem or xmm) to G (xmm). */
   /* 66 0F 6F = MOVDQA -- move from E (mem or xmm) to G (xmm). */
   if (sz == 2 && insn[0] == 0x0F 
       && (insn[1] == 0x28 || insn[1] == 0x10 || insn[1] == 0x6F)) {
      const HChar* wot = insn[1]==0x28 ? "apd" :
                         insn[1]==0x10 ? "upd" : "dqa";
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         putXMMReg( gregOfRM(modrm), 
                    getXMMReg( eregOfRM(modrm) ));
         DIP("mov%s %s,%s\n", wot, nameXMMReg(eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
         delta += 2+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         if (insn[1] == 0x28/*movapd*/ || insn[1] == 0x6F/*movdqa*/)
            gen_SEGV_if_not_16_aligned( addr );
         putXMMReg( gregOfRM(modrm), 
                    loadLE(Ity_V128, mkexpr(addr)) );
         DIP("mov%s %s,%s\n", wot, dis_buf,
                                   nameXMMReg(gregOfRM(modrm)));
         delta += 2+alen;
      }
      goto decode_success;
   }

   /* 66 0F 29 = MOVAPD -- move from G (xmm) to E (mem or xmm). */
   /* 66 0F 11 = MOVUPD -- move from G (xmm) to E (mem or xmm). */
   if (sz == 2 && insn[0] == 0x0F 
       && (insn[1] == 0x29 || insn[1] == 0x11)) {
      const HChar* wot = insn[1]==0x29 ? "apd" : "upd";
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         /* fall through; awaiting test case */
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         if (insn[1] == 0x29/*movapd*/)
            gen_SEGV_if_not_16_aligned( addr );
         storeLE( mkexpr(addr), getXMMReg(gregOfRM(modrm)) );
         DIP("mov%s %s,%s\n", wot, nameXMMReg(gregOfRM(modrm)),
                                   dis_buf );
         delta += 2+alen;
         goto decode_success;
      }
   }

   /* 66 0F 6E = MOVD from r/m32 to xmm, zeroing high 3/4 of xmm. */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x6E) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         delta += 2+1;
         putXMMReg(
            gregOfRM(modrm),
            unop( Iop_32UtoV128, getIReg(4, eregOfRM(modrm)) ) 
         );
         DIP("movd %s, %s\n", 
             nameIReg(4,eregOfRM(modrm)), nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         putXMMReg(
            gregOfRM(modrm),
            unop( Iop_32UtoV128,loadLE(Ity_I32, mkexpr(addr)) ) 
         );
         DIP("movd %s, %s\n", dis_buf, nameXMMReg(gregOfRM(modrm)));
      }
      goto decode_success;
   }

   /* 66 0F 7E = MOVD from xmm low 1/4 to r/m32. */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x7E) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         delta += 2+1;
         putIReg( 4, eregOfRM(modrm),
                  getXMMRegLane32(gregOfRM(modrm), 0) );
         DIP("movd %s, %s\n", 
             nameXMMReg(gregOfRM(modrm)), nameIReg(4,eregOfRM(modrm)));
      } else {
         addr = disAMode( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         storeLE( mkexpr(addr),
                  getXMMRegLane32(gregOfRM(modrm), 0) );
         DIP("movd %s, %s\n", nameXMMReg(gregOfRM(modrm)), dis_buf);
      }
      goto decode_success;
   }

   /* 66 0F 7F = MOVDQA -- move from G (xmm) to E (mem or xmm). */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x7F) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         delta += 2+1;
         putXMMReg( eregOfRM(modrm),
                    getXMMReg(gregOfRM(modrm)) );
         DIP("movdqa %s, %s\n", nameXMMReg(gregOfRM(modrm)), 
                                nameXMMReg(eregOfRM(modrm)));
      } else {
         addr = disAMode( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         gen_SEGV_if_not_16_aligned( addr );
         storeLE( mkexpr(addr), getXMMReg(gregOfRM(modrm)) );
         DIP("movdqa %s, %s\n", nameXMMReg(gregOfRM(modrm)), dis_buf);
      }
      goto decode_success;
   }

   /* F3 0F 6F = MOVDQU -- move from E (mem or xmm) to G (xmm). */
   /* Unfortunately can't simply use the MOVDQA case since the
      prefix lengths are different (66 vs F3) */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x6F) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         putXMMReg( gregOfRM(modrm), 
                    getXMMReg( eregOfRM(modrm) ));
         DIP("movdqu %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                               nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         putXMMReg( gregOfRM(modrm), 
                    loadLE(Ity_V128, mkexpr(addr)) );
         DIP("movdqu %s,%s\n", dis_buf,
                               nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }
      goto decode_success;
   }

   /* F3 0F 7F = MOVDQU -- move from G (xmm) to E (mem or xmm). */
   /* Unfortunately can't simply use the MOVDQA case since the
      prefix lengths are different (66 vs F3) */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x7F) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         delta += 3+1;
         putXMMReg( eregOfRM(modrm),
                    getXMMReg(gregOfRM(modrm)) );
         DIP("movdqu %s, %s\n", nameXMMReg(gregOfRM(modrm)), 
                                nameXMMReg(eregOfRM(modrm)));
      } else {
         addr = disAMode( &alen, sorb, delta+3, dis_buf );
         delta += 3+alen;
         storeLE( mkexpr(addr), getXMMReg(gregOfRM(modrm)) );
         DIP("movdqu %s, %s\n", nameXMMReg(gregOfRM(modrm)), dis_buf);
      }
      goto decode_success;
   }

   /* F2 0F D6 = MOVDQ2Q -- move from E (lo half xmm, not mem) to G (mmx). */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0xD6) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         do_MMX_preamble();
         putMMXReg( gregOfRM(modrm), 
                    getXMMRegLane64( eregOfRM(modrm), 0 ));
         DIP("movdq2q %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                nameMMXReg(gregOfRM(modrm)));
         delta += 3+1;
         goto decode_success;
      } else {
         /* fall through, apparently no mem case for this insn */
      }
   }

   /* 66 0F 16 = MOVHPD -- move from mem to high half of XMM. */
   /* These seems identical to MOVHPS.  This instruction encoding is
      completely crazy. */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x16) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         /* fall through; apparently reg-reg is not possible */
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         putXMMRegLane64( gregOfRM(modrm), 1/*upper lane*/,
                          loadLE(Ity_I64, mkexpr(addr)) );
         DIP("movhpd %s,%s\n", dis_buf, 
                               nameXMMReg( gregOfRM(modrm) ));
         goto decode_success;
      }
   }

   /* 66 0F 17 = MOVHPD -- move from high half of XMM to mem. */
   /* Again, this seems identical to MOVHPS. */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x17) {
      if (!epartIsReg(insn[2])) {
         delta += 2;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;
         storeLE( mkexpr(addr), 
                  getXMMRegLane64( gregOfRM(insn[2]),
                                   1/*upper lane*/ ) );
         DIP("movhpd %s,%s\n", nameXMMReg( gregOfRM(insn[2]) ),
                               dis_buf);
         goto decode_success;
      }
      /* else fall through */
   }

   /* 66 0F 12 = MOVLPD -- move from mem to low half of XMM. */
   /* Identical to MOVLPS ? */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x12) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         /* fall through; apparently reg-reg is not possible */
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         delta += 2+alen;
         putXMMRegLane64( gregOfRM(modrm),  0/*lower lane*/,
                          loadLE(Ity_I64, mkexpr(addr)) );
         DIP("movlpd %s, %s\n", 
             dis_buf, nameXMMReg( gregOfRM(modrm) ));
         goto decode_success;
      }
   }

   /* 66 0F 13 = MOVLPD -- move from low half of XMM to mem. */
   /* Identical to MOVLPS ? */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x13) {
      if (!epartIsReg(insn[2])) {
         delta += 2;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;
         storeLE( mkexpr(addr), 
                  getXMMRegLane64( gregOfRM(insn[2]), 
                                   0/*lower lane*/ ) );
         DIP("movlpd %s, %s\n", nameXMMReg( gregOfRM(insn[2]) ),
                                dis_buf);
         goto decode_success;
      }
      /* else fall through */
   }

   /* 66 0F 50 = MOVMSKPD - move 2 sign bits from 2 x F64 in xmm(E) to
      2 lowest bits of ireg(G) */
   if (insn[0] == 0x0F && insn[1] == 0x50) {
      modrm = getIByte(delta+2);
      if (sz == 2 && epartIsReg(modrm)) {
         Int src;
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         delta += 2+1;
         src = eregOfRM(modrm);
         assign( t0, binop( Iop_And32,
                            binop(Iop_Shr32, getXMMRegLane32(src,1), mkU8(31)),
                            mkU32(1) ));
         assign( t1, binop( Iop_And32,
                            binop(Iop_Shr32, getXMMRegLane32(src,3), mkU8(30)),
                            mkU32(2) ));
         putIReg(4, gregOfRM(modrm),
                    binop(Iop_Or32, mkexpr(t0), mkexpr(t1))
                 );
         DIP("movmskpd %s,%s\n", nameXMMReg(src), 
                                 nameIReg(4, gregOfRM(modrm)));
         goto decode_success;
      }
      /* else fall through */
   }

   /* 66 0F F7 = MASKMOVDQU -- store selected bytes of double quadword */
   if (insn[0] == 0x0F && insn[1] == 0xF7) {
      modrm = getIByte(delta+2);
      if (sz == 2 && epartIsReg(modrm)) {
         IRTemp regD    = newTemp(Ity_V128);
         IRTemp mask    = newTemp(Ity_V128);
         IRTemp olddata = newTemp(Ity_V128);
         IRTemp newdata = newTemp(Ity_V128);
                addr    = newTemp(Ity_I32);

         assign( addr, handleSegOverride( sorb, getIReg(4, R_EDI) ));
         assign( regD, getXMMReg( gregOfRM(modrm) ));

         /* Unfortunately can't do the obvious thing with SarN8x16
            here since that can't be re-emitted as SSE2 code - no such
            insn. */
	 assign( 
            mask, 
            binop(Iop_64HLtoV128,
                  binop(Iop_SarN8x8, 
                        getXMMRegLane64( eregOfRM(modrm), 1 ), 
                        mkU8(7) ),
                  binop(Iop_SarN8x8, 
                        getXMMRegLane64( eregOfRM(modrm), 0 ), 
                        mkU8(7) ) ));
         assign( olddata, loadLE( Ity_V128, mkexpr(addr) ));
         assign( newdata, 
                 binop(Iop_OrV128, 
                       binop(Iop_AndV128, 
                             mkexpr(regD), 
                             mkexpr(mask) ),
                       binop(Iop_AndV128, 
                             mkexpr(olddata),
                             unop(Iop_NotV128, mkexpr(mask)))) );
         storeLE( mkexpr(addr), mkexpr(newdata) );

         delta += 2+1;
         DIP("maskmovdqu %s,%s\n", nameXMMReg( eregOfRM(modrm) ),
                                   nameXMMReg( gregOfRM(modrm) ) );
         goto decode_success;
      }
      /* else fall through */
   }

   /* 66 0F E7 = MOVNTDQ -- for us, just a plain SSE store. */
   if (insn[0] == 0x0F && insn[1] == 0xE7) {
      modrm = getIByte(delta+2);
      if (sz == 2 && !epartIsReg(modrm)) {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         storeLE( mkexpr(addr), getXMMReg(gregOfRM(modrm)) );
         DIP("movntdq %s,%s\n", dis_buf,
                                nameXMMReg(gregOfRM(modrm)));
         delta += 2+alen;
         goto decode_success;
      }
      /* else fall through */
   }

   /* 0F C3 = MOVNTI -- for us, just a plain ireg store. */
   if (insn[0] == 0x0F && insn[1] == 0xC3) {
      vassert(sz == 4);
      modrm = getIByte(delta+2);
      if (!epartIsReg(modrm)) {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         storeLE( mkexpr(addr), getIReg(4, gregOfRM(modrm)) );
         DIP("movnti %s,%s\n", dis_buf,
                               nameIReg(4, gregOfRM(modrm)));
         delta += 2+alen;
         goto decode_success;
      }
      /* else fall through */
   }

   /* 66 0F D6 = MOVQ -- move 64 bits from G (lo half xmm) to E (mem
      or lo half xmm).  */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD6) {
      modrm = getIByte(delta+2);
      if (epartIsReg(modrm)) {
         /* fall through, awaiting test case */
         /* dst: lo half copied, hi half zeroed */
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         storeLE( mkexpr(addr), 
                  getXMMRegLane64( gregOfRM(modrm), 0 ));
         DIP("movq %s,%s\n", nameXMMReg(gregOfRM(modrm)), dis_buf );
         delta += 2+alen;
         goto decode_success;
      }
   }

   /* F3 0F D6 = MOVQ2DQ -- move from E (mmx) to G (lo half xmm, zero
      hi half). */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0xD6) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         do_MMX_preamble();
         putXMMReg( gregOfRM(modrm), 
                    unop(Iop_64UtoV128, getMMXReg( eregOfRM(modrm) )) );
         DIP("movq2dq %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
         goto decode_success;
      } else {
         /* fall through, apparently no mem case for this insn */
      }
   }

   /* F3 0F 7E = MOVQ -- move 64 bits from E (mem or lo half xmm) to
      G (lo half xmm).  Upper half of G is zeroed out. */
   /* F2 0F 10 = MOVSD -- move 64 bits from E (mem or lo half xmm) to
      G (lo half xmm).  If E is mem, upper half of G is zeroed out.
      If E is reg, upper half of G is unchanged. */
   if ((insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x10)
       || (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x7E)) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         putXMMRegLane64( gregOfRM(modrm), 0,
                          getXMMRegLane64( eregOfRM(modrm), 0 ));
         if (insn[0] == 0xF3/*MOVQ*/) {
            /* zero bits 127:64 */
            putXMMRegLane64( gregOfRM(modrm), 1, mkU64(0) );
         }
         DIP("movsd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                              nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         /* zero bits 127:64 */
         putXMMRegLane64( gregOfRM(modrm), 1, mkU64(0) );
         /* write bits 63:0 */
         putXMMRegLane64( gregOfRM(modrm), 0,
                          loadLE(Ity_I64, mkexpr(addr)) );
         DIP("movsd %s,%s\n", dis_buf,
                              nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }
      goto decode_success;
   }

   /* F2 0F 11 = MOVSD -- move 64 bits from G (lo half xmm) to E (mem
      or lo half xmm). */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x11) {
      vassert(sz == 4);
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         putXMMRegLane64( eregOfRM(modrm), 0,
                          getXMMRegLane64( gregOfRM(modrm), 0 ));
         DIP("movsd %s,%s\n", nameXMMReg(gregOfRM(modrm)),
                              nameXMMReg(eregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         storeLE( mkexpr(addr),
                  getXMMRegLane64(gregOfRM(modrm), 0) );
         DIP("movsd %s,%s\n", nameXMMReg(gregOfRM(modrm)),
                              dis_buf);
         delta += 3+alen;
      }
      goto decode_success;
   }

   /* 66 0F 59 = MULPD -- mul 64Fx2 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x59) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "mulpd", Iop_Mul64Fx2 );
      goto decode_success;
   }

   /* F2 0F 59 = MULSD -- mul 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x59) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo64( sorb, delta+3, "mulsd", Iop_Mul64F0x2 );
      goto decode_success;
   }

   /* 66 0F 56 = ORPD -- G = G and E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x56) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "orpd", Iop_OrV128 );
      goto decode_success;
   }

   /* 66 0F C6 /r ib = SHUFPD -- shuffle packed F64s */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xC6) {
      Int    select;
      IRTemp sV = newTemp(Ity_V128);
      IRTemp dV = newTemp(Ity_V128);
      IRTemp s1 = newTemp(Ity_I64);
      IRTemp s0 = newTemp(Ity_I64);
      IRTemp d1 = newTemp(Ity_I64);
      IRTemp d0 = newTemp(Ity_I64);

      modrm = insn[2];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         select = (Int)insn[3];
         delta += 2+2;
         DIP("shufpd $%d,%s,%s\n", select, 
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         select = (Int)insn[2+alen];
         delta += 3+alen;
         DIP("shufpd $%d,%s,%s\n", select, 
                                   dis_buf,
                                   nameXMMReg(gregOfRM(modrm)));
      }

      assign( d1, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( d0, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( s1, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( s0, unop(Iop_V128to64,   mkexpr(sV)) );

#     define SELD(n) mkexpr((n)==0 ? d0 : d1)
#     define SELS(n) mkexpr((n)==0 ? s0 : s1)

      putXMMReg(
         gregOfRM(modrm), 
         binop(Iop_64HLtoV128, SELS((select>>1)&1), SELD((select>>0)&1) )
      );

#     undef SELD
#     undef SELS

      goto decode_success;
   }

   /* 66 0F 51 = SQRTPD -- approx sqrt 64Fx2 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x51) {
      delta = dis_SSE_E_to_G_unary_all( sorb, delta+2, 
                                        "sqrtpd", Iop_Sqrt64Fx2 );
      goto decode_success;
   }

   /* F2 0F 51 = SQRTSD -- approx sqrt 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x51) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_unary_lo64( sorb, delta+3, 
                                         "sqrtsd", Iop_Sqrt64F0x2 );
      goto decode_success;
   }

   /* 66 0F 5C = SUBPD -- sub 64Fx2 from R/M to R */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x5C) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "subpd", Iop_Sub64Fx2 );
      goto decode_success;
   }

   /* F2 0F 5C = SUBSD -- sub 64F0x2 from R/M to R */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5C) {
      vassert(sz == 4);
      delta = dis_SSE_E_to_G_lo64( sorb, delta+3, "subsd", Iop_Sub64F0x2 );
      goto decode_success;
   }

   /* 66 0F 15 = UNPCKHPD -- unpack and interleave high part F64s */
   /* 66 0F 14 = UNPCKLPD -- unpack and interleave low part F64s */
   /* These just appear to be special cases of SHUFPS */
   if (sz == 2 && insn[0] == 0x0F && (insn[1] == 0x15 || insn[1] == 0x14)) {
      IRTemp s1 = newTemp(Ity_I64);
      IRTemp s0 = newTemp(Ity_I64);
      IRTemp d1 = newTemp(Ity_I64);
      IRTemp d0 = newTemp(Ity_I64);
      IRTemp sV = newTemp(Ity_V128);
      IRTemp dV = newTemp(Ity_V128);
      Bool   hi = toBool(insn[1] == 0x15);

      modrm = insn[2];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("unpck%sps %s,%s\n", hi ? "h" : "l",
                                  nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("unpck%sps %s,%s\n", hi ? "h" : "l",
                                  dis_buf,
                                  nameXMMReg(gregOfRM(modrm)));
      }

      assign( d1, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( d0, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( s1, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( s0, unop(Iop_V128to64,   mkexpr(sV)) );

      if (hi) {
         putXMMReg( gregOfRM(modrm), 
                    binop(Iop_64HLtoV128, mkexpr(s1), mkexpr(d1)) );
      } else {
         putXMMReg( gregOfRM(modrm), 
                    binop(Iop_64HLtoV128, mkexpr(s0), mkexpr(d0)) );
      }

      goto decode_success;
   }

   /* 66 0F 57 = XORPD -- G = G and E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x57) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "xorpd", Iop_XorV128 );
      goto decode_success;
   }

   /* 66 0F 6B = PACKSSDW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x6B) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "packssdw",
                                 Iop_QNarrowBin32Sto16Sx8, True );
      goto decode_success;
   }

   /* 66 0F 63 = PACKSSWB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x63) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "packsswb",
                                 Iop_QNarrowBin16Sto8Sx16, True );
      goto decode_success;
   }

   /* 66 0F 67 = PACKUSWB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x67) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "packuswb",
                                 Iop_QNarrowBin16Sto8Ux16, True );
      goto decode_success;
   }

   /* 66 0F FC = PADDB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xFC) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddb", Iop_Add8x16, False );
      goto decode_success;
   }

   /* 66 0F FE = PADDD */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xFE) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddd", Iop_Add32x4, False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE2 ---*** */
   /* 0F D4 = PADDQ -- add 64x1 */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xD4) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "paddq", False );
      goto decode_success;
   }

   /* 66 0F D4 = PADDQ */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD4) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddq", Iop_Add64x2, False );
      goto decode_success;
   }

   /* 66 0F FD = PADDW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xFD) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddw", Iop_Add16x8, False );
      goto decode_success;
   }

   /* 66 0F EC = PADDSB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xEC) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddsb", Iop_QAdd8Sx16, False );
      goto decode_success;
   }

   /* 66 0F ED = PADDSW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xED) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddsw", Iop_QAdd16Sx8, False );
      goto decode_success;
   }

   /* 66 0F DC = PADDUSB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xDC) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddusb", Iop_QAdd8Ux16, False );
      goto decode_success;
   }

   /* 66 0F DD = PADDUSW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xDD) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "paddusw", Iop_QAdd16Ux8, False );
      goto decode_success;
   }

   /* 66 0F DB = PAND */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xDB) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "pand", Iop_AndV128 );
      goto decode_success;
   }

   /* 66 0F DF = PANDN */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xDF) {
      delta = dis_SSE_E_to_G_all_invG( sorb, delta+2, "pandn", Iop_AndV128 );
      goto decode_success;
   }

   /* 66 0F E0 = PAVGB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE0) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pavgb", Iop_Avg8Ux16, False );
      goto decode_success;
   }

   /* 66 0F E3 = PAVGW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE3) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pavgw", Iop_Avg16Ux8, False );
      goto decode_success;
   }

   /* 66 0F 74 = PCMPEQB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x74) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pcmpeqb", Iop_CmpEQ8x16, False );
      goto decode_success;
   }

   /* 66 0F 76 = PCMPEQD */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x76) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pcmpeqd", Iop_CmpEQ32x4, False );
      goto decode_success;
   }

   /* 66 0F 75 = PCMPEQW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x75) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pcmpeqw", Iop_CmpEQ16x8, False );
      goto decode_success;
   }

   /* 66 0F 64 = PCMPGTB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x64) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pcmpgtb", Iop_CmpGT8Sx16, False );
      goto decode_success;
   }

   /* 66 0F 66 = PCMPGTD */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x66) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pcmpgtd", Iop_CmpGT32Sx4, False );
      goto decode_success;
   }

   /* 66 0F 65 = PCMPGTW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x65) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pcmpgtw", Iop_CmpGT16Sx8, False );
      goto decode_success;
   }

   /* 66 0F C5 = PEXTRW -- extract 16-bit field from xmm(E) and put 
      zero-extend of it in ireg(G). */
   if (insn[0] == 0x0F && insn[1] == 0xC5) {
      modrm = insn[2];
      if (sz == 2 && epartIsReg(modrm)) {
         t5 = newTemp(Ity_V128);
         t4 = newTemp(Ity_I16);
         assign(t5, getXMMReg(eregOfRM(modrm)));
         breakup128to32s( t5, &t3, &t2, &t1, &t0 );
         switch (insn[3] & 7) {
            case 0:  assign(t4, unop(Iop_32to16,   mkexpr(t0))); break;
            case 1:  assign(t4, unop(Iop_32HIto16, mkexpr(t0))); break;
            case 2:  assign(t4, unop(Iop_32to16,   mkexpr(t1))); break;
            case 3:  assign(t4, unop(Iop_32HIto16, mkexpr(t1))); break;
            case 4:  assign(t4, unop(Iop_32to16,   mkexpr(t2))); break;
            case 5:  assign(t4, unop(Iop_32HIto16, mkexpr(t2))); break;
            case 6:  assign(t4, unop(Iop_32to16,   mkexpr(t3))); break;
            case 7:  assign(t4, unop(Iop_32HIto16, mkexpr(t3))); break;
            default: vassert(0); /*NOTREACHED*/
         }
         putIReg(4, gregOfRM(modrm), unop(Iop_16Uto32, mkexpr(t4)));
         DIP("pextrw $%d,%s,%s\n",
             (Int)insn[3], nameXMMReg(eregOfRM(modrm)),
                           nameIReg(4,gregOfRM(modrm)));
         delta += 4;
         goto decode_success;
      } 
      /* else fall through */
   }

   /* 66 0F C4 = PINSRW -- get 16 bits from E(mem or low half ireg) and
      put it into the specified lane of xmm(G). */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xC4) {
      Int lane;
      t4 = newTemp(Ity_I16);
      modrm = insn[2];

      if (epartIsReg(modrm)) {
         assign(t4, getIReg(2, eregOfRM(modrm)));
         delta += 3+1;
         lane = insn[3+1-1];
         DIP("pinsrw $%d,%s,%s\n", (Int)lane, 
                                   nameIReg(2,eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         delta += 3+alen;
         lane = insn[3+alen-1];
         assign(t4, loadLE(Ity_I16, mkexpr(addr)));
         DIP("pinsrw $%d,%s,%s\n", (Int)lane, 
                                   dis_buf,
                                   nameXMMReg(gregOfRM(modrm)));
      }

      putXMMRegLane16( gregOfRM(modrm), lane & 7, mkexpr(t4) );
      goto decode_success;
   }

   /* 66 0F F5 = PMADDWD -- Multiply and add packed integers from
      E(xmm or mem) to G(xmm) */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF5) {
      IRTemp s1V  = newTemp(Ity_V128);
      IRTemp s2V  = newTemp(Ity_V128);
      IRTemp dV   = newTemp(Ity_V128);
      IRTemp s1Hi = newTemp(Ity_I64);
      IRTemp s1Lo = newTemp(Ity_I64);
      IRTemp s2Hi = newTemp(Ity_I64);
      IRTemp s2Lo = newTemp(Ity_I64);
      IRTemp dHi  = newTemp(Ity_I64);
      IRTemp dLo  = newTemp(Ity_I64);
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         assign( s1V, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("pmaddwd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( s1V, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("pmaddwd %s,%s\n", dis_buf,
                                nameXMMReg(gregOfRM(modrm)));
      }
      assign( s2V, getXMMReg(gregOfRM(modrm)) );
      assign( s1Hi, unop(Iop_V128HIto64, mkexpr(s1V)) );
      assign( s1Lo, unop(Iop_V128to64,   mkexpr(s1V)) );
      assign( s2Hi, unop(Iop_V128HIto64, mkexpr(s2V)) );
      assign( s2Lo, unop(Iop_V128to64,   mkexpr(s2V)) );
      assign( dHi, mkIRExprCCall(
                      Ity_I64, 0/*regparms*/,
                      "x86g_calculate_mmx_pmaddwd", 
                      &x86g_calculate_mmx_pmaddwd,
                      mkIRExprVec_2( mkexpr(s1Hi), mkexpr(s2Hi))
                   ));
      assign( dLo, mkIRExprCCall(
                      Ity_I64, 0/*regparms*/,
                      "x86g_calculate_mmx_pmaddwd", 
                      &x86g_calculate_mmx_pmaddwd,
                      mkIRExprVec_2( mkexpr(s1Lo), mkexpr(s2Lo))
                   ));
      assign( dV, binop(Iop_64HLtoV128, mkexpr(dHi), mkexpr(dLo))) ;
      putXMMReg(gregOfRM(modrm), mkexpr(dV));
      goto decode_success;
   }

   /* 66 0F EE = PMAXSW -- 16x8 signed max */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xEE) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pmaxsw", Iop_Max16Sx8, False );
      goto decode_success;
   }

   /* 66 0F DE = PMAXUB -- 8x16 unsigned max */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xDE) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pmaxub", Iop_Max8Ux16, False );
      goto decode_success;
   }

   /* 66 0F EA = PMINSW -- 16x8 signed min */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xEA) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pminsw", Iop_Min16Sx8, False );
      goto decode_success;
   }

   /* 66 0F DA = PMINUB -- 8x16 unsigned min */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xDA) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pminub", Iop_Min8Ux16, False );
      goto decode_success;
   }

   /* 66 0F D7 = PMOVMSKB -- extract sign bits from each of 16 lanes
      in xmm(E), turn them into a byte, and put zero-extend of it in
      ireg(G). */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD7) {
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         assign(t0, getXMMRegLane64(eregOfRM(modrm), 0));
         assign(t1, getXMMRegLane64(eregOfRM(modrm), 1));
         t5 = newTemp(Ity_I32);
         assign(t5,
                unop(Iop_16Uto32,
                     binop(Iop_8HLto16,
                           unop(Iop_GetMSBs8x8, mkexpr(t1)),
                           unop(Iop_GetMSBs8x8, mkexpr(t0)))));
         putIReg(4, gregOfRM(modrm), mkexpr(t5));
         DIP("pmovmskb %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameIReg(4,gregOfRM(modrm)));
         delta += 3;
         goto decode_success;
      } 
      /* else fall through */
   }

   /* 66 0F E4 = PMULHUW -- 16x8 hi-half of unsigned widening multiply */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE4) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pmulhuw", Iop_MulHi16Ux8, False );
      goto decode_success;
   }

   /* 66 0F E5 = PMULHW -- 16x8 hi-half of signed widening multiply */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE5) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pmulhw", Iop_MulHi16Sx8, False );
      goto decode_success;
   }

   /* 66 0F D5 = PMULHL -- 16x8 multiply */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD5) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "pmullw", Iop_Mul16x8, False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE2 ---*** */
   /* 0F F4 = PMULUDQ -- unsigned widening multiply of 32-lanes 0 x
      0 to form 64-bit result */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xF4) {
      IRTemp sV = newTemp(Ity_I64);
      IRTemp dV = newTemp(Ity_I64);
      t1 = newTemp(Ity_I32);
      t0 = newTemp(Ity_I32);
      modrm = insn[2];

      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("pmuludq %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 2+alen;
         DIP("pmuludq %s,%s\n", dis_buf,
                                nameMMXReg(gregOfRM(modrm)));
      }

      assign( t0, unop(Iop_64to32, mkexpr(dV)) );
      assign( t1, unop(Iop_64to32, mkexpr(sV)) );
      putMMXReg( gregOfRM(modrm),
                 binop( Iop_MullU32, mkexpr(t0), mkexpr(t1) ) );
      goto decode_success;
   }

   /* 66 0F F4 = PMULUDQ -- unsigned widening multiply of 32-lanes 0 x
      0 to form lower 64-bit half and lanes 2 x 2 to form upper 64-bit
      half */
   /* This is a really poor translation -- could be improved if
      performance critical */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF4) {
      IRTemp sV, dV;
      IRTemp s3, s2, s1, s0, d3, d2, d1, d0;
      sV = newTemp(Ity_V128);
      dV = newTemp(Ity_V128);
      s3 = s2 = s1 = s0 = d3 = d2 = d1 = d0 = IRTemp_INVALID;
      t1 = newTemp(Ity_I64);
      t0 = newTemp(Ity_I64);
      modrm = insn[2];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("pmuludq %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("pmuludq %s,%s\n", dis_buf,
                                nameXMMReg(gregOfRM(modrm)));
      }

      breakup128to32s( dV, &d3, &d2, &d1, &d0 );
      breakup128to32s( sV, &s3, &s2, &s1, &s0 );

      assign( t0, binop( Iop_MullU32, mkexpr(d0), mkexpr(s0)) );
      putXMMRegLane64( gregOfRM(modrm), 0, mkexpr(t0) );
      assign( t1, binop( Iop_MullU32, mkexpr(d2), mkexpr(s2)) );
      putXMMRegLane64( gregOfRM(modrm), 1, mkexpr(t1) );
      goto decode_success;
   }

   /* 66 0F EB = POR */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xEB) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "por", Iop_OrV128 );
      goto decode_success;
   }

   /* 66 0F F6 = PSADBW -- 2 x (8x8 -> 48 zeroes ++ u16) Sum Abs Diffs
      from E(xmm or mem) to G(xmm) */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF6) {
      IRTemp s1V  = newTemp(Ity_V128);
      IRTemp s2V  = newTemp(Ity_V128);
      IRTemp dV   = newTemp(Ity_V128);
      IRTemp s1Hi = newTemp(Ity_I64);
      IRTemp s1Lo = newTemp(Ity_I64);
      IRTemp s2Hi = newTemp(Ity_I64);
      IRTemp s2Lo = newTemp(Ity_I64);
      IRTemp dHi  = newTemp(Ity_I64);
      IRTemp dLo  = newTemp(Ity_I64);
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         assign( s1V, getXMMReg(eregOfRM(modrm)) );
         delta += 2+1;
         DIP("psadbw %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                               nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( s1V, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 2+alen;
         DIP("psadbw %s,%s\n", dis_buf,
                               nameXMMReg(gregOfRM(modrm)));
      }
      assign( s2V, getXMMReg(gregOfRM(modrm)) );
      assign( s1Hi, unop(Iop_V128HIto64, mkexpr(s1V)) );
      assign( s1Lo, unop(Iop_V128to64,   mkexpr(s1V)) );
      assign( s2Hi, unop(Iop_V128HIto64, mkexpr(s2V)) );
      assign( s2Lo, unop(Iop_V128to64,   mkexpr(s2V)) );
      assign( dHi, mkIRExprCCall(
                      Ity_I64, 0/*regparms*/,
                      "x86g_calculate_mmx_psadbw", 
                      &x86g_calculate_mmx_psadbw,
                      mkIRExprVec_2( mkexpr(s1Hi), mkexpr(s2Hi))
                   ));
      assign( dLo, mkIRExprCCall(
                      Ity_I64, 0/*regparms*/,
                      "x86g_calculate_mmx_psadbw", 
                      &x86g_calculate_mmx_psadbw,
                      mkIRExprVec_2( mkexpr(s1Lo), mkexpr(s2Lo))
                   ));
      assign( dV, binop(Iop_64HLtoV128, mkexpr(dHi), mkexpr(dLo))) ;
      putXMMReg(gregOfRM(modrm), mkexpr(dV));
      goto decode_success;
   }

   /* 66 0F 70 = PSHUFD -- rearrange 4x32 from E(xmm or mem) to G(xmm) */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x70) {
      Int order;
      IRTemp sV, dV, s3, s2, s1, s0;
      s3 = s2 = s1 = s0 = IRTemp_INVALID;
      sV = newTemp(Ity_V128);
      dV = newTemp(Ity_V128);
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         order = (Int)insn[3];
         delta += 2+2;
         DIP("pshufd $%d,%s,%s\n", order, 
                                   nameXMMReg(eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
	 order = (Int)insn[2+alen];
         delta += 3+alen;
         DIP("pshufd $%d,%s,%s\n", order, 
                                   dis_buf,
                                   nameXMMReg(gregOfRM(modrm)));
      }
      breakup128to32s( sV, &s3, &s2, &s1, &s0 );

#     define SEL(n) \
                ((n)==0 ? s0 : ((n)==1 ? s1 : ((n)==2 ? s2 : s3)))
      assign(dV,
	     mk128from32s( SEL((order>>6)&3), SEL((order>>4)&3),
                           SEL((order>>2)&3), SEL((order>>0)&3) )
      );
      putXMMReg(gregOfRM(modrm), mkexpr(dV));
#     undef SEL
      goto decode_success;
   }

   /* F3 0F 70 = PSHUFHW -- rearrange upper half 4x16 from E(xmm or
      mem) to G(xmm), and copy lower half */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x70) {
      Int order;
      IRTemp sVhi, dVhi, sV, dV, s3, s2, s1, s0;
      s3 = s2 = s1 = s0 = IRTemp_INVALID;
      sV   = newTemp(Ity_V128);
      dV   = newTemp(Ity_V128);
      sVhi = newTemp(Ity_I64);
      dVhi = newTemp(Ity_I64);
      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         order = (Int)insn[4];
         delta += 4+1;
         DIP("pshufhw $%d,%s,%s\n", order, 
                                    nameXMMReg(eregOfRM(modrm)),
                                    nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
	 order = (Int)insn[3+alen];
         delta += 4+alen;
         DIP("pshufhw $%d,%s,%s\n", order, 
                                    dis_buf,
                                    nameXMMReg(gregOfRM(modrm)));
      }
      assign( sVhi, unop(Iop_V128HIto64, mkexpr(sV)) );
      breakup64to16s( sVhi, &s3, &s2, &s1, &s0 );

#     define SEL(n) \
                ((n)==0 ? s0 : ((n)==1 ? s1 : ((n)==2 ? s2 : s3)))
      assign(dVhi,
	     mk64from16s( SEL((order>>6)&3), SEL((order>>4)&3),
                          SEL((order>>2)&3), SEL((order>>0)&3) )
      );
      assign(dV, binop( Iop_64HLtoV128, 
                        mkexpr(dVhi),
                        unop(Iop_V128to64, mkexpr(sV))) );
      putXMMReg(gregOfRM(modrm), mkexpr(dV));
#     undef SEL
      goto decode_success;
   }

   /* F2 0F 70 = PSHUFLW -- rearrange lower half 4x16 from E(xmm or
      mem) to G(xmm), and copy upper half */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x70) {
      Int order;
      IRTemp sVlo, dVlo, sV, dV, s3, s2, s1, s0;
      s3 = s2 = s1 = s0 = IRTemp_INVALID;
      sV   = newTemp(Ity_V128);
      dV   = newTemp(Ity_V128);
      sVlo = newTemp(Ity_I64);
      dVlo = newTemp(Ity_I64);
      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         order = (Int)insn[4];
         delta += 4+1;
         DIP("pshuflw $%d,%s,%s\n", order, 
                                    nameXMMReg(eregOfRM(modrm)),
                                    nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
	 order = (Int)insn[3+alen];
         delta += 4+alen;
         DIP("pshuflw $%d,%s,%s\n", order, 
                                    dis_buf,
                                    nameXMMReg(gregOfRM(modrm)));
      }
      assign( sVlo, unop(Iop_V128to64, mkexpr(sV)) );
      breakup64to16s( sVlo, &s3, &s2, &s1, &s0 );

#     define SEL(n) \
                ((n)==0 ? s0 : ((n)==1 ? s1 : ((n)==2 ? s2 : s3)))
      assign(dVlo,
	     mk64from16s( SEL((order>>6)&3), SEL((order>>4)&3),
                          SEL((order>>2)&3), SEL((order>>0)&3) )
      );
      assign(dV, binop( Iop_64HLtoV128,
                        unop(Iop_V128HIto64, mkexpr(sV)),
                        mkexpr(dVlo) ) );
      putXMMReg(gregOfRM(modrm), mkexpr(dV));
#     undef SEL
      goto decode_success;
   }

   /* 66 0F 72 /6 ib = PSLLD by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x72
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 6) {
      delta = dis_SSE_shiftE_imm( delta+2, "pslld", Iop_ShlN32x4 );
      goto decode_success;
   }

   /* 66 0F F2 = PSLLD by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF2) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "pslld", Iop_ShlN32x4 );
      goto decode_success;
   }

   /* 66 0F 73 /7 ib = PSLLDQ by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x73
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 7) {
      IRTemp sV, dV, hi64, lo64, hi64r, lo64r;
      Int    imm = (Int)insn[3];
      Int    reg = eregOfRM(insn[2]);
      DIP("pslldq $%d,%s\n", imm, nameXMMReg(reg));
      vassert(imm >= 0 && imm <= 255);
      delta += 4;

      sV    = newTemp(Ity_V128);
      dV    = newTemp(Ity_V128);
      hi64  = newTemp(Ity_I64);
      lo64  = newTemp(Ity_I64);
      hi64r = newTemp(Ity_I64);
      lo64r = newTemp(Ity_I64);

      if (imm >= 16) {
         putXMMReg(reg, mkV128(0x0000));
         goto decode_success;
      }

      assign( sV, getXMMReg(reg) );
      assign( hi64, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( lo64, unop(Iop_V128to64, mkexpr(sV)) );

      if (imm == 0) {
         assign( lo64r, mkexpr(lo64) );
         assign( hi64r, mkexpr(hi64) );
      }
      else
      if (imm == 8) {
         assign( lo64r, mkU64(0) );
         assign( hi64r, mkexpr(lo64) );
      }
      else
      if (imm > 8) {
         assign( lo64r, mkU64(0) );
         assign( hi64r, binop( Iop_Shl64, 
                               mkexpr(lo64),
                               mkU8( 8*(imm-8) ) ));
      } else {
         assign( lo64r, binop( Iop_Shl64, 
                               mkexpr(lo64),
                               mkU8(8 * imm) ));
         assign( hi64r, 
                 binop( Iop_Or64,
                        binop(Iop_Shl64, mkexpr(hi64), 
                                         mkU8(8 * imm)),
                        binop(Iop_Shr64, mkexpr(lo64),
                                         mkU8(8 * (8 - imm)) )
                      )
               );
      }
      assign( dV, binop(Iop_64HLtoV128, mkexpr(hi64r), mkexpr(lo64r)) );
      putXMMReg(reg, mkexpr(dV));
      goto decode_success;
   }

   /* 66 0F 73 /6 ib = PSLLQ by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x73
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 6) {
      delta = dis_SSE_shiftE_imm( delta+2, "psllq", Iop_ShlN64x2 );
      goto decode_success;
   }

   /* 66 0F F3 = PSLLQ by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF3) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psllq", Iop_ShlN64x2 );
      goto decode_success;
   }

   /* 66 0F 71 /6 ib = PSLLW by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x71
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 6) {
      delta = dis_SSE_shiftE_imm( delta+2, "psllw", Iop_ShlN16x8 );
      goto decode_success;
   }

   /* 66 0F F1 = PSLLW by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF1) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psllw", Iop_ShlN16x8 );
      goto decode_success;
   }

   /* 66 0F 72 /4 ib = PSRAD by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x72
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 4) {
      delta = dis_SSE_shiftE_imm( delta+2, "psrad", Iop_SarN32x4 );
      goto decode_success;
   }

   /* 66 0F E2 = PSRAD by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE2) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psrad", Iop_SarN32x4 );
      goto decode_success;
   }

   /* 66 0F 71 /4 ib = PSRAW by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x71
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 4) {
      delta = dis_SSE_shiftE_imm( delta+2, "psraw", Iop_SarN16x8 );
      goto decode_success;
   }

   /* 66 0F E1 = PSRAW by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE1) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psraw", Iop_SarN16x8 );
      goto decode_success;
   }

   /* 66 0F 72 /2 ib = PSRLD by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x72
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 2) {
      delta = dis_SSE_shiftE_imm( delta+2, "psrld", Iop_ShrN32x4 );
      goto decode_success;
   }

   /* 66 0F D2 = PSRLD by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD2) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psrld", Iop_ShrN32x4 );
      goto decode_success;
   }

   /* 66 0F 73 /3 ib = PSRLDQ by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x73
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 3) {
      IRTemp sV, dV, hi64, lo64, hi64r, lo64r;
      Int    imm = (Int)insn[3];
      Int    reg = eregOfRM(insn[2]);
      DIP("psrldq $%d,%s\n", imm, nameXMMReg(reg));
      vassert(imm >= 0 && imm <= 255);
      delta += 4;

      sV    = newTemp(Ity_V128);
      dV    = newTemp(Ity_V128);
      hi64  = newTemp(Ity_I64);
      lo64  = newTemp(Ity_I64);
      hi64r = newTemp(Ity_I64);
      lo64r = newTemp(Ity_I64);

      if (imm >= 16) {
         putXMMReg(reg, mkV128(0x0000));
         goto decode_success;
      }

      assign( sV, getXMMReg(reg) );
      assign( hi64, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( lo64, unop(Iop_V128to64, mkexpr(sV)) );

      if (imm == 0) {
         assign( lo64r, mkexpr(lo64) );
         assign( hi64r, mkexpr(hi64) );
      }
      else
      if (imm == 8) {
         assign( hi64r, mkU64(0) );
         assign( lo64r, mkexpr(hi64) );
      }
      else 
      if (imm > 8) {
         assign( hi64r, mkU64(0) );
         assign( lo64r, binop( Iop_Shr64, 
                               mkexpr(hi64),
                               mkU8( 8*(imm-8) ) ));
      } else {
         assign( hi64r, binop( Iop_Shr64, 
                               mkexpr(hi64),
                               mkU8(8 * imm) ));
         assign( lo64r, 
                 binop( Iop_Or64,
                        binop(Iop_Shr64, mkexpr(lo64), 
                                         mkU8(8 * imm)),
                        binop(Iop_Shl64, mkexpr(hi64),
                                         mkU8(8 * (8 - imm)) )
                      )
               );
      }

      assign( dV, binop(Iop_64HLtoV128, mkexpr(hi64r), mkexpr(lo64r)) );
      putXMMReg(reg, mkexpr(dV));
      goto decode_success;
   }

   /* 66 0F 73 /2 ib = PSRLQ by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x73
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 2) {
      delta = dis_SSE_shiftE_imm( delta+2, "psrlq", Iop_ShrN64x2 );
      goto decode_success;
   }

   /* 66 0F D3 = PSRLQ by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD3) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psrlq", Iop_ShrN64x2 );
      goto decode_success;
   }

   /* 66 0F 71 /2 ib = PSRLW by immediate */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x71
       && epartIsReg(insn[2])
       && gregOfRM(insn[2]) == 2) {
      delta = dis_SSE_shiftE_imm( delta+2, "psrlw", Iop_ShrN16x8 );
      goto decode_success;
   }

   /* 66 0F D1 = PSRLW by E */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD1) {
      delta = dis_SSE_shiftG_byE( sorb, delta+2, "psrlw", Iop_ShrN16x8 );
      goto decode_success;
   }

   /* 66 0F F8 = PSUBB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF8) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubb", Iop_Sub8x16, False );
      goto decode_success;
   }

   /* 66 0F FA = PSUBD */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xFA) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubd", Iop_Sub32x4, False );
      goto decode_success;
   }

   /* ***--- this is an MMX class insn introduced in SSE2 ---*** */
   /* 0F FB = PSUBQ -- sub 64x1 */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xFB) {
      do_MMX_preamble();
      delta = dis_MMXop_regmem_to_reg ( 
                sorb, delta+2, insn[1], "psubq", False );
      goto decode_success;
   }

   /* 66 0F FB = PSUBQ */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xFB) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubq", Iop_Sub64x2, False );
      goto decode_success;
   }

   /* 66 0F F9 = PSUBW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xF9) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubw", Iop_Sub16x8, False );
      goto decode_success;
   }

   /* 66 0F E8 = PSUBSB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE8) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubsb", Iop_QSub8Sx16, False );
      goto decode_success;
   }

   /* 66 0F E9 = PSUBSW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xE9) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubsw", Iop_QSub16Sx8, False );
      goto decode_success;
   }

   /* 66 0F D8 = PSUBSB */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD8) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubusb", Iop_QSub8Ux16, False );
      goto decode_success;
   }

   /* 66 0F D9 = PSUBSW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD9) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "psubusw", Iop_QSub16Ux8, False );
      goto decode_success;
   }

   /* 66 0F 68 = PUNPCKHBW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x68) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpckhbw",
                                 Iop_InterleaveHI8x16, True );
      goto decode_success;
   }

   /* 66 0F 6A = PUNPCKHDQ */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x6A) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpckhdq",
                                 Iop_InterleaveHI32x4, True );
      goto decode_success;
   }

   /* 66 0F 6D = PUNPCKHQDQ */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x6D) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpckhqdq",
                                 Iop_InterleaveHI64x2, True );
      goto decode_success;
   }

   /* 66 0F 69 = PUNPCKHWD */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x69) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpckhwd",
                                 Iop_InterleaveHI16x8, True );
      goto decode_success;
   }

   /* 66 0F 60 = PUNPCKLBW */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x60) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpcklbw",
                                 Iop_InterleaveLO8x16, True );
      goto decode_success;
   }

   /* 66 0F 62 = PUNPCKLDQ */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x62) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpckldq",
                                 Iop_InterleaveLO32x4, True );
      goto decode_success;
   }

   /* 66 0F 6C = PUNPCKLQDQ */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x6C) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpcklqdq",
                                 Iop_InterleaveLO64x2, True );
      goto decode_success;
   }

   /* 66 0F 61 = PUNPCKLWD */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0x61) {
      delta = dis_SSEint_E_to_G( sorb, delta+2, 
                                 "punpcklwd",
                                 Iop_InterleaveLO16x8, True );
      goto decode_success;
   }

   /* 66 0F EF = PXOR */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xEF) {
      delta = dis_SSE_E_to_G_all( sorb, delta+2, "pxor", Iop_XorV128 );
      goto decode_success;
   }

//--    /* FXSAVE/FXRSTOR m32 -- load/store the FPU/MMX/SSE state. */
//--    if (insn[0] == 0x0F && insn[1] == 0xAE 
//--        && (!epartIsReg(insn[2]))
//--        && (gregOfRM(insn[2]) == 1 || gregOfRM(insn[2]) == 0) ) {
//--       Bool store = gregOfRM(insn[2]) == 0;
//--       vg_assert(sz == 4);
//--       pair = disAMode ( cb, sorb, eip+2, dis_buf );
//--       t1   = LOW24(pair);
//--       eip += 2+HI8(pair);
//--       uInstr3(cb, store ? SSE2a_MemWr : SSE2a_MemRd, 512,
//--                   Lit16, (((UShort)insn[0]) << 8) | (UShort)insn[1],
//--                   Lit16, (UShort)insn[2],
//--                   TempReg, t1 );
//--       DIP("fx%s %s\n", store ? "save" : "rstor", dis_buf );
//--       goto decode_success;
//--    }

   /* 0F AE /7 = CLFLUSH -- flush cache line */
   if (sz == 4 && insn[0] == 0x0F && insn[1] == 0xAE
       && !epartIsReg(insn[2]) && gregOfRM(insn[2]) == 7) {

      /* This is something of a hack.  We need to know the size of the
         cache line containing addr.  Since we don't (easily), assume
         256 on the basis that no real cache would have a line that
         big.  It's safe to invalidate more stuff than we need, just
         inefficient. */
      UInt lineszB = 256;

      addr = disAMode ( &alen, sorb, delta+2, dis_buf );
      delta += 2+alen;

      /* Round addr down to the start of the containing block. */
      stmt( IRStmt_Put(
               OFFB_CMSTART,
               binop( Iop_And32, 
                      mkexpr(addr), 
                      mkU32( ~(lineszB-1) ))) );

      stmt( IRStmt_Put(OFFB_CMLEN, mkU32(lineszB) ) );

      jmp_lit(&dres, Ijk_InvalICache, (Addr32)(guest_EIP_bbstart+delta));

      DIP("clflush %s\n", dis_buf);
      goto decode_success;
   }

   /* ---------------------------------------------------- */
   /* --- end of the SSE2 decoder.                     --- */
   /* ---------------------------------------------------- */

   /* ---------------------------------------------------- */
   /* --- start of the SSE3 decoder.                   --- */
   /* ---------------------------------------------------- */

   /* Skip parts of the decoder which don't apply given the stated
      guest subarchitecture. */
   /* if (0 == (archinfo->hwcaps & VEX_HWCAPS_X86_SSE3)) */
   /* In fact this is highly bogus; we accept SSE3 insns even on a
      SSE2-only guest since they turn into IR which can be re-emitted
      successfully on an SSE2 host. */
   if (0 == (archinfo->hwcaps & VEX_HWCAPS_X86_SSE2))
      goto after_sse_decoders; /* no SSE3 capabilities */

   insn = (UChar*)&guest_code[delta];

   /* F3 0F 12 = MOVSLDUP -- move from E (mem or xmm) to G (xmm),
      duplicating some lanes (2:2:0:0). */
   /* F3 0F 16 = MOVSHDUP -- move from E (mem or xmm) to G (xmm),
      duplicating some lanes (3:3:1:1). */
   if (sz == 4 && insn[0] == 0xF3 && insn[1] == 0x0F 
       && (insn[2] == 0x12 || insn[2] == 0x16)) {
      IRTemp s3, s2, s1, s0;
      IRTemp sV  = newTemp(Ity_V128);
      Bool   isH = insn[2] == 0x16;
      s3 = s2 = s1 = s0 = IRTemp_INVALID;

      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg( eregOfRM(modrm)) );
         DIP("movs%cdup %s,%s\n", isH ? 'h' : 'l',
                                  nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         DIP("movs%cdup %s,%s\n", isH ? 'h' : 'l',
	     dis_buf,
             nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }

      breakup128to32s( sV, &s3, &s2, &s1, &s0 );
      putXMMReg( gregOfRM(modrm), 
                 isH ? mk128from32s( s3, s3, s1, s1 )
                     : mk128from32s( s2, s2, s0, s0 ) );
      goto decode_success;
   }

   /* F2 0F 12 = MOVDDUP -- move from E (mem or xmm) to G (xmm),
      duplicating some lanes (0:1:0:1). */
   if (sz == 4 && insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x12) {
      IRTemp sV = newTemp(Ity_V128);
      IRTemp d0 = newTemp(Ity_I64);

      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg( eregOfRM(modrm)) );
         DIP("movddup %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
         assign ( d0, unop(Iop_V128to64, mkexpr(sV)) );
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( d0, loadLE(Ity_I64, mkexpr(addr)) );
         DIP("movddup %s,%s\n", dis_buf,
                                nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }

      putXMMReg( gregOfRM(modrm), binop(Iop_64HLtoV128,mkexpr(d0),mkexpr(d0)) );
      goto decode_success;
   }

   /* F2 0F D0 = ADDSUBPS -- 32x4 +/-/+/- from E (mem or xmm) to G (xmm). */
   if (sz == 4 && insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0xD0) {
      IRTemp a3, a2, a1, a0, s3, s2, s1, s0;
      IRTemp eV   = newTemp(Ity_V128);
      IRTemp gV   = newTemp(Ity_V128);
      IRTemp addV = newTemp(Ity_V128);
      IRTemp subV = newTemp(Ity_V128);
      IRTemp rm     = newTemp(Ity_I32);
      a3 = a2 = a1 = a0 = s3 = s2 = s1 = s0 = IRTemp_INVALID;

      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign( eV, getXMMReg( eregOfRM(modrm)) );
         DIP("addsubps %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( eV, loadLE(Ity_V128, mkexpr(addr)) );
         DIP("addsubps %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }

      assign( gV, getXMMReg(gregOfRM(modrm)) );

      assign( rm, get_FAKE_roundingmode() ); /* XXXROUNDINGFIXME */
      assign( addV, triop(Iop_Add32Fx4, mkexpr(rm), mkexpr(gV), mkexpr(eV)) );
      assign( subV, triop(Iop_Sub32Fx4, mkexpr(rm), mkexpr(gV), mkexpr(eV)) );

      breakup128to32s( addV, &a3, &a2, &a1, &a0 );
      breakup128to32s( subV, &s3, &s2, &s1, &s0 );

      putXMMReg( gregOfRM(modrm), mk128from32s( a3, s2, a1, s0 ));
      goto decode_success;
   }

   /* 66 0F D0 = ADDSUBPD -- 64x4 +/- from E (mem or xmm) to G (xmm). */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xD0) {
      IRTemp eV   = newTemp(Ity_V128);
      IRTemp gV   = newTemp(Ity_V128);
      IRTemp addV = newTemp(Ity_V128);
      IRTemp subV = newTemp(Ity_V128);
      IRTemp a1     = newTemp(Ity_I64);
      IRTemp s0     = newTemp(Ity_I64);
      IRTemp rm     = newTemp(Ity_I32);

      modrm = insn[2];
      if (epartIsReg(modrm)) {
         assign( eV, getXMMReg( eregOfRM(modrm)) );
         DIP("addsubpd %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
         delta += 2+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( eV, loadLE(Ity_V128, mkexpr(addr)) );
         DIP("addsubpd %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)));
         delta += 2+alen;
      }

      assign( gV, getXMMReg(gregOfRM(modrm)) );

      assign( rm, get_FAKE_roundingmode() ); /* XXXROUNDINGFIXME */
      assign( addV, triop(Iop_Add64Fx2, mkexpr(rm), mkexpr(gV), mkexpr(eV)) );
      assign( subV, triop(Iop_Sub64Fx2, mkexpr(rm), mkexpr(gV), mkexpr(eV)) );

      assign( a1, unop(Iop_V128HIto64, mkexpr(addV) ));
      assign( s0, unop(Iop_V128to64,   mkexpr(subV) ));

      putXMMReg( gregOfRM(modrm), 
                 binop(Iop_64HLtoV128, mkexpr(a1), mkexpr(s0)) );
      goto decode_success;
   }

   /* F2 0F 7D = HSUBPS -- 32x4 sub across from E (mem or xmm) to G (xmm). */
   /* F2 0F 7C = HADDPS -- 32x4 add across from E (mem or xmm) to G (xmm). */
   if (sz == 4 && insn[0] == 0xF2 && insn[1] == 0x0F 
       && (insn[2] == 0x7C || insn[2] == 0x7D)) {
      IRTemp e3, e2, e1, e0, g3, g2, g1, g0;
      IRTemp eV     = newTemp(Ity_V128);
      IRTemp gV     = newTemp(Ity_V128);
      IRTemp leftV  = newTemp(Ity_V128);
      IRTemp rightV = newTemp(Ity_V128);
      IRTemp rm     = newTemp(Ity_I32);
      Bool   isAdd  = insn[2] == 0x7C;
      const HChar* str = isAdd ? "add" : "sub";
      e3 = e2 = e1 = e0 = g3 = g2 = g1 = g0 = IRTemp_INVALID;

      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign( eV, getXMMReg( eregOfRM(modrm)) );
         DIP("h%sps %s,%s\n", str, nameXMMReg(eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( eV, loadLE(Ity_V128, mkexpr(addr)) );
         DIP("h%sps %s,%s\n", str, dis_buf,
                                   nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }

      assign( gV, getXMMReg(gregOfRM(modrm)) );

      breakup128to32s( eV, &e3, &e2, &e1, &e0 );
      breakup128to32s( gV, &g3, &g2, &g1, &g0 );

      assign( leftV,  mk128from32s( e2, e0, g2, g0 ) );
      assign( rightV, mk128from32s( e3, e1, g3, g1 ) );

      assign( rm, get_FAKE_roundingmode() ); /* XXXROUNDINGFIXME */
      putXMMReg( gregOfRM(modrm), 
                 triop(isAdd ? Iop_Add32Fx4 : Iop_Sub32Fx4, 
                       mkexpr(rm), mkexpr(leftV), mkexpr(rightV) ) );
      goto decode_success;
   }

   /* 66 0F 7D = HSUBPD -- 64x2 sub across from E (mem or xmm) to G (xmm). */
   /* 66 0F 7C = HADDPD -- 64x2 add across from E (mem or xmm) to G (xmm). */
   if (sz == 2 && insn[0] == 0x0F && (insn[1] == 0x7C || insn[1] == 0x7D)) {
      IRTemp e1     = newTemp(Ity_I64);
      IRTemp e0     = newTemp(Ity_I64);
      IRTemp g1     = newTemp(Ity_I64);
      IRTemp g0     = newTemp(Ity_I64);
      IRTemp eV     = newTemp(Ity_V128);
      IRTemp gV     = newTemp(Ity_V128);
      IRTemp leftV  = newTemp(Ity_V128);
      IRTemp rightV = newTemp(Ity_V128);
      IRTemp rm     = newTemp(Ity_I32);
      Bool   isAdd  = insn[1] == 0x7C;
      const HChar* str = isAdd ? "add" : "sub";

      modrm = insn[2];
      if (epartIsReg(modrm)) {
         assign( eV, getXMMReg( eregOfRM(modrm)) );
         DIP("h%spd %s,%s\n", str, nameXMMReg(eregOfRM(modrm)),
                                   nameXMMReg(gregOfRM(modrm)));
         delta += 2+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+2, dis_buf );
         assign( eV, loadLE(Ity_V128, mkexpr(addr)) );
         DIP("h%spd %s,%s\n", str, dis_buf,
                              nameXMMReg(gregOfRM(modrm)));
         delta += 2+alen;
      }

      assign( gV, getXMMReg(gregOfRM(modrm)) );

      assign( e1, unop(Iop_V128HIto64, mkexpr(eV) ));
      assign( e0, unop(Iop_V128to64, mkexpr(eV) ));
      assign( g1, unop(Iop_V128HIto64, mkexpr(gV) ));
      assign( g0, unop(Iop_V128to64, mkexpr(gV) ));

      assign( leftV,  binop(Iop_64HLtoV128, mkexpr(e0),mkexpr(g0)) );
      assign( rightV, binop(Iop_64HLtoV128, mkexpr(e1),mkexpr(g1)) );

      assign( rm, get_FAKE_roundingmode() ); /* XXXROUNDINGFIXME */
      putXMMReg( gregOfRM(modrm), 
                 triop(isAdd ? Iop_Add64Fx2 : Iop_Sub64Fx2, 
                       mkexpr(rm), mkexpr(leftV), mkexpr(rightV) ) );
      goto decode_success;
   }

   /* F2 0F F0 = LDDQU -- move from E (mem or xmm) to G (xmm). */
   if (sz == 4 && insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0xF0) {
      modrm = getIByte(delta+3);
      if (epartIsReg(modrm)) {
         goto decode_failure;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         putXMMReg( gregOfRM(modrm), 
                    loadLE(Ity_V128, mkexpr(addr)) );
         DIP("lddqu %s,%s\n", dis_buf,
                              nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }
      goto decode_success;
   }

   /* ---------------------------------------------------- */
   /* --- end of the SSE3 decoder.                     --- */
   /* ---------------------------------------------------- */

   /* ---------------------------------------------------- */
   /* --- start of the SSSE3 decoder.                  --- */
   /* ---------------------------------------------------- */

   /* 0F 38 04 = PMADDUBSW -- Multiply and Add Packed Signed and
      Unsigned Bytes (MMX) */
   if (sz == 4
       && insn[0] == 0x0F && insn[1] == 0x38 && insn[2] == 0x04) {
      IRTemp sV        = newTemp(Ity_I64);
      IRTemp dV        = newTemp(Ity_I64);
      IRTemp sVoddsSX  = newTemp(Ity_I64);
      IRTemp sVevensSX = newTemp(Ity_I64);
      IRTemp dVoddsZX  = newTemp(Ity_I64);
      IRTemp dVevensZX = newTemp(Ity_I64);

      modrm = insn[3];
      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pmaddubsw %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                  nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pmaddubsw %s,%s\n", dis_buf,
                                  nameMMXReg(gregOfRM(modrm)));
      }

      /* compute dV unsigned x sV signed */
      assign( sVoddsSX,
              binop(Iop_SarN16x4, mkexpr(sV), mkU8(8)) );
      assign( sVevensSX,
              binop(Iop_SarN16x4, 
                    binop(Iop_ShlN16x4, mkexpr(sV), mkU8(8)), 
                    mkU8(8)) );
      assign( dVoddsZX,
              binop(Iop_ShrN16x4, mkexpr(dV), mkU8(8)) );
      assign( dVevensZX,
              binop(Iop_ShrN16x4,
                    binop(Iop_ShlN16x4, mkexpr(dV), mkU8(8)),
                    mkU8(8)) );

      putMMXReg(
         gregOfRM(modrm),
         binop(Iop_QAdd16Sx4,
               binop(Iop_Mul16x4, mkexpr(sVoddsSX), mkexpr(dVoddsZX)),
               binop(Iop_Mul16x4, mkexpr(sVevensSX), mkexpr(dVevensZX))
         )
      );
      goto decode_success;
   }

   /* 66 0F 38 04 = PMADDUBSW -- Multiply and Add Packed Signed and
      Unsigned Bytes (XMM) */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x38 && insn[2] == 0x04) {
      IRTemp sV        = newTemp(Ity_V128);
      IRTemp dV        = newTemp(Ity_V128);
      IRTemp sVoddsSX  = newTemp(Ity_V128);
      IRTemp sVevensSX = newTemp(Ity_V128);
      IRTemp dVoddsZX  = newTemp(Ity_V128);
      IRTemp dVevensZX = newTemp(Ity_V128);

      modrm = insn[3];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pmaddubsw %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pmaddubsw %s,%s\n", dis_buf,
                                  nameXMMReg(gregOfRM(modrm)));
      }

      /* compute dV unsigned x sV signed */
      assign( sVoddsSX,
              binop(Iop_SarN16x8, mkexpr(sV), mkU8(8)) );
      assign( sVevensSX,
              binop(Iop_SarN16x8, 
                    binop(Iop_ShlN16x8, mkexpr(sV), mkU8(8)), 
                    mkU8(8)) );
      assign( dVoddsZX,
              binop(Iop_ShrN16x8, mkexpr(dV), mkU8(8)) );
      assign( dVevensZX,
              binop(Iop_ShrN16x8,
                    binop(Iop_ShlN16x8, mkexpr(dV), mkU8(8)),
                    mkU8(8)) );

      putXMMReg(
         gregOfRM(modrm),
         binop(Iop_QAdd16Sx8,
               binop(Iop_Mul16x8, mkexpr(sVoddsSX), mkexpr(dVoddsZX)),
               binop(Iop_Mul16x8, mkexpr(sVevensSX), mkexpr(dVevensZX))
         )
      );
      goto decode_success;
   }

   /* ***--- these are MMX class insns introduced in SSSE3 ---*** */
   /* 0F 38 03 = PHADDSW -- 16x4 signed qadd across from E (mem or
      mmx) and G to G (mmx). */
   /* 0F 38 07 = PHSUBSW -- 16x4 signed qsub across from E (mem or
      mmx) and G to G (mmx). */
   /* 0F 38 01 = PHADDW -- 16x4 add across from E (mem or mmx) and G
      to G (mmx). */
   /* 0F 38 05 = PHSUBW -- 16x4 sub across from E (mem or mmx) and G
      to G (mmx). */
   /* 0F 38 02 = PHADDD -- 32x2 add across from E (mem or mmx) and G
      to G (mmx). */
   /* 0F 38 06 = PHSUBD -- 32x2 sub across from E (mem or mmx) and G
      to G (mmx). */

   if (sz == 4 
       && insn[0] == 0x0F && insn[1] == 0x38 
       && (insn[2] == 0x03 || insn[2] == 0x07 || insn[2] == 0x01
           || insn[2] == 0x05 || insn[2] == 0x02 || insn[2] == 0x06)) {
      const HChar* str = "???";
      IROp   opV64  = Iop_INVALID;
      IROp   opCatO = Iop_CatOddLanes16x4;
      IROp   opCatE = Iop_CatEvenLanes16x4;
      IRTemp sV     = newTemp(Ity_I64);
      IRTemp dV     = newTemp(Ity_I64);

      modrm = insn[3];

      switch (insn[2]) {
         case 0x03: opV64 = Iop_QAdd16Sx4; str = "addsw"; break;
         case 0x07: opV64 = Iop_QSub16Sx4; str = "subsw"; break;
         case 0x01: opV64 = Iop_Add16x4;   str = "addw";  break;
         case 0x05: opV64 = Iop_Sub16x4;   str = "subw";  break;
         case 0x02: opV64 = Iop_Add32x2;   str = "addd";  break;
         case 0x06: opV64 = Iop_Sub32x2;   str = "subd";  break;
         default: vassert(0);
      }
      if (insn[2] == 0x02 || insn[2] == 0x06) {
         opCatO = Iop_InterleaveHI32x2;
         opCatE = Iop_InterleaveLO32x2;
      }

      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("ph%s %s,%s\n", str, nameMMXReg(eregOfRM(modrm)),
                                  nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("ph%s %s,%s\n", str, dis_buf,
                                  nameMMXReg(gregOfRM(modrm)));
      }

      putMMXReg(
         gregOfRM(modrm),
         binop(opV64,
               binop(opCatE,mkexpr(sV),mkexpr(dV)),
               binop(opCatO,mkexpr(sV),mkexpr(dV))
         )
      );
      goto decode_success;
   }

   /* 66 0F 38 03 = PHADDSW -- 16x8 signed qadd across from E (mem or
      xmm) and G to G (xmm). */
   /* 66 0F 38 07 = PHSUBSW -- 16x8 signed qsub across from E (mem or
      xmm) and G to G (xmm). */
   /* 66 0F 38 01 = PHADDW -- 16x8 add across from E (mem or xmm) and
      G to G (xmm). */
   /* 66 0F 38 05 = PHSUBW -- 16x8 sub across from E (mem or xmm) and
      G to G (xmm). */
   /* 66 0F 38 02 = PHADDD -- 32x4 add across from E (mem or xmm) and
      G to G (xmm). */
   /* 66 0F 38 06 = PHSUBD -- 32x4 sub across from E (mem or xmm) and
      G to G (xmm). */

   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x38 
       && (insn[2] == 0x03 || insn[2] == 0x07 || insn[2] == 0x01
           || insn[2] == 0x05 || insn[2] == 0x02 || insn[2] == 0x06)) {
      const HChar* str = "???";
      IROp   opV64  = Iop_INVALID;
      IROp   opCatO = Iop_CatOddLanes16x4;
      IROp   opCatE = Iop_CatEvenLanes16x4;
      IRTemp sV     = newTemp(Ity_V128);
      IRTemp dV     = newTemp(Ity_V128);
      IRTemp sHi    = newTemp(Ity_I64);
      IRTemp sLo    = newTemp(Ity_I64);
      IRTemp dHi    = newTemp(Ity_I64);
      IRTemp dLo    = newTemp(Ity_I64);

      modrm = insn[3];

      switch (insn[2]) {
         case 0x03: opV64 = Iop_QAdd16Sx4; str = "addsw"; break;
         case 0x07: opV64 = Iop_QSub16Sx4; str = "subsw"; break;
         case 0x01: opV64 = Iop_Add16x4;   str = "addw";  break;
         case 0x05: opV64 = Iop_Sub16x4;   str = "subw";  break;
         case 0x02: opV64 = Iop_Add32x2;   str = "addd";  break;
         case 0x06: opV64 = Iop_Sub32x2;   str = "subd";  break;
         default: vassert(0);
      }
      if (insn[2] == 0x02 || insn[2] == 0x06) {
         opCatO = Iop_InterleaveHI32x2;
         opCatE = Iop_InterleaveLO32x2;
      }

      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg( eregOfRM(modrm)) );
         DIP("ph%s %s,%s\n", str, nameXMMReg(eregOfRM(modrm)),
                                  nameXMMReg(gregOfRM(modrm)));
         delta += 3+1;
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         DIP("ph%s %s,%s\n", str, dis_buf,
                             nameXMMReg(gregOfRM(modrm)));
         delta += 3+alen;
      }

      assign( dHi, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( dLo, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( sHi, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( sLo, unop(Iop_V128to64,   mkexpr(sV)) );

      /* This isn't a particularly efficient way to compute the
         result, but at least it avoids a proliferation of IROps,
         hence avoids complication all the backends. */
      putXMMReg(
         gregOfRM(modrm), 
         binop(Iop_64HLtoV128,
               binop(opV64,
                     binop(opCatE,mkexpr(sHi),mkexpr(sLo)),
                     binop(opCatO,mkexpr(sHi),mkexpr(sLo))
               ),
               binop(opV64,
                     binop(opCatE,mkexpr(dHi),mkexpr(dLo)),
                     binop(opCatO,mkexpr(dHi),mkexpr(dLo))
               )
         )
      );
      goto decode_success;
   }

   /* 0F 38 0B = PMULHRSW -- Packed Multiply High with Round and Scale
      (MMX) */
   if (sz == 4 
       && insn[0] == 0x0F && insn[1] == 0x38 && insn[2] == 0x0B) {
      IRTemp sV = newTemp(Ity_I64);
      IRTemp dV = newTemp(Ity_I64);

      modrm = insn[3];
      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pmulhrsw %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                                 nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pmulhrsw %s,%s\n", dis_buf,
                                 nameMMXReg(gregOfRM(modrm)));
      }

      putMMXReg(
         gregOfRM(modrm),
         dis_PMULHRSW_helper( mkexpr(sV), mkexpr(dV) )
      );
      goto decode_success;
   }

   /* 66 0F 38 0B = PMULHRSW -- Packed Multiply High with Round and
      Scale (XMM) */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x38 && insn[2] == 0x0B) {
      IRTemp sV  = newTemp(Ity_V128);
      IRTemp dV  = newTemp(Ity_V128);
      IRTemp sHi = newTemp(Ity_I64);
      IRTemp sLo = newTemp(Ity_I64);
      IRTemp dHi = newTemp(Ity_I64);
      IRTemp dLo = newTemp(Ity_I64);

      modrm = insn[3];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pmulhrsw %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                                 nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pmulhrsw %s,%s\n", dis_buf,
                                 nameXMMReg(gregOfRM(modrm)));
      }

      assign( dHi, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( dLo, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( sHi, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( sLo, unop(Iop_V128to64,   mkexpr(sV)) );

      putXMMReg(
         gregOfRM(modrm),
         binop(Iop_64HLtoV128,
               dis_PMULHRSW_helper( mkexpr(sHi), mkexpr(dHi) ),
               dis_PMULHRSW_helper( mkexpr(sLo), mkexpr(dLo) )
         )
      );
      goto decode_success;
   }

   /* 0F 38 08 = PSIGNB -- Packed Sign 8x8  (MMX) */
   /* 0F 38 09 = PSIGNW -- Packed Sign 16x4 (MMX) */
   /* 0F 38 09 = PSIGND -- Packed Sign 32x2 (MMX) */
   if (sz == 4 
       && insn[0] == 0x0F && insn[1] == 0x38 
       && (insn[2] == 0x08 || insn[2] == 0x09 || insn[2] == 0x0A)) {
      IRTemp sV      = newTemp(Ity_I64);
      IRTemp dV      = newTemp(Ity_I64);
      const HChar* str = "???";
      Int    laneszB = 0;

      switch (insn[2]) {
         case 0x08: laneszB = 1; str = "b"; break;
         case 0x09: laneszB = 2; str = "w"; break;
         case 0x0A: laneszB = 4; str = "d"; break;
         default: vassert(0);
      }

      modrm = insn[3];
      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("psign%s %s,%s\n", str, nameMMXReg(eregOfRM(modrm)),
                                     nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("psign%s %s,%s\n", str, dis_buf,
                                     nameMMXReg(gregOfRM(modrm)));
      }

      putMMXReg(
         gregOfRM(modrm),
         dis_PSIGN_helper( mkexpr(sV), mkexpr(dV), laneszB )
      );
      goto decode_success;
   }

   /* 66 0F 38 08 = PSIGNB -- Packed Sign 8x16 (XMM) */
   /* 66 0F 38 09 = PSIGNW -- Packed Sign 16x8 (XMM) */
   /* 66 0F 38 09 = PSIGND -- Packed Sign 32x4 (XMM) */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x38 
       && (insn[2] == 0x08 || insn[2] == 0x09 || insn[2] == 0x0A)) {
      IRTemp sV      = newTemp(Ity_V128);
      IRTemp dV      = newTemp(Ity_V128);
      IRTemp sHi     = newTemp(Ity_I64);
      IRTemp sLo     = newTemp(Ity_I64);
      IRTemp dHi     = newTemp(Ity_I64);
      IRTemp dLo     = newTemp(Ity_I64);
      const HChar* str = "???";
      Int    laneszB = 0;

      switch (insn[2]) {
         case 0x08: laneszB = 1; str = "b"; break;
         case 0x09: laneszB = 2; str = "w"; break;
         case 0x0A: laneszB = 4; str = "d"; break;
         default: vassert(0);
      }

      modrm = insn[3];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("psign%s %s,%s\n", str, nameXMMReg(eregOfRM(modrm)),
                                     nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("psign%s %s,%s\n", str, dis_buf,
                                     nameXMMReg(gregOfRM(modrm)));
      }

      assign( dHi, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( dLo, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( sHi, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( sLo, unop(Iop_V128to64,   mkexpr(sV)) );

      putXMMReg(
         gregOfRM(modrm),
         binop(Iop_64HLtoV128,
               dis_PSIGN_helper( mkexpr(sHi), mkexpr(dHi), laneszB ),
               dis_PSIGN_helper( mkexpr(sLo), mkexpr(dLo), laneszB )
         )
      );
      goto decode_success;
   }

   /* 0F 38 1C = PABSB -- Packed Absolute Value 8x8  (MMX) */
   /* 0F 38 1D = PABSW -- Packed Absolute Value 16x4 (MMX) */
   /* 0F 38 1E = PABSD -- Packed Absolute Value 32x2 (MMX) */
   if (sz == 4 
       && insn[0] == 0x0F && insn[1] == 0x38 
       && (insn[2] == 0x1C || insn[2] == 0x1D || insn[2] == 0x1E)) {
      IRTemp sV      = newTemp(Ity_I64);
      const HChar* str = "???";
      Int    laneszB = 0;

      switch (insn[2]) {
         case 0x1C: laneszB = 1; str = "b"; break;
         case 0x1D: laneszB = 2; str = "w"; break;
         case 0x1E: laneszB = 4; str = "d"; break;
         default: vassert(0);
      }

      modrm = insn[3];
      do_MMX_preamble();

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pabs%s %s,%s\n", str, nameMMXReg(eregOfRM(modrm)),
                                    nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pabs%s %s,%s\n", str, dis_buf,
                                    nameMMXReg(gregOfRM(modrm)));
      }

      putMMXReg(
         gregOfRM(modrm),
         dis_PABS_helper( mkexpr(sV), laneszB )
      );
      goto decode_success;
   }

   /* 66 0F 38 1C = PABSB -- Packed Absolute Value 8x16 (XMM) */
   /* 66 0F 38 1D = PABSW -- Packed Absolute Value 16x8 (XMM) */
   /* 66 0F 38 1E = PABSD -- Packed Absolute Value 32x4 (XMM) */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x38 
       && (insn[2] == 0x1C || insn[2] == 0x1D || insn[2] == 0x1E)) {
      IRTemp sV      = newTemp(Ity_V128);
      IRTemp sHi     = newTemp(Ity_I64);
      IRTemp sLo     = newTemp(Ity_I64);
      const HChar* str = "???";
      Int    laneszB = 0;

      switch (insn[2]) {
         case 0x1C: laneszB = 1; str = "b"; break;
         case 0x1D: laneszB = 2; str = "w"; break;
         case 0x1E: laneszB = 4; str = "d"; break;
         default: vassert(0);
      }

      modrm = insn[3];

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pabs%s %s,%s\n", str, nameXMMReg(eregOfRM(modrm)),
                                    nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pabs%s %s,%s\n", str, dis_buf,
                                    nameXMMReg(gregOfRM(modrm)));
      }

      assign( sHi, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( sLo, unop(Iop_V128to64,   mkexpr(sV)) );

      putXMMReg(
         gregOfRM(modrm),
         binop(Iop_64HLtoV128,
               dis_PABS_helper( mkexpr(sHi), laneszB ),
               dis_PABS_helper( mkexpr(sLo), laneszB )
         )
      );
      goto decode_success;
   }

   /* 0F 3A 0F = PALIGNR -- Packed Align Right (MMX) */
   if (sz == 4 
       && insn[0] == 0x0F && insn[1] == 0x3A && insn[2] == 0x0F) {
      IRTemp sV  = newTemp(Ity_I64);
      IRTemp dV  = newTemp(Ity_I64);
      IRTemp res = newTemp(Ity_I64);

      modrm = insn[3];
      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         d32 = (UInt)insn[3+1];
         delta += 3+1+1;
         DIP("palignr $%d,%s,%s\n",  (Int)d32, 
                                     nameMMXReg(eregOfRM(modrm)),
                                     nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         d32 = (UInt)insn[3+alen];
         delta += 3+alen+1;
         DIP("palignr $%d%s,%s\n", (Int)d32,
                                   dis_buf,
                                   nameMMXReg(gregOfRM(modrm)));
      }

      if (d32 == 0) {
         assign( res, mkexpr(sV) );
      }
      else if (d32 >= 1 && d32 <= 7) {
         assign(res, 
                binop(Iop_Or64,
                      binop(Iop_Shr64, mkexpr(sV), mkU8(8*d32)),
                      binop(Iop_Shl64, mkexpr(dV), mkU8(8*(8-d32))
                     )));
      }
      else if (d32 == 8) {
        assign( res, mkexpr(dV) );
      }
      else if (d32 >= 9 && d32 <= 15) {
         assign( res, binop(Iop_Shr64, mkexpr(dV), mkU8(8*(d32-8))) );
      }
      else if (d32 >= 16 && d32 <= 255) {
         assign( res, mkU64(0) );
      }
      else
         vassert(0);

      putMMXReg( gregOfRM(modrm), mkexpr(res) );
      goto decode_success;
   }

   /* 66 0F 3A 0F = PALIGNR -- Packed Align Right (XMM) */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x3A && insn[2] == 0x0F) {
      IRTemp sV  = newTemp(Ity_V128);
      IRTemp dV  = newTemp(Ity_V128);
      IRTemp sHi = newTemp(Ity_I64);
      IRTemp sLo = newTemp(Ity_I64);
      IRTemp dHi = newTemp(Ity_I64);
      IRTemp dLo = newTemp(Ity_I64);
      IRTemp rHi = newTemp(Ity_I64);
      IRTemp rLo = newTemp(Ity_I64);

      modrm = insn[3];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         d32 = (UInt)insn[3+1];
         delta += 3+1+1;
         DIP("palignr $%d,%s,%s\n", (Int)d32,
                                    nameXMMReg(eregOfRM(modrm)),
                                    nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         d32 = (UInt)insn[3+alen];
         delta += 3+alen+1;
         DIP("palignr $%d,%s,%s\n", (Int)d32,
                                    dis_buf,
                                    nameXMMReg(gregOfRM(modrm)));
      }

      assign( dHi, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( dLo, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( sHi, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( sLo, unop(Iop_V128to64,   mkexpr(sV)) );

      if (d32 == 0) {
         assign( rHi, mkexpr(sHi) );
         assign( rLo, mkexpr(sLo) );
      }
      else if (d32 >= 1 && d32 <= 7) {
         assign( rHi, dis_PALIGNR_XMM_helper(dLo, sHi, d32) );
         assign( rLo, dis_PALIGNR_XMM_helper(sHi, sLo, d32) );
      }
      else if (d32 == 8) {
         assign( rHi, mkexpr(dLo) );
         assign( rLo, mkexpr(sHi) );
      }
      else if (d32 >= 9 && d32 <= 15) {
         assign( rHi, dis_PALIGNR_XMM_helper(dHi, dLo, d32-8) );
         assign( rLo, dis_PALIGNR_XMM_helper(dLo, sHi, d32-8) );
      }
      else if (d32 == 16) {
         assign( rHi, mkexpr(dHi) );
         assign( rLo, mkexpr(dLo) );
      }
      else if (d32 >= 17 && d32 <= 23) {
         assign( rHi, binop(Iop_Shr64, mkexpr(dHi), mkU8(8*(d32-16))) );
         assign( rLo, dis_PALIGNR_XMM_helper(dHi, dLo, d32-16) );
      }
      else if (d32 == 24) {
         assign( rHi, mkU64(0) );
         assign( rLo, mkexpr(dHi) );
      }
      else if (d32 >= 25 && d32 <= 31) {
         assign( rHi, mkU64(0) );
         assign( rLo, binop(Iop_Shr64, mkexpr(dHi), mkU8(8*(d32-24))) );
      }
      else if (d32 >= 32 && d32 <= 255) {
         assign( rHi, mkU64(0) );
         assign( rLo, mkU64(0) );
      }
      else
         vassert(0);

      putXMMReg(
         gregOfRM(modrm),
         binop(Iop_64HLtoV128, mkexpr(rHi), mkexpr(rLo))
      );
      goto decode_success;
   }

   /* 0F 38 00 = PSHUFB -- Packed Shuffle Bytes 8x8 (MMX) */
   if (sz == 4 
       && insn[0] == 0x0F && insn[1] == 0x38 && insn[2] == 0x00) {
      IRTemp sV      = newTemp(Ity_I64);
      IRTemp dV      = newTemp(Ity_I64);

      modrm = insn[3];
      do_MMX_preamble();
      assign( dV, getMMXReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getMMXReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pshufb %s,%s\n", nameMMXReg(eregOfRM(modrm)),
                               nameMMXReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         assign( sV, loadLE(Ity_I64, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pshufb %s,%s\n", dis_buf,
                               nameMMXReg(gregOfRM(modrm)));
      }

      putMMXReg(
         gregOfRM(modrm),
         binop(
            Iop_And64,
            /* permute the lanes */
            binop(
               Iop_Perm8x8,
               mkexpr(dV),
               binop(Iop_And64, mkexpr(sV), mkU64(0x0707070707070707ULL))
            ),
            /* mask off lanes which have (index & 0x80) == 0x80 */
            unop(Iop_Not64, binop(Iop_SarN8x8, mkexpr(sV), mkU8(7)))
         )
      );
      goto decode_success;
   }

   /* 66 0F 38 00 = PSHUFB -- Packed Shuffle Bytes 8x16 (XMM) */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x38 && insn[2] == 0x00) {
      IRTemp sV         = newTemp(Ity_V128);
      IRTemp dV         = newTemp(Ity_V128);
      IRTemp sHi        = newTemp(Ity_I64);
      IRTemp sLo        = newTemp(Ity_I64);
      IRTemp dHi        = newTemp(Ity_I64);
      IRTemp dLo        = newTemp(Ity_I64);
      IRTemp rHi        = newTemp(Ity_I64);
      IRTemp rLo        = newTemp(Ity_I64);
      IRTemp sevens     = newTemp(Ity_I64);
      IRTemp mask0x80hi = newTemp(Ity_I64);
      IRTemp mask0x80lo = newTemp(Ity_I64);
      IRTemp maskBit3hi = newTemp(Ity_I64);
      IRTemp maskBit3lo = newTemp(Ity_I64);
      IRTemp sAnd7hi    = newTemp(Ity_I64);
      IRTemp sAnd7lo    = newTemp(Ity_I64);
      IRTemp permdHi    = newTemp(Ity_I64);
      IRTemp permdLo    = newTemp(Ity_I64);

      modrm = insn[3];
      assign( dV, getXMMReg(gregOfRM(modrm)) );

      if (epartIsReg(modrm)) {
         assign( sV, getXMMReg(eregOfRM(modrm)) );
         delta += 3+1;
         DIP("pshufb %s,%s\n", nameXMMReg(eregOfRM(modrm)),
                               nameXMMReg(gregOfRM(modrm)));
      } else {
         addr = disAMode ( &alen, sorb, delta+3, dis_buf );
         gen_SEGV_if_not_16_aligned( addr );
         assign( sV, loadLE(Ity_V128, mkexpr(addr)) );
         delta += 3+alen;
         DIP("pshufb %s,%s\n", dis_buf,
                               nameXMMReg(gregOfRM(modrm)));
      }

      assign( dHi, unop(Iop_V128HIto64, mkexpr(dV)) );
      assign( dLo, unop(Iop_V128to64,   mkexpr(dV)) );
      assign( sHi, unop(Iop_V128HIto64, mkexpr(sV)) );
      assign( sLo, unop(Iop_V128to64,   mkexpr(sV)) );

      assign( sevens, mkU64(0x0707070707070707ULL) );

      /*
      mask0x80hi = Not(SarN8x8(sHi,7))
      maskBit3hi = SarN8x8(ShlN8x8(sHi,4),7)
      sAnd7hi    = And(sHi,sevens)
      permdHi    = Or( And(Perm8x8(dHi,sAnd7hi),maskBit3hi),
                       And(Perm8x8(dLo,sAnd7hi),Not(maskBit3hi)) )
      rHi        = And(permdHi,mask0x80hi)
      */
      assign(
         mask0x80hi,
         unop(Iop_Not64, binop(Iop_SarN8x8,mkexpr(sHi),mkU8(7))));

      assign(
         maskBit3hi,
         binop(Iop_SarN8x8,
               binop(Iop_ShlN8x8,mkexpr(sHi),mkU8(4)),
               mkU8(7)));

      assign(sAnd7hi, binop(Iop_And64,mkexpr(sHi),mkexpr(sevens)));

      assign(
         permdHi,
         binop(
            Iop_Or64,
            binop(Iop_And64,
                  binop(Iop_Perm8x8,mkexpr(dHi),mkexpr(sAnd7hi)),
                  mkexpr(maskBit3hi)),
            binop(Iop_And64,
                  binop(Iop_Perm8x8,mkexpr(dLo),mkexpr(sAnd7hi)),
                  unop(Iop_Not64,mkexpr(maskBit3hi))) ));

      assign(rHi, binop(Iop_And64,mkexpr(permdHi),mkexpr(mask0x80hi)) );

      /* And the same for the lower half of the result.  What fun. */

      assign(
         mask0x80lo,
         unop(Iop_Not64, binop(Iop_SarN8x8,mkexpr(sLo),mkU8(7))));

      assign(
         maskBit3lo,
         binop(Iop_SarN8x8,
               binop(Iop_ShlN8x8,mkexpr(sLo),mkU8(4)),
               mkU8(7)));

      assign(sAnd7lo, binop(Iop_And64,mkexpr(sLo),mkexpr(sevens)));

      assign(
         permdLo,
         binop(
            Iop_Or64,
            binop(Iop_And64,
                  binop(Iop_Perm8x8,mkexpr(dHi),mkexpr(sAnd7lo)),
                  mkexpr(maskBit3lo)),
            binop(Iop_And64,
                  binop(Iop_Perm8x8,mkexpr(dLo),mkexpr(sAnd7lo)),
                  unop(Iop_Not64,mkexpr(maskBit3lo))) ));

      assign(rLo, binop(Iop_And64,mkexpr(permdLo),mkexpr(mask0x80lo)) );

      putXMMReg(
         gregOfRM(modrm),
         binop(Iop_64HLtoV128, mkexpr(rHi), mkexpr(rLo))
      );
      goto decode_success;
   }
   
   /* 0F 38 F0 = MOVBE m16/32(E), r16/32(G) */
   /* 0F 38 F1 = MOVBE r16/32(G), m16/32(E) */
   if ((sz == 2 || sz == 4)
       && insn[0] == 0x0F && insn[1] == 0x38
       && (insn[2] == 0xF0 || insn[2] == 0xF1)
       && !epartIsReg(insn[3])) {

      modrm = insn[3];
      addr = disAMode(&alen, sorb, delta + 3, dis_buf);
      delta += 3 + alen;
      ty = szToITy(sz);
      IRTemp src = newTemp(ty);

      if (insn[2] == 0xF0) { /* LOAD */
         assign(src, loadLE(ty, mkexpr(addr)));
         IRTemp dst = math_BSWAP(src, ty);
         putIReg(sz, gregOfRM(modrm), mkexpr(dst));
         DIP("movbe %s,%s\n", dis_buf, nameIReg(sz, gregOfRM(modrm)));
      } else { /* STORE */
         assign(src, getIReg(sz, gregOfRM(modrm)));
         IRTemp dst = math_BSWAP(src, ty);
         storeLE(mkexpr(addr), mkexpr(dst));
         DIP("movbe %s,%s\n", nameIReg(sz, gregOfRM(modrm)), dis_buf);
      }
      goto decode_success;
   }

   /* ---------------------------------------------------- */
   /* --- end of the SSSE3 decoder.                    --- */
   /* ---------------------------------------------------- */

   /* ---------------------------------------------------- */
   /* --- start of the SSE4 decoder                    --- */
   /* ---------------------------------------------------- */

   /* 66 0F 3A 0B /r ib = ROUNDSD imm8, xmm2/m64, xmm1
      (Partial implementation only -- only deal with cases where
      the rounding mode is specified directly by the immediate byte.)
      66 0F 3A 0A /r ib = ROUNDSS imm8, xmm2/m32, xmm1
      (Limitations ditto)
   */
   if (sz == 2 
       && insn[0] == 0x0F && insn[1] == 0x3A
       && (/*insn[2] == 0x0B || */insn[2] == 0x0A)) {

      Bool   isD = insn[2] == 0x0B;
      IRTemp src = newTemp(isD ? Ity_F64 : Ity_F32);
      IRTemp res = newTemp(isD ? Ity_F64 : Ity_F32);
      Int    imm = 0;

      modrm = insn[3];

      if (epartIsReg(modrm)) {
         assign( src, 
                 isD ? getXMMRegLane64F( eregOfRM(modrm), 0 )
                     : getXMMRegLane32F( eregOfRM(modrm), 0 ) );
         imm = insn[3+1];
         if (imm & ~3) goto decode_failure;
         delta += 3+1+1;
         DIP( "rounds%c $%d,%s,%s\n",
              isD ? 'd' : 's',
              imm, nameXMMReg( eregOfRM(modrm) ),
                   nameXMMReg( gregOfRM(modrm) ) );
      } else {
         addr = disAMode( &alen, sorb, delta+3, dis_buf );
         assign( src, loadLE( isD ? Ity_F64 : Ity_F32, mkexpr(addr) ));
         imm = insn[3+alen];
         if (imm & ~3) goto decode_failure;
         delta += 3+alen+1;
         DIP( "roundsd $%d,%s,%s\n",
              imm, dis_buf, nameXMMReg( gregOfRM(modrm) ) );
      }

      /* (imm & 3) contains an Intel-encoded rounding mode.  Because
         that encoding is the same as the encoding for IRRoundingMode,
         we can use that value directly in the IR as a rounding
         mode. */
      assign(res, binop(isD ? Iop_RoundF64toInt : Iop_RoundF32toInt,
                  mkU32(imm & 3), mkexpr(src)) );

      if (isD)
         putXMMRegLane64F( gregOfRM(modrm), 0, mkexpr(res) );
      else
         putXMMRegLane32F( gregOfRM(modrm), 0, mkexpr(res) );

      goto decode_success;
   }

   /* F3 0F BD -- LZCNT (count leading zeroes.  An AMD extension,
      which we can only decode if we're sure this is an AMD cpu that
      supports LZCNT, since otherwise it's BSR, which behaves
      differently. */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0xBD
       && 0 != (archinfo->hwcaps & VEX_HWCAPS_X86_LZCNT)) {
      vassert(sz == 2 || sz == 4);
      /*IRType*/ ty  = szToITy(sz);
      IRTemp     src = newTemp(ty);
      modrm = insn[3];
      if (epartIsReg(modrm)) {
         assign(src, getIReg(sz, eregOfRM(modrm)));
         delta += 3+1;
         DIP("lzcnt%c %s, %s\n", nameISize(sz),
             nameIReg(sz, eregOfRM(modrm)),
             nameIReg(sz, gregOfRM(modrm)));
      } else {
         addr = disAMode( &alen, sorb, delta+3, dis_buf );
         assign(src, loadLE(ty, mkexpr(addr)));
         delta += 3+alen;
         DIP("lzcnt%c %s, %s\n", nameISize(sz), dis_buf,
             nameIReg(sz, gregOfRM(modrm)));
      }

      IRTemp res = gen_LZCNT(ty, src);
      putIReg(sz, gregOfRM(modrm), mkexpr(res));

      // Update flags.  This is pretty lame .. perhaps can do better
      // if this turns out to be performance critical.
      // O S A P are cleared.  Z is set if RESULT == 0.
      // C is set if SRC is zero.
      IRTemp src32 = newTemp(Ity_I32);
      IRTemp res32 = newTemp(Ity_I32);
      assign(src32, widenUto32(mkexpr(src)));
      assign(res32, widenUto32(mkexpr(res)));

      IRTemp oszacp = newTemp(Ity_I32);
      assign(
         oszacp,
         binop(Iop_Or32,
               binop(Iop_Shl32,
                     unop(Iop_1Uto32,
                          binop(Iop_CmpEQ32, mkexpr(res32), mkU32(0))),
                     mkU8(X86G_CC_SHIFT_Z)),
               binop(Iop_Shl32,
                     unop(Iop_1Uto32,
                          binop(Iop_CmpEQ32, mkexpr(src32), mkU32(0))),
                     mkU8(X86G_CC_SHIFT_C))
         )
      );

      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
      stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(oszacp) ));

      goto decode_success;
   }

   /* ---------------------------------------------------- */
   /* --- end of the SSE4 decoder                      --- */
   /* ---------------------------------------------------- */

   after_sse_decoders:

   /* ---------------------------------------------------- */
   /* --- deal with misc 0x67 pfxs (addr size override) -- */
   /* ---------------------------------------------------- */

   /* 67 E3 = JCXZ (for JECXZ see below) */
   if (insn[0] == 0x67 && insn[1] == 0xE3 && sz == 4) {
      delta += 2;
      d32 = (((Addr32)guest_EIP_bbstart)+delta+1) + getSDisp8(delta);
      delta ++;
      stmt( IRStmt_Exit(
               binop(Iop_CmpEQ16, getIReg(2,R_ECX), mkU16(0)),
               Ijk_Boring,
               IRConst_U32(d32),
               OFFB_EIP
            ));
       DIP("jcxz 0x%x\n", d32);
       goto decode_success;
   }

   /* ---------------------------------------------------- */
   /* --- start of the baseline insn decoder            -- */
   /* ---------------------------------------------------- */

   /* Get the primary opcode. */
   opc = getIByte(delta); delta++;

   /* We get here if the current insn isn't SSE, or this CPU doesn't
      support SSE. */

   switch (opc) {

   /* ------------------------ Control flow --------------- */

   case 0xC2: /* RET imm16 */
      d32 = getUDisp16(delta); 
      delta += 2;
      dis_ret(&dres, d32);
      DIP("ret %d\n", (Int)d32);
      break;
   case 0xC3: /* RET */
      dis_ret(&dres, 0);
      DIP("ret\n");
      break;

   case 0xCF: /* IRET */
      /* Note, this is an extremely kludgey and limited implementation
         of iret.  All it really does is: 
            popl %EIP; popl %CS; popl %EFLAGS.
         %CS is set but ignored (as it is in (eg) popw %cs)". */
      t1 = newTemp(Ity_I32); /* ESP */
      t2 = newTemp(Ity_I32); /* new EIP */
      t3 = newTemp(Ity_I32); /* new CS */
      t4 = newTemp(Ity_I32); /* new EFLAGS */
      assign(t1, getIReg(4,R_ESP));
      assign(t2, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t1),mkU32(0) )));
      assign(t3, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t1),mkU32(4) )));
      assign(t4, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t1),mkU32(8) )));
      /* Get stuff off stack */
      putIReg(4, R_ESP,binop(Iop_Add32, mkexpr(t1), mkU32(12)));
      /* set %CS (which is ignored anyway) */
      putSReg( R_CS, unop(Iop_32to16, mkexpr(t3)) );
      /* set %EFLAGS */
      set_EFLAGS_from_value( t4, False/*!emit_AC_emwarn*/, 0/*unused*/ );
      /* goto new EIP value */
      jmp_treg(&dres, Ijk_Ret, t2);
      vassert(dres.whatNext == Dis_StopHere);
      DIP("iret (very kludgey)\n");
      break;

   case 0xE8: /* CALL J4 */
      d32 = getUDisp32(delta); delta += 4;
      d32 += (guest_EIP_bbstart+delta); 
      /* (guest_eip_bbstart+delta) == return-to addr, d32 == call-to addr */
      if (d32 == guest_EIP_bbstart+delta && getIByte(delta) >= 0x58 
                                         && getIByte(delta) <= 0x5F) {
         /* Specially treat the position-independent-code idiom 
                 call X
              X: popl %reg
            as 
                 movl %eip, %reg.
            since this generates better code, but for no other reason. */
         Int archReg = getIByte(delta) - 0x58;
         /* vex_printf("-- fPIC thingy\n"); */
         putIReg(4, archReg, mkU32(guest_EIP_bbstart+delta));
         delta++; /* Step over the POP */
         DIP("call 0x%x ; popl %s\n",d32,nameIReg(4,archReg));
      } else {
         /* The normal sequence for a call. */
         t1 = newTemp(Ity_I32); 
         assign(t1, binop(Iop_Sub32, getIReg(4,R_ESP), mkU32(4)));
         putIReg(4, R_ESP, mkexpr(t1));
         storeLE( mkexpr(t1), mkU32(guest_EIP_bbstart+delta));
         if (resteerOkFn( callback_opaque, (Addr64)(Addr32)d32 )) {
            /* follow into the call target. */
            dres.whatNext   = Dis_ResteerU;
            dres.continueAt = (Addr64)(Addr32)d32;
         } else {
            jmp_lit(&dres, Ijk_Call, d32);
            vassert(dres.whatNext == Dis_StopHere);
         }
         DIP("call 0x%x\n",d32);
      }
      break;

//--    case 0xC8: /* ENTER */ 
//--       d32 = getUDisp16(eip); eip += 2;
//--       abyte = getIByte(delta); delta++;
//-- 
//--       vg_assert(sz == 4);           
//--       vg_assert(abyte == 0);
//-- 
//--       t1 = newTemp(cb); t2 = newTemp(cb);
//--       uInstr2(cb, GET,   sz, ArchReg, R_EBP, TempReg, t1);
//--       uInstr2(cb, GET,    4, ArchReg, R_ESP, TempReg, t2);
//--       uInstr2(cb, SUB,    4, Literal, 0,     TempReg, t2);
//--       uLiteral(cb, sz);
//--       uInstr2(cb, PUT,    4, TempReg, t2,    ArchReg, R_ESP);
//--       uInstr2(cb, STORE,  4, TempReg, t1,    TempReg, t2);
//--       uInstr2(cb, PUT,    4, TempReg, t2,    ArchReg, R_EBP);
//--       if (d32) {
//--          uInstr2(cb, SUB,    4, Literal, 0,     TempReg, t2);
//--          uLiteral(cb, d32);
//--          uInstr2(cb, PUT,    4, TempReg, t2,    ArchReg, R_ESP);
//--       }
//--       DIP("enter 0x%x, 0x%x", d32, abyte);
//--       break;

   case 0xC9: /* LEAVE */
      vassert(sz == 4);
      t1 = newTemp(Ity_I32); t2 = newTemp(Ity_I32);
      assign(t1, getIReg(4,R_EBP));
      /* First PUT ESP looks redundant, but need it because ESP must
         always be up-to-date for Memcheck to work... */
      putIReg(4, R_ESP, mkexpr(t1));
      assign(t2, loadLE(Ity_I32,mkexpr(t1)));
      putIReg(4, R_EBP, mkexpr(t2));
      putIReg(4, R_ESP, binop(Iop_Add32, mkexpr(t1), mkU32(4)) );
      DIP("leave\n");
      break;

   /* ---------------- Misc weird-ass insns --------------- */

   case 0x27: /* DAA */
   case 0x2F: /* DAS */
   case 0x37: /* AAA */
   case 0x3F: /* AAS */
      /* An ugly implementation for some ugly instructions.  Oh
	 well. */
      if (sz != 4) goto decode_failure;
      t1 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I32);
      /* Make up a 32-bit value (t1), with the old value of AX in the
         bottom 16 bits, and the old OSZACP bitmask in the upper 16
         bits. */
      assign(t1, 
             binop(Iop_16HLto32, 
                   unop(Iop_32to16,
                        mk_x86g_calculate_eflags_all()),
                   getIReg(2, R_EAX)
            ));
      /* Call the helper fn, to get a new AX and OSZACP value, and
         poke both back into the guest state.  Also pass the helper
         the actual opcode so it knows which of the 4 instructions it
         is doing the computation for. */
      vassert(opc == 0x27 || opc == 0x2F || opc == 0x37 || opc == 0x3F);
      assign(t2,
              mkIRExprCCall(
                 Ity_I32, 0/*regparm*/, "x86g_calculate_daa_das_aaa_aas",
                 &x86g_calculate_daa_das_aaa_aas,
                 mkIRExprVec_2( mkexpr(t1), mkU32( opc & 0xFF) )
            ));
     putIReg(2, R_EAX, unop(Iop_32to16, mkexpr(t2) ));

     stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
     stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
     stmt( IRStmt_Put( OFFB_CC_DEP1, 
                       binop(Iop_And32,
                             binop(Iop_Shr32, mkexpr(t2), mkU8(16)),
                             mkU32( X86G_CC_MASK_C | X86G_CC_MASK_P 
                                    | X86G_CC_MASK_A | X86G_CC_MASK_Z 
                                    | X86G_CC_MASK_S| X86G_CC_MASK_O )
                            )
                      )
         );
     /* Set NDEP even though it isn't used.  This makes redundant-PUT
        elimination of previous stores to this field work better. */
     stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
     switch (opc) {
        case 0x27: DIP("daa\n"); break;
        case 0x2F: DIP("das\n"); break;
        case 0x37: DIP("aaa\n"); break;
        case 0x3F: DIP("aas\n"); break;
        default: vassert(0);
     }
     break;

   case 0xD4: /* AAM */
   case 0xD5: /* AAD */
      d32 = getIByte(delta); delta++;
      if (sz != 4 || d32 != 10) goto decode_failure;
      t1 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I32);
      /* Make up a 32-bit value (t1), with the old value of AX in the
         bottom 16 bits, and the old OSZACP bitmask in the upper 16
         bits. */
      assign(t1, 
             binop(Iop_16HLto32, 
                   unop(Iop_32to16,
                        mk_x86g_calculate_eflags_all()),
                   getIReg(2, R_EAX)
            ));
      /* Call the helper fn, to get a new AX and OSZACP value, and
         poke both back into the guest state.  Also pass the helper
         the actual opcode so it knows which of the 2 instructions it
         is doing the computation for. */
      assign(t2,
              mkIRExprCCall(
                 Ity_I32, 0/*regparm*/, "x86g_calculate_aad_aam",
                 &x86g_calculate_aad_aam,
                 mkIRExprVec_2( mkexpr(t1), mkU32( opc & 0xFF) )
            ));
      putIReg(2, R_EAX, unop(Iop_32to16, mkexpr(t2) ));

      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
      stmt( IRStmt_Put( OFFB_CC_DEP1, 
                        binop(Iop_And32,
                              binop(Iop_Shr32, mkexpr(t2), mkU8(16)),
                              mkU32( X86G_CC_MASK_C | X86G_CC_MASK_P 
                                     | X86G_CC_MASK_A | X86G_CC_MASK_Z 
                                     | X86G_CC_MASK_S| X86G_CC_MASK_O )
                             )
                       )
          );
      /* Set NDEP even though it isn't used.  This makes
         redundant-PUT elimination of previous stores to this field
         work better. */
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));

      DIP(opc == 0xD4 ? "aam\n" : "aad\n");
      break;

   /* ------------------------ CWD/CDQ -------------------- */

   case 0x98: /* CBW */
      if (sz == 4) {
         putIReg(4, R_EAX, unop(Iop_16Sto32, getIReg(2, R_EAX)));
         DIP("cwde\n");
      } else {
         vassert(sz == 2);
         putIReg(2, R_EAX, unop(Iop_8Sto16, getIReg(1, R_EAX)));
         DIP("cbw\n");
      }
      break;

   case 0x99: /* CWD/CDQ */
      ty = szToITy(sz);
      putIReg(sz, R_EDX,
                  binop(mkSizedOp(ty,Iop_Sar8), 
                        getIReg(sz, R_EAX),
                        mkU8(sz == 2 ? 15 : 31)) );
      DIP(sz == 2 ? "cwdq\n" : "cdqq\n");
      break;

   /* ------------------------ FPU ops -------------------- */

   case 0x9E: /* SAHF */
      codegen_SAHF();
      DIP("sahf\n");
      break;

   case 0x9F: /* LAHF */
      codegen_LAHF();
      DIP("lahf\n");
      break;

   case 0x9B: /* FWAIT */
      /* ignore? */
      DIP("fwait\n");
      break;

   case 0xD8:
   case 0xD9:
   case 0xDA:
   case 0xDB:
   case 0xDC:
   case 0xDD:
   case 0xDE:
   case 0xDF: {
      Int  delta0    = delta;
      Bool decode_OK = False;
      delta = dis_FPU ( &decode_OK, sorb, delta );
      if (!decode_OK) {
         delta = delta0;
         goto decode_failure;
      }
      break;
   }

   /* ------------------------ INC & DEC ------------------ */

   case 0x40: /* INC eAX */
   case 0x41: /* INC eCX */
   case 0x42: /* INC eDX */
   case 0x43: /* INC eBX */
   case 0x44: /* INC eSP */
   case 0x45: /* INC eBP */
   case 0x46: /* INC eSI */
   case 0x47: /* INC eDI */
      vassert(sz == 2 || sz == 4);
      ty = szToITy(sz);
      t1 = newTemp(ty);
      assign( t1, binop(mkSizedOp(ty,Iop_Add8),
                        getIReg(sz, (UInt)(opc - 0x40)),
                        mkU(ty,1)) );
      setFlags_INC_DEC( True, t1, ty );
      putIReg(sz, (UInt)(opc - 0x40), mkexpr(t1));
      DIP("inc%c %s\n", nameISize(sz), nameIReg(sz,opc-0x40));
      break;

   case 0x48: /* DEC eAX */
   case 0x49: /* DEC eCX */
   case 0x4A: /* DEC eDX */
   case 0x4B: /* DEC eBX */
   case 0x4C: /* DEC eSP */
   case 0x4D: /* DEC eBP */
   case 0x4E: /* DEC eSI */
   case 0x4F: /* DEC eDI */
      vassert(sz == 2 || sz == 4);
      ty = szToITy(sz);
      t1 = newTemp(ty);
      assign( t1, binop(mkSizedOp(ty,Iop_Sub8),
                        getIReg(sz, (UInt)(opc - 0x48)),
                        mkU(ty,1)) );
      setFlags_INC_DEC( False, t1, ty );
      putIReg(sz, (UInt)(opc - 0x48), mkexpr(t1));
      DIP("dec%c %s\n", nameISize(sz), nameIReg(sz,opc-0x48));
      break;

   /* ------------------------ INT ------------------------ */

   case 0xCC: /* INT 3 */
      jmp_lit(&dres, Ijk_SigTRAP, ((Addr32)guest_EIP_bbstart)+delta);
      vassert(dres.whatNext == Dis_StopHere);
      DIP("int $0x3\n");
      break;

   case 0xCD: /* INT imm8 */
      d32 = getIByte(delta); delta++;

      /* For any of the cases where we emit a jump (that is, for all
         currently handled cases), it's important that all ArchRegs
         carry their up-to-date value at this point.  So we declare an
         end-of-block here, which forces any TempRegs caching ArchRegs
         to be flushed. */

      /* Handle int $0x3F .. $0x4F by synthesising a segfault and a
         restart of this instruction (hence the "-2" two lines below,
         to get the restart EIP to be this instruction.  This is
         probably Linux-specific and it would be more correct to only
         do this if the VexAbiInfo says that is what we should do.
         This used to handle just 0x40-0x43; Jikes RVM uses a larger
         range (0x3F-0x49), and this allows some slack as well. */
      if (d32 >= 0x3F && d32 <= 0x4F) {
         jmp_lit(&dres, Ijk_SigSEGV, ((Addr32)guest_EIP_bbstart)+delta-2);
         vassert(dres.whatNext == Dis_StopHere);
         DIP("int $0x%x\n", (Int)d32);
         break;
      }

      /* Handle int $0x80 (linux syscalls), int $0x81 and $0x82
         (darwin syscalls).  As part of this, note where we are, so we
         can back up the guest to this point if the syscall needs to
         be restarted. */
      if (d32 == 0x80) {
         stmt( IRStmt_Put( OFFB_IP_AT_SYSCALL,
                           mkU32(guest_EIP_curr_instr) ) );
         jmp_lit(&dres, Ijk_Sys_int128, ((Addr32)guest_EIP_bbstart)+delta);
         vassert(dres.whatNext == Dis_StopHere);
         DIP("int $0x80\n");
         break;
      }
      if (d32 == 0x81) {
         stmt( IRStmt_Put( OFFB_IP_AT_SYSCALL,
                           mkU32(guest_EIP_curr_instr) ) );
         jmp_lit(&dres, Ijk_Sys_int129, ((Addr32)guest_EIP_bbstart)+delta);
         vassert(dres.whatNext == Dis_StopHere);
         DIP("int $0x81\n");
         break;
      }
      if (d32 == 0x82) {
         stmt( IRStmt_Put( OFFB_IP_AT_SYSCALL,
                           mkU32(guest_EIP_curr_instr) ) );
         jmp_lit(&dres, Ijk_Sys_int130, ((Addr32)guest_EIP_bbstart)+delta);
         vassert(dres.whatNext == Dis_StopHere);
         DIP("int $0x82\n");
         break;
      }

      /* none of the above */
      goto decode_failure;

   /* ------------------------ Jcond, byte offset --------- */

   case 0xEB: /* Jb (jump, byte offset) */
      d32 = (((Addr32)guest_EIP_bbstart)+delta+1) + getSDisp8(delta); 
      delta++;
      if (resteerOkFn( callback_opaque, (Addr64)(Addr32)d32) ) {
         dres.whatNext   = Dis_ResteerU;
         dres.continueAt = (Addr64)(Addr32)d32;
      } else {
         jmp_lit(&dres, Ijk_Boring, d32);
         vassert(dres.whatNext == Dis_StopHere);
      }
      DIP("jmp-8 0x%x\n", d32);
      break;

   case 0xE9: /* Jv (jump, 16/32 offset) */
      vassert(sz == 4); /* JRS added 2004 July 11 */
      d32 = (((Addr32)guest_EIP_bbstart)+delta+sz) + getSDisp(sz,delta); 
      delta += sz;
      if (resteerOkFn( callback_opaque, (Addr64)(Addr32)d32) ) {
         dres.whatNext   = Dis_ResteerU;
         dres.continueAt = (Addr64)(Addr32)d32;
      } else {
         jmp_lit(&dres, Ijk_Boring, d32);
         vassert(dres.whatNext == Dis_StopHere);
      }
      DIP("jmp 0x%x\n", d32);
      break;

   case 0x70:
   case 0x71:
   case 0x72: /* JBb/JNAEb (jump below) */
   case 0x73: /* JNBb/JAEb (jump not below) */
   case 0x74: /* JZb/JEb (jump zero) */
   case 0x75: /* JNZb/JNEb (jump not zero) */
   case 0x76: /* JBEb/JNAb (jump below or equal) */
   case 0x77: /* JNBEb/JAb (jump not below or equal) */
   case 0x78: /* JSb (jump negative) */
   case 0x79: /* JSb (jump not negative) */
   case 0x7A: /* JP (jump parity even) */
   case 0x7B: /* JNP/JPO (jump parity odd) */
   case 0x7C: /* JLb/JNGEb (jump less) */
   case 0x7D: /* JGEb/JNLb (jump greater or equal) */
   case 0x7E: /* JLEb/JNGb (jump less or equal) */
   case 0x7F: /* JGb/JNLEb (jump greater) */
    { Int    jmpDelta;
      const HChar* comment  = "";
      jmpDelta = (Int)getSDisp8(delta);
      vassert(-128 <= jmpDelta && jmpDelta < 128);
      d32 = (((Addr32)guest_EIP_bbstart)+delta+1) + jmpDelta; 
      delta++;
      if (resteerCisOk
          && vex_control.guest_chase_cond
          && (Addr32)d32 != (Addr32)guest_EIP_bbstart
          && jmpDelta < 0
          && resteerOkFn( callback_opaque, (Addr64)(Addr32)d32) ) {
         /* Speculation: assume this backward branch is taken.  So we
            need to emit a side-exit to the insn following this one,
            on the negation of the condition, and continue at the
            branch target address (d32).  If we wind up back at the
            first instruction of the trace, just stop; it's better to
            let the IR loop unroller handle that case. */
         stmt( IRStmt_Exit( 
                  mk_x86g_calculate_condition((X86Condcode)(1 ^ (opc - 0x70))),
                  Ijk_Boring,
                  IRConst_U32(guest_EIP_bbstart+delta),
                  OFFB_EIP ) );
         dres.whatNext   = Dis_ResteerC;
         dres.continueAt = (Addr64)(Addr32)d32;
         comment = "(assumed taken)";
      }
      else
      if (resteerCisOk
          && vex_control.guest_chase_cond
          && (Addr32)d32 != (Addr32)guest_EIP_bbstart
          && jmpDelta >= 0
          && resteerOkFn( callback_opaque, 
                          (Addr64)(Addr32)(guest_EIP_bbstart+delta)) ) {
         /* Speculation: assume this forward branch is not taken.  So
            we need to emit a side-exit to d32 (the dest) and continue
            disassembling at the insn immediately following this
            one. */
         stmt( IRStmt_Exit( 
                  mk_x86g_calculate_condition((X86Condcode)(opc - 0x70)),
                  Ijk_Boring,
                  IRConst_U32(d32),
                  OFFB_EIP ) );
         dres.whatNext   = Dis_ResteerC;
         dres.continueAt = (Addr64)(Addr32)(guest_EIP_bbstart+delta);
         comment = "(assumed not taken)";
      }
      else {
         /* Conservative default translation - end the block at this
            point. */
         jcc_01( &dres, (X86Condcode)(opc - 0x70), 
                 (Addr32)(guest_EIP_bbstart+delta), d32);
         vassert(dres.whatNext == Dis_StopHere);
      }
      DIP("j%s-8 0x%x %s\n", name_X86Condcode(opc - 0x70), d32, comment);
      break;
    }

   case 0xE3: /* JECXZ (for JCXZ see above) */
      if (sz != 4) goto decode_failure;
      d32 = (((Addr32)guest_EIP_bbstart)+delta+1) + getSDisp8(delta);
      delta ++;
      stmt( IRStmt_Exit(
               binop(Iop_CmpEQ32, getIReg(4,R_ECX), mkU32(0)),
            Ijk_Boring,
            IRConst_U32(d32),
            OFFB_EIP
          ));
      DIP("jecxz 0x%x\n", d32);
      break;

   case 0xE0: /* LOOPNE disp8: decrement count, jump if count != 0 && ZF==0 */
   case 0xE1: /* LOOPE  disp8: decrement count, jump if count != 0 && ZF==1 */
   case 0xE2: /* LOOP   disp8: decrement count, jump if count != 0 */
    { /* Again, the docs say this uses ECX/CX as a count depending on
         the address size override, not the operand one.  Since we
         don't handle address size overrides, I guess that means
         ECX. */
      IRExpr* zbit  = NULL;
      IRExpr* count = NULL;
      IRExpr* cond  = NULL;
      const HChar* xtra = NULL;

      if (sz != 4) goto decode_failure;
      d32 = (((Addr32)guest_EIP_bbstart)+delta+1) + getSDisp8(delta);
      delta++;
      putIReg(4, R_ECX, binop(Iop_Sub32, getIReg(4,R_ECX), mkU32(1)));

      count = getIReg(4,R_ECX);
      cond = binop(Iop_CmpNE32, count, mkU32(0));
      switch (opc) {
         case 0xE2: 
            xtra = ""; 
            break;
         case 0xE1: 
            xtra = "e"; 
            zbit = mk_x86g_calculate_condition( X86CondZ );
	    cond = mkAnd1(cond, zbit);
            break;
         case 0xE0: 
            xtra = "ne";
            zbit = mk_x86g_calculate_condition( X86CondNZ );
	    cond = mkAnd1(cond, zbit);
            break;
         default:
	    vassert(0);
      }
      stmt( IRStmt_Exit(cond, Ijk_Boring, IRConst_U32(d32), OFFB_EIP) );

      DIP("loop%s 0x%x\n", xtra, d32);
      break;
    }

   /* ------------------------ IMUL ----------------------- */

   case 0x69: /* IMUL Iv, Ev, Gv */
      delta = dis_imul_I_E_G ( sorb, sz, delta, sz );
      break;
   case 0x6B: /* IMUL Ib, Ev, Gv */
      delta = dis_imul_I_E_G ( sorb, sz, delta, 1 );
      break;

   /* ------------------------ MOV ------------------------ */

   case 0x88: /* MOV Gb,Eb */
      delta = dis_mov_G_E(sorb, 1, delta);
      break;

   case 0x89: /* MOV Gv,Ev */
      delta = dis_mov_G_E(sorb, sz, delta);
      break;

   case 0x8A: /* MOV Eb,Gb */
      delta = dis_mov_E_G(sorb, 1, delta);
      break;
 
   case 0x8B: /* MOV Ev,Gv */
      delta = dis_mov_E_G(sorb, sz, delta);
      break;
 
   case 0x8D: /* LEA M,Gv */
      if (sz != 4)
         goto decode_failure;
      modrm = getIByte(delta);
      if (epartIsReg(modrm)) 
         goto decode_failure;
      /* NOTE!  this is the one place where a segment override prefix
         has no effect on the address calculation.  Therefore we pass
         zero instead of sorb here. */
      addr = disAMode ( &alen, /*sorb*/ 0, delta, dis_buf );
      delta += alen;
      putIReg(sz, gregOfRM(modrm), mkexpr(addr));
      DIP("lea%c %s, %s\n", nameISize(sz), dis_buf, 
                            nameIReg(sz,gregOfRM(modrm)));
      break;

   case 0x8C: /* MOV Sw,Ew -- MOV from a SEGMENT REGISTER */
      delta = dis_mov_Sw_Ew(sorb, sz, delta);
      break;

   case 0x8E: /* MOV Ew,Sw -- MOV to a SEGMENT REGISTER */
      delta = dis_mov_Ew_Sw(sorb, delta);
      break;
 
   case 0xA0: /* MOV Ob,AL */
      sz = 1;
      /* Fall through ... */
   case 0xA1: /* MOV Ov,eAX */
      d32 = getUDisp32(delta); delta += 4;
      ty = szToITy(sz);
      addr = newTemp(Ity_I32);
      assign( addr, handleSegOverride(sorb, mkU32(d32)) );
      putIReg(sz, R_EAX, loadLE(ty, mkexpr(addr)));
      DIP("mov%c %s0x%x, %s\n", nameISize(sz), sorbTxt(sorb),
                                d32, nameIReg(sz,R_EAX));
      break;

   case 0xA2: /* MOV Ob,AL */
      sz = 1;
      /* Fall through ... */
   case 0xA3: /* MOV eAX,Ov */
      d32 = getUDisp32(delta); delta += 4;
      ty = szToITy(sz);
      addr = newTemp(Ity_I32);
      assign( addr, handleSegOverride(sorb, mkU32(d32)) );
      storeLE( mkexpr(addr), getIReg(sz,R_EAX) );
      DIP("mov%c %s, %s0x%x\n", nameISize(sz), nameIReg(sz,R_EAX),
                                sorbTxt(sorb), d32);
      break;

   case 0xB0: /* MOV imm,AL */
   case 0xB1: /* MOV imm,CL */
   case 0xB2: /* MOV imm,DL */
   case 0xB3: /* MOV imm,BL */
   case 0xB4: /* MOV imm,AH */
   case 0xB5: /* MOV imm,CH */
   case 0xB6: /* MOV imm,DH */
   case 0xB7: /* MOV imm,BH */
      d32 = getIByte(delta); delta += 1;
      putIReg(1, opc-0xB0, mkU8(d32));
      DIP("movb $0x%x,%s\n", d32, nameIReg(1,opc-0xB0));
      break;

   case 0xB8: /* MOV imm,eAX */
   case 0xB9: /* MOV imm,eCX */
   case 0xBA: /* MOV imm,eDX */
   case 0xBB: /* MOV imm,eBX */
   case 0xBC: /* MOV imm,eSP */
   case 0xBD: /* MOV imm,eBP */
   case 0xBE: /* MOV imm,eSI */
   case 0xBF: /* MOV imm,eDI */
      d32 = getUDisp(sz,delta); delta += sz;
      putIReg(sz, opc-0xB8, mkU(szToITy(sz), d32));
      DIP("mov%c $0x%x,%s\n", nameISize(sz), d32, nameIReg(sz,opc-0xB8));
      break;

   case 0xC6: /* C6 /0 = MOV Ib,Eb */
      sz = 1;
      goto maybe_do_Mov_I_E;
   case 0xC7: /* C7 /0 = MOV Iv,Ev */
      goto maybe_do_Mov_I_E;

   maybe_do_Mov_I_E:
      modrm = getIByte(delta);
      if (gregOfRM(modrm) == 0) {
         if (epartIsReg(modrm)) {
            delta++; /* mod/rm byte */
            d32 = getUDisp(sz,delta); delta += sz;
            putIReg(sz, eregOfRM(modrm), mkU(szToITy(sz), d32));
            DIP("mov%c $0x%x, %s\n", nameISize(sz), d32, 
                                     nameIReg(sz,eregOfRM(modrm)));
         } else {
            addr = disAMode ( &alen, sorb, delta, dis_buf );
            delta += alen;
            d32 = getUDisp(sz,delta); delta += sz;
            storeLE(mkexpr(addr), mkU(szToITy(sz), d32));
            DIP("mov%c $0x%x, %s\n", nameISize(sz), d32, dis_buf);
         }
         break;
      }
      goto decode_failure;

   /* ------------------------ opl imm, A ----------------- */

   case 0x04: /* ADD Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_Add8, True, delta, "add" );
      break;
   case 0x05: /* ADD Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_Add8, True, delta, "add" );
      break;

   case 0x0C: /* OR Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_Or8, True, delta, "or" );
      break;
   case 0x0D: /* OR Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_Or8, True, delta, "or" );
      break;

   case 0x14: /* ADC Ib, AL */
      delta = dis_op_imm_A(  1, True, Iop_Add8, True, delta, "adc" );
      break;
   case 0x15: /* ADC Iv, eAX */
      delta = dis_op_imm_A( sz, True, Iop_Add8, True, delta, "adc" );
      break;

   case 0x1C: /* SBB Ib, AL */
      delta = dis_op_imm_A( 1, True, Iop_Sub8, True, delta, "sbb" );
      break;
   case 0x1D: /* SBB Iv, eAX */
      delta = dis_op_imm_A( sz, True, Iop_Sub8, True, delta, "sbb" );
      break;

   case 0x24: /* AND Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_And8, True, delta, "and" );
      break;
   case 0x25: /* AND Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_And8, True, delta, "and" );
      break;

   case 0x2C: /* SUB Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_Sub8, True, delta, "sub" );
      break;
   case 0x2D: /* SUB Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_Sub8, True, delta, "sub" );
      break;

   case 0x34: /* XOR Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_Xor8, True, delta, "xor" );
      break;
   case 0x35: /* XOR Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_Xor8, True, delta, "xor" );
      break;

   case 0x3C: /* CMP Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_Sub8, False, delta, "cmp" );
      break;
   case 0x3D: /* CMP Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_Sub8, False, delta, "cmp" );
      break;

   case 0xA8: /* TEST Ib, AL */
      delta = dis_op_imm_A(  1, False, Iop_And8, False, delta, "test" );
      break;
   case 0xA9: /* TEST Iv, eAX */
      delta = dis_op_imm_A( sz, False, Iop_And8, False, delta, "test" );
      break;

   /* ------------------------ opl Ev, Gv ----------------- */
 
   case 0x02: /* ADD Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_Add8, True, 1, delta, "add" );
      break;
   case 0x03: /* ADD Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_Add8, True, sz, delta, "add" );
      break;

   case 0x0A: /* OR Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_Or8, True, 1, delta, "or" );
      break;
   case 0x0B: /* OR Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_Or8, True, sz, delta, "or" );
      break;

   case 0x12: /* ADC Eb,Gb */
      delta = dis_op2_E_G ( sorb, True, Iop_Add8, True, 1, delta, "adc" );
      break;
   case 0x13: /* ADC Ev,Gv */
      delta = dis_op2_E_G ( sorb, True, Iop_Add8, True, sz, delta, "adc" );
      break;

   case 0x1A: /* SBB Eb,Gb */
      delta = dis_op2_E_G ( sorb, True, Iop_Sub8, True, 1, delta, "sbb" );
      break;
   case 0x1B: /* SBB Ev,Gv */
      delta = dis_op2_E_G ( sorb, True, Iop_Sub8, True, sz, delta, "sbb" );
      break;

   case 0x22: /* AND Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_And8, True, 1, delta, "and" );
      break;
   case 0x23: /* AND Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_And8, True, sz, delta, "and" );
      break;

   case 0x2A: /* SUB Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_Sub8, True, 1, delta, "sub" );
      break;
   case 0x2B: /* SUB Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_Sub8, True, sz, delta, "sub" );
      break;

   case 0x32: /* XOR Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_Xor8, True, 1, delta, "xor" );
      break;
   case 0x33: /* XOR Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_Xor8, True, sz, delta, "xor" );
      break;

   case 0x3A: /* CMP Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_Sub8, False, 1, delta, "cmp" );
      break;
   case 0x3B: /* CMP Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_Sub8, False, sz, delta, "cmp" );
      break;

   case 0x84: /* TEST Eb,Gb */
      delta = dis_op2_E_G ( sorb, False, Iop_And8, False, 1, delta, "test" );
      break;
   case 0x85: /* TEST Ev,Gv */
      delta = dis_op2_E_G ( sorb, False, Iop_And8, False, sz, delta, "test" );
      break;

   /* ------------------------ opl Gv, Ev ----------------- */

   case 0x00: /* ADD Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Add8, True, 1, delta, "add" );
      break;
   case 0x01: /* ADD Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Add8, True, sz, delta, "add" );
      break;

   case 0x08: /* OR Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Or8, True, 1, delta, "or" );
      break;
   case 0x09: /* OR Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Or8, True, sz, delta, "or" );
      break;

   case 0x10: /* ADC Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, True,
                            Iop_Add8, True, 1, delta, "adc" );
      break;
   case 0x11: /* ADC Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, True,
                            Iop_Add8, True, sz, delta, "adc" );
      break;

   case 0x18: /* SBB Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, True,
                            Iop_Sub8, True, 1, delta, "sbb" );
      break;
   case 0x19: /* SBB Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, True,
                            Iop_Sub8, True, sz, delta, "sbb" );
      break;

   case 0x20: /* AND Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_And8, True, 1, delta, "and" );
      break;
   case 0x21: /* AND Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_And8, True, sz, delta, "and" );
      break;

   case 0x28: /* SUB Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Sub8, True, 1, delta, "sub" );
      break;
   case 0x29: /* SUB Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Sub8, True, sz, delta, "sub" );
      break;

   case 0x30: /* XOR Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Xor8, True, 1, delta, "xor" );
      break;
   case 0x31: /* XOR Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Xor8, True, sz, delta, "xor" );
      break;

   case 0x38: /* CMP Gb,Eb */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Sub8, False, 1, delta, "cmp" );
      break;
   case 0x39: /* CMP Gv,Ev */
      delta = dis_op2_G_E ( sorb, pfx_lock, False,
                            Iop_Sub8, False, sz, delta, "cmp" );
      break;

   /* ------------------------ POP ------------------------ */

   case 0x58: /* POP eAX */
   case 0x59: /* POP eCX */
   case 0x5A: /* POP eDX */
   case 0x5B: /* POP eBX */
   case 0x5D: /* POP eBP */
   case 0x5E: /* POP eSI */
   case 0x5F: /* POP eDI */
   case 0x5C: /* POP eSP */
      vassert(sz == 2 || sz == 4);
      t1 = newTemp(szToITy(sz)); t2 = newTemp(Ity_I32);
      assign(t2, getIReg(4, R_ESP));
      assign(t1, loadLE(szToITy(sz),mkexpr(t2)));
      putIReg(4, R_ESP, binop(Iop_Add32, mkexpr(t2), mkU32(sz)));
      putIReg(sz, opc-0x58, mkexpr(t1));
      DIP("pop%c %s\n", nameISize(sz), nameIReg(sz,opc-0x58));
      break;

   case 0x9D: /* POPF */
      vassert(sz == 2 || sz == 4);
      t1 = newTemp(Ity_I32); t2 = newTemp(Ity_I32);
      assign(t2, getIReg(4, R_ESP));
      assign(t1, widenUto32(loadLE(szToITy(sz),mkexpr(t2))));
      putIReg(4, R_ESP, binop(Iop_Add32, mkexpr(t2), mkU32(sz)));

      /* Generate IR to set %EFLAGS{O,S,Z,A,C,P,D,ID,AC} from the
	 value in t1. */
      set_EFLAGS_from_value( t1, True/*emit_AC_emwarn*/,
                                 ((Addr32)guest_EIP_bbstart)+delta );

      DIP("popf%c\n", nameISize(sz));
      break;

   case 0x61: /* POPA */
      /* This is almost certainly wrong for sz==2.  So ... */
      if (sz != 4) goto decode_failure;

      /* t5 is the old %ESP value. */
      t5 = newTemp(Ity_I32);
      assign( t5, getIReg(4, R_ESP) );

      /* Reload all the registers, except %esp. */
      putIReg(4,R_EAX, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32(28)) ));
      putIReg(4,R_ECX, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32(24)) ));
      putIReg(4,R_EDX, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32(20)) ));
      putIReg(4,R_EBX, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32(16)) ));
      /* ignore saved %ESP */
      putIReg(4,R_EBP, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32( 8)) ));
      putIReg(4,R_ESI, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32( 4)) ));
      putIReg(4,R_EDI, loadLE(Ity_I32, binop(Iop_Add32,mkexpr(t5),mkU32( 0)) ));

      /* and move %ESP back up */
      putIReg( 4, R_ESP, binop(Iop_Add32, mkexpr(t5), mkU32(8*4)) );

      DIP("popa%c\n", nameISize(sz));
      break;

   case 0x8F: /* POPL/POPW m32 */
     { Int    len;
       UChar  rm = getIByte(delta);

       /* make sure this instruction is correct POP */
       if (epartIsReg(rm) || gregOfRM(rm) != 0)
          goto decode_failure;
       /* and has correct size */
       if (sz != 4 && sz != 2)
          goto decode_failure;
       ty = szToITy(sz);

       t1 = newTemp(Ity_I32); /* stack address */
       t3 = newTemp(ty); /* data */
       /* set t1 to ESP: t1 = ESP */
       assign( t1, getIReg(4, R_ESP) );
       /* load M[ESP] to virtual register t3: t3 = M[t1] */
       assign( t3, loadLE(ty, mkexpr(t1)) );
       
       /* increase ESP; must be done before the STORE.  Intel manual says:
            If the ESP register is used as a base register for addressing
            a destination operand in memory, the POP instruction computes
            the effective address of the operand after it increments the
            ESP register.
       */
       putIReg(4, R_ESP, binop(Iop_Add32, mkexpr(t1), mkU32(sz)) );

       /* resolve MODR/M */
       addr = disAMode ( &len, sorb, delta, dis_buf);
       storeLE( mkexpr(addr), mkexpr(t3) );

       DIP("pop%c %s\n", sz==2 ? 'w' : 'l', dis_buf);

       delta += len;
       break;
     }

   case 0x1F: /* POP %DS */
      dis_pop_segreg( R_DS, sz ); break;
   case 0x07: /* POP %ES */
      dis_pop_segreg( R_ES, sz ); break;
   case 0x17: /* POP %SS */
      dis_pop_segreg( R_SS, sz ); break;

   /* ------------------------ PUSH ----------------------- */

   case 0x50: /* PUSH eAX */
   case 0x51: /* PUSH eCX */
   case 0x52: /* PUSH eDX */
   case 0x53: /* PUSH eBX */
   case 0x55: /* PUSH eBP */
   case 0x56: /* PUSH eSI */
   case 0x57: /* PUSH eDI */
   case 0x54: /* PUSH eSP */
      /* This is the Right Way, in that the value to be pushed is
         established before %esp is changed, so that pushl %esp
         correctly pushes the old value. */
      vassert(sz == 2 || sz == 4);
      ty = sz==2 ? Ity_I16 : Ity_I32;
      t1 = newTemp(ty); t2 = newTemp(Ity_I32);
      assign(t1, getIReg(sz, opc-0x50));
      assign(t2, binop(Iop_Sub32, getIReg(4, R_ESP), mkU32(sz)));
      putIReg(4, R_ESP, mkexpr(t2) );
      storeLE(mkexpr(t2),mkexpr(t1));
      DIP("push%c %s\n", nameISize(sz), nameIReg(sz,opc-0x50));
      break;


   case 0x68: /* PUSH Iv */
      d32 = getUDisp(sz,delta); delta += sz;
      goto do_push_I;
   case 0x6A: /* PUSH Ib, sign-extended to sz */
      d32 = getSDisp8(delta); delta += 1;
      goto do_push_I;
   do_push_I:
      ty = szToITy(sz);
      t1 = newTemp(Ity_I32); t2 = newTemp(ty);
      assign( t1, binop(Iop_Sub32,getIReg(4,R_ESP),mkU32(sz)) );
      putIReg(4, R_ESP, mkexpr(t1) );
      /* stop mkU16 asserting if d32 is a negative 16-bit number
         (bug #132813) */
      if (ty == Ity_I16)
         d32 &= 0xFFFF;
      storeLE( mkexpr(t1), mkU(ty,d32) );
      DIP("push%c $0x%x\n", nameISize(sz), d32);
      break;

   case 0x9C: /* PUSHF */ {
      vassert(sz == 2 || sz == 4);

      t1 = newTemp(Ity_I32);
      assign( t1, binop(Iop_Sub32,getIReg(4,R_ESP),mkU32(sz)) );
      putIReg(4, R_ESP, mkexpr(t1) );

      /* Calculate OSZACP, and patch in fixed fields as per
         Intel docs. 
         - bit 1 is always 1
         - bit 9 is Interrupt Enable (should always be 1 in user mode?)
      */
      t2 = newTemp(Ity_I32);
      assign( t2, binop(Iop_Or32, 
                        mk_x86g_calculate_eflags_all(), 
                        mkU32( (1<<1)|(1<<9) ) ));

      /* Patch in the D flag.  This can simply be a copy of bit 10 of
         baseBlock[OFFB_DFLAG]. */
      t3 = newTemp(Ity_I32);
      assign( t3, binop(Iop_Or32,
                        mkexpr(t2),
                        binop(Iop_And32,
                              IRExpr_Get(OFFB_DFLAG,Ity_I32),
                              mkU32(1<<10))) 
            );

      /* And patch in the ID flag. */
      t4 = newTemp(Ity_I32);
      assign( t4, binop(Iop_Or32,
                        mkexpr(t3),
                        binop(Iop_And32,
                              binop(Iop_Shl32, IRExpr_Get(OFFB_IDFLAG,Ity_I32), 
                                               mkU8(21)),
                              mkU32(1<<21)))
            );

      /* And patch in the AC flag. */
      t5 = newTemp(Ity_I32);
      assign( t5, binop(Iop_Or32,
                        mkexpr(t4),
                        binop(Iop_And32,
                              binop(Iop_Shl32, IRExpr_Get(OFFB_ACFLAG,Ity_I32), 
                                               mkU8(18)),
                              mkU32(1<<18)))
            );

      /* if sz==2, the stored value needs to be narrowed. */
      if (sz == 2)
        storeLE( mkexpr(t1), unop(Iop_32to16,mkexpr(t5)) );
      else 
        storeLE( mkexpr(t1), mkexpr(t5) );

      DIP("pushf%c\n", nameISize(sz));
      break;
   }

   case 0x60: /* PUSHA */
      /* This is almost certainly wrong for sz==2.  So ... */
      if (sz != 4) goto decode_failure;

      /* This is the Right Way, in that the value to be pushed is
         established before %esp is changed, so that pusha
         correctly pushes the old %esp value.  New value of %esp is
         pushed at start. */
      /* t0 is the %ESP value we're going to push. */
      t0 = newTemp(Ity_I32);
      assign( t0, getIReg(4, R_ESP) );

      /* t5 will be the new %ESP value. */
      t5 = newTemp(Ity_I32);
      assign( t5, binop(Iop_Sub32, mkexpr(t0), mkU32(8*4)) );

      /* Update guest state before prodding memory. */
      putIReg(4, R_ESP, mkexpr(t5));

      /* Dump all the registers. */
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32(28)), getIReg(4,R_EAX) );
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32(24)), getIReg(4,R_ECX) );
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32(20)), getIReg(4,R_EDX) );
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32(16)), getIReg(4,R_EBX) );
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32(12)), mkexpr(t0) /*esp*/);
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32( 8)), getIReg(4,R_EBP) );
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32( 4)), getIReg(4,R_ESI) );
      storeLE( binop(Iop_Add32,mkexpr(t5),mkU32( 0)), getIReg(4,R_EDI) );

      DIP("pusha%c\n", nameISize(sz));
      break;

   case 0x0E: /* PUSH %CS */
      dis_push_segreg( R_CS, sz ); break;
   case 0x1E: /* PUSH %DS */
      dis_push_segreg( R_DS, sz ); break;
   case 0x06: /* PUSH %ES */
      dis_push_segreg( R_ES, sz ); break;
   case 0x16: /* PUSH %SS */
      dis_push_segreg( R_SS, sz ); break;

   /* ------------------------ SCAS et al ----------------- */

   case 0xA4: /* MOVS, no REP prefix */
   case 0xA5: 
      if (sorb != 0)
         goto decode_failure; /* else dis_string_op asserts */
      dis_string_op( dis_MOVS, ( opc == 0xA4 ? 1 : sz ), "movs", sorb );
      break;

  case 0xA6: /* CMPSb, no REP prefix */
  case 0xA7:
      if (sorb != 0)
         goto decode_failure; /* else dis_string_op asserts */
      dis_string_op( dis_CMPS, ( opc == 0xA6 ? 1 : sz ), "cmps", sorb );
      break;

   case 0xAA: /* STOS, no REP prefix */
   case 0xAB:
      if (sorb != 0)
         goto decode_failure; /* else dis_string_op asserts */
      dis_string_op( dis_STOS, ( opc == 0xAA ? 1 : sz ), "stos", sorb );
      break;

   case 0xAC: /* LODS, no REP prefix */
   case 0xAD:
      if (sorb != 0)
         goto decode_failure; /* else dis_string_op asserts */
      dis_string_op( dis_LODS, ( opc == 0xAC ? 1 : sz ), "lods", sorb );
      break;

   case 0xAE: /* SCAS, no REP prefix */
   case 0xAF:
      if (sorb != 0) 
         goto decode_failure; /* else dis_string_op asserts */
      dis_string_op( dis_SCAS, ( opc == 0xAE ? 1 : sz ), "scas", sorb );
      break;


   case 0xFC: /* CLD */
      stmt( IRStmt_Put( OFFB_DFLAG, mkU32(1)) );
      DIP("cld\n");
      break;

   case 0xFD: /* STD */
      stmt( IRStmt_Put( OFFB_DFLAG, mkU32(0xFFFFFFFF)) );
      DIP("std\n");
      break;

   case 0xF8: /* CLC */
   case 0xF9: /* STC */
   case 0xF5: /* CMC */
      t0 = newTemp(Ity_I32);
      t1 = newTemp(Ity_I32);
      assign( t0, mk_x86g_calculate_eflags_all() );
      switch (opc) {
         case 0xF8: 
            assign( t1, binop(Iop_And32, mkexpr(t0), 
                                         mkU32(~X86G_CC_MASK_C)));
            DIP("clc\n");
            break;
         case 0xF9: 
            assign( t1, binop(Iop_Or32, mkexpr(t0), 
                                        mkU32(X86G_CC_MASK_C)));
            DIP("stc\n");
            break;
         case 0xF5: 
            assign( t1, binop(Iop_Xor32, mkexpr(t0), 
                                         mkU32(X86G_CC_MASK_C)));
            DIP("cmc\n");
            break;
         default: 
            vpanic("disInstr(x86)(clc/stc/cmc)");
      }
      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
      stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(t1) ));
      /* Set NDEP even though it isn't used.  This makes redundant-PUT
         elimination of previous stores to this field work better. */
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));
      break;

   case 0xD6: /* SALC */
      t0 = newTemp(Ity_I32);
      t1 = newTemp(Ity_I32);
      assign( t0,  binop(Iop_And32,
                         mk_x86g_calculate_eflags_c(),
                         mkU32(1)) );
      assign( t1, binop(Iop_Sar32, 
                        binop(Iop_Shl32, mkexpr(t0), mkU8(31)), 
                        mkU8(31)) );
      putIReg(1, R_EAX, unop(Iop_32to8, mkexpr(t1)) );
      DIP("salc\n");
      break;

   /* REPNE prefix insn */
   case 0xF2: { 
      Addr32 eip_orig = guest_EIP_bbstart + delta_start;
      if (sorb != 0) goto decode_failure;
      abyte = getIByte(delta); delta++;

      if (abyte == 0x66) { sz = 2; abyte = getIByte(delta); delta++; }

      switch (abyte) {
      /* According to the Intel manual, "repne movs" should never occur, but
       * in practice it has happened, so allow for it here... */
      case 0xA4: sz = 1;   /* REPNE MOVS<sz> */
      case 0xA5: 
         dis_REP_op ( &dres, X86CondNZ, dis_MOVS, sz, eip_orig,
                             guest_EIP_bbstart+delta, "repne movs" );
         break;

      case 0xA6: sz = 1;   /* REPNE CMP<sz> */
      case 0xA7:
         dis_REP_op ( &dres, X86CondNZ, dis_CMPS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "repne cmps" );
         break;

      case 0xAA: sz = 1;   /* REPNE STOS<sz> */
      case 0xAB:
         dis_REP_op ( &dres, X86CondNZ, dis_STOS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "repne stos" );
         break;

      case 0xAE: sz = 1;   /* REPNE SCAS<sz> */
      case 0xAF:
         dis_REP_op ( &dres, X86CondNZ, dis_SCAS, sz, eip_orig,
                             guest_EIP_bbstart+delta, "repne scas" );
         break;

      default:
         goto decode_failure;
      }
      break;
   }

   /* REP/REPE prefix insn (for SCAS and CMPS, 0xF3 means REPE,
      for the rest, it means REP) */
   case 0xF3: { 
      Addr32 eip_orig = guest_EIP_bbstart + delta_start;
      abyte = getIByte(delta); delta++;

      if (abyte == 0x66) { sz = 2; abyte = getIByte(delta); delta++; }

      if (sorb != 0 && abyte != 0x0F) goto decode_failure;

      switch (abyte) {
      case 0x0F:
         switch (getIByte(delta)) {
         /* On older CPUs, TZCNT behaves the same as BSF.  */
         case 0xBC: /* REP BSF Gv,Ev */
            delta = dis_bs_E_G ( sorb, sz, delta + 1, True );
            break;
         /* On older CPUs, LZCNT behaves the same as BSR.  */
         case 0xBD: /* REP BSR Gv,Ev */
            delta = dis_bs_E_G ( sorb, sz, delta + 1, False );
            break;
         default:
            goto decode_failure;
         }
         break;

      case 0xA4: sz = 1;   /* REP MOVS<sz> */
      case 0xA5:
         dis_REP_op ( &dres, X86CondAlways, dis_MOVS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "rep movs" );
         break;

      case 0xA6: sz = 1;   /* REPE CMP<sz> */
      case 0xA7:
         dis_REP_op ( &dres, X86CondZ, dis_CMPS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "repe cmps" );
         break;

      case 0xAA: sz = 1;   /* REP STOS<sz> */
      case 0xAB:
         dis_REP_op ( &dres, X86CondAlways, dis_STOS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "rep stos" );
         break;

      case 0xAC: sz = 1;   /* REP LODS<sz> */
      case 0xAD:
         dis_REP_op ( &dres, X86CondAlways, dis_LODS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "rep lods" );
         break;

      case 0xAE: sz = 1;   /* REPE SCAS<sz> */
      case 0xAF: 
         dis_REP_op ( &dres, X86CondZ, dis_SCAS, sz, eip_orig, 
                             guest_EIP_bbstart+delta, "repe scas" );
         break;
      
      case 0x90:           /* REP NOP (PAUSE) */
         /* a hint to the P4 re spin-wait loop */
         DIP("rep nop (P4 pause)\n");
         /* "observe" the hint.  The Vex client needs to be careful not
            to cause very long delays as a result, though. */
         jmp_lit(&dres, Ijk_Yield, ((Addr32)guest_EIP_bbstart)+delta);
         vassert(dres.whatNext == Dis_StopHere);
         break;

      case 0xC3:           /* REP RET -- same as normal ret? */
         dis_ret(&dres, 0);
         DIP("rep ret\n");
         break;

      default:
         goto decode_failure;
      }
      break;
   }

   /* ------------------------ XCHG ----------------------- */

   /* XCHG reg,mem automatically asserts LOCK# even without a LOCK
      prefix; hence it must be translated with an IRCAS (at least, the
      memory variant). */
   case 0x86: /* XCHG Gb,Eb */
      sz = 1;
      /* Fall through ... */
   case 0x87: /* XCHG Gv,Ev */
      modrm = getIByte(delta);
      ty = szToITy(sz);
      t1 = newTemp(ty); t2 = newTemp(ty);
      if (epartIsReg(modrm)) {
         assign(t1, getIReg(sz, eregOfRM(modrm)));
         assign(t2, getIReg(sz, gregOfRM(modrm)));
         putIReg(sz, gregOfRM(modrm), mkexpr(t1));
         putIReg(sz, eregOfRM(modrm), mkexpr(t2));
         delta++;
         DIP("xchg%c %s, %s\n", 
             nameISize(sz), nameIReg(sz,gregOfRM(modrm)), 
                            nameIReg(sz,eregOfRM(modrm)));
      } else {
         *expect_CAS = True;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         assign( t1, loadLE(ty,mkexpr(addr)) );
         assign( t2, getIReg(sz,gregOfRM(modrm)) );
         casLE( mkexpr(addr),
                mkexpr(t1), mkexpr(t2), guest_EIP_curr_instr );
         putIReg( sz, gregOfRM(modrm), mkexpr(t1) );
         delta += alen;
         DIP("xchg%c %s, %s\n", nameISize(sz), 
                                nameIReg(sz,gregOfRM(modrm)), dis_buf);
      }
      break;

   case 0x90: /* XCHG eAX,eAX */
      DIP("nop\n");
      break;
   case 0x91: /* XCHG eAX,eCX */
   case 0x92: /* XCHG eAX,eDX */
   case 0x93: /* XCHG eAX,eBX */
   case 0x94: /* XCHG eAX,eSP */
   case 0x95: /* XCHG eAX,eBP */
   case 0x96: /* XCHG eAX,eSI */
   case 0x97: /* XCHG eAX,eDI */
      codegen_xchg_eAX_Reg ( sz, opc - 0x90 );
      break;

   /* ------------------------ XLAT ----------------------- */

   case 0xD7: /* XLAT */
      if (sz != 4) goto decode_failure; /* sz == 2 is also allowed (0x66) */
      putIReg( 
         1, 
         R_EAX/*AL*/,
         loadLE(Ity_I8, 
                handleSegOverride( 
                   sorb, 
                   binop(Iop_Add32, 
                         getIReg(4, R_EBX), 
                         unop(Iop_8Uto32, getIReg(1, R_EAX/*AL*/))))));

      DIP("xlat%c [ebx]\n", nameISize(sz));
      break;

   /* ------------------------ IN / OUT ----------------------- */

   case 0xE4: /* IN imm8, AL */
      sz = 1; 
      t1 = newTemp(Ity_I32);
      abyte = getIByte(delta); delta++;
      assign(t1, mkU32( abyte & 0xFF ));
      DIP("in%c $%d,%s\n", nameISize(sz), (Int)abyte, nameIReg(sz,R_EAX));
      goto do_IN;
   case 0xE5: /* IN imm8, eAX */
      vassert(sz == 2 || sz == 4);
      t1 = newTemp(Ity_I32);
      abyte = getIByte(delta); delta++;
      assign(t1, mkU32( abyte & 0xFF ));
      DIP("in%c $%d,%s\n", nameISize(sz), (Int)abyte, nameIReg(sz,R_EAX));
      goto do_IN;
   case 0xEC: /* IN %DX, AL */
      sz = 1; 
      t1 = newTemp(Ity_I32);
      assign(t1, unop(Iop_16Uto32, getIReg(2, R_EDX)));
      DIP("in%c %s,%s\n", nameISize(sz), nameIReg(2,R_EDX), 
                                         nameIReg(sz,R_EAX));
      goto do_IN;
   case 0xED: /* IN %DX, eAX */
      vassert(sz == 2 || sz == 4);
      t1 = newTemp(Ity_I32);
      assign(t1, unop(Iop_16Uto32, getIReg(2, R_EDX)));
      DIP("in%c %s,%s\n", nameISize(sz), nameIReg(2,R_EDX), 
                                         nameIReg(sz,R_EAX));
      goto do_IN;
   do_IN: {
      /* At this point, sz indicates the width, and t1 is a 32-bit
         value giving port number. */
      IRDirty* d;
      vassert(sz == 1 || sz == 2 || sz == 4);
      ty = szToITy(sz);
      t2 = newTemp(Ity_I32);
      d = unsafeIRDirty_1_N( 
             t2,
             0/*regparms*/, 
             "x86g_dirtyhelper_IN", 
             &x86g_dirtyhelper_IN,
             mkIRExprVec_2( mkexpr(t1), mkU32(sz) )
          );
      /* do the call, dumping the result in t2. */
      stmt( IRStmt_Dirty(d) );
      putIReg(sz, R_EAX, narrowTo( ty, mkexpr(t2) ) );
      break;
   }

   case 0xE6: /* OUT AL, imm8 */
      sz = 1;
      t1 = newTemp(Ity_I32);
      abyte = getIByte(delta); delta++;
      assign( t1, mkU32( abyte & 0xFF ) );
      DIP("out%c %s,$%d\n", nameISize(sz), nameIReg(sz,R_EAX), (Int)abyte);
      goto do_OUT;
   case 0xE7: /* OUT eAX, imm8 */
      vassert(sz == 2 || sz == 4);
      t1 = newTemp(Ity_I32);
      abyte = getIByte(delta); delta++;
      assign( t1, mkU32( abyte & 0xFF ) );
      DIP("out%c %s,$%d\n", nameISize(sz), nameIReg(sz,R_EAX), (Int)abyte);
      goto do_OUT;
   case 0xEE: /* OUT AL, %DX */
      sz = 1;
      t1 = newTemp(Ity_I32);
      assign( t1, unop(Iop_16Uto32, getIReg(2, R_EDX)) );
      DIP("out%c %s,%s\n", nameISize(sz), nameIReg(sz,R_EAX),
                                          nameIReg(2,R_EDX));
      goto do_OUT;
   case 0xEF: /* OUT eAX, %DX */
      vassert(sz == 2 || sz == 4);
      t1 = newTemp(Ity_I32);
      assign( t1, unop(Iop_16Uto32, getIReg(2, R_EDX)) );
      DIP("out%c %s,%s\n", nameISize(sz), nameIReg(sz,R_EAX),
                                          nameIReg(2,R_EDX));
      goto do_OUT;
   do_OUT: {
      /* At this point, sz indicates the width, and t1 is a 32-bit
         value giving port number. */
      IRDirty* d;
      vassert(sz == 1 || sz == 2 || sz == 4);
      ty = szToITy(sz);
      d = unsafeIRDirty_0_N( 
             0/*regparms*/, 
             "x86g_dirtyhelper_OUT", 
             &x86g_dirtyhelper_OUT,
             mkIRExprVec_3( mkexpr(t1),
                            widenUto32( getIReg(sz, R_EAX) ), 
                            mkU32(sz) )
          );
      stmt( IRStmt_Dirty(d) );
      break;
   }

   /* ------------------------ (Grp1 extensions) ---------- */

   case 0x82: /* Grp1 Ib,Eb too.  Apparently this is the same as 
                 case 0x80, but only in 32-bit mode. */
      /* fallthru */
   case 0x80: /* Grp1 Ib,Eb */
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      sz    = 1;
      d_sz  = 1;
      d32   = getUChar(delta + am_sz);
      delta = dis_Grp1 ( sorb, pfx_lock, delta, modrm, am_sz, d_sz, sz, d32 );
      break;

   case 0x81: /* Grp1 Iv,Ev */
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      d_sz  = sz;
      d32   = getUDisp(d_sz, delta + am_sz);
      delta = dis_Grp1 ( sorb, pfx_lock, delta, modrm, am_sz, d_sz, sz, d32 );
      break;

   case 0x83: /* Grp1 Ib,Ev */
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 1;
      d32   = getSDisp8(delta + am_sz);
      delta = dis_Grp1 ( sorb, pfx_lock, delta, modrm, am_sz, d_sz, sz, d32 );
      break;

   /* ------------------------ (Grp2 extensions) ---------- */

   case 0xC0: { /* Grp2 Ib,Eb */
      Bool decode_OK = True;
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 1;
      d32   = getUChar(delta + am_sz);
      sz    = 1;
      delta = dis_Grp2 ( sorb, delta, modrm, am_sz, d_sz, sz, 
                         mkU8(d32 & 0xFF), NULL, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }
   case 0xC1: { /* Grp2 Ib,Ev */
      Bool decode_OK = True;
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 1;
      d32   = getUChar(delta + am_sz);
      delta = dis_Grp2 ( sorb, delta, modrm, am_sz, d_sz, sz, 
                         mkU8(d32 & 0xFF), NULL, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }
   case 0xD0: { /* Grp2 1,Eb */
      Bool decode_OK = True;
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 0;
      d32   = 1;
      sz    = 1;
      delta = dis_Grp2 ( sorb, delta, modrm, am_sz, d_sz, sz, 
                         mkU8(d32), NULL, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }
   case 0xD1: { /* Grp2 1,Ev */
      Bool decode_OK = True;
      modrm = getUChar(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 0;
      d32   = 1;
      delta = dis_Grp2 ( sorb, delta, modrm, am_sz, d_sz, sz, 
                         mkU8(d32), NULL, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }
   case 0xD2: { /* Grp2 CL,Eb */
      Bool decode_OK = True;
      modrm = getUChar(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 0;
      sz    = 1;
      delta = dis_Grp2 ( sorb, delta, modrm, am_sz, d_sz, sz, 
                         getIReg(1,R_ECX), "%cl", &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }
   case 0xD3: { /* Grp2 CL,Ev */
      Bool decode_OK = True;
      modrm = getIByte(delta);
      am_sz = lengthAMode(delta);
      d_sz  = 0;
      delta = dis_Grp2 ( sorb, delta, modrm, am_sz, d_sz, sz, 
                         getIReg(1,R_ECX), "%cl", &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }

   /* ------------------------ (Grp3 extensions) ---------- */

   case 0xF6: { /* Grp3 Eb */
      Bool decode_OK = True;
      delta = dis_Grp3 ( sorb, pfx_lock, 1, delta, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }
   case 0xF7: { /* Grp3 Ev */
      Bool decode_OK = True;
      delta = dis_Grp3 ( sorb, pfx_lock, sz, delta, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }

   /* ------------------------ (Grp4 extensions) ---------- */

   case 0xFE: { /* Grp4 Eb */
      Bool decode_OK = True;
      delta = dis_Grp4 ( sorb, pfx_lock, delta, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }

   /* ------------------------ (Grp5 extensions) ---------- */

   case 0xFF: { /* Grp5 Ev */
      Bool decode_OK = True;
      delta = dis_Grp5 ( sorb, pfx_lock, sz, delta, &dres, &decode_OK );
      if (!decode_OK)
         goto decode_failure;
      break;
   }

   /* ------------------------ Escapes to 2-byte opcodes -- */

   case 0x0F: {
      opc = getIByte(delta); delta++;
      switch (opc) {

      /* =-=-=-=-=-=-=-=-=- Grp8 =-=-=-=-=-=-=-=-=-=-=-= */

      case 0xBA: { /* Grp8 Ib,Ev */
         Bool decode_OK = False;
         modrm = getUChar(delta);
         am_sz = lengthAMode(delta);
         d32   = getSDisp8(delta + am_sz);
         delta = dis_Grp8_Imm ( sorb, pfx_lock, delta, modrm, 
                                am_sz, sz, d32, &decode_OK );
         if (!decode_OK)
            goto decode_failure;
         break;
      }

      /* =-=-=-=-=-=-=-=-=- BSF/BSR -=-=-=-=-=-=-=-=-=-= */

      case 0xBC: /* BSF Gv,Ev */
         delta = dis_bs_E_G ( sorb, sz, delta, True );
         break;
      case 0xBD: /* BSR Gv,Ev */
         delta = dis_bs_E_G ( sorb, sz, delta, False );
         break;

      /* =-=-=-=-=-=-=-=-=- BSWAP -=-=-=-=-=-=-=-=-=-=-= */

      case 0xC8: /* BSWAP %eax */
      case 0xC9:
      case 0xCA:
      case 0xCB:
      case 0xCC:
      case 0xCD:
      case 0xCE:
      case 0xCF: /* BSWAP %edi */
         /* AFAICS from the Intel docs, this only exists at size 4. */
         if (sz != 4) goto decode_failure;
         
         t1 = newTemp(Ity_I32);
         assign( t1, getIReg(4, opc-0xC8) );
         t2 = math_BSWAP(t1, Ity_I32);

         putIReg(4, opc-0xC8, mkexpr(t2));
         DIP("bswapl %s\n", nameIReg(4, opc-0xC8));
         break;

      /* =-=-=-=-=-=-=-=-=- BT/BTS/BTR/BTC =-=-=-=-=-=-= */

      case 0xA3: /* BT Gv,Ev */
         delta = dis_bt_G_E ( vbi, sorb, pfx_lock, sz, delta, BtOpNone );
         break;
      case 0xB3: /* BTR Gv,Ev */
         delta = dis_bt_G_E ( vbi, sorb, pfx_lock, sz, delta, BtOpReset );
         break;
      case 0xAB: /* BTS Gv,Ev */
         delta = dis_bt_G_E ( vbi, sorb, pfx_lock, sz, delta, BtOpSet );
         break;
      case 0xBB: /* BTC Gv,Ev */
         delta = dis_bt_G_E ( vbi, sorb, pfx_lock, sz, delta, BtOpComp );
         break;

      /* =-=-=-=-=-=-=-=-=- CMOV =-=-=-=-=-=-=-=-=-=-=-= */
 
      case 0x40:
      case 0x41:
      case 0x42: /* CMOVBb/CMOVNAEb (cmov below) */
      case 0x43: /* CMOVNBb/CMOVAEb (cmov not below) */
      case 0x44: /* CMOVZb/CMOVEb (cmov zero) */
      case 0x45: /* CMOVNZb/CMOVNEb (cmov not zero) */
      case 0x46: /* CMOVBEb/CMOVNAb (cmov below or equal) */
      case 0x47: /* CMOVNBEb/CMOVAb (cmov not below or equal) */
      case 0x48: /* CMOVSb (cmov negative) */
      case 0x49: /* CMOVSb (cmov not negative) */
      case 0x4A: /* CMOVP (cmov parity even) */
      case 0x4B: /* CMOVNP (cmov parity odd) */
      case 0x4C: /* CMOVLb/CMOVNGEb (cmov less) */
      case 0x4D: /* CMOVGEb/CMOVNLb (cmov greater or equal) */
      case 0x4E: /* CMOVLEb/CMOVNGb (cmov less or equal) */
      case 0x4F: /* CMOVGb/CMOVNLEb (cmov greater) */
         delta = dis_cmov_E_G(sorb, sz, (X86Condcode)(opc - 0x40), delta);
         break;

      /* =-=-=-=-=-=-=-=-=- CMPXCHG -=-=-=-=-=-=-=-=-=-= */

      case 0xB0: /* CMPXCHG Gb,Eb */
         delta = dis_cmpxchg_G_E ( sorb, pfx_lock, 1, delta );
         break;
      case 0xB1: /* CMPXCHG Gv,Ev */
         delta = dis_cmpxchg_G_E ( sorb, pfx_lock, sz, delta );
         break;

      case 0xC7: { /* CMPXCHG8B Gv (0F C7 /1) */
         IRTemp expdHi    = newTemp(Ity_I32);
         IRTemp expdLo    = newTemp(Ity_I32);
         IRTemp dataHi    = newTemp(Ity_I32);
         IRTemp dataLo    = newTemp(Ity_I32);
         IRTemp oldHi     = newTemp(Ity_I32);
         IRTemp oldLo     = newTemp(Ity_I32);
         IRTemp flags_old = newTemp(Ity_I32);
         IRTemp flags_new = newTemp(Ity_I32);
         IRTemp success   = newTemp(Ity_I1);

         /* Translate this using a DCAS, even if there is no LOCK
            prefix.  Life is too short to bother with generating two
            different translations for the with/without-LOCK-prefix
            cases. */
         *expect_CAS = True;

	 /* Decode, and generate address. */
         if (sz != 4) goto decode_failure;
         modrm = getIByte(delta);
         if (epartIsReg(modrm)) goto decode_failure;
         if (gregOfRM(modrm) != 1) goto decode_failure;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;

         /* Get the expected and new values. */
         assign( expdHi, getIReg(4,R_EDX) );
         assign( expdLo, getIReg(4,R_EAX) );
         assign( dataHi, getIReg(4,R_ECX) );
         assign( dataLo, getIReg(4,R_EBX) );

         /* Do the DCAS */
         stmt( IRStmt_CAS(
                  mkIRCAS( oldHi, oldLo, 
                           Iend_LE, mkexpr(addr), 
                           mkexpr(expdHi), mkexpr(expdLo),
                           mkexpr(dataHi), mkexpr(dataLo)
               )));

         /* success when oldHi:oldLo == expdHi:expdLo */
         assign( success,
                 binop(Iop_CasCmpEQ32,
                       binop(Iop_Or32,
                             binop(Iop_Xor32, mkexpr(oldHi), mkexpr(expdHi)),
                             binop(Iop_Xor32, mkexpr(oldLo), mkexpr(expdLo))
                       ),
                       mkU32(0)
                 ));

         /* If the DCAS is successful, that is to say oldHi:oldLo ==
            expdHi:expdLo, then put expdHi:expdLo back in EDX:EAX,
            which is where they came from originally.  Both the actual
            contents of these two regs, and any shadow values, are
            unchanged.  If the DCAS fails then we're putting into
            EDX:EAX the value seen in memory. */
         putIReg(4, R_EDX,
                    IRExpr_ITE( mkexpr(success),
                                mkexpr(expdHi), mkexpr(oldHi)
                ));
         putIReg(4, R_EAX,
                    IRExpr_ITE( mkexpr(success),
                                mkexpr(expdLo), mkexpr(oldLo)
                ));

         /* Copy the success bit into the Z flag and leave the others
            unchanged */
         assign( flags_old, widenUto32(mk_x86g_calculate_eflags_all()));
         assign( 
            flags_new,
            binop(Iop_Or32,
                  binop(Iop_And32, mkexpr(flags_old), 
                                   mkU32(~X86G_CC_MASK_Z)),
                  binop(Iop_Shl32, 
                        binop(Iop_And32, 
                              unop(Iop_1Uto32, mkexpr(success)), mkU32(1)), 
                        mkU8(X86G_CC_SHIFT_Z)) ));

         stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(X86G_CC_OP_COPY) ));
         stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(flags_new) ));
         stmt( IRStmt_Put( OFFB_CC_DEP2, mkU32(0) ));
         /* Set NDEP even though it isn't used.  This makes
            redundant-PUT elimination of previous stores to this field
            work better. */
         stmt( IRStmt_Put( OFFB_CC_NDEP, mkU32(0) ));

         /* Sheesh.  Aren't you glad it was me and not you that had to
	    write and validate all this grunge? */

	 DIP("cmpxchg8b %s\n", dis_buf);
	 break;
      }

      /* =-=-=-=-=-=-=-=-=- CPUID -=-=-=-=-=-=-=-=-=-=-= */

      case 0xA2: { /* CPUID */
         /* Uses dirty helper: 
               void dirtyhelper_CPUID_sse[012] ( VexGuestX86State* )
            declared to mod eax, wr ebx, ecx, edx
         */
         IRDirty* d     = NULL;
         void*    fAddr = NULL;
         const HChar* fName = NULL;
         if (archinfo->hwcaps & VEX_HWCAPS_X86_SSE2) {
            fName = "x86g_dirtyhelper_CPUID_sse2";
            fAddr = &x86g_dirtyhelper_CPUID_sse2; 
         } 
         else
         if (archinfo->hwcaps & VEX_HWCAPS_X86_SSE1) {
            fName = "x86g_dirtyhelper_CPUID_sse1";
            fAddr = &x86g_dirtyhelper_CPUID_sse1; 
         } 
         else
         if (archinfo->hwcaps & VEX_HWCAPS_X86_MMXEXT) {
            fName = "x86g_dirtyhelper_CPUID_mmxext";
            fAddr = &x86g_dirtyhelper_CPUID_mmxext;
         }
         else
         if (archinfo->hwcaps == 0/*no SSE*/) {
            fName = "x86g_dirtyhelper_CPUID_sse0";
            fAddr = &x86g_dirtyhelper_CPUID_sse0; 
         } else
            vpanic("disInstr(x86)(cpuid)");

         vassert(fName); vassert(fAddr);
         d = unsafeIRDirty_0_N ( 0/*regparms*/, 
                                 fName, fAddr, mkIRExprVec_1(IRExpr_BBPTR()) );
         /* declare guest state effects */
         d->nFxState = 4;
         vex_bzero(&d->fxState, sizeof(d->fxState));
         d->fxState[0].fx     = Ifx_Modify;
         d->fxState[0].offset = OFFB_EAX;
         d->fxState[0].size   = 4;
         d->fxState[1].fx     = Ifx_Write;
         d->fxState[1].offset = OFFB_EBX;
         d->fxState[1].size   = 4;
         d->fxState[2].fx     = Ifx_Modify;
         d->fxState[2].offset = OFFB_ECX;
         d->fxState[2].size   = 4;
         d->fxState[3].fx     = Ifx_Write;
         d->fxState[3].offset = OFFB_EDX;
         d->fxState[3].size   = 4;
         /* execute the dirty call, side-effecting guest state */
         stmt( IRStmt_Dirty(d) );
         /* CPUID is a serialising insn.  So, just in case someone is
            using it as a memory fence ... */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("cpuid\n");
         break;
      }

//--          if (!VG_(cpu_has_feature)(VG_X86_FEAT_CPUID))
//--             goto decode_failure;
//-- 
//--          t1 = newTemp(cb);
//--          t2 = newTemp(cb);
//--          t3 = newTemp(cb);
//--          t4 = newTemp(cb);
//--          uInstr0(cb, CALLM_S, 0);
//-- 
//--          uInstr2(cb, GET,   4, ArchReg, R_EAX, TempReg, t1);
//--          uInstr1(cb, PUSH,  4, TempReg, t1);
//-- 
//--          uInstr2(cb, MOV,   4, Literal, 0, TempReg, t2);
//--          uLiteral(cb, 0);
//--          uInstr1(cb, PUSH,  4, TempReg, t2);
//-- 
//--          uInstr2(cb, MOV,   4, Literal, 0, TempReg, t3);
//--          uLiteral(cb, 0);
//--          uInstr1(cb, PUSH,  4, TempReg, t3);
//-- 
//--          uInstr2(cb, MOV,   4, Literal, 0, TempReg, t4);
//--          uLiteral(cb, 0);
//--          uInstr1(cb, PUSH,  4, TempReg, t4);
//-- 
//--          uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_CPUID));
//--          uFlagsRWU(cb, FlagsEmpty, FlagsEmpty, FlagsEmpty);
//-- 
//--          uInstr1(cb, POP,   4, TempReg, t4);
//--          uInstr2(cb, PUT,   4, TempReg, t4, ArchReg, R_EDX);
//-- 
//--          uInstr1(cb, POP,   4, TempReg, t3);
//--          uInstr2(cb, PUT,   4, TempReg, t3, ArchReg, R_ECX);
//-- 
//--          uInstr1(cb, POP,   4, TempReg, t2);
//--          uInstr2(cb, PUT,   4, TempReg, t2, ArchReg, R_EBX);
//-- 
//--          uInstr1(cb, POP,   4, TempReg, t1);
//--          uInstr2(cb, PUT,   4, TempReg, t1, ArchReg, R_EAX);
//-- 
//--          uInstr0(cb, CALLM_E, 0);
//--          DIP("cpuid\n");
//--          break;
//-- 
      /* =-=-=-=-=-=-=-=-=- MOVZX, MOVSX =-=-=-=-=-=-=-= */

      case 0xB6: /* MOVZXb Eb,Gv */
         if (sz != 2 && sz != 4)
            goto decode_failure;
         delta = dis_movx_E_G ( sorb, delta, 1, sz, False );
         break;

      case 0xB7: /* MOVZXw Ew,Gv */
         if (sz != 4)
            goto decode_failure;
         delta = dis_movx_E_G ( sorb, delta, 2, 4, False );
         break;

      case 0xBE: /* MOVSXb Eb,Gv */
         if (sz != 2 && sz != 4)
            goto decode_failure;
         delta = dis_movx_E_G ( sorb, delta, 1, sz, True );
         break;

      case 0xBF: /* MOVSXw Ew,Gv */
         if (sz != 4 && /* accept movsww, sigh, see #250799 */sz != 2)
            goto decode_failure;
         delta = dis_movx_E_G ( sorb, delta, 2, sz, True );
         break;

//--       /* =-=-=-=-=-=-=-=-=-=-= MOVNTI -=-=-=-=-=-=-=-=-= */
//-- 
//--       case 0xC3: /* MOVNTI Gv,Ev */
//--          vg_assert(sz == 4);
//--          modrm = getUChar(eip);
//--          vg_assert(!epartIsReg(modrm));
//--          t1 = newTemp(cb);
//--          uInstr2(cb, GET, 4, ArchReg, gregOfRM(modrm), TempReg, t1);
//--          pair = disAMode ( cb, sorb, eip, dis_buf );
//--          t2 = LOW24(pair);
//--          eip += HI8(pair);
//--          uInstr2(cb, STORE, 4, TempReg, t1, TempReg, t2);
//--          DIP("movnti %s,%s\n", nameIReg(4,gregOfRM(modrm)), dis_buf);
//--          break;

      /* =-=-=-=-=-=-=-=-=- MUL/IMUL =-=-=-=-=-=-=-=-=-= */

      case 0xAF: /* IMUL Ev, Gv */
         delta = dis_mul_E_G ( sorb, sz, delta );
         break;

      /* =-=-=-=-=-=-=-=-=- NOPs =-=-=-=-=-=-=-=-=-=-=-= */

      case 0x1F:
         modrm = getUChar(delta);
         if (epartIsReg(modrm)) goto decode_failure;
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;
         DIP("nop%c %s\n", nameISize(sz), dis_buf);
         break;

      /* =-=-=-=-=-=-=-=-=- Jcond d32 -=-=-=-=-=-=-=-=-= */
      case 0x80:
      case 0x81:
      case 0x82: /* JBb/JNAEb (jump below) */
      case 0x83: /* JNBb/JAEb (jump not below) */
      case 0x84: /* JZb/JEb (jump zero) */
      case 0x85: /* JNZb/JNEb (jump not zero) */
      case 0x86: /* JBEb/JNAb (jump below or equal) */
      case 0x87: /* JNBEb/JAb (jump not below or equal) */
      case 0x88: /* JSb (jump negative) */
      case 0x89: /* JSb (jump not negative) */
      case 0x8A: /* JP (jump parity even) */
      case 0x8B: /* JNP/JPO (jump parity odd) */
      case 0x8C: /* JLb/JNGEb (jump less) */
      case 0x8D: /* JGEb/JNLb (jump greater or equal) */
      case 0x8E: /* JLEb/JNGb (jump less or equal) */
      case 0x8F: /* JGb/JNLEb (jump greater) */
       { Int    jmpDelta;
         const HChar* comment  = "";
         jmpDelta = (Int)getUDisp32(delta);
         d32 = (((Addr32)guest_EIP_bbstart)+delta+4) + jmpDelta;
         delta += 4;
         if (resteerCisOk
             && vex_control.guest_chase_cond
             && (Addr32)d32 != (Addr32)guest_EIP_bbstart
             && jmpDelta < 0
             && resteerOkFn( callback_opaque, (Addr64)(Addr32)d32) ) {
            /* Speculation: assume this backward branch is taken.  So
               we need to emit a side-exit to the insn following this
               one, on the negation of the condition, and continue at
               the branch target address (d32).  If we wind up back at
               the first instruction of the trace, just stop; it's
               better to let the IR loop unroller handle that case.*/
            stmt( IRStmt_Exit( 
                     mk_x86g_calculate_condition((X86Condcode)
                                                 (1 ^ (opc - 0x80))),
                     Ijk_Boring,
                     IRConst_U32(guest_EIP_bbstart+delta),
                     OFFB_EIP ) );
            dres.whatNext   = Dis_ResteerC;
            dres.continueAt = (Addr64)(Addr32)d32;
            comment = "(assumed taken)";
         }
         else
         if (resteerCisOk
             && vex_control.guest_chase_cond
             && (Addr32)d32 != (Addr32)guest_EIP_bbstart
             && jmpDelta >= 0
             && resteerOkFn( callback_opaque, 
                             (Addr64)(Addr32)(guest_EIP_bbstart+delta)) ) {
            /* Speculation: assume this forward branch is not taken.
               So we need to emit a side-exit to d32 (the dest) and
               continue disassembling at the insn immediately
               following this one. */
            stmt( IRStmt_Exit( 
                     mk_x86g_calculate_condition((X86Condcode)(opc - 0x80)),
                     Ijk_Boring,
                     IRConst_U32(d32),
                     OFFB_EIP ) );
            dres.whatNext   = Dis_ResteerC;
            dres.continueAt = (Addr64)(Addr32)(guest_EIP_bbstart+delta);
            comment = "(assumed not taken)";
         }
         else {
            /* Conservative default translation - end the block at
               this point. */
            jcc_01( &dres, (X86Condcode)(opc - 0x80), 
                    (Addr32)(guest_EIP_bbstart+delta), d32);
            vassert(dres.whatNext == Dis_StopHere);
         }
         DIP("j%s-32 0x%x %s\n", name_X86Condcode(opc - 0x80), d32, comment);
         break;
       }

      /* =-=-=-=-=-=-=-=-=- RDTSC -=-=-=-=-=-=-=-=-=-=-= */
      case 0x31: { /* RDTSC */
         IRTemp   val  = newTemp(Ity_I64);
         IRExpr** args = mkIRExprVec_0();
         IRDirty* d    = unsafeIRDirty_1_N ( 
                            val, 
                            0/*regparms*/, 
                            "x86g_dirtyhelper_RDTSC", 
                            &x86g_dirtyhelper_RDTSC, 
                            args 
                         );
         /* execute the dirty call, dumping the result in val. */
         stmt( IRStmt_Dirty(d) );
         putIReg(4, R_EDX, unop(Iop_64HIto32, mkexpr(val)));
         putIReg(4, R_EAX, unop(Iop_64to32, mkexpr(val)));
         DIP("rdtsc\n");
         break;
      }

      /* =-=-=-=-=-=-=-=-=- PUSH/POP Sreg =-=-=-=-=-=-=-=-=-= */

      case 0xA1: /* POP %FS */
         dis_pop_segreg( R_FS, sz ); break;
      case 0xA9: /* POP %GS */
         dis_pop_segreg( R_GS, sz ); break;

      case 0xA0: /* PUSH %FS */
         dis_push_segreg( R_FS, sz ); break;
      case 0xA8: /* PUSH %GS */
         dis_push_segreg( R_GS, sz ); break;

      /* =-=-=-=-=-=-=-=-=- SETcc Eb =-=-=-=-=-=-=-=-=-= */
      case 0x90:
      case 0x91:
      case 0x92: /* set-Bb/set-NAEb (jump below) */
      case 0x93: /* set-NBb/set-AEb (jump not below) */
      case 0x94: /* set-Zb/set-Eb (jump zero) */
      case 0x95: /* set-NZb/set-NEb (jump not zero) */
      case 0x96: /* set-BEb/set-NAb (jump below or equal) */
      case 0x97: /* set-NBEb/set-Ab (jump not below or equal) */
      case 0x98: /* set-Sb (jump negative) */
      case 0x99: /* set-Sb (jump not negative) */
      case 0x9A: /* set-P (jump parity even) */
      case 0x9B: /* set-NP (jump parity odd) */
      case 0x9C: /* set-Lb/set-NGEb (jump less) */
      case 0x9D: /* set-GEb/set-NLb (jump greater or equal) */
      case 0x9E: /* set-LEb/set-NGb (jump less or equal) */
      case 0x9F: /* set-Gb/set-NLEb (jump greater) */
         t1 = newTemp(Ity_I8);
         assign( t1, unop(Iop_1Uto8,mk_x86g_calculate_condition(opc-0x90)) );
         modrm = getIByte(delta);
         if (epartIsReg(modrm)) {
            delta++;
            putIReg(1, eregOfRM(modrm), mkexpr(t1));
            DIP("set%s %s\n", name_X86Condcode(opc-0x90), 
                              nameIReg(1,eregOfRM(modrm)));
         } else {
           addr = disAMode ( &alen, sorb, delta, dis_buf );
           delta += alen;
           storeLE( mkexpr(addr), mkexpr(t1) );
           DIP("set%s %s\n", name_X86Condcode(opc-0x90), dis_buf);
         }
         break;

      /* =-=-=-=-=-=-=-=-=- SHLD/SHRD -=-=-=-=-=-=-=-=-= */

      case 0xA4: /* SHLDv imm8,Gv,Ev */
         modrm = getIByte(delta);
         d32   = delta + lengthAMode(delta);
         vex_sprintf(dis_buf, "$%d", getIByte(d32));
         delta = dis_SHLRD_Gv_Ev ( 
                  sorb, delta, modrm, sz, 
                  mkU8(getIByte(d32)), True, /* literal */
                  dis_buf, True );
         break;
      case 0xA5: /* SHLDv %cl,Gv,Ev */
         modrm = getIByte(delta);
         delta = dis_SHLRD_Gv_Ev ( 
                    sorb, delta, modrm, sz,
                    getIReg(1,R_ECX), False, /* not literal */
                    "%cl", True );
         break;

      case 0xAC: /* SHRDv imm8,Gv,Ev */
         modrm = getIByte(delta);
         d32   = delta + lengthAMode(delta);
         vex_sprintf(dis_buf, "$%d", getIByte(d32));
         delta = dis_SHLRD_Gv_Ev ( 
                    sorb, delta, modrm, sz, 
                    mkU8(getIByte(d32)), True, /* literal */
                    dis_buf, False );
         break;
      case 0xAD: /* SHRDv %cl,Gv,Ev */
         modrm = getIByte(delta);
         delta = dis_SHLRD_Gv_Ev ( 
                    sorb, delta, modrm, sz, 
                    getIReg(1,R_ECX), False, /* not literal */
                    "%cl", False );
         break;

      /* =-=-=-=-=-=-=-=-=- SYSENTER -=-=-=-=-=-=-=-=-=-= */

      case 0x34:
         /* Simple implementation needing a long explaination.

            sysenter is a kind of syscall entry.  The key thing here
            is that the return address is not known -- that is
            something that is beyond Vex's knowledge.  So this IR
            forces a return to the scheduler, which can do what it
            likes to simulate the systenter, but it MUST set this
            thread's guest_EIP field with the continuation address
            before resuming execution.  If that doesn't happen, the
            thread will jump to address zero, which is probably
            fatal. 
         */

         /* Note where we are, so we can back up the guest to this
            point if the syscall needs to be restarted. */
         stmt( IRStmt_Put( OFFB_IP_AT_SYSCALL,
                           mkU32(guest_EIP_curr_instr) ) );
         jmp_lit(&dres, Ijk_Sys_sysenter, 0/*bogus next EIP value*/);
         vassert(dres.whatNext == Dis_StopHere);
         DIP("sysenter");
         break;

      /* =-=-=-=-=-=-=-=-=- XADD -=-=-=-=-=-=-=-=-=-= */

      case 0xC0: { /* XADD Gb,Eb */
         Bool decodeOK;
         delta = dis_xadd_G_E ( sorb, pfx_lock, 1, delta, &decodeOK );
         if (!decodeOK) goto decode_failure;
         break;
      }
      case 0xC1: { /* XADD Gv,Ev */
         Bool decodeOK;
         delta = dis_xadd_G_E ( sorb, pfx_lock, sz, delta, &decodeOK );
         if (!decodeOK) goto decode_failure;
         break;
      }

      /* =-=-=-=-=-=-=-=-=- MMXery =-=-=-=-=-=-=-=-=-=-= */

      case 0x71: 
      case 0x72: 
      case 0x73: /* PSLLgg/PSRAgg/PSRLgg mmxreg by imm8 */

      case 0x6E: /* MOVD (src)ireg-or-mem, (dst)mmxreg */
      case 0x7E: /* MOVD (src)mmxreg, (dst)ireg-or-mem */
      case 0x7F: /* MOVQ (src)mmxreg, (dst)mmxreg-or-mem */
      case 0x6F: /* MOVQ (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xFC: 
      case 0xFD: 
      case 0xFE: /* PADDgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xEC: 
      case 0xED: /* PADDSgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xDC:
      case 0xDD: /* PADDUSgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xF8: 
      case 0xF9: 
      case 0xFA: /* PSUBgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xE8: 
      case 0xE9: /* PSUBSgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xD8: 
      case 0xD9: /* PSUBUSgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xE5: /* PMULHW (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xD5: /* PMULLW (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xF5: /* PMADDWD (src)mmxreg-or-mem, (dst)mmxreg */

      case 0x74: 
      case 0x75: 
      case 0x76: /* PCMPEQgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0x64: 
      case 0x65: 
      case 0x66: /* PCMPGTgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0x6B: /* PACKSSDW (src)mmxreg-or-mem, (dst)mmxreg */
      case 0x63: /* PACKSSWB (src)mmxreg-or-mem, (dst)mmxreg */
      case 0x67: /* PACKUSWB (src)mmxreg-or-mem, (dst)mmxreg */

      case 0x68: 
      case 0x69: 
      case 0x6A: /* PUNPCKHgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0x60: 
      case 0x61: 
      case 0x62: /* PUNPCKLgg (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xDB: /* PAND (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xDF: /* PANDN (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xEB: /* POR (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xEF: /* PXOR (src)mmxreg-or-mem, (dst)mmxreg */

      case 0xF1: /* PSLLgg (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xF2: 
      case 0xF3: 

      case 0xD1: /* PSRLgg (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xD2: 
      case 0xD3: 

      case 0xE1: /* PSRAgg (src)mmxreg-or-mem, (dst)mmxreg */
      case 0xE2: 
      {
         Int  delta0    = delta-1;
         Bool decode_OK = False;

         /* If sz==2 this is SSE, and we assume sse idec has
            already spotted those cases by now. */
         if (sz != 4)
            goto decode_failure;

         delta = dis_MMX ( &decode_OK, sorb, sz, delta-1 );
         if (!decode_OK) {
            delta = delta0;
            goto decode_failure;
         }
         break;
      }

      case 0x0E: /* FEMMS */
      case 0x77: /* EMMS */
         if (sz != 4)
            goto decode_failure;
         do_EMMS_preamble();
         DIP("{f}emms\n");
         break;

      /* =-=-=-=-=-=-=-=-=- SGDT and SIDT =-=-=-=-=-=-=-=-=-=-= */
      case 0x01: /* 0F 01 /0 -- SGDT */
                 /* 0F 01 /1 -- SIDT */
      {
          /* This is really revolting, but ... since each processor
             (core) only has one IDT and one GDT, just let the guest
             see it (pass-through semantics).  I can't see any way to
             construct a faked-up value, so don't bother to try. */
         modrm = getUChar(delta);
         addr = disAMode ( &alen, sorb, delta, dis_buf );
         delta += alen;
         if (epartIsReg(modrm)) goto decode_failure;
         if (gregOfRM(modrm) != 0 && gregOfRM(modrm) != 1)
            goto decode_failure;
         switch (gregOfRM(modrm)) {
            case 0: DIP("sgdt %s\n", dis_buf); break;
            case 1: DIP("sidt %s\n", dis_buf); break;
            default: vassert(0); /*NOTREACHED*/
         }

         IRDirty* d = unsafeIRDirty_0_N (
                          0/*regparms*/,
                          "x86g_dirtyhelper_SxDT",
                          &x86g_dirtyhelper_SxDT,
                          mkIRExprVec_2( mkexpr(addr),
                                         mkU32(gregOfRM(modrm)) )
                      );
         /* declare we're writing memory */
         d->mFx   = Ifx_Write;
         d->mAddr = mkexpr(addr);
         d->mSize = 6;
         stmt( IRStmt_Dirty(d) );
         break;
      }

      case 0x05: /* AMD's syscall */
         stmt( IRStmt_Put( OFFB_IP_AT_SYSCALL,
              mkU32(guest_EIP_curr_instr) ) );
         jmp_lit(&dres, Ijk_Sys_syscall, ((Addr32)guest_EIP_bbstart)+delta);
         vassert(dres.whatNext == Dis_StopHere);
         DIP("syscall\n");
         break;

      /* =-=-=-=-=-=-=-=-=- unimp2 =-=-=-=-=-=-=-=-=-=-= */

      default:
         goto decode_failure;
   } /* switch (opc) for the 2-byte opcodes */
   goto decode_success;
   } /* case 0x0F: of primary opcode */

   /* ------------------------ ??? ------------------------ */
  
  default:
  decode_failure:
   /* All decode failures end up here. */
   if (sigill_diag) {
      vex_printf("vex x86->IR: unhandled instruction bytes: "
                 "0x%x 0x%x 0x%x 0x%x\n",
                 (Int)getIByte(delta_start+0),
                 (Int)getIByte(delta_start+1),
                 (Int)getIByte(delta_start+2),
                 (Int)getIByte(delta_start+3) );
   }

   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      EIP should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   stmt( IRStmt_Put( OFFB_EIP, mkU32(guest_EIP_curr_instr) ) );
   jmp_lit(&dres, Ijk_NoDecode, guest_EIP_curr_instr);
   vassert(dres.whatNext == Dis_StopHere);
   dres.len = 0;
   /* We also need to say that a CAS is not expected now, regardless
      of what it might have been set to at the start of the function,
      since the IR that we've emitted just above (to synthesis a
      SIGILL) does not involve any CAS, and presumably no other IR has
      been emitted for this (non-decoded) insn. */
   *expect_CAS = False;
   return dres;

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   switch (dres.whatNext) {
      case Dis_Continue:
         stmt( IRStmt_Put( OFFB_EIP, mkU32(guest_EIP_bbstart + delta) ) );
         break;
      case Dis_ResteerU:
      case Dis_ResteerC:
         stmt( IRStmt_Put( OFFB_EIP, mkU32(dres.continueAt) ) );
         break;
      case Dis_StopHere:
         break;
      default:
         vassert(0);
   }

   DIP("\n");
   dres.len = delta - delta_start;
   return dres;
}

#undef DIP
#undef DIS


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_X86 ( IRSB*        irsb_IN,
                         Bool         (*resteerOkFn) ( void*, Addr64 ),
                         Bool         resteerCisOk,
                         void*        callback_opaque,
                         UChar*       guest_code_IN,
                         Long         delta,
                         Addr64       guest_IP,
                         VexArch      guest_arch,
                         VexArchInfo* archinfo,
                         VexAbiInfo*  abiinfo,
                         Bool         host_bigendian_IN,
                         Bool         sigill_diag_IN )
{
   Int       i, x1, x2;
   Bool      expect_CAS, has_CAS;
   DisResult dres;

   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchX86);
   guest_code           = guest_code_IN;
   irsb                 = irsb_IN;
   host_is_bigendian    = host_bigendian_IN;
   guest_EIP_curr_instr = (Addr32)guest_IP;
   guest_EIP_bbstart    = (Addr32)toUInt(guest_IP - delta);

   x1 = irsb_IN->stmts_used;
   expect_CAS = False;
   dres = disInstr_X86_WRK ( &expect_CAS, resteerOkFn,
                             resteerCisOk,
                             callback_opaque,
                             delta, archinfo, abiinfo, sigill_diag_IN );
   x2 = irsb_IN->stmts_used;
   vassert(x2 >= x1);

   /* See comment at the top of disInstr_X86_WRK for meaning of
      expect_CAS.  Here, we (sanity-)check for the presence/absence of
      IRCAS as directed by the returned expect_CAS value. */
   has_CAS = False;
   for (i = x1; i < x2; i++) {
      if (irsb_IN->stmts[i]->tag == Ist_CAS)
         has_CAS = True;
   }

   if (expect_CAS != has_CAS) {
      /* inconsistency detected.  re-disassemble the instruction so as
         to generate a useful error message; then assert. */
      vex_traceflags |= VEX_TRACE_FE;
      dres = disInstr_X86_WRK ( &expect_CAS, resteerOkFn,
                                resteerCisOk,
                                callback_opaque,
                                delta, archinfo, abiinfo, sigill_diag_IN );
      for (i = x1; i < x2; i++) {
         vex_printf("\t\t");
         ppIRStmt(irsb_IN->stmts[i]);
         vex_printf("\n");
      }
      /* Failure of this assertion is serious and denotes a bug in
         disInstr. */
      vpanic("disInstr_X86: inconsistency in LOCK prefix handling");
   }

   return dres;
}


/*--------------------------------------------------------------------*/
/*--- end                                         guest_x86_toIR.c ---*/
/*--------------------------------------------------------------------*/
