
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-ppc32/hdefs.c) is                       ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

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

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"
#include "host-ppc32/hdefs.h"


/* --------- Registers. --------- */

void ppHRegPPC32 ( HReg reg ) 
{
   Int r;
   static HChar* ireg32_names[32] 
      = { "%r0",  "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
          "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
          "%r16", "%r17", "%r18", "%r19", "%r20", "%r21", "%r22", "%r23",
          "%r24", "%r25", "%r26", "%r27", "%r28", "%r29", "%r30", "%r31" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
   case HRcInt32:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%s", ireg32_names[r]);
      return;
   case HRcFlt64:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%%fr%d", r);
      return;
   case HRcVec128:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%%v%d", r);
      return;
   default:
      vpanic("ppHRegPPC32");
   }
}

HReg hregPPC32_GPR0  ( void ) { return mkHReg( 0, HRcInt32, False); }
HReg hregPPC32_GPR1  ( void ) { return mkHReg( 1, HRcInt32, False); }
HReg hregPPC32_GPR2  ( void ) { return mkHReg( 2, HRcInt32, False); }
HReg hregPPC32_GPR3  ( void ) { return mkHReg( 3, HRcInt32, False); }
HReg hregPPC32_GPR4  ( void ) { return mkHReg( 4, HRcInt32, False); }
HReg hregPPC32_GPR5  ( void ) { return mkHReg( 5, HRcInt32, False); }
HReg hregPPC32_GPR6  ( void ) { return mkHReg( 6, HRcInt32, False); }
HReg hregPPC32_GPR7  ( void ) { return mkHReg( 7, HRcInt32, False); }
HReg hregPPC32_GPR8  ( void ) { return mkHReg( 8, HRcInt32, False); }
HReg hregPPC32_GPR9  ( void ) { return mkHReg( 9, HRcInt32, False); }
HReg hregPPC32_GPR10 ( void ) { return mkHReg(10, HRcInt32, False); }
HReg hregPPC32_GPR11 ( void ) { return mkHReg(11, HRcInt32, False); }
HReg hregPPC32_GPR12 ( void ) { return mkHReg(12, HRcInt32, False); }
HReg hregPPC32_GPR13 ( void ) { return mkHReg(13, HRcInt32, False); }
HReg hregPPC32_GPR14 ( void ) { return mkHReg(14, HRcInt32, False); }
HReg hregPPC32_GPR15 ( void ) { return mkHReg(15, HRcInt32, False); }
HReg hregPPC32_GPR16 ( void ) { return mkHReg(16, HRcInt32, False); }
HReg hregPPC32_GPR17 ( void ) { return mkHReg(17, HRcInt32, False); }
HReg hregPPC32_GPR18 ( void ) { return mkHReg(18, HRcInt32, False); }
HReg hregPPC32_GPR19 ( void ) { return mkHReg(19, HRcInt32, False); }
HReg hregPPC32_GPR20 ( void ) { return mkHReg(20, HRcInt32, False); }
HReg hregPPC32_GPR21 ( void ) { return mkHReg(21, HRcInt32, False); }
HReg hregPPC32_GPR22 ( void ) { return mkHReg(22, HRcInt32, False); }
HReg hregPPC32_GPR23 ( void ) { return mkHReg(23, HRcInt32, False); }
HReg hregPPC32_GPR24 ( void ) { return mkHReg(24, HRcInt32, False); }
HReg hregPPC32_GPR25 ( void ) { return mkHReg(25, HRcInt32, False); }
HReg hregPPC32_GPR26 ( void ) { return mkHReg(26, HRcInt32, False); }
HReg hregPPC32_GPR27 ( void ) { return mkHReg(27, HRcInt32, False); }
HReg hregPPC32_GPR28 ( void ) { return mkHReg(28, HRcInt32, False); }
HReg hregPPC32_GPR29 ( void ) { return mkHReg(29, HRcInt32, False); }
HReg hregPPC32_GPR30 ( void ) { return mkHReg(30, HRcInt32, False); }
HReg hregPPC32_GPR31 ( void ) { return mkHReg(31, HRcInt32, False); }

HReg hregPPC32_FPR0  ( void ) { return mkHReg( 0, HRcFlt64, False); }
HReg hregPPC32_FPR1  ( void ) { return mkHReg( 1, HRcFlt64, False); }
HReg hregPPC32_FPR2  ( void ) { return mkHReg( 2, HRcFlt64, False); }
HReg hregPPC32_FPR3  ( void ) { return mkHReg( 3, HRcFlt64, False); }
HReg hregPPC32_FPR4  ( void ) { return mkHReg( 4, HRcFlt64, False); }
HReg hregPPC32_FPR5  ( void ) { return mkHReg( 5, HRcFlt64, False); }
HReg hregPPC32_FPR6  ( void ) { return mkHReg( 6, HRcFlt64, False); }
HReg hregPPC32_FPR7  ( void ) { return mkHReg( 7, HRcFlt64, False); }
HReg hregPPC32_FPR8  ( void ) { return mkHReg( 8, HRcFlt64, False); }
HReg hregPPC32_FPR9  ( void ) { return mkHReg( 9, HRcFlt64, False); }
HReg hregPPC32_FPR10 ( void ) { return mkHReg(10, HRcFlt64, False); }
HReg hregPPC32_FPR11 ( void ) { return mkHReg(11, HRcFlt64, False); }
HReg hregPPC32_FPR12 ( void ) { return mkHReg(12, HRcFlt64, False); }
HReg hregPPC32_FPR13 ( void ) { return mkHReg(13, HRcFlt64, False); }
HReg hregPPC32_FPR14 ( void ) { return mkHReg(14, HRcFlt64, False); }
HReg hregPPC32_FPR15 ( void ) { return mkHReg(15, HRcFlt64, False); }
HReg hregPPC32_FPR16 ( void ) { return mkHReg(16, HRcFlt64, False); }
HReg hregPPC32_FPR17 ( void ) { return mkHReg(17, HRcFlt64, False); }
HReg hregPPC32_FPR18 ( void ) { return mkHReg(18, HRcFlt64, False); }
HReg hregPPC32_FPR19 ( void ) { return mkHReg(19, HRcFlt64, False); }
HReg hregPPC32_FPR20 ( void ) { return mkHReg(20, HRcFlt64, False); }
HReg hregPPC32_FPR21 ( void ) { return mkHReg(21, HRcFlt64, False); }
HReg hregPPC32_FPR22 ( void ) { return mkHReg(22, HRcFlt64, False); }
HReg hregPPC32_FPR23 ( void ) { return mkHReg(23, HRcFlt64, False); }
HReg hregPPC32_FPR24 ( void ) { return mkHReg(24, HRcFlt64, False); }
HReg hregPPC32_FPR25 ( void ) { return mkHReg(25, HRcFlt64, False); }
HReg hregPPC32_FPR26 ( void ) { return mkHReg(26, HRcFlt64, False); }
HReg hregPPC32_FPR27 ( void ) { return mkHReg(27, HRcFlt64, False); }
HReg hregPPC32_FPR28 ( void ) { return mkHReg(28, HRcFlt64, False); }
HReg hregPPC32_FPR29 ( void ) { return mkHReg(29, HRcFlt64, False); }
HReg hregPPC32_FPR30 ( void ) { return mkHReg(30, HRcFlt64, False); }
HReg hregPPC32_FPR31 ( void ) { return mkHReg(31, HRcFlt64, False); }

HReg hregPPC32_VR0  ( void ) { return mkHReg( 0, HRcVec128, False); }
HReg hregPPC32_VR1  ( void ) { return mkHReg( 1, HRcVec128, False); }
HReg hregPPC32_VR2  ( void ) { return mkHReg( 2, HRcVec128, False); }
HReg hregPPC32_VR3  ( void ) { return mkHReg( 3, HRcVec128, False); }
HReg hregPPC32_VR4  ( void ) { return mkHReg( 4, HRcVec128, False); }
HReg hregPPC32_VR5  ( void ) { return mkHReg( 5, HRcVec128, False); }
HReg hregPPC32_VR6  ( void ) { return mkHReg( 6, HRcVec128, False); }
HReg hregPPC32_VR7  ( void ) { return mkHReg( 7, HRcVec128, False); }
HReg hregPPC32_VR8  ( void ) { return mkHReg( 8, HRcVec128, False); }
HReg hregPPC32_VR9  ( void ) { return mkHReg( 9, HRcVec128, False); }
HReg hregPPC32_VR10 ( void ) { return mkHReg(10, HRcVec128, False); }
HReg hregPPC32_VR11 ( void ) { return mkHReg(11, HRcVec128, False); }
HReg hregPPC32_VR12 ( void ) { return mkHReg(12, HRcVec128, False); }
HReg hregPPC32_VR13 ( void ) { return mkHReg(13, HRcVec128, False); }
HReg hregPPC32_VR14 ( void ) { return mkHReg(14, HRcVec128, False); }
HReg hregPPC32_VR15 ( void ) { return mkHReg(15, HRcVec128, False); }
HReg hregPPC32_VR16 ( void ) { return mkHReg(16, HRcVec128, False); }
HReg hregPPC32_VR17 ( void ) { return mkHReg(17, HRcVec128, False); }
HReg hregPPC32_VR18 ( void ) { return mkHReg(18, HRcVec128, False); }
HReg hregPPC32_VR19 ( void ) { return mkHReg(19, HRcVec128, False); }
HReg hregPPC32_VR20 ( void ) { return mkHReg(20, HRcVec128, False); }
HReg hregPPC32_VR21 ( void ) { return mkHReg(21, HRcVec128, False); }
HReg hregPPC32_VR22 ( void ) { return mkHReg(22, HRcVec128, False); }
HReg hregPPC32_VR23 ( void ) { return mkHReg(23, HRcVec128, False); }
HReg hregPPC32_VR24 ( void ) { return mkHReg(24, HRcVec128, False); }
HReg hregPPC32_VR25 ( void ) { return mkHReg(25, HRcVec128, False); }
HReg hregPPC32_VR26 ( void ) { return mkHReg(26, HRcVec128, False); }
HReg hregPPC32_VR27 ( void ) { return mkHReg(27, HRcVec128, False); }
HReg hregPPC32_VR28 ( void ) { return mkHReg(28, HRcVec128, False); }
HReg hregPPC32_VR29 ( void ) { return mkHReg(29, HRcVec128, False); }
HReg hregPPC32_VR30 ( void ) { return mkHReg(30, HRcVec128, False); }
HReg hregPPC32_VR31 ( void ) { return mkHReg(31, HRcVec128, False); }

void getAllocableRegs_PPC32 ( Int* nregs, HReg** arr )
{
   UInt i=0;
   *nregs = 90 - 24 - 24;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   // GPR0 = scratch reg where possible - some ops interpret as value zero
   // GPR1 = stack pointer
   // GPR2 = TOC pointer
   (*arr)[i++] = hregPPC32_GPR3();
   (*arr)[i++] = hregPPC32_GPR4();
   (*arr)[i++] = hregPPC32_GPR5();
   (*arr)[i++] = hregPPC32_GPR6();
   (*arr)[i++] = hregPPC32_GPR7();
   (*arr)[i++] = hregPPC32_GPR8();
   (*arr)[i++] = hregPPC32_GPR9();
   (*arr)[i++] = hregPPC32_GPR10();
   (*arr)[i++] = hregPPC32_GPR11();
   (*arr)[i++] = hregPPC32_GPR12();
   // GPR13 = thread specific pointer
   (*arr)[i++] = hregPPC32_GPR14();
   (*arr)[i++] = hregPPC32_GPR15();
   (*arr)[i++] = hregPPC32_GPR16();
   (*arr)[i++] = hregPPC32_GPR17();
   (*arr)[i++] = hregPPC32_GPR18();
   (*arr)[i++] = hregPPC32_GPR19();
   (*arr)[i++] = hregPPC32_GPR20();
   (*arr)[i++] = hregPPC32_GPR21();
   (*arr)[i++] = hregPPC32_GPR22();
   (*arr)[i++] = hregPPC32_GPR23();
   (*arr)[i++] = hregPPC32_GPR24();
   (*arr)[i++] = hregPPC32_GPR25();
   (*arr)[i++] = hregPPC32_GPR26();
   (*arr)[i++] = hregPPC32_GPR27();
   (*arr)[i++] = hregPPC32_GPR28();
   (*arr)[i++] = hregPPC32_GPR29();
   // GPR30 AltiVec spill reg temporary
   // GPR31 = GuestStatePtr

   /* For both ppc32-linux and ppc64-linux, f14-f31 are callee save.
      So use them. */
   (*arr)[i++] = hregPPC32_FPR14();
   (*arr)[i++] = hregPPC32_FPR15();
   (*arr)[i++] = hregPPC32_FPR16();
   (*arr)[i++] = hregPPC32_FPR17();
   (*arr)[i++] = hregPPC32_FPR18();
   (*arr)[i++] = hregPPC32_FPR19();
   (*arr)[i++] = hregPPC32_FPR20();
   (*arr)[i++] = hregPPC32_FPR21();
/*
   (*arr)[i++] = hregPPC32_FPR8();
   (*arr)[i++] = hregPPC32_FPR9();
   (*arr)[i++] = hregPPC32_FPR10();
   (*arr)[i++] = hregPPC32_FPR11();
   (*arr)[i++] = hregPPC32_FPR12();
   (*arr)[i++] = hregPPC32_FPR13();
   (*arr)[i++] = hregPPC32_FPR14();
   (*arr)[i++] = hregPPC32_FPR15();
   (*arr)[i++] = hregPPC32_FPR16();
   (*arr)[i++] = hregPPC32_FPR17();
   (*arr)[i++] = hregPPC32_FPR18();
   (*arr)[i++] = hregPPC32_FPR19();
   (*arr)[i++] = hregPPC32_FPR20();
   (*arr)[i++] = hregPPC32_FPR21();
   (*arr)[i++] = hregPPC32_FPR22();
   (*arr)[i++] = hregPPC32_FPR23();
   (*arr)[i++] = hregPPC32_FPR24();
   (*arr)[i++] = hregPPC32_FPR25();
   (*arr)[i++] = hregPPC32_FPR26();
   (*arr)[i++] = hregPPC32_FPR27();
   (*arr)[i++] = hregPPC32_FPR28();
   (*arr)[i++] = hregPPC32_FPR29();
   (*arr)[i++] = hregPPC32_FPR30();
   (*arr)[i++] = hregPPC32_FPR31();
*/
   /* For both ppc32-linux and ppc64-linux, v20-v31 are callee-save.
      So use them. */
   (*arr)[i++] = hregPPC32_VR20();
   (*arr)[i++] = hregPPC32_VR21();
   (*arr)[i++] = hregPPC32_VR22();
   (*arr)[i++] = hregPPC32_VR23();
   (*arr)[i++] = hregPPC32_VR24();
   (*arr)[i++] = hregPPC32_VR25();
   (*arr)[i++] = hregPPC32_VR26();
   (*arr)[i++] = hregPPC32_VR27();
/*
   (*arr)[i++] = hregPPC32_VR8();
   (*arr)[i++] = hregPPC32_VR9();
   (*arr)[i++] = hregPPC32_VR10();
   (*arr)[i++] = hregPPC32_VR11();
   (*arr)[i++] = hregPPC32_VR12();
   (*arr)[i++] = hregPPC32_VR13();
   (*arr)[i++] = hregPPC32_VR14();
   (*arr)[i++] = hregPPC32_VR15();
   (*arr)[i++] = hregPPC32_VR16();
   (*arr)[i++] = hregPPC32_VR17();
   (*arr)[i++] = hregPPC32_VR18();
   (*arr)[i++] = hregPPC32_VR19();
   (*arr)[i++] = hregPPC32_VR20();
   (*arr)[i++] = hregPPC32_VR21();
   (*arr)[i++] = hregPPC32_VR22();
   (*arr)[i++] = hregPPC32_VR23();
   (*arr)[i++] = hregPPC32_VR24();
   (*arr)[i++] = hregPPC32_VR25();
   (*arr)[i++] = hregPPC32_VR26();
   (*arr)[i++] = hregPPC32_VR27();
   (*arr)[i++] = hregPPC32_VR28();
   (*arr)[i++] = hregPPC32_VR29();
   (*arr)[i++] = hregPPC32_VR30();
   (*arr)[i++] = hregPPC32_VR31();
*/
   vassert(i == *nregs);
}


/* --------- Condition codes, Intel encoding. --------- */

HChar* showPPC32CondCode ( PPC32CondCode cond )
{
   if (cond.test == Pct_ALWAYS) return "always";

   switch (cond.flag) {
      case Pcf_7SO: return (cond.test == Pct_TRUE) ? "cr7.so=1" : "cr7.so=0";
      case Pcf_7EQ: return (cond.test == Pct_TRUE) ? "cr7.eq=1" : "cr7.eq=0";
      case Pcf_7GT: return (cond.test == Pct_TRUE) ? "cr7.gt=1" : "cr7.gt=0";
      case Pcf_7LT: return (cond.test == Pct_TRUE) ? "cr7.lt=1" : "cr7.lt=0";
      default: vpanic("ppPPC32CondCode");
   }
}

/* construct condition code */
PPC32CondCode mk_PPCCondCode ( PPC32CondTest test, PPC32CondFlag flag )
{
   PPC32CondCode cc;
   cc.flag = flag;
   cc.test = test;
   return cc;
}

/* false->true, true->false */
PPC32CondTest invertCondTest ( PPC32CondTest ct )
{
   vassert(ct != Pct_ALWAYS);
   return (ct == Pct_TRUE) ? Pct_FALSE : Pct_TRUE;
}


/* --------- PPCAMode: memory address expressions. --------- */

PPC32AMode* PPC32AMode_IR ( Int idx, HReg base ) {
   PPC32AMode* am = LibVEX_Alloc(sizeof(PPC32AMode));
   vassert(idx >= -0x8000 && idx < 0x8000);
   am->tag = Pam_IR;
   am->Pam.IR.base = base;
   am->Pam.IR.index = idx;
   return am;
}
PPC32AMode* PPC32AMode_RR ( HReg idx, HReg base ) {
   PPC32AMode* am = LibVEX_Alloc(sizeof(PPC32AMode));
   am->tag = Pam_RR;
   am->Pam.RR.base = base;
   am->Pam.RR.index = idx;
   return am;
}

PPC32AMode* dopyPPC32AMode ( PPC32AMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      return PPC32AMode_IR( am->Pam.IR.index, am->Pam.IR.base );
   case Pam_RR: 
      return PPC32AMode_RR( am->Pam.RR.index, am->Pam.RR.base );
   default:
      vpanic("dopyPPC32AMode");
   }
}

void ppPPC32AMode ( PPC32AMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      if (am->Pam.IR.index == 0)
         vex_printf("0(");
      else
         vex_printf("%d(", (Int)am->Pam.IR.index);
      ppHRegPPC32(am->Pam.IR.base);
      vex_printf(")");
      return;
   case Pam_RR:
      ppHRegPPC32(am->Pam.RR.base);
      vex_printf(",");
      ppHRegPPC32(am->Pam.RR.index);
      return;
   default:
      vpanic("ppPPC32AMode");
   }
}

static void addRegUsage_PPC32AMode ( HRegUsage* u, PPC32AMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      addHRegUse(u, HRmRead, am->Pam.IR.base);
      return;
   case Pam_RR:
      addHRegUse(u, HRmRead, am->Pam.RR.base);
      addHRegUse(u, HRmRead, am->Pam.RR.index);
      return;
   default:
      vpanic("addRegUsage_PPC32AMode");
   }
}

static void mapRegs_PPC32AMode ( HRegRemap* m, PPC32AMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      am->Pam.IR.base = lookupHRegRemap(m, am->Pam.IR.base);
      return;
   case Pam_RR:
      am->Pam.RR.base = lookupHRegRemap(m, am->Pam.RR.base);
      am->Pam.RR.index = lookupHRegRemap(m, am->Pam.RR.index);
      return;
   default:
      vpanic("mapRegs_PPC32AMode");
   }
}

/* --------- Operand, which can be a reg or a u16/s16. --------- */

PPC32RH* PPC32RH_Imm ( Bool syned, UShort imm16 ) {
   PPC32RH* op       = LibVEX_Alloc(sizeof(PPC32RH));
   op->tag           = Prh_Imm;
   op->Prh.Imm.syned = syned;
   op->Prh.Imm.imm16 = imm16;
   /* If this is a signed value, ensure it's not -32768, so that we
      are guaranteed always to be able to negate if needed. */
   if (syned)
      vassert(imm16 != 0x8000);
   vassert(syned == True || syned == False);
   return op;
}
PPC32RH* PPC32RH_Reg ( HReg reg ) {
   PPC32RH* op     = LibVEX_Alloc(sizeof(PPC32RH));
   op->tag         = Prh_Reg;
   op->Prh.Reg.reg = reg;
   return op;
}

void ppPPC32RH ( PPC32RH* op ) {
   switch (op->tag) {
   case Prh_Imm: 
      if (op->Prh.Imm.syned)
         vex_printf("%d", (Int)(Short)op->Prh.Imm.imm16);
      else
         vex_printf("%u", (UInt)(UShort)op->Prh.Imm.imm16);
      return;
   case Prh_Reg: 
      ppHRegPPC32(op->Prh.Reg.reg);
      return;
   default: 
      vpanic("ppPPC32RH");
   }
}

/* An PPC32RH can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_PPC32RH ( HRegUsage* u, PPC32RH* op ) {
   switch (op->tag) {
   case Prh_Imm: 
      return;
   case Prh_Reg: 
      addHRegUse(u, HRmRead, op->Prh.Reg.reg);
      return;
   default: 
      vpanic("addRegUsage_PPC32RH");
   }
}

static void mapRegs_PPC32RH ( HRegRemap* m, PPC32RH* op ) {
   switch (op->tag) {
   case Prh_Imm: 
      return;
   case Prh_Reg: 
      op->Prh.Reg.reg = lookupHRegRemap(m, op->Prh.Reg.reg);
      return;
   default: 
      vpanic("mapRegs_PPC32RH");
   }
}


/* --------- Operand, which can be a reg or a u32. --------- */

PPC32RI* PPC32RI_Imm ( UInt imm32 ) {
   PPC32RI* op = LibVEX_Alloc(sizeof(PPC32RI));
   op->tag       = Pri_Imm;
   op->Pri.Imm   = imm32;
   return op;
}
PPC32RI* PPC32RI_Reg ( HReg reg ) {
   PPC32RI* op = LibVEX_Alloc(sizeof(PPC32RI));
   op->tag       = Pri_Reg;
   op->Pri.Reg   = reg;
   return op;
}

void ppPPC32RI ( PPC32RI* dst ) {
   switch (dst->tag) {
      case Pri_Imm: 
         vex_printf("0x%x", dst->Pri.Imm);
         break;
      case Pri_Reg: 
         ppHRegPPC32(dst->Pri.Reg);
         break;
      default: 
         vpanic("ppPPC32RI");
   }
}

/* An PPC32RI can only be used in a "read" context (what would it
   mean to write or modify a literal?) and so we enumerate its
   registers accordingly. */
static void addRegUsage_PPC32RI ( HRegUsage* u, PPC32RI* dst ) {
   switch (dst->tag) {
      case Pri_Imm: 
         return;
      case Pri_Reg: 
         addHRegUse(u, HRmRead, dst->Pri.Reg);
         return;
      default: 
         vpanic("addRegUsage_PPC32RI");
   }
}

static void mapRegs_PPC32RI ( HRegRemap* m, PPC32RI* dst ) {
   switch (dst->tag) {
      case Pri_Imm: 
         return;
      case Pri_Reg: 
         dst->Pri.Reg = lookupHRegRemap(m, dst->Pri.Reg);
         return;
      default: 
         vpanic("mapRegs_PPC32RI");
   }
}


/* --------- Operand, which can be a vector reg or a simm5. --------- */

PPC32VI5s* PPC32VI5s_Imm ( Char simm5 ) {
   PPC32VI5s* op = LibVEX_Alloc(sizeof(PPC32VI5s));
   op->tag       = Pvi_Imm;
   op->Pvi.Imm5s = simm5;
   vassert(simm5 >= -16 && simm5 <= 15);
   return op;
}
PPC32VI5s* PPC32VI5s_Reg ( HReg reg ) {
   PPC32VI5s* op = LibVEX_Alloc(sizeof(PPC32VI5s));
   op->tag       = Pvi_Reg;
   op->Pvi.Reg   = reg;
   vassert(hregClass(reg) == HRcVec128);
   return op;
}

void ppPPC32VI5s ( PPC32VI5s* src ) {
   switch (src->tag) {
      case Pvi_Imm: 
         vex_printf("%d", (Int)src->Pvi.Imm5s);
         break;
      case Pvi_Reg: 
         ppHRegPPC32(src->Pvi.Reg);
         break;
      default: 
         vpanic("ppPPC32VI5s");
   }
}

/* An PPC32VI5s can only be used in a "read" context (what would it
   mean to write or modify a literal?) and so we enumerate its
   registers accordingly. */
static void addRegUsage_PPC32VI5s ( HRegUsage* u, PPC32VI5s* dst ) {
   switch (dst->tag) {
      case Pvi_Imm: 
         return;
      case Pvi_Reg: 
         addHRegUse(u, HRmRead, dst->Pvi.Reg);
         return;
      default: 
         vpanic("addRegUsage_PPC32VI5s");
   }
}

static void mapRegs_PPC32VI5s ( HRegRemap* m, PPC32VI5s* dst ) {
   switch (dst->tag) {
      case Pvi_Imm: 
         return;
      case Pvi_Reg: 
         dst->Pvi.Reg = lookupHRegRemap(m, dst->Pvi.Reg);
         return;
      default: 
         vpanic("mapRegs_PPC32VI5s");
   }
}


/* --------- Instructions. --------- */

HChar* showPPC32UnaryOp ( PPC32UnaryOp op ) {
   switch (op) {
   case Pun_NOT: return "not";
   case Pun_NEG: return "neg";
   case Pun_CLZ: return "cntlzw";
   default: vpanic("showPPC32UnaryOp");
   }
}

HChar* showPPC32AluOp ( PPC32AluOp op, Bool immR ) {
   switch (op) {
      case Palu_ADD: return immR ? "addi"  : "add";
      case Palu_SUB: return immR ? "subi"  : "sub";
      case Palu_AND: return immR ? "andi." : "and";
      case Palu_OR:  return immR ? "ori"   : "or";
      case Palu_XOR: return immR ? "xori"  : "xor";
      case Palu_SHL: return immR ? "slwi"  : "slw";
      case Palu_SHR: return immR ? "srwi"  : "srw";
      case Palu_SAR: return immR ? "srawi" : "sraw";
      default: vpanic("showPPC32AluOp");
   }
}

HChar* showPPC32FpOp ( PPC32FpOp op ) {
   switch (op) {
      case Pfp_ADD:    return "fadd";
      case Pfp_SUB:    return "fsub";
      case Pfp_MUL:    return "fmul";
      case Pfp_DIV:    return "fdiv";
      case Pfp_SQRT:   return "fsqrt";
      case Pfp_ABS:    return "fabs";
      case Pfp_NEG:    return "fneg";
      case Pfp_MOV:    return "fmr";
      default: vpanic("showPPC32FpOp");
   }
}

HChar* showPPC32AvOp ( PPC32AvOp op ) {
   switch (op) {

   /* Unary */
   case Pav_MOV:       return "vmr";      /* Mov */
     
   case Pav_AND:       return "vand";     /* Bitwise */
   case Pav_OR:        return "vor";
   case Pav_XOR:       return "vxor";
   case Pav_NOT:       return "vnot";

   case Pav_UNPCKH8S:  return "vupkhsb";  /* Unpack */
   case Pav_UNPCKH16S: return "vupkhsh";
   case Pav_UNPCKL8S:  return "vupklsb";
   case Pav_UNPCKL16S: return "vupklsh";
   case Pav_UNPCKHPIX: return "vupkhpx";
   case Pav_UNPCKLPIX: return "vupklpx";

   /* Integer binary */
   case Pav_ADDU:      return "vaddu_m";  // b,h,w
   case Pav_QADDU:     return "vaddu_s";  // b,h,w
   case Pav_QADDS:     return "vadds_s";  // b,h,w
     
   case Pav_SUBU:      return "vsubu_m";  // b,h,w
   case Pav_QSUBU:     return "vsubu_s";  // b,h,w
   case Pav_QSUBS:     return "vsubs_s";  // b,h,w
     
   case Pav_OMULU:     return "vmulou";   // b,h
   case Pav_OMULS:     return "vmulos";   // b,h
   case Pav_EMULU:     return "vmuleu";   // b,h
   case Pav_EMULS:     return "vmules";   // b,h
  
   case Pav_AVGU:      return "vavgu";    // b,h,w
   case Pav_AVGS:      return "vavgs";    // b,h,w
     
   case Pav_MAXU:      return "vmaxu";    // b,h,w
   case Pav_MAXS:      return "vmaxs";    // b,h,w
     
   case Pav_MINU:      return "vminu";    // b,h,w
   case Pav_MINS:      return "vmins";    // b,h,w
     
   /* Compare (always affects CR field 6) */
   case Pav_CMPEQU:    return "vcmpequ";  // b,h,w
   case Pav_CMPGTU:    return "vcmpgtu";  // b,h,w
   case Pav_CMPGTS:    return "vcmpgts";  // b,h,w

   /* Shift */
   case Pav_SHL:       return "vsl";      // ' ',b,h,w
   case Pav_SHR:       return "vsr";      // ' ',b,h,w
   case Pav_SAR:       return "vsra";     // b,h,w
   case Pav_ROTL:      return "vrl";      // b,h,w

   /* Pack */
   case Pav_PACKUU:    return "vpku_um";  // h,w
   case Pav_QPACKUU:   return "vpku_us";  // h,w
   case Pav_QPACKSU:   return "vpks_us";  // h,w
   case Pav_QPACKSS:   return "vpks_ss";  // h,w
   case Pav_PACKPXL:   return "vpkpx";

   /* Merge */
   case Pav_MRGHI:     return "vmrgh";    // b,h,w
   case Pav_MRGLO:     return "vmrgl";    // b,h,w

   default: vpanic("showPPC32AvOp");
   }
}

HChar* showPPC32AvFpOp ( PPC32AvFpOp op ) {
   switch (op) {
   /* Floating Point Binary */
   case Pavfp_ADDF:      return "vaddfp";
   case Pavfp_SUBF:      return "vsubfp";
   case Pavfp_MULF:      return "vmaddfp";
   case Pavfp_MAXF:      return "vmaxfp";
   case Pavfp_MINF:      return "vminfp";
   case Pavfp_CMPEQF:    return "vcmpeqfp";
   case Pavfp_CMPGTF:    return "vcmpgtfp";
   case Pavfp_CMPGEF:    return "vcmpgefp";
     
   /* Floating Point Unary */
   case Pavfp_RCPF:      return "vrefp";
   case Pavfp_RSQRTF:    return "vrsqrtefp";
   case Pavfp_CVTU2F:    return "vcfux";
   case Pavfp_CVTS2F:    return "vcfsx";
   case Pavfp_QCVTF2U:   return "vctuxs";
   case Pavfp_QCVTF2S:   return "vctsxs";
   case Pavfp_ROUNDM:    return "vrfim";
   case Pavfp_ROUNDP:    return "vrfip";
   case Pavfp_ROUNDN:    return "vrfin";
   case Pavfp_ROUNDZ:    return "vrfiz";

   default: vpanic("showPPC32AvFpOp");
   }
}

PPC32Instr* PPC32Instr_LI32 ( HReg dst, UInt imm32 )
{
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_LI32;
   i->Pin.LI32.dst   = dst;
   i->Pin.LI32.imm32 = imm32;
   return i;
}
PPC32Instr* PPC32Instr_Alu32 ( PPC32AluOp op, HReg dst, 
                               HReg srcL, PPC32RH* srcR ) {
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_Alu32;
   i->Pin.Alu32.op   = op;
   i->Pin.Alu32.dst  = dst;
   i->Pin.Alu32.srcL = srcL;
   i->Pin.Alu32.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AddSubC32 ( Bool isAdd, Bool setC,
                                   HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i          = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                 = Pin_AddSubC32;
   i->Pin.AddSubC32.isAdd = isAdd;
   i->Pin.AddSubC32.setC  = setC;
   i->Pin.AddSubC32.dst   = dst;
   i->Pin.AddSubC32.srcL  = srcL;
   i->Pin.AddSubC32.srcR  = srcR;
   return i;
}
PPC32Instr* PPC32Instr_Cmp32 ( Bool syned, UInt crfD, 
                               HReg srcL, PPC32RH* srcR ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_Cmp32;
   i->Pin.Cmp32.syned = syned;
   i->Pin.Cmp32.crfD  = crfD;
   i->Pin.Cmp32.srcL  = srcL;
   i->Pin.Cmp32.srcR  = srcR;
   return i;
}
PPC32Instr* PPC32Instr_Unary32  ( PPC32UnaryOp op, HReg dst, HReg src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_Unary32;
   i->Pin.Unary32.op  = op;
   i->Pin.Unary32.dst = dst;
   i->Pin.Unary32.src = src;
   return i;
}
PPC32Instr* PPC32Instr_MulL ( Bool syned, Bool hi32, 
                              HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_MulL;
   i->Pin.MulL.syned = syned;
   i->Pin.MulL.hi32  = hi32;
   i->Pin.MulL.dst   = dst;
   i->Pin.MulL.srcL  = srcL;
   i->Pin.MulL.srcR  = srcR;
   /* if doing the low 32, the signedness is irrelevant, but tie it
      down anyway. */
   if (!hi32) vassert(!syned);
   return i;
}
PPC32Instr* PPC32Instr_Div ( Bool syned, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_Div;
   i->Pin.Div.syned   = syned;
   i->Pin.Div.dst     = dst;
   i->Pin.Div.srcL    = srcL;
   i->Pin.Div.srcR    = srcR;
   return i;
}
PPC32Instr* PPC32Instr_Call ( PPC32CondCode cond, 
                              Addr32 target, UInt argiregs ) {
   UInt mask;
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_Call;
   i->Pin.Call.cond     = cond;
   i->Pin.Call.target   = target;
   i->Pin.Call.argiregs = argiregs;
   /* Only r3 .. r10 inclusive may be used as arg regs. Hence: */
   mask = (1<<3)|(1<<4)|(1<<5)|(1<<6)|(1<<7)|(1<<8)|(1<<9)|(1<<10);
   vassert(0 == (argiregs & ~mask));
   return i;
}
PPC32Instr* PPC32Instr_Goto ( IRJumpKind jk, 
                              PPC32CondCode cond, PPC32RI* dst ) {
   PPC32Instr* i    = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag           = Pin_Goto;
   i->Pin.Goto.cond = cond;
   i->Pin.Goto.dst  = dst;
   i->Pin.Goto.jk   = jk;
   return i;
}
PPC32Instr* PPC32Instr_CMov32  ( PPC32CondCode cond, 
                                 HReg dst, PPC32RI* src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_CMov32;
   i->Pin.CMov32.cond = cond;
   i->Pin.CMov32.src  = src;
   i->Pin.CMov32.dst  = dst;
   vassert(cond.test != Pct_ALWAYS);
   return i;
}
PPC32Instr* PPC32Instr_Load ( UChar sz, Bool syned,
                              HReg dst, PPC32AMode* src ) {
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_Load;
   i->Pin.Load.sz    = sz;
   i->Pin.Load.syned = syned;
   i->Pin.Load.src   = src;
   i->Pin.Load.dst   = dst;
   vassert(sz == 1 || sz == 2 || sz == 4);
   return i;
}
PPC32Instr* PPC32Instr_Store ( UChar sz, PPC32AMode* dst, HReg src ) {
   PPC32Instr* i    = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag           = Pin_Store;
   i->Pin.Store.sz  = sz;
   i->Pin.Store.src = src;
   i->Pin.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4);
   return i;
}
PPC32Instr* PPC32Instr_Set32 ( PPC32CondCode cond, HReg dst ) {
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_Set32;
   i->Pin.Set32.cond = cond;
   i->Pin.Set32.dst  = dst;
   return i;
}
PPC32Instr* PPC32Instr_MfCR ( HReg dst )
{
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_MfCR;
   i->Pin.MfCR.dst   = dst;
   return i;
}
PPC32Instr* PPC32Instr_MFence ( void )
{
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_MFence;
   return i;
}

PPC32Instr* PPC32Instr_FpUnary ( PPC32FpOp op, HReg dst, HReg src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_FpUnary;
   i->Pin.FpUnary.op  = op;
   i->Pin.FpUnary.dst = dst;
   i->Pin.FpUnary.src = src;
   return i;
}
PPC32Instr* PPC32Instr_FpBinary ( PPC32FpOp op, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_FpBinary;
   i->Pin.FpBinary.op   = op;
   i->Pin.FpBinary.dst  = dst;
   i->Pin.FpBinary.srcL = srcL;
   i->Pin.FpBinary.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_FpLdSt ( Bool isLoad, UChar sz, HReg reg, PPC32AMode* addr ) {
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_FpLdSt;
   i->Pin.FpLdSt.isLoad = isLoad;
   i->Pin.FpLdSt.sz     = sz;
   i->Pin.FpLdSt.reg    = reg;
   i->Pin.FpLdSt.addr   = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}
PPC32Instr* PPC32Instr_FpF64toF32 ( HReg dst, HReg src ) {
   PPC32Instr* i         = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                = Pin_FpF64toF32;
   i->Pin.FpF64toF32.dst = dst;
   i->Pin.FpF64toF32.src = src;
   return i;
}
PPC32Instr* PPC32Instr_FpF64toI32 ( HReg dst, HReg src ) {
   PPC32Instr* i         = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                = Pin_FpF64toI32;
   i->Pin.FpF64toI32.dst = dst;
   i->Pin.FpF64toI32.src = src;
   return i;
}
PPC32Instr* PPC32Instr_FpCMov ( PPC32CondCode cond, HReg dst, HReg src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_FpCMov;
   i->Pin.FpCMov.cond = cond;
   i->Pin.FpCMov.dst  = dst;
   i->Pin.FpCMov.src  = src;
   vassert(cond.test != Pct_ALWAYS);
   return i;
}
PPC32Instr* PPC32Instr_FpLdFPSCR ( HReg src ) {
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_FpLdFPSCR;
   i->Pin.FpLdFPSCR.src = src;
   return i;
}
PPC32Instr* PPC32Instr_FpCmp ( HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_FpCmp;
   i->Pin.FpCmp.dst  = dst;
   i->Pin.FpCmp.srcL = srcL;
   i->Pin.FpCmp.srcR = srcR;
   return i;
}

/* Read/Write Link Register */
PPC32Instr* PPC32Instr_RdWrLR ( Bool wrLR, HReg gpr ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_RdWrLR;
   i->Pin.RdWrLR.wrLR = wrLR;
   i->Pin.RdWrLR.gpr  = gpr;
   return i;
}

/* AltiVec */
PPC32Instr* PPC32Instr_AvLdSt ( Bool isLoad, UChar sz, HReg reg, PPC32AMode* addr ) {
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_AvLdSt;
   i->Pin.AvLdSt.isLoad = isLoad;
   i->Pin.AvLdSt.sz     = sz;
   i->Pin.AvLdSt.reg    = reg;
   i->Pin.AvLdSt.addr   = addr;
   return i;
}
PPC32Instr* PPC32Instr_AvUnary ( PPC32AvOp op, HReg dst, HReg src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_AvUnary;
   i->Pin.AvUnary.op  = op;
   i->Pin.AvUnary.dst = dst;
   i->Pin.AvUnary.src = src;
   return i;
}
PPC32Instr* PPC32Instr_AvBinary ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_AvBinary;
   i->Pin.AvBinary.op   = op;
   i->Pin.AvBinary.dst  = dst;
   i->Pin.AvBinary.srcL = srcL;
   i->Pin.AvBinary.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvBin8x16 ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i          = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                 = Pin_AvBin8x16;
   i->Pin.AvBin8x16.op   = op;
   i->Pin.AvBin8x16.dst  = dst;
   i->Pin.AvBin8x16.srcL = srcL;
   i->Pin.AvBin8x16.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvBin16x8 ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i          = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                 = Pin_AvBin16x8;
   i->Pin.AvBin16x8.op   = op;
   i->Pin.AvBin16x8.dst  = dst;
   i->Pin.AvBin16x8.srcL = srcL;
   i->Pin.AvBin16x8.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvBin32x4 ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i          = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                 = Pin_AvBin32x4;
   i->Pin.AvBin32x4.op   = op;
   i->Pin.AvBin32x4.dst  = dst;
   i->Pin.AvBin32x4.srcL = srcL;
   i->Pin.AvBin32x4.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvBin32Fx4 ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i          = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                 = Pin_AvBin32Fx4;
   i->Pin.AvBin32Fx4.op   = op;
   i->Pin.AvBin32Fx4.dst  = dst;
   i->Pin.AvBin32Fx4.srcL = srcL;
   i->Pin.AvBin32Fx4.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvUn32Fx4 ( PPC32AvOp op, HReg dst, HReg src ) {
   PPC32Instr* i        = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag               = Pin_AvUn32Fx4;
   i->Pin.AvUn32Fx4.op  = op;
   i->Pin.AvUn32Fx4.dst = dst;
   i->Pin.AvUn32Fx4.src = src;
   return i;
}
PPC32Instr* PPC32Instr_AvPerm ( HReg dst, HReg srcL, HReg srcR, HReg ctl ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_AvPerm;
   i->Pin.AvPerm.dst  = dst;
   i->Pin.AvPerm.srcL = srcL;
   i->Pin.AvPerm.srcR = srcR;
   i->Pin.AvPerm.ctl  = ctl;
   return i;
}
PPC32Instr* PPC32Instr_AvSel ( HReg ctl, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_AvSel;
   i->Pin.AvSel.ctl  = ctl;
   i->Pin.AvSel.dst  = dst;
   i->Pin.AvSel.srcL = srcL;
   i->Pin.AvSel.srcR = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvShlDbl ( UChar shift, HReg dst, HReg srcL, HReg srcR ) {
   PPC32Instr* i         = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                = Pin_AvShlDbl;
   i->Pin.AvShlDbl.shift = shift;
   i->Pin.AvShlDbl.dst   = dst;
   i->Pin.AvShlDbl.srcL  = srcL;
   i->Pin.AvShlDbl.srcR  = srcR;
   return i;
}
PPC32Instr* PPC32Instr_AvSplat ( UChar sz, HReg dst, PPC32VI5s* src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_AvSplat;
   i->Pin.AvSplat.sz  = sz;
   i->Pin.AvSplat.dst = dst;
   i->Pin.AvSplat.src = src;
   return i;
}
PPC32Instr* PPC32Instr_AvCMov ( PPC32CondCode cond, HReg dst, HReg src ) {
   PPC32Instr* i      = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag             = Pin_AvCMov;
   i->Pin.AvCMov.cond = cond;
   i->Pin.AvCMov.dst  = dst;
   i->Pin.AvCMov.src  = src;
   vassert(cond.test != Pct_ALWAYS);
   return i;
}
PPC32Instr* PPC32Instr_AvLdVSCR ( HReg src ) {
   PPC32Instr* i       = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag              = Pin_AvLdVSCR;
   i->Pin.AvLdVSCR.src = src;
   return i;
}


/* Pretty Print instructions */
static void ppLoadImm ( HReg dst, UInt imm ) {
   if (imm < 0x10000) {
      vex_printf("li ");
      ppHRegPPC32(dst);
      vex_printf(",0x%x", imm);
   } else {
      vex_printf("lis ");
      ppHRegPPC32(dst);
      vex_printf(",0x%x ; ", imm >> 16);
      vex_printf("ori ");
      ppHRegPPC32(dst);
      vex_printf(",");
      ppHRegPPC32(dst);
      vex_printf(",0x%x", imm & 0xFFFF);
   }
}

static void ppMovReg ( HReg dst, HReg src ) {
   if (hregNumber(dst) != hregNumber(src)) {
      vex_printf("mr ");
      ppHRegPPC32(dst);
      vex_printf(",");
      ppHRegPPC32(src);
   }
}

void ppPPC32Instr ( PPC32Instr* i )
{
   switch (i->tag) {
   case Pin_LI32:
      vex_printf("li32 ");
      ppHRegPPC32(i->Pin.LI32.dst);
      vex_printf(",0x%x", i->Pin.LI32.imm32);
      break;
   case Pin_Alu32:
      /* special-case "mr" */
      if (i->Pin.Alu32.op == Palu_OR &&   // or Rd,Rs,Rs == mr Rd,Rs
          i->Pin.Alu32.srcR->tag == Prh_Reg &&
          i->Pin.Alu32.srcR->Prh.Reg.reg == i->Pin.Alu32.srcL) {
         vex_printf("mr ");
         ppHRegPPC32(i->Pin.Alu32.dst);
         vex_printf(",");
         ppHRegPPC32(i->Pin.Alu32.srcL);
      } else {
         /* generic */
         vex_printf("%s ", 
                    showPPC32AluOp(i->Pin.Alu32.op,
                                   toBool(i->Pin.Alu32.srcR->tag == Prh_Imm)));
         ppHRegPPC32(i->Pin.Alu32.dst);
         vex_printf(",");
         ppHRegPPC32(i->Pin.Alu32.srcL);
         vex_printf(",");
         ppPPC32RH(i->Pin.Alu32.srcR);
      }
      return;
   case Pin_AddSubC32:
      vex_printf("%s%s ",
                 i->Pin.AddSubC32.isAdd ? "add" : "sub",
                 i->Pin.AddSubC32.setC ? "c" : "e");
      ppHRegPPC32(i->Pin.AddSubC32.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AddSubC32.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AddSubC32.srcR);
      return;
   case Pin_Cmp32:
      vex_printf("%s%s %%cr%u,",
                 i->Pin.Cmp32.syned ? "cmp" : "cmpl",
                 i->Pin.Cmp32.srcR->tag == Prh_Imm ? "i" : "",
                 i->Pin.Cmp32.crfD);
      ppHRegPPC32(i->Pin.Cmp32.srcL);
      vex_printf(",");
      ppPPC32RH(i->Pin.Cmp32.srcR);
      return;
   case Pin_Unary32:
      vex_printf("%s ", showPPC32UnaryOp(i->Pin.Unary32.op));
      ppHRegPPC32(i->Pin.Unary32.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.Unary32.src);
      return;
   case Pin_MulL:
      vex_printf("mul%s%s ",
                 i->Pin.MulL.hi32 ? "hw" : "lw",
                 i->Pin.MulL.hi32 ? (i->Pin.MulL.syned ? "s" : "u") : "");
      ppHRegPPC32(i->Pin.MulL.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.MulL.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.MulL.srcR);
      return;
   case Pin_Div:
      vex_printf("divw%s ",
                 i->Pin.Div.syned ? "" : "u");
      ppHRegPPC32(i->Pin.Div.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.Div.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.Div.srcR);
      return;
   case Pin_Call: {
      Int n;
      vex_printf("call: ");
      if (i->Pin.Call.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) ", showPPC32CondCode(i->Pin.Call.cond));
      }
      vex_printf("{ ");
      ppLoadImm(hregPPC32_GPR12(), i->Pin.Call.target);
      vex_printf(" ; mtctr r12 ; bctrl [");
      for (n = 0; n < 32; n++) {
         if (i->Pin.Call.argiregs & (1<<n)) {
            vex_printf("r%d", n);
            if ((i->Pin.Call.argiregs >> n) > 1)
               vex_printf(",");
         }
      }
      vex_printf("] }");
      break;
   }
   case Pin_Goto:
      vex_printf("goto: ");
      if (i->Pin.Goto.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) ", showPPC32CondCode(i->Pin.Goto.cond));
      }
      vex_printf("{ ");
      if (i->Pin.Goto.jk != Ijk_Boring) {
         vex_printf("li %%r31,$");
         ppIRJumpKind(i->Pin.Goto.jk);
         vex_printf(" ; ");
      }
      if (i->Pin.Goto.dst->tag == Pri_Imm) {
         ppLoadImm(hregPPC32_GPR3(), i->Pin.Goto.dst->Pri.Imm);
      } else {
         ppMovReg(hregPPC32_GPR3(), i->Pin.Goto.dst->Pri.Reg);
      }
      vex_printf(" ; blr }");
      return;
   case Pin_CMov32:
      vex_printf("cmov32 (%s) ", showPPC32CondCode(i->Pin.CMov32.cond));
      ppHRegPPC32(i->Pin.CMov32.dst);
      vex_printf(",");
      ppPPC32RI(i->Pin.CMov32.src);
      vex_printf(": ");
      if (i->Pin.CMov32.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) ", showPPC32CondCode(i->Pin.CMov32.cond));
      }
      vex_printf("{ ");
      if (i->Pin.CMov32.src->tag == Pri_Imm) {
         ppLoadImm(i->Pin.CMov32.dst, i->Pin.CMov32.src->Pri.Imm);
      } else {
         ppMovReg(i->Pin.CMov32.dst, i->Pin.CMov32.src->Pri.Reg);
      }
      vex_printf(" }");
      return;
   case Pin_Load: {
      UChar sz = i->Pin.Load.sz;
      Bool syned = i->Pin.Load.syned;
      Bool idxd = toBool(i->Pin.Load.src->tag == Pam_RR);
      vex_printf("l%c%c%s ",
                 (sz==1) ? 'b' : (sz==2 ? 'h' : 'w'),
                 syned ? 'a' : 'z',
                 idxd ? "x" : "" );
      ppHRegPPC32(i->Pin.Load.dst);
      vex_printf(",");
      ppPPC32AMode(i->Pin.Load.src);
      return;
   }
   case Pin_Store: {
      UChar sz = i->Pin.Store.sz;
      Bool idxd = toBool(i->Pin.Store.dst->tag == Pam_RR);
      vex_printf("st%c%s ",
                 (sz==1) ? 'b' : (sz==2 ? 'h' : 'w'),
                 idxd ? "x" : "" );
      ppHRegPPC32(i->Pin.Store.src);
      vex_printf(",");
      ppPPC32AMode(i->Pin.Store.dst);
      return;
   }
   case Pin_Set32: {
      PPC32CondCode cc = i->Pin.Set32.cond;
      vex_printf("set32 (%s),", showPPC32CondCode(cc));
      ppHRegPPC32(i->Pin.Set32.dst);
      if (cc.test == Pct_ALWAYS) {
         vex_printf(": { li ");
         ppHRegPPC32(i->Pin.Set32.dst);
         vex_printf(",1 }");
      } else {
         vex_printf(": { mfcr r0 ; rlwinm ");
         ppHRegPPC32(i->Pin.Set32.dst);
         vex_printf(",r0,%u,31,31", cc.flag+1);
         if (cc.test == Pct_FALSE) {
            vex_printf("; xori ");
            ppHRegPPC32(i->Pin.Set32.dst);
            vex_printf(",");
            ppHRegPPC32(i->Pin.Set32.dst);
            vex_printf("1");
         }
         vex_printf(" }");
      }
      return;
   }
   case Pin_MfCR:
      vex_printf("mfcr ");
      ppHRegPPC32(i->Pin.MfCR.dst);
      break;
   case Pin_MFence:
      vex_printf("mfence (=sync)");
      return;

   case Pin_FpUnary:
      vex_printf("%s ", showPPC32FpOp(i->Pin.FpUnary.op));
      ppHRegPPC32(i->Pin.FpUnary.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpUnary.src);
      return;
   case Pin_FpBinary:
      vex_printf("%s ", showPPC32FpOp(i->Pin.FpBinary.op));
      ppHRegPPC32(i->Pin.FpBinary.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpBinary.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpBinary.srcR);
      return;
   case Pin_FpLdSt: {
      UChar sz = i->Pin.FpLdSt.sz;
      Bool idxd = toBool(i->Pin.FpLdSt.addr->tag == Pam_RR);
      if (i->Pin.FpLdSt.isLoad) {
         vex_printf("lf%c%s ",
                    (sz==4 ? 's' : 'd'),
                    idxd ? "x" : "" );
         ppHRegPPC32(i->Pin.FpLdSt.reg);
         vex_printf(",");
         ppPPC32AMode(i->Pin.FpLdSt.addr);
      } else {
         vex_printf("stf%c%s ",
                    (sz==4 ? 's' : 'd'),
                    idxd ? "x" : "" );
         ppHRegPPC32(i->Pin.FpLdSt.reg);
         vex_printf(",");
         ppPPC32AMode(i->Pin.FpLdSt.addr);
      }
      return;
   }
   case Pin_FpF64toF32:
      vex_printf("frsp ");
      ppHRegPPC32(i->Pin.FpF64toF32.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpF64toF32.src);
      return;
   case Pin_FpF64toI32:
      vex_printf("fctiw %%fr7,");
      ppHRegPPC32(i->Pin.FpF64toI32.src);
      vex_printf("; stfiwx %%fr7,%%r0,%%r1");
      vex_printf("; lwzx ");
      ppHRegPPC32(i->Pin.FpF64toI32.dst);
      vex_printf(",%%r0,%%r1");
      return;
   case Pin_FpCMov:
      vex_printf("fpcmov (%s) ", showPPC32CondCode(i->Pin.FpCMov.cond));
      ppHRegPPC32(i->Pin.FpCMov.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpCMov.src);
      vex_printf(": ");
      vex_printf("if (fr_dst != fr_src) { ");
      if (i->Pin.FpCMov.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) { ", showPPC32CondCode(i->Pin.FpCMov.cond));
      }
      vex_printf("fmr ");
      ppHRegPPC32(i->Pin.FpCMov.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpCMov.src);
      if (i->Pin.FpCMov.cond.test != Pct_ALWAYS)
         vex_printf(" }");
      vex_printf(" }");
      return;
   case Pin_FpLdFPSCR:
      vex_printf("mtfsf 0xFF,");
      ppHRegPPC32(i->Pin.FpLdFPSCR.src);
      return;
   case Pin_FpCmp:
      vex_printf("fcmpo %%cr1,");
      ppHRegPPC32(i->Pin.FpCmp.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpCmp.srcR);
      vex_printf("; mfcr ");
      ppHRegPPC32(i->Pin.FpCmp.dst);
      vex_printf("; rlwinm ");
      ppHRegPPC32(i->Pin.FpCmp.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.FpCmp.dst);
      vex_printf(",8,28,31");
      return;

   case Pin_RdWrLR:
      vex_printf("%s ", i->Pin.RdWrLR.wrLR ? "mtlr" : "mflr");
      ppHRegPPC32(i->Pin.RdWrLR.gpr);
      return;

   case Pin_AvLdSt: {
      UChar  sz = i->Pin.AvLdSt.sz;
      HChar* str_size;
      if (i->Pin.AvLdSt.addr->tag == Pam_IR) {
         ppLoadImm(hregPPC32_GPR30(), i->Pin.AvLdSt.addr->Pam.RR.index);
         vex_printf(" ; ");
      }
      str_size = sz==1 ? "eb" : sz==2 ? "eh" : sz==4 ? "ew" : "";
      if (i->Pin.AvLdSt.isLoad)
         vex_printf("lv%sx ", str_size);
      else
         vex_printf("stv%sx ", str_size);
      ppHRegPPC32(i->Pin.AvLdSt.reg);
      vex_printf(",");
      if (i->Pin.AvLdSt.addr->tag == Pam_IR)
         vex_printf("%%r30");
      else 
         ppHRegPPC32(i->Pin.AvLdSt.addr->Pam.RR.index);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvLdSt.addr->Pam.RR.base);
      return;
   }
   case Pin_AvUnary:
      vex_printf("%s ", showPPC32AvOp(i->Pin.AvUnary.op));
      ppHRegPPC32(i->Pin.AvUnary.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvUnary.src);
      return;
   case Pin_AvBinary:
      vex_printf("%s ", showPPC32AvOp(i->Pin.AvBinary.op));
      ppHRegPPC32(i->Pin.AvBinary.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBinary.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBinary.srcR);
      return;
   case Pin_AvBin8x16:
      vex_printf("%s(b) ", showPPC32AvOp(i->Pin.AvBin8x16.op));
      ppHRegPPC32(i->Pin.AvBin8x16.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin8x16.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin8x16.srcR);
      return;
   case Pin_AvBin16x8:
      vex_printf("%s(h) ", showPPC32AvOp(i->Pin.AvBin16x8.op));
      ppHRegPPC32(i->Pin.AvBin16x8.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin16x8.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin16x8.srcR);
      return;
   case Pin_AvBin32x4:
      vex_printf("%s(w) ", showPPC32AvOp(i->Pin.AvBin32x4.op));
      ppHRegPPC32(i->Pin.AvBin32x4.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin32x4.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin32x4.srcR);
      return;
   case Pin_AvBin32Fx4:
      vex_printf("%s ", showPPC32AvFpOp(i->Pin.AvBin32Fx4.op));
      ppHRegPPC32(i->Pin.AvBin32Fx4.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin32Fx4.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvBin32Fx4.srcR);
      return;
   case Pin_AvUn32Fx4:
      vex_printf("%s ", showPPC32AvFpOp(i->Pin.AvUn32Fx4.op));
      ppHRegPPC32(i->Pin.AvUn32Fx4.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvUn32Fx4.src);
      return;
   case Pin_AvPerm:
      vex_printf("vperm ");
      ppHRegPPC32(i->Pin.AvPerm.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvPerm.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvPerm.srcR);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvPerm.ctl);
      return;

   case Pin_AvSel:
      vex_printf("vsel ");
      ppHRegPPC32(i->Pin.AvSel.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvSel.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvSel.srcR);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvSel.ctl);
      return;

   case Pin_AvShlDbl:
      vex_printf("vsldoi ");
      ppHRegPPC32(i->Pin.AvShlDbl.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvShlDbl.srcL);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvShlDbl.srcR);
      vex_printf(",%d", i->Pin.AvShlDbl.shift);
      return;

   case Pin_AvSplat: {
      UChar sz = i->Pin.AvSplat.sz;
      UChar ch_sz = toUChar( (sz == 8) ? 'b' : (sz == 16) ? 'h' : 'w' );
      vex_printf("vsplt%s%c ",
                 i->Pin.AvSplat.src->tag == Pvi_Imm ? "is" : "", ch_sz);
      ppHRegPPC32(i->Pin.AvSplat.dst);
      vex_printf(",");
      ppPPC32VI5s(i->Pin.AvSplat.src);
      if (i->Pin.AvSplat.src->tag == Pvi_Reg)
         vex_printf(", %d", (128/sz)-1);   /* louis lane */
      return;
   }

   case Pin_AvCMov:
      vex_printf("avcmov (%s) ", showPPC32CondCode(i->Pin.AvCMov.cond));
      ppHRegPPC32(i->Pin.AvCMov.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvCMov.src);
      vex_printf(": ");
      vex_printf("if (v_dst != v_src) { ");
      if (i->Pin.AvCMov.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) { ", showPPC32CondCode(i->Pin.AvCMov.cond));
      }
      vex_printf("vmr ");
      ppHRegPPC32(i->Pin.AvCMov.dst);
      vex_printf(",");
      ppHRegPPC32(i->Pin.AvCMov.src);
      if (i->Pin.FpCMov.cond.test != Pct_ALWAYS)
         vex_printf(" }");
      vex_printf(" }");
      return;

   case Pin_AvLdVSCR:
      vex_printf("mtvscr ");
      ppHRegPPC32(i->Pin.AvLdVSCR.src);
      return;

   default:
      vex_printf("\nppPPC32Instr(ppc32): No such tag(%d)\n", (Int)i->tag);
      vpanic("ppPPC32Instr(ppc32)");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_PPC32Instr ( HRegUsage* u, PPC32Instr* i )
{
   initHRegUsage(u);
   switch (i->tag) {
   case Pin_LI32:
      addHRegUse(u, HRmWrite, i->Pin.LI32.dst);
      break;
   case Pin_Alu32:
      addHRegUse(u, HRmRead, i->Pin.Alu32.srcL);
      addRegUsage_PPC32RH(u, i->Pin.Alu32.srcR);
      addHRegUse(u, HRmWrite, i->Pin.Alu32.dst);
      return;
   case Pin_AddSubC32:
      addHRegUse(u, HRmWrite, i->Pin.AddSubC32.dst);
      addHRegUse(u, HRmRead, i->Pin.AddSubC32.srcL);
      addHRegUse(u, HRmRead, i->Pin.AddSubC32.srcR);
      return;
   case Pin_Cmp32:
      addHRegUse(u, HRmRead, i->Pin.Cmp32.srcL);
      addRegUsage_PPC32RH(u, i->Pin.Cmp32.srcR);
      return;
   case Pin_Unary32:
      addHRegUse(u, HRmWrite, i->Pin.Unary32.dst);
      addHRegUse(u, HRmRead, i->Pin.Unary32.src);
      return;
   case Pin_MulL:
      addHRegUse(u, HRmWrite, i->Pin.MulL.dst);
      addHRegUse(u, HRmRead, i->Pin.MulL.srcL);
      addHRegUse(u, HRmRead, i->Pin.MulL.srcR);
      return;
   case Pin_Div:
      addHRegUse(u, HRmWrite, i->Pin.Div.dst);
      addHRegUse(u, HRmRead, i->Pin.Div.srcL);
      addHRegUse(u, HRmRead, i->Pin.Div.srcR);
      return;
   case Pin_Call:
      /* This is a bit subtle. */
      /* First off, claim it trashes all the caller-saved regs
         which fall within the register allocator's jurisdiction.
         These I believe to be: r3 to r12.
      */
      addHRegUse(u, HRmWrite, hregPPC32_GPR3());
      addHRegUse(u, HRmWrite, hregPPC32_GPR4());
      addHRegUse(u, HRmWrite, hregPPC32_GPR5());
      addHRegUse(u, HRmWrite, hregPPC32_GPR6());
      addHRegUse(u, HRmWrite, hregPPC32_GPR7());
      addHRegUse(u, HRmWrite, hregPPC32_GPR8());
      addHRegUse(u, HRmWrite, hregPPC32_GPR9());
      addHRegUse(u, HRmWrite, hregPPC32_GPR10());
      addHRegUse(u, HRmWrite, hregPPC32_GPR11());
      addHRegUse(u, HRmWrite, hregPPC32_GPR12());
      
      /* Now we have to state any parameter-carrying registers
         which might be read.  This depends on the argiregs field. */
      if (i->Pin.Call.argiregs & (1<<10)) addHRegUse(u, HRmRead, hregPPC32_GPR10());
      if (i->Pin.Call.argiregs & (1<<9)) addHRegUse(u, HRmRead, hregPPC32_GPR9());
      if (i->Pin.Call.argiregs & (1<<8)) addHRegUse(u, HRmRead, hregPPC32_GPR8());
      if (i->Pin.Call.argiregs & (1<<7)) addHRegUse(u, HRmRead, hregPPC32_GPR7());
      if (i->Pin.Call.argiregs & (1<<6)) addHRegUse(u, HRmRead, hregPPC32_GPR6());
      if (i->Pin.Call.argiregs & (1<<5)) addHRegUse(u, HRmRead, hregPPC32_GPR5());
      if (i->Pin.Call.argiregs & (1<<4)) addHRegUse(u, HRmRead, hregPPC32_GPR4());
      if (i->Pin.Call.argiregs & (1<<3)) addHRegUse(u, HRmRead, hregPPC32_GPR3());

      vassert(0 == (i->Pin.Call.argiregs
                    & ~((1<<3)|(1<<4)|(1<<5)|(1<<6)
                        |(1<<7)|(1<<8)|(1<<9)|(1<<10))));

      /* Finally, there is the issue that the insn trashes a
         register because the literal target address has to be
         loaded into a register.  %r12 seems a suitable victim.
         (Can't use %r0, as use ops that interpret it as value zero).  */
      addHRegUse(u, HRmWrite, hregPPC32_GPR12());
      /* Upshot of this is that the assembler really must use %r12,
         and no other, as a destination temporary. */
      return;
   case Pin_Goto:
      addRegUsage_PPC32RI(u, i->Pin.Goto.dst);
      /* GPR3 holds destination address from Pin_Goto */
      addHRegUse(u, HRmWrite, hregPPC32_GPR3());
      if (i->Pin.Goto.jk != Ijk_Boring)
         addHRegUse(u, HRmWrite, GuestStatePtr);
      return;
   case Pin_CMov32:
      addRegUsage_PPC32RI(u, i->Pin.CMov32.src);
      addHRegUse(u, HRmWrite, i->Pin.CMov32.dst);
      return;
   case Pin_Load:
      addRegUsage_PPC32AMode(u, i->Pin.Load.src);
      addHRegUse(u, HRmWrite, i->Pin.Load.dst);
      return;
   case Pin_Store:
      addHRegUse(u, HRmRead, i->Pin.Store.src);
      addRegUsage_PPC32AMode(u, i->Pin.Store.dst);
      return;
   case Pin_Set32:
      addHRegUse(u, HRmWrite, i->Pin.Set32.dst);
      return;
   case Pin_MfCR:
      addHRegUse(u, HRmWrite, i->Pin.MfCR.dst);
      return;
   case Pin_MFence:
      return;

   case Pin_FpUnary:
      addHRegUse(u, HRmWrite, i->Pin.FpUnary.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpUnary.src);
      return;
   case Pin_FpBinary:
      addHRegUse(u, HRmWrite, i->Pin.FpBinary.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpBinary.srcL);
      addHRegUse(u, HRmRead,  i->Pin.FpBinary.srcR);
      return;
   case Pin_FpLdSt:
      addHRegUse(u, (i->Pin.FpLdSt.isLoad ? HRmWrite : HRmRead),
                 i->Pin.FpLdSt.reg);
      addRegUsage_PPC32AMode(u, i->Pin.FpLdSt.addr);
      return;
   case Pin_FpF64toF32:
      addHRegUse(u, HRmWrite, i->Pin.FpF64toF32.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpF64toF32.src);
      return;
   case Pin_FpF64toI32:
      addHRegUse(u, HRmWrite,  i->Pin.FpF64toI32.dst);
      addHRegUse(u, HRmWrite, hregPPC32_FPR7());
      addHRegUse(u, HRmRead,   i->Pin.FpF64toI32.src);
      return;
   case Pin_FpCMov:
      addHRegUse(u, HRmModify, i->Pin.FpCMov.dst);
      addHRegUse(u, HRmRead, i->Pin.FpCMov.src);
      return;
   case Pin_FpLdFPSCR:
      addHRegUse(u, HRmRead, i->Pin.FpLdFPSCR.src);
      return;
   case Pin_FpCmp:
      addHRegUse(u, HRmWrite, i->Pin.FpCmp.dst);
      addHRegUse(u, HRmRead,   i->Pin.FpCmp.srcL);
      addHRegUse(u, HRmRead,   i->Pin.FpCmp.srcR);
      return;

   case Pin_RdWrLR:
      addHRegUse(u, (i->Pin.RdWrLR.wrLR ? HRmRead : HRmWrite),
                 i->Pin.RdWrLR.gpr);
      return;

   case Pin_AvLdSt:
      addHRegUse(u, (i->Pin.AvLdSt.isLoad ? HRmWrite : HRmRead),
                 i->Pin.AvLdSt.reg);
      if (i->Pin.AvLdSt.addr->tag == Pam_IR)
         addHRegUse(u, HRmWrite, hregPPC32_GPR30());
      addRegUsage_PPC32AMode(u, i->Pin.AvLdSt.addr);
      return;
   case Pin_AvUnary:
      addHRegUse(u, HRmWrite, i->Pin.AvUnary.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvUnary.src);
      return;
   case Pin_AvBinary:
      if (i->Pin.AvBinary.op == Pav_XOR
          && i->Pin.AvBinary.dst == i->Pin.AvBinary.srcL
          && i->Pin.AvBinary.dst == i->Pin.AvBinary.srcR) {
         /* reg-alloc needs to understand 'xor r,r,r' as a write of r */
         /* (as opposed to a rite of passage :-) */
         addHRegUse(u, HRmWrite, i->Pin.AvBinary.dst);
      } else {
         addHRegUse(u, HRmWrite, i->Pin.AvBinary.dst);
         addHRegUse(u, HRmRead,  i->Pin.AvBinary.srcL);
         addHRegUse(u, HRmRead,  i->Pin.AvBinary.srcR);
      }
      return;
   case Pin_AvBin8x16:
      addHRegUse(u, HRmWrite, i->Pin.AvBin8x16.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin8x16.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin8x16.srcR);
      return;
   case Pin_AvBin16x8:
      addHRegUse(u, HRmWrite, i->Pin.AvBin16x8.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin16x8.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin16x8.srcR);
      return;
   case Pin_AvBin32x4:
      addHRegUse(u, HRmWrite, i->Pin.AvBin32x4.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32x4.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32x4.srcR);
      return;
   case Pin_AvBin32Fx4:
      addHRegUse(u, HRmWrite, i->Pin.AvBin32Fx4.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32Fx4.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32Fx4.srcR);
      if (i->Pin.AvBin32Fx4.op == Pavfp_MULF)
         addHRegUse(u, HRmWrite, hregPPC32_GPR29());
      return;
   case Pin_AvUn32Fx4:
      addHRegUse(u, HRmWrite, i->Pin.AvUn32Fx4.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvUn32Fx4.src);
      return;
   case Pin_AvPerm:
      addHRegUse(u, HRmWrite, i->Pin.AvPerm.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvPerm.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvPerm.srcR);
      addHRegUse(u, HRmRead,  i->Pin.AvPerm.ctl);
      return;
   case Pin_AvSel:
      addHRegUse(u, HRmWrite, i->Pin.AvSel.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvSel.ctl);
      addHRegUse(u, HRmRead,  i->Pin.AvSel.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvSel.srcR);
      return;
   case Pin_AvShlDbl:
      addHRegUse(u, HRmWrite, i->Pin.AvShlDbl.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvShlDbl.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvShlDbl.srcR);
      return;
   case Pin_AvSplat:
      addHRegUse(u, HRmWrite,  i->Pin.AvSplat.dst);
      addRegUsage_PPC32VI5s(u, i->Pin.AvSplat.src);
      return;
   case Pin_AvCMov:
      addHRegUse(u, HRmModify, i->Pin.AvCMov.dst);
      addHRegUse(u, HRmRead, i->Pin.AvCMov.src);
      return;
   case Pin_AvLdVSCR:
      addHRegUse(u, HRmRead, i->Pin.AvLdVSCR.src);
      return;

   default:
      ppPPC32Instr(i);
      vpanic("getRegUsage_PPC32Instr");
   }
}

/* local helper */
static void mapReg(HRegRemap* m, HReg* r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_PPC32Instr (HRegRemap* m, PPC32Instr* i)
{
   switch (i->tag) {
   case Pin_LI32:
      mapReg(m, &i->Pin.LI32.dst);
      return;
   case Pin_Alu32:
      mapReg(m, &i->Pin.Alu32.dst);
      mapReg(m, &i->Pin.Alu32.srcL);
      mapRegs_PPC32RH(m, i->Pin.Alu32.srcR);
      return;
   case Pin_AddSubC32:
      mapReg(m, &i->Pin.AddSubC32.dst);
      mapReg(m, &i->Pin.AddSubC32.srcL);
      mapReg(m, &i->Pin.AddSubC32.srcR);
      return;
   case Pin_Cmp32:
      mapReg(m, &i->Pin.Cmp32.srcL);
      mapRegs_PPC32RH(m, i->Pin.Cmp32.srcR);
      return;
   case Pin_Unary32:
      mapReg(m, &i->Pin.Unary32.dst);
      mapReg(m, &i->Pin.Unary32.src);
      return;
   case Pin_MulL:
      mapReg(m, &i->Pin.MulL.dst);
      mapReg(m, &i->Pin.MulL.srcL);
      mapReg(m, &i->Pin.MulL.srcR);
      return;
   case Pin_Div:
      mapReg(m, &i->Pin.Div.dst);
      mapReg(m, &i->Pin.Div.srcL);
      mapReg(m, &i->Pin.Div.srcR);
      return;
   case Pin_Call:
      return;
   case Pin_Goto:
      mapRegs_PPC32RI(m, i->Pin.Goto.dst);
      return;
   case Pin_CMov32:
      mapRegs_PPC32RI(m, i->Pin.CMov32.src);
      mapReg(m, &i->Pin.CMov32.dst);
      return;
   case Pin_Load:
      mapRegs_PPC32AMode(m, i->Pin.Load.src);
      mapReg(m, &i->Pin.Load.dst);
      return;
   case Pin_Store:
      mapReg(m, &i->Pin.Store.src);
      mapRegs_PPC32AMode(m, i->Pin.Store.dst);
      return;
   case Pin_Set32:
      mapReg(m, &i->Pin.Set32.dst);
      return;
   case Pin_MfCR:
      mapReg(m, &i->Pin.MfCR.dst);
      return;
   case Pin_MFence:
      return;
   case Pin_FpUnary:
      mapReg(m, &i->Pin.FpUnary.dst);
      mapReg(m, &i->Pin.FpUnary.src);
      return;
   case Pin_FpBinary:
      mapReg(m, &i->Pin.FpBinary.dst);
      mapReg(m, &i->Pin.FpBinary.srcL);
      mapReg(m, &i->Pin.FpBinary.srcR);
      return;
   case Pin_FpLdSt:
      mapReg(m, &i->Pin.FpLdSt.reg);
      mapRegs_PPC32AMode(m, i->Pin.FpLdSt.addr);
      return;
   case Pin_FpF64toF32:
      mapReg(m, &i->Pin.FpF64toF32.dst);
      mapReg(m, &i->Pin.FpF64toF32.src);
      return;
   case Pin_FpF64toI32:
      mapReg(m, &i->Pin.FpF64toI32.dst);
      mapReg(m, &i->Pin.FpF64toI32.src);
      return;
   case Pin_FpCMov:
      mapReg(m, &i->Pin.FpCMov.dst);
      mapReg(m, &i->Pin.FpCMov.src);
      return;
   case Pin_FpLdFPSCR:
      mapReg(m, &i->Pin.FpLdFPSCR.src);
      return;
   case Pin_FpCmp:
      mapReg(m, &i->Pin.FpCmp.dst);
      mapReg(m, &i->Pin.FpCmp.srcL);
      mapReg(m, &i->Pin.FpCmp.srcR);
      return;
   case Pin_RdWrLR:
      mapReg(m, &i->Pin.RdWrLR.gpr);
      return;
   case Pin_AvLdSt:
      mapReg(m, &i->Pin.AvLdSt.reg);
      mapRegs_PPC32AMode(m, i->Pin.AvLdSt.addr);
      return;
   case Pin_AvUnary:
      mapReg(m, &i->Pin.AvUnary.dst);
      mapReg(m, &i->Pin.AvUnary.src);
      return;
   case Pin_AvBinary:
      mapReg(m, &i->Pin.AvBinary.dst);
      mapReg(m, &i->Pin.AvBinary.srcL);
      mapReg(m, &i->Pin.AvBinary.srcR);
      return;
   case Pin_AvBin8x16:
      mapReg(m, &i->Pin.AvBin8x16.dst);
      mapReg(m, &i->Pin.AvBin8x16.srcL);
      mapReg(m, &i->Pin.AvBin8x16.srcR);
      return;
   case Pin_AvBin16x8:
      mapReg(m, &i->Pin.AvBin16x8.dst);
      mapReg(m, &i->Pin.AvBin16x8.srcL);
      mapReg(m, &i->Pin.AvBin16x8.srcR);
      return;
   case Pin_AvBin32x4:
      mapReg(m, &i->Pin.AvBin32x4.dst);
      mapReg(m, &i->Pin.AvBin32x4.srcL);
      mapReg(m, &i->Pin.AvBin32x4.srcR);
      return;
   case Pin_AvBin32Fx4:
      mapReg(m, &i->Pin.AvBin32Fx4.dst);
      mapReg(m, &i->Pin.AvBin32Fx4.srcL);
      mapReg(m, &i->Pin.AvBin32Fx4.srcR);
      return;
   case Pin_AvUn32Fx4:
      mapReg(m, &i->Pin.AvUn32Fx4.dst);
      mapReg(m, &i->Pin.AvUn32Fx4.src);
      return;
   case Pin_AvPerm:
      mapReg(m, &i->Pin.AvPerm.dst);
      mapReg(m, &i->Pin.AvPerm.srcL);
      mapReg(m, &i->Pin.AvPerm.srcR);
      mapReg(m, &i->Pin.AvPerm.ctl);
      return;
   case Pin_AvSel:
      mapReg(m, &i->Pin.AvSel.dst);
      mapReg(m, &i->Pin.AvSel.srcL);
      mapReg(m, &i->Pin.AvSel.srcR);
      mapReg(m, &i->Pin.AvSel.ctl);
      return;
   case Pin_AvShlDbl:
      mapReg(m, &i->Pin.AvShlDbl.dst);
      mapReg(m, &i->Pin.AvShlDbl.srcL);
      mapReg(m, &i->Pin.AvShlDbl.srcR);
      return;
   case Pin_AvSplat:
      mapReg(m, &i->Pin.AvSplat.dst);
      mapRegs_PPC32VI5s(m, i->Pin.AvSplat.src);
      return;
   case Pin_AvCMov:
     mapReg(m, &i->Pin.AvCMov.dst);
     mapReg(m, &i->Pin.AvCMov.src);
     return;
   case Pin_AvLdVSCR:
      mapReg(m, &i->Pin.AvLdVSCR.src);
      return;

   default:
      ppPPC32Instr(i);
      vpanic("mapRegs_PPC32Instr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_PPC32Instr ( PPC32Instr* i, HReg* src, HReg* dst )
{
   /* Moves between integer regs */
   if (i->tag == Pin_Alu32) {
      // or Rd,Rs,Rs == mr Rd,Rs
      if (i->Pin.Alu32.op != Palu_OR)
         return False;
      if (i->Pin.Alu32.srcR->tag != Prh_Reg)
         return False;
      if (i->Pin.Alu32.srcR->Prh.Reg.reg != i->Pin.Alu32.srcL)
         return False;
      *src = i->Pin.Alu32.srcL;
      *dst = i->Pin.Alu32.dst;
      return True;
   }
   /* Moves between FP regs */
   if (i->tag == Pin_FpUnary) {
      if (i->Pin.FpUnary.op != Pfp_MOV)
         return False;
      *src = i->Pin.FpUnary.src;
      *dst = i->Pin.FpUnary.dst;
      return True;
   }
   return False;
}


/* Generate ppc32 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */
PPC32Instr* genSpill_PPC32 ( HReg rreg, UShort offsetB )
{
   PPC32AMode* am;
   vassert(!hregIsVirtual(rreg));
   am = PPC32AMode_IR(offsetB, GuestStatePtr);
   
   switch (hregClass(rreg)) {
   case HRcInt32:
      return PPC32Instr_Store( 4, am, rreg);
   case HRcFlt64:
      return PPC32Instr_FpLdSt ( False/*store*/, 8, rreg, am );
   case HRcVec128:
      // XXX: GPR30 used as spill register to kludge AltiVec AMode_IR
      return PPC32Instr_AvLdSt ( False/*store*/, 16, rreg, am );
   default: 
      ppHRegClass(hregClass(rreg));
      vpanic("genSpill_PPC32: unimplemented regclass");
   }
}

PPC32Instr* genReload_PPC32 ( HReg rreg, UShort offsetB )
{
   PPC32AMode* am;
   vassert(!hregIsVirtual(rreg));
   am = PPC32AMode_IR(offsetB, GuestStatePtr);

   switch (hregClass(rreg)) {
   case HRcInt32:
      return PPC32Instr_Load( 4, False, rreg, am );
   case HRcFlt64:
      return PPC32Instr_FpLdSt ( True/*load*/, 8, rreg, am );
   case HRcVec128:
      // XXX: GPR30 used as spill register to kludge AltiVec AMode_IR
      return PPC32Instr_AvLdSt ( True/*load*/, 16, rreg, am );
   default: 
      ppHRegClass(hregClass(rreg));
      vpanic("genReload_PPC32: unimplemented regclass");
   }
}


/* --------- The ppc32 assembler (bleh.) --------- */

static UInt iregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 32);
   return n;
}

static UInt fregNo ( HReg fr )
{
   UInt n;
   vassert(hregClass(fr) == HRcFlt64);
   vassert(!hregIsVirtual(fr));
   n = hregNumber(fr);
   vassert(n <= 32);
   return n;
}

static UInt vregNo ( HReg v )
{
   UInt n;
   vassert(hregClass(v) == HRcVec128);
   vassert(!hregIsVirtual(v));
   n = hregNumber(v);
   vassert(n <= 32);
   return n;
}

/* Emit 32bit instruction big-endianly */
static UChar* emit32 ( UChar* p, UInt w32 )
{
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >>  8) & 0x000000FF);
   *p++ = toUChar((w32)       & 0x000000FF);
   return p;
}

/* The following mkForm[...] functions refer to PPC32 instruction forms
   as per PPC32 p576
 */

static UChar* mkFormD ( UChar* p, UInt opc1, UInt r1, UInt r2, UInt imm )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1  < 0x20);
   vassert(r2  < 0x20);
   imm = imm & 0xFFFF;
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (imm));
   return emit32(p, theInstr);
}

static UChar* mkFormX ( UChar* p, UInt opc1, UInt r1, UInt r2,
                        UInt r3, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1  < 0x20);
   vassert(r2  < 0x20);
   vassert(r3  < 0x20);
   vassert(opc2 < 0x400);
   vassert(b0  < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

static UChar* mkFormXO ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt r3, UInt b10, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1  < 0x20);
   vassert(r2  < 0x20);
   vassert(r3  < 0x20);
   vassert(b10 < 0x2);
   vassert(opc2 < 0x200);
   vassert(b0  < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (b10 << 10) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

static UChar* mkFormXL ( UChar* p, UInt opc1, UInt f1, UInt f2,
                         UInt f3, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(f1  < 0x20);
   vassert(f2  < 0x20);
   vassert(f3  < 0x20);
   vassert(opc2 < 0x400);
   vassert(b0  < 0x2);
   theInstr = ((opc1<<26) | (f1<<21) | (f2<<16) | (f3<<11) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

// Note: for split field ops, give mnemonic arg
static UChar* mkFormXFX ( UChar* p, UInt r1, UInt f2, UInt opc2 )
{
   UInt theInstr;
   vassert(r1  < 0x20);
   vassert(f2  < 0x20);
   vassert(opc2 < 0x400);
   switch (opc2) {
   case 144:  // mtcrf
      vassert(f2 < 0x100);
      f2 = f2 << 1;
      break;
   case 339:  // mfspr
   case 371:  // mftb
   case 467:  // mtspr
      vassert(f2 < 0x400);
      f2 = ((f2>>5) & 0x1F) | ((f2 & 0x1F)<<5);  // re-arrange split field
      break;
   default: vpanic("mkFormXFX(PPC32)");
   }
   theInstr = ((31<<26) | (r1<<21) | (f2<<11) | (opc2<<1));
   return emit32(p, theInstr);
}

// Only used by mtfsf
static UChar* mkFormXFL ( UChar* p, UInt FM, UInt freg )
{
   UInt theInstr;
   vassert(FM   < 0x100);
   vassert(freg < 0x20);
   theInstr = ((63<<26) | (FM<<17) | (freg<<11) | (711<<1));
   return emit32(p, theInstr);
}

#if 0
// 'b'
static UChar* mkFormI ( UChar* p, UInt LI, UInt AA, UInt LK )
{
   UInt theInstr;
   vassert(LI  < 0x1000000);
   vassert(AA  < 0x2);
   vassert(LK  < 0x2);
   theInstr = ((18<<26) | (LI<<2) | (AA<<1) | (LK));
   return emit32(p, theInstr);
}
#endif

// 'bc'
static UChar* mkFormB ( UChar* p, UInt BO, UInt BI,
                        UInt BD, UInt AA, UInt LK )
{
   UInt theInstr;
   vassert(BO  < 0x20);
   vassert(BI  < 0x20);
   vassert(BD  < 0x4000);
   vassert(AA  < 0x2);
   vassert(LK  < 0x2);
   theInstr = ((16<<26) | (BO<<21) | (BI<<16) | (BD<<2) | (AA<<1) | (LK));
   return emit32(p, theInstr);
}

// rotates
static UChar* mkFormM ( UChar* p, UInt opc1, UInt r1, UInt r2,
                        UInt f3, UInt MB, UInt ME, UInt Rc )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1  < 0x20);
   vassert(r2  < 0x20);
   vassert(f3  < 0x20);
   vassert(MB  < 0x20);
   vassert(ME  < 0x20);
   vassert(Rc  < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (f3<<11) | (MB<<6) | (ME<<1) | (Rc));
   return emit32(p, theInstr);
}

static UChar* mkFormA ( UChar* p, UInt opc1, UInt r1, UInt r2,
                        UInt r3, UInt r4, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(r4   < 0x20);
   vassert(opc2 < 0x20);
   vassert(b0   < 0x2 );
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) |
               (r4<<6) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

static UChar* doAMode_IR ( UChar* p, UInt opc1, UInt rSD, PPC32AMode* am )
{
   UInt rA, idx;
   vassert(am->tag == Pam_IR);
   vassert(am->Pam.IR.index < 0x10000);

   rA  = iregNo(am->Pam.IR.base);
   idx = am->Pam.IR.index;

   p = mkFormD(p, opc1, rSD, rA, idx);
   return p;
}


static UChar* doAMode_RR ( UChar* p, UInt opc1, UInt opc2,
                           UInt rSD, PPC32AMode* am )
{
   UInt rA, rB;
   vassert(am->tag == Pam_RR);

   rA  = iregNo(am->Pam.RR.base);
   rB  = iregNo(am->Pam.RR.index);
   
   p = mkFormX(p, opc1, rSD, rA, rB, opc2, 0);
   return p;
}

/* Load imm to r_dst */
static UChar* mkLoadImm ( UChar* p, UInt r_dst, UInt imm )
{
   vassert(r_dst < 0x20);

   if (imm >= 0xFFFF8000 || imm <= 0x7FFF) { // sign-extendable from 16 bits?
      // addi r_dst,0,imm  => li r_dst,imm
      p = mkFormD(p, 14, r_dst, 0, imm & 0xFFFF);
   } else {
      // addis r_dst,r0,(imm>>16) => lis r_dst, (imm>>16)
      p = mkFormD(p, 15, r_dst, 0, (imm>>16) & 0xFFFF);
      // ori r_dst, r_dst, (imm & 0xFFFF)
      p = mkFormD(p, 24, r_dst, r_dst, imm & 0xFFFF);
   }
   return p;
}

/* Move r_dst to r_src */
static UChar* mkMoveReg ( UChar* p, UInt r_dst, UInt r_src )
{
   vassert(r_dst < 0x20);
   vassert(r_src < 0x20);

   if (r_dst != r_src) {
      /* or r_dst, r_src, r_src */
      p = mkFormX(p, 31, r_src, r_dst, r_src, 444, 0 );
   }
   return p;
}

static UChar* mkFormVX ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt r3, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(opc2 < 0x800);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) | opc2);
   return emit32(p, theInstr);
}

static UChar* mkFormVXR ( UChar* p, UInt opc1, UInt r1, UInt r2,
                          UInt r3, UInt Rc, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(Rc   < 0x2);
   vassert(opc2 < 0x400);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) | (Rc<<10) | opc2);
   return emit32(p, theInstr);
}

static UChar* mkFormVA ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt r3, UInt r4, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(r4   < 0x20);
   vassert(opc2 < 0x40);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) | (r4<<6) | opc2);
   return emit32(p, theInstr);
}



/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */

Int emit_PPC32Instr ( UChar* buf, Int nbuf, PPC32Instr* i )
{
   UChar* p = &buf[0];
   UChar* ptmp = p;
   vassert(nbuf >= 32);

//   vex_printf("asm  ");ppPPC32Instr(i); vex_printf("\n");

   switch (i->tag) {

   case Pin_LI32:
      p = mkLoadImm(p, iregNo(i->Pin.LI32.dst), i->Pin.LI32.imm32);
      goto done;

   case Pin_Alu32: {
      PPC32RH* srcR   = i->Pin.Alu32.srcR;
      Bool     immR   = toBool(srcR->tag == Prh_Imm);
      UInt     r_dst  = iregNo(i->Pin.Alu32.dst);
      UInt     r_srcL = iregNo(i->Pin.Alu32.srcL);
      UInt     r_srcR = immR ? (-1)/*bogus*/ : iregNo(srcR->Prh.Reg.reg);

      switch (i->Pin.Alu32.op) {

      case Palu_ADD:
         if (immR) {
            /* addi (PPC32 p350) */
            vassert(srcR->Prh.Imm.syned);
            vassert(srcR->Prh.Imm.imm16 != 0x8000);
            p = mkFormD(p, 14, r_dst, r_srcL, srcR->Prh.Imm.imm16);
         } else {
            /* add (PPC32 p347) */
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 266, 0);
         }
         break;

      case Palu_SUB:
         if (immR) {
            /* addi (PPC32 p350), but with negated imm */
            vassert(srcR->Prh.Imm.syned);
            vassert(srcR->Prh.Imm.imm16 != 0x8000);
            p = mkFormD(p, 14, r_dst, r_srcL, (- srcR->Prh.Imm.imm16));
         } else {
            /* subf (PPC32 p537), with args the "wrong" way round */
            p = mkFormXO(p, 31, r_dst, r_srcR, r_srcL, 0, 40, 0);
         }
         break;

      case Palu_AND:
         if (immR) {
            /* andi. (PPC32 p358) */
            vassert(!srcR->Prh.Imm.syned);
            p = mkFormD(p, 28, r_srcL, r_dst, srcR->Prh.Imm.imm16);
         } else {
            /* and (PPC32 p356) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 28, 0);
         }
         break;

      case Palu_OR:
         if (immR) {
            /* ori (PPC32 p497) */
            vassert(!srcR->Prh.Imm.syned);
            p = mkFormD(p, 24, r_srcL, r_dst, srcR->Prh.Imm.imm16);
         } else {
            /* or (PPC32 p495) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 444, 0);
         }
         break;

      case Palu_XOR:
         if (immR) {
            /* xori (PPC32 p550) */
            vassert(!srcR->Prh.Imm.syned);
            p = mkFormD(p, 26, r_srcL, r_dst, srcR->Prh.Imm.imm16);
         } else {
            /* xor (PPC32 p549) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 316, 0);
         }
         break;

      case Palu_SHL:
         if (immR) {
            /* rd = rs << n, 1 <= n <= 31
               is
               rlwinm rd,rs,n,0,31-n  (PPC32 p501)
            */
            UInt n = srcR->Prh.Imm.imm16;
            vassert(!srcR->Prh.Imm.syned);
	    vassert(n > 0 && n < 32);
            p = mkFormM(p, 21, r_srcL, r_dst, n, 0, 31-n, 0);
         } else {
            /* slw (PPC32 p505) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 24, 0);
         }
         break;

      case Palu_SHR:
         if (immR) {
            /* rd = rs >>u n, 1 <= n <= 31
               is
               rlwinm rd,rs,32-n,n,31  (PPC32 p501)
            */
            UInt n = srcR->Prh.Imm.imm16;
            vassert(!srcR->Prh.Imm.syned);
	    vassert(n > 0 && n < 32);
            p = mkFormM(p, 21, r_srcL, r_dst, 32-n, n, 31, 0);
         } else {
            /* srw (PPC32 p508) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 536, 0);
         }
         break;

      case Palu_SAR:
         if (immR) {
            /* srawi (PPC32 p507) */
            UInt n = srcR->Prh.Imm.imm16;
            vassert(!srcR->Prh.Imm.syned);
	    vassert(n > 0 && n < 32);
            p = mkFormX(p, 31, r_srcL, r_dst, n, 824, 0);
         } else {
            /* sraw (PPC32 p506) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 792, 0);
         }
         break;

      default:
         goto bad;
      }
      goto done;
   }

   case Pin_AddSubC32: {
      Bool isAdd    = i->Pin.AddSubC32.isAdd;
      Bool setC = i->Pin.AddSubC32.setC;
      UInt r_srcL   = iregNo(i->Pin.AddSubC32.srcL);
      UInt r_srcR   = iregNo(i->Pin.AddSubC32.srcR);
      UInt r_dst    = iregNo(i->Pin.AddSubC32.dst);
      
      if (isAdd) {
         if (setC) /* addc (PPC32 p348) */
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 10, 0);
         else          /* adde (PPC32 p349) */
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 138, 0);
      } else {
         /* subfX, with args the "wrong" way round */
         if (setC) /* subfc (PPC32 p538) */
            p = mkFormXO(p, 31, r_dst, r_srcR, r_srcL, 0, 8, 0);
         else          /* subfe (PPC32 p539) */
            p = mkFormXO(p, 31, r_dst, r_srcR, r_srcL, 0, 136, 0);
      }
      goto done;
   }

   case Pin_Cmp32: {
      Bool syned  = i->Pin.Cmp32.syned;
      UInt fld1   = i->Pin.Cmp32.crfD << 2;
      UInt r_srcL = iregNo(i->Pin.Cmp32.srcL);
      UInt r_srcR, imm_srcR;
      PPC32RH* srcR = i->Pin.Cmp32.srcR;

      switch (srcR->tag) {
      case Prh_Imm:
         /* cmpi  (signed)   (PPC32 p368)  or 
            cmpli (unsigned) (PPC32 p370) */
         imm_srcR = srcR->Prh.Imm.imm16;
         if (syned) {
            vassert(srcR->Prh.Imm.syned);
            vassert(imm_srcR != 0x8000);
         } else {
            vassert(!srcR->Prh.Imm.syned);
         }
         p = mkFormD(p, syned ? 11 : 10, fld1, r_srcL, imm_srcR);
         break;
      case Prh_Reg:
         /* cmpi  (signed)   (PPC32 p367)  or 
            cmpli (unsigned) (PPC32 p379) */
         r_srcR = iregNo(srcR->Prh.Reg.reg);
         p = mkFormX(p, 31, fld1, r_srcL, r_srcR, syned ? 0 : 32, 0);
         break;
      default: 
         goto bad;
      }        
      goto done;
   }

   case Pin_Unary32: {
      UInt r_dst = iregNo(i->Pin.Unary32.dst);
      UInt r_src = iregNo(i->Pin.Unary32.src);

      switch (i->Pin.Unary32.op) {
      case Pun_NOT:  // nor r_dst,r_src,r_src
         p = mkFormX(p, 31, r_src, r_dst, r_src, 124, 0);
         break;
      case Pun_NEG:  // neg r_dst,r_src
         p = mkFormXO(p, 31, r_dst, r_src, 0, 0, 104, 0);
         break;
      case Pun_CLZ:  // cntlzw r_dst, r_src
         p = mkFormX(p, 31, r_src, r_dst, 0, 26, 0);
         break;
      default: goto bad;
      }
      goto done;
   }

   case Pin_MulL: {
      Bool syned  = i->Pin.MulL.syned;
      UInt r_dst  = iregNo(i->Pin.MulL.dst);
      UInt r_srcL = iregNo(i->Pin.MulL.srcL);
      UInt r_srcR = iregNo(i->Pin.MulL.srcR);

      if (i->Pin.MulL.hi32) {
         // mul hi words, must consider sign
         if (syned) {
            // mulhw r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 75, 0);
         } else {
            // mulhwu r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 11, 0);
         }
      } else {
         // mul low word, sign is irrelevant
         vassert(!i->Pin.MulL.syned);
         // mullw r_dst,r_srcL,r_srcR
         p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 235, 0);
      }
      goto done;
   }

   case Pin_Div: {
      Bool syned  = i->Pin.MulL.syned;
      UInt r_dst  = iregNo(i->Pin.Div.dst);
      UInt r_srcL = iregNo(i->Pin.Div.srcL);
      UInt r_srcR = iregNo(i->Pin.Div.srcR);

      if (syned == True) {
         // divw r_dst,r_srcL,r_srcR
         p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 491, 0);
      } else {
         // divwu r_dst,r_srcL,r_srcR
         p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 459, 0);
      }
      goto done;
   }

   case Pin_Call: {
      PPC32CondCode cond = i->Pin.Call.cond;
      UInt r_dst = 12;
      /* As per detailed comment for Pin_Call in
         getRegUsage_PPC32Instr above, %r12 is used as an address temp */

      /* jump over the following insns if condition does not hold */
      if (cond.test != Pct_ALWAYS) {
         /* jmp fwds if !condition */
         /* don't know how many bytes to jump over yet...
            make space for a jump instruction and fill in later. */
         ptmp = p; /* fill in this bit later */
         p += 4;                                               // p += 4
      }

      /* load target to r_dst */
      p = mkLoadImm(p, r_dst, i->Pin.Call.target);             // p += 4|8

      /* mtspr 9,r_dst => move r_dst to count register */
      p = mkFormXFX(p, r_dst, 9, 467);                         // p += 4
      
      /* bctrl => branch to count register (and save to lr) */
      p = mkFormXL(p, 19, Pct_ALWAYS, 0, 0, 528, 1);           // p += 4

      /* Fix up the conditional jump, if there was one. */
      if (cond.test != Pct_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta >= 16 && delta <= 20);
         /* bc !ct,cf,delta */
         mkFormB(ptmp, invertCondTest(cond.test), cond.flag, (delta>>2), 0, 0);
      }
      goto done;
   }

   case Pin_Goto: {
      UInt  trc      = 0;
      UChar r_return = 3;    /* Put target addr into %r3 */
      PPC32CondCode cond = i->Pin.Goto.cond;
      UInt r_dst, imm_dst;
      
      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (cond.test != Pct_ALWAYS) {
         /* jmp fwds if !condition */
         /* don't know how many bytes to jump over yet...
            make space for a jump instruction and fill in later. */
         ptmp = p; /* fill in this bit later */
         p += 4;
      }

      // cond succeeds...
      
      /* If a non-boring, set GuestStatePtr appropriately. */
      switch (i->Pin.Goto.jk) {
         case Ijk_ClientReq:   trc = VEX_TRC_JMP_CLIENTREQ;   break;
         case Ijk_Sys_syscall: trc = VEX_TRC_JMP_SYS_SYSCALL; break;
         case Ijk_Yield:       trc = VEX_TRC_JMP_YIELD;       break;
         case Ijk_EmWarn:      trc = VEX_TRC_JMP_EMWARN;      break;
         case Ijk_MapFail:     trc = VEX_TRC_JMP_MAPFAIL;     break;
         case Ijk_NoDecode:    trc = VEX_TRC_JMP_NODECODE;    break;
         case Ijk_TInval:      trc = VEX_TRC_JMP_TINVAL;      break;
         case Ijk_Ret:
         case Ijk_Call:
         case Ijk_Boring:
            break;
         default: 
            ppIRJumpKind(i->Pin.Goto.jk);
            vpanic("emit_PPC32Instr.Pin_Goto: unknown jump kind");
      }
      if (trc !=0) {
         vassert(trc < 0x10000);
         /* addi r31,0,trc */
         p = mkFormD(p, 14, 31, 0, trc);               // p += 4
      }

      /* Get the destination address into %r_return */
      if (i->Pin.Goto.dst->tag == Pri_Imm) {
         imm_dst = i->Pin.Goto.dst->Pri.Imm;
         p = mkLoadImm(p, r_return, imm_dst);          // p += 4|8
      } else {
         vassert(i->Pin.Goto.dst->tag == Pri_Reg);
         r_dst = iregNo(i->Pin.Goto.dst->Pri.Reg);
         p = mkMoveReg(p, r_return, r_dst);            // p += 4
      }
      
      /* blr */
      p = mkFormXL(p, 19, Pct_ALWAYS, 0, 0, 16, 0);    // p += 4

      /* Fix up the conditional jump, if there was one. */
      if (cond.test != Pct_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta >= 12 && delta <= 20);
         /* bc !ct,cf,delta */
         mkFormB(ptmp, invertCondTest(cond.test), cond.flag, delta>>2, 0, 0);
      }
      goto done;
   }

   case Pin_CMov32: {
      UInt r_dst, imm_src, r_src;
      PPC32CondCode cond;
      vassert(i->Pin.CMov32.cond.test != Pct_ALWAYS);

      r_dst = iregNo(i->Pin.CMov32.dst);
      cond = i->Pin.CMov32.cond;

      /* branch (if cond fails) over move instrs */
      if (cond.test != Pct_ALWAYS) {
         /* don't know how many bytes to jump over yet...
            make space for a jump instruction and fill in later. */
         ptmp = p; /* fill in this bit later */
         p += 4;
      }

      // cond true: move src => dst
      switch (i->Pin.CMov32.src->tag) {
      case Pri_Imm:
         imm_src = i->Pin.CMov32.src->Pri.Imm;
         p = mkLoadImm(p, r_dst, imm_src);
         break;
      case Pri_Reg:
         r_src = iregNo(i->Pin.CMov32.src->Pri.Reg);
         p = mkMoveReg(p, r_dst, r_src);
         break;
      default: goto bad;
      }

      /* Fix up the conditional jump, if there was one. */
      if (cond.test != Pct_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta >= 4 && delta <= 12);
         /* bc !ct,cf,delta */
         mkFormB(ptmp, invertCondTest(cond.test), cond.flag, (delta>>2), 0, 0);
      }
      goto done;
   }

   case Pin_Load: {
      PPC32AMode* am_addr = i->Pin.Load.src;
      UInt r_dst = iregNo(i->Pin.Load.dst);
      Bool syned = i->Pin.Load.syned;
      UInt opc1, opc2, sz = i->Pin.Load.sz;
      switch (i->Pin.Load.src->tag) {
      case Pam_IR:
         if (sz == 2) {  // the only signed load
            opc1 = (syned) ? 42: 40;
         } else {
            vassert(syned == False);
            opc1 = (sz == 1) ? 34 : 32;   // 1:4
         }
         p = doAMode_IR(p, opc1, r_dst, am_addr);
         goto done;
      case Pam_RR:
         if (sz == 2) {  // the only signed load
            opc2 = (syned) ? 343: 279;
         } else {
            vassert(syned == False);
            opc2 = (sz == 1) ? 87 : 23;   // 1:4
         }
         p = doAMode_RR(p, 31, opc2, r_dst, am_addr);
         goto done;
      default:
         goto bad;
      }
   }

   case Pin_Set32: {
      /* Make the destination register be 1 or 0, depending on whether
         the relevant condition holds. */
      UInt r_dst = iregNo(i->Pin.Set32.dst);
      PPC32CondCode cond = i->Pin.Set32.cond;
      UInt rot_imm;
      UInt r_tmp;

      if (cond.test == Pct_ALWAYS) {
         // Just load 1 to dst => li dst,1
         p = mkFormD(p, 14, r_dst, 0, 1);
      } else {
         rot_imm = 1 + cond.flag;
         r_tmp = 0;  // Not within scope of regalloc, so no need to declare.

         // r_tmp = CR  => mfcr r_tmp
         p = mkFormX(p, 31, r_tmp, 0, 0, 19, 0);

         // r_dst = flag (rotate left and mask)
         //  => rlwinm r_dst,r_tmp,rot_imm,31,31
         p = mkFormM(p, 21, r_tmp, r_dst, rot_imm, 31, 31, 0);

         if (cond.test == Pct_FALSE) {
            // flip bit  => xori r_dst,r_dst,1
            p = mkFormD(p, 26, r_dst, r_dst, 1);
         }
      }
      goto done;
   }

   case Pin_MfCR:
      // mfcr dst
      p = mkFormX(p, 31, iregNo(i->Pin.MfCR.dst), 0, 0, 19, 0);
      goto done;

   case Pin_MFence: {
      p = mkFormX(p, 31, 0, 0, 0, 598, 0);   // sync, PPC32 p616
// CAB: Should this be isync?
//    p = mkFormXL(p, 19, 0, 0, 0, 150, 0);  // isync, PPC32 p467
      goto done;
   }

   case Pin_Store: {
      PPC32AMode* am_addr = i->Pin.Store.dst;
      UInt r_src = iregNo(i->Pin.Store.src);
      UInt opc1, opc2, sz = i->Pin.Store.sz;
      switch (i->Pin.Store.dst->tag) {
      case Pam_IR:
         opc1 = (sz == 1) ? 38 : ((sz == 2) ? 44 : 36);      // 1:2:4
         p = doAMode_IR(p, opc1, r_src, am_addr);
         goto done;
      case Pam_RR:
         opc2 = (sz == 1) ? 215 : ((sz == 2) ? 407 : 151);   // 1:2:4
         p = doAMode_RR(p, 31, opc2, r_src, am_addr);
         goto done;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_FpUnary: {
      UInt fr_dst = fregNo(i->Pin.FpUnary.dst);
      UInt fr_src = fregNo(i->Pin.FpUnary.src);
      switch (i->Pin.FpUnary.op) {
      case Pfp_SQRT:  // fsqrt, PPC32 p427
         p = mkFormA( p, 63, fr_dst, 0, fr_src, 0, 22, 0 );
         break;
      case Pfp_ABS:   // fabs, PPC32 p399
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 264, 0);
         break;
      case Pfp_NEG:   // fneg, PPC32 p416
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 40, 0);
         break;
      case Pfp_MOV:   // fmr, PPC32 p410
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 72, 0);
         break;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_FpBinary: {
      UInt fr_dst  = fregNo(i->Pin.FpBinary.dst);
      UInt fr_srcL = fregNo(i->Pin.FpBinary.srcL);
      UInt fr_srcR = fregNo(i->Pin.FpBinary.srcR);
      switch (i->Pin.FpBinary.op) {
      case Pfp_ADD:   // fadd, PPC32 p400
         p = mkFormA( p, 63, fr_dst, fr_srcL, fr_srcR, 0, 21, 0 );
         break;
      case Pfp_SUB:   // fsub, PPC32 p429
         p = mkFormA( p, 63, fr_dst, fr_srcL, fr_srcR, 0, 20, 0 );
         break;
      case Pfp_MUL:   // fmul, PPC32 p413
         p = mkFormA( p, 63, fr_dst, fr_srcL, 0, fr_srcR, 25, 0 );
         break;
      case Pfp_DIV:   // fdiv, PPC32 p406
         p = mkFormA( p, 63, fr_dst, fr_srcL, fr_srcR, 0, 18, 0 );
         break;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_FpLdSt: {
      PPC32AMode* am_addr = i->Pin.FpLdSt.addr;
      UInt f_reg = fregNo(i->Pin.FpLdSt.reg);
      Bool idxd = toBool(i->Pin.FpLdSt.addr->tag == Pam_RR);
      UChar sz = i->Pin.FpLdSt.sz;
      vassert(sz == 4 || sz == 8);

      if (i->Pin.FpLdSt.isLoad) {   // Load from memory
         if (idxd) {  // lf[s|d]x, PPC32 p444|440
            p = doAMode_RR(p, 31, ((sz == 4) ? 535 : 599), f_reg, am_addr);
         } else {     // lf[s|d], PPC32 p441|437
            p = doAMode_IR(p, ((sz == 4) ? 48 : 50), f_reg, am_addr);
         }
      } else {                      // Store to memory
         if (idxd) { // stf[s|d]x, PPC32 p521|516
            p = doAMode_RR(p, 31, ((sz == 4) ? 663 : 727), f_reg, am_addr);
         } else {    // stf[s|d], PPC32 p518|513
            p = doAMode_IR(p, ((sz == 4) ? 52 : 54), f_reg, am_addr);
         }
      }
      goto done;
   }

   case Pin_FpF64toF32: {
      UInt fr_dst = fregNo(i->Pin.FpF64toF32.dst);
      UInt fr_src = fregNo(i->Pin.FpF64toF32.src);
      // frsp, PPC32 p423
      p = mkFormX(p, 63, fr_dst, 0, fr_src, 12, 0);
      goto done;
   }

   case Pin_FpF64toI32: {
      UInt  r_dst   = iregNo(i->Pin.FpF64toI32.dst);
      UInt  fr_src  = fregNo(i->Pin.FpF64toI32.src);
      UChar fr_tmp  = 7;                // Temp freg
      PPC32AMode* am_addr;

      // fctiw (conv f64 to i32), PPC32 p404
      p = mkFormX(p, 63, fr_tmp, 0, fr_src, 14, 0);

      // No RI form of stfiwx, so need PPC32AMode_RR:
      am_addr = PPC32AMode_RR( StackFramePtr, hregPPC32_GPR0() );

      // stfiwx (store fp64[lo32] as int32), PPC32 p517
      p = doAMode_RR(p, 31, 983, fr_tmp, am_addr);

      // lwzx (load int32), PPC32 p463
      p = doAMode_RR(p, 31, 23, r_dst, am_addr);
      goto done;
   }

   case Pin_FpCMov: {
      UInt fr_dst      = fregNo(i->Pin.FpCMov.dst);
      UInt fr_src      = fregNo(i->Pin.FpCMov.src);
      PPC32CondCode cc = i->Pin.FpCMov.cond;

      if (fr_dst == fr_src) goto done;
      
      vassert(cc.test != Pct_ALWAYS);

      /* jmp fwds if !condition */
      if (cc.test != Pct_ALWAYS) {
         /* bc !ct,cf,n_bytes>>2 */
         p = mkFormB(p, invertCondTest(cc.test), cc.flag, 8>>2, 0, 0);
      }

      // fmr, PPC32 p410
      p = mkFormX(p, 63, fr_dst, 0, fr_src, 72, 0);
      goto done;
   }

   case Pin_FpLdFPSCR: {
      UInt fr_src = fregNo(i->Pin.FpLdFPSCR.src);
      p = mkFormXFL(p, 0xFF, fr_src);     // mtfsf, PPC32 p480
      goto done;
   }

   case Pin_FpCmp: {
      UChar crfD    = 1;
      UInt  r_dst   = iregNo(i->Pin.FpCmp.dst);
      UInt  fr_srcL = fregNo(i->Pin.FpCmp.srcL);
      UInt  fr_srcR = fregNo(i->Pin.FpCmp.srcR);
      vassert(crfD < 8);
      // fcmpo, PPC32 p402
      p = mkFormX(p, 63, crfD<<2, fr_srcL, fr_srcR, 32, 0);

      // mfcr (mv CR to r_dst), PPC32 p467
      p = mkFormX(p, 31, r_dst, 0, 0, 19, 0);
      
      // rlwinm r_dst,r_dst,8,28,31, PPC32 p501
      //  => rotate field 1 to bottomw of word, masking out upper 28
      p = mkFormM(p, 21, r_dst, r_dst, 8, 28, 31, 0);
      goto done;
   }

   case Pin_RdWrLR: {
      UInt reg = iregNo(i->Pin.RdWrLR.gpr);
      /* wrLR==True ? mtlr r4 : mflr r4 */
      p = mkFormXFX(p, reg, 8, (i->Pin.RdWrLR.wrLR==True) ? 467 : 339);
      goto done;
   }


   /* AltiVec */
   case Pin_AvLdSt: {
      UInt opc2, v_reg, r_idx, r_base;
      UChar sz   = i->Pin.AvLdSt.sz;
      Bool  idxd = toBool(i->Pin.AvLdSt.addr->tag == Pam_RR);
      vassert(sz == 1 || sz == 2 || sz == 4 || sz == 16);

      v_reg  = vregNo(i->Pin.AvLdSt.reg);
      r_base = iregNo(i->Pin.AvLdSt.addr->Pam.RR.base);

      // Only have AltiVec AMode_RR: kludge AMode_IR
      if (!idxd) {
         r_idx = 30;                       // XXX: Using r30 as temp
         p = mkLoadImm(p, r_idx, i->Pin.AvLdSt.addr->Pam.IR.index);
      } else {
         r_idx  = iregNo(i->Pin.AvLdSt.addr->Pam.RR.index);
      }

      if (i->Pin.FpLdSt.isLoad) {  // Load from memory (1,2,4,16)
         opc2 = (sz == 1) ? 7 : (sz == 2) ? 39 : (sz == 4) ? 71 : 103;
         p = mkFormX(p, 31, v_reg, r_idx, r_base, opc2, 0);
      } else {                      // Store to memory (1,2,4,16)
         opc2 = (sz == 1) ? 135 : (sz == 2) ? 167 : (sz == 4) ? 199 : 231;
         p = mkFormX(p, 31, v_reg, r_idx, r_base, opc2, 0);
      }
      goto done;
   }

   case Pin_AvUnary: {
      UInt v_dst = vregNo(i->Pin.AvUnary.dst);
      UInt v_src = vregNo(i->Pin.AvUnary.src);
      UInt opc2;
      switch (i->Pin.AvUnary.op) {
      case Pav_MOV:       opc2 = 1156; break; // vor vD,vS,vS
      case Pav_NOT:       opc2 = 1284; break; // vnor vD,vS,vS
      case Pav_UNPCKH8S:  opc2 =  526; break; // vupkhsb
      case Pav_UNPCKH16S: opc2 =  590; break; // vupkhsh
      case Pav_UNPCKL8S:  opc2 =  654; break; // vupklsb
      case Pav_UNPCKL16S: opc2 =  718; break; // vupklsh
      case Pav_UNPCKHPIX: opc2 =  846; break; // vupkhpx
      case Pav_UNPCKLPIX: opc2 =  974; break; // vupklpx
      default:
         goto bad;
      }
      switch (i->Pin.AvUnary.op) {
      case Pav_MOV:
      case Pav_NOT:
         p = mkFormVX( p, 4, v_dst, v_src, v_src, opc2 );
         break;
      default:
         p = mkFormVX( p, 4, v_dst, 0, v_src, opc2 );
         break;
      }
      goto done;
   }

   case Pin_AvBinary: {
      UInt v_dst  = vregNo(i->Pin.AvBinary.dst);
      UInt v_srcL = vregNo(i->Pin.AvBinary.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBinary.srcR);
      UInt opc2;
      if (i->Pin.AvBinary.op == Pav_SHL) {
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1036 ); // vslo
         p = mkFormVX( p, 4, v_dst, v_dst,  v_srcR, 452 );  // vsl
         goto done;
      }
      if (i->Pin.AvBinary.op == Pav_SHR) {
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1100 ); // vsro
         p = mkFormVX( p, 4, v_dst, v_dst,  v_srcR, 708 );  // vsr
         goto done;
      }
      switch (i->Pin.AvBinary.op) {
      /* Bitwise */
      case Pav_AND:       opc2 = 1028; break; // vand
      case Pav_OR:        opc2 = 1156; break; // vor
      case Pav_XOR:       opc2 = 1220; break; // vxor
      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin8x16: {
      UInt v_dst  = vregNo(i->Pin.AvBin8x16.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin8x16.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin8x16.srcR);
      UInt opc2;
      switch (i->Pin.AvBin8x16.op) {

      case Pav_ADDU:     opc2 =    0; break; // vaddubm
      case Pav_QADDU:    opc2 =  512; break; // vaddubs
      case Pav_QADDS:    opc2 =  768; break; // vaddsbs

      case Pav_SUBU:     opc2 = 1024; break; // vsububm
      case Pav_QSUBU:    opc2 = 1536; break; // vsububs
      case Pav_QSUBS:    opc2 = 1792; break; // vsubsbs

      case Pav_OMULU:   opc2 =    8; break; // vmuloub
      case Pav_OMULS:   opc2 =  264; break; // vmulosb
      case Pav_EMULU:   opc2 =  520; break; // vmuleub
      case Pav_EMULS:   opc2 =  776; break; // vmulesb

      case Pav_AVGU:     opc2 = 1026; break; // vavgub
      case Pav_AVGS:     opc2 = 1282; break; // vavgsb
      case Pav_MAXU:     opc2 =    2; break; // vmaxub
      case Pav_MAXS:     opc2 =  258; break; // vmaxsb
      case Pav_MINU:     opc2 =  514; break; // vminub
      case Pav_MINS:     opc2 =  770; break; // vminsb

      case Pav_CMPEQU:   opc2 =    6; break; // vcmpequb
      case Pav_CMPGTU:   opc2 =  518; break; // vcmpgtub
      case Pav_CMPGTS:   opc2 =  774; break; // vcmpgtsb

      case Pav_SHL:      opc2 =  260; break; // vslb
      case Pav_SHR:      opc2 =  516; break; // vsrb
      case Pav_SAR:      opc2 =  772; break; // vsrab
      case Pav_ROTL:     opc2 =    4; break; // vrlb

      case Pav_MRGHI:    opc2 =   12; break; // vmrghb
      case Pav_MRGLO:    opc2 =  268; break; // vmrglb

      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin16x8: {
      UInt v_dst  = vregNo(i->Pin.AvBin16x8.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin16x8.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin16x8.srcR);
      UInt opc2;
      switch (i->Pin.AvBin16x8.op) {

      case Pav_ADDU:    opc2 =   64; break; // vadduhm
      case Pav_QADDU:   opc2 =  576; break; // vadduhs
      case Pav_QADDS:   opc2 =  832; break; // vaddshs

      case Pav_SUBU:    opc2 = 1088; break; // vsubuhm
      case Pav_QSUBU:   opc2 = 1600; break; // vsubuhs
      case Pav_QSUBS:   opc2 = 1856; break; // vsubshs

      case Pav_OMULU:   opc2 =   72; break; // vmulouh
      case Pav_OMULS:   opc2 =  328; break; // vmulosh
      case Pav_EMULU:   opc2 =  584; break; // vmuleuh
      case Pav_EMULS:   opc2 =  840; break; // vmulesh

      case Pav_AVGU:    opc2 = 1090; break; // vavguh
      case Pav_AVGS:    opc2 = 1346; break; // vavgsh
      case Pav_MAXU:    opc2 =   66; break; // vmaxuh
      case Pav_MAXS:    opc2 =  322; break; // vmaxsh
      case Pav_MINS:    opc2 =  834; break; // vminsh
      case Pav_MINU:    opc2 =  578; break; // vminuh

      case Pav_CMPEQU:  opc2 =   70; break; // vcmpequh
      case Pav_CMPGTU:  opc2 =  582; break; // vcmpgtuh
      case Pav_CMPGTS:  opc2 =  838; break; // vcmpgtsh

      case Pav_SHL:     opc2 =  324; break; // vslh
      case Pav_SHR:     opc2 =  580; break; // vsrh
      case Pav_SAR:     opc2 =  836; break; // vsrah
      case Pav_ROTL:    opc2 =   68; break; // vrlh

      case Pav_PACKUU:  opc2 =   14; break; // vpkuhum
      case Pav_QPACKUU: opc2 =  142; break; // vpkuhus
      case Pav_QPACKSU: opc2 =  270; break; // vpkshus
      case Pav_QPACKSS: opc2 =  398; break; // vpkshss
      case Pav_PACKPXL: opc2 =  782; break; // vpkpx

      case Pav_MRGHI:   opc2 =   76; break; // vmrghh
      case Pav_MRGLO:   opc2 =  332; break; // vmrglh

      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin32x4: {
      UInt v_dst  = vregNo(i->Pin.AvBin32x4.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin32x4.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin32x4.srcR);
      UInt opc2;
      switch (i->Pin.AvBin32x4.op) {

      case Pav_ADDU:    opc2 =  128; break; // vadduwm
      case Pav_QADDU:   opc2 =  640; break; // vadduws
      case Pav_QADDS:   opc2 =  896; break; // vaddsws

      case Pav_SUBU:    opc2 = 1152; break; // vsubuwm
      case Pav_QSUBU:   opc2 = 1664; break; // vsubuws
      case Pav_QSUBS:   opc2 = 1920; break; // vsubsws

      case Pav_AVGU:    opc2 = 1154; break; // vavguw
      case Pav_AVGS:    opc2 = 1410; break; // vavgsw

      case Pav_MAXU:    opc2 =  130; break; // vmaxuw
      case Pav_MAXS:    opc2 =  386; break; // vmaxsw

      case Pav_MINS:    opc2 =  898; break; // vminsw
      case Pav_MINU:    opc2 =  642; break; // vminuw

      case Pav_CMPEQU:  opc2 =  134; break; // vcmpequw
      case Pav_CMPGTS:  opc2 =  902; break; // vcmpgtsw
      case Pav_CMPGTU:  opc2 =  646; break; // vcmpgtuw

      case Pav_SHL:     opc2 =  388; break; // vslw
      case Pav_SHR:     opc2 =  644; break; // vsrw
      case Pav_SAR:     opc2 =  900; break; // vsraw
      case Pav_ROTL:    opc2 =  132; break; // vrlw

      case Pav_PACKUU:  opc2 =   78; break; // vpkuwum
      case Pav_QPACKUU: opc2 =  206; break; // vpkuwus
      case Pav_QPACKSU: opc2 =  334; break; // vpkswus
      case Pav_QPACKSS: opc2 =  462; break; // vpkswss

      case Pav_MRGHI:   opc2 =  140; break; // vmrghw
      case Pav_MRGLO:   opc2 =  396; break; // vmrglw

      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin32Fx4: {
      UInt v_dst  = vregNo(i->Pin.AvBin32Fx4.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin32Fx4.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin32Fx4.srcR);
      switch (i->Pin.AvBin32Fx4.op) {

      case Pavfp_ADDF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 10 );   // vaddfp
         break;
      case Pavfp_SUBF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 74 );   // vsubfp
         break;
      case Pavfp_MAXF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1034 ); // vmaxfp
         break;
      case Pavfp_MINF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1098 ); // vminfp
         break;

      case Pavfp_MULF: {
         /* Make a vmulfp from a vmaddfp:
            load -0.0 (0x8000_0000) to each 32-bit word of vB
            this makes the add a noop.
         */
         UInt vB = 29;                    // XXX: Using r29 for temp
         UInt konst = 0x1F;

         // Better way to load -0.0 (0x80000000) ?
         // vspltisw vB,0x1F   (0x1F => each word of vB)
         p = mkFormVX( p, 4, vB, konst, 0, 908 );

         // vslw vB,vB,vB  (each word of vB = (0x1F << 0x1F) = 0x80000000
         p = mkFormVX( p, 4, vB, vB, vB, 388 );

         // Finally, do the multiply:
         p = mkFormVA( p, 4, v_dst, v_srcL, vB, v_srcR, 46 );
         break;
      }
      case Pavfp_CMPEQF:
         p = mkFormVXR( p, 4, v_dst, v_srcL, v_srcR, 0, 198 ); // vcmpeqfp
         break;
      case Pavfp_CMPGTF:
         p = mkFormVXR( p, 4, v_dst, v_srcL, v_srcR, 0, 710 ); // vcmpgtfp
         break;
      case Pavfp_CMPGEF:
         p = mkFormVXR( p, 4, v_dst, v_srcL, v_srcR, 0, 454 ); // vcmpgefp
         break;

      default:
         goto bad;
      }
      goto done;
   }

   case Pin_AvUn32Fx4: {
      UInt v_dst = vregNo(i->Pin.AvUn32Fx4.dst);
      UInt v_src = vregNo(i->Pin.AvUn32Fx4.src);
      UInt opc2;
      switch (i->Pin.AvUn32Fx4.op) {
      case Pavfp_RCPF:    opc2 =  266; break; // vrefp
      case Pavfp_RSQRTF:  opc2 =  330; break; // vrsqrtefp
      case Pavfp_CVTU2F:  opc2 =  778; break; // vcfux
      case Pavfp_CVTS2F:  opc2 =  842; break; // vcfsx
      case Pavfp_QCVTF2U: opc2 =  906; break; // vctuxs
      case Pavfp_QCVTF2S: opc2 =  970; break; // vctsxs
      case Pavfp_ROUNDM:  opc2 =  714; break; // vrfim
      case Pavfp_ROUNDP:  opc2 =  650; break; // vrfip
      case Pavfp_ROUNDN:  opc2 =  522; break; // vrfin
      case Pavfp_ROUNDZ:  opc2 =  586; break; // vrfiz
      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, 0, v_src, opc2 );
      goto done;
   }

   case Pin_AvPerm: {  // vperm
      UInt v_dst  = vregNo(i->Pin.AvPerm.dst);
      UInt v_srcL = vregNo(i->Pin.AvPerm.srcL);
      UInt v_srcR = vregNo(i->Pin.AvPerm.srcR);
      UInt v_ctl  = vregNo(i->Pin.AvPerm.ctl);
      p = mkFormVA( p, 4, v_dst, v_srcL, v_srcR, v_ctl, 43 );
      goto done;
   }

   case Pin_AvSel: {  // vsel
      UInt v_ctl  = vregNo(i->Pin.AvSel.ctl);
      UInt v_dst  = vregNo(i->Pin.AvSel.dst);
      UInt v_srcL = vregNo(i->Pin.AvSel.srcL);
      UInt v_srcR = vregNo(i->Pin.AvSel.srcR);
      p = mkFormVA( p, 4, v_dst, v_srcL, v_srcR, v_ctl, 42 );
      goto done;
   }

   case Pin_AvShlDbl: {  // vsldoi
      UInt shift  = i->Pin.AvShlDbl.shift;
      UInt v_dst  = vregNo(i->Pin.AvShlDbl.dst);
      UInt v_srcL = vregNo(i->Pin.AvShlDbl.srcL);
      UInt v_srcR = vregNo(i->Pin.AvShlDbl.srcR);
      vassert(shift <= 0xF);
      p = mkFormVA( p, 4, v_dst, v_srcL, v_srcR, shift, 44 );
      goto done;
   }

   case Pin_AvSplat: { // vsplt(is)(b,h,w)
      UInt v_dst = vregNo(i->Pin.AvShlDbl.dst);
      UChar sz   = i->Pin.AvSplat.sz;
      UInt v_src, opc2;
      vassert(sz == 8 || sz == 16 || sz == 32);

      if (i->Pin.AvSplat.src->tag == Pvi_Imm) {
         Char simm5;
         opc2 = (sz == 8) ? 780 : (sz == 16) ? 844 : 908;   // 8,16,32
         /* expects 5-bit-signed-imm */
         simm5 = i->Pin.AvSplat.src->Pvi.Imm5s;
         vassert(simm5 >= -16 && simm5 <= 15);
         simm5 = simm5 & 0x1F;
         p = mkFormVX( p, 4, v_dst, (UInt)simm5, 0, opc2 );
      }
      else {  // Pri_Reg
         UInt lowest_lane;
         opc2 = (sz == 8) ? 524 : (sz == 16) ? 588 : 652;  // 8,16,32
         vassert(hregClass(i->Pin.AvSplat.src->Pvi.Reg) == HRcVec128);
         v_src = vregNo(i->Pin.AvSplat.src->Pvi.Reg);
         lowest_lane = (128/sz)-1;
         p = mkFormVX( p, 4, v_dst, lowest_lane, v_src, opc2 );
      }
      goto done;
   }

   case Pin_AvCMov: {
      UInt v_dst      = vregNo(i->Pin.AvCMov.dst);
      UInt v_src      = vregNo(i->Pin.AvCMov.src);
      PPC32CondCode cc = i->Pin.AvCMov.cond;

      if (v_dst == v_src) goto done;
      
      vassert(cc.test != Pct_ALWAYS);

      /* jmp fwds 2 insns if !condition */
      if (cc.test != Pct_ALWAYS) {
         /* bc !ct,cf,n_bytes>>2 */
         p = mkFormB(p, invertCondTest(cc.test), cc.flag, 8>>2, 0, 0);
      }
      /* vmr */
      p = mkFormVX( p, 4, v_dst, v_src, v_src, 1156 );
      goto done;
   }

   case Pin_AvLdVSCR: {  // mtvscr
      UInt v_src = vregNo(i->Pin.AvLdVSCR.src);
      p = mkFormVX( p, 4, 0, 0, v_src, 1604 );
      goto done;
   }

   default: 
      goto bad;
   }

  bad:
   vex_printf("\n=> ");
   ppPPC32Instr(i);
   vpanic("emit_PPC32Instr");
   /*NOTREACHED*/
   
  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];
}

/*---------------------------------------------------------------*/
/*--- end                                  host-ppc32/hdefs.c ---*/
/*---------------------------------------------------------------*/
