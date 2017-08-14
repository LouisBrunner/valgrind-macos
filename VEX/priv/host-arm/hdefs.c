
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-arm/hdefs.c) is                         ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

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

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"
#include "host-arm/hdefs.h"



/* --------- Registers. --------- */

/* The usual HReg abstraction.
   There are 16 general purpose regs.
*/


/* --------- Registers. --------- */

void ppHRegARM ( HReg reg )  {
   Int r;
   static HChar* ireg32_names[16] 
     = { "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8",
	 "r9", "r10", "r11", "r12", "r13", "r14", "r15" };

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("%s", ireg32_names[r]);
         return;
      default:
         vpanic("ppHRegARM");
   }
}

HReg hregARM_R0 ( void ) { return mkHReg(0, HRcInt32, False); }
HReg hregARM_R1 ( void ) { return mkHReg(1, HRcInt32, False); }
HReg hregARM_R2 ( void ) { return mkHReg(2, HRcInt32, False); }
HReg hregARM_R3 ( void ) { return mkHReg(3, HRcInt32, False); }
HReg hregARM_R4 ( void ) { return mkHReg(4, HRcInt32, False); }
HReg hregARM_R5 ( void ) { return mkHReg(5, HRcInt32, False); }
HReg hregARM_R6 ( void ) { return mkHReg(6, HRcInt32, False); }
HReg hregARM_R7 ( void ) { return mkHReg(7, HRcInt32, False); }
HReg hregARM_R8 ( void ) { return mkHReg(8, HRcInt32, False); }
HReg hregARM_R9 ( void ) { return mkHReg(9, HRcInt32, False); }
HReg hregARM_R10 ( void ) { return mkHReg(10, HRcInt32, False); }
HReg hregARM_R11 ( void ) { return mkHReg(11, HRcInt32, False); }
HReg hregARM_R12 ( void ) { return mkHReg(12, HRcInt32, False); }
HReg hregARM_R13 ( void ) { return mkHReg(13, HRcInt32, False); }
HReg hregARM_R14 ( void ) { return mkHReg(14, HRcInt32, False); }
HReg hregARM_R15 ( void ) { return mkHReg(15, HRcInt32, False); }

void getAllocableRegs_ARM ( Int* nregs, HReg** arr ) {
   *nregs = 20;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   (*arr)[0] = hregARM_R0();
   (*arr)[1] = hregARM_R1();
   (*arr)[2] = hregARM_R2();
   (*arr)[3] = hregARM_R3();
   (*arr)[4] = hregARM_R4();
   (*arr)[5] = hregARM_R5();
   (*arr)[6] = hregARM_R6();
   (*arr)[7] = hregARM_R7();
   (*arr)[8] = hregARM_R8();
   (*arr)[9] = hregARM_R9();
   (*arr)[10] = hregARM_R10();
   (*arr)[11] = hregARM_R11();
   (*arr)[12] = hregARM_R12();
   (*arr)[13] = hregARM_R13();
   (*arr)[14] = hregARM_R14();
   (*arr)[15] = hregARM_R15();
}



/* --------- Condition codes, ARM encoding. --------- */

HChar* showARMCondCode ( ARMCondCode cond ) {
   switch (cond) {
       case ARMccEQ:    return "eq";
       case ARMccNE:    return "ne";
       case ARMccHS:    return "hs";
       case ARMccLO:    return "lo";
       case ARMccMI:    return "mi";
       case ARMccPL:    return "pl";
       case ARMccVS:    return "vs";
       case ARMccVC:    return "vc";
       case ARMccHI:    return "hi";
       case ARMccLS:    return "ls";
       case ARMccGE:    return "ge";
       case ARMccLT:    return "lt";
       case ARMccGT:    return "gt";
       case ARMccLE:    return "le";
       case ARMccAL:    return "al"; // default
       case ARMccNV:    return "nv";
       default: vpanic("showARMCondCode");
   }
}




/* --------- ARMAMode1: memory address expressions. --------- */

ARMAMode1* ARMAMode1_I12A ( ARMImm12A imm ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_I12A;
    am->ARMam1.I12A.imm = imm;
    return am;
}
ARMAMode1* ARMAMode1_ShlI ( HReg Rm, ARMImm5 imm ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_ShlI;
    am->ARMam1.ShlI.Rm = Rm;
    am->ARMam1.ShlI.imm = imm;
    return am;
}
ARMAMode1* ARMAMode1_ShrI ( HReg Rm, ARMImm5 imm ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_ShrI;
    am->ARMam1.ShrI.Rm = Rm;
    am->ARMam1.ShrI.imm = imm;
    return am;
}
ARMAMode1* ARMAMode1_SarI ( HReg Rm, ARMImm5 imm ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_SarI;
    am->ARMam1.SarI.Rm = Rm;
    am->ARMam1.SarI.imm = imm;
    return am;
}
ARMAMode1* ARMAMode1_ShlR ( HReg Rm, HReg Rs ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_ShlR;
    am->ARMam1.ShlR.Rm = Rm;
    am->ARMam1.ShlR.Rs = Rs;
    return am;
}
ARMAMode1* ARMAMode1_ShrR ( HReg Rm, HReg Rs ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_ShrR;
    am->ARMam1.ShrR.Rm = Rm;
    am->ARMam1.ShrR.Rs = Rs;
    return am;
}
ARMAMode1* ARMAMode1_SarR ( HReg Rm, HReg Rs ) {
    ARMAMode1* am = LibVEX_Alloc(sizeof(ARMAMode1));
    am->tag = ARMam1_SarR;
    am->ARMam1.SarR.Rm = Rm;
    am->ARMam1.SarR.Rs = Rs;
    return am;
}

ARMAMode1* dopyARMAMode1 ( ARMAMode1* am ) {
   switch (am->tag) {
   case ARMam1_I12A:
       return ARMAMode1_I12A( am->ARMam1.I12A.imm );
   case ARMam1_ShlI:
       return ARMAMode1_ShlI( am->ARMam1.ShlI.Rm, am->ARMam1.ShlI.imm );
   case ARMam1_ShrI:
       return ARMAMode1_ShrI( am->ARMam1.ShrI.Rm, am->ARMam1.ShrI.imm );
   case ARMam1_SarI:
       return ARMAMode1_SarI( am->ARMam1.SarI.Rm, am->ARMam1.SarI.imm );
   case ARMam1_ShlR:
       return ARMAMode1_ShlR( am->ARMam1.ShlR.Rm, am->ARMam1.ShlR.Rs );
   case ARMam1_ShrR:
       return ARMAMode1_ShrR( am->ARMam1.ShrR.Rm, am->ARMam1.ShrR.Rs );
   case ARMam1_SarR:
       return ARMAMode1_SarR( am->ARMam1.SarR.Rm, am->ARMam1.SarR.Rs );
   default:
       vpanic("dopyARMAMode1");
   }
}

void ppARMAMode1 ( ARMAMode1* am ) {
   switch (am->tag) {
   case ARMam1_I12A:
   case ARMam1_ShlI:
   case ARMam1_ShrI:
   case ARMam1_SarI:
   case ARMam1_ShlR:
   case ARMam1_ShrR:
   case ARMam1_SarR:
       vex_printf("ppARMAMode1: Not implemented");
       break;
   default:
       vpanic("ppARMAMode1");
   }
}

/*
static void addRegUsage_ARMAMode1 ( HRegUsage* u, ARMAMode1* am ) {
static void mapRegs_ARMAMode1 ( HRegRemap* m, ARMAMode1* am ) {
*/


/* ------ ARMAMode1_I12A Helper function ------
  Given imm32, find immed_8, rotate_imm.
  ARM ARM A5-6: imm32 = immed_8 ROR (rotate_imm * 2)
*/
Bool mk_ARMImm12A ( UInt imm32, ARMImm12A* imm12a ) {
//    UInt imm32_orig = imm32;
    UInt shr=0, rot=0;
    imm12a->imm = 0;
    imm12a->rot = 0;
    
    // Easiest case: no shift needed
    if (imm32 > 0xFF) {
	// Next easiest: just a shift to the right needed
	while ((imm32 & 1) == 0) { imm32 = imm32 >> 1;  shr++; }
	rot = 32 - shr;

	if (imm32 > 0xFF) {
	    // Hardest: Need to rol (some minimum amount)
	    // valid byte could be split over first and last bytes...
	    
	    // ROL 7 (worst case for still valid imm32):
	    imm32 = (imm32 << 7) | (imm32 << (32-7));
	    // ShR (reverse rol) if went too far:
	    while ((imm32 & 1) == 0) { imm32 = imm32 >> 1; shr++; }
	    rot = 7 - shr;   // if valid imm32, shr < 7
	    
	    if (imm32 > 0xFF) {  // Can't represent this value
//		vex_printf("Error: Can't represent imm32: 0x%x", imm32_orig);
		return False;
	    }
	}
    }
    // Valid imm32 so far...

    if (rot & 1) {
	rot--;
	imm32 = imm32 << 1;
	if (imm32 > 0xFF) {
	    // Can't represent this value (can only shift even n)
//	    vex_printf("Error: Can't represent imm32: 0x%x\n", imm32_orig);
	    return False;
	}
    }

    imm12a->imm = imm32;
    imm12a->rot = rot / 2;
    
    vassert((imm12a->imm & 0xFF) == imm12a->imm);
    vassert((imm12a->rot & 0xF ) == imm12a->rot);
    return True;
}





/* --------- ARMAMode2: memory address expressions. --------- */

ARMAMode2* ARMAMode2_RI ( HReg Rn, ARMImm12 imm ) {
    ARMAMode2* am = LibVEX_Alloc(sizeof(ARMAMode2));
    am->tag = ARMam2_RI;
    am->ARMam2.RI.Rn = Rn;
    am->ARMam2.RI.imm = imm;
    return am;
}
ARMAMode2* ARMAMode2_RR ( HReg Rn, HReg Rm ) {
    ARMAMode2* am = LibVEX_Alloc(sizeof(ARMAMode2));
    am->tag = ARMam2_RR;
    am->ARMam2.RR.Rn = Rn;
    am->ARMam2.RR.Rm = Rm;
    return am;
}
ARMAMode2* ARMAMode2_RRS ( HReg Rn, HReg Rm, ARMImm5 shift ) {
    ARMAMode2* am = LibVEX_Alloc(sizeof(ARMAMode2));
    am->tag = ARMam2_RRS;
    am->ARMam2.RRS.Rn = Rn;
    am->ARMam2.RRS.Rm = Rm;
    am->ARMam2.RRS.shift = shift;
    return am;
}

ARMAMode2* dopyARMAMode2 ( ARMAMode2* am ) {
   switch (am->tag) {
   case ARMam2_RI:
       return ARMAMode2_RI( am->ARMam2.RI.Rn, am->ARMam2.RI.imm );
   case ARMam2_RR:
       return ARMAMode2_RR( am->ARMam2.RR.Rn, am->ARMam2.RR.Rm );
   case ARMam2_RRS:
       return ARMAMode2_RRS( am->ARMam2.RRS.Rn, am->ARMam2.RRS.Rm,
			     am->ARMam2.RRS.shift );
   default:
       vpanic("dopyARMAMode2");
   }
}

void ppARMAMode2 ( ARMAMode2* am ) {
   switch (am->tag) {
   case ARMam2_RI:
   case ARMam2_RR:
   case ARMam2_RRS:
       vex_printf("ppARMAMode2: Not implemented");
       break;
   default:
       vpanic("ppARMAMode2");
   }
}

/*
static void addRegUsage_ARMAMode2 ( HRegUsage* u, ARMAMode1* am ) {
static void mapRegs_ARMAMode2 ( HRegRemap* m, ARMAMode1* am ) {
*/


/* --------- ARMAMode3: memory address expressions. --------- */

ARMAMode3* ARMAMode3_RI ( HReg Rn, ARMImm8 imm ) {
    ARMAMode3* am = LibVEX_Alloc(sizeof(ARMAMode3));
    am->tag = ARMam3_RI;
    am->ARMam3.RI.Rn = Rn;
    am->ARMam3.RI.imm = imm;
    return am;
}
ARMAMode3* ARMAMode3_RR ( HReg Rn, HReg Rm ) {
    ARMAMode3* am = LibVEX_Alloc(sizeof(ARMAMode3));
    am->tag = ARMam3_RR;
    am->ARMam3.RR.Rn = Rn;
    am->ARMam3.RR.Rm = Rm;
    return am;
}

ARMAMode3* dopyARMAMode3 ( ARMAMode3* am ) {
   switch (am->tag) {
   case ARMam3_RI:
       return ARMAMode3_RI( am->ARMam3.RI.Rn, am->ARMam3.RI.imm );
   case ARMam3_RR:
       return ARMAMode3_RR( am->ARMam3.RR.Rn, am->ARMam3.RR.Rm );
   default:
       vpanic("dopyARMAMode3");
   }
}

void ppARMAMode3 ( ARMAMode3* am ) {
   switch (am->tag) {
   case ARMam3_RI:
   case ARMam3_RR:
       vex_printf("ppARMAMode3: Not implemented");
       break;
   default:
       vpanic("ppARMAMode3");
   }
}

/*
static void addRegUsage_ARMAMode1 ( HRegUsage* u, ARMAMode2* am ) {
static void mapRegs_ARMAMode2 ( HRegRemap* m, ARMAMode2* am ) {
*/

/* ------ Branch destination ------ */

ARMBranchDest* ARMBranchDest_Imm ( ARMImm24 imm24 ) {
   ARMBranchDest* branch_dest = LibVEX_Alloc(sizeof(ARMBranchDest));
   branch_dest->tag = ARMbdImm;
   branch_dest->ARMbd.Imm.imm24 = imm24;
   return branch_dest;
}
ARMBranchDest* ARMBranchDest_Reg ( HReg reg ) {
   ARMBranchDest* branch_dest = LibVEX_Alloc(sizeof(ARMBranchDest));
   branch_dest->tag = ARMbdReg;
   branch_dest->ARMbd.Reg.reg = reg;
   return branch_dest;
}

void ppARMBranchDest ( ARMBranchDest* branch_dest ) {
    switch (branch_dest->tag) {
    case ARMbdImm:
    case ARMbdReg:
       vex_printf("ppARMBranchDest: Not implemented");
       break;
    default:
	vpanic("ppX86RM");
    }
}




/* --------- Instructions. --------- */

HChar* showARMAluOp ( ARMAluOp op ) {
    switch (op) {
    case ARMalu_AND:  return "and";
    case ARMalu_ORR:  return "orr";
    case ARMalu_EOR:  return "eor";
    case ARMalu_SUB:  return "sub";
    case ARMalu_RSB:  return "rsb";
    case ARMalu_ADD:  return "add";
    case ARMalu_ADC:  return "adc";
    case ARMalu_SBC:  return "sbc";
    case ARMalu_RSC:  return "rsc";
    case ARMalu_TST:  return "tst";
    case ARMalu_TEQ:  return "teq";
    case ARMalu_CMP:  return "cmp";
    case ARMalu_CMN:  return "cmn";
    case ARMalu_MOV:  return "mov";
    case ARMalu_MVN:  return "mvn";
    case ARMalu_BIC:  return "bic";
    default: vpanic("showARMAluOp");
    }
}

/* --- Addressing Mode 1 --- */
ARMInstr* ARMInstr_DPCmp ( ARMAluOp op, HReg Rn,
			   ARMAMode1* shifter_op ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_DPCmp;
    i->ARMin.DPCmp.op = op;
    i->ARMin.DPCmp.Rn = Rn;
    i->ARMin.DPCmp.shifter_op = shifter_op;
    return i;
}

ARMInstr* ARMInstr_DPInstr1 ( ARMAluOp op, HReg Rd,
			      ARMAMode1* shifter_op ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_DPInstr1;
    i->ARMin.DPInstr1.op = op;
    i->ARMin.DPInstr1.Rd = Rd;
    i->ARMin.DPInstr1.shifter_op = shifter_op;
    return i;
}

ARMInstr* ARMInstr_DPInstr2 ( ARMAluOp op, HReg Rd, HReg Rn,
			      ARMAMode1* shifter_op ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_DPInstr2;
    i->ARMin.DPInstr2.op = op;
    i->ARMin.DPInstr2.Rd = Rd;
    i->ARMin.DPInstr2.Rn = Rn;
    i->ARMin.DPInstr2.shifter_op = shifter_op;
    return i;
}

/* --- Addressing Mode 2 --- */
ARMInstr* ARMInstr_LoadUB ( HReg Rd, ARMAMode2* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_LoadUB;
    i->ARMin.LoadUB.Rd = Rd;
    i->ARMin.LoadUB.addr_mode = addr_mode;
    return i;
}

ARMInstr* ARMInstr_StoreB ( HReg Rd, ARMAMode2* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_StoreB;
    i->ARMin.StoreB.Rd = Rd;
    i->ARMin.StoreB.addr_mode = addr_mode;
    return i;
}

ARMInstr* ARMInstr_LoadW ( HReg Rd, ARMAMode2* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_LoadW;
    i->ARMin.LoadW.Rd = Rd;
    i->ARMin.LoadW.addr_mode = addr_mode;
    return i;
}

ARMInstr* ARMInstr_StoreW ( HReg Rd, ARMAMode2* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_StoreW;
    i->ARMin.StoreW.Rd = Rd;
    i->ARMin.StoreW.addr_mode = addr_mode;
    return i;
}

/* --- Addressing Mode 3 --- */
ARMInstr* ARMInstr_LoadSB ( HReg Rd, ARMAMode3* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_LoadSB;
    i->ARMin.LoadSB.Rd = Rd;
    i->ARMin.LoadSB.addr_mode = addr_mode;
    return i;
}

ARMInstr* ARMInstr_LoadUH ( HReg Rd, ARMAMode3* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_LoadUH;
    i->ARMin.LoadUH.Rd = Rd;
    i->ARMin.LoadUH.addr_mode = addr_mode;
    return i;
}

ARMInstr* ARMInstr_LoadSH ( HReg Rd, ARMAMode3* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_LoadSH;
    i->ARMin.LoadSH.Rd = Rd;
    i->ARMin.LoadSH.addr_mode = addr_mode;
    return i;
}

ARMInstr* ARMInstr_StoreH ( HReg Rd, ARMAMode3* addr_mode ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_StoreH;
    i->ARMin.StoreH.Rd = Rd;
    i->ARMin.StoreH.addr_mode = addr_mode;
    return i;
}

/* --- Branch --- */
ARMInstr* ARMInstr_Branch ( ARMCondCode cond, ARMBranchDest* dest ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_Branch;
    i->ARMin.Branch.cond = cond;
    i->ARMin.Branch.dest = dest;
    return i;
}

ARMInstr* ARMInstr_BranchL ( ARMCondCode cond, ARMBranchDest* dest ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_BranchL;
    i->ARMin.BranchL.dest = dest;
    return i;
}

/* --- Literal --- */
ARMInstr* ARMInstr_Literal ( HReg reg, UInt imm ) {
    ARMInstr* i       = LibVEX_Alloc(sizeof(ARMInstr));
    i->tag = ARMin_Literal;
    i->ARMin.Literal.reg = reg;
    i->ARMin.Literal.imm = imm;
    return i;
}


void ppARMInstr ( ARMInstr* i ) {
    switch (i->tag) {
    case ARMin_DPCmp:
    case ARMin_DPInstr1:
    case ARMin_DPInstr2:
    case ARMin_LoadUB:
    case ARMin_StoreB:
    case ARMin_LoadW:
    case ARMin_StoreW:
    case ARMin_LoadSB:
    case ARMin_LoadUH:
    case ARMin_LoadSH:
    case ARMin_StoreH:
    case ARMin_Branch:
    case ARMin_BranchL:
    case ARMin_Literal:
	vex_printf("ppARMInstr: Not implemented");
	break;

    default:
	vpanic("ppARMInstr");
    }
}




/* --------- Helpers for register allocation. --------- */

void getRegUsage_ARMInstr ( HRegUsage* u, ARMInstr* i ) {
//    Bool unary;
    initHRegUsage(u);
    switch (i->tag) {
    case ARMin_DPCmp:
    case ARMin_DPInstr1:
    case ARMin_DPInstr2:
    case ARMin_LoadUB:
    case ARMin_StoreB:
    case ARMin_LoadW:
    case ARMin_StoreW:
    case ARMin_LoadSB:
    case ARMin_LoadUH:
    case ARMin_LoadSH:
    case ARMin_StoreH:
    case ARMin_Branch:
    case ARMin_BranchL:
    case ARMin_Literal:

    default:
	ppARMInstr(i);
	vpanic("getRegUsage_ARMInstr");
    }
}


/* local helper */
#if 0
static void mapReg(HRegRemap* m, HReg* r) {
   *r = lookupHRegRemap(m, *r);
}
#endif

void mapRegs_ARMInstr ( HRegRemap* m, ARMInstr* i ) {
    switch (i->tag) {
    case ARMin_DPCmp:
    case ARMin_DPInstr1:
    case ARMin_DPInstr2:
    case ARMin_LoadUB:
    case ARMin_StoreB:
    case ARMin_LoadW:
    case ARMin_StoreW:
    case ARMin_LoadSB:
    case ARMin_LoadUH:
    case ARMin_LoadSH:
    case ARMin_StoreH:
    case ARMin_Branch:
    case ARMin_BranchL:
    case ARMin_Literal:

    default:
	ppARMInstr(i);
	vpanic("getRegUsage_ARMInstr");
    }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_ARMInstr ( ARMInstr* i, HReg* src, HReg* dst ) {
    return False;  // No optimisations for now...
}


/* Generate x86 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

ARMInstr* genSpill_ARM ( HReg rreg, Int offsetB ) {
//   ARMAMode1* am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   switch (hregClass(rreg)) {

   default: 
       ppHRegClass(hregClass(rreg));
       vpanic("genSpill_ARM: unimplemented regclass");
   }
}

ARMInstr* genReload_ARM ( HReg rreg, Int offsetB ) {
//   ARMAMode1* am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   switch (hregClass(rreg)) {

   default: 
       ppHRegClass(hregClass(rreg));
       vpanic("genReload_ARM: unimplemented regclass");
   }
}


/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */

Int emit_ARMInstr ( UChar* buf, Int nbuf, ARMInstr* i ) {
//    UInt irno, opc, opc_rr, subopc_imm, opc_imma, opc_cl, opc_imm, subopc;
    
//    UInt   xtra;
    UChar* p = &buf[0];
//    UChar* ptmp;
    vassert(nbuf >= 32);

    switch (i->tag) {
    case ARMin_DPCmp:
    case ARMin_DPInstr1:
    case ARMin_DPInstr2:
    case ARMin_LoadUB:
    case ARMin_StoreB:
    case ARMin_LoadW:
    case ARMin_StoreW:
    case ARMin_LoadSB:
    case ARMin_LoadUH:
    case ARMin_LoadSH:
    case ARMin_StoreH:
    case ARMin_Branch:
    case ARMin_BranchL:
    case ARMin_Literal:
    default: 
	goto bad;
    }

 bad:
    ppARMInstr(i);
    vpanic("emit_ARMInstr");
    /*NOTREACHED*/
    
// done:
    vassert(p - &buf[0] <= 32);
    return p - &buf[0];
}


/*---------------------------------------------------------------*/
/*--- end                                    host-x86/hdefs.c ---*/
/*---------------------------------------------------------------*/
