
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-x86/hdefs.c) is                         ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"
#include "host-x86/hdefs.h"


/* --------- Registers. --------- */

void ppHRegX86 ( HReg reg ) 
{
   Int r;
   static Char* ireg32_names[8] 
     = { "%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 8);
         vex_printf("%s", ireg32_names[r]);
         return;
      case HRcFloat:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 4);
         vex_printf("%%fake%d", r);
         return;
      case HRcVector:
         vpanic("ppHRegX86: real vector reg");
     default:
         vpanic("ppHRegX86");
   }
}

HReg hregX86_EAX ( void ) { return mkHReg(0, HRcInt, False); }
HReg hregX86_ECX ( void ) { return mkHReg(1, HRcInt, False); }
HReg hregX86_EDX ( void ) { return mkHReg(2, HRcInt, False); }
HReg hregX86_EBX ( void ) { return mkHReg(3, HRcInt, False); }
HReg hregX86_ESP ( void ) { return mkHReg(4, HRcInt, False); }
HReg hregX86_EBP ( void ) { return mkHReg(5, HRcInt, False); }
HReg hregX86_ESI ( void ) { return mkHReg(6, HRcInt, False); }
HReg hregX86_EDI ( void ) { return mkHReg(7, HRcInt, False); }

HReg hregX86_FAKE0 ( void ) { return mkHReg(0, HRcFloat, False); }
HReg hregX86_FAKE1 ( void ) { return mkHReg(1, HRcFloat, False); }
HReg hregX86_FAKE2 ( void ) { return mkHReg(2, HRcFloat, False); }
HReg hregX86_FAKE3 ( void ) { return mkHReg(3, HRcFloat, False); }

void getAllocableRegs_X86 ( Int* nregs, HReg** arr )
{
   *nregs = 10;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   (*arr)[0] = hregX86_EAX();
   (*arr)[1] = hregX86_EBX();
   (*arr)[2] = hregX86_ECX();
   (*arr)[3] = hregX86_EDX();
   (*arr)[4] = hregX86_ESI();
   (*arr)[5] = hregX86_EDI();
   (*arr)[6] = hregX86_FAKE0();
   (*arr)[7] = hregX86_FAKE1();
   (*arr)[8] = hregX86_FAKE2();
   (*arr)[9] = hregX86_FAKE3();
}


/* --------- Condition codes, Intel encoding. --------- */

Char* showX86CondCode ( X86CondCode cond )
{
   switch (cond) {
      case Xcc_O:      return "o";
      case Xcc_NO:     return "no";
      case Xcc_B:      return "b";
      case Xcc_NB:     return "nb";
      case Xcc_Z:      return "z";
      case Xcc_NZ:     return "nz";
      case Xcc_BE:     return "be";
      case Xcc_NBE:    return "nbe";
      case Xcc_S:      return "s";
      case Xcc_NS:     return "ns";
      case Xcc_P:      return "p";
      case Xcc_NP:     return "np";
      case Xcc_L:      return "l";
      case Xcc_NL:     return "nl";
      case Xcc_LE:     return "le";
      case Xcc_NLE:    return "nle";
      case Xcc_ALWAYS: return "ALWAYS";
      default: vpanic("ppX86CondCode");
   }
}


/* --------- X86AMode: memory address expressions. --------- */

X86AMode* X86AMode_IR ( UInt imm32, HReg reg ) {
   X86AMode* am = LibVEX_Alloc(sizeof(X86AMode));
   am->tag = Xam_IR;
   am->Xam.IR.imm = imm32;
   am->Xam.IR.reg = reg;
   return am;
}
X86AMode* X86AMode_IRRS ( UInt imm32, HReg base, HReg indEx, Int shift ) {
   X86AMode* am = LibVEX_Alloc(sizeof(X86AMode));
   am->tag = Xam_IRRS;
   am->Xam.IRRS.imm = imm32;
   am->Xam.IRRS.base = base;
   am->Xam.IRRS.index = indEx;
   am->Xam.IRRS.shift = shift;
   vassert(shift >= 0 && shift <= 3);
   return am;
}

void ppX86AMode ( X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         if (am->Xam.IR.imm == 0)
            vex_printf("(");
         else
            vex_printf("0x%x(", am->Xam.IR.imm);
         ppHRegX86(am->Xam.IR.reg);
         vex_printf(")");
         return;
      case Xam_IRRS:
         vex_printf("0x%x(", am->Xam.IRRS.imm);
         ppHRegX86(am->Xam.IRRS.base);
         vex_printf(",");
         ppHRegX86(am->Xam.IRRS.index);
         vex_printf(",%d)", 1 << am->Xam.IRRS.shift);
         return;
      default:
         vpanic("ppX86AMode");
   }
}

static void addRegUsage_X86AMode ( HRegUsage* u, X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         addHRegUse(u, HRmRead, am->Xam.IR.reg);
         return;
      case Xam_IRRS:
         addHRegUse(u, HRmRead, am->Xam.IRRS.base);
         addHRegUse(u, HRmRead, am->Xam.IRRS.index);
         return;
      default:
         vpanic("addRegUsage_X86AMode");
   }
}

static void mapRegs_X86AMode ( HRegRemap* m, X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         am->Xam.IR.reg = lookupHRegRemap(m, am->Xam.IR.reg);
         return;
      case Xam_IRRS:
         am->Xam.IRRS.base = lookupHRegRemap(m, am->Xam.IRRS.base);
         am->Xam.IRRS.index = lookupHRegRemap(m, am->Xam.IRRS.index);
         return;
      default:
         vpanic("mapRegs_X86AMode");
   }
}

/* --------- Operand, which can be reg, immediate or memory. --------- */

X86RMI* X86RMI_Imm ( UInt imm32 ) {
   X86RMI* op         = LibVEX_Alloc(sizeof(X86RMI));
   op->tag            = Xrmi_Imm;
   op->Xrmi.Imm.imm32 = imm32;
   return op;
}
X86RMI* X86RMI_Reg ( HReg reg ) {
   X86RMI* op       = LibVEX_Alloc(sizeof(X86RMI));
   op->tag          = Xrmi_Reg;
   op->Xrmi.Reg.reg = reg;
   return op;
}
X86RMI* X86RMI_Mem ( X86AMode* am ) {
   X86RMI* op      = LibVEX_Alloc(sizeof(X86RMI));
   op->tag         = Xrmi_Mem;
   op->Xrmi.Mem.am = am;
   return op;
}

void ppX86RMI ( X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         vex_printf("$0x%x", op->Xrmi.Imm.imm32);
         return;
      case Xrmi_Reg: 
         ppHRegX86(op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         ppX86AMode(op->Xrmi.Mem.am);
         return;
     default: 
         vpanic("ppX86RMI");
   }
}

/* An X86RMI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_X86RMI ( HRegUsage* u, X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         return;
      case Xrmi_Reg: 
         addHRegUse(u, HRmRead, op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         addRegUsage_X86AMode(u, op->Xrmi.Mem.am);
         return;
      default: 
         vpanic("addRegUsage_X86RMI");
   }
}

static void mapRegs_X86RMI ( HRegRemap* m, X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         return;
      case Xrmi_Reg: 
         op->Xrmi.Reg.reg = lookupHRegRemap(m, op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         mapRegs_X86AMode(m, op->Xrmi.Mem.am);
         return;
      default: 
         vpanic("mapRegs_X86RMI");
   }
}


/* --------- Operand, which can be reg or immediate only. --------- */

X86RI* X86RI_Imm ( UInt imm32 ) {
   X86RI* op         = LibVEX_Alloc(sizeof(X86RI));
   op->tag           = Xri_Imm;
   op->Xri.Imm.imm32 = imm32;
   return op;
}
X86RI* X86RI_Reg ( HReg reg ) {
   X86RI* op       = LibVEX_Alloc(sizeof(X86RI));
   op->tag         = Xri_Reg;
   op->Xri.Reg.reg = reg;
   return op;
}

void ppX86RI ( X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         vex_printf("$0x%x", op->Xri.Imm.imm32);
         return;
      case Xri_Reg: 
         ppHRegX86(op->Xri.Reg.reg);
         return;
     default: 
         vpanic("ppX86RI");
   }
}

/* An X86RI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_X86RI ( HRegUsage* u, X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         return;
      case Xri_Reg: 
         addHRegUse(u, HRmRead, op->Xri.Reg.reg);
         return;
      default: 
         vpanic("addRegUsage_X86RI");
   }
}

static void mapRegs_X86RI ( HRegRemap* m, X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         return;
      case Xri_Reg: 
         op->Xri.Reg.reg = lookupHRegRemap(m, op->Xri.Reg.reg);
         return;
      default: 
         vpanic("mapRegs_X86RI");
   }
}


/* --------- Operand, which can be reg or memory only. --------- */

X86RM* X86RM_Reg ( HReg reg ) {
   X86RM* op       = LibVEX_Alloc(sizeof(X86RM));
   op->tag         = Xrm_Reg;
   op->Xrm.Reg.reg = reg;
   return op;
}
X86RM* X86RM_Mem ( X86AMode* am ) {
   X86RM* op      = LibVEX_Alloc(sizeof(X86RM));
   op->tag        = Xrm_Mem;
   op->Xrm.Mem.am = am;
   return op;
}

void ppX86RM ( X86RM* op ) {
   switch (op->tag) {
      case Xrm_Mem: 
         ppX86AMode(op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         ppHRegX86(op->Xrm.Reg.reg);
         return;
     default: 
         vpanic("ppX86RM");
   }
}

/* Because an X86RM can be both a source or destination operand, we
   have to supply a mode -- pertaining to the operand as a whole --
   indicating how it's being used. */
static void addRegUsage_X86RM ( HRegUsage* u, X86RM* op, HRegMode mode ) {
   switch (op->tag) {
      case Xrm_Mem: 
         /* Memory is read, written or modified.  So we just want to
            know the regs read by the amode. */
         addRegUsage_X86AMode(u, op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         /* reg is read, written or modified.  Add it in the
            appropriate way. */
         addHRegUse(u, mode, op->Xrm.Reg.reg);
         return;
     default: 
         vpanic("addRegUsage_X86RM");
   }
}

static void mapRegs_X86RM ( HRegRemap* m, X86RM* op )
{
   switch (op->tag) {
      case Xrm_Mem: 
         mapRegs_X86AMode(m, op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         op->Xrm.Reg.reg = lookupHRegRemap(m, op->Xrm.Reg.reg);
         return;
     default: 
         vpanic("mapRegs_X86RM");
   }
}


/* --------- Instructions. --------- */

Char* showX86ScalarSz ( X86ScalarSz sz ) {
   switch (sz) {
      case Xss_16: return "w";
      case Xss_32: return "l";
      default: vpanic("showX86ScalarSz");
   }
}

Char* showX86UnaryOp ( X86UnaryOp op ) {
   switch (op) {
      case Xun_Not: return "not";
      case Xun_Neg: return "neg";
      default: vpanic("showX86UnaryOp");
   }
}

Char* showX86AluOp ( X86AluOp op ) {
   switch (op) {
      case Xalu_MOV:  return "mov";
      case Xalu_CMP:  return "cmp";
      case Xalu_ADD:  return "add";
      case Xalu_SUB:  return "sub";
      case Xalu_ADC:  return "adc";
      case Xalu_SBB:  return "sbb";
      case Xalu_AND:  return "and";
      case Xalu_OR:   return "or";
      case Xalu_XOR:  return "xor";
      case Xalu_MUL:  return "mul";
      default: vpanic("showX86AluOp");
   }
}

Char* showX86ShiftOp ( X86ShiftOp op ) {
   switch (op) {
      case Xsh_SHL: return "shl";
      case Xsh_SHR: return "shr";
      case Xsh_SAR: return "sar";
      case Xsh_ROL: return "rol";
      case Xsh_ROR: return "ror";
      default: vpanic("showX86ShiftOp");
   }
}

Char* showX86FpOp ( X86FpOp op ) {
   switch (op) {
      case Xfp_ADD:    return "add";
      case Xfp_SUB:    return "sub";
      case Xfp_MUL:    return "mul";
      case Xfp_DIV:    return "div";
      case Xfp_SQRT:   return "sqrt";
      case Xfp_NEG:    return "chs";
      case Xfp_ABS:    return "abs";
      case Xfp_MOV:    return "mov";
      case Xfp_SIN:    return "sin";
      case Xfp_COS:    return "cos";
      default: vpanic("showX86FpOp");
   }
}

X86Instr* X86Instr_Alu32R ( X86AluOp op, X86RMI* src, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
   i->tag            = Xin_Alu32R;
   i->Xin.Alu32R.op  = op;
   i->Xin.Alu32R.src = src;
   i->Xin.Alu32R.dst = dst;
   return i;
}
X86Instr* X86Instr_Alu32M ( X86AluOp op, X86RI* src, X86AMode* dst ) {
   X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
   i->tag            = Xin_Alu32M;
   i->Xin.Alu32M.op  = op;
   i->Xin.Alu32M.src = src;
   i->Xin.Alu32M.dst = dst;
   vassert(op != Xalu_MUL);
   return i;
}
X86Instr* X86Instr_Sh32 ( X86ShiftOp op, UInt src, X86RM* dst ) {
   X86Instr* i     = LibVEX_Alloc(sizeof(X86Instr));
   i->tag          = Xin_Sh32;
   i->Xin.Sh32.op  = op;
   i->Xin.Sh32.src = src;
   i->Xin.Sh32.dst = dst;
   return i;
}
X86Instr* X86Instr_Test32  ( X86RI* src, X86RM* dst ) {
   X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
   i->tag            = Xin_Test32;
   i->Xin.Test32.src = src;
   i->Xin.Test32.dst = dst;
   return i;
}
X86Instr* X86Instr_Unary32  ( X86UnaryOp op, X86RM* dst ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_Unary32;
   i->Xin.Unary32.op  = op;
   i->Xin.Unary32.dst = dst;
   return i;
}
X86Instr* X86Instr_MulL ( Bool syned, X86ScalarSz ssz , X86RM* src ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_MulL;
   i->Xin.MulL.syned  = syned;
   i->Xin.MulL.ssz    = ssz;
   i->Xin.MulL.src    = src;
   return i;
}
X86Instr* X86Instr_Div ( Bool syned, X86ScalarSz ssz, X86RM* src ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_Div;
   i->Xin.Div.syned   = syned;
   i->Xin.Div.ssz     = ssz;
   i->Xin.Div.src     = src;
   return i;
}
X86Instr* X86Instr_Sh3232  ( X86ShiftOp op, UInt amt, HReg src, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
   i->tag            = Xin_Sh3232;
   i->Xin.Sh3232.op  = op;
   i->Xin.Sh3232.amt = amt;
   i->Xin.Sh3232.src = src;
   i->Xin.Sh3232.dst = dst;
   vassert(op == Xsh_SHL || op == Xsh_SHR);
   return i;
}
X86Instr* X86Instr_Push( X86RMI* src ) {
   X86Instr* i     = LibVEX_Alloc(sizeof(X86Instr));
   i->tag          = Xin_Push;
   i->Xin.Push.src = src;
   return i;
}
X86Instr* X86Instr_Call ( HReg target ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_Call;
   i->Xin.Call.target = target;
   return i;
}
X86Instr* X86Instr_Goto ( IRJumpKind jk, X86CondCode cond, X86RI* dst ) {
   X86Instr* i      = LibVEX_Alloc(sizeof(X86Instr));
   i->tag           = Xin_Goto;
   i->Xin.Goto.cond = cond;
   i->Xin.Goto.dst  = dst;
   i->Xin.Goto.jk   = jk;
   /* non-Boring conditional jumps are not allowed. */
   vassert(jk == Ijk_Boring || cond == Xcc_ALWAYS);
   return i;
}
X86Instr* X86Instr_CMov32  ( X86CondCode cond, X86RM* src, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_CMov32;
   i->Xin.CMov32.cond = cond;
   i->Xin.CMov32.src  = src;
   i->Xin.CMov32.dst  = dst;
   vassert(cond != Xcc_ALWAYS);
   return i;
}
X86Instr* X86Instr_LoadEX ( UChar szSmall, Bool syned,
                            X86AMode* src, HReg dst ) {
   X86Instr* i           = LibVEX_Alloc(sizeof(X86Instr));
   i->tag                = Xin_LoadEX;
   i->Xin.LoadEX.szSmall = szSmall;
   i->Xin.LoadEX.syned   = syned;
   i->Xin.LoadEX.src     = src;
   i->Xin.LoadEX.dst     = dst;
   vassert(szSmall == 1 || szSmall == 2);
   return i;
}
X86Instr* X86Instr_Store ( UChar sz, HReg src, X86AMode* dst ) {
   X86Instr* i      = LibVEX_Alloc(sizeof(X86Instr));
   i->tag           = Xin_Store;
   i->Xin.Store.sz  = sz;
   i->Xin.Store.src = src;
   i->Xin.Store.dst = dst;
   vassert(sz == 1 || sz == 2);
   return i;
}
X86Instr* X86Instr_Set32 ( X86CondCode cond, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
   i->tag            = Xin_Set32;
   i->Xin.Set32.cond = cond;
   i->Xin.Set32.dst  = dst;
   return i;
}
X86Instr* X86Instr_Bsfr32 ( Bool isFwds, HReg src, HReg dst )
{
   X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
   i->tag               = Xin_Bsfr32;
   i->Xin.Bsfr32.isFwds = isFwds;
   i->Xin.Bsfr32.src    = src;
   i->Xin.Bsfr32.dst    = dst;
   return i;
}
X86Instr* X86Instr_FpUnary ( X86FpOp op, HReg src, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_FpUnary;
   i->Xin.FpUnary.op  = op;
   i->Xin.FpUnary.src = src;
   i->Xin.FpUnary.dst = dst;
   return i;
}
X86Instr* X86Instr_FpBinary ( X86FpOp op, HReg srcL, HReg srcR, HReg dst ) {
   X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
   i->tag               = Xin_FpBinary;
   i->Xin.FpBinary.op   = op;
   i->Xin.FpBinary.srcL = srcL;
   i->Xin.FpBinary.srcR = srcR;
   i->Xin.FpBinary.dst  = dst;
   return i;
}
X86Instr* X86Instr_FpLdSt ( Bool isLoad, UChar sz, HReg reg, X86AMode* addr ) {
   X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
   i->tag               = Xin_FpLdSt;
   i->Xin.FpLdSt.isLoad = isLoad;
   i->Xin.FpLdSt.sz     = sz;
   i->Xin.FpLdSt.reg    = reg;
   i->Xin.FpLdSt.addr   = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}
X86Instr* X86Instr_FpLdStI ( Bool isLoad, UChar sz, HReg reg, X86AMode* addr ) {
   X86Instr* i           = LibVEX_Alloc(sizeof(X86Instr));
   i->tag                = Xin_FpLdStI;
   i->Xin.FpLdStI.isLoad = isLoad;
   i->Xin.FpLdStI.sz     = sz;
   i->Xin.FpLdStI.reg    = reg;
   i->Xin.FpLdStI.addr   = addr;
   vassert(sz == 2 || sz == 4 || sz == 8);
   return i;
}
X86Instr* X86Instr_FpCMov ( X86CondCode cond, HReg src, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
   i->tag             = Xin_FpCMov;
   i->Xin.FpCMov.cond = cond;
   i->Xin.FpCMov.src  = src;
   i->Xin.FpCMov.dst  = dst;
   vassert(cond != Xcc_ALWAYS);
   return i;
}
X86Instr* X86Instr_FpLdStCW ( Bool isLoad, X86AMode* addr )
{
   X86Instr* i            = LibVEX_Alloc(sizeof(X86Instr));
   i->tag                 = Xin_FpLdStCW;
   i->Xin.FpLdStCW.isLoad = isLoad;
   i->Xin.FpLdStCW.addr   = addr;
   return i;
}
X86Instr* X86Instr_FpCmp ( HReg srcL, HReg srcR, HReg dst )
{
   X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
   i->tag            = Xin_FpCmp;
   i->Xin.FpCmp.srcL = srcL;
   i->Xin.FpCmp.srcR = srcR;
   i->Xin.FpCmp.dst  = dst;
   return i;
}


void ppX86Instr ( X86Instr* i ) {
   switch (i->tag) {
      case Xin_Alu32R:
         vex_printf("%sl ", showX86AluOp(i->Xin.Alu32R.op));
         ppX86RMI(i->Xin.Alu32R.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         vex_printf("%sl ", showX86AluOp(i->Xin.Alu32M.op));
         ppX86RI(i->Xin.Alu32M.src);
         vex_printf(",");
         ppX86AMode(i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         vex_printf("%sl ", showX86ShiftOp(i->Xin.Sh32.op));
         if (i->Xin.Sh32.src == 0)
           vex_printf("%%cl,"); 
         else 
            vex_printf("$%d,", i->Xin.Sh32.src);
         ppX86RM(i->Xin.Sh32.dst);
         return;
      case Xin_Test32:
         vex_printf("testl ");
         ppX86RI(i->Xin.Test32.src);
         vex_printf(",");
         ppX86RM(i->Xin.Test32.dst);
         return;
      case Xin_Unary32:
         vex_printf("%sl ", showX86UnaryOp(i->Xin.Unary32.op));
         ppX86RM(i->Xin.Unary32.dst);
         return;
      case Xin_MulL:
         vex_printf("%cmul%s ",
                    i->Xin.MulL.syned ? 's' : 'u',
                    showX86ScalarSz(i->Xin.MulL.ssz));
         ppX86RM(i->Xin.MulL.src);
         return;
      case Xin_Div:
         vex_printf("%cdiv%s ",
                    i->Xin.Div.syned ? 's' : 'u',
                    showX86ScalarSz(i->Xin.Div.ssz));
         ppX86RM(i->Xin.Div.src);
         return;
      case Xin_Sh3232:
         vex_printf("%sdl ", showX86ShiftOp(i->Xin.Sh3232.op));
         if (i->Xin.Sh3232.amt == 0)
           vex_printf(" %%cl,"); 
         else 
            vex_printf(" $%d,", i->Xin.Sh3232.amt);
         ppHRegX86(i->Xin.Sh3232.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Sh3232.dst);
         return;
      case Xin_Push:
         vex_printf("pushl ");
         ppX86RMI(i->Xin.Push.src);
         return;
      case Xin_Call:
         vex_printf("call *");
         ppHRegX86(i->Xin.Call.target);
         break;
      case Xin_Goto:
         if (i->Xin.Goto.jk == Ijk_ClientReq 
             || i->Xin.Goto.jk == Ijk_Syscall) {
            vex_printf("movl $");
            ppIRJumpKind(i->Xin.Goto.jk);
            vex_printf(", %%ebp ; ");
         }
         if (i->Xin.Goto.cond == Xcc_ALWAYS) {
            vex_printf("movl ");
            ppX86RI(i->Xin.Goto.dst);
            vex_printf(",%%eax ; ret");
         } else {
            vex_printf("if (%%eflags.%s) { movl ", 
                       showX86CondCode(i->Xin.Goto.cond));
            ppX86RI(i->Xin.Goto.dst);
            vex_printf(",%%eax ; ret }");
         }
         return;
      case Xin_CMov32:
         vex_printf("cmov%s ", showX86CondCode(i->Xin.CMov32.cond));
         ppX86RM(i->Xin.CMov32.src);
         vex_printf(",");
         ppHRegX86(i->Xin.CMov32.dst);
         return;
      case Xin_LoadEX:
         vex_printf("mov%c%cl ",
                    i->Xin.LoadEX.syned ? 's' : 'z',
                    i->Xin.LoadEX.szSmall==1 ? 'b' : 'w');
         ppX86AMode(i->Xin.LoadEX.src);
         vex_printf(",");
         ppHRegX86(i->Xin.LoadEX.dst);
         return;
      case Xin_Store:
         vex_printf("mov%c ", i->Xin.Store.sz==1 ? 'b' : 'w');
         ppHRegX86(i->Xin.Store.src);
         vex_printf(",");
         ppX86AMode(i->Xin.Store.dst);
         return;
      case Xin_Set32:
         vex_printf("setl%s ", showX86CondCode(i->Xin.Set32.cond));
         ppHRegX86(i->Xin.Set32.dst);
         return;
      case Xin_Bsfr32:
         vex_printf("bs%cl ", i->Xin.Bsfr32.isFwds ? 'f' : 'r');
         ppHRegX86(i->Xin.Bsfr32.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Bsfr32.dst);
         return;
      case Xin_FpUnary:
         vex_printf("g%sD ", showX86FpOp(i->Xin.FpUnary.op));
         ppHRegX86(i->Xin.FpUnary.src);
         vex_printf(",");
         ppHRegX86(i->Xin.FpUnary.dst);
         break;
      case Xin_FpBinary:
         vex_printf("g%sD ", showX86FpOp(i->Xin.FpBinary.op));
         ppHRegX86(i->Xin.FpBinary.srcL);
         vex_printf(",");
         ppHRegX86(i->Xin.FpBinary.srcR);
         vex_printf(",");
         ppHRegX86(i->Xin.FpBinary.dst);
         break;
      case Xin_FpLdSt:
         if (i->Xin.FpLdSt.isLoad) {
            vex_printf("gld%c " , i->Xin.FpLdSt.sz==8 ? 'D' : 'F');
            ppX86AMode(i->Xin.FpLdSt.addr);
            vex_printf(", ");
            ppHRegX86(i->Xin.FpLdSt.reg);
         } else {
            vex_printf("gst%c " , i->Xin.FpLdSt.sz==8 ? 'D' : 'F');
            ppHRegX86(i->Xin.FpLdSt.reg);
            vex_printf(", ");
            ppX86AMode(i->Xin.FpLdSt.addr);
         }
         return;
      case Xin_FpLdStI:
         if (i->Xin.FpLdStI.isLoad) {
            vex_printf("gild%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
                                  i->Xin.FpLdStI.sz==4 ? "l" : "w");
            ppX86AMode(i->Xin.FpLdStI.addr);
            vex_printf(", ");
            ppHRegX86(i->Xin.FpLdStI.reg);
         } else {
            vex_printf("gist%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
                                  i->Xin.FpLdStI.sz==4 ? "l" : "w");
            ppHRegX86(i->Xin.FpLdStI.reg);
            vex_printf(", ");
            ppX86AMode(i->Xin.FpLdStI.addr);
         }
         return;
      case Xin_FpCMov:
         vex_printf("gcmov%s ", showX86CondCode(i->Xin.FpCMov.cond));
         ppHRegX86(i->Xin.FpCMov.src);
         vex_printf(",");
         ppHRegX86(i->Xin.FpCMov.dst);
         return;
      case Xin_FpLdStCW:
         vex_printf(i->Xin.FpLdStCW.isLoad ? "fldcw " : "fstcw ");
         ppX86AMode(i->Xin.FpLdStCW.addr);
         return;
      case Xin_FpCmp:
         vex_printf("gcmp ");
         ppHRegX86(i->Xin.FpCmp.srcL);
         vex_printf(",");
         ppHRegX86(i->Xin.FpCmp.srcR);
         vex_printf(",");
         ppHRegX86(i->Xin.FpCmp.dst);
         break;
      default:
         vpanic("ppX86Instr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_X86Instr (HRegUsage* u, X86Instr* i)
{
   initHRegUsage(u);
   switch (i->tag) {
      case Xin_Alu32R:
         addRegUsage_X86RMI(u, i->Xin.Alu32R.src);
         if (i->Xin.Alu32R.op == Xalu_MOV) {
            addHRegUse(u, HRmWrite,  i->Xin.Alu32R.dst);
            return;
         }
         if (i->Xin.Alu32R.op == Xalu_CMP) { 
            addHRegUse(u, HRmRead,  i->Xin.Alu32R.dst);
            return;
         }
         addHRegUse(u, HRmModify, i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         addRegUsage_X86RI(u, i->Xin.Alu32M.src);
         addRegUsage_X86AMode(u, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         addRegUsage_X86RM(u, i->Xin.Sh32.dst, HRmModify);
         if (i->Xin.Sh32.src == 0)
            addHRegUse(u, HRmRead, hregX86_ECX());
         return;
      case Xin_Test32:
         addRegUsage_X86RI(u, i->Xin.Test32.src);
         addRegUsage_X86RM(u, i->Xin.Test32.dst, HRmRead);
         return;
      case Xin_Unary32:
         addRegUsage_X86RM(u, i->Xin.Unary32.dst, HRmModify);
         return;
      case Xin_MulL:
         addRegUsage_X86RM(u, i->Xin.MulL.src, HRmRead);
         addHRegUse(u, HRmModify, hregX86_EAX());
         addHRegUse(u, HRmWrite, hregX86_EDX());
         return;
      case Xin_Div:
         addRegUsage_X86RM(u, i->Xin.Div.src, HRmRead);
         addHRegUse(u, HRmModify, hregX86_EAX());
         addHRegUse(u, HRmModify, hregX86_EDX());
         return;
      case Xin_Sh3232:
         addHRegUse(u, HRmRead, i->Xin.Sh3232.src);
         addHRegUse(u, HRmModify, i->Xin.Sh3232.dst);
         if (i->Xin.Sh3232.amt == 0)
            addHRegUse(u, HRmRead, hregX86_ECX());
         return;
      case Xin_Push:
         addRegUsage_X86RMI(u, i->Xin.Push.src);
         addHRegUse(u, HRmModify, hregX86_ESP());
         return;
      case Xin_Call:
         addHRegUse(u, HRmRead, i->Xin.Call.target);
         /* claim it trashes all the callee-saved regs */
         /* which I believe to be %eax,%ecx,%edx. */
         addHRegUse(u, HRmWrite, hregX86_EAX());
         addHRegUse(u, HRmWrite, hregX86_ECX());
         addHRegUse(u, HRmWrite, hregX86_EDX());
         return;
      case Xin_Goto:
         addRegUsage_X86RI(u, i->Xin.Goto.dst);
         addHRegUse(u, HRmWrite, hregX86_EAX());
         if (i->Xin.Goto.jk == Ijk_ClientReq 
             || i->Xin.Goto.jk == Ijk_Syscall)
            addHRegUse(u, HRmWrite, hregX86_EBP());
         return;
      case Xin_CMov32:
         addRegUsage_X86RM(u, i->Xin.CMov32.src, HRmRead);
         addHRegUse(u, HRmModify, i->Xin.CMov32.dst);
         return;
      case Xin_LoadEX:
         addRegUsage_X86AMode(u, i->Xin.LoadEX.src);
         addHRegUse(u, HRmWrite, i->Xin.LoadEX.dst);
         return;
      case Xin_Store:
         addHRegUse(u, HRmRead, i->Xin.Store.src);
         addRegUsage_X86AMode(u, i->Xin.Store.dst);
         return;
      case Xin_Set32:
         addHRegUse(u, HRmWrite, i->Xin.Set32.dst);
         return;
      case Xin_Bsfr32:
         addHRegUse(u, HRmRead, i->Xin.Bsfr32.src);
         addHRegUse(u, HRmWrite, i->Xin.Bsfr32.dst);
         return;
      case Xin_FpUnary:
         addHRegUse(u, HRmRead, i->Xin.FpUnary.src);
         addHRegUse(u, HRmWrite, i->Xin.FpUnary.dst);
         return;
      case Xin_FpBinary:
         addHRegUse(u, HRmRead, i->Xin.FpBinary.srcL);
         addHRegUse(u, HRmRead, i->Xin.FpBinary.srcR);
         addHRegUse(u, HRmWrite, i->Xin.FpBinary.dst);
         return;
      case Xin_FpLdSt:
         addRegUsage_X86AMode(u, i->Xin.FpLdSt.addr);
         addHRegUse(u, i->Xin.FpLdSt.isLoad ? HRmWrite : HRmRead,
                       i->Xin.FpLdSt.reg);
         return;
      case Xin_FpLdStI:
         addRegUsage_X86AMode(u, i->Xin.FpLdStI.addr);
         addHRegUse(u, i->Xin.FpLdStI.isLoad ? HRmWrite : HRmRead,
                       i->Xin.FpLdStI.reg);
         return;
      case Xin_FpCMov:
         addHRegUse(u, HRmRead, i->Xin.FpCMov.src);
         addHRegUse(u, HRmModify, i->Xin.FpCMov.dst);
         return;
      case Xin_FpLdStCW:
         addRegUsage_X86AMode(u, i->Xin.FpLdStCW.addr);
         return;
      case Xin_FpCmp:
         addHRegUse(u, HRmRead, i->Xin.FpCmp.srcL);
         addHRegUse(u, HRmRead, i->Xin.FpCmp.srcR);
         addHRegUse(u, HRmWrite, i->Xin.FpCmp.dst);
         addHRegUse(u, HRmWrite, hregX86_EAX());
         return;
      default:
         ppX86Instr(i);
         vpanic("getRegUsage_X86Instr");
   }
}

/* local helper */
static void mapReg(HRegRemap* m, HReg* r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_X86Instr (HRegRemap* m, X86Instr* i)
{
   switch (i->tag) {
      case Xin_Alu32R:
         mapRegs_X86RMI(m, i->Xin.Alu32R.src);
         mapReg(m, &i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         mapRegs_X86RI(m, i->Xin.Alu32M.src);
         mapRegs_X86AMode(m, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         mapRegs_X86RM(m, i->Xin.Sh32.dst);
         return;
      case Xin_Test32:
         mapRegs_X86RI(m, i->Xin.Test32.src);
         mapRegs_X86RM(m, i->Xin.Test32.dst);
         return;
      case Xin_Unary32:
         mapRegs_X86RM(m, i->Xin.Unary32.dst);
         return;
      case Xin_MulL:
         mapRegs_X86RM(m, i->Xin.MulL.src);
         return;
      case Xin_Div:
         mapRegs_X86RM(m, i->Xin.Div.src);
         return;
      case Xin_Sh3232:
         mapReg(m, &i->Xin.Sh3232.src);
         mapReg(m, &i->Xin.Sh3232.dst);
         return;
      case Xin_Push:
         mapRegs_X86RMI(m, i->Xin.Push.src);
         return;
      case Xin_Call:
         mapReg(m, &i->Xin.Call.target);
         return;
      case Xin_Goto:
         mapRegs_X86RI(m, i->Xin.Goto.dst);
         return;
      case Xin_CMov32:
         mapRegs_X86RM(m, i->Xin.CMov32.src);
         mapReg(m, &i->Xin.CMov32.dst);
         return;
      case Xin_LoadEX:
         mapRegs_X86AMode(m, i->Xin.LoadEX.src);
         mapReg(m, &i->Xin.LoadEX.dst);
         return;
      case Xin_Store:
         mapReg(m, &i->Xin.Store.src);
         mapRegs_X86AMode(m, i->Xin.Store.dst);
         return;
      case Xin_Set32:
         mapReg(m, &i->Xin.Set32.dst);
         return;
      case Xin_Bsfr32:
         mapReg(m, &i->Xin.Bsfr32.src);
         mapReg(m, &i->Xin.Bsfr32.dst);
         return;
      case Xin_FpUnary:
         mapReg(m, &i->Xin.FpUnary.src);
         mapReg(m, &i->Xin.FpUnary.dst);
         return;
      case Xin_FpBinary:
         mapReg(m, &i->Xin.FpBinary.srcL);
         mapReg(m, &i->Xin.FpBinary.srcR);
         mapReg(m, &i->Xin.FpBinary.dst);
         return;
      case Xin_FpLdSt:
         mapRegs_X86AMode(m, i->Xin.FpLdSt.addr);
         mapReg(m, &i->Xin.FpLdSt.reg);
         return;
      case Xin_FpLdStI:
         mapRegs_X86AMode(m, i->Xin.FpLdStI.addr);
         mapReg(m, &i->Xin.FpLdStI.reg);
         return;
      case Xin_FpCMov:
         mapReg(m, &i->Xin.FpCMov.src);
         mapReg(m, &i->Xin.FpCMov.dst);
         return;
      case Xin_FpLdStCW:
         mapRegs_X86AMode(m, i->Xin.FpLdStCW.addr);
         return;
      case Xin_FpCmp:
         mapReg(m, &i->Xin.FpCmp.srcL);
         mapReg(m, &i->Xin.FpCmp.srcR);
         mapReg(m, &i->Xin.FpCmp.dst);
         return;
      default:
         ppX86Instr(i);
         vpanic("mapRegs_X86Instr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_X86Instr ( X86Instr* i, HReg* src, HReg* dst )
{
   /* Moves between integer regs */
   if (i->tag == Xin_Alu32R) {
      if (i->Xin.Alu32R.op != Xalu_MOV)
         return False;
      if (i->Xin.Alu32R.src->tag != Xrmi_Reg)
         return False;
      *src = i->Xin.Alu32R.src->Xrmi.Reg.reg;
      *dst = i->Xin.Alu32R.dst;
      return True;
   }
   /* Moves between FP regs */
   if (i->tag == Xin_FpUnary) {
      if (i->Xin.FpUnary.op != Xfp_MOV)
         return False;
      *src = i->Xin.FpUnary.src;
      *dst = i->Xin.FpUnary.dst;
      return True;
   }
   return False;
}


/* x86 spill/reload using the hacked104 testbed.  Spill slots
   start at word 55, and there are 100 in total. 
*/

X86Instr* genSpill_X86 ( HReg rreg, Int offset )
{
   Int base = 4 * 55;
   vassert(offset >= 0);
   vassert(offset <= 4*(100-1));
   vassert(!hregIsVirtual(rreg));
   switch (hregClass(rreg)) {
      case HRcInt:
        return
        X86Instr_Alu32M ( Xalu_MOV, X86RI_Reg(rreg), 
                          X86AMode_IR(offset + base, 
                                      hregX86_EBP()));
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_X86: unimplemented regclass");
   }
}

X86Instr* genReload_X86 ( HReg rreg, Int offset )
{
   Int base = 4 * 55;
   vassert(offset >= 0);
   vassert(offset <= 4*(100-1));
   vassert(!hregIsVirtual(rreg));
   switch (hregClass(rreg)) {
      case HRcInt:
        return
        X86Instr_Alu32R ( Xalu_MOV, 
                          X86RMI_Mem(X86AMode_IR(offset + base, 
                                                 hregX86_EBP())),
                          rreg );
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_X86: unimplemented regclass");
   }
}


/* --------- The x86 assembler (bleh.) --------- */

static UInt iregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 7);
   return n;
}

static UInt fregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcFloat);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 3);
   return n;
}

static UChar mkModRegRM ( UChar mod, UChar reg, UChar regmem )
{
   return ((mod & 3) << 6) | ((reg & 7) << 3) | (regmem & 7);
}

static UChar mkSIB ( Int shift, Int regindex, Int regbase )
{
   return ((shift & 3) << 6) | ((regindex & 7) << 3) | (regbase & 7);
}

static UChar* emit32 ( UChar* p, UInt w32 )
{
   *p++ = (w32)       & 0x000000FF;
   *p++ = (w32 >>  8) & 0x000000FF;
   *p++ = (w32 >> 16) & 0x000000FF;
   *p++ = (w32 >> 24) & 0x000000FF;
   return p;
}

/* Does a sign-extend of the lowest 8 bits give 
   the original number? */
static Bool fits8bits ( UInt w32 )
{
   Int i32 = (Int)w32;
   return i32 == ((i32 << 24) >> 24);
}


/* Forming mod-reg-rm bytes and scale-index-base bytes.

     greg,  0(ereg)    |  ereg != ESP && ereg != EBP
                       =  00 greg ereg

     greg,  d8(ereg)   |  ereg != ESP
                       =  01 greg ereg, d8

     greg,  d32(ereg)  |  ereg != ESP
                       =  10 greg ereg, d32

     greg,  d8(%esp)   =  01 greg 100, 0x24, d8

     -----------------------------------------------

     greg,  d8(base,index,scale)  
               |  index != ESP
               =  01 greg 100, scale index base, d8

     greg,  d32(base,index,scale)
               |  index != ESP
               =  10 greg 100, scale index base, d32
*/
static UChar* doAMode_M ( UChar* p, HReg greg, X86AMode* am ) 
{
   if (am->tag == Xam_IR) {
      if (am->Xam.IR.imm == 0 
          && am->Xam.IR.reg != hregX86_ESP()
          && am->Xam.IR.reg != hregX86_EBP() ) {
         *p++ = mkModRegRM(0, iregNo(greg), iregNo(am->Xam.IR.reg));
         return p;
      }
      if (fits8bits(am->Xam.IR.imm)
          && am->Xam.IR.reg != hregX86_ESP()) {
         *p++ = mkModRegRM(1, iregNo(greg), iregNo(am->Xam.IR.reg));
         *p++ = am->Xam.IR.imm & 0xFF;
         return p;
      }
      if (am->Xam.IR.reg != hregX86_ESP()) {
         *p++ = mkModRegRM(2, iregNo(greg), iregNo(am->Xam.IR.reg));
         p = emit32(p, am->Xam.IR.imm);
         return p;
      }
      if (am->Xam.IR.reg == hregX86_ESP()
          && fits8bits(am->Xam.IR.imm)) {
 	 *p++ = mkModRegRM(1, iregNo(greg), 4);
         *p++ = 0x24;
         *p++ = am->Xam.IR.imm & 0xFF;
         return p;
      }
      ppX86AMode(am);
      vpanic("doAMode_M: can't emit amode IR");
      /*NOTREACHED*/
   }
   if (am->tag == Xam_IRRS) {
      if (fits8bits(am->Xam.IRRS.imm)
          && am->Xam.IRRS.index != hregX86_ESP()) {
         *p++ = mkModRegRM(1, iregNo(greg), 4);
         *p++ = mkSIB(am->Xam.IRRS.shift, am->Xam.IRRS.index, 
                                          am->Xam.IRRS.base);
         *p++ = am->Xam.IRRS.imm & 0xFF;
         return p;
      }
      if (am->Xam.IRRS.index != hregX86_ESP()) {
         *p++ = mkModRegRM(2, iregNo(greg), 4);
         *p++ = mkSIB(am->Xam.IRRS.shift, am->Xam.IRRS.index,
                                          am->Xam.IRRS.base);
         p = emit32(p, am->Xam.IRRS.imm);
         return p;
      }
      ppX86AMode(am);
      vpanic("doAMode_M: can't emit amode IRRS");
      /*NOTREACHED*/
   }
   vpanic("doAMode_M: unknown amode");
   /*NOTREACHED*/
}


/* Emit a mod-reg-rm byte when the rm bit denotes a reg. */
static UChar* doAMode_R ( UChar* p, HReg greg, HReg ereg ) 
{
   *p++ = mkModRegRM(3, iregNo(greg), iregNo(ereg));
   return p;
}


/* Emit ffree %st(7) */
static UChar* do_ffree_st7 ( UChar* p )
{
   *p++ = 0xDD;
   *p++ = 0xC7;
   return p;
}

/* Emit fstp %st(i), 1 <= i <= 5 */
static UChar* do_fstp_st ( UChar* p, Int i )
{
   vassert(1 <= i && i <= 5);
   *p++ = 0xDD;
   *p++ = 0xD8+i;
   return p;
}

/* Emit fld %st(i), 0 <= i <= 5 */
static UChar* do_fld_st ( UChar* p, Int i )
{
   vassert(0 <= i && i <= 5);
   *p++ = 0xD9;
   *p++ = 0xC0+i;
   return p;
}

/* Emit f<op> %st(0) */
static UChar* do_fop1_st ( UChar* p, X86FpOp op )
{
   switch (op) {
      case Xfp_NEG:    *p++ = 0xD9; *p++ = 0xE0; break;
      case Xfp_ABS:    *p++ = 0xD9; *p++ = 0xE1; break;
      case Xfp_SQRT:   *p++ = 0xD9; *p++ = 0xFA; break;
      case Xfp_ROUND:  *p++ = 0xD9; *p++ = 0xFC; break;
      case Xfp_SIN:    *p++ = 0xD9; *p++ = 0xFE; break;
      case Xfp_COS:    *p++ = 0xD9; *p++ = 0xFF; break;
      default: vpanic("do_fop1_st: unknown op");
   }
   return p;
}

/* Emit f<op> %st(i), 1 <= i <= 5 */
static UChar* do_fop2_st ( UChar* p, X86FpOp op, Int i )
{
#  define fake(_n) mkHReg((_n), HRcInt, False)
   Int subopc;
   switch (op) {
      case Xfp_ADD: subopc = 0; break;
      case Xfp_SUB: subopc = 4; break;
      case Xfp_MUL: subopc = 1; break;
      case Xfp_DIV: subopc = 6; break;
      default: vpanic("do_fop2_st: unknown op");
   }
   *p++ = 0xD8;
   p    = doAMode_R(p, fake(subopc), fake(i));
   return p;
#  undef fake
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */

Int emit_X86Instr ( UChar* buf, Int nbuf, X86Instr* i )
{
   UInt opc, opc_rr, subopc_imm, opc_imma, opc_cl, opc_imm, subopc;

   UChar* p = &buf[0];
   UChar* ptmp;
   vassert(nbuf >= 32);

   /* Wrap an integer as a int register, for use assembling
      GrpN insns, in which the greg field is used as a sub-opcode
      and does not really contain a register. */
#  define fake(_n) mkHReg((_n), HRcInt, False)

   switch (i->tag) {

   case Xin_Alu32R:
      /* Deal specially with MOV */
      if (i->Xin.Alu32R.op == Xalu_MOV) {
         switch (i->Xin.Alu32R.src->tag) {
            case Xrmi_Imm:
               *p++ = 0xB8 + iregNo(i->Xin.Alu32R.dst);
               p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
               goto done;
            case Xrmi_Reg:
               *p++ = 0x89;
               p = doAMode_R(p, i->Xin.Alu32R.src->Xrmi.Reg.reg,
                                i->Xin.Alu32R.dst);
               goto done;
            case Xrmi_Mem:
               *p++ = 0x8B;
               p = doAMode_M(p, i->Xin.Alu32R.dst, 
                                i->Xin.Alu32R.src->Xrmi.Mem.am);
               goto done;
            default:
               goto bad;
         }
      }
      /* MUL */
      if (i->Xin.Alu32R.op == Xalu_MUL) {
         switch (i->Xin.Alu32R.src->tag) {
            case Xrmi_Reg:
               *p++ = 0x0F;
               *p++ = 0xAF;
               p = doAMode_R(p, i->Xin.Alu32R.dst,
                                i->Xin.Alu32R.src->Xrmi.Reg.reg);
               goto done;
            case Xrmi_Mem:
               *p++ = 0x0F;
               *p++ = 0xAF;
               p = doAMode_M(p, i->Xin.Alu32R.dst,
                                i->Xin.Alu32R.src->Xrmi.Mem.am);
               goto done;
            case Xrmi_Imm:
               if (fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
                  *p++ = 0x6B;
                  p = doAMode_R(p, i->Xin.Alu32R.dst, i->Xin.Alu32R.dst);
                  *p++ = 0xFF & i->Xin.Alu32R.src->Xrmi.Imm.imm32;
               } else {
                  *p++ = 0x69;
                  p = doAMode_R(p, i->Xin.Alu32R.dst, i->Xin.Alu32R.dst);
                  p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
               }
               goto done;
            default:
               goto bad;
         }
      }
      /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP */
      opc = opc_rr = subopc_imm = opc_imma = 0;
      switch (i->Xin.Alu32R.op) {
         case Xalu_ADD: opc = 0x03; opc_rr = 0x01; 
                        subopc_imm = 0; opc_imma = 0x05; break;
         case Xalu_SUB: opc = 0x2B; opc_rr = 0x29; 
                        subopc_imm = 5; opc_imma = 0x2D; break;
         case Xalu_AND: opc = 0x23; opc_rr = 0x21; 
                        subopc_imm = 4; opc_imma = 0x25; break;
         case Xalu_XOR: opc = 0x33; opc_rr = 0x31; 
                        subopc_imm = 6; opc_imma = 0x35; break;
         case Xalu_OR:  opc = 0x0B; opc_rr = 0x09; 
                        subopc_imm = 1; opc_imma = 0x0D; break;
         case Xalu_CMP: opc = 0x3B; opc_rr = 0x39; 
                        subopc_imm = 7; opc_imma = 0x3D; break;
         default: goto bad;
      }
      switch (i->Xin.Alu32R.src->tag) {
         case Xrmi_Imm:
            if (i->Xin.Alu32R.dst == hregX86_EAX()
                && !fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
               *p++ = opc_imma;
               p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
            } else
               if (fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
               *p++ = 0x83; 
               p    = doAMode_R(p, fake(subopc_imm), i->Xin.Alu32R.dst);
               *p++ = 0xFF & i->Xin.Alu32R.src->Xrmi.Imm.imm32;
            } else {
               *p++ = 0x81; 
               p    = doAMode_R(p, fake(subopc_imm), i->Xin.Alu32R.dst);
               p    = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
            }
            goto done;
         case Xrmi_Reg:
            *p++ = opc_rr;
            p = doAMode_R(p, i->Xin.Alu32R.src->Xrmi.Reg.reg,
                             i->Xin.Alu32R.dst);
            goto done;
         case Xrmi_Mem:
            *p++ = opc;
            p = doAMode_M(p, i->Xin.Alu32R.dst,
                             i->Xin.Alu32R.src->Xrmi.Mem.am);
            goto done;
         default: 
            goto bad;
      }
      break;

   case Xin_Alu32M:
      /* Deal specially with MOV */
      if (i->Xin.Alu32M.op == Xalu_MOV) {
         switch (i->Xin.Alu32M.src->tag) {
            case Xri_Reg:
               *p++ = 0x89;
               p = doAMode_M(p, i->Xin.Alu32M.src->Xri.Reg.reg,
                                i->Xin.Alu32M.dst);
               goto done;
            case Xri_Imm:
               *p++ = 0xC7;
               p = doAMode_M(p, fake(0), i->Xin.Alu32M.dst);
               p = emit32(p, i->Xin.Alu32M.src->Xri.Imm.imm32);
               goto done;
            default: 
               goto bad;
         }
      }
      /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP.  MUL is not
         allowed here. */
      opc = subopc_imm = opc_imma = 0;
      switch (i->Xin.Alu32M.op) {
         case Xalu_ADD: opc = 0x01; subopc_imm = 0; break;
         case Xalu_SUB: opc = 0x29; subopc_imm = 5; break;
         default: goto bad;
      }
      switch (i->Xin.Alu32M.src->tag) {
         case Xri_Reg:
            *p++ = opc;
            p = doAMode_M(p, i->Xin.Alu32M.src->Xri.Reg.reg,
                             i->Xin.Alu32M.dst);
            goto done;
         case Xri_Imm:
            if (fits8bits(i->Xin.Alu32M.src->Xri.Imm.imm32)) {
               *p++ = 0x83;
               p    = doAMode_M(p, fake(subopc_imm), i->Xin.Alu32M.dst);
               *p++ = 0xFF & i->Xin.Alu32M.src->Xri.Imm.imm32;
               goto done;
            } else {
               *p++ = 0x81;
               p    = doAMode_M(p, fake(subopc_imm), i->Xin.Alu32M.dst);
               p    = emit32(p, i->Xin.Alu32M.src->Xri.Imm.imm32);
               goto done;
            }
         default: 
            goto bad;
      }
      break;

   case Xin_Sh32:
      opc_cl = opc_imm = subopc = 0;
      switch (i->Xin.Sh32.op) {
         case Xsh_SHR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 5; break;
         case Xsh_SAR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 7; break;
         case Xsh_SHL: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 4; break;
         default: goto bad;
      }
      if (i->Xin.Sh32.src == 0) {
         *p++ = opc_cl;
         switch (i->Xin.Sh32.dst->tag) {
            case Xrm_Reg:
               p = doAMode_R(p, fake(subopc), 
                                i->Xin.Sh32.dst->Xrm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      } else {
         *p++ = opc_imm;
         switch (i->Xin.Sh32.dst->tag) {
            case Xrm_Reg:
               p = doAMode_R(p, fake(subopc), 
                                i->Xin.Sh32.dst->Xrm.Reg.reg);
               *p++ = (UChar)(i->Xin.Sh32.src);
               goto done;
            default:
               goto bad;
         }
      }
      break;

   case Xin_Test32:
      if (i->Xin.Test32.src->tag == Xri_Imm
          && i->Xin.Test32.dst->tag == Xrm_Reg) {
         /* testl $imm32, %reg */
         *p++ = 0xF7;
         p = doAMode_R(p, fake(0), i->Xin.Test32.dst->Xrm.Reg.reg);
         p = emit32(p, i->Xin.Test32.src->Xri.Imm.imm32);
         goto done;
      }
      break;

   case Xin_Unary32:
      if (i->Xin.Unary32.op == Xun_Not) {
         *p++ = 0xF7;
         if (i->Xin.Unary32.dst->tag == Xrm_Reg) {
            p = doAMode_R(p, fake(2), i->Xin.Unary32.dst->Xrm.Reg.reg);
            goto done;
         } else {
            goto bad;
         }
      }
      break;

   case Xin_MulL:
      subopc = i->Xin.MulL.syned ? 5 : 4;
      if (i->Xin.MulL.ssz == Xss_32) {
         *p++ = 0xF7;
         switch (i->Xin.MulL.src->tag)  {
            case Xrm_Mem:
               p = doAMode_M(p, fake(subopc),
                                i->Xin.MulL.src->Xrm.Mem.am);
               goto done;
            case Xrm_Reg:
               p = doAMode_R(p, fake(subopc), 
                                i->Xin.MulL.src->Xrm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      }
      break;

   case Xin_Div:
      subopc = i->Xin.Div.syned ? 7 : 6;
      if (i->Xin.Div.ssz == Xss_32) {
         *p++ = 0xF7;
         switch (i->Xin.Div.src->tag)  {
            case Xrm_Mem:
               p = doAMode_M(p, fake(subopc),
                                i->Xin.Div.src->Xrm.Mem.am);
               goto done;
            case Xrm_Reg:
               p = doAMode_R(p, fake(subopc), 
                                i->Xin.Div.src->Xrm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      }
      break;

   case Xin_Sh3232:
      vassert(i->Xin.Sh3232.op == Xsh_SHL || i->Xin.Sh3232.op == Xsh_SHR);
      if (i->Xin.Sh3232.amt == 0) {
         /* shldl/shrdl by %cl */
         *p++ = 0x0F;
         if (i->Xin.Sh3232.op == Xsh_SHL) {
            *p++ = 0xA5;
         } else {
            *p++ = 0xAD;
         }
         p = doAMode_R(p, i->Xin.Sh3232.src, i->Xin.Sh3232.dst);
         goto done;
      }
      break;

   case Xin_Push:
      switch (i->Xin.Push.src->tag) {
         case Xrmi_Mem: 
            *p++ = 0xFF;
            p = doAMode_M(p, fake(6), i->Xin.Push.src->Xrmi.Mem.am);
            goto done;
         case Xrmi_Imm:
            *p++ = 0x68;
            p = emit32(p, i->Xin.Push.src->Xrmi.Imm.imm32);
            goto done;
         case Xrmi_Reg:
            *p++ = 0x50 + iregNo(i->Xin.Push.src->Xrmi.Reg.reg);
            goto done;
        default: 
            goto bad;
      }

   case Xin_Call:
      *p++ = 0xFF;
      p = doAMode_R(p, fake(2), i->Xin.Call.target);
      goto done;

   case Xin_Goto:
      /* If a non-boring unconditional jump, set %ebp appropriately.
         The magic numbers here have to match those defined in
         vg_constants.h. */
      if (i->Xin.Goto.cond == Xcc_ALWAYS
          && (i->Xin.Goto.jk == Ijk_ClientReq 
              || i->Xin.Goto.jk == Ijk_Syscall)) {
         /* movl $magic_number, %ebp */
         *p++ = 0xBD;
         switch (i->Xin.Goto.jk) {
            case Ijk_ClientReq: 
               /* 23 == VG_TRC_EBP_JMP_CLIENTREQ */
               p = emit32(p, 23); break;
            case Ijk_Syscall: 
               /* 19 == VG_TRC_EBP_JMP_SYSCALL */
               p = emit32(p, 19); break;
            default: 
               ppIRJumpKind(i->Xin.Goto.jk);
               vpanic("emit_X86Instr.Xin_Goto: unknown jump kind");
         }
      }
      /* unconditional jump to immediate */
      if (i->Xin.Goto.cond == Xcc_ALWAYS
          && i->Xin.Goto.dst->tag == Xri_Imm) {
         /* movl $immediate, %eax ; ret */
         *p++ = 0xB8;
         p = emit32(p, i->Xin.Goto.dst->Xri.Imm.imm32);
         *p++ = 0xC3;
         goto done;
      }
      /* unconditional jump to reg */
      if (i->Xin.Goto.cond == Xcc_ALWAYS
          && i->Xin.Goto.dst->tag == Xri_Reg) {
         /* movl %reg, %eax ; ret */
         if (i->Xin.Goto.dst->Xri.Reg.reg != hregX86_EAX()) {
            *p++ = 0x89;
            p = doAMode_R(p, i->Xin.Goto.dst->Xri.Reg.reg, hregX86_EAX());
         }
         *p++ = 0xC3;
         goto done;
      }
      /* conditional jump to immediate */
      if (i->Xin.Goto.cond != Xcc_ALWAYS
          && i->Xin.Goto.dst->tag == Xri_Imm) {
         vassert(i->Xin.Goto.jk == Ijk_Boring);
         /* jmp fwds if !condition */
         *p++ = 0x70 + (i->Xin.Goto.cond ^ 1);
         *p++ = 6; /* # of bytes in the next bit */
         /* movl $immediate, %eax ; ret */
         *p++ = 0xB8;
         p = emit32(p, i->Xin.Goto.dst->Xri.Imm.imm32);
         *p++ = 0xC3;
         goto done;
      }
      break;

   case Xin_CMov32:
      vassert(i->Xin.CMov32.cond != Xcc_ALWAYS);
#if 0
      /* This generates cmov, which is illegal on P5. */
      *p++ = 0x0F;
      *p++ = 0x40 + i->Xin.CMov32.cond;
      if (i->Xin.CMov32.src->tag == Xrm_Reg) {
         p = doAMode_R(p, i->Xin.CMov32.dst, i->Xin.CMov32.src->Xrm.Reg.reg);
         goto done;
      }
      if (i->Xin.CMov32.src->tag == Xrm_Mem) {
         p = doAMode_M(p, i->Xin.CMov32.dst, i->Xin.CMov32.src->Xrm.Mem.am);
         goto done;
      }
#else
      /* P5 friendly version: conditional jump over an unconditional
         move. */
      /* jmp fwds if !condition */
      *p++ = 0x70 + (i->Xin.CMov32.cond ^ 1);
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      switch (i->Xin.CMov32.src->tag) {
         case Xrm_Reg:
            /* Big sigh.  This is movl E -> G ... */
            *p++ = 0x89;
            p = doAMode_R(p, i->Xin.CMov32.src->Xrm.Reg.reg,
                             i->Xin.CMov32.dst);
                             
            break;
         case Xrm_Mem:
            /* ... whereas this is movl G -> E.  That's why the args
               to doAMode_R appear to be the wrong way round in the
               Xrm_Reg case. */
            *p++ = 0x8B;
            p = doAMode_M(p, i->Xin.CMov32.dst, 
                             i->Xin.CMov32.src->Xrm.Mem.am);
            break;
         default:
            goto bad;
      }
      /* Fill in the jump offset. */
      *(ptmp-1) = p - ptmp;
      goto done;
#endif
      break;

   case Xin_LoadEX:
      if (i->Xin.LoadEX.szSmall == 1 && !i->Xin.LoadEX.syned) {
         /* movzbl */
         *p++ = 0x0F;
         *p++ = 0xB6;
         p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
         goto done;
      }
      if (i->Xin.LoadEX.szSmall == 2 && !i->Xin.LoadEX.syned) {
         /* movzwl */
         *p++ = 0x0F;
         *p++ = 0xB7;
         p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
         goto done;
      }
      break;

   case Xin_Set32:
      /* Make the destination register be 1 or 0, depending on whether
         the relevant condition holds.  We have to dodge and weave
         when the destination is %esi or %edi as we cannot directly
         emit the native 'setb %reg' for those.  Further complication:
         the top 24 bits of the destination should be forced to zero,
         but doing 'xor %r,%r' kills the flag(s) we are about to read.
         Sigh.  So start off my moving $0 into the dest. */

      /* Do we need to swap in %eax? */
      if (iregNo(i->Xin.Set32.dst) >= 4) {
         /* xchg %eax, %dst */
         *p++ = 0x90 + iregNo(i->Xin.Set32.dst);
         /* movl $0, %eax */
         *p++ = 0xB8 + iregNo(hregX86_EAX());
         p = emit32(p, 0);
         /* setb lo8(%eax) */
         *p++ = 0x0F; 
         *p++ = 0x90 + (UChar)(i->Xin.Set32.cond);
         p = doAMode_R(p, fake(0), hregX86_EAX());
         /* xchg %eax, %dst */
         *p++ = 0x90 + iregNo(i->Xin.Set32.dst);
      } else {
         /* movl $0, %dst */
         *p++ = 0xB8 + iregNo(i->Xin.Set32.dst);
         p = emit32(p, 0);
         /* setb lo8(%dst) */
         *p++ = 0x0F; 
         *p++ = 0x90 + (UChar)(i->Xin.Set32.cond);
         p = doAMode_R(p, fake(0), i->Xin.Set32.dst);
      }
      goto done;

   case Xin_Bsfr32:
      *p++ = 0x0F;
      if (i->Xin.Bsfr32.isFwds) {
         *p++ = 0xBC;
      } else {
         *p++ = 0xBD;
      }
      p = doAMode_R(p, i->Xin.Bsfr32.dst, i->Xin.Bsfr32.src);
      goto done;

   case Xin_Store:
      if (i->Xin.Store.sz == 2) {
         /* This case, at least, is simple, given that we can
            reference the low 16 bits of any integer register. */
         *p++ = 0x66;
         *p++ = 0x89;
         p = doAMode_M(p, i->Xin.Store.src, i->Xin.Store.dst);
         goto done;
      }

      if (i->Xin.Store.sz == 1) {
         /* We have to do complex dodging and weaving if src is not
            the low 8 bits of %eax/%ebx/%ecx/%edx. */
         if (iregNo(i->Xin.Store.src) < 4) {
            /* we're OK, can do it directly */
            *p++ = 0x88;
            p = doAMode_M(p, i->Xin.Store.src, i->Xin.Store.dst);
           goto done;
         } else {
            /* Bleh.  This means the source is %edi or %esi.  Since
               the address mode can only mention three registers, at
               least one of %eax/%ebx/%ecx/%edx must be available to
               temporarily swap the source into, so the store can
               happen.  So we have to look at the regs mentioned
               in the amode. */
            HReg swap = INVALID_HREG;
            HReg  eax = hregX86_EAX(), ebx = hregX86_EBX(), 
                  ecx = hregX86_ECX(), edx = hregX86_EDX();
            Bool a_ok = True, b_ok = True, c_ok = True, d_ok = True;
            HRegUsage u;
            Int j;
            initHRegUsage(&u);
            addRegUsage_X86AMode(&u,  i->Xin.Store.dst);
            for (j = 0; j < u.n_used; j++) {
               HReg r = u.hreg[j];
               if (r == eax) a_ok = False;
               if (r == ebx) b_ok = False;
               if (r == ecx) c_ok = False;
               if (r == edx) d_ok = False;
            }
            if (a_ok) swap = eax;
            if (b_ok) swap = ebx;
            if (c_ok) swap = ecx;
            if (d_ok) swap = edx;
            vassert(swap != INVALID_HREG);
            /* xchgl %source, %swap. Could do better if swap is %eax. */
            *p++ = 0x87;
            p = doAMode_R(p, i->Xin.Store.src, swap);
            /* movb lo8{%swap}, (dst) */
            *p++ = 0x88;
            p = doAMode_M(p, swap, i->Xin.Store.dst);
            /* xchgl %source, %swap. Could do better if swap is %eax. */
            *p++ = 0x87;
            p = doAMode_R(p, i->Xin.Store.src, swap);
            goto done;
         }
      } /* if (i->Xin.Store.sz == 1) */
      break;

   case Xin_FpUnary:
      /* gop %src, %dst
         --> ffree %st7 ; fld %st(src) ; fop %st(0) ; fstp %st(1+dst)
      */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+hregNumber(i->Xin.FpUnary.src));
      p = do_fop1_st(p, i->Xin.FpUnary.op);
      p = do_fstp_st(p, 1+hregNumber(i->Xin.FpUnary.dst));
      goto done;

   case Xin_FpBinary:
      if (i->Xin.FpBinary.op == Xfp_ATAN) {
         /* Have to do this specially. */
         /* ffree %st7 ; fld %st(srcL) ; fld %st(srcR+1) ; fpatan ; fstp(1+dst) */
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+hregNumber(i->Xin.FpBinary.srcL));
         p = do_fld_st(p, 1+hregNumber(i->Xin.FpBinary.srcR));
         *p++ = 0xD9; *p++ = 0xF3;
         p = do_fstp_st(p, 1+hregNumber(i->Xin.FpBinary.dst));
         goto done;
      }
      /* General case */
      /* gop %srcL, %srcR, %dst
         --> ffree %st7 ; fld %st(srcL) ; fop %st(1+srcR) ; fstp %st(1+dst)
      */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+hregNumber(i->Xin.FpBinary.srcL));
      p = do_fop2_st(p, i->Xin.FpBinary.op, 1+hregNumber(i->Xin.FpBinary.srcR));
      p = do_fstp_st(p, 1+hregNumber(i->Xin.FpBinary.dst));
      goto done;

   case Xin_FpLdSt:
      if (i->Xin.FpLdSt.isLoad) {
         /* Load from memory into %fakeN.  
            --> ffree %st(7) ; fld{s/l} amode ; fstp st(N+1) 
         */
         p = do_ffree_st7(p);
         *p++ = i->Xin.FpLdSt.sz==4 ? 0xD9 : 0xDD;
	 p = doAMode_M(p, fake(0)/*subopcode*/, i->Xin.FpLdSt.addr);
         p = do_fstp_st(p, 1+hregNumber(i->Xin.FpLdSt.reg));
         goto done;
      } else {
         /* Store from %fakeN into memory.
            --> ffree %st(7) ; fld st(N) ; fstp{l|s} amode
	 */
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+hregNumber(i->Xin.FpLdSt.reg));
         *p++ = i->Xin.FpLdSt.sz==4 ? 0xD9 : 0xDD;
         p = doAMode_M(p, fake(3)/*subopcode*/, i->Xin.FpLdSt.addr);
         goto done;
      }
      break;

   case Xin_FpLdStI:
      if (i->Xin.FpLdStI.isLoad) {
         /* Load from memory into %fakeN, converting from an int.  
            --> ffree %st(7) ; fild{w/l/ll} amode ; fstp st(N+1) 
         */
         switch (i->Xin.FpLdStI.sz) {
            case 8:  opc = 0xDF; subopc_imm = 5; break;
            case 4:  opc = 0xDB; subopc_imm = 0; break;
            case 2:  vassert(0); opc = 0xDF; subopc_imm = 0; break;
            default: vpanic("emitX86Instr(Xin_FpLdStI-load)");
         }
         p = do_ffree_st7(p);
         *p++ = opc;
         p = doAMode_M(p, fake(subopc_imm)/*subopcode*/, i->Xin.FpLdStI.addr);
         p = do_fstp_st(p, 1+hregNumber(i->Xin.FpLdStI.reg));
         goto done;
      } else {
         /* Store from %fakeN into memory, converting to an int.
            --> ffree %st(7) ; fld st(N) ; fistp{w/l/ll} amode
	 */
         switch (i->Xin.FpLdStI.sz) {
            case 8:  opc = 0xDF; subopc_imm = 7; break;
            case 4:  opc = 0xDB; subopc_imm = 3; break;
            case 2:  opc = 0xDF; subopc_imm = 3; break;
            default: vpanic("emitX86Instr(Xin_FpLdStI-store)");
         }
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+hregNumber(i->Xin.FpLdStI.reg));
         *p++ = opc;
         p = doAMode_M(p, fake(subopc_imm)/*subopcode*/, i->Xin.FpLdStI.addr);
         goto done;
      }
      break;

   case Xin_FpCMov:
      /* jmp fwds if !condition */
      *p++ = 0x70 + (i->Xin.FpCMov.cond ^ 1);
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      /* ffree %st7 ; fld %st(src) ; fstp %st(1+dst) */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+fregNo(i->Xin.FpCMov.src));
      p = do_fstp_st(p, 1+fregNo(i->Xin.FpCMov.dst));

      /* Fill in the jump offset. */
      *(ptmp-1) = p - ptmp;
      goto done;

   case Xin_FpLdStCW:
      if (i->Xin.FpLdStCW.isLoad) {
         *p++ = 0xD9;
         p = doAMode_M(p, fake(5)/*subopcode*/, i->Xin.FpLdStCW.addr);
      } else {
         vassert(0);
      }
      goto done;


   case Xin_FpCmp:
      /* gcmp %fL, %fR, %dst
         -> ffree %st7; fpush %fL ; fucomp %(fR+1) ; 
            fnstsw %ax ; movl %eax, %dst 
      */
      /* ffree %st7 */
      p = do_ffree_st7(p);
      /* fpush %fL */
      p = do_fld_st(p, 0+fregNo(i->Xin.FpCmp.srcL));
      /* fucomp %(fR+1) */
      *p++ = 0xDD;
      *p++ = 0xE8 + (7 & (1+fregNo(i->Xin.FpCmp.srcR)));
      /* fnstsw %ax */
      *p++ = 0xDF;
      *p++ = 0xE0;
      /*  movl %eax, %dst */
      *p++ = 0x89;
      p = doAMode_R(p, hregX86_EAX(), i->Xin.FpCmp.dst);
      goto done;

   default: 
      goto bad;
   }

  bad:
   ppX86Instr(i);
   vpanic("emit_X86Instr");
   /*NOTREACHED*/
   
  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];

#  undef fake
}


#if 0
/* Self-contained test; can be called directly from
   main. */
void test_asm86 ( void )
{
  UChar buf[32];
  Int i, n;
  HReg edi = hregX86_EDI();
  HReg esi = hregX86_ESI();
  HReg ecx = hregX86_ECX();
  HReg ebp = hregX86_EBP();
  HReg eax = hregX86_EAX();
  HReg esp = hregX86_ESP();

#define T(_iii)                                  \
  do { X86Instr* iii = _iii;                     \
       vex_printf("\n   ");                      \
       ppX86Instr(iii);                          \
       vex_printf("\n   ");                      \
       n = emit_X86Instr( buf, 32, iii );        \
       for (i = 0; i < n; i++) {                 \
           if (buf[i] < 0x10)                    \
              vex_printf("0%x ", (Int)buf[i]);   \
           else                                  \
              vex_printf("%x ", (Int)buf[i]);    \
       }                                         \
       vex_printf("\n");                         \
  } while (0)

#if 0
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Reg(esi), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0x12345678), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IR(0,esi)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IR(0,ebp)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IR(1,esi)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IR(1,ebp)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IR(127,esi)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IR(256,esi)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IRRS(1,esi,ecx,0)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IRRS(1,esi,ecx,3)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IRRS(127,esi,ecx,3)), edi) );
T( X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(X86AMode_IRRS(256,esi,ecx,3)), edi) );
#endif

#if 0
T( X86Instr_Alu32M(Xalu_MOV, X86RI_Imm(9), X86AMode_IR(0,esi)) );
T( X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(edi), X86AMode_IR(0,esi)) );
T( X86Instr_Alu32M(Xalu_MOV, X86RI_Imm(999), X86AMode_IRRS(256,esi,ecx,3)) );
T( X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(ebp), X86AMode_IRRS(256,esi,ecx,3)) );
#endif

#if 0
T( X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(0x42), eax) );
T( X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(0x41424344), eax) );
T( X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(0x42), esp) );
T( X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(0x41424344), esp) );
T( X86Instr_Alu32R(Xalu_ADD, X86RMI_Reg(esi), edi) );
T( X86Instr_Alu32R(Xalu_ADD, X86RMI_Mem(X86AMode_IR(1,esi)), edi) );
#endif

#if 0
T( X86Instr_Alu32R(Xalu_SUB, X86RMI_Imm(0x42), eax) );
T( X86Instr_Alu32R(Xalu_SUB, X86RMI_Imm(0x41424344), eax) );
T( X86Instr_Alu32R(Xalu_SUB, X86RMI_Imm(0x42), esp) );
T( X86Instr_Alu32R(Xalu_SUB, X86RMI_Imm(0x41424344), esp) );
T( X86Instr_Alu32R(Xalu_SUB, X86RMI_Reg(esi), edi) );
T( X86Instr_Alu32R(Xalu_SUB, X86RMI_Mem(X86AMode_IR(1,esi)), edi) );
#endif

#if 0
T( X86Instr_Alu32M(Xalu_ADD, X86RI_Imm(0x42), X86AMode_IR(0x99,esi)) );
T( X86Instr_Alu32M(Xalu_ADD, X86RI_Imm(0x4243), X86AMode_IR(0x99,esi)) );
T( X86Instr_Alu32M(Xalu_ADD, X86RI_Reg(ecx), X86AMode_IR(0x99,ebp)) );
T( X86Instr_Alu32M(Xalu_ADD, X86RI_Reg(ecx), X86AMode_IR(0x80,ebp)) );
T( X86Instr_Alu32M(Xalu_ADD, X86RI_Reg(ecx), X86AMode_IR(0x7F,ebp)) );
#endif

#if 1
T( X86Instr_Alu32M(Xalu_SUB, X86RI_Imm(0x42), X86AMode_IR(0x99,esi)) );
T( X86Instr_Alu32M(Xalu_SUB, X86RI_Imm(0x4243), X86AMode_IR(0x99,esi)) );
T( X86Instr_Alu32M(Xalu_SUB, X86RI_Reg(ecx), X86AMode_IR(0x99,ebp)) );
T( X86Instr_Alu32M(Xalu_SUB, X86RI_Reg(ecx), X86AMode_IR(0x80,ebp)) );
T( X86Instr_Alu32M(Xalu_SUB, X86RI_Reg(ecx), X86AMode_IR(0x7F,ebp)) );
#endif

#undef T
}
#endif


/*---------------------------------------------------------------*/
/*--- end                                    host-x86/hdefs.c ---*/
/*---------------------------------------------------------------*/
