
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (x86h_defs.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libjit_basictypes.h"
#include "libjit.h"

#include "vex_util.h"
#include "host_regs.h"
#include "x86h_defs.h"


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
         vassert(r >= 0 && r < 6);
         vex_printf("%%fake%d", r);
         return;
      case HRcVector:
         vpanic("ppHRegX86: real vector reg");
     default:
         vpanic("ppHRegX86");
   }
}

HReg hregX86_EAX ( void ) { return mkHReg(0, HRcInt, False); }
HReg hregX86_EBX ( void ) { return mkHReg(3, HRcInt, False); }
HReg hregX86_ECX ( void ) { return mkHReg(1, HRcInt, False); }
HReg hregX86_EDX ( void ) { return mkHReg(2, HRcInt, False); }
HReg hregX86_ESP ( void ) { return mkHReg(4, HRcInt, False); }
HReg hregX86_EBP ( void ) { return mkHReg(5, HRcInt, False); }
HReg hregX86_ESI ( void ) { return mkHReg(6, HRcInt, False); }
HReg hregX86_EDI ( void ) { return mkHReg(7, HRcInt, False); }

void getAllocableRegs_X86 ( Int* nregs, HReg** arr )
{
   *nregs = 6;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   *arr[0] = hregX86_EAX();
   *arr[1] = hregX86_EBX();
   *arr[2] = hregX86_ECX();
   *arr[3] = hregX86_EDX();
   *arr[4] = hregX86_ESI();
   *arr[5] = hregX86_EDI();
}


/* --------- X86AMode: memory address expressions. --------- */

X86AMode* X86AMode_IR ( UInt imm32, HReg reg ) {
   X86AMode* am = LibVEX_Alloc(sizeof(X86AMode));
   am->tag = Xam_IR;
   am->Xam.IR.imm = imm32;
   am->Xam.IR.reg = reg;
   return am;
}

X86AMode* X86AMode_IRRS ( UInt imm32, HReg base, HReg index, Int shift ) {
   X86AMode* am = LibVEX_Alloc(sizeof(X86AMode));
   am->tag = Xam_IRRS;
   am->Xam.IRRS.imm = imm32;
   am->Xam.IRRS.base = base;
   am->Xam.IRRS.index = index;
   am->Xam.IRRS.shift = shift;
   vassert(shift >= 0 && shift <= 3);
   return am;
}

void ppX86AMode ( X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         vex_printf("0x%x(", am->Xam.IR.imm);
         ppHRegX86(am->Xam.IR.reg);
         vex_printf(")");
         return;
      case Xam_IRRS:
         vex_printf("0x%x(", am->Xam.IRRS.imm);
         ppHRegX86(am->Xam.IRRS.base);
         vex_printf(",");
         ppHRegX86(am->Xam.IRRS.index);
         vex_printf(",%d)", am->Xam.IRRS.shift);
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

void ppX86AluOp ( X86AluOp op ) {
   Char* name;
   switch (op) {
      case Xalu_MOV: name = "mov"; break;
      case Xalu_ADD: name = "add"; break;
      case Xalu_SUB: name = "sub"; break;
      case Xalu_ADC: name = "adc"; break;
      case Xalu_SBB: name = "sbb"; break;
      case Xalu_AND: name = "and"; break;
      case Xalu_OR:  name = "or";  break;
      case Xalu_XOR: name = "xor"; break;
      default: vpanic("ppX86AluOp");
   }
   vex_printf("%s", name);
}

void ppX86ShiftOp ( X86ShiftOp op ) {
   Char* name;
   switch (op) {
      case Xsh_SHL: name = "shl"; break;
      case Xsh_SHR: name = "shr"; break;
      case Xsh_SAR: name = "sar"; break;
      case Xsh_ROL: name = "rol"; break;
      case Xsh_ROR: name = "ror"; break;
      default: vpanic("ppX86ShiftOp");
   }
   vex_printf("%s", name);
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

X86Instr* X86Instr_RET ( void ) {
   X86Instr* i     = LibVEX_Alloc(sizeof(X86Instr));
   i->tag          = Xin_RET;
   return i;
}

void ppX86Instr ( X86Instr* i ) {
   switch (i->tag) {
      case Xin_Alu32R:
         ppX86AluOp(i->Xin.Alu32R.op);
         vex_printf("l ");
         ppX86RMI(i->Xin.Alu32R.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         ppX86AluOp(i->Xin.Alu32M.op);
         vex_printf("l ");
         ppX86RI(i->Xin.Alu32M.src);
         vex_printf(",");
         ppX86AMode(i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         ppX86ShiftOp(i->Xin.Sh32.op);
         vex_printf("l ");
         if (i->Xin.Sh32.src == 0)
	   vex_printf(" %%cl,"); 
         else 
            vex_printf(" $%d,", i->Xin.Sh32.src);
         ppX86RM(i->Xin.Sh32.dst);
         return;
      case Xin_RET:
         vex_printf("ret");
         return;
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
         if (i->Xin.Alu32R.op == Xalu_MOV)
            addHRegUse(u, HRmWrite,  i->Xin.Alu32R.dst);
         else
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
      case Xin_RET:
         /* Using our calling conventions, %eax is live into a ret,
            because we know the dispatcher -- to which we're returning
            -- uses that value as the next guest address.  Hmm.  What
            if we're simulating a 64-bit guest on an x86 host? */
         addHRegUse(u, HRmRead, hregX86_EAX());
         return;
      default:
         ppX86Instr(i);
         vpanic("getRegUsage_X86Instr");
   }
}

void mapRegs_X86Instr (HRegRemap* m, X86Instr* i)
{
   switch (i->tag) {
      case Xin_Alu32R:
         mapRegs_X86RMI(m, i->Xin.Alu32R.src);
         i->Xin.Alu32R.dst = lookupHRegRemap(m, i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         mapRegs_X86RI(m, i->Xin.Alu32M.src);
         mapRegs_X86AMode(m, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         mapRegs_X86RM(m, i->Xin.Sh32.dst);
         return;
      case Xin_RET:
         return;
      default:
         ppX86Instr(i);
         vpanic("mapRegs_X86Instr");
   }
}

Bool isMove_X86Instr ( X86Instr* i, HReg* src, HReg* dst )
{
   if (i->tag != Xin_Alu32R)
      return False;
   if (i->Xin.Alu32R.op != Xalu_MOV)
      return False;
   if (i->Xin.Alu32R.src->tag != Xrmi_Reg)
      return False;
   *src = i->Xin.Alu32R.src->Xrmi.Reg.reg;
   *dst = i->Xin.Alu32R.dst;
   return True;
}

X86Instr* genSpill_X86 ( HReg rreg, Int offset )
{
   vassert(!hregIsVirtual(rreg));
   switch (hregClass(rreg)) {
      case HRcInt:
        return
	X86Instr_Alu32M ( Xalu_MOV, X86RI_Reg(rreg), 
		          X86AMode_IR(offset + 0x1000, 
                                      hregX86_EBP()));
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_X86: unimplemented regclass");
   }
}

X86Instr* genReload_X86 ( HReg rreg, Int offset )
{
   vassert(!hregIsVirtual(rreg));
   switch (hregClass(rreg)) {
      case HRcInt:
        return
	X86Instr_Alu32R ( Xalu_MOV, 
		          X86RMI_Mem(X86AMode_IR(offset + 0x1000, 
                                                 hregX86_EBP())),
                          rreg );
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_X86: unimplemented regclass");
   }
}

/*---------------------------------------------------------------*/
/*--- end                                         x86h_defs.c ---*/
/*---------------------------------------------------------------*/
