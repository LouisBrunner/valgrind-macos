/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                  host_s390_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2011

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
*/

/* Contributed by Florian Krohm */

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"
#include "libvex_guest_offsets.h"
#include "libvex_s390x_common.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_s390_defs.h"
#include "host_s390_disasm.h"
#include <stdarg.h>

/* KLUDGE: We need to know the hwcaps of the host when generating
   code. But that info is not passed to emit_S390Instr. Only mode64 is
   being passed. So, ideally, we want this passed as an argument, too.
   Until then, we use a global variable. This variable is set as a side
   effect of iselSB_S390. This is safe because instructions are selected
   before they are emitted. */
const VexArchInfo *s390_archinfo_host;

/*------------------------------------------------------------*/
/*--- Registers                                            ---*/
/*------------------------------------------------------------*/

/* Decompile the given register into a static buffer and return it */
const HChar *
s390_hreg_as_string(HReg reg)
{
   static HChar buf[10];

   static const HChar ireg_names[16][5] = {
      "%r0",  "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
      "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"
   };

   static const HChar freg_names[16][5] = {
      "%f0",  "%f1",  "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7",
      "%f8",  "%f9",  "%f10", "%f11", "%f12", "%f13", "%f14", "%f15"
   };

   UInt r;  /* hregNumber() returns an UInt */

   r = hregNumber(reg);

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      buf[0] = '\0';
      switch (hregClass(reg)) {
      case HRcInt64: vex_sprintf(buf, "%%vR%d", r); break;
      case HRcFlt64: vex_sprintf(buf, "%%vF%d", r); break;
      default:       goto fail;
      }
      return buf;
   }

   /* But specific for real regs. */
   vassert(r < 16);

   switch (hregClass(reg)) {
   case HRcInt64: return ireg_names[r];
   case HRcFlt64: return freg_names[r];
   default:       goto fail;
   }

 fail: vpanic("s390_hreg_as_string");
}


/* Tell the register allocator which registers can be allocated. */
void
s390_hreg_get_allocable(Int *nregs, HReg **arr)
{
   UInt i;

   /* Total number of allocable registers (all classes) */
   *nregs =  16 /* GPRs */
      -  1 /* r0 */
      -  1 /* r12 register holding VG_(dispatch_ctr) */
      -  1 /* r13 guest state pointer */
      -  1 /* r14 link register */
      -  1 /* r15 stack pointer */
      + 16 /* FPRs */
      ;

   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));

   i = 0;

   /* GPR0 is not available because it is interpreted as 0, when used
      as a base or index register. */
   (*arr)[i++] = mkHReg(1,  HRcInt64, False);
   (*arr)[i++] = mkHReg(2,  HRcInt64, False);
   (*arr)[i++] = mkHReg(3,  HRcInt64, False);
   (*arr)[i++] = mkHReg(4,  HRcInt64, False);
   (*arr)[i++] = mkHReg(5,  HRcInt64, False);
   (*arr)[i++] = mkHReg(6,  HRcInt64, False);
   (*arr)[i++] = mkHReg(7,  HRcInt64, False);
   (*arr)[i++] = mkHReg(8,  HRcInt64, False);
   (*arr)[i++] = mkHReg(9,  HRcInt64, False);
   /* GPR10 and GPR11 are used for instructions that use register pairs.
      Otherwise, they are available to the allocator */
   (*arr)[i++] = mkHReg(10, HRcInt64, False);
   (*arr)[i++] = mkHReg(11, HRcInt64, False);
   /* GPR12 is not available because it caches VG_(dispatch_ctr) */
   /* GPR13 is not available because it is used as guest state pointer */
   /* GPR14 is not available because it is used as link register */
   /* GPR15 is not available because it is used as stack pointer */

   /* Add the available real (non-virtual) FPRs */
   (*arr)[i++] = mkHReg(0,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(1,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(2,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(3,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(4,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(5,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(6,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(7,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(8,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(9,  HRcFlt64, False);
   (*arr)[i++] = mkHReg(10, HRcFlt64, False);
   (*arr)[i++] = mkHReg(11, HRcFlt64, False);
   (*arr)[i++] = mkHReg(12, HRcFlt64, False);
   (*arr)[i++] = mkHReg(13, HRcFlt64, False);
   (*arr)[i++] = mkHReg(14, HRcFlt64, False);
   (*arr)[i++] = mkHReg(15, HRcFlt64, False);
   /* FPR12 - FPR15 are also used as register pairs for 128-bit
      floating point operations */
}


/* Return the real register that holds the guest state pointer */
HReg
s390_hreg_guest_state_pointer(void)
{
   return mkHReg(S390_REGNO_GUEST_STATE_POINTER, HRcInt64, False);
}

/* Is VALUE within the domain of a 20-bit signed integer. */
static __inline__ Bool
fits_signed_20bit(Int value)
{
   return ((value << 12) >> 12) == value;
}


/* Is VALUE within the domain of a 12-bit unsigned integer. */
static __inline__ Bool
fits_unsigned_12bit(Int value)
{
   return (value & 0xFFF) == value;
}

/*------------------------------------------------------------*/
/*--- Addressing modes (amodes)                            ---*/
/*------------------------------------------------------------*/

/* Construct a b12 amode. */
s390_amode *
s390_amode_b12(Int d, HReg b)
{
   s390_amode *am = LibVEX_Alloc(sizeof(s390_amode));

   vassert(fits_unsigned_12bit(d));

   am->tag = S390_AMODE_B12;
   am->d = d;
   am->b = b;
   am->x = 0;  /* hregNumber(0) == 0 */

   return am;
}


/* Construct a b20 amode. */
s390_amode *
s390_amode_b20(Int d, HReg b)
{
   s390_amode *am = LibVEX_Alloc(sizeof(s390_amode));

   vassert(fits_signed_20bit(d));

   am->tag = S390_AMODE_B20;
   am->d = d;
   am->b = b;
   am->x = 0;  /* hregNumber(0) == 0 */

   return am;
}


/* Construct a bx12 amode. */
s390_amode *
s390_amode_bx12(Int d, HReg b, HReg x)
{
   s390_amode *am = LibVEX_Alloc(sizeof(s390_amode));

   vassert(fits_unsigned_12bit(d));
   vassert(b != 0);
   vassert(x != 0);

   am->tag = S390_AMODE_BX12;
   am->d = d;
   am->b = b;
   am->x = x;

   return am;
}


/* Construct a bx20 amode. */
s390_amode *
s390_amode_bx20(Int d, HReg b, HReg x)
{
   s390_amode *am = LibVEX_Alloc(sizeof(s390_amode));

   vassert(fits_signed_20bit(d));
   vassert(b != 0);
   vassert(x != 0);

   am->tag = S390_AMODE_BX20;
   am->d = d;
   am->b = b;
   am->x = x;

   return am;
}


/* Construct an AMODE for accessing the guest state at OFFSET */
s390_amode *
s390_amode_for_guest_state(Int offset)
{
   if (fits_unsigned_12bit(offset))
      return s390_amode_b12(offset, s390_hreg_guest_state_pointer());

   vpanic("invalid guest state offset");
}


/* Decompile the given amode into a static buffer and return it. */
const HChar *
s390_amode_as_string(const s390_amode *am)
{
   static HChar buf[30];
   HChar *p;

   buf[0] = '\0';
   p = buf;

   switch (am->tag) {
   case S390_AMODE_B12:
   case S390_AMODE_B20:
      vex_sprintf(p, "%d(%s)", am->d, s390_hreg_as_string(am->b));
      break;

   case S390_AMODE_BX12:
   case S390_AMODE_BX20:
      /* s390_hreg_as_string returns pointer to local buffer. Need to
         split this into two printfs */
      p += vex_sprintf(p, "%d(%s,", am->d, s390_hreg_as_string(am->x));
      vex_sprintf(p, "%s)", s390_hreg_as_string(am->b));
      break;

   default:
      vpanic("s390_amode_as_string");
   }

   return buf;
}


/* Helper function for s390_amode_is_sane */
static __inline__ Bool
is_virtual_gpr(HReg reg)
{
   return hregIsVirtual(reg) && hregClass(reg) == HRcInt64;
}


/* Sanity check for an amode */
Bool
s390_amode_is_sane(const s390_amode *am)
{
   switch (am->tag) {
   case S390_AMODE_B12:
      return is_virtual_gpr(am->b) && fits_unsigned_12bit(am->d);

   case S390_AMODE_B20:
      return is_virtual_gpr(am->b) && fits_signed_20bit(am->d);

   case S390_AMODE_BX12:
      return is_virtual_gpr(am->b) && is_virtual_gpr(am->x) &&
             fits_unsigned_12bit(am->d);

   case S390_AMODE_BX20:
      return is_virtual_gpr(am->b) && is_virtual_gpr(am->x) &&
             fits_signed_20bit(am->d);

   default:
      vpanic("s390_amode_is_sane");
   }
}


/* Record the register use of an amode */
void
s390_amode_get_reg_usage(HRegUsage *u, const s390_amode *am)
{
   switch (am->tag) {
   case S390_AMODE_B12:
   case S390_AMODE_B20:
      addHRegUse(u, HRmRead, am->b);
      return;

   case S390_AMODE_BX12:
   case S390_AMODE_BX20:
      addHRegUse(u, HRmRead, am->b);
      addHRegUse(u, HRmRead, am->x);
      return;

   default:
      vpanic("s390_amode_get_reg_usage");
   }
}


void
s390_amode_map_regs(HRegRemap *m, s390_amode *am)
{
   switch (am->tag) {
   case S390_AMODE_B12:
   case S390_AMODE_B20:
      am->b = lookupHRegRemap(m, am->b);
      return;

   case S390_AMODE_BX12:
   case S390_AMODE_BX20:
      am->b = lookupHRegRemap(m, am->b);
      am->x = lookupHRegRemap(m, am->x);
      return;

   default:
      vpanic("s390_amode_map_regs");
   }
}



void
ppS390AMode(struct s390_amode *am)
{
   vex_printf("%s", s390_amode_as_string(am));
}

void
ppS390Instr(struct s390_insn *insn, Bool mode64)
{
   vex_printf("%s", s390_insn_as_string(insn));
}

void
ppHRegS390(HReg reg)
{
   vex_printf("%s", s390_hreg_as_string(reg));
}

/*------------------------------------------------------------*/
/*--- Helpers for register allocation                      ---*/
/*------------------------------------------------------------*/

/* Called once per translation. */
void
getAllocableRegs_S390(Int *nregs, HReg **arr, Bool mode64)
{
   s390_hreg_get_allocable(nregs, arr);
}


/* Tell the register allocator how the given instruction uses the registers
   it refers to. */
void
getRegUsage_S390Instr(HRegUsage *u, struct s390_insn *insn, Bool mode64)
{
   s390_insn_get_reg_usage(u, insn);
}


/* Map the registers of the given instruction */
void
mapRegs_S390Instr(HRegRemap *m, struct s390_insn *insn, Bool mode64)
{
   s390_insn_map_regs(m, insn);
}


/* Figure out if the given insn represents a reg-reg move, and if so
   assign the source and destination to *src and *dst.  If in doubt say No.
   Used by the register allocator to do move coalescing. */
Bool
isMove_S390Instr(struct s390_insn *insn, HReg *src, HReg *dst)
{
   return s390_insn_is_reg_reg_move(insn, src, dst);
}


/* Generate s390 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. This is like an Ist_Put */
void
genSpill_S390(HInstr **i1, HInstr **i2, HReg rreg, Int offsetB, Bool mode64)
{
   s390_amode *am;

   vassert(offsetB >= 0);
   vassert(offsetB <= (1 << 12));  /* because we use b12 amode */
   vassert(!hregIsVirtual(rreg));

   *i1 = *i2 = NULL;

   am = s390_amode_for_guest_state(offsetB);

   switch (hregClass(rreg)) {
   case HRcInt64:
   case HRcFlt64:
      *i1 = s390_insn_store(8, am, rreg);
      return;

   default:
      ppHRegClass(hregClass(rreg));
      vpanic("genSpill_S390: unimplemented regclass");
   }
}


/* This is like an Iex_Get */
void
genReload_S390(HInstr **i1, HInstr **i2, HReg rreg, Int offsetB, Bool mode64)
{
   s390_amode *am;

   vassert(offsetB >= 0);
   vassert(offsetB <= (1 << 12));  /* because we use b12 amode */
   vassert(!hregIsVirtual(rreg));

   *i1 = *i2 = NULL;

   am = s390_amode_for_guest_state(offsetB);

   switch (hregClass(rreg)) {
   case HRcInt64:
   case HRcFlt64:
      *i1 = s390_insn_load(8, rreg, am);
      return;

   default:
      ppHRegClass(hregClass(rreg));
      vpanic("genReload_S390: unimplemented regclass");
   }
}

/* Helper function for s390_insn_get_reg_usage */
static void
s390_opnd_RMI_get_reg_usage(HRegUsage *u, s390_opnd_RMI op)
{
   switch (op.tag) {
   case S390_OPND_REG:
      addHRegUse(u, HRmRead, op.variant.reg);
      break;

   case S390_OPND_AMODE:
      s390_amode_get_reg_usage(u, op.variant.am);
      break;

   case S390_OPND_IMMEDIATE:
      break;

   default:
      vpanic("s390_opnd_RMI_get_reg_usage");
   }
}


/* Tell the register allocator how the given insn uses the registers */
void
s390_insn_get_reg_usage(HRegUsage *u, const s390_insn *insn)
{
   initHRegUsage(u);

   switch (insn->tag) {
   case S390_INSN_LOAD:
      addHRegUse(u, HRmWrite, insn->variant.load.dst);
      s390_amode_get_reg_usage(u, insn->variant.load.src);
      break;

   case S390_INSN_LOAD_IMMEDIATE:
      addHRegUse(u, HRmWrite, insn->variant.load_immediate.dst);
      break;

   case S390_INSN_STORE:
      addHRegUse(u, HRmRead, insn->variant.store.src);
      s390_amode_get_reg_usage(u, insn->variant.store.dst);
      break;

   case S390_INSN_MOVE:
      addHRegUse(u, HRmRead,  insn->variant.move.src);
      addHRegUse(u, HRmWrite, insn->variant.move.dst);
      break;

   case S390_INSN_COND_MOVE:
      s390_opnd_RMI_get_reg_usage(u, insn->variant.cond_move.src);
      addHRegUse(u, HRmWrite, insn->variant.cond_move.dst);
      break;

   case S390_INSN_ALU:
      addHRegUse(u, HRmWrite, insn->variant.alu.dst);
      addHRegUse(u, HRmRead,  insn->variant.alu.dst);  /* op1 */
      s390_opnd_RMI_get_reg_usage(u, insn->variant.alu.op2);
      break;

   case S390_INSN_MUL:
      addHRegUse(u, HRmRead,  insn->variant.mul.dst_lo);  /* op1 */
      addHRegUse(u, HRmWrite, insn->variant.mul.dst_lo);
      addHRegUse(u, HRmWrite, insn->variant.mul.dst_hi);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.mul.op2);
      break;

   case S390_INSN_DIV:
      addHRegUse(u, HRmRead,  insn->variant.div.op1_lo);
      addHRegUse(u, HRmRead,  insn->variant.div.op1_hi);
      addHRegUse(u, HRmWrite, insn->variant.div.op1_lo);
      addHRegUse(u, HRmWrite, insn->variant.div.op1_hi);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.div.op2);
      break;

   case S390_INSN_DIVS:
      addHRegUse(u, HRmRead,  insn->variant.divs.op1);
      addHRegUse(u, HRmWrite, insn->variant.divs.op1); /* quotient */
      addHRegUse(u, HRmWrite, insn->variant.divs.rem); /* remainder */
      s390_opnd_RMI_get_reg_usage(u, insn->variant.divs.op2);
      break;

   case S390_INSN_CLZ:
      addHRegUse(u, HRmWrite, insn->variant.clz.num_bits);
      addHRegUse(u, HRmWrite, insn->variant.clz.clobber);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.clz.src);
      break;

   case S390_INSN_UNOP:
      addHRegUse(u, HRmWrite, insn->variant.unop.dst);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.unop.src);
      break;

   case S390_INSN_TEST:
      s390_opnd_RMI_get_reg_usage(u, insn->variant.test.src);
      break;

   case S390_INSN_CC2BOOL:
      addHRegUse(u, HRmWrite, insn->variant.cc2bool.dst);
      break;

   case S390_INSN_CAS:
      addHRegUse(u, HRmRead,  insn->variant.cas.op1);
      s390_amode_get_reg_usage(u, insn->variant.cas.op2);
      addHRegUse(u, HRmRead,  insn->variant.cas.op3);
      addHRegUse(u, HRmWrite,  insn->variant.cas.old_mem);
      break;

   case S390_INSN_COMPARE:
      addHRegUse(u, HRmRead, insn->variant.compare.src1);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.compare.src2);
      break;

   case S390_INSN_BRANCH:
      s390_opnd_RMI_get_reg_usage(u, insn->variant.branch.dst);
      /* The destination address is loaded into S390_REGNO_RETURN_VALUE.
         See s390_insn_branch_emit. */
      addHRegUse(u, HRmWrite,
                 mkHReg(S390_REGNO_RETURN_VALUE, HRcInt64, False));
      break;

   case S390_INSN_HELPER_CALL: {
      UInt i;

      /* Assume that all volatile registers are clobbered. ABI says,
         volatile registers are: r0 - r5. Valgrind's register allocator
         does not know about r0, so we can leave that out */
      for (i = 1; i <= 5; ++i) {
         addHRegUse(u, HRmWrite, mkHReg(i, HRcInt64, False));
      }

      /* Ditto for floating point registers. f0 - f7 are volatile */
      for (i = 0; i <= 7; ++i) {
         addHRegUse(u, HRmWrite, mkHReg(i, HRcFlt64, False));
      }

      /* The registers that are used for passing arguments will be read.
         Not all of them may, but in general we need to assume that. */
      for (i = 0; i < insn->variant.helper_call.num_args; ++i) {
         addHRegUse(u, HRmRead, mkHReg(s390_gprno_from_arg_index(i),
                                       HRcInt64, False));
      }

      /* s390_insn_helper_call_emit also reads / writes the link register
         and stack pointer. But those registers are not visible to the
         register allocator. So we don't need to do anything for them. */
      break;
   }

   case S390_INSN_BFP_TRIOP:
      addHRegUse(u, HRmWrite, insn->variant.bfp_triop.dst);
      addHRegUse(u, HRmRead,  insn->variant.bfp_triop.dst);  /* first */
      addHRegUse(u, HRmRead,  insn->variant.bfp_triop.op2);  /* second */
      addHRegUse(u, HRmRead,  insn->variant.bfp_triop.op3);  /* third */
      break;

   case S390_INSN_BFP_BINOP:
      addHRegUse(u, HRmWrite, insn->variant.bfp_binop.dst);
      addHRegUse(u, HRmRead,  insn->variant.bfp_binop.dst);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp_binop.op2);  /* right */
      break;

   case S390_INSN_BFP_UNOP:
      addHRegUse(u, HRmWrite, insn->variant.bfp_unop.dst);
      addHRegUse(u, HRmRead,  insn->variant.bfp_unop.op);  /* operand */
      break;

   case S390_INSN_BFP_COMPARE:
      addHRegUse(u, HRmWrite, insn->variant.bfp_compare.dst);
      addHRegUse(u, HRmRead,  insn->variant.bfp_compare.op1);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp_compare.op2);  /* right */
      break;

   case S390_INSN_BFP128_BINOP:
      addHRegUse(u, HRmWrite, insn->variant.bfp128_binop.dst_hi);
      addHRegUse(u, HRmWrite, insn->variant.bfp128_binop.dst_lo);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_binop.dst_hi);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp128_binop.dst_lo);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp128_binop.op2_hi);  /* right */
      addHRegUse(u, HRmRead,  insn->variant.bfp128_binop.op2_lo);  /* right */
      break;

   case S390_INSN_BFP128_COMPARE:
      addHRegUse(u, HRmWrite, insn->variant.bfp128_compare.dst);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_compare.op1_hi);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp128_compare.op1_lo);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp128_compare.op2_hi);  /* right */
      addHRegUse(u, HRmRead,  insn->variant.bfp128_compare.op2_lo);  /* right */
      break;

   case S390_INSN_BFP128_UNOP:
      addHRegUse(u, HRmWrite, insn->variant.bfp128_unop.dst_hi);
      addHRegUse(u, HRmWrite, insn->variant.bfp128_unop.dst_lo);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_unop.op_hi);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_unop.op_lo);
      break;

   case S390_INSN_BFP128_CONVERT_TO:
      addHRegUse(u, HRmWrite, insn->variant.bfp128_unop.dst_hi);
      addHRegUse(u, HRmWrite, insn->variant.bfp128_unop.dst_lo);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_unop.op_hi);
      break;

   case S390_INSN_BFP128_CONVERT_FROM:
      addHRegUse(u, HRmWrite, insn->variant.bfp128_unop.dst_hi);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_unop.op_hi);
      addHRegUse(u, HRmRead,  insn->variant.bfp128_unop.op_lo);
      break;

   case S390_INSN_MFENCE:
      break;

   default:
      vpanic("s390_insn_get_reg_usage");
   }
}


/* Helper function for s390_insn_map_regs */
static void
s390_opnd_RMI_map_regs(HRegRemap *m, s390_opnd_RMI *op)
{
   switch (op->tag) {
   case S390_OPND_REG:
      op->variant.reg = lookupHRegRemap(m, op->variant.reg);
      break;

   case S390_OPND_IMMEDIATE:
      break;

   case S390_OPND_AMODE:
      s390_amode_map_regs(m, op->variant.am);
      break;

   default:
      vpanic("s390_opnd_RMI_map_regs");
   }
}


void
s390_insn_map_regs(HRegRemap *m, s390_insn *insn)
{
   switch (insn->tag) {
   case S390_INSN_LOAD:
      insn->variant.load.dst = lookupHRegRemap(m, insn->variant.load.dst);
      s390_amode_map_regs(m, insn->variant.load.src);
      break;

   case S390_INSN_STORE:
      s390_amode_map_regs(m, insn->variant.store.dst);
      insn->variant.store.src = lookupHRegRemap(m, insn->variant.store.src);
      break;

   case S390_INSN_MOVE:
      insn->variant.move.dst = lookupHRegRemap(m, insn->variant.move.dst);
      insn->variant.move.src = lookupHRegRemap(m, insn->variant.move.src);
      break;

   case S390_INSN_COND_MOVE:
      insn->variant.cond_move.dst = lookupHRegRemap(m, insn->variant.cond_move.dst);
      s390_opnd_RMI_map_regs(m, &insn->variant.cond_move.src);
      break;

   case S390_INSN_LOAD_IMMEDIATE:
      insn->variant.load_immediate.dst =
         lookupHRegRemap(m, insn->variant.load_immediate.dst);
      break;

   case S390_INSN_ALU:
      insn->variant.alu.dst = lookupHRegRemap(m, insn->variant.alu.dst);
      s390_opnd_RMI_map_regs(m, &insn->variant.alu.op2);
      break;

   case S390_INSN_MUL:
      insn->variant.mul.dst_hi = lookupHRegRemap(m, insn->variant.mul.dst_hi);
      insn->variant.mul.dst_lo = lookupHRegRemap(m, insn->variant.mul.dst_lo);
      s390_opnd_RMI_map_regs(m, &insn->variant.mul.op2);
      break;

   case S390_INSN_DIV:
      insn->variant.div.op1_hi = lookupHRegRemap(m, insn->variant.div.op1_hi);
      insn->variant.div.op1_lo = lookupHRegRemap(m, insn->variant.div.op1_lo);
      s390_opnd_RMI_map_regs(m, &insn->variant.div.op2);
      break;

   case S390_INSN_DIVS:
      insn->variant.divs.op1 = lookupHRegRemap(m, insn->variant.divs.op1);
      insn->variant.divs.rem = lookupHRegRemap(m, insn->variant.divs.rem);
      s390_opnd_RMI_map_regs(m, &insn->variant.divs.op2);
      break;

   case S390_INSN_CLZ:
      insn->variant.clz.num_bits = lookupHRegRemap(m, insn->variant.clz.num_bits);
      insn->variant.clz.clobber  = lookupHRegRemap(m, insn->variant.clz.clobber);
      s390_opnd_RMI_map_regs(m, &insn->variant.clz.src);
      break;

   case S390_INSN_UNOP:
      insn->variant.unop.dst = lookupHRegRemap(m, insn->variant.unop.dst);
      s390_opnd_RMI_map_regs(m, &insn->variant.unop.src);
      break;

   case S390_INSN_TEST:
      s390_opnd_RMI_map_regs(m, &insn->variant.test.src);
      break;

   case S390_INSN_CC2BOOL:
      insn->variant.cc2bool.dst = lookupHRegRemap(m, insn->variant.cc2bool.dst);
      break;

   case S390_INSN_CAS:
      insn->variant.cas.op1 = lookupHRegRemap(m, insn->variant.cas.op1);
      s390_amode_map_regs(m, insn->variant.cas.op2);
      insn->variant.cas.op3 = lookupHRegRemap(m, insn->variant.cas.op3);
      insn->variant.cas.old_mem = lookupHRegRemap(m, insn->variant.cas.old_mem);
      break;

   case S390_INSN_COMPARE:
      insn->variant.compare.src1 = lookupHRegRemap(m, insn->variant.compare.src1);
      s390_opnd_RMI_map_regs(m, &insn->variant.compare.src2);
      break;

   case S390_INSN_BRANCH:
      s390_opnd_RMI_map_regs(m, &insn->variant.branch.dst);
      /* No need to map S390_REGNO_RETURN_VALUE. It's not virtual */
      break;

   case S390_INSN_HELPER_CALL:
      /* s390_insn_helper_call_emit also reads / writes the link register
         and stack pointer. But those registers are not visible to the
         register allocator. So we don't need to do anything for them.
         As for the arguments of the helper call -- they will be loaded into
         non-virtual registers. Again, we don't need to do anything for those
         here. */
      break;

   case S390_INSN_BFP_TRIOP:
      insn->variant.bfp_triop.dst = lookupHRegRemap(m, insn->variant.bfp_triop.dst);
      insn->variant.bfp_triop.op2 = lookupHRegRemap(m, insn->variant.bfp_triop.op2);
      insn->variant.bfp_triop.op3 = lookupHRegRemap(m, insn->variant.bfp_triop.op3);
      break;

   case S390_INSN_BFP_BINOP:
      insn->variant.bfp_binop.dst = lookupHRegRemap(m, insn->variant.bfp_binop.dst);
      insn->variant.bfp_binop.op2 = lookupHRegRemap(m, insn->variant.bfp_binop.op2);
      break;

   case S390_INSN_BFP_UNOP:
      insn->variant.bfp_unop.dst = lookupHRegRemap(m, insn->variant.bfp_unop.dst);
      insn->variant.bfp_unop.op  = lookupHRegRemap(m, insn->variant.bfp_unop.op);
      break;

   case S390_INSN_BFP_COMPARE:
      insn->variant.bfp_compare.dst = lookupHRegRemap(m, insn->variant.bfp_compare.dst);
      insn->variant.bfp_compare.op1 = lookupHRegRemap(m, insn->variant.bfp_compare.op1);
      insn->variant.bfp_compare.op2 = lookupHRegRemap(m, insn->variant.bfp_compare.op2);
      break;

   case S390_INSN_BFP128_BINOP:
      insn->variant.bfp128_binop.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp128_binop.dst_hi);
      insn->variant.bfp128_binop.dst_lo =
         lookupHRegRemap(m, insn->variant.bfp128_binop.dst_lo);
      insn->variant.bfp128_binop.op2_hi =
         lookupHRegRemap(m, insn->variant.bfp128_binop.op2_hi);
      insn->variant.bfp128_binop.op2_lo =
         lookupHRegRemap(m, insn->variant.bfp128_binop.op2_lo);
      break;

   case S390_INSN_BFP128_COMPARE:
      insn->variant.bfp128_compare.dst =
         lookupHRegRemap(m, insn->variant.bfp128_compare.dst);
      insn->variant.bfp128_compare.op1_hi =
         lookupHRegRemap(m, insn->variant.bfp128_compare.op1_hi);
      insn->variant.bfp128_compare.op1_lo =
         lookupHRegRemap(m, insn->variant.bfp128_compare.op1_lo);
      insn->variant.bfp128_compare.op2_hi =
         lookupHRegRemap(m, insn->variant.bfp128_compare.op2_hi);
      insn->variant.bfp128_compare.op2_lo =
         lookupHRegRemap(m, insn->variant.bfp128_compare.op2_lo);
      break;

   case S390_INSN_BFP128_UNOP:
      insn->variant.bfp128_unop.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp128_unop.dst_hi);
      insn->variant.bfp128_unop.dst_lo =
         lookupHRegRemap(m, insn->variant.bfp128_unop.dst_lo);
      insn->variant.bfp128_unop.op_hi =
         lookupHRegRemap(m, insn->variant.bfp128_unop.op_hi);
      insn->variant.bfp128_unop.op_lo =
         lookupHRegRemap(m, insn->variant.bfp128_unop.op_lo);
      break;

   case S390_INSN_BFP128_CONVERT_TO:
      insn->variant.bfp128_unop.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp128_unop.dst_hi);
      insn->variant.bfp128_unop.dst_lo =
         lookupHRegRemap(m, insn->variant.bfp128_unop.dst_lo);
      insn->variant.bfp128_unop.op_hi =
         lookupHRegRemap(m, insn->variant.bfp128_unop.op_hi);
      break;

   case S390_INSN_BFP128_CONVERT_FROM:
      insn->variant.bfp128_unop.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp128_unop.dst_hi);
      insn->variant.bfp128_unop.op_hi =
         lookupHRegRemap(m, insn->variant.bfp128_unop.op_hi);
      insn->variant.bfp128_unop.op_lo =
         lookupHRegRemap(m, insn->variant.bfp128_unop.op_lo);
      break;

   case S390_INSN_MFENCE:
      break;

   default:
      vpanic("s390_insn_map_regs");
   }
}


/* Return True, if INSN is a move between two registers of the same class.
   In that case assign the source and destination registers to SRC and DST,
   respectively. */
Bool
s390_insn_is_reg_reg_move(const s390_insn *insn, HReg *src, HReg *dst)
{
   if (insn->tag == S390_INSN_MOVE &&
       hregClass(insn->variant.move.src) == hregClass(insn->variant.move.dst)) {
      *src = insn->variant.move.src;
      *dst = insn->variant.move.dst;
      return True;
   }

   return False;
}


/*------------------------------------------------------------*/
/*--- Functions to emit a sequence of bytes                ---*/
/*------------------------------------------------------------*/


static __inline__ UChar *
emit_2bytes(UChar *p, ULong val)
{
   return (UChar *)__builtin_memcpy(p, ((UChar *)&val) + 6, 2) + 2;
}


static __inline__ UChar *
emit_4bytes(UChar *p, ULong val)
{
   return (UChar *)__builtin_memcpy(p, ((UChar *)&val) + 4, 4) + 4;
}


static __inline__ UChar *
emit_6bytes(UChar *p, ULong val)
{
   return (UChar *)__builtin_memcpy(p, ((UChar *)&val) + 2, 6) + 6;
}


/*------------------------------------------------------------*/
/*--- Functions to emit various instruction formats        ---*/
/*------------------------------------------------------------*/


static UChar *
emit_RI(UChar *p, UInt op, UChar r1, UShort i2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 20;
   the_insn |= ((ULong)i2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RIL(UChar *p, ULong op, UChar r1, UInt i2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 36;
   the_insn |= ((ULong)i2) << 0;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_RR(UChar *p, UInt op, UChar r1, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 4;
   the_insn |= ((ULong)r2) << 0;

   return emit_2bytes(p, the_insn);
}


static UChar *
emit_RRE(UChar *p, UInt op, UChar r1, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 4;
   the_insn |= ((ULong)r2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RRF(UChar *p, UInt op, UChar r1, UChar r3, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 12;
   the_insn |= ((ULong)r3) << 4;
   the_insn |= ((ULong)r2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RRF3(UChar *p, UInt op, UChar r3, UChar r1, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r3) << 12;
   the_insn |= ((ULong)r1) << 4;
   the_insn |= ((ULong)r2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RS(UChar *p, UInt op, UChar r1, UChar r3, UChar b2, UShort d2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 20;
   the_insn |= ((ULong)r3) << 16;
   the_insn |= ((ULong)b2) << 12;
   the_insn |= ((ULong)d2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RSY(UChar *p, ULong op, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 36;
   the_insn |= ((ULong)r3) << 32;
   the_insn |= ((ULong)b2) << 28;
   the_insn |= ((ULong)dl2) << 16;
   the_insn |= ((ULong)dh2) << 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_RX(UChar *p, UInt op, UChar r1, UChar x2, UChar b2, UShort d2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 20;
   the_insn |= ((ULong)x2) << 16;
   the_insn |= ((ULong)b2) << 12;
   the_insn |= ((ULong)d2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RXY(UChar *p, ULong op, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 36;
   the_insn |= ((ULong)x2) << 32;
   the_insn |= ((ULong)b2) << 28;
   the_insn |= ((ULong)dl2) << 16;
   the_insn |= ((ULong)dh2) << 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_S(UChar *p, UInt op, UChar b2, UShort d2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)b2) << 12;
   the_insn |= ((ULong)d2) << 0;

   return emit_4bytes(p, the_insn);
}


/*------------------------------------------------------------*/
/*--- Functions to emit particular instructions            ---*/
/*------------------------------------------------------------*/


static UChar *
s390_emit_AR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "ar", r1, r2);

   return emit_RR(p, 0x1a00, r1, r2);
}


static UChar *
s390_emit_AGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "agr", r1, r2);

   return emit_RRE(p, 0xb9080000, r1, r2);
}


static UChar *
s390_emit_A(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "a", r1, d2, x2, b2);

   return emit_RX(p, 0x5a000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_AY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ay", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000005aULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_AG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ag", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000008ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_AFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "afi", r1, i2);

   return emit_RIL(p, 0xc20900000000ULL, r1, i2);
}


static UChar *
s390_emit_AGFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "agfi", r1, i2);

   return emit_RIL(p, 0xc20800000000ULL, r1, i2);
}


static UChar *
s390_emit_AH(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "ah", r1, d2, x2, b2);

   return emit_RX(p, 0x4a000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_AHY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ahy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000007aULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_AHI(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "ahi", r1, (Int)(Short)i2);

   return emit_RI(p, 0xa70a0000, r1, i2);
}


static UChar *
s390_emit_AGHI(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "aghi", r1, (Int)(Short)i2);

   return emit_RI(p, 0xa70b0000, r1, i2);
}


static UChar *
s390_emit_NR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "nr", r1, r2);

   return emit_RR(p, 0x1400, r1, r2);
}


static UChar *
s390_emit_NGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "ngr", r1, r2);

   return emit_RRE(p, 0xb9800000, r1, r2);
}


static UChar *
s390_emit_N(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "n", r1, d2, x2, b2);

   return emit_RX(p, 0x54000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_NY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ny", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000054ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_NG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ng", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000080ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_NIHF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "nihf", r1, i2);

   return emit_RIL(p, 0xc00a00000000ULL, r1, i2);
}


static UChar *
s390_emit_NILF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "nilf", r1, i2);

   return emit_RIL(p, 0xc00b00000000ULL, r1, i2);
}


static UChar *
s390_emit_NILL(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "nill", r1, i2);

   return emit_RI(p, 0xa5070000, r1, i2);
}


static UChar *
s390_emit_BASR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "basr", r1, r2);

   return emit_RR(p, 0x0d00, r1, r2);
}


static UChar *
s390_emit_BCR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(XMNM, GPR), S390_XMNM_BCR, r1, r2);

   return emit_RR(p, 0x0700, r1, r2);
}


static UChar *
s390_emit_BRC(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(XMNM, PCREL), S390_XMNM_BRC, r1, (Int)(Short)i2);

   return emit_RI(p, 0xa7040000, r1, i2);
}


static UChar *
s390_emit_CR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "cr", r1, r2);

   return emit_RR(p, 0x1900, r1, r2);
}


static UChar *
s390_emit_CGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "cgr", r1, r2);

   return emit_RRE(p, 0xb9200000, r1, r2);
}


static UChar *
s390_emit_C(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "c", r1, d2, x2, b2);

   return emit_RX(p, 0x59000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_CY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "cy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000059ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_CG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "cg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000020ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_CFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "cfi", r1, i2);

   return emit_RIL(p, 0xc20d00000000ULL, r1, i2);
}


static UChar *
s390_emit_CS(UChar *p, UChar r1, UChar r3, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, UDXB), "cs", r1, r3, d2, 0, b2);

   return emit_RS(p, 0xba000000, r1, r3, b2, d2);
}


static UChar *
s390_emit_CSY(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "csy", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb0000000014ULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_CSG(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "csg", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb0000000030ULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_CLR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "clr", r1, r2);

   return emit_RR(p, 0x1500, r1, r2);
}


static UChar *
s390_emit_CLGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "clgr", r1, r2);

   return emit_RRE(p, 0xb9210000, r1, r2);
}


static UChar *
s390_emit_CL(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "cl", r1, d2, x2, b2);

   return emit_RX(p, 0x55000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_CLY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "cly", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000055ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_CLG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "clg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000021ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_CLFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "clfi", r1, i2);

   return emit_RIL(p, 0xc20f00000000ULL, r1, i2);
}


static UChar *
s390_emit_DR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "dr", r1, r2);

   return emit_RR(p, 0x1d00, r1, r2);
}


static UChar *
s390_emit_D(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "d", r1, d2, x2, b2);

   return emit_RX(p, 0x5d000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_DLR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "dlr", r1, r2);

   return emit_RRE(p, 0xb9970000, r1, r2);
}


static UChar *
s390_emit_DLGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "dlgr", r1, r2);

   return emit_RRE(p, 0xb9870000, r1, r2);
}


static UChar *
s390_emit_DL(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "dl", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000097ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_DLG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "dlg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000087ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_DSGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "dsgr", r1, r2);

   return emit_RRE(p, 0xb90d0000, r1, r2);
}


static UChar *
s390_emit_DSG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "dsg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000000dULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_XR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "xr", r1, r2);

   return emit_RR(p, 0x1700, r1, r2);
}


static UChar *
s390_emit_XGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "xgr", r1, r2);

   return emit_RRE(p, 0xb9820000, r1, r2);
}


static UChar *
s390_emit_X(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "x", r1, d2, x2, b2);

   return emit_RX(p, 0x57000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_XY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "xy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000057ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_XG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "xg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000082ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_XIHF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "xihf", r1, i2);

   return emit_RIL(p, 0xc00600000000ULL, r1, i2);
}


static UChar *
s390_emit_XILF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "xilf", r1, i2);

   return emit_RIL(p, 0xc00700000000ULL, r1, i2);
}


static UChar *
s390_emit_FLOGR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "flogr", r1, r2);

   return emit_RRE(p, 0xb9830000, r1, r2);
}


static UChar *
s390_emit_IC(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "ic", r1, d2, x2, b2);

   return emit_RX(p, 0x43000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_ICY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "icy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000073ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_IIHF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "iihf", r1, i2);

   return emit_RIL(p, 0xc00800000000ULL, r1, i2);
}


static UChar *
s390_emit_IIHH(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "iihh", r1, i2);

   return emit_RI(p, 0xa5000000, r1, i2);
}


static UChar *
s390_emit_IIHL(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "iihl", r1, i2);

   return emit_RI(p, 0xa5010000, r1, i2);
}


static UChar *
s390_emit_IILF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "iilf", r1, i2);

   return emit_RIL(p, 0xc00900000000ULL, r1, i2);
}


static UChar *
s390_emit_IILH(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "iilh", r1, i2);

   return emit_RI(p, 0xa5020000, r1, i2);
}


static UChar *
s390_emit_IILL(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "iill", r1, i2);

   return emit_RI(p, 0xa5030000, r1, i2);
}


static UChar *
s390_emit_IPM(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, GPR), "ipm", r1);

   return emit_RRE(p, 0xb2220000, r1, r2);
}


static UChar *
s390_emit_LR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lr", r1, r2);

   return emit_RR(p, 0x1800, r1, r2);
}


static UChar *
s390_emit_LGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lgr", r1, r2);

   return emit_RRE(p, 0xb9040000, r1, r2);
}


static UChar *
s390_emit_LGFR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lgfr", r1, r2);

   return emit_RRE(p, 0xb9140000, r1, r2);
}


static UChar *
s390_emit_L(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "l", r1, d2, x2, b2);

   return emit_RX(p, 0x58000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_LY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ly", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000058ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000004ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGF(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lgf", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000014ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "lgfi", r1, i2);

   return emit_RIL(p, 0xc00100000000ULL, r1, i2);
}


static UChar *
s390_emit_LTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "ltr", r1, r2);

   return emit_RR(p, 0x1200, r1, r2);
}


static UChar *
s390_emit_LTGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "ltgr", r1, r2);

   return emit_RRE(p, 0xb9020000, r1, r2);
}


static UChar *
s390_emit_LT(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lt", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000012ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LTG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ltg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000002ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LBR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lbr", r1, r2);

   return emit_RRE(p, 0xb9260000, r1, r2);
}


static UChar *
s390_emit_LGBR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lgbr", r1, r2);

   return emit_RRE(p, 0xb9060000, r1, r2);
}


static UChar *
s390_emit_LB(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lb", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000076ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGB(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lgb", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000077ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LCR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lcr", r1, r2);

   return emit_RR(p, 0x1300, r1, r2);
}


static UChar *
s390_emit_LCGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lcgr", r1, r2);

   return emit_RRE(p, 0xb9030000, r1, r2);
}


static UChar *
s390_emit_LHR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lhr", r1, r2);

   return emit_RRE(p, 0xb9270000, r1, r2);
}


static UChar *
s390_emit_LGHR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "lghr", r1, r2);

   return emit_RRE(p, 0xb9070000, r1, r2);
}


static UChar *
s390_emit_LH(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "lh", r1, d2, x2, b2);

   return emit_RX(p, 0x48000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_LHY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lhy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000078ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGH(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lgh", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000015ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LHI(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "lhi", r1, (Int)(Short)i2);

   return emit_RI(p, 0xa7080000, r1, i2);
}


static UChar *
s390_emit_LGHI(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "lghi", r1, (Int)(Short)i2);

   return emit_RI(p, 0xa7090000, r1, i2);
}


static UChar *
s390_emit_LLGFR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "llgfr", r1, r2);

   return emit_RRE(p, 0xb9160000, r1, r2);
}


static UChar *
s390_emit_LLGF(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "llgf", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000016ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LLCR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "llcr", r1, r2);

   return emit_RRE(p, 0xb9940000, r1, r2);
}


static UChar *
s390_emit_LLGCR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "llgcr", r1, r2);

   return emit_RRE(p, 0xb9840000, r1, r2);
}


static UChar *
s390_emit_LLC(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "llc", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000094ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LLGC(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "llgc", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000090ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LLHR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "llhr", r1, r2);

   return emit_RRE(p, 0xb9950000, r1, r2);
}


static UChar *
s390_emit_LLGHR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "llghr", r1, r2);

   return emit_RRE(p, 0xb9850000, r1, r2);
}


static UChar *
s390_emit_LLH(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "llh", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000095ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LLGH(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "llgh", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000091ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LLILF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "llilf", r1, i2);

   return emit_RIL(p, 0xc00f00000000ULL, r1, i2);
}


static UChar *
s390_emit_LLILH(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "llilh", r1, i2);

   return emit_RI(p, 0xa50e0000, r1, i2);
}


static UChar *
s390_emit_LLILL(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "llill", r1, i2);

   return emit_RI(p, 0xa50f0000, r1, i2);
}


static UChar *
s390_emit_MR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "mr", r1, r2);

   return emit_RR(p, 0x1c00, r1, r2);
}


static UChar *
s390_emit_M(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "m", r1, d2, x2, b2);

   return emit_RX(p, 0x5c000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_MFY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "mfy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000005cULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MH(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "mh", r1, d2, x2, b2);

   return emit_RX(p, 0x4c000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_MHY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "mhy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000007cULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MHI(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "mhi", r1, (Int)(Short)i2);

   return emit_RI(p, 0xa70c0000, r1, i2);
}


static UChar *
s390_emit_MLR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "mlr", r1, r2);

   return emit_RRE(p, 0xb9960000, r1, r2);
}


static UChar *
s390_emit_MLGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "mlgr", r1, r2);

   return emit_RRE(p, 0xb9860000, r1, r2);
}


static UChar *
s390_emit_ML(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ml", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000096ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MLG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "mlg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000086ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MSR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "msr", r1, r2);

   return emit_RRE(p, 0xb2520000, r1, r2);
}


static UChar *
s390_emit_MSGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "msgr", r1, r2);

   return emit_RRE(p, 0xb90c0000, r1, r2);
}


static UChar *
s390_emit_MS(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "ms", r1, d2, x2, b2);

   return emit_RX(p, 0x71000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_MSY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "msy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000051ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MSG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "msg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000000cULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MSFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "msfi", r1, i2);

   return emit_RIL(p, 0xc20100000000ULL, r1, i2);
}


static UChar *
s390_emit_MSGFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "msgfi", r1, i2);

   return emit_RIL(p, 0xc20000000000ULL, r1, i2);
}


static UChar *
s390_emit_OR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "or", r1, r2);

   return emit_RR(p, 0x1600, r1, r2);
}


static UChar *
s390_emit_OGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "ogr", r1, r2);

   return emit_RRE(p, 0xb9810000, r1, r2);
}


static UChar *
s390_emit_O(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "o", r1, d2, x2, b2);

   return emit_RX(p, 0x56000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_OY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "oy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000056ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_OG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "og", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000081ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_OIHF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "oihf", r1, i2);

   return emit_RIL(p, 0xc00c00000000ULL, r1, i2);
}


static UChar *
s390_emit_OILF(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "oilf", r1, i2);

   return emit_RIL(p, 0xc00d00000000ULL, r1, i2);
}


static UChar *
s390_emit_OILL(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "oill", r1, i2);

   return emit_RI(p, 0xa50b0000, r1, i2);
}


static UChar *
s390_emit_SLL(UChar *p, UChar r1, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "sll", r1, d2, 0, b2);

   return emit_RS(p, 0x89000000, r1, 0, b2, d2);
}


static UChar *
s390_emit_SLLG(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "sllg", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb000000000dULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_SRA(UChar *p, UChar r1, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "sra", r1, d2, 0, b2);

   return emit_RS(p, 0x8a000000, r1, 0, b2, d2);
}


static UChar *
s390_emit_SRAG(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "srag", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb000000000aULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_SRL(UChar *p, UChar r1, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "srl", r1, d2, 0, b2);

   return emit_RS(p, 0x88000000, r1, 0, b2, d2);
}


static UChar *
s390_emit_SRLG(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "srlg", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb000000000cULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_ST(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "st", r1, d2, x2, b2);

   return emit_RX(p, 0x50000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_STY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "sty", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000050ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "stg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000024ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STC(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "stc", r1, d2, x2, b2);

   return emit_RX(p, 0x42000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_STCY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "stcy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000072ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STH(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "sth", r1, d2, x2, b2);

   return emit_RX(p, 0x40000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_STHY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "sthy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000070ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_SR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "sr", r1, r2);

   return emit_RR(p, 0x1b00, r1, r2);
}


static UChar *
s390_emit_SGR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, GPR), "sgr", r1, r2);

   return emit_RRE(p, 0xb9090000, r1, r2);
}


static UChar *
s390_emit_S(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "s", r1, d2, x2, b2);

   return emit_RX(p, 0x5b000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_SY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "sy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000005bULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_SG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "sg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000009ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_SH(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UDXB), "sh", r1, d2, x2, b2);

   return emit_RX(p, 0x4b000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_SHY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "shy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000007bULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_SLFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "slfi", r1, i2);

   return emit_RIL(p, 0xc20500000000ULL, r1, i2);
}


static UChar *
s390_emit_SLGFI(UChar *p, UChar r1, UInt i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "slgfi", r1, i2);

   return emit_RIL(p, 0xc20400000000ULL, r1, i2);
}


static UChar *
s390_emit_LDR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "ldr", r1, r2);

   return emit_RR(p, 0x2800, r1, r2);
}


static UChar *
s390_emit_LE(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, UDXB), "le", r1, d2, x2, b2);

   return emit_RX(p, 0x78000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_LD(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, UDXB), "ld", r1, d2, x2, b2);

   return emit_RX(p, 0x68000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_LEY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, SDXB), "ley", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xed0000000064ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LDY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, SDXB), "ldy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xed0000000065ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LFPC(UChar *p, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, UDXB), "lfpc", d2, 0, b2);

   return emit_S(p, 0xb29d0000, b2, d2);
}


static UChar *
s390_emit_LDGR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_fgx);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "ldgr", r1, r2);

   return emit_RRE(p, 0xb3c10000, r1, r2);
}


static UChar *
s390_emit_LGDR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_fgx);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, FPR), "lgdr", r1, r2);

   return emit_RRE(p, 0xb3cd0000, r1, r2);
}


static UChar *
s390_emit_LZER(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, FPR), "lzer", r1);

   return emit_RRE(p, 0xb3740000, r1, r2);
}


static UChar *
s390_emit_LZDR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, FPR), "lzdr", r1);

   return emit_RRE(p, 0xb3750000, r1, r2);
}


static UChar *
s390_emit_SFPC(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, GPR), "sfpc", r1);

   return emit_RRE(p, 0xb3840000, r1, r2);
}


static UChar *
s390_emit_STE(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, UDXB), "ste", r1, d2, x2, b2);

   return emit_RX(p, 0x70000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_STD(UChar *p, UChar r1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, UDXB), "std", r1, d2, x2, b2);

   return emit_RX(p, 0x60000000, r1, x2, b2, d2);
}


static UChar *
s390_emit_STEY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, SDXB), "stey", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xed0000000066ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STDY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, SDXB), "stdy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xed0000000067ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STFPC(UChar *p, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, UDXB), "stfpc", d2, 0, b2);

   return emit_S(p, 0xb29c0000, b2, d2);
}


static UChar *
s390_emit_AEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "aebr", r1, r2);

   return emit_RRE(p, 0xb30a0000, r1, r2);
}


static UChar *
s390_emit_ADBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "adbr", r1, r2);

   return emit_RRE(p, 0xb31a0000, r1, r2);
}


static UChar *
s390_emit_AXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "axbr", r1, r2);

   return emit_RRE(p, 0xb34a0000, r1, r2);
}


static UChar *
s390_emit_CEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cebr", r1, r2);

   return emit_RRE(p, 0xb3090000, r1, r2);
}


static UChar *
s390_emit_CDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cdbr", r1, r2);

   return emit_RRE(p, 0xb3190000, r1, r2);
}


static UChar *
s390_emit_CXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cxbr", r1, r2);

   return emit_RRE(p, 0xb3490000, r1, r2);
}


static UChar *
s390_emit_CEFBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cefbr", r1, r2);

   return emit_RRE(p, 0xb3940000, r1, r2);
}


static UChar *
s390_emit_CDFBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cdfbr", r1, r2);

   return emit_RRE(p, 0xb3950000, r1, r2);
}


static UChar *
s390_emit_CXFBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cxfbr", r1, r2);

   return emit_RRE(p, 0xb3960000, r1, r2);
}


static UChar *
s390_emit_CEGBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cegbr", r1, r2);

   return emit_RRE(p, 0xb3a40000, r1, r2);
}


static UChar *
s390_emit_CDGBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cdgbr", r1, r2);

   return emit_RRE(p, 0xb3a50000, r1, r2);
}


static UChar *
s390_emit_CXGBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cxgbr", r1, r2);

   return emit_RRE(p, 0xb3a60000, r1, r2);
}


static UChar *
s390_emit_CFEBR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cfebr", r1, r3, r2);

   return emit_RRF3(p, 0xb3980000, r3, r1, r2);
}


static UChar *
s390_emit_CFDBR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cfdbr", r1, r3, r2);

   return emit_RRF3(p, 0xb3990000, r3, r1, r2);
}


static UChar *
s390_emit_CFXBR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cfxbr", r1, r3, r2);

   return emit_RRF3(p, 0xb39a0000, r3, r1, r2);
}


static UChar *
s390_emit_CGEBR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cgebr", r1, r3, r2);

   return emit_RRF3(p, 0xb3a80000, r3, r1, r2);
}


static UChar *
s390_emit_CGDBR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cgdbr", r1, r3, r2);

   return emit_RRF3(p, 0xb3a90000, r3, r1, r2);
}


static UChar *
s390_emit_CGXBR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cgxbr", r1, r3, r2);

   return emit_RRF3(p, 0xb3aa0000, r3, r1, r2);
}


static UChar *
s390_emit_DEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "debr", r1, r2);

   return emit_RRE(p, 0xb30d0000, r1, r2);
}


static UChar *
s390_emit_DDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "ddbr", r1, r2);

   return emit_RRE(p, 0xb31d0000, r1, r2);
}


static UChar *
s390_emit_DXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "dxbr", r1, r2);

   return emit_RRE(p, 0xb34d0000, r1, r2);
}


static UChar *
s390_emit_LCEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lcebr", r1, r2);

   return emit_RRE(p, 0xb3030000, r1, r2);
}


static UChar *
s390_emit_LCDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lcdbr", r1, r2);

   return emit_RRE(p, 0xb3130000, r1, r2);
}


static UChar *
s390_emit_LCXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lcxbr", r1, r2);

   return emit_RRE(p, 0xb3430000, r1, r2);
}


static UChar *
s390_emit_LDEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "ldebr", r1, r2);

   return emit_RRE(p, 0xb3040000, r1, r2);
}


static UChar *
s390_emit_LXDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lxdbr", r1, r2);

   return emit_RRE(p, 0xb3050000, r1, r2);
}


static UChar *
s390_emit_LXEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lxebr", r1, r2);

   return emit_RRE(p, 0xb3060000, r1, r2);
}


static UChar *
s390_emit_LNEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lnebr", r1, r2);

   return emit_RRE(p, 0xb3010000, r1, r2);
}


static UChar *
s390_emit_LNDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lndbr", r1, r2);

   return emit_RRE(p, 0xb3110000, r1, r2);
}


static UChar *
s390_emit_LNXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lnxbr", r1, r2);

   return emit_RRE(p, 0xb3410000, r1, r2);
}


static UChar *
s390_emit_LPEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lpebr", r1, r2);

   return emit_RRE(p, 0xb3000000, r1, r2);
}


static UChar *
s390_emit_LPDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lpdbr", r1, r2);

   return emit_RRE(p, 0xb3100000, r1, r2);
}


static UChar *
s390_emit_LPXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lpxbr", r1, r2);

   return emit_RRE(p, 0xb3400000, r1, r2);
}


static UChar *
s390_emit_LEDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "ledbr", r1, r2);

   return emit_RRE(p, 0xb3440000, r1, r2);
}


static UChar *
s390_emit_LDXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "ldxbr", r1, r2);

   return emit_RRE(p, 0xb3450000, r1, r2);
}


static UChar *
s390_emit_LEXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "lexbr", r1, r2);

   return emit_RRE(p, 0xb3460000, r1, r2);
}


static UChar *
s390_emit_MEEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "meebr", r1, r2);

   return emit_RRE(p, 0xb3170000, r1, r2);
}


static UChar *
s390_emit_MDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "mdbr", r1, r2);

   return emit_RRE(p, 0xb31c0000, r1, r2);
}


static UChar *
s390_emit_MXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "mxbr", r1, r2);

   return emit_RRE(p, 0xb34c0000, r1, r2);
}


static UChar *
s390_emit_MAEBR(UChar *p, UChar r1, UChar r3, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, FPR), "maebr", r1, r3, r2);

   return emit_RRF(p, 0xb30e0000, r1, r3, r2);
}


static UChar *
s390_emit_MADBR(UChar *p, UChar r1, UChar r3, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, FPR), "madbr", r1, r3, r2);

   return emit_RRF(p, 0xb31e0000, r1, r3, r2);
}


static UChar *
s390_emit_MSEBR(UChar *p, UChar r1, UChar r3, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, FPR), "msebr", r1, r3, r2);

   return emit_RRF(p, 0xb30f0000, r1, r3, r2);
}


static UChar *
s390_emit_MSDBR(UChar *p, UChar r1, UChar r3, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, FPR), "msdbr", r1, r3, r2);

   return emit_RRF(p, 0xb31f0000, r1, r3, r2);
}


static UChar *
s390_emit_SQEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "sqebr", r1, r2);

   return emit_RRE(p, 0xb3140000, r1, r2);
}


static UChar *
s390_emit_SQDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "sqdbr", r1, r2);

   return emit_RRE(p, 0xb3150000, r1, r2);
}


static UChar *
s390_emit_SQXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "sqxbr", r1, r2);

   return emit_RRE(p, 0xb3160000, r1, r2);
}


static UChar *
s390_emit_SEBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "sebr", r1, r2);

   return emit_RRE(p, 0xb30b0000, r1, r2);
}


static UChar *
s390_emit_SDBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "sdbr", r1, r2);

   return emit_RRE(p, 0xb31b0000, r1, r2);
}


static UChar *
s390_emit_SXBR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "sxbr", r1, r2);

   return emit_RRE(p, 0xb34b0000, r1, r2);
}


/* Provide a symbolic name for register "R0" */
#define R0 0

/* Split up a 20-bit displacement into its high and low piece
   suitable for passing as function arguments */
#define DISP20(d) (((UInt)d) & 0xFFF), ((((UInt)d) >> 12) & 0xFF)

/*---------------------------------------------------------------*/
/*--- Helper functions                                        ---*/
/*---------------------------------------------------------------*/

static __inline__ Bool
uint_fits_signed_16bit(UInt val)
{
   int v = val & 0xFFFFu;

   /* sign extend */
   v = (v << 16) >> 16;

   return val == (UInt)v;
}


static __inline__ Bool
ulong_fits_signed_16bit(ULong val)
{
   Long v = val & 0xFFFFu;

   /* sign extend */
   v = (v << 48) >> 48;

   return val == (ULong)v;
}


static __inline__ Bool
ulong_fits_signed_32bit(ULong val)
{
   Long v = val & 0xFFFFFFFFu;

   /* sign extend */
   v = (v << 32) >> 32;

   return val == (ULong)v;
}


static __inline__ Bool
ulong_fits_unsigned_32bit(ULong val)
{
   return (val & 0xFFFFFFFFu) == val;
}


/* Load a 64-bit immediate VAL into register REG. */
static UChar *
s390_emit_load_64imm(UChar *p, UChar reg, ULong val)
{
   if (ulong_fits_signed_16bit(val)) {
      return s390_emit_LGHI(p, reg, val);
   }

   if (s390_host_has_eimm) {
      if (ulong_fits_unsigned_32bit(val)) {
         return s390_emit_LLILF(p, reg, val);
      }
      if (ulong_fits_signed_32bit(val)) {
         /* LGFI's sign extension will recreate the correct 64-bit value */
         return s390_emit_LGFI(p, reg, val);
      }
      /* Do it in two steps: upper half [0:31] and lower half [32:63] */
      p =  s390_emit_IIHF(p, reg, val >> 32);
      return s390_emit_IILF(p, reg, val & 0xFFFFFFFF);
   }

   /* Fall back */
   if (ulong_fits_unsigned_32bit(val)) {
      p = s390_emit_LLILH(p, reg, (val >> 16) & 0xFFFF); /* sets val[32:47]
                                                            val[0:31] = 0 */
      p = s390_emit_IILL(p, reg, val & 0xFFFF);          /* sets val[48:63] */
      return p;
   }

   p = s390_emit_IIHH(p, reg, (val >> 48) & 0xFFFF);
   p = s390_emit_IIHL(p, reg, (val >> 32) & 0xFFFF);
   p = s390_emit_IILH(p, reg, (val >> 16) & 0xFFFF);
   p = s390_emit_IILL(p, reg, val & 0xFFFF);

   return p;
}

/* Load a 32-bit immediate VAL into register REG. */
static UChar *
s390_emit_load_32imm(UChar *p, UChar reg, UInt val)
{
   if (uint_fits_signed_16bit(val)) {
      /* LHI's sign extension will recreate the correct 32-bit value */
      return s390_emit_LHI(p, reg, val);
   }
   if (s390_host_has_eimm) {
      return s390_emit_IILF(p, reg, val);
   }
   /* val[0:15]  --> (val >> 16) & 0xFFFF
      val[16:31] --> val & 0xFFFF */
   p = s390_emit_IILH(p, reg, (val >> 16) & 0xFFFF);
   return s390_emit_IILL(p, reg, val & 0xFFFF);
}

/*------------------------------------------------------------*/
/*--- Wrapper functions                                    ---*/
/*------------------------------------------------------------*/

/* r1[32:63],r1+1[32:63] = r1+1[32:63] * memory[op2addr][0:31] */
static UChar *
s390_emit_MFYw(UChar *p, UChar r1, UChar x, UChar b,  UShort dl, UChar dh)
{
   if (s390_host_has_gie) {
      return s390_emit_MFY(p, r1, x, b, dl, dh);
   }

   /* Load from memory into R0, then MULTIPLY with R1 */
   p = s390_emit_LY(p, R0, x, b, dl, dh);
   return s390_emit_MR(p, r1, R0);
}

/* r1[32:63] = r1[32:63] * memory[op2addr][0:15] */
static UChar *
s390_emit_MHYw(UChar *p, UChar r1, UChar x, UChar b,  UShort dl, UChar dh)
{
   if (s390_host_has_gie) {
      return s390_emit_MHY(p, r1, x, b, dl, dh);
   }

   /* Load from memory into R0, then MULTIPLY with R1 */
   p = s390_emit_LHY(p, R0, x, b, dl, dh);
   return s390_emit_MSR(p, r1, R0);
}

/* r1[32:63] = r1[32:63] * i2 */
static UChar *
s390_emit_MSFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_gie) {
      return s390_emit_MSFI(p, r1, i2);
   }

   /* Load I2 into R0; then MULTIPLY R0 with R1 */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_MSR(p, r1, R0);
}


/* r1[32:63] = r1[32:63] & i2 */
static UChar *
s390_emit_NILFw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_NILF(p, r1, i2);
   }

   /* Load I2 into R0; then AND R0 with R1 */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_NR(p, r1, R0);
}


/* r1[32:63] = r1[32:63] | i2 */
static UChar *
s390_emit_OILFw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_OILF(p, r1, i2);
   }

   /* Load I2 into R0; then AND R0 with R1 */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_OR(p, r1, R0);
}


/* r1[32:63] = r1[32:63] ^ i2 */
static UChar *
s390_emit_XILFw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_XILF(p, r1, i2);
   }

   /* Load I2 into R0; then AND R0 with R1 */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_XR(p, r1, R0);
}


/* r1[32:63] = sign_extend(mem[op2addr][0:7]) */
static UChar *
s390_emit_LBw(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (s390_host_has_ldisp) {
      return s390_emit_LB(p, r1, x2, b2, dl2, dh2);
   }

   p = s390_emit_IC(p, r1, x2, b2, dl2);    /* r1[56:63] = mem[op2addr][0:7] */
   p = s390_emit_SLL(p, r1, R0, 24);        /* r1 = r1 << 24  */
   return s390_emit_SRA(p, r1, R0, 24);     /* r1 = r1 >>a 24 */
}


/*  r1[32:63] = sign_extend(r2[56:63]) */
static UChar *
s390_emit_LBRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LBR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);               /* r1 = r2 */
   p = s390_emit_SLL(p, r1, R0, 24);          /* r1 = r1 << 24  */
   return s390_emit_SRA(p, r1, R0, 24);       /* r1 = r1 >>a 24 */
}


/* r1[0:63] = sign_extend(mem[op2addr][0:7]) */
static UChar *
s390_emit_LGBw(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   vassert(s390_host_has_ldisp || dh2 == 0);

   if (s390_host_has_ldisp) {
      return s390_emit_LGB(p, r1, x2, b2, dl2, dh2);
   }

   p = s390_emit_IC(p, r1, x2, b2, dl2);             /* r1[56:63] = mem[op2addr][0:7] */
   p = s390_emit_SLLG(p, r1, r1, R0, DISP20(56));    /* r1 = r1 << 56  */
   return s390_emit_SRAG(p, r1, r1, R0, DISP20(56)); /* r1 = r1 >>a 56 */
}


/*  r1[0:63] = sign_extend(r2[56:63]) */
static UChar *
s390_emit_LGBRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LGBR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);                       /* r1 = r2 */
   p = s390_emit_SLLG(p, r1, r1, R0, DISP20(56));     /* r1 = r1 << 56  */
   return s390_emit_SRAG(p, r1, r1, R0, DISP20(56));  /* r1 = r1 >>a 56 */
}


/* r1[32:63] = sign_extend(r2[48:63]) */
static UChar *
s390_emit_LHRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LHR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);               /* r1 = r2 */
   p = s390_emit_SLL(p, r1, R0, 16);          /* r1 = r1 << 16  */
   return s390_emit_SRA(p, r1, R0, 16);       /* r1 = r1 >>a 16 */
}


/* r1[0:63] = sign_extend(r2[48:63]) */
static UChar *
s390_emit_LGHRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LGHR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);               /* r1 = r2 */
   p = s390_emit_SLLG(p, r1, r1, R0, DISP20(48));     /* r1 = r1 << 48  */
   return s390_emit_SRAG(p, r1, r1, R0, DISP20(48));  /* r1 = r1 >>a 48 */
}


/* r1[0:63] = sign_extend(i2) */
static UChar *
s390_emit_LGFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LGFI(p, r1, i2);
   }

   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_LGFR(p, r1, R0);
}


/* r1[32:63] = zero_extend($r2[56:63]) */
static UChar *
s390_emit_LLCRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLCR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);
   p = s390_emit_LHI(p, R0, 0xFF);
   return s390_emit_NR(p, r1, R0);
}


/* r1[0:63] = zero_extend($r2[56:63]) */
static UChar *
s390_emit_LLGCRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLGCR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);
   p = s390_emit_LLILL(p, R0, 0xFF);
   return s390_emit_NGR(p, r1, R0);
}


/* r1[32:63] = zero_extend(r2[48:63]) */
static UChar *
s390_emit_LLHRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLHR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);
   p = s390_emit_LLILL(p, R0, 0xFFFF);
   return s390_emit_NR(p, r1, R0);
}


/* r1[0:63] = zero_extend(r2[48:63]) */
static UChar *
s390_emit_LLGHRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLGHR(p, r1, r2);
   }

   p = s390_emit_LR(p, r1, r2);
   p = s390_emit_LLILL(p, R0, 0xFFFF);
   return s390_emit_NGR(p, r1, R0);
}


/* r1[32:63] = zero_extend(mem[op2addr][0:7]) */
static UChar *
s390_emit_LLCw(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl, UChar dh)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLC(p, r1, x2, b2, dl, dh);
   }

   if (dh == 0) {
      p = s390_emit_IC(p, r1, x2, b2, dl);
   } else {
      p = s390_emit_ICY(p, r1, x2, b2, dl, dh);
   }
   p = s390_emit_LLILL(p, R0, 0xFF);
   return s390_emit_NR(p, r1, R0);
}


/* r1[32:63] = zero_extend(mem[op2addr][0:15]) */
static UChar *
s390_emit_LLHw(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl, UChar dh)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLH(p, r1, x2, b2, dl, dh);
   }

   p = s390_emit_LLGH(p, r1, x2, b2, dl, dh);
   p = s390_emit_LLILL(p, R0, 0xFFFF);
   return s390_emit_NR(p, r1, R0);
}


/* r1[0:63] = zero_extend(i2) */
static UChar *
s390_emit_LLILFw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_LLILF(p, r1, i2);
   }

   p = s390_emit_LLILH(p, r1, (i2 >> 16) & 0xFFFF);  /* i2[0:15] */
   return s390_emit_OILL(p, r1, i2 & 0xFFFF);
}


/* r1[32:63] = r1[32:63] + i2 */
static UChar *
s390_emit_AFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_AFI(p, r1, i2);
   }
   /* Load 32 bit immediate to R0 then add */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_AR(p, r1, R0);
}


/* r1[32:63] = r1[32:63] - i2 */
static UChar *
s390_emit_SLFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_SLFI(p, r1, i2);
   }

   /* Load 32 bit immediate to R0 then subtract */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_SR(p, r1, R0);
}


/* r1[0:63] = r1[0:63] - zero_extend(i2) */
static UChar *
s390_emit_SLGFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_SLGFI(p, r1, i2);
   }

   /* Load zero-extended 32 bit immediate to R0 then subtract */
   p = s390_emit_load_64imm(p, R0, i2);
   return s390_emit_SGR(p, r1, R0);
}


static UChar *
s390_emit_LTw(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl, UChar dh)
{
   if (s390_host_has_eimm) {
      return s390_emit_LT(p, r1, x2, b2, dl, dh);
   }
   /* Load 32 bit from memory to R0 then compare */
   if (dh == 0) {
      p = s390_emit_L(p, R0, x2, b2, dl);
   } else {
      p = s390_emit_LY(p, R0, x2, b2, dl, dh);
   }
   return s390_emit_LTR(p, r1, R0);
}


static UChar *
s390_emit_LTGw(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl, UChar dh)
{
   if (s390_host_has_eimm) {
      return s390_emit_LTG(p, r1, x2, b2, dl, dh);
   }
   /* Load 64 bit from memory to R0 then compare */
   p = s390_emit_LG(p, R0, x2, b2, dl, dh);
   return s390_emit_LTGR(p, r1, R0);
}


static UChar *
s390_emit_CFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_CFI(p, r1, i2);
   }
   /* Load 32 bit immediate to R0 then compare */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_CR(p, r1, R0);
}


static UChar *
s390_emit_CLFIw(UChar *p, UChar r1, UInt i2)
{
   if (s390_host_has_eimm) {
      return s390_emit_CLFI(p, r1, i2);
   }
   /* Load 32 bit immediate to R0 then compare */
   p = s390_emit_load_32imm(p, R0, i2);
   return s390_emit_CLR(p, r1, R0);
}


static UChar *
s390_emit_LGDRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_fgx) {
      return s390_emit_LGDR(p, r1, r2);
   }

   /* Store the FPR at memory[sp - 8]. This is safe because SP grows towards
      smaller addresses and is 8-byte aligned. Then load the GPR from that
      memory location/ */
   if (s390_host_has_ldisp) {
      p = s390_emit_STDY(p, r2, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
      return s390_emit_LG(p, r1, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
   }

   /* No long displacement. Need to adjust SP explicitly as to avoid negative
      displacements. */
   p = s390_emit_AGHI(p, S390_REGNO_STACK_POINTER, -8);
   p = s390_emit_STD(p, r2, R0, S390_REGNO_STACK_POINTER, 0);
   p = s390_emit_LG(p, r1, R0, S390_REGNO_STACK_POINTER, DISP20(0));
   return s390_emit_AGHI(p, S390_REGNO_STACK_POINTER, 8);
}


static UChar *
s390_emit_LDGRw(UChar *p, UChar r1, UChar r2)
{
   if (s390_host_has_fgx) {
      return s390_emit_LDGR(p, r1, r2);
   }

   /* Store the GPR at memory[sp - 8]. This is safe because SP grows towards
      smaller addresses and is 8-byte aligned. Then load the FPR from that
      memory location/ */
   if (s390_host_has_ldisp) {
      p = s390_emit_STG(p, r2, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
      return s390_emit_LDY(p, r1, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
   }

   /* No long displacement. Need to adjust SP explicitly as to avoid negative
      displacements. */
   p = s390_emit_AGHI(p, S390_REGNO_STACK_POINTER, -8);
   p = s390_emit_STG(p, r2, R0, S390_REGNO_STACK_POINTER, DISP20(0));
   p = s390_emit_LD(p, r1, R0, S390_REGNO_STACK_POINTER, 0);
   return s390_emit_AGHI(p, S390_REGNO_STACK_POINTER, 8);
}


/*---------------------------------------------------------------*/
/*--- Constructors for the various s390_insn kinds            ---*/
/*---------------------------------------------------------------*/

s390_insn *
s390_insn_load(UChar size, HReg dst, s390_amode *src)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_LOAD;
   insn->size = size;
   insn->variant.load.src  = src;
   insn->variant.load.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   return insn;
}


s390_insn *
s390_insn_store(UChar size, s390_amode *dst, HReg src)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_STORE;
   insn->size = size;
   insn->variant.store.src  = src;
   insn->variant.store.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   return insn;
}


s390_insn *
s390_insn_move(UChar size, HReg dst, HReg src)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_MOVE;
   insn->size = size;
   insn->variant.move.src  = src;
   insn->variant.move.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   return insn;
}


s390_insn *
s390_insn_cond_move(UChar size, s390_cc_t cond, HReg dst, s390_opnd_RMI src)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_COND_MOVE;
   insn->size = size;
   insn->variant.cond_move.cond = cond;
   insn->variant.cond_move.src  = src;
   insn->variant.cond_move.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   return insn;
}


s390_insn *
s390_insn_load_immediate(UChar size, HReg dst, ULong value)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_LOAD_IMMEDIATE;
   insn->size = size;
   insn->variant.load_immediate.dst   = dst;
   insn->variant.load_immediate.value = value;

   return insn;
}


s390_insn *
s390_insn_alu(UChar size, s390_alu_t tag, HReg dst, s390_opnd_RMI op2)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_ALU;
   insn->size = size;
   insn->variant.alu.tag = tag;
   insn->variant.alu.dst = dst;
   insn->variant.alu.op2 = op2;

   return insn;
}


s390_insn *
s390_insn_mul(UChar size, HReg dst_hi, HReg dst_lo, s390_opnd_RMI op2,
              Bool signed_multiply)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(! hregIsVirtual(dst_hi));
   vassert(! hregIsVirtual(dst_lo));

   insn->tag  = S390_INSN_MUL;
   insn->size = size;
   insn->variant.mul.dst_hi = dst_hi;
   insn->variant.mul.dst_lo = dst_lo;
   insn->variant.mul.op2 = op2;
   insn->variant.mul.signed_multiply = signed_multiply;

   return insn;
}


s390_insn *
s390_insn_div(UChar size, HReg op1_hi, HReg op1_lo, s390_opnd_RMI op2,
              Bool signed_divide)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 4 || size == 8);
   vassert(! hregIsVirtual(op1_hi));
   vassert(! hregIsVirtual(op1_lo));

   insn->tag  = S390_INSN_DIV;
   insn->size = size;
   insn->variant.div.op1_hi = op1_hi;
   insn->variant.div.op1_lo = op1_lo;
   insn->variant.div.op2 = op2;
   insn->variant.div.signed_divide = signed_divide;

   return insn;
}


s390_insn *
s390_insn_divs(UChar size, HReg rem, HReg op1, s390_opnd_RMI op2)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 8);
   vassert(! hregIsVirtual(op1));
   vassert(! hregIsVirtual(rem));

   insn->tag  = S390_INSN_DIVS;
   insn->size = size;
   insn->variant.divs.rem = rem;   /* remainder */
   insn->variant.divs.op1 = op1;   /* also quotient */
   insn->variant.divs.op2 = op2;

   return insn;
}


s390_insn *
s390_insn_clz(UChar size, HReg num_bits, HReg clobber, s390_opnd_RMI src)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 8);
   vassert(! hregIsVirtual(num_bits));
   vassert(! hregIsVirtual(clobber));

   insn->tag  = S390_INSN_CLZ;
   insn->size = size;
   insn->variant.clz.num_bits = num_bits;
   insn->variant.clz.clobber  = clobber;
   insn->variant.clz.src = src;

   return insn;
}


s390_insn *
s390_insn_unop(UChar size, s390_unop_t tag, HReg dst, s390_opnd_RMI opnd)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_UNOP;
   insn->size = size;
   insn->variant.unop.tag = tag;
   insn->variant.unop.dst = dst;
   insn->variant.unop.src = opnd;

   return insn;
}


s390_insn *
s390_insn_test(UChar size, s390_opnd_RMI src)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_TEST;
   insn->size = size;
   insn->variant.test.src = src;

   return insn;
}


s390_insn *
s390_insn_cc2bool(HReg dst, s390_cc_t cond)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_CC2BOOL;
   insn->size = 0;   /* does not matter */
   insn->variant.cc2bool.cond = cond;
   insn->variant.cc2bool.dst  = dst;

   return insn;
}


s390_insn *
s390_insn_cas(UChar size, HReg op1, s390_amode *op2, HReg op3, HReg old_mem)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 4 || size == 8);
   vassert(op2->x == 0);

   insn->tag  = S390_INSN_CAS;
   insn->size = size;
   insn->variant.cas.op1 = op1;
   insn->variant.cas.op2 = op2;
   insn->variant.cas.op3 = op3;
   insn->variant.cas.old_mem = old_mem;

   return insn;
}


s390_insn *
s390_insn_compare(UChar size, HReg src1, s390_opnd_RMI src2,
                  Bool signed_comparison)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_COMPARE;
   insn->size = size;
   insn->variant.compare.src1 = src1;
   insn->variant.compare.src2 = src2;
   insn->variant.compare.signed_comparison = signed_comparison;

   return insn;
}


s390_insn *
s390_insn_branch(IRJumpKind kind, s390_cc_t cond, s390_opnd_RMI dst)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BRANCH;
   insn->size = 0;  /* does not matter */
   insn->variant.branch.kind = kind;
   insn->variant.branch.dst  = dst;
   insn->variant.branch.cond = cond;

   return insn;
}


s390_insn *
s390_insn_helper_call(s390_cc_t cond, Addr64 target, UInt num_args,
                      HChar *name)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_HELPER_CALL;
   insn->size = 0;  /* does not matter */
   insn->variant.helper_call.cond = cond;
   insn->variant.helper_call.target = target;
   insn->variant.helper_call.num_args = num_args;
   insn->variant.helper_call.name = name;

   return insn;
}


s390_insn *
s390_insn_bfp_triop(UChar size, s390_bfp_triop_t tag, HReg dst, HReg op2,
                    HReg op3, s390_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP_TRIOP;
   insn->size = size;
   insn->variant.bfp_triop.tag = tag;
   insn->variant.bfp_triop.dst = dst;
   insn->variant.bfp_triop.op2 = op2;
   insn->variant.bfp_triop.op3 = op3;
   insn->variant.bfp_triop.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_bfp_binop(UChar size, s390_bfp_binop_t tag, HReg dst, HReg op2,
                    s390_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP_BINOP;
   insn->size = size;
   insn->variant.bfp_binop.tag = tag;
   insn->variant.bfp_binop.dst = dst;
   insn->variant.bfp_binop.op2 = op2;
   insn->variant.bfp_binop.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_bfp_unop(UChar size, s390_bfp_unop_t tag, HReg dst, HReg op,
                   s390_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP_UNOP;
   insn->size = size;
   insn->variant.bfp_unop.tag = tag;
   insn->variant.bfp_unop.dst = dst;
   insn->variant.bfp_unop.op  = op;
   insn->variant.bfp_unop.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_bfp_compare(UChar size, HReg dst, HReg op1, HReg op2)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_BFP_COMPARE;
   insn->size = size;
   insn->variant.bfp_compare.dst = dst;
   insn->variant.bfp_compare.op1 = op1;
   insn->variant.bfp_compare.op2 = op2;

   return insn;
}


s390_insn *
s390_insn_bfp128_binop(UChar size, s390_bfp_binop_t tag, HReg dst_hi,
                       HReg dst_lo, HReg op2_hi, HReg op2_lo,
                       s390_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP128_BINOP;
   insn->size = size;
   insn->variant.bfp128_binop.tag = tag;
   insn->variant.bfp128_binop.dst_hi = dst_hi;
   insn->variant.bfp128_binop.dst_lo = dst_lo;
   insn->variant.bfp128_binop.op2_hi = op2_hi;
   insn->variant.bfp128_binop.op2_lo = op2_lo;
   insn->variant.bfp128_binop.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_bfp128_unop(UChar size, s390_bfp_unop_t tag, HReg dst_hi,
                      HReg dst_lo, HReg op_hi, HReg op_lo,
                      s390_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP128_UNOP;
   insn->size = size;
   insn->variant.bfp128_unop.tag = tag;
   insn->variant.bfp128_unop.dst_hi = dst_hi;
   insn->variant.bfp128_unop.dst_lo = dst_lo;
   insn->variant.bfp128_unop.op_hi = op_hi;
   insn->variant.bfp128_unop.op_lo = op_lo;
   insn->variant.bfp128_unop.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_bfp128_compare(UChar size, HReg dst, HReg op1_hi, HReg op1_lo,
                         HReg op2_hi, HReg op2_lo)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP128_COMPARE;
   insn->size = size;
   insn->variant.bfp128_compare.dst = dst;
   insn->variant.bfp128_compare.op1_hi = op1_hi;
   insn->variant.bfp128_compare.op1_lo = op1_lo;
   insn->variant.bfp128_compare.op2_hi = op2_hi;
   insn->variant.bfp128_compare.op2_lo = op2_lo;

   return insn;
}


s390_insn *
s390_insn_bfp128_convert_to(UChar size, s390_bfp_unop_t tag, HReg dst_hi,
                            HReg dst_lo, HReg op)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP128_CONVERT_TO;
   insn->size = size;
   insn->variant.bfp128_unop.tag = tag;
   insn->variant.bfp128_unop.dst_hi = dst_hi;
   insn->variant.bfp128_unop.dst_lo = dst_lo;
   insn->variant.bfp128_unop.op_hi = op;
   insn->variant.bfp128_unop.op_lo = INVALID_HREG;  /* unused */
   insn->variant.bfp128_unop.rounding_mode = S390_ROUND_NEAREST_EVEN; /* unused */

   return insn;
}


s390_insn *
s390_insn_bfp128_convert_from(UChar size, s390_bfp_unop_t tag, HReg dst,
                              HReg op_hi, HReg op_lo,
                              s390_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_BFP128_CONVERT_FROM;
   insn->size = size;
   insn->variant.bfp128_unop.tag = tag;
   insn->variant.bfp128_unop.dst_hi = dst;
   insn->variant.bfp128_unop.dst_lo = INVALID_HREG;  /* unused */
   insn->variant.bfp128_unop.op_hi = op_hi;
   insn->variant.bfp128_unop.op_lo = op_lo;
   insn->variant.bfp128_unop.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_mfence(void)
{
   s390_insn *insn = LibVEX_Alloc(sizeof(s390_insn));

   insn->tag  = S390_INSN_MFENCE;
   insn->size = 0;   /* not needed */

   return insn;
}


/*---------------------------------------------------------------*/
/*--- Debug print                                             ---*/
/*---------------------------------------------------------------*/

static const HChar *
s390_cc_as_string(s390_cc_t cc)
{
   switch (cc) {
   case S390_CC_NEVER:  return "never";
   case S390_CC_OVFL:   return "overflow";
   case S390_CC_H:      return "greater than";     /* A > B ; high */
   case S390_CC_NLE:    return "not low or equal";
   case S390_CC_L:      return "less than";        /* A < B ; low */
   case S390_CC_NHE:    return "not high or equal";
   case S390_CC_LH:     return "low or high";
   case S390_CC_NE:     return "not equal";        /* A != B ; not zero */
   case S390_CC_E:      return "equal";            /* A == B ; zero */
   case S390_CC_NLH:    return "not low or high";
   case S390_CC_HE:     return "greater or equal"; /* A >= B ; high or equal*/
   case S390_CC_NL:     return "not low";          /* not low */
   case S390_CC_LE:     return "less or equal";    /* A <= B ; low or equal */
   case S390_CC_NH:     return "not high";
   case S390_CC_NO:     return "not overflow";
   case S390_CC_ALWAYS: return "always";
   default:
      vpanic("s390_cc_as_string");
   }
}


/* Helper function for writing out a V insn */
static void
s390_sprintf(HChar *buf, HChar *fmt, ...)
{
   HChar *p;
   ULong value;
   va_list args;
   va_start(args, fmt);

   p = buf;
   for ( ; *fmt; ++fmt) {
      Int c = *fmt;

      if (c != '%') {
         *p++ = c;
         continue;
      }

      c = *++fmt;  /* next char */
      switch (c) {
      case '%':
         *p++ = c;   /* %% */
         continue;

      case 's':     /* %s */
         p += vex_sprintf(p, "%s", va_arg(args, HChar *));
         continue;

      case 'M':     /* %M = mnemonic */
         p += vex_sprintf(p, "%-8s", va_arg(args, HChar *));
         continue;

      case 'R':     /* %R = register */
         p += vex_sprintf(p, "%s", s390_hreg_as_string(va_arg(args, HReg)));
         continue;

      case 'A':     /* %A = amode */
         p += vex_sprintf(p, "%s",
                          s390_amode_as_string(va_arg(args, s390_amode *)));
         continue;

      case 'C':     /* %C = condition code */
         p += vex_sprintf(p, "%s", s390_cc_as_string(va_arg(args, s390_cc_t)));
         continue;

      case 'L': {   /* %L = argument list in helper call*/
         UInt i, num_args;

         num_args = va_arg(args, UInt);

         for (i = 0; i < num_args; ++i) {
            if (i != 0) p += vex_sprintf(p, ", ");
            p += vex_sprintf(p, "r%d", s390_gprno_from_arg_index(i));
         }
         continue;
      }

      case 'O': {   /* %O = RMI operand */
         s390_opnd_RMI *op = va_arg(args, s390_opnd_RMI *);

         switch (op->tag) {
         case S390_OPND_REG:
            p += vex_sprintf(p, "%s", s390_hreg_as_string(op->variant.reg));
            continue;

         case S390_OPND_AMODE:
            p += vex_sprintf(p, "%s", s390_amode_as_string(op->variant.am));
            continue;

         case S390_OPND_IMMEDIATE:
            value = op->variant.imm;
            goto print_value;

         default:
            goto fail;
         }
      }

      case 'I':     /* %I = immediate value */
         value = va_arg(args, ULong);
         goto print_value;

      print_value:
         if ((Long)value < 0)
            p += vex_sprintf(p, "%lld", (Long)value);
         else if (value < 100)
            p += vex_sprintf(p, "%llu", value);
         else
            p += vex_sprintf(p, "0x%llx", value);
         continue;

      default:
         goto fail;
      }
   }
   *p = '\0';
   va_end(args);

   return;

 fail: vpanic("s390_printf");
}


/* Decompile the given insn into a static buffer and return it */
const HChar *
s390_insn_as_string(const s390_insn *insn)
{
   static HChar buf[300];
   const HChar *op;
   HChar *p;

   buf[0] = '\0';

   switch (insn->tag) {
   case S390_INSN_LOAD:
      s390_sprintf(buf, "%M %R,%A", "v-load", insn->variant.load.dst,
                   insn->variant.load.src);
      break;

   case S390_INSN_STORE:
      s390_sprintf(buf, "%M %R,%A", "v-store", insn->variant.store.src,
                   insn->variant.store.dst);
      break;

   case S390_INSN_MOVE:
      s390_sprintf(buf, "%M %R,%R", "v-move", insn->variant.move.dst,
                   insn->variant.move.src);
      break;

   case S390_INSN_COND_MOVE:
      s390_sprintf(buf, "%M if (%C) %R,%O", "v-move",
                   insn->variant.cond_move.cond, insn->variant.cond_move.dst,
                   &insn->variant.cond_move.src);
      break;

   case S390_INSN_LOAD_IMMEDIATE:
      s390_sprintf(buf, "%M %R,%I", "v-loadi", insn->variant.load_immediate.dst,
                   insn->variant.load_immediate.value);
      break;

   case S390_INSN_ALU:
      switch (insn->variant.alu.tag) {
      case S390_ALU_ADD:  op = "v-add";  break;
      case S390_ALU_SUB:  op = "v-sub";  break;
      case S390_ALU_MUL:  op = "v-mul";  break;
      case S390_ALU_AND:  op = "v-and";  break;
      case S390_ALU_OR:   op = "v-or";   break;
      case S390_ALU_XOR:  op = "v-xor";  break;
      case S390_ALU_LSH:  op = "v-lsh";  break;
      case S390_ALU_RSH:  op = "v-rsh";  break;
      case S390_ALU_RSHA: op = "v-rsha"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%O", op, insn->variant.alu.dst,
                   insn->variant.alu.dst   /* op1 same as dst */,
                   &insn->variant.alu.op2);
      break;

   case S390_INSN_MUL:
      if (insn->variant.mul.signed_multiply) {
         op = "v-muls";
      } else {
         op = "v-mulu";
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.mul.dst_hi,
                   &insn->variant.mul.op2);
      break;

   case S390_INSN_DIV:
      if (insn->variant.div.signed_divide) {
         op = "v-divs";
      } else {
         op = "v-divu";
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.div.op1_hi,
                   &insn->variant.div.op2);
      break;

   case S390_INSN_DIVS:
      s390_sprintf(buf, "%M %R,%O", "v-divsi", insn->variant.divs.op1,
                   &insn->variant.divs.op2);
      break;

   case S390_INSN_CLZ:
      s390_sprintf(buf, "%M %R,%O", "v-clz", insn->variant.clz.num_bits,
                   &insn->variant.clz.src);
      break;

   case S390_INSN_UNOP:
      switch (insn->variant.unop.tag) {
      case S390_ZERO_EXTEND_8:
      case S390_ZERO_EXTEND_16:
      case S390_ZERO_EXTEND_32:
         op = "v-zerox";
         break;

      case S390_SIGN_EXTEND_8:
      case S390_SIGN_EXTEND_16:
      case S390_SIGN_EXTEND_32:
         op = "v-signx";
         break;

      case S390_NEGATE:
         op = "v-neg";
         break;

      default:
         goto fail;
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.unop.dst,
                   &insn->variant.unop.src);
      break;

   case S390_INSN_TEST:
      s390_sprintf(buf, "%M %O", "v-test", &insn->variant.test.src);
      break;

   case S390_INSN_CC2BOOL:
      s390_sprintf(buf, "%M %R,%C", "v-cc2b", insn->variant.cc2bool.dst,
                   insn->variant.cc2bool.cond);
      break;

   case S390_INSN_CAS:
      s390_sprintf(buf, "%M %R,%A,%R,%R", "v-cas", insn->variant.cas.op1,
                   insn->variant.cas.op2, insn->variant.cas.op3,
                   insn->variant.cas.old_mem);
      break;

   case S390_INSN_COMPARE:
      if (insn->variant.compare.signed_comparison) {
         op = "v-cmps";
      } else {
         op = "v-cmpu";
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.compare.src1,
                   &insn->variant.compare.src2);
      break;

   case S390_INSN_BRANCH:
      switch (insn->variant.branch.kind) {
      case Ijk_ClientReq:   op = "clientreq"; break;
      case Ijk_Sys_syscall: op = "syscall";   break;
      case Ijk_Yield:       op = "yield";     break;
      case Ijk_EmWarn:      op = "emwarn";    break;
      case Ijk_EmFail:      op = "emfail";    break;
      case Ijk_MapFail:     op = "mapfail";   break;
      case Ijk_NoDecode:    op = "nodecode";  break;
      case Ijk_TInval:      op = "tinval";    break;
      case Ijk_NoRedir:     op = "noredir";   break;
      case Ijk_SigTRAP:     op = "sigtrap";   break;
      case Ijk_Boring:      op = "goto";      break;
      case Ijk_Call:        op = "call";      break;
      case Ijk_Ret:         op = "return";    break;
      default:
         goto fail;
      }
      s390_sprintf(buf, "if (%C) %s %O", insn->variant.branch.cond, op,
                   &insn->variant.branch.dst);
      break;

   case S390_INSN_HELPER_CALL: {

      if (insn->variant.helper_call.cond != S390_CC_ALWAYS) {
         s390_sprintf(buf, "%M if (%C) %s{%I}(%L)", "v-call",
                      insn->variant.helper_call.cond,
                      insn->variant.helper_call.name,
                      insn->variant.helper_call.target,
                      insn->variant.helper_call.num_args);
      } else {
         s390_sprintf(buf, "%M %s{%I}(%L)", "v-call",
                      insn->variant.helper_call.name,
                      insn->variant.helper_call.target,
                      insn->variant.helper_call.num_args);
      }
      break;
   }

   case S390_INSN_BFP_TRIOP:
      switch (insn->variant.bfp_triop.tag) {
      case S390_BFP_MADD:  op = "v-fmadd";  break;
      case S390_BFP_MSUB:  op = "v-fmsub";  break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%R,%R", op, insn->variant.bfp_triop.dst,
                   insn->variant.bfp_triop.dst  /* op1 same as dst */,
                   insn->variant.bfp_triop.op2, insn->variant.bfp_triop.op3);
      break;

   case S390_INSN_BFP_BINOP:
      switch (insn->variant.bfp_binop.tag) {
      case S390_BFP_ADD:      op = "v-fadd";  break;
      case S390_BFP_SUB:      op = "v-fsub";  break;
      case S390_BFP_MUL:      op = "v-fmul";  break;
      case S390_BFP_DIV:      op = "v-fdiv";  break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%R", op, insn->variant.bfp_binop.dst,
                   insn->variant.bfp_binop.dst  /* op1 same as dst */,
                   insn->variant.bfp_binop.op2);
      break;

   case S390_INSN_BFP_COMPARE:
      s390_sprintf(buf, "%M %R,%R,%R", "v-fcmp", insn->variant.bfp_compare.dst,
                   insn->variant.bfp_compare.op1, insn->variant.bfp_compare.op2);
      break;

   case S390_INSN_BFP_UNOP:
      switch (insn->variant.bfp_unop.tag) {
      case S390_BFP_ABS:         op = "v-fabs";  break;
      case S390_BFP_NABS:        op = "v-fnabs"; break;
      case S390_BFP_NEG:         op = "v-fneg";  break;
      case S390_BFP_SQRT:        op = "v-fsqrt"; break;
      case S390_BFP_I32_TO_F32:
      case S390_BFP_I32_TO_F64:
      case S390_BFP_I32_TO_F128:
      case S390_BFP_I64_TO_F32:
      case S390_BFP_I64_TO_F64:
      case S390_BFP_I64_TO_F128: op = "v-i2f"; break;
      case S390_BFP_F32_TO_I32:
      case S390_BFP_F32_TO_I64:
      case S390_BFP_F64_TO_I32:
      case S390_BFP_F64_TO_I64:
      case S390_BFP_F128_TO_I32:
      case S390_BFP_F128_TO_I64: op = "v-f2i"; break;
      case S390_BFP_F32_TO_F64:
      case S390_BFP_F32_TO_F128:
      case S390_BFP_F64_TO_F32:
      case S390_BFP_F64_TO_F128:
      case S390_BFP_F128_TO_F32:
      case S390_BFP_F128_TO_F64: op = "v-f2f"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R", op, insn->variant.bfp_unop.dst,
                   insn->variant.bfp_unop.op);
      break;

   case S390_INSN_BFP128_BINOP:
      switch (insn->variant.bfp128_binop.tag) {
      case S390_BFP_ADD:      op = "v-fadd";  break;
      case S390_BFP_SUB:      op = "v-fsub";  break;
      case S390_BFP_MUL:      op = "v-fmul";  break;
      case S390_BFP_DIV:      op = "v-fdiv";  break;
      default: goto fail;
      }
      /* Only write the register that identifies the register pair */
      s390_sprintf(buf, "%M %R,%R,%R", op, insn->variant.bfp128_binop.dst_hi,
                   insn->variant.bfp128_binop.dst_hi  /* op1 same as dst */,
                   insn->variant.bfp128_binop.op2_hi);
      break;

   case S390_INSN_BFP128_COMPARE:
      /* Only write the register that identifies the register pair */
      s390_sprintf(buf, "%M %R,%R,%R", "v-fcmp", insn->variant.bfp128_compare.dst,
                   insn->variant.bfp128_compare.op1_hi,
                   insn->variant.bfp128_compare.op2_hi);
      break;

   case S390_INSN_BFP128_UNOP:
   case S390_INSN_BFP128_CONVERT_TO:
   case S390_INSN_BFP128_CONVERT_FROM:
      switch (insn->variant.bfp128_unop.tag) {
      case S390_BFP_ABS:         op = "v-fabs";  break;
      case S390_BFP_NABS:        op = "v-fnabs"; break;
      case S390_BFP_NEG:         op = "v-fneg";  break;
      case S390_BFP_SQRT:        op = "v-fsqrt"; break;
      case S390_BFP_I32_TO_F128:
      case S390_BFP_I64_TO_F128: op = "v-i2f";   break;
      case S390_BFP_F128_TO_I32:
      case S390_BFP_F128_TO_I64: op = "v-f2i";   break;
      case S390_BFP_F32_TO_F128:
      case S390_BFP_F64_TO_F128:
      case S390_BFP_F128_TO_F32:
      case S390_BFP_F128_TO_F64: op = "v-f2f";   break;
      default: goto fail;
      }
      /* Only write the register that identifies the register pair */
      s390_sprintf(buf, "%M %R,%R", op, insn->variant.bfp128_unop.dst_hi,
                   insn->variant.bfp128_unop.op_hi);
      break;

   case S390_INSN_MFENCE:
      s390_sprintf(buf, "%M", "v-mfence");
      return buf;   /* avoid printing "size = ..." which is meaningless */

   default: goto fail;
   }

   /* Write out how many bytes are involved in the operation */

   {
      UInt len, i;

      for (p = buf; *p; ++p)
         continue;

      len = p - buf;

      if (len < 32) {
         for (i = len; i < 32; ++i)
            p += vex_sprintf(p, " ");
      } else {
         p += vex_sprintf(p, "\t");
      }
   }

   /* Special cases first */
   switch (insn->tag) {
   case S390_INSN_UNOP:
      switch (insn->variant.unop.tag) {
      case S390_SIGN_EXTEND_8:
      case S390_ZERO_EXTEND_8:  p += vex_sprintf(p, "1 -> "); goto common;
      case S390_SIGN_EXTEND_16:
      case S390_ZERO_EXTEND_16: p += vex_sprintf(p, "2 -> "); goto common;
      case S390_SIGN_EXTEND_32:
      case S390_ZERO_EXTEND_32: p += vex_sprintf(p, "4 -> "); goto common;
      default:
         goto common;
      }

   case S390_INSN_BFP_UNOP:
      switch (insn->variant.bfp_unop.tag) {
      case S390_BFP_I32_TO_F32:
      case S390_BFP_I32_TO_F64:
      case S390_BFP_I32_TO_F128:
      case S390_BFP_F32_TO_I32:
      case S390_BFP_F32_TO_I64:
      case S390_BFP_F32_TO_F64:
      case S390_BFP_F32_TO_F128: p += vex_sprintf(p, "4 -> "); goto common;
      case S390_BFP_I64_TO_F32:
      case S390_BFP_I64_TO_F64:
      case S390_BFP_I64_TO_F128:
      case S390_BFP_F64_TO_I32:
      case S390_BFP_F64_TO_I64:
      case S390_BFP_F64_TO_F32:
      case S390_BFP_F64_TO_F128: p += vex_sprintf(p, "8 -> "); goto common;
      case S390_BFP_F128_TO_I32:
      case S390_BFP_F128_TO_I64:
      case S390_BFP_F128_TO_F32:
      case S390_BFP_F128_TO_F64: p += vex_sprintf(p, "16 -> "); goto common;
      default:
         goto common;
      }

   case S390_INSN_BFP128_UNOP:
   case S390_INSN_BFP128_CONVERT_TO:
   case S390_INSN_BFP128_CONVERT_FROM:
      switch (insn->variant.bfp128_unop.tag) {
      case S390_BFP_I32_TO_F128:
      case S390_BFP_F32_TO_F128: p += vex_sprintf(p, "4 -> "); goto common;
      case S390_BFP_I64_TO_F128:
      case S390_BFP_F64_TO_F128: p += vex_sprintf(p, "8 -> "); goto common;
      case S390_BFP_F128_TO_I32:
      case S390_BFP_F128_TO_I64:
      case S390_BFP_F128_TO_F32:
      case S390_BFP_F128_TO_F64: p += vex_sprintf(p, "16 -> "); goto common;
      default:
         goto common;
      }

   default:
      goto common;
   }

   /* Common case */
 common:
   vex_sprintf(p, "%u bytes", (UInt)insn->size);

   return buf;

 fail: vpanic("s390_insn_as_string");
}



/* Load NUM bytes from memory into register REG using addressing mode AM. */
static UChar *
s390_emit_load_mem(UChar *p, UInt num, UChar reg, const s390_amode *am)
{
   UInt b = hregNumber(am->b);
   UInt x = hregNumber(am->x);  /* 0 for B12 and B20 */
   UInt d = am->d;

   switch (am->tag) {
   case S390_AMODE_B12:
   case S390_AMODE_BX12:
      switch (num) {
      case 1: return s390_emit_IC(p, reg, x, b, d);
      case 2: return s390_emit_LH(p, reg, x, b, d);
      case 4: return s390_emit_L(p, reg, x, b, d);
      case 8: return s390_emit_LG(p, reg, x, b, DISP20(d));
      default: goto fail;
      }
      break;

   case S390_AMODE_B20:
   case S390_AMODE_BX20:
      switch (num) {
      case 1: return s390_emit_ICY(p, reg, x, b, DISP20(d));
      case 2: return s390_emit_LHY(p, reg, x, b, DISP20(d));
      case 4: return s390_emit_LY(p, reg, x, b, DISP20(d));
      case 8: return s390_emit_LG(p, reg, x, b, DISP20(d));
      default: goto fail;
      }
      break;

   default: goto fail;
   }

 fail:
   vpanic("s390_emit_load_mem");
}


/* Load condition code into register REG */
static UChar *
s390_emit_load_cc(UChar *p, UChar reg)
{
   p = s390_emit_LGHI(p, reg, 0);  /* Clear out, cc not affected */
   p = s390_emit_IPM(p, reg, reg);
   /* Shift 28 bits to the right --> [0,1,2,3] */
   return s390_emit_SRL(p, reg, 0, 28); /* REG = cc */
}


/*---------------------------------------------------------------*/
/*--- Code generation                                         ---*/
/*---------------------------------------------------------------*/

/* Do not load more bytes than requested. */
static UChar *
s390_insn_load_emit(UChar *buf, const s390_insn *insn)
{
   UInt r, x, b, d;
   const s390_amode *src;

   src = insn->variant.load.src;

   r = hregNumber(insn->variant.load.dst);

   if (hregClass(insn->variant.load.dst) == HRcFlt64) {
      b = hregNumber(src->b);
      x = hregNumber(src->x);  /* 0 for B12 and B20 */
      d = src->d;

      switch (insn->size) {

      case 4:
         switch (src->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            return s390_emit_LE(buf, r, x, b, d);

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            return s390_emit_LEY(buf, r, x, b, DISP20(d));
         }
         break;

      case 8:
         switch (src->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            return s390_emit_LD(buf, r, x, b, d);

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            return s390_emit_LDY(buf, r, x, b, DISP20(d));
         }
         break;
      }
      vpanic("s390_insn_load_emit");
   }

   /* Integer stuff */
   return s390_emit_load_mem(buf, insn->size, r, src);
}


static UChar *
s390_insn_store_emit(UChar *buf, const s390_insn *insn)
{
   UInt r, x, b, d;
   const s390_amode *dst;

   dst = insn->variant.store.dst;

   r = hregNumber(insn->variant.store.src);
   b = hregNumber(dst->b);
   x = hregNumber(dst->x);  /* 0 for B12 and B20 */
   d = dst->d;

   if (hregClass(insn->variant.store.src) == HRcFlt64) {
      switch (insn->size) {

      case 4:
         switch (dst->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            return s390_emit_STE(buf, r, x, b, d);

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            return s390_emit_STEY(buf, r, x, b, DISP20(d));
         }
         break;

      case 8:
         switch (dst->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            return s390_emit_STD(buf, r, x, b, d);

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            return s390_emit_STDY(buf, r, x, b, DISP20(d));
         }
         break;
      }
      vpanic("s390_insn_store_emit");
   }

   /* Integer stuff */
   switch (insn->size) {
   case 1:
      switch (dst->tag) {
      case S390_AMODE_B12:
      case S390_AMODE_BX12:
         return s390_emit_STC(buf, r, x, b, d);

      case S390_AMODE_B20:
      case S390_AMODE_BX20:
         return s390_emit_STCY(buf, r, x, b, DISP20(d));
      }
      break;

   case 2:
      switch (dst->tag) {
      case S390_AMODE_B12:
      case S390_AMODE_BX12:
         return s390_emit_STH(buf, r, x, b, d);

      case S390_AMODE_B20:
      case S390_AMODE_BX20:
         return s390_emit_STHY(buf, r, x, b, DISP20(d));
      }
      break;

   case 4:
      switch (dst->tag) {
      case S390_AMODE_B12:
      case S390_AMODE_BX12:
         return s390_emit_ST(buf, r, x, b, d);

      case S390_AMODE_B20:
      case S390_AMODE_BX20:
         return s390_emit_STY(buf, r, x, b, DISP20(d));
      }
      break;

   case 8:
      return s390_emit_STG(buf, r, x, b, DISP20(d));

   default:
      break;
   }

   vpanic("s390_insn_store_emit");
}


static UChar *
s390_insn_move_emit(UChar *buf, const s390_insn *insn)
{
   UInt dst, src;
   HRegClass dst_class, src_class;

   dst = hregNumber(insn->variant.move.dst);
   src = hregNumber(insn->variant.move.src);

   dst_class = hregClass(insn->variant.move.dst);
   src_class = hregClass(insn->variant.move.src);

   if (dst_class == src_class) {
      if (dst_class == HRcInt64)
         return s390_emit_LGR(buf, dst, src);
      if (dst_class == HRcFlt64)
         return s390_emit_LDR(buf, dst, src);
   } else {
      if (dst_class == HRcFlt64 && src_class == HRcInt64)
         return s390_emit_LDGRw(buf, dst, src);
      if (dst_class == HRcInt64 && src_class == HRcFlt64)
         return s390_emit_LGDRw(buf, dst, src);
      /* A move between floating point registers and general purpose
         registers of different size should never occur and indicates
         an error elsewhere. */
   }

   vpanic("s390_insn_move_emit");
}


static UChar *
s390_insn_load_immediate_emit(UChar *buf, const s390_insn *insn)
{
   UInt  r;
   ULong value = insn->variant.load_immediate.value;

   r = hregNumber(insn->variant.load_immediate.dst);

   if (hregClass(insn->variant.load_immediate.dst) == HRcFlt64) {
      vassert(value == 0);
      switch (insn->size) {
      case 4: return s390_emit_LZER(buf, r, value);
      case 8: return s390_emit_LZDR(buf, r, value);
      }
      vpanic("s390_insn_load_immediate_emit");
   }

   switch (insn->size) {
   case 1:
   case 2:
      /* Load the immediate values as a 4 byte value. That does not hurt as
         those extra bytes will not be looked at. Fall through .... */
   case 4:
      return s390_emit_load_32imm(buf, r, value);

   case 8:
      return s390_emit_load_64imm(buf, r, value);
   }

   vpanic("s390_insn_load_immediate_emit");
}


/* There is no easy way to do ALU operations on 1-byte or 2-byte operands.
   So we simply perform a 4-byte operation. Doing so uses possibly undefined
   bits and produces an undefined result in those extra bit positions. But
   upstream does not look at those positions, so this is OK. */
static UChar *
s390_insn_alu_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI op2;
   UInt dst;

   dst = hregNumber(insn->variant.alu.dst);
   op2 = insn->variant.alu.op2;

   /* Second operand is in a register */
   if (op2.tag == S390_OPND_REG) {
      UInt r2 = hregNumber(op2.variant.reg);

      switch (insn->size) {
      case 1:
      case 2:
      case 4:
         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD:  return s390_emit_AR(buf, dst, r2);
         case S390_ALU_SUB:  return s390_emit_SR(buf, dst, r2);
         case S390_ALU_MUL:  return s390_emit_MSR(buf, dst, r2);
         case S390_ALU_AND:  return s390_emit_NR(buf, dst, r2);
         case S390_ALU_OR:   return s390_emit_OR(buf, dst, r2);
         case S390_ALU_XOR:  return s390_emit_XR(buf, dst, r2);
         case S390_ALU_LSH:  return s390_emit_SLL(buf, dst, r2, 0);
         case S390_ALU_RSH:  return s390_emit_SRL(buf, dst, r2, 0);
         case S390_ALU_RSHA: return s390_emit_SRA(buf, dst, r2, 0);
         }
         goto fail;

      case 8:
         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD:  return s390_emit_AGR(buf, dst, r2);
         case S390_ALU_SUB:  return s390_emit_SGR(buf, dst, r2);
         case S390_ALU_MUL:  return s390_emit_MSGR(buf, dst, r2);
         case S390_ALU_AND:  return s390_emit_NGR(buf, dst, r2);
         case S390_ALU_OR:   return s390_emit_OGR(buf, dst, r2);
         case S390_ALU_XOR:  return s390_emit_XGR(buf, dst, r2);
         case S390_ALU_LSH:  return s390_emit_SLLG(buf, dst, dst, r2, DISP20(0));
         case S390_ALU_RSH:  return s390_emit_SRLG(buf, dst, dst, r2, DISP20(0));
         case S390_ALU_RSHA: return s390_emit_SRAG(buf, dst, dst, r2, DISP20(0));
         }
         goto fail;
      }
      goto fail;
   }

   /* 2nd operand is in memory */
   if (op2.tag == S390_OPND_AMODE) {
      UInt b, x, d;
      const s390_amode *src = op2.variant.am;

      b = hregNumber(src->b);
      x = hregNumber(src->x);  /* 0 for B12 and B20 */
      d = src->d;

      /* Shift operands are special here as there are no opcodes that
         allow a memory operand. So we first load the 2nd operand into
         some register. R0 is used to save restore the contents of the
         chosen register.. */

      if (insn->variant.alu.tag == S390_ALU_LSH ||
          insn->variant.alu.tag == S390_ALU_RSH ||
          insn->variant.alu.tag == S390_ALU_RSHA) {
         UInt b2;

         /* Choose a register (other than DST or R0) into which to stick the
            shift amount. The following works because r15 is reserved and
            thusly dst != 15. */
         vassert(dst != 15);  /* extra paranoia */
         b2 = (dst + 1) % 16;
         
         buf = s390_emit_LGR(buf, R0, b2);  /* save */

         /* Loading SRC to B2 does not modify R0. */
         buf = s390_emit_load_mem(buf, insn->size, b2, src);

         if (insn->size == 8) {
            switch (insn->variant.alu.tag) {
            case S390_ALU_LSH:
               buf = s390_emit_SLLG(buf, dst, dst, b2, DISP20(0));
               break;
            case S390_ALU_RSH:
               buf = s390_emit_SRLG(buf, dst, dst, b2, DISP20(0));
               break;
            case S390_ALU_RSHA:
               buf = s390_emit_SRAG(buf, dst, dst, b2, DISP20(0));
               break;
            default: /* unreachable */
               goto fail;
            }
         } else {
            switch (insn->variant.alu.tag) {
            case S390_ALU_LSH:
               buf = s390_emit_SLL(buf, dst, b2, 0);
               break;
            case S390_ALU_RSH:
               buf = s390_emit_SRL(buf, dst, b2, 0);
               break;
            case S390_ALU_RSHA:
               buf = s390_emit_SRA(buf, dst, b2, 0);
               break;
            default: /* unreachable */
               goto fail;
            }
         }
         return s390_emit_LGR(buf, b2, R0);  /* restore */
      }

      switch (insn->size) {
      case 1:
         /* Move the byte from memory into scratch register r0 */
         buf = s390_emit_load_mem(buf, 1, R0, src);

         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD: return s390_emit_AR(buf, dst, R0);
         case S390_ALU_SUB: return s390_emit_SR(buf, dst, R0);
         case S390_ALU_MUL: return s390_emit_MSR(buf, dst, R0);
         case S390_ALU_AND: return s390_emit_NR(buf, dst, R0);
         case S390_ALU_OR:  return s390_emit_OR(buf, dst, R0);
         case S390_ALU_XOR: return s390_emit_XR(buf, dst, R0);
         case S390_ALU_LSH:
         case S390_ALU_RSH:
         case S390_ALU_RSHA: ; /* avoid GCC warning */
         }
         goto fail;

      case 2:
         switch (src->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            switch (insn->variant.alu.tag) {
            case S390_ALU_ADD:
               return s390_emit_AH(buf, dst, x, b, d);

            case S390_ALU_SUB:
               return s390_emit_SH(buf, dst, x, b, d);

            case S390_ALU_MUL:
               return s390_emit_MH(buf, dst, x, b, d);

               /* For bitwise operations: Move two bytes from memory into scratch
                  register r0; then perform operation */
            case S390_ALU_AND:
               buf = s390_emit_LH(buf, R0, x, b, d);
               return s390_emit_NR(buf, dst, R0);

            case S390_ALU_OR:
               buf = s390_emit_LH(buf, R0, x, b, d);
               return s390_emit_OR(buf, dst, R0);

            case S390_ALU_XOR:
               buf = s390_emit_LH(buf, R0, x, b, d);
               return s390_emit_XR(buf, dst, R0);

            case S390_ALU_LSH:
            case S390_ALU_RSH:
            case S390_ALU_RSHA: ; /* avoid GCC warning */
            }
            goto fail;

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            switch (insn->variant.alu.tag) {
            case S390_ALU_ADD:
               return s390_emit_AHY(buf, dst, x, b, DISP20(d));

            case S390_ALU_SUB:
               return s390_emit_SHY(buf, dst, x, b, DISP20(d));

            case S390_ALU_MUL:
               return s390_emit_MHYw(buf, dst, x, b, DISP20(d));

               /* For bitwise operations: Move two bytes from memory into scratch
                  register r0; then perform operation */
            case S390_ALU_AND:
               buf = s390_emit_LHY(buf, R0, x, b, DISP20(d));
               return s390_emit_NR(buf, dst, R0);

            case S390_ALU_OR:
               buf = s390_emit_LHY(buf, R0, x, b, DISP20(d));
               return s390_emit_OR(buf, dst, R0);

            case S390_ALU_XOR:
               buf = s390_emit_LHY(buf, R0, x, b, DISP20(d));
               return s390_emit_XR(buf, dst, R0);

            case S390_ALU_LSH:
            case S390_ALU_RSH:
            case S390_ALU_RSHA: ; /* avoid GCC warning */
            }
            goto fail;
         }
         goto fail;

      case 4:
         switch (src->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            switch (insn->variant.alu.tag) {
            case S390_ALU_ADD: return s390_emit_A(buf, dst, x, b, d);
            case S390_ALU_SUB: return s390_emit_S(buf, dst, x, b, d);
            case S390_ALU_MUL: return s390_emit_MS(buf, dst, x, b, d);
            case S390_ALU_AND: return s390_emit_N(buf, dst, x, b, d);
            case S390_ALU_OR:  return s390_emit_O(buf, dst, x, b, d);
            case S390_ALU_XOR: return s390_emit_X(buf, dst, x, b, d);
            case S390_ALU_LSH:
            case S390_ALU_RSH:
            case S390_ALU_RSHA: ; /* avoid GCC warning */
            }
            goto fail;

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            switch (insn->variant.alu.tag) {
            case S390_ALU_ADD: return s390_emit_AY(buf, dst, x, b, DISP20(d));
            case S390_ALU_SUB: return s390_emit_SY(buf, dst, x, b, DISP20(d));
            case S390_ALU_MUL: return s390_emit_MSY(buf, dst, x, b, DISP20(d));
            case S390_ALU_AND: return s390_emit_NY(buf, dst, x, b, DISP20(d));
            case S390_ALU_OR:  return s390_emit_OY(buf, dst, x, b, DISP20(d));
            case S390_ALU_XOR: return s390_emit_XY(buf, dst, x, b, DISP20(d));
            case S390_ALU_LSH:
            case S390_ALU_RSH:
            case S390_ALU_RSHA: ; /* avoid GCC warning */
            }
            goto fail;
         }
         goto fail;

      case 8:
         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD: return s390_emit_AG(buf, dst, x, b, DISP20(d));
         case S390_ALU_SUB: return s390_emit_SG(buf, dst, x, b, DISP20(d));
         case S390_ALU_MUL: return s390_emit_MSG(buf, dst, x, b, DISP20(d));
         case S390_ALU_AND: return s390_emit_NG(buf, dst, x, b, DISP20(d));
         case S390_ALU_OR:  return s390_emit_OG(buf, dst, x, b, DISP20(d));
         case S390_ALU_XOR: return s390_emit_XG(buf, dst, x, b, DISP20(d));
         case S390_ALU_LSH:
         case S390_ALU_RSH:
         case S390_ALU_RSHA: ; /* avoid GCC warning */
         }
         goto fail;
      }
      goto fail;
   }

   /* 2nd operand is an immediate value */
   if (op2.tag == S390_OPND_IMMEDIATE) {
      ULong value;

      /* No masking of the value is required as it is not sign extended */
      value = op2.variant.imm;

      switch (insn->size) {
      case 1:
      case 2:
         /* There is no 1-byte opcode. Do the computation in
            2 bytes. The extra byte will be ignored. */
         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD:
            return s390_emit_AHI(buf, dst, value);

         case S390_ALU_SUB:
            return s390_emit_SLFIw(buf, dst, value);

         case S390_ALU_MUL:
            return s390_emit_MHI(buf, dst, value);

         case S390_ALU_AND: return s390_emit_NILL(buf, dst, value);
         case S390_ALU_OR:  return s390_emit_OILL(buf, dst, value);
         case S390_ALU_XOR:
            /* There is no XILL instruction.  Load the immediate value into
               R0 and combine with the destination register. */
            buf = s390_emit_LHI(buf, R0, value);
            return s390_emit_XR(buf, dst, R0);

         case S390_ALU_LSH:
            return s390_emit_SLL(buf, dst, 0, value);

         case S390_ALU_RSH:
            return s390_emit_SRL(buf, dst, 0, value);

         case S390_ALU_RSHA:
            return s390_emit_SRA(buf, dst, 0, value);
         }
         goto fail;

      case 4:
         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD:
            if (uint_fits_signed_16bit(value)) {
               return s390_emit_AHI(buf, dst, value);
            }
            return s390_emit_AFIw(buf, dst, value);

         case S390_ALU_SUB:  return s390_emit_SLFIw(buf, dst, value);
         case S390_ALU_MUL:  return s390_emit_MSFIw(buf, dst, value);
         case S390_ALU_AND:  return s390_emit_NILFw(buf, dst, value);
         case S390_ALU_OR:   return s390_emit_OILFw(buf, dst, value);
         case S390_ALU_XOR:  return s390_emit_XILFw(buf, dst, value);
         case S390_ALU_LSH:  return s390_emit_SLL(buf, dst, 0, value);
         case S390_ALU_RSH:  return s390_emit_SRL(buf, dst, 0, value);
         case S390_ALU_RSHA: return s390_emit_SRA(buf, dst, 0, value);
         }
         goto fail;

      case 8:
         switch (insn->variant.alu.tag) {
         case S390_ALU_ADD:
            if (ulong_fits_signed_16bit(value)) {
               return s390_emit_AGHI(buf, dst, value);
            }
            if (ulong_fits_signed_32bit(value) && s390_host_has_eimm) {
               return s390_emit_AGFI(buf, dst, value);
            }
            /* Load constant into R0 then add */
            buf = s390_emit_load_64imm(buf, R0, value);
            return s390_emit_AGR(buf, dst, R0);

         case S390_ALU_SUB:
            if (ulong_fits_unsigned_32bit(value)) {
               return s390_emit_SLGFIw(buf, dst, value);
            }
            /* Load value into R0; then subtract from destination reg */
            buf = s390_emit_load_64imm(buf, R0, value);
            return s390_emit_SGR(buf, dst, R0);

         case S390_ALU_MUL:
            if (ulong_fits_signed_32bit(value) && s390_host_has_gie) {
               return s390_emit_MSGFI(buf, dst, value);
            }
            /* Load constant into R0 then add */
            buf = s390_emit_load_64imm(buf, R0, value);
            return s390_emit_MSGR(buf, dst, R0);

            /* Do it in two steps: upper half [0:31] and lower half [32:63] */
         case S390_ALU_AND:
            if (s390_host_has_eimm) {
               buf  = s390_emit_NIHF(buf, dst, value >> 32);
               return s390_emit_NILF(buf, dst, value & 0xFFFFFFFF);
            }
            /* Load value into R0; then combine with destination reg */
            buf = s390_emit_load_64imm(buf, R0, value);
            return s390_emit_NGR(buf, dst, R0);

         case S390_ALU_OR:
            if (s390_host_has_eimm) {
               buf  = s390_emit_OIHF(buf, dst, value >> 32);
               return s390_emit_OILF(buf, dst, value & 0xFFFFFFFF);
            }
            /* Load value into R0; then combine with destination reg */
            buf = s390_emit_load_64imm(buf, R0, value);
            return s390_emit_OGR(buf, dst, R0);

         case S390_ALU_XOR:
            if (s390_host_has_eimm) {
               buf  = s390_emit_XIHF(buf, dst, value >> 32);
               return s390_emit_XILF(buf, dst, value & 0xFFFFFFFF);
            }
            /* Load value into R0; then combine with destination reg */
            buf = s390_emit_load_64imm(buf, R0, value);
            return s390_emit_XGR(buf, dst, R0);

            /* No special considerations for long displacement here. Only the six
               least significant bits of VALUE will be taken; all other bits are
               ignored. So the DH2 bits are irrelevant and do not influence the
               shift operation, independent of whether long-displacement is available
               or not. */
         case S390_ALU_LSH:  return s390_emit_SLLG(buf, dst, dst, 0, DISP20(value));
         case S390_ALU_RSH:  return s390_emit_SRLG(buf, dst, dst, 0, DISP20(value));
         case S390_ALU_RSHA: return s390_emit_SRAG(buf, dst, dst, 0, DISP20(value));
         }
         goto fail;
      }
      goto fail;
   }

 fail:
   vpanic("s390_insn_alu_emit");
}


static UChar *
s390_widen_emit(UChar *buf, const s390_insn *insn, UInt from_size,
                Bool sign_extend)
{
   s390_opnd_RMI opnd = insn->variant.unop.src;

   switch (opnd.tag) {
   case S390_OPND_REG: {
      UChar r1 = hregNumber(insn->variant.unop.dst);
      UChar r2 = hregNumber(opnd.variant.reg);

      switch (from_size) {
      case 1:
         /* Widening to a half-word is implemented like widening to a word
            because the upper half-word will not be looked at. */
         if (insn->size == 4 || insn->size == 2) {  /* 8 --> 32    8 --> 16 */
            if (sign_extend)
               return s390_emit_LBRw(buf, r1, r2);
            else
               return s390_emit_LLCRw(buf, r1, r2);
         }
         if (insn->size == 8) {  /* 8 --> 64 */
            if (sign_extend)
               return s390_emit_LGBRw(buf, r1, r2);
            else
               return s390_emit_LLGCRw(buf, r1, r2);
         }
         goto fail;

      case 2:
         if (insn->size == 4) {  /* 16 --> 32 */
            if (sign_extend)
               return s390_emit_LHRw(buf, r1, r2);
            else
               return s390_emit_LLHRw(buf, r1, r2);
         }
         if (insn->size == 8) {  /* 16 --> 64 */
            if (sign_extend)
               return s390_emit_LGHRw(buf, r1, r2);
            else
               return s390_emit_LLGHRw(buf, r1, r2);
         }
         goto fail;

      case 4:
         if (insn->size == 8) {  /* 32 --> 64 */
            if (sign_extend)
               return s390_emit_LGFR(buf, r1, r2);
            else
               return s390_emit_LLGFR(buf, r1, r2);
         }
         goto fail;

      default: /* unexpected "from" size */
         goto fail;
      }
   }

   case S390_OPND_AMODE: {
      UChar r1 = hregNumber(insn->variant.unop.dst);
      const s390_amode *src = opnd.variant.am;
      UChar b = hregNumber(src->b);
      UChar x = hregNumber(src->x);
      Int   d = src->d;

      switch (from_size) {
      case 1:
         if (insn->size == 4 || insn->size == 2) {
            if (sign_extend)
               return s390_emit_LBw(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_LLCw(buf, r1, x, b, DISP20(d));
         }
         if (insn->size == 8) {
            if (sign_extend)
               return s390_emit_LGBw(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_LLGC(buf, r1, x, b, DISP20(d));
         }
         goto fail;

      case 2:
         if (insn->size == 4) {  /* 16 --> 32 */
            if (sign_extend == 0)
               return s390_emit_LLHw(buf, r1, x, b, DISP20(d));

            switch (src->tag) {
            case S390_AMODE_B12:
            case S390_AMODE_BX12:
               return s390_emit_LH(buf, r1, x, b, d);

            case S390_AMODE_B20:
            case S390_AMODE_BX20:
               return s390_emit_LHY(buf, r1, x, b, DISP20(d));
            }
            goto fail;
         }
         if (insn->size == 8) {  /* 16 --> 64 */
            if (sign_extend)
               return s390_emit_LGH(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_LLGH(buf, r1, x, b, DISP20(d));
         }
         goto fail;

      case 4:
         if (insn->size == 8) {  /* 32 --> 64 */
            if (sign_extend)
               return s390_emit_LGF(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_LLGF(buf, r1, x, b, DISP20(d));
         }
         goto fail;

      default: /* unexpected "from" size */
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      UChar r1 = hregNumber(insn->variant.unop.dst);
      ULong value = opnd.variant.imm;

      switch (from_size) {
      case 1:
         if (insn->size == 4 || insn->size == 2) {  /* 8 --> 32   8 --> 16 */
            if (sign_extend) {
               /* host can do the sign extension to 16-bit; LHI does the rest */
               return s390_emit_LHI(buf, r1, (Short)(Char)(UChar)value);
            } else {
               return s390_emit_LHI(buf, r1, value);
            }
         }
         if (insn->size == 8) {  /* 8 --> 64 */
            if (sign_extend) {
               /* host can do the sign extension to 16-bit; LGHI does the rest */
               return s390_emit_LGHI(buf, r1, (Short)(Char)(UChar)value);
            } else {
               return s390_emit_LGHI(buf, r1, value);
            }
         }
         goto fail;

      case 2:
         if (insn->size == 4) {  /* 16 --> 32 */
            return s390_emit_LHI(buf, r1, value);
         }
         if (insn->size == 8) {  /* 16 --> 64 */
            if (sign_extend)
               return s390_emit_LGHI(buf, r1, value);
            else
               return s390_emit_LLILL(buf, r1, value);
         }
         goto fail;

      case 4:
         if (insn->size == 8) {  /* 32 --> 64 */
            if (sign_extend)
               return s390_emit_LGFIw(buf, r1, value);
            else
               return s390_emit_LLILFw(buf, r1, value);
         }
         goto fail;

      default: /* unexpected "from" size */
         goto fail;
      }
   }
   }

 fail:
   vpanic("s390_widen_emit");
}


static UChar *
s390_negate_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI opnd;

   opnd = insn->variant.unop.src;

   switch (opnd.tag) {
   case S390_OPND_REG: {
      UChar r1 = hregNumber(insn->variant.unop.dst);
      UChar r2 = hregNumber(opnd.variant.reg);

      switch (insn->size) {
      case 1:
      case 2:
      case 4:
         return s390_emit_LCR(buf, r1, r2);

      case 8:
         return s390_emit_LCGR(buf, r1, r2);

      default:
         goto fail;
      }
   }

   case S390_OPND_AMODE: {
      UChar r1 = hregNumber(insn->variant.unop.dst);

      /* Load bytes into scratch register R0, then negate */
      buf = s390_emit_load_mem(buf, insn->size, R0, opnd.variant.am);

      switch (insn->size) {
      case 1:
      case 2:
      case 4:
         return s390_emit_LCR(buf, r1, R0);

      case 8:
         return s390_emit_LCGR(buf, r1, R0);

      default:
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      UChar r1 = hregNumber(insn->variant.unop.dst);
      ULong value = opnd.variant.imm;

      value = ~value + 1;   /* two's complement */

      switch (insn->size) {
      case 1:
      case 2:
         /* Load the immediate values as a 4 byte value. That does not hurt as
            those extra bytes will not be looked at. Fall through .... */
      case 4:
         return s390_emit_load_32imm(buf, r1, value);

      case 8:
         return s390_emit_load_64imm(buf, r1, value);

      default:
         goto fail;
      }
   }
   }

 fail:
   vpanic("s390_negate_emit");
}


static UChar *
s390_insn_unop_emit(UChar *buf, const s390_insn *insn)
{
   switch (insn->variant.unop.tag) {
   case S390_ZERO_EXTEND_8:  return s390_widen_emit(buf, insn, 1, 0);
   case S390_ZERO_EXTEND_16: return s390_widen_emit(buf, insn, 2, 0);
   case S390_ZERO_EXTEND_32: return s390_widen_emit(buf, insn, 4, 0);

   case S390_SIGN_EXTEND_8:  return s390_widen_emit(buf, insn, 1, 1);
   case S390_SIGN_EXTEND_16: return s390_widen_emit(buf, insn, 2, 1);
   case S390_SIGN_EXTEND_32: return s390_widen_emit(buf, insn, 4, 1);

   case S390_NEGATE:         return s390_negate_emit(buf, insn);
   }

   vpanic("s390_insn_unop_emit");
}


/* Only 4-byte and 8-byte operands are handled. 1-byte and 2-byte
   comparisons will have been converted to 4-byte comparisons in
   s390_isel_cc and should not occur here. */
static UChar *
s390_insn_test_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI opnd;

   opnd = insn->variant.test.src;

   switch (opnd.tag) {
   case S390_OPND_REG: {
      UInt reg = hregNumber(opnd.variant.reg);

      switch (insn->size) {
      case 4:
         return s390_emit_LTR(buf, reg, reg);

      case 8:
         return s390_emit_LTGR(buf, reg, reg);

      default:
         goto fail;
      }
   }

   case S390_OPND_AMODE: {
      const s390_amode *am = opnd.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      switch (insn->size) {
      case 4:
         return s390_emit_LTw(buf, R0, x, b, DISP20(d));

      case 8:
         return s390_emit_LTGw(buf, R0, x, b, DISP20(d));

      default:
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      ULong value = opnd.variant.imm;

      switch (insn->size) {
      case 4:
         buf = s390_emit_load_32imm(buf, R0, value);
         return s390_emit_LTR(buf, R0, R0);

      case 8:
         buf = s390_emit_load_64imm(buf, R0, value);
         return s390_emit_LTGR(buf, R0, R0);

      default:
         goto fail;
      }
   }

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_test_emit");
}


static UChar *
s390_insn_cc2bool_emit(UChar *buf, const s390_insn *insn)
{
   UChar r1 = hregNumber(insn->variant.cc2bool.dst);
   s390_cc_t cond = insn->variant.cc2bool.cond;

   /* Make the destination register be 1 or 0, depending on whether
      the relevant condition holds. A 64-bit value is computed. */
   if (cond == S390_CC_ALWAYS)
      return s390_emit_LGHI(buf, r1, 1);  /* r1 = 1 */

   buf = s390_emit_load_cc(buf, r1);                 /* r1 = cc */
   buf = s390_emit_LGHI(buf, R0, cond);              /* r0 = mask */
   buf = s390_emit_SLLG(buf, r1, R0, r1, DISP20(0)); /* r1 = mask << cc */
   buf = s390_emit_SRLG(buf, r1, r1, 0,  DISP20(3)); /* r1 = r1 >> 3 */
   buf = s390_emit_NILL(buf, r1, 1);                 /* r1 = r1 & 0x1 */

   return buf;
}


/* Only 4-byte and 8-byte operands are handled. */
static UChar *
s390_insn_cas_emit(UChar *buf, const s390_insn *insn)
{
   UChar r1, r3, b, old;
   Int d;
   s390_amode *am;

   r1 = hregNumber(insn->variant.cas.op1); /* expected value */
   r3 = hregNumber(insn->variant.cas.op3);
   old= hregNumber(insn->variant.cas.old_mem);
   am = insn->variant.cas.op2;
   b  = hregNumber(am->b);
   d  = am->d;

   switch (insn->size) {
   case 4:
      /* r1 must no be overwritten. So copy it to R0 and let CS clobber it */
      buf = s390_emit_LR(buf, R0, r1);
      if (am->tag == S390_AMODE_B12)
         buf = s390_emit_CS(buf, R0, r3, b, d);
      else
         buf = s390_emit_CSY(buf, R0, r3, b, DISP20(d));
      /* Now copy R0 which has the old memory value to OLD */
      return s390_emit_LR(buf, old, R0);

   case 8:
      /* r1 must no be overwritten. So copy it to R0 and let CS clobber it */
      buf = s390_emit_LGR(buf, R0, r1);
      buf = s390_emit_CSG(buf, R0, r3, b, DISP20(d));
      /* Now copy R0 which has the old memory value to OLD */
      return s390_emit_LGR(buf, old, R0);

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_cas_emit");
}


/* Only 4-byte and 8-byte comparisons are handled. 1-byte and 2-byte
   comparisons will have been converted to 4-byte comparisons in
   s390_isel_cc and should not occur here. */
static UChar *
s390_insn_compare_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI op2;
   HReg op1;
   Bool signed_comparison;

   op1 = insn->variant.compare.src1;
   op2 = insn->variant.compare.src2;
   signed_comparison = insn->variant.compare.signed_comparison;

   switch (op2.tag) {
   case S390_OPND_REG: {
      UInt r1 = hregNumber(op1);
      UInt r2 = hregNumber(op2.variant.reg);

      switch (insn->size) {
      case 4:
         if (signed_comparison)
            return s390_emit_CR(buf, r1, r2);
         else
            return s390_emit_CLR(buf, r1, r2);

      case 8:
         if (signed_comparison)
            return s390_emit_CGR(buf, r1, r2);
         else
            return s390_emit_CLGR(buf, r1, r2);

      default:
         goto fail;
      }
   }

   case S390_OPND_AMODE: {
      UChar r1 = hregNumber(op1);
      const s390_amode *am = op2.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      switch (insn->size) {
      case 4:
         switch (am->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            if (signed_comparison)
               return s390_emit_C(buf, r1, x, b, d);
            else
               return s390_emit_CL(buf, r1, x, b, d);

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            if (signed_comparison)
               return s390_emit_CY(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_CLY(buf, r1, x, b, DISP20(d));
         }
         goto fail;

      case 8:
         if (signed_comparison)
            return s390_emit_CG(buf, r1, x, b, DISP20(d));
         else
            return s390_emit_CLG(buf, r1, x, b, DISP20(d));

      default:
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      UChar r1 = hregNumber(op1);
      ULong value = op2.variant.imm;

      switch (insn->size) {
      case 4:
         if (signed_comparison)
            return s390_emit_CFIw(buf, r1, value);
         else
            return s390_emit_CLFIw(buf, r1, value);

      case 8:
         buf = s390_emit_load_64imm(buf, R0, value);
         if (signed_comparison)
            return s390_emit_CGR(buf, r1, R0);
         else
            return s390_emit_CLGR(buf, r1, R0);

      default:
         goto fail;
      }
   }

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_compare_emit");
}


static UChar *
s390_insn_mul_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI op2;
   UChar r1;
   Bool signed_multiply;

   /* The register number identifying the register pair */
   r1  = hregNumber(insn->variant.mul.dst_hi);

   op2 = insn->variant.mul.op2;
   signed_multiply = insn->variant.mul.signed_multiply;

   switch (op2.tag) {
   case S390_OPND_REG: {
      UInt r2 = hregNumber(op2.variant.reg);

      switch (insn->size) {
      case 1:
      case 2:
      case 4:
         if (signed_multiply)
            return s390_emit_MR(buf, r1, r2);
         else
            return s390_emit_MLR(buf, r1, r2);

      case 8:
         if (signed_multiply)
            vpanic("s390_insn_mul_emit");
         else
            return s390_emit_MLGR(buf, r1, r2);

      default:
         goto fail;
      }
   }

   case S390_OPND_AMODE: {
      const s390_amode *am = op2.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      switch (insn->size) {
      case 1:
      case 2:
         /* Load bytes into scratch register R0, then multiply */
         buf = s390_emit_load_mem(buf, insn->size, R0, am);
         if (signed_multiply)
            return s390_emit_MR(buf, r1, R0);
         else
            return s390_emit_MLR(buf, r1, R0);

      case 4:
         switch (am->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            if (signed_multiply)
               return s390_emit_M(buf, r1, x, b, d);
            else
               return s390_emit_ML(buf, r1, x, b, DISP20(d));

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            if (signed_multiply)
               return s390_emit_MFYw(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_ML(buf, r1, x, b, DISP20(d));
         }
         goto fail;

      case 8:
         if (signed_multiply)
            vpanic("s390_insn_mul_emit");
         else
            return s390_emit_MLG(buf, r1, x, b, DISP20(d));

      default:
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      ULong value = op2.variant.imm;

      switch (insn->size) {
      case 1:
      case 2:
      case 4:
         buf = s390_emit_load_32imm(buf, R0, value);
         if (signed_multiply)
            return s390_emit_MR(buf, r1, R0);
         else
            return s390_emit_MLR(buf, r1, R0);

      case 8:
         buf = s390_emit_load_64imm(buf, R0, value);
         if (signed_multiply)
            vpanic("s390_insn_mul_emit");
         else
            return s390_emit_MLGR(buf, r1, R0);

      default:
         goto fail;
      }
   }

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_mul_emit");
}


static UChar *
s390_insn_div_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI op2;
   UChar r1;
   Bool signed_divide;

   r1  = hregNumber(insn->variant.div.op1_hi);
   op2 = insn->variant.div.op2;
   signed_divide = insn->variant.div.signed_divide;

   switch (op2.tag) {
   case S390_OPND_REG: {
      UInt r2 = hregNumber(op2.variant.reg);

      switch (insn->size) {
      case 4:
         if (signed_divide)
            return s390_emit_DR(buf, r1, r2);
         else
            return s390_emit_DLR(buf, r1, r2);

      case 8:
         if (signed_divide)
            vpanic("s390_insn_div_emit");
         else
            return s390_emit_DLGR(buf, r1, r2);

      default:
         goto fail;
      }
   }

   case S390_OPND_AMODE: {
      const s390_amode *am = op2.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      switch (insn->size) {
      case 4:
         switch (am->tag) {
         case S390_AMODE_B12:
         case S390_AMODE_BX12:
            if (signed_divide)
               return s390_emit_D(buf, r1, x, b, d);
            else
               return s390_emit_DL(buf, r1, x, b, DISP20(d));

         case S390_AMODE_B20:
         case S390_AMODE_BX20:
            if (signed_divide) {
               buf = s390_emit_LY(buf, R0, x, b, DISP20(d));
               return s390_emit_DR(buf, r1, R0);
            } else
               return s390_emit_DL(buf, r1, x, b, DISP20(d));
         }
         goto fail;

      case 8:
         if (signed_divide)
            vpanic("s390_insn_div_emit");
         else
            return s390_emit_DLG(buf, r1, x, b, DISP20(d));

      default:
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      ULong value = op2.variant.imm;

      switch (insn->size) {
      case 4:
         buf = s390_emit_load_32imm(buf, R0, value);
         if (signed_divide)
            return s390_emit_DR(buf, r1, R0);
         else
            return s390_emit_DLR(buf, r1, R0);

      case 8:
         buf = s390_emit_load_64imm(buf, R0, value);
         if (signed_divide)
            vpanic("s390_insn_div_emit");
         else
            return s390_emit_DLGR(buf, r1, R0);

      default:
         goto fail;
      }
   }

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_div_emit");
}


static UChar *
s390_insn_divs_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI op2;
   UChar r1;

   r1  = hregNumber(insn->variant.divs.rem);
   op2 = insn->variant.divs.op2;

   switch (op2.tag) {
   case S390_OPND_REG: {
      UInt r2 = hregNumber(op2.variant.reg);

      return s390_emit_DSGR(buf, r1, r2);
   }

   case S390_OPND_AMODE: {
      const s390_amode *am = op2.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      return s390_emit_DSG(buf, r1, x, b, DISP20(d));
   }

   case S390_OPND_IMMEDIATE: {
      ULong value = op2.variant.imm;

      buf = s390_emit_load_64imm(buf, R0, value);
      return s390_emit_DSGR(buf, r1, R0);
   }

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_divs_emit");
}


static UChar *
s390_insn_clz_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI src;
   UChar r1, r1p1, r2, *p;

   r1   = hregNumber(insn->variant.clz.num_bits);
   r1p1 = hregNumber(insn->variant.clz.clobber);

   vassert((r1 & 0x1) == 0);
   vassert(r1p1 == r1 + 1);

   p = buf;
   src = insn->variant.clz.src;

   /* Get operand and move it to r2 */
   switch (src.tag) {
   case S390_OPND_REG:
      r2 = hregNumber(src.variant.reg);
      break;

   case S390_OPND_AMODE: {
      const s390_amode *am = src.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      p  = s390_emit_LG(p, R0, x, b, DISP20(d));
      r2 = R0;
      break;
   }

   case S390_OPND_IMMEDIATE: {
      ULong value = src.variant.imm;

      p  = s390_emit_load_64imm(p, R0, value);
      r2 = R0;
      break;
   }

   default:
      goto fail;
   }

   /* Use FLOGR if you can */
   if (s390_host_has_eimm) {
      return s390_emit_FLOGR(p, r1, r2);
   }

   /*
      r0 = r2;
      r1 = 64;
      while (r0 != 0) {
        r1 -= 1;
        r0 >>= 1;
      }
   */
   p = s390_emit_LTGR(p, R0, r2);
   p = s390_emit_LLILL(p, r1,  64);

   p = s390_emit_BRC(p, S390_CC_E, (4 + 4 + 6 + 4 + 4)/ 2);  /* 4 bytes */
   p = s390_emit_AGHI(p, r1, (UShort)-1);         /* r1  -= 1;  4 bytes */
   p = s390_emit_SRLG(p, R0, R0, R0, DISP20(1));  /* r0 >>= 1;  6 bytes */
   p = s390_emit_LTGR(p, R0, R0);                 /* set cc     4 bytes */
   p = s390_emit_BRC(p, S390_CC_NE,               /*            4 bytes */
                     (UShort)(-(4 + 6 + 4) / 2));
   return p;

 fail:
   vpanic("s390_insn_clz_emit");
}


static UChar *
s390_insn_branch_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI dst;
   s390_cc_t cond;
   UInt       trc;
   UChar *p, *ptmp = 0;  /* avoid compiler warnings */

   cond = insn->variant.branch.cond;
   dst  = insn->variant.branch.dst;

   p = buf;
   trc = 0;

   if (cond != S390_CC_ALWAYS) {
      /* So we have something like this
         if (cond) goto X;
         Y: ...
         We convert this into
         if (! cond) goto Y;        // BRC insn; 4 bytes
         return_reg = X;
         return to dispatcher
         Y:
      */
      ptmp = p; /* 4 bytes (a BRC insn) to be filled in here */
      p += 4;
   }

   /* If a non-boring, set guest-state-pointer appropriately. */

   switch (insn->variant.branch.kind) {
   case Ijk_ClientReq:   trc = VEX_TRC_JMP_CLIENTREQ;   break;
   case Ijk_Sys_syscall: trc = VEX_TRC_JMP_SYS_SYSCALL; break;
   case Ijk_Yield:       trc = VEX_TRC_JMP_YIELD;       break;
   case Ijk_EmWarn:      trc = VEX_TRC_JMP_EMWARN;      break;
   case Ijk_EmFail:      trc = VEX_TRC_JMP_EMFAIL;      break;
   case Ijk_MapFail:     trc = VEX_TRC_JMP_MAPFAIL;     break;
   case Ijk_NoDecode:    trc = VEX_TRC_JMP_NODECODE;    break;
   case Ijk_TInval:      trc = VEX_TRC_JMP_TINVAL;      break;
   case Ijk_NoRedir:     trc = VEX_TRC_JMP_NOREDIR;     break;
   case Ijk_SigTRAP:     trc = VEX_TRC_JMP_SIGTRAP;     break;
   case Ijk_Ret:         trc = 0; break;
   case Ijk_Call:        trc = 0; break;
   case Ijk_Boring:      trc = 0; break;
      break;

   default:
      vpanic("s390_insn_branch_emit: unknown jump kind");
   }

   /* Get the destination address into the return register */
   switch (dst.tag) {
   case S390_OPND_REG:
      p = s390_emit_LGR(p, S390_REGNO_RETURN_VALUE, hregNumber(dst.variant.reg));
      break;

   case S390_OPND_AMODE: {
      const s390_amode *am = dst.variant.am;
      UChar b = hregNumber(am->b);
      UChar x = hregNumber(am->x);
      Int   d = am->d;

      p = s390_emit_LG(p, S390_REGNO_RETURN_VALUE, x, b, DISP20(d));
      break;
   }

   case S390_OPND_IMMEDIATE:
      p = s390_emit_load_64imm(p, S390_REGNO_RETURN_VALUE, dst.variant.imm);
      break;

   default:
      goto fail;
   }

   if (trc != 0) {
      /* Something special. Set guest-state pointer appropriately */
      p = s390_emit_LGHI(p, S390_REGNO_GUEST_STATE_POINTER, trc);
   } else {
      /* Nothing special needs to be done for calls and returns. */
   }

   p = s390_emit_BCR(p, S390_CC_ALWAYS, S390_REGNO_LINK_REGISTER);

   if (cond != S390_CC_ALWAYS) {
      Int delta = p - ptmp;

      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(ptmp, s390_cc_invert(cond), delta);
   }

   return p;

 fail:
   vpanic("s390_insn_branch_emit");
}


static UChar *
s390_insn_helper_call_emit(UChar *buf, const s390_insn *insn)
{
   s390_cc_t cond;
   ULong target;
   UChar *ptmp = buf;

   cond = insn->variant.helper_call.cond;
   target = insn->variant.helper_call.target;

   if (cond != S390_CC_ALWAYS) {
      /* So we have something like this
         if (cond) call X;
         Y: ...
         We convert this into
         if (! cond) goto Y;        // BRC opcode; 4 bytes
         call X;
         Y:
      */
      /* 4 bytes (a BRC insn) to be filled in here */
      buf += 4;
   }

   /* Load the target address into a register, that
      (a) is not used for passing parameters to the helper and
      (b) can be clobbered by the callee
      r1 looks like a good choice.
      Also, need to arrange for the return address be put into the
      link-register */
   buf = s390_emit_load_64imm(buf, 1, target);

   /* Stash away the client's FPC register because the helper might change it. */
   buf = s390_emit_STFPC(buf, S390_REGNO_STACK_POINTER, S390_OFFSET_SAVED_FPC_C);

   /* Before we can call the helper, we need to save the link register,
      because the BASR will overwrite it. We cannot use a register for that.
      (a) Volatile registers will be modified by the helper.
      (b) For saved registers the client code assumes that they have not
          changed after the function returns. So we cannot use it to store
          the link register.
      In the dispatcher, before calling the client code, we have arranged for
      a location on the stack for this purpose. See dispatch-s390x-linux.S. */
   buf = s390_emit_STG(buf, S390_REGNO_LINK_REGISTER, 0,        // save LR
                       S390_REGNO_STACK_POINTER, S390_OFFSET_SAVED_LR, 0);
   buf = s390_emit_BASR(buf, S390_REGNO_LINK_REGISTER, 1);      // call helper
   buf = s390_emit_LG(buf, S390_REGNO_LINK_REGISTER, 0,         // restore LR
                      S390_REGNO_STACK_POINTER, S390_OFFSET_SAVED_LR, 0);
   buf = s390_emit_LFPC(buf, S390_REGNO_STACK_POINTER,          // restore FPC
                        S390_OFFSET_SAVED_FPC_C);

   if (cond != S390_CC_ALWAYS) {
      Int delta = buf - ptmp;

      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(ptmp, s390_cc_invert(cond), delta);
   }

   return buf;
}


static UChar *
s390_insn_cond_move_emit(UChar *buf, const s390_insn *insn)
{
   HReg dst;
   s390_opnd_RMI src;
   s390_cc_t cond;
   UChar *p, *ptmp = 0;   /* avoid compiler warnings */

   cond = insn->variant.cond_move.cond;
   dst  = insn->variant.cond_move.dst;
   src  = insn->variant.cond_move.src;

   p = buf;

   /* Branch (if cond fails) over move instrs */
   if (cond != S390_CC_ALWAYS) {
      /* Don't know how many bytes to jump over yet.
         Make space for a BRC instruction (4 bytes) and fill in later. */
      ptmp = p;   /*  to be filled in here */
      p += 4;
   }

   // cond true: move src => dst

   switch (src.tag) {
   case S390_OPND_REG:
      p = s390_emit_LGR(p, hregNumber(dst), hregNumber(src.variant.reg));
      break;

   case S390_OPND_AMODE:
      p = s390_emit_load_mem(p, insn->size, hregNumber(dst), src.variant.am);
      break;

   case S390_OPND_IMMEDIATE: {
      ULong value = src.variant.imm;
      UInt  r = hregNumber(dst);

      switch (insn->size) {
      case 1:
      case 2:
         /* Load the immediate values as a 4 byte value. That does not hurt as
            those extra bytes will not be looked at. Fall through .... */
      case 4:
         p = s390_emit_load_32imm(p, r, value);
         break;

      case 8:
         p = s390_emit_load_64imm(p, r, value);
         break;
      }
      break;
   }

   default:
      goto fail;
   }

   if (cond != S390_CC_ALWAYS) {
      Int delta = p - ptmp;

      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(ptmp, s390_cc_invert(cond), delta);
   }

   return p;

 fail:
   vpanic("s390_insn_cond_move_emit");
}


/* Little helper function to the rounding mode in the real FPC
   register */
static UChar *
s390_set_fpc_rounding_mode(UChar *buf, s390_round_t rounding_mode)
{
   UChar bits;

   /* Determine BFP rounding bits */
   switch (rounding_mode) {
   case S390_ROUND_NEAREST_EVEN: bits = 0; break;
   case S390_ROUND_ZERO:         bits = 1; break;
   case S390_ROUND_POSINF:       bits = 2; break;
   case S390_ROUND_NEGINF:       bits = 3; break;
   default: vpanic("invalid rounding mode\n");
   }

   /* Copy FPC from guest state to R0 and OR in the new rounding mode */
   buf = s390_emit_L(buf, R0, 0, S390_REGNO_GUEST_STATE_POINTER,
                     OFFSET_s390x_fpc);   // r0 = guest_fpc

   buf = s390_emit_NILL(buf, R0, 0xFFFC); /* Clear out right-most 2 bits */
   buf = s390_emit_OILL(buf, R0, bits);   /* OR in the new rounding mode */
   buf = s390_emit_SFPC(buf, R0, 0);      /* Load FPC register from R0 */

   return buf;
}


static UChar *
s390_insn_bfp_triop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1 = hregNumber(insn->variant.bfp_triop.dst);
   UInt r2 = hregNumber(insn->variant.bfp_triop.op2);
   UInt r3 = hregNumber(insn->variant.bfp_triop.op3);
   s390_round_t rounding_mode = insn->variant.bfp_triop.rounding_mode;

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      buf = s390_set_fpc_rounding_mode(buf, rounding_mode);
   }

   switch (insn->size) {
   case 4:
      switch (insn->variant.bfp_triop.tag) {
      case S390_BFP_MADD:  buf = s390_emit_MAEBR(buf, r1, r3, r2); break;
      case S390_BFP_MSUB:  buf = s390_emit_MSEBR(buf, r1, r3, r2); break;
      default:  goto fail;
      }
      break;

   case 8:
      switch (insn->variant.bfp_triop.tag) {
      case S390_BFP_MADD:  buf = s390_emit_MADBR(buf, r1, r3, r2); break;
      case S390_BFP_MSUB:  buf = s390_emit_MSDBR(buf, r1, r3, r2); break;
      default:  goto fail;
      }
      break;

   default:  goto fail;
   }

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      /* Restore FPC register from guest state */
      buf = s390_emit_LFPC(buf, S390_REGNO_GUEST_STATE_POINTER,
                           OFFSET_s390x_fpc);   // fpc = guest_fpc
   }
   return buf;

 fail:
   vpanic("s390_insn_bfp_triop_emit");
}


static UChar *
s390_insn_bfp_binop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1 = hregNumber(insn->variant.bfp_binop.dst);
   UInt r2 = hregNumber(insn->variant.bfp_binop.op2);
   s390_round_t rounding_mode = insn->variant.bfp_binop.rounding_mode;

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      buf = s390_set_fpc_rounding_mode(buf, rounding_mode);
   }

   switch (insn->size) {
   case 4:
      switch (insn->variant.bfp_binop.tag) {
      case S390_BFP_ADD:     buf = s390_emit_AEBR(buf, r1, r2);  break;
      case S390_BFP_SUB:     buf = s390_emit_SEBR(buf, r1, r2);  break;
      case S390_BFP_MUL:     buf = s390_emit_MEEBR(buf, r1, r2); break;
      case S390_BFP_DIV:     buf = s390_emit_DEBR(buf, r1, r2);  break;
      default:  goto fail;
      }
      break;

   case 8:
      switch (insn->variant.bfp_binop.tag) {
      case S390_BFP_ADD:     buf = s390_emit_ADBR(buf, r1, r2); break;
      case S390_BFP_SUB:     buf = s390_emit_SDBR(buf, r1, r2); break;
      case S390_BFP_MUL:     buf = s390_emit_MDBR(buf, r1, r2); break;
      case S390_BFP_DIV:     buf = s390_emit_DDBR(buf, r1, r2); break;
      default:  goto fail;
      }
      break;

   default:  goto fail;
   }

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      /* Restore FPC register from guest state */
      buf = s390_emit_LFPC(buf, S390_REGNO_GUEST_STATE_POINTER,
                           OFFSET_s390x_fpc);
   }
   return buf;

 fail:
   vpanic("s390_insn_bfp_binop_emit");
}


static UChar *
s390_insn_bfp_unop_emit(UChar *buf, const s390_insn *insn)
{
   UInt  r1 = hregNumber(insn->variant.bfp_unop.dst);
   UInt  r2 = hregNumber(insn->variant.bfp_unop.op);
   s390_round_t rounding_mode = insn->variant.bfp_unop.rounding_mode;
   s390_round_t m3 = rounding_mode;

   /* The "convert to fixed" instructions have a field for the rounding
      mode and no FPC modification is necessary. So we handle them
      upfront. */
   switch (insn->variant.bfp_unop.tag) {
   case S390_BFP_F32_TO_I32:  return s390_emit_CFEBR(buf, m3, r1, r2);
   case S390_BFP_F64_TO_I32:  return s390_emit_CFDBR(buf, m3, r1, r2);
   case S390_BFP_F32_TO_I64:  return s390_emit_CGEBR(buf, m3, r1, r2);
   case S390_BFP_F64_TO_I64:  return s390_emit_CGDBR(buf, m3, r1, r2);
   default: break;
   }

   /* For all other insns if a special rounding mode is requested,
      we need to set the FPC first and restore it later. */
   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      buf = s390_set_fpc_rounding_mode(buf, rounding_mode);
   }

   switch (insn->variant.bfp_unop.tag) {
   case S390_BFP_ABS:
      switch (insn->size) {
      case 4:   buf = s390_emit_LPEBR(buf, r1, r2); break;
      case 8:   buf = s390_emit_LPDBR(buf, r1, r2); break;
      case 16:  buf = s390_emit_LPXBR(buf, r1, r2); break;
      default:  goto fail;
      }
      break;

   case S390_BFP_NABS:
      switch (insn->size) {
      case 4:   buf = s390_emit_LNEBR(buf, r1, r2); break;
      case 8:   buf = s390_emit_LNDBR(buf, r1, r2); break;
      case 16:  buf = s390_emit_LNXBR(buf, r1, r2); break;
      default:  goto fail;
      }
      break;

   case S390_BFP_NEG:
      switch (insn->size) {
      case 4:   buf = s390_emit_LCEBR(buf, r1, r2); break;
      case 8:   buf = s390_emit_LCDBR(buf, r1, r2); break;
      case 16:  buf = s390_emit_LCXBR(buf, r1, r2); break;
      default:  goto fail;
      }
      break;

   case S390_BFP_SQRT:
      switch (insn->size) {
      case 4:   buf = s390_emit_SQEBR(buf, r1, r2); break;
      case 8:   buf = s390_emit_SQDBR(buf, r1, r2); break;
      case 16:  buf = s390_emit_SQXBR(buf, r1, r2); break;
      default:  goto fail;
      }
      break;

   case S390_BFP_I32_TO_F32:  buf = s390_emit_CEFBR(buf, r1, r2); break;
   case S390_BFP_I32_TO_F64:  buf = s390_emit_CDFBR(buf, r1, r2); break;
   case S390_BFP_I32_TO_F128: buf = s390_emit_CXFBR(buf, r1, r2); break;
   case S390_BFP_I64_TO_F32:  buf = s390_emit_CEGBR(buf, r1, r2); break;
   case S390_BFP_I64_TO_F64:  buf = s390_emit_CDGBR(buf, r1, r2); break;
   case S390_BFP_I64_TO_F128: buf = s390_emit_CXGBR(buf, r1, r2); break;

   case S390_BFP_F32_TO_F64:  buf = s390_emit_LDEBR(buf, r1, r2); break;
   case S390_BFP_F32_TO_F128: buf = s390_emit_LXEBR(buf, r1, r2); break;
   case S390_BFP_F64_TO_F32:  buf = s390_emit_LEDBR(buf, r1, r2); break;
   case S390_BFP_F64_TO_F128: buf = s390_emit_LXDBR(buf, r1, r2); break;

   default: goto fail;
   }

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      /* Restore FPC register from guest state */
      buf = s390_emit_LFPC(buf, S390_REGNO_GUEST_STATE_POINTER,
                           OFFSET_s390x_fpc);   // fpc = guest_fpc
   }
   return buf;

 fail:
   vpanic("s390_insn_bfp_unop_emit");
}


static UChar *
s390_insn_bfp_compare_emit(UChar *buf, const s390_insn *insn)
{
   UInt dst = hregNumber(insn->variant.bfp_compare.dst);
   UInt r1  = hregNumber(insn->variant.bfp_compare.op1);
   UInt r2  = hregNumber(insn->variant.bfp_compare.op2);

   switch (insn->size) {
   case 4:
      buf = s390_emit_CEBR(buf, r1, r2);
      break;

   case 8:
      buf = s390_emit_CDBR(buf, r1, r2);
      break;

   default:  goto fail;
   }

   return s390_emit_load_cc(buf, dst);  /* Load condition code into DST */

 fail:
   vpanic("s390_insn_bfp_compare_emit");
}


static UChar *
s390_insn_bfp128_binop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1_hi = hregNumber(insn->variant.bfp128_binop.dst_hi);
   UInt r1_lo = hregNumber(insn->variant.bfp128_binop.dst_lo);
   UInt r2_hi = hregNumber(insn->variant.bfp128_binop.op2_hi);
   UInt r2_lo = hregNumber(insn->variant.bfp128_binop.op2_lo);
   s390_round_t rounding_mode = insn->variant.bfp_binop.rounding_mode;

   /* Paranoia */
   vassert(insn->size == 16);
   vassert(r1_lo == r1_hi + 2);
   vassert(r2_lo == r2_hi + 2);
   vassert((r1_hi & 0x2) == 0);
   vassert((r2_hi & 0x2) == 0);

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      buf = s390_set_fpc_rounding_mode(buf, rounding_mode);
   }

   switch (insn->variant.bfp128_binop.tag) {
   case S390_BFP_ADD:     buf = s390_emit_AXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_SUB:     buf = s390_emit_SXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_MUL:     buf = s390_emit_MXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_DIV:     buf = s390_emit_DXBR(buf, r1_hi, r2_hi); break;
   default:  goto fail;
   }

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      /* Restore FPC register from guest state */
      buf = s390_emit_LFPC(buf, S390_REGNO_GUEST_STATE_POINTER,
                           OFFSET_s390x_fpc);   // fpc = guest_fpc
   }
   return buf;

 fail:
   vpanic("s390_insn_bfp128_binop_emit");
}


static UChar *
s390_insn_bfp128_compare_emit(UChar *buf, const s390_insn *insn)
{
   UInt dst   = hregNumber(insn->variant.bfp128_compare.dst);
   UInt r1_hi = hregNumber(insn->variant.bfp128_compare.op1_hi);
   UInt r1_lo = hregNumber(insn->variant.bfp128_compare.op1_lo);
   UInt r2_hi = hregNumber(insn->variant.bfp128_compare.op2_hi);
   UInt r2_lo = hregNumber(insn->variant.bfp128_compare.op2_lo);

   /* Paranoia */
   vassert(insn->size == 16);
   vassert(r1_lo == r1_hi + 2);
   vassert(r2_lo == r2_hi + 2);
   vassert((r1_hi & 0x2) == 0);
   vassert((r2_hi & 0x2) == 0);

   buf = s390_emit_CXBR(buf, r1_hi, r2_hi);

   /* Load condition code into DST */
   return s390_emit_load_cc(buf, dst);
}


static UChar *
s390_insn_bfp128_unop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1_hi = hregNumber(insn->variant.bfp128_unop.dst_hi);
   UInt r1_lo = hregNumber(insn->variant.bfp128_unop.dst_lo);
   UInt r2_hi = hregNumber(insn->variant.bfp128_unop.op_hi);
   UInt r2_lo = hregNumber(insn->variant.bfp128_unop.op_lo);
   s390_round_t rounding_mode = insn->variant.bfp_binop.rounding_mode;

   /* Paranoia */
   vassert(insn->size == 16);
   vassert(r1_lo == r1_hi + 2);
   vassert(r2_lo == r2_hi + 2);
   vassert((r1_hi & 0x2) == 0);
   vassert((r2_hi & 0x2) == 0);

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      buf = s390_set_fpc_rounding_mode(buf, rounding_mode);
   }

   switch (insn->variant.bfp128_unop.tag) {
   case S390_BFP_ABS:         buf = s390_emit_LPXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_NABS:        buf = s390_emit_LNXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_NEG:         buf = s390_emit_LCXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_SQRT:        buf = s390_emit_SQXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_F128_TO_F32: buf = s390_emit_LEXBR(buf, r1_hi, r2_hi); break;
   case S390_BFP_F128_TO_F64: buf = s390_emit_LDXBR(buf, r1_hi, r2_hi); break;
   default:  goto fail;
   }

   if (rounding_mode != S390_ROUND_NEAREST_EVEN) {
      /* Restore FPC register from guest state */
      buf = s390_emit_LFPC(buf, S390_REGNO_GUEST_STATE_POINTER,
                           OFFSET_s390x_fpc);   // fpc = guest_fpc
   }
   return buf;

 fail:
   vpanic("s390_insn_bfp128_unop_emit");
}


/* Conversion to 128-bit BFP does not require a rounding mode */
static UChar *
s390_insn_bfp128_convert_to_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1_hi = hregNumber(insn->variant.bfp128_unop.dst_hi);
   UInt r1_lo = hregNumber(insn->variant.bfp128_unop.dst_lo);
   UInt r2    = hregNumber(insn->variant.bfp128_unop.op_hi);

   /* Paranoia */
   vassert(insn->size == 16);
   vassert(r1_lo == r1_hi + 2);
   vassert((r1_hi & 0x2) == 0);

   switch (insn->variant.bfp128_unop.tag) {
   case S390_BFP_I32_TO_F128: buf = s390_emit_CXFBR(buf, r1_hi, r2); break;
   case S390_BFP_I64_TO_F128: buf = s390_emit_CXGBR(buf, r1_hi, r2); break;
   case S390_BFP_F32_TO_F128: buf = s390_emit_LXEBR(buf, r1_hi, r2); break;
   case S390_BFP_F64_TO_F128: buf = s390_emit_LXDBR(buf, r1_hi, r2); break;
   default:  goto fail;
   }

   return buf;

 fail:
   vpanic("s390_insn_bfp128_convert_to_emit");
}


static UChar *
s390_insn_bfp128_convert_from_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1    = hregNumber(insn->variant.bfp128_unop.dst_hi);
   UInt r2_hi = hregNumber(insn->variant.bfp128_unop.op_hi);
   UInt r2_lo = hregNumber(insn->variant.bfp128_unop.op_lo);
   s390_round_t rounding_mode = insn->variant.bfp_binop.rounding_mode;

   /* Paranoia */
   vassert(insn->size != 16);
   vassert(r2_lo == r2_hi + 2);
   vassert((r2_hi & 0x2) == 0);

   /* The "convert to fixed" instructions have a field for the rounding
      mode and no FPC modification is necessary. So we handle them
      upfront. */
   switch (insn->variant.bfp_unop.tag) {
   case S390_BFP_F128_TO_I32: return s390_emit_CFXBR(buf, rounding_mode,
                                                     r1, r2_hi);  break;
   case S390_BFP_F128_TO_I64: return s390_emit_CGXBR(buf, rounding_mode,
                                                     r1, r2_hi);  break;
   default: break;
   }

   vpanic("s390_insn_bfp128_convert_from_emit");
}


static UChar *
s390_insn_mfence_emit(UChar *buf, const s390_insn *insn)
{
   return s390_emit_BCR(buf, 0xF, 0x0);
}


Int
emit_S390Instr(UChar *buf, Int nbuf, struct s390_insn *insn,
               Bool mode64, void *dispatch)
{
   UChar *end;

   switch (insn->tag) {
   case S390_INSN_LOAD:
      end = s390_insn_load_emit(buf, insn);
      break;

   case S390_INSN_STORE:
      end = s390_insn_store_emit(buf, insn);
      break;

   case S390_INSN_MOVE:
      end = s390_insn_move_emit(buf, insn);
      break;

   case S390_INSN_COND_MOVE:
      end = s390_insn_cond_move_emit(buf, insn);
      break;

   case S390_INSN_LOAD_IMMEDIATE:
      end = s390_insn_load_immediate_emit(buf, insn);
      break;

   case S390_INSN_ALU:
      end = s390_insn_alu_emit(buf, insn);
      break;

   case S390_INSN_MUL:
      end = s390_insn_mul_emit(buf, insn);
      break;

   case S390_INSN_DIV:
      end = s390_insn_div_emit(buf, insn);
      break;

   case S390_INSN_DIVS:
      end = s390_insn_divs_emit(buf, insn);
      break;

   case S390_INSN_CLZ:
      end = s390_insn_clz_emit(buf, insn);
      break;

   case S390_INSN_UNOP:
      end = s390_insn_unop_emit(buf, insn);
      break;

   case S390_INSN_TEST:
      end = s390_insn_test_emit(buf, insn);
      break;

   case S390_INSN_CC2BOOL:
      end = s390_insn_cc2bool_emit(buf, insn);
      break;

   case S390_INSN_CAS:
      end = s390_insn_cas_emit(buf, insn);
      break;

   case S390_INSN_COMPARE:
      end = s390_insn_compare_emit(buf, insn);
      break;

   case S390_INSN_BRANCH:
      end = s390_insn_branch_emit(buf, insn);
      break;

   case S390_INSN_HELPER_CALL:
      end = s390_insn_helper_call_emit(buf, insn);
      break;

   case S390_INSN_BFP_TRIOP:
      end = s390_insn_bfp_triop_emit(buf, insn);
      break;

   case S390_INSN_BFP_BINOP:
      end = s390_insn_bfp_binop_emit(buf, insn);
      break;

   case S390_INSN_BFP_UNOP:
      end = s390_insn_bfp_unop_emit(buf, insn);
      break;

   case S390_INSN_BFP_COMPARE:
      end = s390_insn_bfp_compare_emit(buf, insn);
      break;

   case S390_INSN_BFP128_BINOP:
      end = s390_insn_bfp128_binop_emit(buf, insn);
      break;

   case S390_INSN_BFP128_COMPARE:
      end = s390_insn_bfp128_compare_emit(buf, insn);
      break;

   case S390_INSN_BFP128_UNOP:
      end = s390_insn_bfp128_unop_emit(buf, insn);
      break;

   case S390_INSN_BFP128_CONVERT_TO:
      end = s390_insn_bfp128_convert_to_emit(buf, insn);
      break;

   case S390_INSN_BFP128_CONVERT_FROM:
      end = s390_insn_bfp128_convert_from_emit(buf, insn);
      break;

   case S390_INSN_MFENCE:
      end = s390_insn_mfence_emit(buf, insn);
      break;

   default:
      vpanic("s390_insn_emit");
   }

   vassert(end - buf <= nbuf);

   return end - buf;
}


/*---------------------------------------------------------------*/
/*--- end                                    host_s390_defs.c ---*/
/*---------------------------------------------------------------*/
