/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                  host_s390_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2020
   Copyright (C) 2012-2017  Florian Krohm   (britzel@acm.org)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Florian Krohm */

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"
#include "libvex_s390x_common.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_s390_defs.h"
#include "s390_disasm.h"
#include "guest_s390_defs.h"    /* S390X_GUEST_OFFSET */
#include <stdarg.h>

/*------------------------------------------------------------*/
/*--- Forward declarations                                 ---*/
/*------------------------------------------------------------*/

static void s390_insn_map_regs(HRegRemap *, s390_insn *);
static void s390_insn_get_reg_usage(HRegUsage *u, const s390_insn *);
static UInt s390_tchain_load64_len(void);


/*------------------------------------------------------------*/
/*--- Registers                                            ---*/
/*------------------------------------------------------------*/

/* A mapping from register number to register index */
static Int gpr_index[16];  // GPR regno -> register index
static Int vr_index[32];   // VR regno -> register index

HReg
s390_hreg_gpr(UInt regno)
{
   Int ix = gpr_index[regno];
   vassert(ix >= 0);
   return mkHReg(/*virtual*/False, HRcInt64, regno, ix);
}

HReg
s390_hreg_fpr(UInt regno)
{
   Int ix = vr_index[regno];
   vassert(ix >= 0);
   return mkHReg(/*virtual*/False, HRcFlt64, regno, ix);
}

HReg
s390_hreg_vr(UInt regno)
{
   Int ix = vr_index[regno];
   vassert(ix >= 0);
   return mkHReg(/*virtual*/False, HRcVec128, regno, ix);
}

static __inline__ UInt
hregNumber(HReg reg)
{
   return hregEncoding(reg);
}

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

   static const HChar vreg_names[32][5] = {
      "%v0",  "%v1",  "%v2",  "%v3",  "%v4",  "%v5",  "%v6",  "%v7",
      "%v8",  "%v9",  "%v10", "%v11", "%v12", "%v13", "%v14", "%v15",
      "%v16",  "%v17",  "%v18", "%v19", "%v20", "%v21", "%v22", "%v23",
      "%v24",  "%v25",  "%v26", "%v27", "%v28", "%v29", "%v30", "%v31"
   };

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      UInt r = hregIndex(reg);
      buf[0] = '\0';
      switch (hregClass(reg)) {
      case HRcInt64: vex_sprintf(buf, "%%vR%u", r); break;
      case HRcFlt64: vex_sprintf(buf, "%%vF%u", r); break;
      case HRcVec128: vex_sprintf(buf, "%%vV%u", r); break;
      default:       goto fail;
      }
      return buf;
   }

   /* But specific for real regs. */
   UInt r = hregNumber(reg);
   switch (hregClass(reg)) {
   case HRcInt64:  vassert(r < 16); return ireg_names[r];
   case HRcFlt64:  vassert(r < 16); return freg_names[r];
   case HRcVec128: vassert(r < 32); return vreg_names[r];
   default:        goto fail;
   }

 fail: vpanic("s390_hreg_as_string");
}


/* Return the real register that holds the guest state pointer */
HReg
s390_hreg_guest_state_pointer(void)
{
   return s390_hreg_gpr(S390_REGNO_GUEST_STATE_POINTER);
}

/* Return the real register that holds the stack pointer */
HReg
s390_hreg_stack_pointer(void)
{
   return s390_hreg_gpr(S390_REGNO_STACK_POINTER);
}


/* Is VALUE within the domain of a 20-bit signed integer. */
static __inline__ Bool
fits_signed_20bit(Int value)
{
   UInt uval = value;
   return ((Int)(uval << 12) >> 12) == value;
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
   s390_amode *am = LibVEX_Alloc_inline(sizeof(s390_amode));

   vassert(fits_unsigned_12bit(d));

   am->tag = S390_AMODE_B12;
   am->d = d;
   am->b = b;
   am->x = s390_hreg_gpr(0);  /* hregNumber(am->x) == 0 */

   return am;
}


/* Construct a b20 amode. */
s390_amode *
s390_amode_b20(Int d, HReg b)
{
   s390_amode *am = LibVEX_Alloc_inline(sizeof(s390_amode));

   vassert(fits_signed_20bit(d));

   am->tag = S390_AMODE_B20;
   am->d = d;
   am->b = b;
   am->x = s390_hreg_gpr(0);  /* hregNumber(am->x) == 0 */

   return am;
}


/* Construct a bx12 amode. */
s390_amode *
s390_amode_bx12(Int d, HReg b, HReg x)
{
   s390_amode *am = LibVEX_Alloc_inline(sizeof(s390_amode));

   vassert(fits_unsigned_12bit(d));
   vassert(hregNumber(b) != 0);
   vassert(hregNumber(x) != 0);

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
   s390_amode *am = LibVEX_Alloc_inline(sizeof(s390_amode));

   vassert(fits_signed_20bit(d));
   vassert(hregNumber(b) != 0);
   vassert(hregNumber(x) != 0);

   am->tag = S390_AMODE_BX20;
   am->d = d;
   am->b = b;
   am->x = x;

   return am;
}


/* Construct an AMODE for accessing the guest state at OFFSET. 
   OFFSET can be at most 3 * sizeof(VexGuestS390XState) + LibVEX_N_SPILL_BYTES
   which may be too large for a B12 addressing mode. 
   Use a B20 amode as a fallback which will be safe for any offset.
*/
s390_amode *
s390_amode_for_guest_state(Int offset)
{
   if (fits_unsigned_12bit(offset))
      return s390_amode_b12(offset, s390_hreg_guest_state_pointer());

   if (fits_signed_20bit(offset))
      return s390_amode_b20(offset, s390_hreg_guest_state_pointer());

   vpanic("invalid guest state offset");
}


/* Construct an AMODE for accessing stack pointer at OFFSET.
   OFFSET can be at most 3 * sizeof(VexGuestS390XState) + LibVEX_N_SPILL_BYTES
   which may be too large for a B12 addressing mode.
   Use a B20 amode as a fallback which will be safe for any offset.
*/
s390_amode *
s390_amode_for_stack_pointer(Int offset)
{
   if (fits_unsigned_12bit(offset))
      return s390_amode_b12(offset, s390_hreg_stack_pointer());

   if (fits_signed_20bit(offset))
      return s390_amode_b20(offset, s390_hreg_stack_pointer());

   vpanic("invalid stack pointer offset");
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

/* Helper function for all vector operations */
static UChar
s390_getM_from_size(const UChar size) {
   switch(size) {
   case 1:
      return 0;
   case 2:
      return 1;
   case 4:
      return 2;
   case 8:
      return 3;
   case 16:
      return 4;
   default:
      vex_printf("size=%d\n", size);
      vpanic("s390_getM_from_size: unknown size");
   }
}

/* Helper for generating RXB field in vector instructions */
static UChar
s390_update_rxb(const UChar rxb, const UChar index, UChar* vr) {
   vassert((index >= 1) && (index <= 4));
   UChar result = rxb;
   if(vr != NULL) {
      if(*vr >= 16) {
         result |= 1 << (4 - index);
         *vr -= 16;
      }
   }
   return result;
}

/* Sanity check for an amode */
Bool
s390_amode_is_sane(const s390_amode *am)
{
   switch (am->tag) {
   case S390_AMODE_B12:
      return (is_virtual_gpr(am->b) || sameHReg(am->b, s390_hreg_gpr(0))) &&
             fits_unsigned_12bit(am->d);

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

static Bool
s390_amode_is_constant(const s390_amode *am)
{
   return am->tag == S390_AMODE_B12 && sameHReg(am->b, s390_hreg_gpr(0));
}


/* Record the register use of an amode */
static void
s390_amode_get_reg_usage(HRegUsage *u, const s390_amode *am)
{
   if (!sameHReg(am->b, s390_hreg_gpr(0)))
      addHRegUse(u, HRmRead, am->b);
   if (!sameHReg(am->x, s390_hreg_gpr(0)))
      addHRegUse(u, HRmRead, am->x);
}


static void
s390_amode_map_regs(HRegRemap *m, s390_amode *am)
{
   if (!sameHReg(am->b, s390_hreg_gpr(0)))
      am->b = lookupHRegRemap(m, am->b);
   if (!sameHReg(am->x, s390_hreg_gpr(0)))
      am->x = lookupHRegRemap(m, am->x);
}


void
ppS390AMode(const s390_amode *am)
{
   vex_printf("%s", s390_amode_as_string(am));
}

void
ppS390Instr(const s390_insn *insn, Bool mode64)
{
   vex_printf("%s", s390_insn_as_string(insn));
}

UInt
ppHRegS390(HReg reg)
{
   return vex_printf("%s", s390_hreg_as_string(reg));
}

/*------------------------------------------------------------*/
/*--- Helpers for register allocation                      ---*/
/*------------------------------------------------------------*/

/* Initialise and return the "register universe", i.e. a list of
   all hardware registers. Called once. */
const RRegUniverse *
getRRegUniverse_S390(void)
{
   static RRegUniverse all_regs;
   static Bool initialised = False;
   RRegUniverse *ru = &all_regs;
   
   if (LIKELY(initialised))
      return ru;

   RRegUniverse__init(ru);

   /* Assign invalid values to the gpr/vr_index */
   for (UInt i = 0; i < sizeof gpr_index / sizeof gpr_index[0]; ++i)
      gpr_index[i] = -1;
   for (UInt i = 0; i < sizeof vr_index / sizeof vr_index[0]; ++i)
      vr_index[i] = -1;


   /* Add the registers that are available to the register allocator.
      GPRs:  registers 6..11 are callee saved, list them first
             registers 1..5 are caller saved, list them after
      FPRs:  registers 8..15 are callee saved, list them first
             registers 0..7 are caller saved, list them after
             FPR12 - FPR15 are also used as register pairs for 128-bit
             floating point operations
      VRs:   registers 0..31 are available
   */
   ru->allocable_start[HRcInt64] = ru->size;
   for (UInt regno = 6; regno <= 11; ++regno) {
      gpr_index[regno] = ru->size;
      ru->regs[ru->size++] = s390_hreg_gpr(regno);
   }
   for (UInt regno = 1; regno <= 5; ++regno) {
      gpr_index[regno] = ru->size;
      ru->regs[ru->size++] = s390_hreg_gpr(regno);
   }
   ru->allocable_end[HRcInt64] = ru->size - 1;

   ru->allocable_start[HRcFlt64] = ru->size;
   for (UInt regno = 8; regno <= 15; ++regno) {
      vr_index[regno] = ru->size;
      ru->regs[ru->size++] = s390_hreg_fpr(regno);
   }
   for (UInt regno = 0; regno <= 7; ++regno) {
      vr_index[regno] = ru->size;
      ru->regs[ru->size++] = s390_hreg_fpr(regno);
   }
   ru->allocable_end[HRcFlt64] = ru->size - 1;

   ru->allocable_start[HRcVec128] = ru->size;
   for (UInt regno = 16; regno <= 31; ++regno) {
      vr_index[regno] = ru->size;
      ru->regs[ru->size++] = s390_hreg_vr(regno);
   }
   ru->allocable_end[HRcVec128] = ru->size - 1;
   ru->allocable = ru->size;

   /* Add the registers that are not available for allocation.
      r0  -- cannot be used as a base or index register
      r12 -- scratch register for translation chaining support
      r13 -- guest state pointer
      r14 -- link register
      r15 -- stack pointer
   */
   UInt other[] = { 0, 12, 13, 14, 15 };
   for (UInt i = 0; i < sizeof other / sizeof other[0]; ++i) {
      gpr_index[other[i]] = ru->size;
      ru->regs[ru->size++] = s390_hreg_gpr(other[i]);
   }

   /* Sanity checking */
   for (UInt i = 0; i < sizeof gpr_index / sizeof gpr_index[0]; ++i)
      vassert(gpr_index[i] >= 0);
   for (UInt i = 0; i < sizeof vr_index / sizeof vr_index[0]; ++i)
      vassert(vr_index[i] >= 0);
                 
   initialised = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}

/* Tell the register allocator how the given instruction uses the registers
   it refers to. */
void
getRegUsage_S390Instr(HRegUsage *u, const s390_insn *insn, Bool mode64)
{
   s390_insn_get_reg_usage(u, insn);
}


/* Map the registers of the given instruction */
void
mapRegs_S390Instr(HRegRemap *m, s390_insn *insn, Bool mode64)
{
   s390_insn_map_regs(m, insn);
}


/* Generate s390 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. This is like an Ist_Put */
void
genSpill_S390(HInstr **i1, HInstr **i2, HReg rreg, Int offsetB, Bool mode64)
{
   s390_amode *am;

   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   *i1 = *i2 = NULL;

   am = s390_amode_for_guest_state(offsetB);

   switch (hregClass(rreg)) {
   case HRcInt64:
   case HRcFlt64:
      *i1 = s390_insn_store(8, am, rreg);
      return;
   case HRcVec128:
      *i1 = s390_insn_store(16, am, rreg);
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
   vassert(!hregIsVirtual(rreg));

   *i1 = *i2 = NULL;

   am = s390_amode_for_guest_state(offsetB);

   switch (hregClass(rreg)) {
   case HRcInt64:
   case HRcFlt64:
      *i1 = s390_insn_load(8, rreg, am);
      return;
   case HRcVec128:
      *i1 = s390_insn_load(16, rreg, am);
      return;
   default:
      ppHRegClass(hregClass(rreg));
      vpanic("genReload_S390: unimplemented regclass");
   }
}

/* Direct reload function.  For the given vreg (currently located at the given
   spill offset) and a given instruction that reads vreg exactly once, return a
   variant of the instruction that references the spill slot directly.  Return
   NULL if no such instruction is found. */
HInstr *
directReload_S390(HInstr* i, HReg vreg, Short spill_off)
{
   s390_insn* insn = (s390_insn *) i;

   /* For simplicity, reject spill offsets that may cause trouble with 12-bit
      addressing.  They probably shouldn't occur anyway. */
   if (!fits_unsigned_12bit(spill_off + 15))
      return NULL;

   /* In case of a spilled GPR, adjust the offset to be right-aligned within the
      spill slot. */
   Int delta = hregClass(vreg) == HRcInt64 ? 8 - insn->size : 0;
   s390_amode* vreg_am = s390_amode_for_guest_state(spill_off + delta);
   s390_opnd_RMI vreg_opnd;
   vreg_opnd.tag = S390_OPND_AMODE;
   vreg_opnd.variant.am = vreg_am;

   /* v-move <reg>,<vreg> */
   if (insn->tag == S390_INSN_MOVE
       && sameHReg(insn->variant.move.src, vreg)) {
      return s390_insn_load(insn->size, insn->variant.move.dst, vreg_am);
   }

   /* v-store <vreg>,<addr> */
   if (insn->tag == S390_INSN_STORE
       && sameHReg(insn->variant.store.src, vreg)
       && insn->variant.store.dst->tag == S390_AMODE_B12) {
      return s390_insn_memcpy(insn->size, insn->variant.store.dst, vreg_am);
   }

   /* v-test <vreg> */
   if (insn->tag == S390_INSN_TEST
       && insn->variant.test.src.tag == S390_OPND_REG
       && sameHReg(insn->variant.test.src.variant.reg, vreg)) {
      return s390_insn_test(insn->size, vreg_opnd);
   }

   /* v-<alu> <reg>,<vreg> */
   if (insn->tag == S390_INSN_ALU
       && insn->variant.alu.op2.tag == S390_OPND_REG
       && sameHReg(insn->variant.alu.op2.variant.reg, vreg)) {
      return s390_insn_alu(insn->size, insn->variant.alu.tag,
                           insn->variant.alu.dst, vreg_opnd);
   }

   /* v-vgetelem <reg>,<vreg> */
   if (insn->tag == S390_INSN_VEC_AMODEOP
       && insn->variant.vec_amodeop.tag == S390_VEC_GET_ELEM
       && insn->size == 8
       && sameHReg(insn->variant.vec_amodeop.op1, vreg)
       && s390_amode_is_constant(insn->variant.vec_amodeop.op2)) {
      vreg_am->d += 8 * insn->variant.vec_amodeop.op2->d;
      return s390_insn_load(insn->size, insn->variant.vec_amodeop.dst, vreg_am);
   }

   /* v-<unop> <reg>,<vreg> */
   if (insn->tag == S390_INSN_UNOP
       && insn->variant.unop.src.tag == S390_OPND_REG
       && sameHReg(insn->variant.unop.src.variant.reg, vreg)
       && hregClass(vreg) == HRcInt64) {
      /* Some operations define the input size to be different from the insn's
         `size' field.  Adjust the address accordingly. */
      switch (insn->variant.unop.tag) {
      case S390_ZERO_EXTEND_8:
      case S390_SIGN_EXTEND_8:  vreg_am->d = spill_off + 7; break;
      case S390_ZERO_EXTEND_16:
      case S390_SIGN_EXTEND_16: vreg_am->d = spill_off + 6; break;
      case S390_ZERO_EXTEND_32:
      case S390_SIGN_EXTEND_32: vreg_am->d = spill_off + 4; break;
      case S390_NEGATE:         /* Nothing to adjust. */    break;
      default:
         goto no_match;
      }
      return s390_insn_unop(insn->size, insn->variant.unop.tag,
                            insn->variant.unop.dst, vreg_opnd);
   }

   /* v-vrep <reg>,<vreg>,<idx> */
   if (insn->tag == S390_INSN_VEC_REPLICATE
       && sameHReg(insn->variant.vec_replicate.op1, vreg)) {
      vreg_am->d += insn->size * insn->variant.vec_replicate.idx;
      return s390_insn_unop(insn->size, S390_VEC_DUPLICATE,
                            insn->variant.vec_replicate.dst, vreg_opnd);
   }

no_match:
   return NULL;
}

s390_insn* genMove_S390(HReg from, HReg to, Bool mode64)
{
   switch (hregClass(from)) {
   case HRcInt64:
      return s390_insn_move(sizeofIRType(Ity_I64), to, from);
   case HRcFlt64:
      return s390_insn_move(sizeofIRType(Ity_F64), to, from);
   case HRcVec128:
      return s390_insn_move(sizeofIRType(Ity_V128), to, from);
   default:
      ppHRegClass(hregClass(from));
      vpanic("genMove_S390: unimplemented regclass");
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
static void
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

      if (hregClass(insn->variant.move.src) == hregClass(insn->variant.move.dst)) {
         u->isRegRegMove = True;
         u->regMoveSrc   = insn->variant.move.src;
         u->regMoveDst   = insn->variant.move.dst;
      }
      break;

   case S390_INSN_MEMCPY:
      s390_amode_get_reg_usage(u, insn->variant.memcpy.src);
      s390_amode_get_reg_usage(u, insn->variant.memcpy.dst);
      break;

   case S390_INSN_COND_MOVE:
      switch (insn->variant.cond_move.cond) {
      case S390_CC_NEVER:
         break;
      case S390_CC_ALWAYS:
         s390_opnd_RMI_get_reg_usage(u, insn->variant.cond_move.src);
         addHRegUse(u, HRmWrite, insn->variant.cond_move.dst);
         break;
      default:
         s390_opnd_RMI_get_reg_usage(u, insn->variant.cond_move.src);
         addHRegUse(u, HRmModify, insn->variant.cond_move.dst);
         break;
      }
      break;

   case S390_INSN_ALU:
      addHRegUse(u, HRmModify, insn->variant.alu.dst); /* op1 */
      s390_opnd_RMI_get_reg_usage(u, insn->variant.alu.op2);
      break;

   case S390_INSN_SMUL:
   case S390_INSN_UMUL:
      addHRegUse(u, HRmModify, insn->variant.mul.dst_lo); /* op1 */
      addHRegUse(u, HRmWrite, insn->variant.mul.dst_hi);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.mul.op2);
      break;

   case S390_INSN_SDIV:
   case S390_INSN_UDIV:
      addHRegUse(u, HRmModify, insn->variant.div.op1_lo);
      addHRegUse(u, HRmModify, insn->variant.div.op1_hi);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.div.op2);
      break;

   case S390_INSN_DIVS:
      addHRegUse(u, HRmModify, insn->variant.divs.op1); /* quotient */
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

   case S390_INSN_CDAS: {
      s390_cdas *cdas = insn->variant.cdas.details;

      addHRegUse(u, HRmRead,  cdas->op1_high);
      addHRegUse(u, HRmRead,  cdas->op1_low);
      s390_amode_get_reg_usage(u, cdas->op2);
      addHRegUse(u, HRmRead,  cdas->op3_high);
      addHRegUse(u, HRmRead,  cdas->op3_low);
      addHRegUse(u, HRmWrite, cdas->old_mem_high);
      addHRegUse(u, HRmWrite, cdas->old_mem_low);
      addHRegUse(u, HRmWrite, cdas->scratch);
      break;
   }

   case S390_INSN_COMPARE:
      addHRegUse(u, HRmRead, insn->variant.compare.src1);
      s390_opnd_RMI_get_reg_usage(u, insn->variant.compare.src2);
      break;

   case S390_INSN_HELPER_CALL: {
      UInt i;

      /* Assume that all volatile registers are clobbered. ABI says,
         volatile registers are: r0 - r5. Valgrind's register allocator
         does not know about r0, so we can leave that out */
      for (i = 1; i <= 5; ++i) {
         addHRegUse(u, HRmWrite, s390_hreg_gpr(i));
      }

      /* Ditto for floating point registers. f0 - f7 are volatile */
      for (i = 0; i <= 7; ++i) {
         addHRegUse(u, HRmWrite, s390_hreg_fpr(i));
      }

      /* Ditto for all allocatable vector registers. */
      for (i = 16; i <= 31; ++i) {
         addHRegUse(u, HRmWrite, s390_hreg_vr(i));
      }

      /* The registers that are used for passing arguments will be read.
         Not all of them may, but in general we need to assume that. */
      for (i = 0; i < insn->variant.helper_call.details->num_args; ++i) {
         addHRegUse(u, HRmRead, s390_hreg_gpr(s390_gprno_from_arg_index(i)));
      }

      /* s390_insn_helper_call_emit also reads / writes the link register
         and stack pointer. But those registers are not visible to the
         register allocator. So we don't need to do anything for them. */
      break;
   }

   case S390_INSN_BFP_TRIOP:
      addHRegUse(u, HRmModify, insn->variant.bfp_triop.dst); /* first */
      addHRegUse(u, HRmRead,  insn->variant.bfp_triop.op2);  /* second */
      addHRegUse(u, HRmRead,  insn->variant.bfp_triop.op3);  /* third */
      break;

   case S390_INSN_BFP_BINOP:
      addHRegUse(u, HRmModify, insn->variant.bfp_binop.dst_hi); /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp_binop.op2_hi);  /* right */
      if (insn->size == 16) {
         addHRegUse(u, HRmModify, insn->variant.bfp_binop.dst_lo); /* left */
         addHRegUse(u, HRmRead,  insn->variant.bfp_binop.op2_lo);  /* right */
      }
      break;

   case S390_INSN_BFP_UNOP:
      addHRegUse(u, HRmWrite, insn->variant.bfp_unop.dst_hi);
      addHRegUse(u, HRmRead,  insn->variant.bfp_unop.op_hi);  /* operand */
      if (insn->size == 16) {
         addHRegUse(u, HRmWrite, insn->variant.bfp_unop.dst_lo);
         addHRegUse(u, HRmRead,  insn->variant.bfp_unop.op_lo);  /* operand */
      }
      break;

   case S390_INSN_BFP_COMPARE:
      addHRegUse(u, HRmWrite, insn->variant.bfp_compare.dst);
      addHRegUse(u, HRmRead,  insn->variant.bfp_compare.op1_hi);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.bfp_compare.op2_hi);  /* right */
      if (insn->size == 16) {
         addHRegUse(u, HRmRead,  insn->variant.bfp_compare.op1_lo);  /* left */
         addHRegUse(u, HRmRead,  insn->variant.bfp_compare.op2_lo);  /* right */
      }
      break;

   case S390_INSN_BFP_CONVERT:
      addHRegUse(u, HRmWrite, insn->variant.bfp_convert.dst_hi);
      if (! hregIsInvalid(insn->variant.bfp_convert.dst_lo))
         addHRegUse(u, HRmWrite, insn->variant.bfp_convert.dst_lo);
      addHRegUse(u, HRmRead,  insn->variant.bfp_convert.op_hi);
      if (! hregIsInvalid(insn->variant.bfp_convert.op_lo))
         addHRegUse(u, HRmRead, insn->variant.bfp_convert.op_lo);
      break;

   case S390_INSN_DFP_BINOP: {
      s390_dfp_binop *dfp_binop = insn->variant.dfp_binop.details;

      addHRegUse(u, HRmWrite, dfp_binop->dst_hi);
      addHRegUse(u, HRmRead,  dfp_binop->op2_hi);  /* left */
      addHRegUse(u, HRmRead,  dfp_binop->op3_hi);  /* right */
      if (insn->size == 16) {
         addHRegUse(u, HRmWrite, dfp_binop->dst_lo);
         addHRegUse(u, HRmRead,  dfp_binop->op2_lo);  /* left */
         addHRegUse(u, HRmRead,  dfp_binop->op3_lo);  /* right */
      }
      break;
   }

   case S390_INSN_DFP_UNOP:
      addHRegUse(u, HRmWrite, insn->variant.dfp_unop.dst_hi);
      addHRegUse(u, HRmRead,  insn->variant.dfp_unop.op_hi);  /* operand */
      if (insn->size == 16) {
         addHRegUse(u, HRmWrite, insn->variant.dfp_unop.dst_lo);
         addHRegUse(u, HRmRead,  insn->variant.dfp_unop.op_lo);  /* operand */
      }
      break;

   case S390_INSN_DFP_INTOP:
      addHRegUse(u, HRmWrite, insn->variant.dfp_intop.dst_hi);
      addHRegUse(u, HRmRead,  insn->variant.dfp_intop.op2);
      addHRegUse(u, HRmRead,  insn->variant.dfp_intop.op3_hi);
      if (insn->size == 16) {
         addHRegUse(u, HRmWrite, insn->variant.dfp_intop.dst_lo);
         addHRegUse(u, HRmRead,  insn->variant.dfp_intop.op3_lo);
      }
      break;

   case S390_INSN_DFP_COMPARE:
      addHRegUse(u, HRmWrite, insn->variant.dfp_compare.dst);
      addHRegUse(u, HRmRead,  insn->variant.dfp_compare.op1_hi);  /* left */
      addHRegUse(u, HRmRead,  insn->variant.dfp_compare.op2_hi);  /* right */
      if (insn->size == 16) {
         addHRegUse(u, HRmRead,  insn->variant.dfp_compare.op1_lo);  /* left */
         addHRegUse(u, HRmRead,  insn->variant.dfp_compare.op2_lo);  /* right */
      }
      break;

   case S390_INSN_DFP_CONVERT:
      addHRegUse(u, HRmWrite, insn->variant.dfp_convert.dst_hi);
      if (! hregIsInvalid(insn->variant.dfp_convert.dst_lo))
         addHRegUse(u, HRmWrite, insn->variant.dfp_convert.dst_lo);
      addHRegUse(u, HRmRead,  insn->variant.dfp_convert.op_hi);  /* operand */
      if (! hregIsInvalid(insn->variant.dfp_convert.op_lo))
         addHRegUse(u, HRmRead, insn->variant.dfp_convert.op_lo); /* operand */
      break;

   case S390_INSN_DFP_REROUND:
      addHRegUse(u, HRmWrite, insn->variant.dfp_reround.dst_hi);
      addHRegUse(u, HRmRead,  insn->variant.dfp_reround.op2);     /* left */
      addHRegUse(u, HRmRead,  insn->variant.dfp_reround.op3_hi);  /* right */
      if (insn->size == 16) {
         addHRegUse(u, HRmWrite, insn->variant.dfp_reround.dst_lo);
         addHRegUse(u, HRmRead,  insn->variant.dfp_reround.op3_lo); /* right */
      }
      break;

   case S390_INSN_FP_CONVERT: {
      s390_fp_convert *fp_convert = insn->variant.fp_convert.details;

      addHRegUse(u, HRmWrite, fp_convert->dst_hi);
      if (! hregIsInvalid(fp_convert->dst_lo))
         addHRegUse(u, HRmWrite, fp_convert->dst_lo);
      addHRegUse(u, HRmRead,  fp_convert->op_hi);
      if (! hregIsInvalid(fp_convert->op_lo))
         addHRegUse(u, HRmRead, fp_convert->op_lo);
      addHRegUse(u, HRmWrite, fp_convert->r1);
      break;
   }

   case S390_INSN_MIMM:
      s390_amode_get_reg_usage(u, insn->variant.mimm.dst);
      break;

   case S390_INSN_MADD:
      s390_amode_get_reg_usage(u, insn->variant.madd.dst);
      break;

   case S390_INSN_MFENCE:
      break;

   case S390_INSN_SET_FPC_BFPRM:
      addHRegUse(u, HRmRead,  insn->variant.set_fpc_bfprm.mode);
      break;

   case S390_INSN_SET_FPC_DFPRM:
      addHRegUse(u, HRmRead,  insn->variant.set_fpc_dfprm.mode);
      break;

   case S390_INSN_EVCHECK:
      s390_amode_get_reg_usage(u, insn->variant.evcheck.counter);
      s390_amode_get_reg_usage(u, insn->variant.evcheck.fail_addr);
      break;

   case S390_INSN_PROFINC:
      /* Does not use any register visible to the register allocator */
      break;

   case S390_INSN_XDIRECT:
      s390_amode_get_reg_usage(u, insn->variant.xdirect.guest_IA);
      break;

   case S390_INSN_XINDIR:
      addHRegUse(u, HRmRead, insn->variant.xindir.dst);
      s390_amode_get_reg_usage(u, insn->variant.xindir.guest_IA);
      break;

   case S390_INSN_XASSISTED:
      addHRegUse(u, HRmRead, insn->variant.xassisted.dst);
      s390_amode_get_reg_usage(u, insn->variant.xassisted.guest_IA);
      break;

   case S390_INSN_VEC_AMODEOP:
      addHRegUse(u, HRmWrite, insn->variant.vec_amodeop.dst);
      addHRegUse(u, HRmRead, insn->variant.vec_amodeop.op1);
      s390_amode_get_reg_usage(u, insn->variant.vec_amodeop.op2);
      break;

   case S390_INSN_VEC_AMODEINTOP:
      addHRegUse(u, HRmModify, insn->variant.vec_amodeintop.dst);
      s390_amode_get_reg_usage(u, insn->variant.vec_amodeintop.op2);
      addHRegUse(u, HRmRead, insn->variant.vec_amodeintop.op3);
      break;

   case S390_INSN_VEC_BINOP:
      addHRegUse(u, HRmWrite, insn->variant.vec_binop.dst);
      addHRegUse(u, HRmRead, insn->variant.vec_binop.op1);
      addHRegUse(u, HRmRead, insn->variant.vec_binop.op2);
      break;

   case S390_INSN_VEC_TRIOP:
      addHRegUse(u, HRmWrite, insn->variant.vec_triop.dst);
      addHRegUse(u, HRmRead, insn->variant.vec_triop.op1);
      addHRegUse(u, HRmRead, insn->variant.vec_triop.op2);
      addHRegUse(u, HRmRead, insn->variant.vec_triop.op3);
      break;

   case S390_INSN_VEC_REPLICATE:
      addHRegUse(u, HRmWrite, insn->variant.vec_replicate.dst);
      addHRegUse(u, HRmRead, insn->variant.vec_replicate.op1);
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


static void
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

   case S390_INSN_MEMCPY:
      s390_amode_map_regs(m, insn->variant.memcpy.dst);
      s390_amode_map_regs(m, insn->variant.memcpy.src);
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

   case S390_INSN_SMUL:
   case S390_INSN_UMUL:
      insn->variant.mul.dst_hi = lookupHRegRemap(m, insn->variant.mul.dst_hi);
      insn->variant.mul.dst_lo = lookupHRegRemap(m, insn->variant.mul.dst_lo);
      s390_opnd_RMI_map_regs(m, &insn->variant.mul.op2);
      break;

   case S390_INSN_SDIV:
   case S390_INSN_UDIV:
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

   case S390_INSN_CDAS: {
      s390_cdas *cdas = insn->variant.cdas.details;

      cdas->op1_high = lookupHRegRemap(m, cdas->op1_high);
      cdas->op1_low  = lookupHRegRemap(m, cdas->op1_low);
      s390_amode_map_regs(m, cdas->op2);
      cdas->op3_high = lookupHRegRemap(m, cdas->op3_high);
      cdas->op3_low  = lookupHRegRemap(m, cdas->op3_low);
      cdas->old_mem_high = lookupHRegRemap(m, cdas->old_mem_high);
      cdas->old_mem_low  = lookupHRegRemap(m, cdas->old_mem_low);
      cdas->scratch  = lookupHRegRemap(m, cdas->scratch);
      break;
   }

   case S390_INSN_COMPARE:
      insn->variant.compare.src1 = lookupHRegRemap(m, insn->variant.compare.src1);
      s390_opnd_RMI_map_regs(m, &insn->variant.compare.src2);
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
      insn->variant.bfp_triop.dst =
         lookupHRegRemap(m, insn->variant.bfp_triop.dst);
      insn->variant.bfp_triop.op2 =
         lookupHRegRemap(m, insn->variant.bfp_triop.op2);
      insn->variant.bfp_triop.op3 =
         lookupHRegRemap(m, insn->variant.bfp_triop.op3);
      break;

   case S390_INSN_BFP_BINOP:
      insn->variant.bfp_binop.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp_binop.dst_hi);
      insn->variant.bfp_binop.op2_hi =
         lookupHRegRemap(m, insn->variant.bfp_binop.op2_hi);
      if (insn->size == 16) {
         insn->variant.bfp_binop.dst_lo =
            lookupHRegRemap(m, insn->variant.bfp_binop.dst_lo);
         insn->variant.bfp_binop.op2_lo  =
            lookupHRegRemap(m, insn->variant.bfp_binop.op2_lo);
      }
      break;

   case S390_INSN_BFP_UNOP:
      insn->variant.bfp_unop.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp_unop.dst_hi);
      insn->variant.bfp_unop.op_hi  =
         lookupHRegRemap(m, insn->variant.bfp_unop.op_hi);
      if (insn->size == 16) {
         insn->variant.bfp_unop.dst_lo =
            lookupHRegRemap(m, insn->variant.bfp_unop.dst_lo);
         insn->variant.bfp_unop.op_lo  =
            lookupHRegRemap(m, insn->variant.bfp_unop.op_lo);
      }
      break;

   case S390_INSN_BFP_COMPARE:
      insn->variant.bfp_compare.dst =
         lookupHRegRemap(m, insn->variant.bfp_compare.dst);
      insn->variant.bfp_compare.op1_hi =
         lookupHRegRemap(m, insn->variant.bfp_compare.op1_hi);
      insn->variant.bfp_compare.op2_hi =
         lookupHRegRemap(m, insn->variant.bfp_compare.op2_hi);
      if (insn->size == 16) {
         insn->variant.bfp_compare.op1_lo =
            lookupHRegRemap(m, insn->variant.bfp_compare.op1_lo);
         insn->variant.bfp_compare.op2_lo =
            lookupHRegRemap(m, insn->variant.bfp_compare.op2_lo);
      }
      break;

   case S390_INSN_BFP_CONVERT:
      insn->variant.bfp_convert.dst_hi =
         lookupHRegRemap(m, insn->variant.bfp_convert.dst_hi);
      if (! hregIsInvalid(insn->variant.bfp_convert.dst_lo))
         insn->variant.bfp_convert.dst_lo =
            lookupHRegRemap(m, insn->variant.bfp_convert.dst_lo);
      insn->variant.bfp_convert.op_hi =
         lookupHRegRemap(m, insn->variant.bfp_convert.op_hi);
      if (! hregIsInvalid(insn->variant.bfp_convert.op_lo))
         insn->variant.bfp_convert.op_lo =
            lookupHRegRemap(m, insn->variant.bfp_convert.op_lo);
      break;

   case S390_INSN_DFP_BINOP: {
      s390_dfp_binop *dfp_binop = insn->variant.dfp_binop.details;

      dfp_binop->dst_hi = lookupHRegRemap(m, dfp_binop->dst_hi);
      dfp_binop->op2_hi = lookupHRegRemap(m, dfp_binop->op2_hi);
      dfp_binop->op3_hi = lookupHRegRemap(m, dfp_binop->op3_hi);
      if (insn->size == 16) {
         dfp_binop->dst_lo = lookupHRegRemap(m, dfp_binop->dst_lo);
         dfp_binop->op2_lo = lookupHRegRemap(m, dfp_binop->op2_lo);
         dfp_binop->op3_lo = lookupHRegRemap(m, dfp_binop->op3_lo);
      }
      break;
   }

   case S390_INSN_DFP_UNOP:
      insn->variant.dfp_unop.dst_hi =
         lookupHRegRemap(m, insn->variant.dfp_unop.dst_hi);
      insn->variant.dfp_unop.op_hi  =
         lookupHRegRemap(m, insn->variant.dfp_unop.op_hi);
      if (insn->size == 16) {
         insn->variant.dfp_unop.dst_lo =
            lookupHRegRemap(m, insn->variant.dfp_unop.dst_lo);
         insn->variant.dfp_unop.op_lo  =
            lookupHRegRemap(m, insn->variant.dfp_unop.op_lo);
      }
      break;

   case S390_INSN_DFP_INTOP:
      insn->variant.dfp_intop.dst_hi =
         lookupHRegRemap(m, insn->variant.dfp_intop.dst_hi);
      insn->variant.dfp_intop.op2    =
         lookupHRegRemap(m, insn->variant.dfp_intop.op2);
      insn->variant.dfp_intop.op3_hi =
         lookupHRegRemap(m, insn->variant.dfp_intop.op3_hi);
      if (insn->size == 16) {
         insn->variant.dfp_intop.dst_lo =
            lookupHRegRemap(m, insn->variant.dfp_intop.dst_lo);
         insn->variant.dfp_intop.op3_lo =
            lookupHRegRemap(m, insn->variant.dfp_intop.op3_lo);
      }
      break;

   case S390_INSN_DFP_COMPARE:
      insn->variant.dfp_compare.dst =
         lookupHRegRemap(m, insn->variant.dfp_compare.dst);
      insn->variant.dfp_compare.op1_hi =
         lookupHRegRemap(m, insn->variant.dfp_compare.op1_hi);
      insn->variant.dfp_compare.op2_hi =
         lookupHRegRemap(m, insn->variant.dfp_compare.op2_hi);
      if (insn->size == 16) {
         insn->variant.dfp_compare.op1_lo =
            lookupHRegRemap(m, insn->variant.dfp_compare.op1_lo);
         insn->variant.dfp_compare.op2_lo =
            lookupHRegRemap(m, insn->variant.dfp_compare.op2_lo);
      }
      break;

   case S390_INSN_DFP_CONVERT:
      insn->variant.dfp_convert.dst_hi =
         lookupHRegRemap(m, insn->variant.dfp_convert.dst_hi);
      if (! hregIsInvalid(insn->variant.dfp_convert.dst_lo))
         insn->variant.dfp_convert.dst_lo =
            lookupHRegRemap(m, insn->variant.dfp_convert.dst_lo);
      insn->variant.dfp_convert.op_hi =
         lookupHRegRemap(m, insn->variant.dfp_convert.op_hi);
      if (! hregIsInvalid(insn->variant.dfp_convert.op_lo))
         insn->variant.dfp_convert.op_lo =
            lookupHRegRemap(m, insn->variant.dfp_convert.op_lo);
      break;

   case S390_INSN_DFP_REROUND:
      insn->variant.dfp_reround.dst_hi =
         lookupHRegRemap(m, insn->variant.dfp_reround.dst_hi);
      insn->variant.dfp_reround.op2    =
         lookupHRegRemap(m, insn->variant.dfp_reround.op2);
      insn->variant.dfp_reround.op3_hi =
         lookupHRegRemap(m, insn->variant.dfp_reround.op3_hi);
      if (insn->size == 16) {
         insn->variant.dfp_reround.dst_lo =
            lookupHRegRemap(m, insn->variant.dfp_reround.dst_lo);
         insn->variant.dfp_reround.op3_lo =
            lookupHRegRemap(m, insn->variant.dfp_reround.op3_lo);
      }
      break;

   case S390_INSN_FP_CONVERT: {
      s390_fp_convert *fp_convert = insn->variant.fp_convert.details;

      fp_convert->dst_hi = lookupHRegRemap(m, fp_convert->dst_hi);
      if (! hregIsInvalid(fp_convert->dst_lo))
         fp_convert->dst_lo = lookupHRegRemap(m, fp_convert->dst_lo);
      fp_convert->op_hi = lookupHRegRemap(m, fp_convert->op_hi);
      if (! hregIsInvalid(fp_convert->op_lo))
         fp_convert->op_lo = lookupHRegRemap(m, fp_convert->op_lo);
      fp_convert->r1 = lookupHRegRemap(m, fp_convert->r1);
      break;
   }

   case S390_INSN_MIMM:
      s390_amode_map_regs(m, insn->variant.mimm.dst);
      break;

   case S390_INSN_MADD:
      s390_amode_map_regs(m, insn->variant.madd.dst);
      break;

   case S390_INSN_MFENCE:
      break;

   case S390_INSN_SET_FPC_BFPRM:
      insn->variant.set_fpc_bfprm.mode =
         lookupHRegRemap(m, insn->variant.set_fpc_bfprm.mode);
      break;

   case S390_INSN_SET_FPC_DFPRM:
      insn->variant.set_fpc_dfprm.mode =
         lookupHRegRemap(m, insn->variant.set_fpc_dfprm.mode);
      break;

   case S390_INSN_EVCHECK:
      s390_amode_map_regs(m, insn->variant.evcheck.counter);
      s390_amode_map_regs(m, insn->variant.evcheck.fail_addr);
      break;

   case S390_INSN_PROFINC:
      /* Does not use any register visible to the register allocator */
      break;

   case S390_INSN_XDIRECT:
      s390_amode_map_regs(m, insn->variant.xdirect.guest_IA);
      break;

   case S390_INSN_XINDIR:
      s390_amode_map_regs(m, insn->variant.xindir.guest_IA);
      insn->variant.xindir.dst =
         lookupHRegRemap(m, insn->variant.xindir.dst);
      break;

   case S390_INSN_XASSISTED:
      s390_amode_map_regs(m, insn->variant.xassisted.guest_IA);
      insn->variant.xassisted.dst =
         lookupHRegRemap(m, insn->variant.xassisted.dst);
      break;

   case S390_INSN_VEC_AMODEOP:
      insn->variant.vec_amodeop.dst =
         lookupHRegRemap(m, insn->variant.vec_amodeop.dst);
      insn->variant.vec_amodeop.op1 =
         lookupHRegRemap(m, insn->variant.vec_amodeop.op1);
      s390_amode_map_regs(m, insn->variant.vec_amodeop.op2);
      break;

   case S390_INSN_VEC_AMODEINTOP:
      insn->variant.vec_amodeintop.dst =
         lookupHRegRemap(m, insn->variant.vec_amodeintop.dst);
      s390_amode_map_regs(m, insn->variant.vec_amodeintop.op2);
      insn->variant.vec_amodeintop.op3 =
         lookupHRegRemap(m, insn->variant.vec_amodeintop.op3);
      break;

   case S390_INSN_VEC_BINOP:
      insn->variant.vec_binop.dst =
         lookupHRegRemap(m, insn->variant.vec_binop.dst);
      insn->variant.vec_binop.op1 =
         lookupHRegRemap(m, insn->variant.vec_binop.op1);
      insn->variant.vec_binop.op2 =
         lookupHRegRemap(m, insn->variant.vec_binop.op2);
      break;

   case S390_INSN_VEC_TRIOP:
      insn->variant.vec_triop.dst =
         lookupHRegRemap(m, insn->variant.vec_triop.dst);
      insn->variant.vec_triop.op1 =
         lookupHRegRemap(m, insn->variant.vec_triop.op1);
      insn->variant.vec_triop.op2 =
         lookupHRegRemap(m, insn->variant.vec_triop.op2);
      insn->variant.vec_triop.op3 =
         lookupHRegRemap(m, insn->variant.vec_triop.op3);
      break;

   case S390_INSN_VEC_REPLICATE:
      insn->variant.vec_replicate.dst =
         lookupHRegRemap(m, insn->variant.vec_replicate.dst);
      insn->variant.vec_replicate.op1 =
         lookupHRegRemap(m, insn->variant.vec_replicate.op1);
      break;

   default:
      vpanic("s390_insn_map_regs");
   }
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
emit_RIE(UChar *p, ULong op, UChar r1, UShort i2, UChar m3)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 36;
   the_insn |= ((ULong)m3) << 32;
   the_insn |= ((ULong)i2) << 16;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_RIEf(UChar *p, ULong op, UChar r1, UChar r2,
          UChar i3, Char i4, UChar i5)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r1) << 36;
   the_insn |= ((ULong)r2) << 32;
   the_insn |= ((ULong)i3) << 24;
   the_insn |= ((ULong)i4) << 16;
   the_insn |= ((ULong)i5) << 8;

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
emit_RRF2(UChar *p, UInt op, UChar m3, UChar m4, UChar r1, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)m3) << 12;
   the_insn |= ((ULong)m4) << 8;
   the_insn |= ((ULong)r1) << 4;
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
emit_RRF4(UChar *p, UInt op, UChar r3, UChar m4, UChar r1, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r3) << 12;
   the_insn |= ((ULong)m4) << 8;
   the_insn |= ((ULong)r1) << 4;
   the_insn |= ((ULong)r2) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_RRF5(UChar *p, UInt op, UChar m4, UChar r1, UChar r2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)m4) << 8;
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
emit_RXF(UChar *p, ULong op, UChar r3, UChar x2, UChar b2, UShort d2, UChar r1)
{
   ULong the_insn = op;

   the_insn |= ((ULong)r3) << 36;
   the_insn |= ((ULong)x2) << 32;
   the_insn |= ((ULong)b2) << 28;
   the_insn |= ((ULong)d2) << 16;
   the_insn |= ((ULong)r1) << 12;

   return emit_6bytes(p, the_insn);
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


static UChar *
emit_SI(UChar *p, UInt op, UChar i2, UChar b1, UShort d1)
{
   ULong the_insn = op;

   the_insn |= ((ULong)i2) << 16;
   the_insn |= ((ULong)b1) << 12;
   the_insn |= ((ULong)d1) << 0;

   return emit_4bytes(p, the_insn);
}


static UChar *
emit_SIL(UChar *p, ULong op, UChar b1, UShort d1, UShort i2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)b1) << 28;
   the_insn |= ((ULong)d1) << 16;
   the_insn |= ((ULong)i2) << 0;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_SIY(UChar *p, ULong op, UChar i2, UChar b1, UShort dl1, UChar dh1)
{
   ULong the_insn = op;

   the_insn |= ((ULong)i2) << 32;
   the_insn |= ((ULong)b1) << 28;
   the_insn |= ((ULong)dl1) << 16;
   the_insn |= ((ULong)dh1) << 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_SSa(UChar *p, ULong op, UChar l, UChar b1, UShort d1, UChar b2, UShort d2)
{
   ULong the_insn = op;

   the_insn |= ((ULong)l)  << 32;
   the_insn |= ((ULong)b1) << 28;
   the_insn |= ((ULong)d1) << 16;
   the_insn |= ((ULong)b2) << 12;
   the_insn |= ((ULong)d2) << 0;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRI_VI(UChar *p, ULong op, UChar v1, UShort i2)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)i2) << 16;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRI_VIM(UChar *p, ULong op, UChar v1, UShort i2, UChar m3)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)i2) << 16;
   the_insn |= ((ULong)m3) << 12;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRI_VVMM(UChar *p, ULong op, UChar v1, UChar v3, UShort i2, UChar m4)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v3);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v3) << 32;
   the_insn |= ((ULong)i2) << 16;
   the_insn |= ((ULong)m4) << 12;
   the_insn |= ((ULong)rxb) << 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRX(UChar *p, ULong op, UChar v1, UChar x2, UChar b2, UShort d2, UChar m3)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)x2) << 32;
   the_insn |= ((ULong)b2) << 28;
   the_insn |= ((ULong)d2) << 16;
   the_insn |= ((ULong)m3) << 12;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRS(UChar *p, ULong op, UChar reg1, UChar b2, UShort d2, UChar reg3, UChar m4)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &reg1);
   rxb = s390_update_rxb(rxb, 2, &reg3);

   the_insn |= ((ULong)reg1) << 36;
   the_insn |= ((ULong)reg3) << 32;
   the_insn |= ((ULong)b2)   << 28;
   the_insn |= ((ULong)d2)   << 16;
   the_insn |= ((ULong)m4)   << 12;
   the_insn |= ((ULong)rxb)  << 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRR_VVM(UChar *p, ULong op, UChar v1, UChar v2, UChar m4)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v2);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v2) << 32;
   the_insn |= ((ULong)m4) << 12;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}

static UChar *
emit_VRR_VVMMM(UChar *p, ULong op, UChar v1, UChar v2, UChar m3, UChar m4,
               UChar m5)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v2);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v2) << 32;
   the_insn |= ((ULong)m5) << 20;
   the_insn |= ((ULong)m4) << 16;
   the_insn |= ((ULong)m3) << 12;
   the_insn |= ((ULong)rxb) << 8;

   return emit_6bytes(p, the_insn);
}

static UChar *
emit_VRR_VVVM(UChar *p, ULong op, UChar v1, UChar v2, UChar v3, UChar m4)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v2);
   rxb = s390_update_rxb(rxb, 3, &v3);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v2) << 32;
   the_insn |= ((ULong)v3) << 28;
   the_insn |= ((ULong)m4) << 12;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}


static UChar *
emit_VRR_VVV(UChar *p, ULong op, UChar v1, UChar v2, UChar v3)
{
   return emit_VRR_VVVM(p, op, v1, v2, v3, 0);
}


static UChar *
emit_VRR_VV(UChar *p, ULong op, UChar v1, UChar v2)
{
   return emit_VRR_VVM(p, op, v1, v2, 0);
}


static UChar *
emit_VRR_VVVV(UChar *p, ULong op, UChar v1, UChar v2, UChar v3, UChar v4)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v2);
   rxb = s390_update_rxb(rxb, 3, &v3);
   rxb = s390_update_rxb(rxb, 4, &v4);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v2) << 32;
   the_insn |= ((ULong)v3) << 28;
   the_insn |= ((ULong)v4) << 12;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}

static UChar *
emit_VRRe_VVVVMM(UChar *p, ULong op, UChar v1, UChar v2, UChar v3, UChar v4,
                 UChar m5, UChar m6)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v2);
   rxb = s390_update_rxb(rxb, 3, &v3);
   rxb = s390_update_rxb(rxb, 4, &v4);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v2) << 32;
   the_insn |= ((ULong)v3) << 28;
   the_insn |= ((ULong)m6) << 24;
   the_insn |= ((ULong)m5) << 16;
   the_insn |= ((ULong)v4) << 12;
   the_insn |= ((ULong)rxb) << 8;

   return emit_6bytes(p, the_insn);
}

static UChar *
emit_VRR_VRR(UChar *p, ULong op, UChar v1, UChar r2, UChar r3)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)r2) << 32;
   the_insn |= ((ULong)r3) << 28;
   the_insn |= ((ULong)rxb)<< 8;

   return emit_6bytes(p, the_insn);
}

static UChar *
emit_VRR_VVVMMM(UChar *p, ULong op, UChar v1, UChar v2, UChar v3, UChar m4,
                UChar m5, UChar m6)
{
   ULong the_insn = op;
   ULong rxb = s390_update_rxb(0, 1, &v1);
   rxb = s390_update_rxb(rxb, 2, &v2);
   rxb = s390_update_rxb(rxb, 3, &v3);

   the_insn |= ((ULong)v1) << 36;
   the_insn |= ((ULong)v2) << 32;
   the_insn |= ((ULong)v3) << 28;
   the_insn |= ((ULong)m6) << 20;
   the_insn |= ((ULong)m5) << 16;
   the_insn |= ((ULong)m4) << 12;
   the_insn |= ((ULong)rxb) << 8;

   return emit_6bytes(p, the_insn);
}

static UChar*
emit_VRR_VVVMM(UChar *p, ULong op, UChar v1, UChar v2, UChar v3, UChar m4,
               UChar m5)
{
   return emit_VRR_VVVMMM(p, op, v1, v2, v3, m4, m5, 0);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ay", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000005aULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_AG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_AGSI(UChar *p, UChar i2, UChar b1, UShort dl1, UChar dh1)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, SDXB, INT), "agsi", dh1, dl1, 0, b1, (Int)(Char)i2);

   return emit_SIY(p, 0xeb000000007aULL, i2, b1, dl1, dh1);
}


static UChar *
s390_emit_ASI(UChar *p, UChar i2, UChar b1, UShort dl1, UChar dh1)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, SDXB, INT), "asi", dh1, dl1, 0, b1, (Int)(Char)i2);

   return emit_SIY(p, 0xeb000000006aULL, i2, b1, dl1, dh1);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ny", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000054ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_NG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_TM(UChar *p, UChar i2, UChar b1, UShort d1)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDXB, INT), "tm", d1, 0, b1, i2);

   return emit_SI(p, 0x91000000, i2, b1, d1);
}


static UChar *
s390_emit_TMY(UChar *p, UChar i2, UChar b1, UShort dl1, UChar dh1)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, SDXB, INT), "tmy", dh1, dl1, 0, b1, (Int)(Char)i2);

   return emit_SIY(p, 0xeb0000000051ULL, i2, b1, dl1, dh1);
}


static UChar *
s390_emit_TMLL(UChar *p, UChar r1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "tmll", r1, i2);

   return emit_RI(p, 0xa7010000, r1, i2);
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
s390_emit_BRCL(UChar *p, UChar r1, ULong i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(XMNM, PCREL), S390_XMNM_BRCL, r1, i2);

   return emit_RIL(p, 0xc00400000000ULL, r1, i2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "cy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000059ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_CG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_CGFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, INT), "cgfi", r1, i2);

   return emit_RIL(p, 0xc20c00000000ULL, r1, i2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "csy", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb0000000014ULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_CSG(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "csg", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb0000000030ULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_CDS(UChar *p, UChar r1, UChar r3, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, UDXB), "cds", r1, r3, d2, 0, b2);

   return emit_RS(p, 0xbb000000, r1, r3, b2, d2);
}


static UChar *
s390_emit_CDSY(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "cdsy", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb0000000031ULL, r1, r3, b2, dl2, dh2);
}


static UChar *
s390_emit_CDSG(UChar *p, UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), "cdsg", r1, r3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb000000003eULL, r1, r3, b2, dl2, dh2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "cly", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000055ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_CLG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_CLGFI(UChar *p, UChar r1, UInt i2)
{
   vassert(s390_host_has_eimm);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, UINT), "clgfi", r1, i2);

   return emit_RIL(p, 0xc20e00000000ULL, r1, i2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "dl", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000097ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_DLG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "xy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000057ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_XG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_XC(UChar *p, UInt l, UChar b1, UShort d1, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDLB, UDXB), "xc", d1, l, b1, d2, 0, b2);

   return emit_SSa(p, 0xd70000000000ULL, l, b1, d1, b2, d2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ly", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000058ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lg", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000004ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGF(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lb", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000076ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGB(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "lhy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000078ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LGH(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_MG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "mg", r1, dh2, dl2, x2, b2);

    return emit_RXY(p, 0xe30000000084ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MGRK(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, GPR), "mgrk", r1, r2, r3);

   return emit_RRF3(p, 0xb9ec0000, r3, r1, r2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "ml", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000096ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MLG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "msy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000051ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_MSG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_MVC(UChar *p, UInt l, UChar b1, UShort d1, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDLB, UDXB), "mvc", d1, l, b1, d2, 0, b2);

   return emit_SSa(p, 0xd20000000000ULL, l, b1, d1, b2, d2);
}


static UChar *
s390_emit_MVI(UChar *p, UChar i2, UChar b1, UShort d1)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDXB, INT), "mvi", d1, 0, b1, i2);

   return emit_SI(p, 0x92000000, i2, b1, d1);
}


static UChar *
s390_emit_MVHHI(UChar *p, UChar b1, UShort d1, UShort i2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDXB, INT), "mvhhi", d1, 0, b1, i2);

   return emit_SIL(p, 0xe54400000000ULL, b1, d1, i2);
}


static UChar *
s390_emit_MVHI(UChar *p, UChar b1, UShort d1, UShort i2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDXB, INT), "mvhi", d1, 0, b1, i2);

   return emit_SIL(p, 0xe54c00000000ULL, b1, d1, i2);
}


static UChar *
s390_emit_MVGHI(UChar *p, UChar b1, UShort d1, UShort i2)
{
   vassert(s390_host_has_gie);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, UDXB, INT), "mvghi", d1, 0, b1, i2);

   return emit_SIL(p, 0xe54800000000ULL, b1, d1, i2);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "oy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000056ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_OG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "sty", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe30000000050ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, SDXB), "sy", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xe3000000005bULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_SG(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, SDXB), "ley", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xed0000000064ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_LDY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_SFPC(UChar *p, UChar r1)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC2(MNM, GPR), "sfpc", r1);

   return emit_RRE(p, 0xb3840000, r1, 0);
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
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, SDXB), "stey", r1, dh2, dl2, x2, b2);

   return emit_RXY(p, 0xed0000000066ULL, r1, x2, b2, dl2, dh2);
}


static UChar *
s390_emit_STDY(UChar *p, UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
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
s390_emit_CEFBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cefbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT),
                     "cefbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3940000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDFBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cdfbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT),
                     "cdfbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3950000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXFBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cxfbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT),
                     "cxfbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3960000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CEGBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cegbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT),
                     "cegbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3a40000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDGBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cdgbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT),
                     "cdgbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3a50000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXGBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cxgbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT),
                     "cxgbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3a60000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CELFBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "celfbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3900000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDLFBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cdlfbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3910000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXLFBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cxlfbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3920000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CELGBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "celgbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3a00000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDLGBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cdlgbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3a10000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXLGBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cxlgbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3a20000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLFEBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clfebr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb39c0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLFDBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clfdbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb39d0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLFXBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clfxbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb39e0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLGEBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clgebr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3ac0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLGDBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clgdbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3ad0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLGXBR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clgxbr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3ae0000, m3, m4, r1, r2);
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
s390_emit_LEDBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, FPR), "ledbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT),
                     "ledbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3440000, m3, m4, r1, r2);
}


static UChar *
s390_emit_LDXBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, FPR), "ldxbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT),
                     "ldxbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3450000, m3, m4, r1, r2);
}


static UChar *
s390_emit_LEXBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, FPR), "lexbr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT),
                     "lexbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3460000, m3, m4, r1, r2);
}


static UChar *
s390_emit_FIEBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, UINT, FPR), "fiebr", r1, m3, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT),
                     "fiebra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3570000, m3, m4, r1, r2);
}


static UChar *
s390_emit_FIDBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, UINT, FPR), "fidbr", r1, m3, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT),
                     "fidbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb35f0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_FIXBRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, UINT, FPR), "fixbr", r1, m3, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT),
                     "fixbra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3470000, m3, m4, r1, r2);
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


static UChar *
s390_emit_ADTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "adtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "adtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3d20000, r3, m4, r1, r2);
}


static UChar *
s390_emit_AXTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "axtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "axtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3da0000, r3, m4, r1, r2);
}


static UChar *
s390_emit_CDTR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cdtr", r1, r2);

   return emit_RRE(p, 0xb3e40000, r1, r2);
}


static UChar *
s390_emit_CXTR(UChar *p, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cxtr", r1, r2);

   return emit_RRE(p, 0xb3ec0000, r1, r2);
}


static UChar *
s390_emit_CDGTRA(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0);
   vassert(m3 == 0 || s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m3 == 0)
         s390_disasm(ENC3(MNM, FPR, GPR), "cdgtr", r1, r2);
      else
         s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cdgtra", r1, m3, r2, m4);
   }

   return emit_RRF2(p, 0xb3f10000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXGTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0);
   /* rounding mode m3 is not considered, as the corresponding
      IRop (Iop_I64StoD128) does not take rounding mode. */
   vassert(m3 == 0);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, GPR), "cxgtr", r1, r2);

   return emit_RRF2(p, 0xb3f90000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDFTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cdftr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9510000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXFTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cxftr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9590000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDLFTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cdlftr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9530000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXLFTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cxlftr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb95b0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CDLGTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cdlgtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9520000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CXLGTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), "cxlgtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb95a0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CEDTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cedtr", r1, r2);

   return emit_RRE(p, 0xb3f40000, r1, r2);
}


static UChar *
s390_emit_CEXTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, FPR, FPR), "cextr", r1, r2);

   return emit_RRE(p, 0xb3fc0000, r1, r2);
}


static UChar *
s390_emit_CFDTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "cfdtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9410000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CFXTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "cfxtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9490000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CGDTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0);
   vassert(s390_host_has_fpext || m3 < 1 || m3 > 7);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cgdtr", r1, m3, r2);

   return emit_RRF2(p, 0xb3e10000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CGXTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0);
   vassert(s390_host_has_fpext || m3 < 1 || m3 > 7);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, FPR), "cgxtr", r1, m3, r2);

   return emit_RRF2(p, 0xb3e90000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLFDTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clfdtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9430000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLFXTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clfxtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb94b0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLGDTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clgdtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb9420000, m3, m4, r1, r2);
}


static UChar *
s390_emit_CLGXTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(m4 == 0);
   vassert(s390_host_has_dfp);
   vassert(s390_host_has_fpext);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), "clgxtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb94a0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_DDTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "ddtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "ddtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3d10000, r3, m4, r1, r2);
}


static UChar *
s390_emit_DXTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "dxtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "dxtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3d90000, r3, m4, r1, r2);
}


static UChar *
s390_emit_EEDTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, FPR), "eedtr", r1, r2);

   return emit_RRE(p, 0xb3e50000, r1, r2);
}


static UChar *
s390_emit_EEXTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, FPR), "eextr", r1, r2);

   return emit_RRE(p, 0xb3ed0000, r1, r2);
}


static UChar *
s390_emit_ESDTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, FPR), "esdtr", r1, r2);

   return emit_RRE(p, 0xb3e70000, r1, r2);
}


static UChar *
s390_emit_ESXTR(UChar *p, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, GPR, FPR), "esxtr", r1, r2);

   return emit_RRE(p, 0xb3ef0000, r1, r2);
}


static UChar *
s390_emit_IEDTR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, GPR), "iedtr", r1, r3, r2);

   return emit_RRF(p, 0xb3f60000, r3, r1, r2);
}


static UChar *
s390_emit_IEXTR(UChar *p, UChar r3, UChar r1, UChar r2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, GPR), "iextr", r1, r3, r2);

   return emit_RRF(p, 0xb3fe0000, r3, r1, r2);
}


static UChar *
s390_emit_LDETR(UChar *p, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, UINT), "ldetr", r1, r2, m4);

   return emit_RRF5(p, 0xb3d40000, m4, r1, r2);
}


static UChar *
s390_emit_LXDTR(UChar *p, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, UINT), "lxdtr", r1, r2, m4);

   return emit_RRF5(p, 0xb3dc0000, m4, r1, r2);
}


static UChar *
s390_emit_LEDTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0);
   vassert(s390_host_has_fpext || m3 < 1 || m3 > 7);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT), "ledtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3d50000, m3, m4, r1, r2);
}


static UChar *
s390_emit_LDXTR(UChar *p, UChar m3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0);
   vassert(s390_host_has_fpext || m3 < 1 || m3 > 7);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT), "ldxtr", r1, m3, r2, m4);

   return emit_RRF2(p, 0xb3dd0000, m3, m4, r1, r2);
}


static UChar *
s390_emit_MDTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "mdtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "mdtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3d00000, r3, m4, r1, r2);
}


static UChar *
s390_emit_MXTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "mxtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "mxtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3d80000, r3, m4, r1, r2);
}


static UChar *
emit_E(UChar *p, UInt op)
{
   ULong the_insn = op;

   return emit_2bytes(p, the_insn);
}


static UChar *
s390_emit_PFPO(UChar *p)
{
   vassert(s390_host_has_pfpo);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      s390_disasm(ENC1(MNM), "pfpo");
   }

   return emit_E(p, 0x010a);
}


static UChar *
s390_emit_QADTR(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "qadtr", r1, r3, r2, m4);

   return emit_RRF4(p, 0xb3f50000, r3, m4, r1, r2);
}


static UChar *
s390_emit_QAXTR(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "qaxtr", r1, r3, r2, m4);

   return emit_RRF4(p, 0xb3fd0000, r3, m4, r1, r2);
}


static UChar *
s390_emit_RRDTR(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, FPR, GPR, UINT), "rrdtr", r1, r3, r2, m4);

   return emit_RRF4(p, 0xb3f70000, r3, m4, r1, r2);
}


static UChar *
s390_emit_RRXTR(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, FPR, FPR, GPR, UINT), "rrxtr", r1, r3, r2, m4);

   return emit_RRF4(p, 0xb3ff0000, r3, m4, r1, r2);
}


static UChar *
s390_emit_SDTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "sdtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "sdtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3d30000, r3, m4, r1, r2);
}


static UChar *
s390_emit_SXTRA(UChar *p, UChar r3, UChar m4, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   vassert(m4 == 0 || s390_host_has_fpext);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
      if (m4 == 0)
         s390_disasm(ENC4(MNM, FPR, FPR, FPR), "sxtr", r1, r2, r3);
      else
         s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), "sxtra", r1, r2, r3, m4);
   }

   return emit_RRF4(p, 0xb3db0000, r3, m4, r1, r2);
}


static UChar *
s390_emit_SLDT(UChar *p, UChar r3, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, UDXB), "sldt", r1, r3, 0, 0, r2);

   return emit_RXF(p, 0xED0000000040ULL, r3, 0, r2, 0, r1);
}


static UChar *
s390_emit_SLXT(UChar *p, UChar r3, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, UDXB), "slxt", r1, r3, 0, 0, r2);

   return emit_RXF(p, 0xED0000000048ULL, r3, 0, r2, 0, r1);
}


static UChar *
s390_emit_SRDT(UChar *p, UChar r3, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, UDXB), "srdt", r1, r3, 0, 0, r2);

   return emit_RXF(p, 0xED0000000041ULL, r3, 0, r2, 0, r1);
}


static UChar *
s390_emit_SRXT(UChar *p, UChar r3, UChar r1, UChar r2)
{
   vassert(s390_host_has_dfp);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, FPR, FPR, UDXB), "srxt", r1, r3, 0, 0, r2);

   return emit_RXF(p, 0xED0000000049ULL, r3, 0, r2, 0, r1);
}


static UChar *
s390_emit_LOCGR(UChar *p, UChar m3, UChar r1, UChar r2)
{
   vassert(s390_host_has_lsc);
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, GPR, UINT), "locgr", r1, r2, m3);

   return emit_RRF3(p, 0xb9e20000, m3, r1, r2);
}


static UChar *
s390_emit_LOC(UChar *p, UChar r1, UChar m3, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, SDXB), "loc", r1, m3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb00000000f2ULL, r1, m3, b2, dl2, dh2);
}


static UChar *
s390_emit_LOCG(UChar *p, UChar r1, UChar m3, UChar b2, UShort dl2, UChar dh2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, UINT, SDXB), "locg", r1, m3, dh2, dl2, 0, b2);

   return emit_RSY(p, 0xeb00000000e2ULL, r1, m3, b2, dl2, dh2);
}

static UChar *
s390_emit_LOCGHI(UChar *p, UChar r1, UShort i2, UChar m3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, GPR, INT, UINT), "locghi", r1, (Int)(Short)i2, m3);

   return emit_RIE(p, 0xec0000000046ULL, r1, i2, m3);
}

static UChar *
s390_emit_RISBG(UChar *p, UChar r1, UChar r2, UChar i3, Char i4, UChar i5)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, GPR, GPR, UINT, UINT, UINT),
                  "risbg", r1, r2, i3, i4, i5);

   return emit_RIEf(p, 0xec0000000055ULL, r1, r2, i3, i4, i5);
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
   UInt v = val & 0xFFFFu;

   /* sign extend */
   v = (Int)(v << 16) >> 16;

   return val == v;
}


static __inline__ Bool
ulong_fits_signed_16bit(ULong val)
{
   ULong v = val & 0xFFFFu;

   /* sign extend */
   v = (Long)(v << 48) >> 48;

   return val == v;
}


static __inline__ Bool
ulong_fits_signed_32bit(ULong val)
{
   ULong v = val & 0xFFFFFFFFu;

   /* sign extend */
   v = (Long)(v << 32) >> 32;

   return val == v;
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
   p = s390_emit_STDY(p, r2, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
   return s390_emit_LG(p, r1, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
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
   p = s390_emit_STG(p, r2, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
   return s390_emit_LDY(p, r1, R0, S390_REGNO_STACK_POINTER, DISP20(-8));
}


static UChar *
s390_emit_VL(UChar *p, UChar v1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, VR, UDXB), "vl", v1, d2, x2, b2);

   return emit_VRX(p, 0xE70000000006ULL, v1, x2, b2, d2, 0);
}

static UChar *
s390_emit_VLR(UChar *p, UChar v1, UChar v2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, VR, UDXB), "vlr", v1, v2);

   return emit_VRR_VV(p, 0xE70000000056ULL, v1, v2);
}


static UChar *
s390_emit_VLREP(UChar *p, UChar v1, UChar x2, UChar b2, UShort d2, UShort m3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, UDXB, UINT), "vlrep", v1, d2, x2, b2, m3);

   return emit_VRX(p, 0xE70000000005ULL, v1, x2, b2, d2, m3);
}


static UChar *
s390_emit_VST(UChar *p, UChar v1, UChar x2, UChar b2, UShort d2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, VR, UDXB), "vst", v1, d2, x2, b2);

   return emit_VRX(p, 0xE7000000000eULL, v1, x2, b2, d2, 0);
}


static UChar *
s390_emit_VLGV(UChar *p, UChar r1, UChar b2, UShort d2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, GPR, UDXB, VR, UINT), "vlgv", r1, d2, 0, b2, v3, m4);

   return emit_VRS(p, 0xE70000000021ULL, r1, b2, d2, v3, m4);
}


static UChar *
s390_emit_VLVG(UChar *p, UChar v1, UChar b2, UShort d2, UChar r3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, UDXB, GPR, UINT), "vlvg", v1, d2, 0, b2, r3, m4);

   return emit_VRS(p, 0xE70000000022ULL, v1, b2, d2, r3, m4);
}


static UChar *
s390_emit_VPERM(UChar *p, UChar v1, UChar v2, UChar v3, UChar v4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, VR), "vperm", v1, v2, v3, v4);

   return emit_VRR_VVVV(p, 0xE7000000008cULL, v1, v2, v3, v4);
}

static UChar *
s390_emit_VO(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vo", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000006aULL, v1, v2, v3);
}

static UChar *
s390_emit_VOC(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "voc", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000006fULL, v1, v2, v3);
}

static UChar *
s390_emit_VX(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vx", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000006dULL, v1, v2, v3);
}

static UChar *
s390_emit_VN(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vn", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE70000000068ULL, v1, v2, v3);
}

static UChar*
s390_emit_VCEQ(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vceq", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000f8ULL, v1, v2, v3, m4);
}


static UChar *
s390_emit_VGBM(UChar *p, UChar v1, UShort i2)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC3(MNM, VR, UINT), "vgbm", v1, i2);

   return emit_VRI_VI(p, 0xE70000000044ULL, v1, i2);
}


static UChar *
s390_emit_VPK(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vpk", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000094ULL, v1, v2, v3, m4);
}


static UChar *
s390_emit_VPKS(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), "vpks", v1, v2, v3, m4, 0);

   return emit_VRR_VVVM(p, 0xE70000000097ULL, v1, v2, v3, m4);
}


static UChar *
s390_emit_VPKLS(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), "vpkls", v1, v2, v3, m4, 0);

   return emit_VRR_VVVM(p, 0xE70000000095ULL, v1, v2, v3, m4);
}


static UChar *
s390_emit_VREP(UChar *p, UChar v1, UChar v3, UShort i2, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, UINT, UINT), "vrep", v1, v3, i2, m4);

   return emit_VRI_VVMM(p, 0xE7000000004DULL, v1, v3, i2, m4);
}


static UChar *
s390_emit_VREPI(UChar *p, UChar v1, UShort i2, UChar m3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, UINT, UINT), "vrepi", v1, i2, m3);

   return emit_VRI_VIM(p, 0xE70000000045ULL, v1, i2, m3);
}


static UChar *
s390_emit_VUPH(UChar *p, UChar v1, UChar v3, UChar m3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, UINT), "vuph", v1, v3, m3);

   return emit_VRR_VVM(p, 0xE700000000D7ULL, v1, v3, m3);
}


static UChar *
s390_emit_VUPLH(UChar *p, UChar v1, UChar v3, UChar m3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, UINT), "vuplh", v1, v3, m3);

   return emit_VRR_VVM(p, 0xE700000000D5ULL, v1, v3, m3);
}


static UChar*
s390_emit_VMRH(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmrh", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000061ULL, v1, v2, v3, m4);
}


static UChar*
s390_emit_VMRL(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmrl", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000060ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VA(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "va", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000f3ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VS(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vs", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000f7ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VNO(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vno", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000006bULL, v1, v2, v3);
}

static UChar *
s390_emit_VCH(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vch", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000fbULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VCHL(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vchl", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000f9ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VCLZ(UChar *p, UChar v1, UChar v2, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, UINT), "vclz", v1, v2, m4);

   return emit_VRR_VVM(p, 0xE70000000053ULL, v1, v2, m4);
}

static UChar *
s390_emit_VCTZ(UChar *p, UChar v1, UChar v2, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, UINT), "vctz", v1, v2, m4);

   return emit_VRR_VVM(p, 0xE70000000052ULL, v1, v2, m4);
}

static UChar *
s390_emit_VPOPCT(UChar *p, UChar v1, UChar v2, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, UINT), "vpopct", v1, v2, m4);

   return emit_VRR_VVM(p, 0xE70000000050ULL, v1, v2, m4);
}

static UChar *
s390_emit_VMX(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmx", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000ffULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VMXL(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmxl", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000fdULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VMN(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmn", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000feULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VMNL(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmnl", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000fcULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VAVG(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vavg", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000f2ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VAVGL(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vavgl", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000f0ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VLP(UChar *p, UChar v1, UChar v2, UChar m3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, UINT), "vlp", v1, v2, m3);

   return emit_VRR_VVM(p, 0xE700000000DFULL, v1, v2, m3);
}

static UChar *
s390_emit_VMH(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmh", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000a3ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VMLH(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmlh", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000a1ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VML(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vml", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000a2ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VME(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vme", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000a6ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VMLE(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vmle", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE700000000a4ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VESLV(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "veslv", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000070ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VESRAV(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vesrav", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE7000000007aULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VESRLV(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vesrlv", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000078ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VESL(UChar *p, UChar v1, UChar b2, UShort d2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, UDXB, VR, UINT), "vesl", v1, d2, 0, b2, v3, m4);

   return emit_VRS(p, 0xE70000000030ULL, v1, b2, d2, v3, m4);
}

static UChar *
s390_emit_VESRA(UChar *p, UChar v1, UChar b2, UShort d2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, UDXB, VR, UINT), "vesra", v1, d2, 0, b2, v3, m4);

   return emit_VRS(p, 0xE7000000003aULL, v1, b2, d2, v3, m4);
}

static UChar *
s390_emit_VESRL(UChar *p, UChar v1, UChar b2, UShort d2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, UDXB, VR, UINT), "vesrl", v1, d2, 0, b2, v3, m4);

   return emit_VRS(p, 0xE70000000038ULL, v1, b2, d2, v3, m4);
}

static UChar *
s390_emit_VERLLV(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "verllv", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000073ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VSL(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vsl", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE70000000074ULL, v1, v2, v3);
}

static UChar *
s390_emit_VSRL(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vsrl", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000007cULL, v1, v2, v3);
}

static UChar *
s390_emit_VSRA(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vsra", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000007eULL, v1, v2, v3);
}

static UChar *
s390_emit_VSLB(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vslb", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE70000000075ULL, v1, v2, v3);
}

static UChar *
s390_emit_VSRLB(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vsrlb", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000007dULL, v1, v2, v3);
}

static UChar *
s390_emit_VSRAB(UChar *p, UChar v1, UChar v2, UChar v3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, VR, VR), "vsrab", v1, v2, v3);

   return emit_VRR_VVV(p, 0xE7000000007fULL, v1, v2, v3);
}

static UChar *
s390_emit_VSUM(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vsum", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000064ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VSUMG(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vsumg", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000065ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VSUMQ(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), "vsumq", v1, v2, v3, m4);

   return emit_VRR_VVVM(p, 0xE70000000067ULL, v1, v2, v3, m4);
}

static UChar *
s390_emit_VLVGP(UChar *p, UChar v1, UChar r2, UChar r3)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC4(MNM, VR, GPR, GPR), "vlvgp", v1, r2, r3);

   return emit_VRR_VRR(p, 0xE70000000062ULL, v1, r2, r3);
}

static UChar *
s390_emit_VFPSO(UChar *p, UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, UINT, UINT, UINT), "vfpso", v1, v2, m3, m4,
                  m5);

   return emit_VRR_VVMMM(p, 0xE700000000CCULL, v1, v2, m3, m4, m5);
}

static UChar *
s390_emit_VFA(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), "vfa", v1, v2, v3, m4, m5);

   return emit_VRR_VVVMM(p, 0xE700000000e3ULL, v1, v2, v3, m4, m5);
}

static UChar *
s390_emit_VFS(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), "vfs", v1, v2, v3, m4, m5);

   return emit_VRR_VVVMM(p, 0xE700000000e2ULL, v1, v2, v3, m4, m5);
}

static UChar *
s390_emit_VFM(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), "vfm", v1, v2, v3, m4, m5);

   return emit_VRR_VVVMM(p, 0xE700000000e7ULL, v1, v2, v3, m4, m5);
}

static UChar *
s390_emit_VFD(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), "vfd", v1, v2, v3, m4, m5);

   return emit_VRR_VVVMM(p, 0xE700000000e5ULL, v1, v2, v3, m4, m5);
}

static UChar *
s390_emit_VFSQ(UChar *p, UChar v1, UChar v2, UChar m3, UChar m4)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC5(MNM, VR, VR, UINT, UINT), "vfsq", v1, v2, m3, m4);

   return emit_VRR_VVMMM(p, 0xE700000000CEULL, v1, v2, m3, m4, 0);
}

static UChar *
s390_emit_VFMA(UChar *p, UChar v1, UChar v2, UChar v3, UChar v4, UChar m5,
               UChar m6)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC7(MNM, VR, VR, VR, VR, UINT, UINT), "vfma",
                  v1, v2, v3, v4, m5, m6);

   return emit_VRRe_VVVVMM(p, 0xE7000000008fULL, v1, v2, v3, v4, m5, m6);
}

static UChar *
s390_emit_VFMS(UChar *p, UChar v1, UChar v2, UChar v3, UChar v4, UChar m5,
               UChar m6)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC7(MNM, VR, VR, VR, VR, UINT, UINT), "vfms",
                  v1, v2, v3, v4, m5, m6);

   return emit_VRRe_VVVVMM(p, 0xE7000000008eULL, v1, v2, v3, v4, m5, m6);
}

static UChar *
s390_emit_VFCE(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5,
               UChar m6)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC7(MNM, VR, VR, VR, UINT, UINT, UINT), "vfce",
                  v1, v2, v3, m4, m5, m6);

   return emit_VRR_VVVMMM(p, 0xE700000000e8ULL, v1, v2, v3, m4, m5, m6);
}

static UChar *
s390_emit_VFCH(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5,
               UChar m6)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC7(MNM, VR, VR, VR, UINT, UINT, UINT), "vfch",
                  v1, v2, v3, m4, m5, m6);

   return emit_VRR_VVVMMM(p, 0xE700000000ebULL, v1, v2, v3, m4, m5, m6);
}

static UChar *
s390_emit_VFCHE(UChar *p, UChar v1, UChar v2, UChar v3, UChar m4, UChar m5,
                UChar m6)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM))
      s390_disasm(ENC7(MNM, VR, VR, VR, UINT, UINT, UINT), "vfche",
                  v1, v2, v3, m4, m5, m6);

   return emit_VRR_VVVMMM(p, 0xE700000000eaULL, v1, v2, v3, m4, m5, m6);
}

/*---------------------------------------------------------------*/
/*--- Constructors for the various s390_insn kinds            ---*/
/*---------------------------------------------------------------*/

s390_insn *
s390_insn_load(UChar size, HReg dst, s390_amode *src)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_LOAD;
   insn->size = size;
   insn->variant.load.src  = src;
   insn->variant.load.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8 || size == 16);

   return insn;
}


s390_insn *
s390_insn_store(UChar size, s390_amode *dst, HReg src)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_STORE;
   insn->size = size;
   insn->variant.store.src  = src;
   insn->variant.store.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8 || size == 16);

   return insn;
}


s390_insn *
s390_insn_move(UChar size, HReg dst, HReg src)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_MOVE;
   insn->size = size;
   insn->variant.move.src  = src;
   insn->variant.move.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8 || size ==16);

   return insn;
}


s390_insn *
s390_insn_memcpy(UChar size, s390_amode *dst, s390_amode *src)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   /* This insn will be mapped to MVC which requires base register
      plus 12-bit displacement */
   vassert(src->tag == S390_AMODE_B12);
   vassert(dst->tag == S390_AMODE_B12);

   insn->tag  = S390_INSN_MEMCPY;
   insn->size = size;
   insn->variant.memcpy.src = src;
   insn->variant.memcpy.dst = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8 || size == 16);

   return insn;
}


s390_insn *
s390_insn_cond_move(UChar size, s390_cc_t cond, HReg dst, s390_opnd_RMI src)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_COND_MOVE;
   insn->size = size;
   insn->variant.cond_move.cond = cond;
   insn->variant.cond_move.src  = src;
   insn->variant.cond_move.dst  = dst;

   vassert(size == 1 || size == 2 || size == 4 || size == 8 || size == 16);

   return insn;
}


s390_insn *
s390_insn_load_immediate(UChar size, HReg dst, ULong value)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_LOAD_IMMEDIATE;
   insn->size = size;
   insn->variant.load_immediate.dst   = dst;
   insn->variant.load_immediate.value = value;

   return insn;
}


s390_insn *
s390_insn_alu(UChar size, s390_alu_t tag, HReg dst, s390_opnd_RMI op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

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
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(! hregIsVirtual(dst_hi));
   vassert(! hregIsVirtual(dst_lo));

   insn->tag  = signed_multiply ? S390_INSN_SMUL : S390_INSN_UMUL;
   insn->size = size;
   insn->variant.mul.dst_hi = dst_hi;
   insn->variant.mul.dst_lo = dst_lo;
   insn->variant.mul.op2 = op2;

   return insn;
}


s390_insn *
s390_insn_div(UChar size, HReg op1_hi, HReg op1_lo, s390_opnd_RMI op2,
              Bool signed_divide)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);
   vassert(! hregIsVirtual(op1_hi));
   vassert(! hregIsVirtual(op1_lo));

   insn->tag  = signed_divide ? S390_INSN_SDIV : S390_INSN_UDIV;
   insn->size = size;
   insn->variant.div.op1_hi = op1_hi;
   insn->variant.div.op1_lo = op1_lo;
   insn->variant.div.op2 = op2;

   return insn;
}


s390_insn *
s390_insn_divs(UChar size, HReg rem, HReg op1, s390_opnd_RMI op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

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
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

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
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

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
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   insn->tag  = S390_INSN_TEST;
   insn->size = size;
   insn->variant.test.src = src;

   return insn;
}


s390_insn *
s390_insn_cc2bool(HReg dst, s390_cc_t cond)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_CC2BOOL;
   insn->size = 0;   /* does not matter */
   insn->variant.cc2bool.cond = cond;
   insn->variant.cc2bool.dst  = dst;

   return insn;
}


s390_insn *
s390_insn_cas(UChar size, HReg op1, s390_amode *op2, HReg op3, HReg old_mem)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);
   vassert(hregNumber(op2->x) == 0);
   vassert(op2->tag == S390_AMODE_B12 || op2->tag == S390_AMODE_B20);

   insn->tag  = S390_INSN_CAS;
   insn->size = size;
   insn->variant.cas.op1 = op1;
   insn->variant.cas.op2 = op2;
   insn->variant.cas.op3 = op3;
   insn->variant.cas.old_mem = old_mem;

   return insn;
}


s390_insn *
s390_insn_cdas(UChar size, HReg op1_high, HReg op1_low, s390_amode *op2,
               HReg op3_high, HReg op3_low, HReg old_mem_high, HReg old_mem_low,
               HReg scratch)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));
   s390_cdas *cdas = LibVEX_Alloc_inline(sizeof(s390_cdas));

   vassert(size == 4 || size == 8);
   vassert(hregNumber(op2->x) == 0);
   vassert(hregNumber(scratch) == 1);  /* r0,r1 used as scratch reg pair */
   vassert(op2->tag == S390_AMODE_B12 || op2->tag == S390_AMODE_B20);

   insn->tag  = S390_INSN_CDAS;
   insn->size = size;
   insn->variant.cdas.details = cdas;

   cdas->op1_high = op1_high;
   cdas->op1_low  = op1_low;
   cdas->op2 = op2;
   cdas->op3_high = op3_high;
   cdas->op3_low  = op3_low;
   cdas->old_mem_high = old_mem_high;
   cdas->old_mem_low  = old_mem_low;
   cdas->scratch = scratch;

   return insn;
}


s390_insn *
s390_insn_compare(UChar size, HReg src1, s390_opnd_RMI src2,
                  Bool signed_comparison)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_COMPARE;
   insn->size = size;
   insn->variant.compare.src1 = src1;
   insn->variant.compare.src2 = src2;
   insn->variant.compare.signed_comparison = signed_comparison;

   return insn;
}


s390_insn *
s390_insn_helper_call(s390_cc_t cond, Addr64 target, UInt num_args,
                      const HChar *name, RetLoc rloc)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));
   s390_helper_call *helper_call = LibVEX_Alloc_inline(sizeof(s390_helper_call));

   insn->tag  = S390_INSN_HELPER_CALL;
   insn->size = 0;  /* does not matter */
   insn->variant.helper_call.details = helper_call;

   helper_call->cond = cond;
   helper_call->target = target;
   helper_call->num_args = num_args;
   helper_call->name = name;
   helper_call->rloc = rloc;

   vassert(is_sane_RetLoc(rloc));

   return insn;
}


s390_insn *
s390_insn_bfp_triop(UChar size, s390_bfp_triop_t tag, HReg dst, HReg op2,
                    HReg op3)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_BFP_TRIOP;
   insn->size = size;
   insn->variant.bfp_triop.tag = tag;
   insn->variant.bfp_triop.dst = dst;
   insn->variant.bfp_triop.op2 = op2;
   insn->variant.bfp_triop.op3 = op3;

   return insn;
}


s390_insn *
s390_insn_bfp_binop(UChar size, s390_bfp_binop_t tag, HReg dst, HReg op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_BFP_BINOP;
   insn->size = size;
   insn->variant.bfp_binop.tag = tag;
   insn->variant.bfp_binop.dst_hi = dst;
   insn->variant.bfp_binop.op2_hi = op2;
   insn->variant.bfp_binop.dst_lo = INVALID_HREG;
   insn->variant.bfp_binop.op2_lo = INVALID_HREG;

   return insn;
}


s390_insn *
s390_insn_bfp_unop(UChar size, s390_bfp_unop_t tag, HReg dst, HReg op)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_BFP_UNOP;
   insn->size = size;
   insn->variant.bfp_unop.tag = tag;
   insn->variant.bfp_unop.dst_hi = dst;
   insn->variant.bfp_unop.op_hi  = op;
   insn->variant.bfp_unop.dst_lo = INVALID_HREG;
   insn->variant.bfp_unop.op_lo  = INVALID_HREG;

   return insn;
}


s390_insn *
s390_insn_bfp_compare(UChar size, HReg dst, HReg op1, HReg op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_BFP_COMPARE;
   insn->size = size;
   insn->variant.bfp_compare.dst = dst;
   insn->variant.bfp_compare.op1_hi = op1;
   insn->variant.bfp_compare.op2_hi = op2;
   insn->variant.bfp_compare.op1_lo = INVALID_HREG;
   insn->variant.bfp_compare.op2_lo = INVALID_HREG;

   return insn;
}


s390_insn *
s390_insn_bfp_convert(UChar size, s390_bfp_conv_t tag, HReg dst, HReg op,
                      s390_bfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_BFP_CONVERT;
   insn->size = size;
   insn->variant.bfp_convert.tag = tag;
   insn->variant.bfp_convert.dst_hi = dst;
   insn->variant.bfp_convert.op_hi  = op;
   insn->variant.bfp_convert.dst_lo = INVALID_HREG;
   insn->variant.bfp_convert.op_lo  = INVALID_HREG;
   insn->variant.bfp_convert.rounding_mode = rounding_mode;

   return insn;
}


/* Check validity of a register pair for 128-bit FP. Valid register
   pairs are (0,2), (1,3), (4, 6), (5, 7), (8, 10), (9, 11), (12, 14),
   and (13, 15). */
static Bool
is_valid_fp128_regpair(HReg hi, HReg lo)
{
   UInt hi_regno = hregNumber(hi);
   UInt lo_regno = hregNumber(lo);

   if (lo_regno != hi_regno + 2) return False;
   if ((hi_regno & 0x2) != 0) return False;

   return True;
}

s390_insn *
s390_insn_bfp128_binop(UChar size, s390_bfp_binop_t tag, HReg dst_hi,
                       HReg dst_lo, HReg op2_hi, HReg op2_lo)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
   vassert(is_valid_fp128_regpair(op2_hi, op2_lo));

   insn->tag  = S390_INSN_BFP_BINOP;
   insn->size = size;
   insn->variant.bfp_binop.tag = tag;
   insn->variant.bfp_binop.dst_hi = dst_hi;
   insn->variant.bfp_binop.dst_lo = dst_lo;
   insn->variant.bfp_binop.op2_hi = op2_hi;
   insn->variant.bfp_binop.op2_lo = op2_lo;

   return insn;
}


s390_insn *
s390_insn_bfp128_unop(UChar size, s390_bfp_unop_t tag, HReg dst_hi,
                      HReg dst_lo, HReg op_hi, HReg op_lo)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
   vassert(is_valid_fp128_regpair(op_hi, op_lo));

   insn->tag  = S390_INSN_BFP_UNOP;
   insn->size = size;
   insn->variant.bfp_unop.tag = tag;
   insn->variant.bfp_unop.dst_hi = dst_hi;
   insn->variant.bfp_unop.dst_lo = dst_lo;
   insn->variant.bfp_unop.op_hi = op_hi;
   insn->variant.bfp_unop.op_lo = op_lo;

   return insn;
}


s390_insn *
s390_insn_bfp128_compare(UChar size, HReg dst, HReg op1_hi, HReg op1_lo,
                         HReg op2_hi, HReg op2_lo)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(op1_hi, op1_lo));
   vassert(is_valid_fp128_regpair(op2_hi, op2_lo));

   insn->tag  = S390_INSN_BFP_COMPARE;
   insn->size = size;
   insn->variant.bfp_compare.dst = dst;
   insn->variant.bfp_compare.op1_hi = op1_hi;
   insn->variant.bfp_compare.op1_lo = op1_lo;
   insn->variant.bfp_compare.op2_hi = op2_hi;
   insn->variant.bfp_compare.op2_lo = op2_lo;

   return insn;
}


s390_insn *
s390_insn_bfp128_convert(UChar size, s390_bfp_conv_t tag, HReg dst_hi,
                         HReg dst_lo, HReg op_hi, HReg op_lo,
                         s390_bfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   if (size == 16) {
      /* From smaller or equal size to 16 bytes */
      vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
      vassert(hregIsInvalid(op_lo)
              || is_valid_fp128_regpair(op_hi, op_lo));
   } else {
      /* From 16 bytes to smaller size */
      vassert(is_valid_fp128_regpair(op_hi, op_lo));
   }

   insn->tag  = S390_INSN_BFP_CONVERT;
   insn->size = size;
   insn->variant.bfp_convert.tag = tag;
   insn->variant.bfp_convert.dst_hi = dst_hi;
   insn->variant.bfp_convert.dst_lo = dst_lo;
   insn->variant.bfp_convert.op_hi = op_hi;
   insn->variant.bfp_convert.op_lo = op_lo;
   insn->variant.bfp_convert.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_bfp128_convert_to(UChar size, s390_bfp_conv_t tag, HReg dst_hi,
                            HReg dst_lo, HReg op)
{
   /* Conversion to bfp128 never requires a rounding mode. Provide default
      rounding mode. It will not be used when emitting insns. */
   s390_bfp_round_t rounding_mode = S390_BFP_ROUND_NEAREST_EVEN;

   return s390_insn_bfp128_convert(size, tag, dst_hi, dst_lo, op,
                                   INVALID_HREG, rounding_mode);
}


s390_insn *
s390_insn_bfp128_convert_from(UChar size, s390_bfp_conv_t tag, HReg dst_hi,
                              HReg dst_lo, HReg op_hi, HReg op_lo,
                              s390_bfp_round_t rounding_mode)
{
   return s390_insn_bfp128_convert(size, tag, dst_hi, dst_lo, op_hi, op_lo,
                                   rounding_mode);
}


s390_insn *
s390_insn_dfp_binop(UChar size, s390_dfp_binop_t tag, HReg dst, HReg op2,
                    HReg op3, s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));
   s390_dfp_binop *dfp_binop = LibVEX_Alloc_inline(sizeof(s390_dfp_binop));

   vassert(size == 8);

   insn->tag  = S390_INSN_DFP_BINOP;
   insn->size = size;
   insn->variant.dfp_binop.details = dfp_binop;

   dfp_binop->tag = tag;
   dfp_binop->dst_hi = dst;
   dfp_binop->op2_hi = op2;
   dfp_binop->op3_hi = op3;
   dfp_binop->dst_lo = INVALID_HREG;
   dfp_binop->op2_lo = INVALID_HREG;
   dfp_binop->op3_lo = INVALID_HREG;
   dfp_binop->rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_dfp_unop(UChar size, s390_dfp_unop_t tag, HReg dst, HReg op)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 8);

   insn->tag  = S390_INSN_DFP_UNOP;
   insn->size = size;
   insn->variant.dfp_unop.tag = tag;
   insn->variant.dfp_unop.dst_hi = dst;
   insn->variant.dfp_unop.op_hi  = op;
   insn->variant.dfp_unop.dst_lo = INVALID_HREG;
   insn->variant.dfp_unop.op_lo  = INVALID_HREG;

   return insn;
}


s390_insn *
s390_insn_dfp_intop(UChar size, s390_dfp_intop_t tag, HReg dst, HReg op2,
                    HReg op3)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 8);

   insn->tag  = S390_INSN_DFP_INTOP;
   insn->size = size;
   insn->variant.dfp_intop.tag = tag;
   insn->variant.dfp_intop.dst_hi = dst;
   insn->variant.dfp_intop.op2    = op2;
   insn->variant.dfp_intop.op3_hi = op3;
   insn->variant.dfp_intop.dst_lo = INVALID_HREG;
   insn->variant.dfp_intop.op3_lo = INVALID_HREG;

   return insn;
}


s390_insn *
s390_insn_dfp_compare(UChar size, s390_dfp_cmp_t tag, HReg dst,
                      HReg op1, HReg op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 8);

   insn->tag  = S390_INSN_DFP_COMPARE;
   insn->size = size;
   insn->variant.dfp_compare.tag = tag;
   insn->variant.dfp_compare.dst = dst;
   insn->variant.dfp_compare.op1_hi = op1;
   insn->variant.dfp_compare.op2_hi = op2;
   insn->variant.dfp_compare.op1_lo = INVALID_HREG;
   insn->variant.dfp_compare.op2_lo = INVALID_HREG;

   return insn;
}


s390_insn *
s390_insn_dfp_convert(UChar size, s390_dfp_conv_t tag, HReg dst, HReg op,
                      s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_DFP_CONVERT;
   insn->size = size;
   insn->variant.dfp_convert.tag = tag;
   insn->variant.dfp_convert.dst_hi = dst;
   insn->variant.dfp_convert.op_hi  = op;
   insn->variant.dfp_convert.dst_lo = INVALID_HREG;
   insn->variant.dfp_convert.op_lo  = INVALID_HREG;
   insn->variant.dfp_convert.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_dfp_reround(UChar size, HReg dst, HReg op2, HReg op3,
                      s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 8);

   insn->tag  = S390_INSN_DFP_REROUND;
   insn->size = size;
   insn->variant.dfp_reround.dst_hi = dst;
   insn->variant.dfp_reround.op2 = op2;
   insn->variant.dfp_reround.op3_hi = op3;
   insn->variant.dfp_reround.dst_lo = INVALID_HREG;
   insn->variant.dfp_reround.op3_lo = INVALID_HREG;
   insn->variant.dfp_reround.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_fp_convert(UChar size, s390_fp_conv_t tag, HReg dst, HReg op,
                     HReg r1, s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));
   s390_fp_convert *fp_convert = LibVEX_Alloc_inline(sizeof(s390_fp_convert));

   vassert(size == 4 || size == 8);

   insn->tag  = S390_INSN_FP_CONVERT;
   insn->size = size;
   insn->variant.fp_convert.details = fp_convert;

   fp_convert->tag = tag;
   fp_convert->dst_hi = dst;
   fp_convert->op_hi  = op;
   fp_convert->r1 = r1;
   fp_convert->dst_lo = INVALID_HREG;
   fp_convert->op_lo  = INVALID_HREG;
   fp_convert->rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_fp128_convert(UChar size, s390_fp_conv_t tag, HReg dst_hi,
                        HReg dst_lo, HReg op_hi, HReg op_lo, HReg r1,
                        s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));
   s390_fp_convert *fp_convert = LibVEX_Alloc_inline(sizeof(s390_fp_convert));

   vassert(size == 16);

   insn->tag  = S390_INSN_FP_CONVERT;
   insn->size = size;
   insn->variant.fp_convert.details = fp_convert;

   fp_convert->tag = tag;
   fp_convert->dst_hi = dst_hi;
   fp_convert->dst_lo = dst_lo;
   fp_convert->op_hi  = op_hi;
   fp_convert->r1 = r1;
   fp_convert->op_lo  = op_lo;
   fp_convert->rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_dfp128_binop(UChar size, s390_dfp_binop_t tag, HReg dst_hi,
                       HReg dst_lo, HReg op2_hi, HReg op2_lo, HReg op3_hi,
                       HReg op3_lo, s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));
   s390_dfp_binop *dfp_binop = LibVEX_Alloc_inline(sizeof(s390_dfp_binop));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
   vassert(is_valid_fp128_regpair(op2_hi, op2_lo));
   vassert(is_valid_fp128_regpair(op3_hi, op3_lo));

   insn->tag  = S390_INSN_DFP_BINOP;
   insn->size = size;
   insn->variant.dfp_binop.details = dfp_binop;

   dfp_binop->tag = tag;
   dfp_binop->dst_hi = dst_hi;
   dfp_binop->dst_lo = dst_lo;
   dfp_binop->op2_hi = op2_hi;
   dfp_binop->op2_lo = op2_lo;
   dfp_binop->op3_hi = op3_hi;
   dfp_binop->op3_lo = op3_lo;
   dfp_binop->rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_dfp128_unop(UChar size, s390_dfp_unop_t tag, HReg dst,
                      HReg op_hi, HReg op_lo)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   /* destination is an 8 byte integer value */
   vassert(size == 8);
   vassert(is_valid_fp128_regpair(op_hi, op_lo));

   insn->tag  = S390_INSN_DFP_UNOP;
   insn->size = size;
   insn->variant.dfp_unop.tag = tag;
   insn->variant.dfp_unop.dst_hi = dst;
   insn->variant.dfp_unop.dst_lo = INVALID_HREG;
   insn->variant.dfp_unop.op_hi = op_hi;
   insn->variant.dfp_unop.op_lo = op_lo;

   return insn;
}


s390_insn *
s390_insn_dfp128_intop(UChar size, s390_dfp_intop_t tag, HReg dst_hi,
                       HReg dst_lo, HReg op2, HReg op3_hi, HReg op3_lo)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
   vassert(is_valid_fp128_regpair(op3_hi, op3_lo));

   insn->tag  = S390_INSN_DFP_INTOP;
   insn->size = size;
   insn->variant.dfp_intop.tag = tag;
   insn->variant.dfp_intop.dst_hi = dst_hi;
   insn->variant.dfp_intop.dst_lo = dst_lo;
   insn->variant.dfp_intop.op2    = op2;
   insn->variant.dfp_intop.op3_hi = op3_hi;
   insn->variant.dfp_intop.op3_lo = op3_lo;

   return insn;
}


s390_insn *
s390_insn_dfp128_compare(UChar size, s390_dfp_cmp_t tag, HReg dst, HReg op1_hi,
                         HReg op1_lo, HReg op2_hi, HReg op2_lo)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(op1_hi, op1_lo));
   vassert(is_valid_fp128_regpair(op2_hi, op2_lo));

   insn->tag  = S390_INSN_DFP_COMPARE;
   insn->size = size;
   insn->variant.dfp_compare.tag = tag;
   insn->variant.dfp_compare.dst = dst;
   insn->variant.dfp_compare.op1_hi = op1_hi;
   insn->variant.dfp_compare.op1_lo = op1_lo;
   insn->variant.dfp_compare.op2_hi = op2_hi;
   insn->variant.dfp_compare.op2_lo = op2_lo;

   return insn;
}


static s390_insn *
s390_insn_dfp128_convert(UChar size, s390_dfp_conv_t tag, HReg dst_hi,
                         HReg dst_lo, HReg op_hi, HReg op_lo,
                         s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   if (size == 16) {
      /* From smaller size to 16 bytes */
      vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
      vassert(hregIsInvalid(op_lo));
   } else {
      /* From 16 bytes to smaller size */
      vassert(is_valid_fp128_regpair(op_hi, op_lo));
   }

   insn->tag  = S390_INSN_DFP_CONVERT;
   insn->size = size;
   insn->variant.dfp_convert.tag = tag;
   insn->variant.dfp_convert.dst_hi = dst_hi;
   insn->variant.dfp_convert.dst_lo = dst_lo;
   insn->variant.dfp_convert.op_hi = op_hi;
   insn->variant.dfp_convert.op_lo = op_lo;
   insn->variant.dfp_convert.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_dfp128_convert_to(UChar size, s390_dfp_conv_t tag, HReg dst_hi,
                            HReg dst_lo, HReg op)
{
   /* Conversion to dfp128 never requires a rounding mode. Provide default
      rounding mode. It will not be used when emitting insns. */
   s390_dfp_round_t rounding_mode = S390_DFP_ROUND_NEAREST_EVEN_4;

   return s390_insn_dfp128_convert(size, tag, dst_hi, dst_lo, op,
                                   INVALID_HREG, rounding_mode);
}


s390_insn *
s390_insn_dfp128_convert_from(UChar size, s390_dfp_conv_t tag, HReg dst_hi,
                              HReg dst_lo, HReg op_hi, HReg op_lo,
                              s390_dfp_round_t rounding_mode)
{
   return s390_insn_dfp128_convert(size, tag, dst_hi, dst_lo, op_hi, op_lo,
                                   rounding_mode);
}


s390_insn *
s390_insn_dfp128_reround(UChar size, HReg dst_hi, HReg dst_lo, HReg op2,
                         HReg op3_hi, HReg op3_lo,
                         s390_dfp_round_t rounding_mode)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 16);
   vassert(is_valid_fp128_regpair(dst_hi, dst_lo));
   vassert(is_valid_fp128_regpair(op3_hi, op3_lo));

   insn->tag  = S390_INSN_DFP_REROUND;
   insn->size = size;
   insn->variant.dfp_reround.dst_hi = dst_hi;
   insn->variant.dfp_reround.dst_lo = dst_lo;
   insn->variant.dfp_reround.op2    = op2;
   insn->variant.dfp_reround.op3_hi = op3_hi;
   insn->variant.dfp_reround.op3_lo = op3_lo;
   insn->variant.dfp_reround.rounding_mode = rounding_mode;

   return insn;
}


s390_insn *
s390_insn_mfence(void)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_MFENCE;
   insn->size = 0;   /* not needed */

   return insn;
}


s390_insn *
s390_insn_mimm(UChar size, s390_amode *dst, ULong value)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   /* This insn will be mapped to insns that require base register
      plus 12-bit displacement */
   vassert(dst->tag == S390_AMODE_B12);

   insn->tag  = S390_INSN_MIMM;
   insn->size = size;
   insn->variant.mimm.dst = dst;
   insn->variant.mimm.value = value;

   return insn;
}


s390_insn *
s390_insn_madd(UChar size, s390_amode *dst, UChar delta, ULong value)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 4 || size == 8);

   /* This insn will be mapped to an ASI or AGSI so we can only allow base
      register plus 12-bit / 20-bit displacement. */
   vassert(dst->tag == S390_AMODE_B12 || dst->tag == S390_AMODE_B20);
   /* ASI and AGSI require the GIE facility */
   vassert(s390_host_has_gie);

   insn->tag  = S390_INSN_MADD;
   insn->size = size;
   insn->variant.madd.dst   = dst;
   insn->variant.madd.delta = delta;
   insn->variant.madd.value = value;

   return insn;
}


s390_insn *
s390_insn_set_fpc_bfprm(UChar size, HReg mode)
{
   vassert(size == 4);

   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_SET_FPC_BFPRM;
   insn->size = size;
   insn->variant.set_fpc_bfprm.mode = mode;

   return insn;
}


s390_insn *
s390_insn_set_fpc_dfprm(UChar size, HReg mode)
{
   vassert(size == 4);

   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_SET_FPC_DFPRM;
   insn->size = size;
   insn->variant.set_fpc_dfprm.mode = mode;

   return insn;
}


s390_insn *
s390_insn_xdirect(s390_cc_t cond, Addr64 dst, s390_amode *guest_IA,
                  Bool to_fast_entry)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(guest_IA->tag == S390_AMODE_B12);

   insn->tag  = S390_INSN_XDIRECT;
   insn->size = 0;   /* does not matter */

   insn->variant.xdirect.cond = cond;
   insn->variant.xdirect.dst = dst;
   insn->variant.xdirect.guest_IA = guest_IA;
   insn->variant.xdirect.to_fast_entry = to_fast_entry;

   return insn;
}


s390_insn *
s390_insn_xindir(s390_cc_t cond, HReg dst, s390_amode *guest_IA)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(guest_IA->tag == S390_AMODE_B12);

   insn->tag  = S390_INSN_XINDIR;
   insn->size = 0;   /* does not matter */

   insn->variant.xindir.cond = cond;
   insn->variant.xindir.dst = dst;
   insn->variant.xindir.guest_IA = guest_IA;

   return insn;
}


s390_insn *
s390_insn_xassisted(s390_cc_t cond, HReg dst, s390_amode *guest_IA,
                    IRJumpKind kind)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(guest_IA->tag == S390_AMODE_B12);

   insn->tag  = S390_INSN_XASSISTED;
   insn->size = 0;   /* does not matter */

   insn->variant.xassisted.cond = cond;
   insn->variant.xassisted.dst = dst;
   insn->variant.xassisted.guest_IA = guest_IA;
   insn->variant.xassisted.kind = kind;

   return insn;
}


s390_insn *
s390_insn_evcheck(s390_amode *counter, s390_amode *fail_addr)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(counter->tag   == S390_AMODE_B12);
   vassert(fail_addr->tag == S390_AMODE_B12);

   insn->tag  = S390_INSN_EVCHECK;
   insn->size = 0;   /* does not matter */

   insn->variant.evcheck.counter = counter;
   insn->variant.evcheck.fail_addr = fail_addr;

   return insn;
}


s390_insn *
s390_insn_profinc(void)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_PROFINC;
   insn->size = 0;   /* does not matter */

   return insn;
}


s390_insn *
s390_insn_vec_amodeop(UChar size, s390_vec_amodeop_t tag, HReg dst, HReg op1,
                    s390_amode *op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   insn->tag  = S390_INSN_VEC_AMODEOP;
   insn->size = size;
   insn->variant.vec_amodeop.tag = tag;
   insn->variant.vec_amodeop.dst = dst;
   insn->variant.vec_amodeop.op1 = op1;
   insn->variant.vec_amodeop.op2 = op2;

   return insn;
}

s390_insn *s390_insn_vec_amodeintop(UChar size, s390_vec_amodeintop_t tag, HReg dst,
                                    s390_amode* op2, HReg op3)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 1 || size == 2 || size == 4 || size == 8);

   insn->tag  = S390_INSN_VEC_AMODEINTOP;
   insn->size = size;
   insn->variant.vec_amodeintop.tag = tag;
   insn->variant.vec_amodeintop.dst = dst;
   insn->variant.vec_amodeintop.op2 = op2;
   insn->variant.vec_amodeintop.op3 = op3;

   return insn;
}

s390_insn *s390_insn_vec_binop(UChar size, s390_vec_binop_t tag, HReg dst,
                               HReg op1, HReg op2)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   vassert(size == 1 || size == 2 || size == 4 || size == 8 || size == 16);

   insn->tag  = S390_INSN_VEC_BINOP;
   insn->size = size;
   insn->variant.vec_binop.tag = tag;
   insn->variant.vec_binop.dst = dst;
   insn->variant.vec_binop.op1 = op1;
   insn->variant.vec_binop.op2 = op2;

   return insn;
}

s390_insn *s390_insn_vec_triop(UChar size, s390_vec_triop_t tag, HReg dst,
                               HReg op1, HReg op2, HReg op3)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));


   insn->tag  = S390_INSN_VEC_TRIOP;
   insn->size = size;
   insn->variant.vec_triop.tag = tag;
   insn->variant.vec_triop.dst = dst;
   insn->variant.vec_triop.op1 = op1;
   insn->variant.vec_triop.op2 = op2;
   insn->variant.vec_triop.op3 = op3;

   return insn;
}

s390_insn *s390_insn_vec_replicate(UChar size, HReg dst, HReg op1,
                                   UChar idx)
{
   s390_insn *insn = LibVEX_Alloc_inline(sizeof(s390_insn));

   insn->tag  = S390_INSN_VEC_REPLICATE;
   insn->size = size;
   insn->variant.vec_replicate.dst = dst;
   insn->variant.vec_replicate.op1 = op1;
   insn->variant.vec_replicate.idx = idx;

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


static const HChar *
s390_jump_kind_as_string(IRJumpKind kind)
{
   switch (kind) {
   case Ijk_Boring:      return "Boring";
   case Ijk_Call:        return "Call";
   case Ijk_Ret:         return "Return";
   case Ijk_ClientReq:   return "ClientReq";
   case Ijk_Yield:       return "Yield";
   case Ijk_EmWarn:      return "EmWarn";
   case Ijk_EmFail:      return "EmFail";
   case Ijk_NoDecode:    return "NoDecode";
   case Ijk_MapFail:     return "MapFail";
   case Ijk_InvalICache: return "Invalidate";
   case Ijk_NoRedir:     return "NoRedir";
   case Ijk_SigTRAP:     return "SigTRAP";
   case Ijk_SigFPE:      return "SigFPE";
   case Ijk_SigSEGV:     return "SigSEGV";
   case Ijk_SigBUS:      return "SigBUS";
   case Ijk_Sys_syscall: return "Sys_syscall";
   default:
      vpanic("s390_jump_kind_as_string");
   }
}


/* Helper function for writing out a V insn */
static void
s390_sprintf(HChar *buf, const HChar *fmt, ...)
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

      case 'G':     /* %G = guest state @ offset */
         p += vex_sprintf(p, "guest[%u]", va_arg(args, UInt));
         continue;

      case 'C':     /* %C = condition code */
         p += vex_sprintf(p, "%s", s390_cc_as_string(va_arg(args, s390_cc_t)));
         continue;

      case 'J':     /* &J = jump kind */
         p += vex_sprintf(p, "%s",
                          s390_jump_kind_as_string(va_arg(args, IRJumpKind)));
         continue;

      case 'L': {   /* %L = argument list in helper call*/
         UInt i, num_args;

         num_args = va_arg(args, UInt);

         for (i = 0; i < num_args; ++i) {
            if (i != 0) p += vex_sprintf(p, ", ");
            p += vex_sprintf(p, "r%u", s390_gprno_from_arg_index(i));
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
   static HChar buf[300];  // large enough
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

   case S390_INSN_MEMCPY:
      s390_sprintf(buf, "%M %A,%A", "v-memcpy", insn->variant.memcpy.dst,
                   insn->variant.memcpy.src);
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
      case S390_ALU_ILIH: op = "v-ilih"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.alu.dst, /* also op1 */
                   &insn->variant.alu.op2);
      break;

   case S390_INSN_SMUL:
   case S390_INSN_UMUL:
      if (insn->tag == S390_INSN_SMUL) {
         op = "v-muls";
      } else {
         op = "v-mulu";
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.mul.dst_hi,
                   &insn->variant.mul.op2);
      break;

   case S390_INSN_SDIV:
   case S390_INSN_UDIV:
      if (insn->tag == S390_INSN_SDIV) {
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

      case S390_VEC_FILL:
         op = "v-vfill";
         break;

      case S390_VEC_DUPLICATE:
         op = "v-vdup";
         break;

      case S390_VEC_UNPACKLOWS:
         op = "v-vunpacks";
         break;

      case S390_VEC_UNPACKLOWU:
         op = "v-vunpacku";
         break;

      case S390_VEC_ABS:
         op = "v-vabs";
         break;

      case S390_VEC_COUNT_LEADING_ZEROES:
         op = "v-vclz";
         break;

      case S390_VEC_COUNT_TRAILING_ZEROES:
         op = "v-vctz";
         break;

      case S390_VEC_COUNT_ONES:
         op = "v-vpopct";
         break;

      case S390_VEC_FLOAT_NEG:
         op = "v-vfloatneg";
         break;

      case S390_VEC_FLOAT_ABS:
         op = "v-vfloatabs";
         break;

      case S390_VEC_FLOAT_NABS:
         op = "v-vfloatnabs";
         break;

      case S390_VEC_FLOAT_SQRT:
         op = "v-vfloatsqrt";
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

   case S390_INSN_CDAS: {
      s390_cdas *cdas = insn->variant.cdas.details;

      s390_sprintf(buf, "%M %R,%R,%A,%R,%R,%R,%R", "v-cdas",
                   cdas->op1_high, cdas->op1_low, cdas->op2, cdas->op3_high,
                   cdas->op3_low, cdas->old_mem_high, cdas->old_mem_low);
      break;
   }

   case S390_INSN_COMPARE:
      if (insn->variant.compare.signed_comparison) {
         op = "v-cmps";
      } else {
         op = "v-cmpu";
      }
      s390_sprintf(buf, "%M %R,%O", op, insn->variant.compare.src1,
                   &insn->variant.compare.src2);
      break;

   case S390_INSN_HELPER_CALL: {
      s390_helper_call *helper_call = insn->variant.helper_call.details;
      s390_sprintf(buf, "%M if (%C) %s{%I}(%L)", "v-call",
                   helper_call->cond,
                   helper_call->name,
                   helper_call->target,
                   helper_call->num_args);
      return buf;   /* avoid printing "size = ..." which is meaningless */
   }

   case S390_INSN_BFP_TRIOP:
      switch (insn->variant.bfp_triop.tag) {
      case S390_BFP_MADD:  op = "v-fmadd";  break;
      case S390_BFP_MSUB:  op = "v-fmsub";  break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%R", op,
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
      s390_sprintf(buf, "%M %R,%R", op,
                   insn->variant.bfp_binop.dst_hi  /* op1 same as dst */,
                   insn->variant.bfp_binop.op2_hi);
      break;

   case S390_INSN_BFP_COMPARE:
      s390_sprintf(buf, "%M %R,%R,%R", "v-fcmp", insn->variant.bfp_compare.dst,
                   insn->variant.bfp_compare.op1_hi,
                   insn->variant.bfp_compare.op2_hi);
      break;

   case S390_INSN_BFP_UNOP:
      switch (insn->variant.bfp_unop.tag) {
      case S390_BFP_ABS:         op = "v-fabs";  break;
      case S390_BFP_NABS:        op = "v-fnabs"; break;
      case S390_BFP_NEG:         op = "v-fneg";  break;
      case S390_BFP_SQRT:        op = "v-fsqrt"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R", op, insn->variant.bfp_unop.dst_hi,
                   insn->variant.bfp_unop.op_hi);
      break;

   case S390_INSN_BFP_CONVERT:
      switch (insn->variant.bfp_convert.tag) {
      case S390_BFP_I32_TO_F32:
      case S390_BFP_I32_TO_F64:
      case S390_BFP_I32_TO_F128:
      case S390_BFP_I64_TO_F32:
      case S390_BFP_I64_TO_F64:
      case S390_BFP_I64_TO_F128: op = "v-i2f"; break;
      case S390_BFP_U32_TO_F32:
      case S390_BFP_U32_TO_F64:
      case S390_BFP_U32_TO_F128:
      case S390_BFP_U64_TO_F32:
      case S390_BFP_U64_TO_F64:
      case S390_BFP_U64_TO_F128: op = "v-u2f"; break;
      case S390_BFP_F32_TO_I32:
      case S390_BFP_F32_TO_I64:
      case S390_BFP_F64_TO_I32:
      case S390_BFP_F64_TO_I64:
      case S390_BFP_F128_TO_I32:
      case S390_BFP_F128_TO_I64: op = "v-f2i"; break;
      case S390_BFP_F32_TO_U32:
      case S390_BFP_F32_TO_U64:
      case S390_BFP_F64_TO_U32:
      case S390_BFP_F64_TO_U64:
      case S390_BFP_F128_TO_U32:
      case S390_BFP_F128_TO_U64: op = "v-f2u"; break;
      case S390_BFP_F32_TO_F64:
      case S390_BFP_F32_TO_F128:
      case S390_BFP_F64_TO_F32:
      case S390_BFP_F64_TO_F128:
      case S390_BFP_F128_TO_F32:
      case S390_BFP_F128_TO_F64: op = "v-f2f"; break;
      case S390_BFP_F32_TO_F32I:
      case S390_BFP_F64_TO_F64I:
      case S390_BFP_F128_TO_F128I: op = "v-f2fi"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R", op, insn->variant.bfp_convert.dst_hi,
                   insn->variant.bfp_convert.op_hi);
      break;

   case S390_INSN_DFP_BINOP: {
      s390_dfp_binop *dfp_binop = insn->variant.dfp_binop.details;

      switch (dfp_binop->tag) {
      case S390_DFP_ADD:  op = "v-dadd";  break;
      case S390_DFP_SUB:  op = "v-dsub";  break;
      case S390_DFP_MUL:  op = "v-dmul";  break;
      case S390_DFP_DIV:  op = "v-ddiv";  break;
      case S390_DFP_QUANTIZE:  op = "v-dqua";  break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%R", op, dfp_binop->dst_hi,
                   dfp_binop->op2_hi, dfp_binop->op3_hi);
      break;
   }

   case S390_INSN_DFP_UNOP:
      switch (insn->variant.dfp_unop.tag) {
      case S390_DFP_EXTRACT_EXP_D64:
      case S390_DFP_EXTRACT_EXP_D128:  op = "v-d2exp";  break;
      case S390_DFP_EXTRACT_SIG_D64:
      case S390_DFP_EXTRACT_SIG_D128:  op = "v-d2sig";  break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R", op, insn->variant.dfp_unop.dst_hi,
                   insn->variant.dfp_unop.op_hi);
      break;

   case S390_INSN_DFP_INTOP:
      switch (insn->variant.dfp_intop.tag) {
      case S390_DFP_SHIFT_LEFT:  op = "v-dshl"; break;
      case S390_DFP_SHIFT_RIGHT: op = "v-dshr"; break;
      case S390_DFP_INSERT_EXP:  op = "v-diexp"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%R", op, insn->variant.dfp_intop.dst_hi,
                   insn->variant.dfp_intop.op2,
                   insn->variant.dfp_intop.op3_hi);
      break;

   case S390_INSN_DFP_COMPARE:
      switch (insn->variant.dfp_compare.tag) {
      case S390_DFP_COMPARE:     op = "v-dcmp"; break;
      case S390_DFP_COMPARE_EXP: op = "v-dcmpexp"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R,%R", op, insn->variant.dfp_compare.dst,
                   insn->variant.dfp_compare.op1_hi,
                   insn->variant.dfp_compare.op2_hi);
      break;

   case S390_INSN_DFP_CONVERT:
      switch (insn->variant.dfp_convert.tag) {
      case S390_DFP_D32_TO_D64:
      case S390_DFP_D64_TO_D32:
      case S390_DFP_D64_TO_D128:
      case S390_DFP_D128_TO_D64: op = "v-d2d"; break;
      case S390_DFP_I32_TO_D64:
      case S390_DFP_I32_TO_D128:
      case S390_DFP_I64_TO_D64:
      case S390_DFP_I64_TO_D128: op = "v-i2d"; break;
      case S390_DFP_U32_TO_D64:
      case S390_DFP_U32_TO_D128:
      case S390_DFP_U64_TO_D64:
      case S390_DFP_U64_TO_D128: op = "v-u2d"; break;
      case S390_DFP_D64_TO_I32:
      case S390_DFP_D128_TO_I32:
      case S390_DFP_D64_TO_I64:
      case S390_DFP_D128_TO_I64: op = "v-d2i"; break;
      case S390_DFP_D64_TO_U32:
      case S390_DFP_D64_TO_U64:
      case S390_DFP_D128_TO_U32:
      case S390_DFP_D128_TO_U64: op = "v-d2u"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R", op, insn->variant.dfp_convert.dst_hi,
                   insn->variant.dfp_convert.op_hi);
      break;

   case S390_INSN_DFP_REROUND:
      s390_sprintf(buf, "%M %R,%R,%R", "v-drrnd",
                   insn->variant.dfp_reround.dst_hi,
                   insn->variant.dfp_reround.op2,
                   insn->variant.dfp_reround.op3_hi);
      break;

   case S390_INSN_FP_CONVERT: {
      s390_fp_convert *fp_convert = insn->variant.fp_convert.details;

      switch (fp_convert->tag) {
      case S390_FP_F32_TO_D32:
      case S390_FP_F32_TO_D64:
      case S390_FP_F32_TO_D128:
      case S390_FP_F64_TO_D32:
      case S390_FP_F64_TO_D64:
      case S390_FP_F64_TO_D128:
      case S390_FP_F128_TO_D32:
      case S390_FP_F128_TO_D64:
      case S390_FP_F128_TO_D128: op = "v-f2d"; break;
      case S390_FP_D32_TO_F32:
      case S390_FP_D32_TO_F64:
      case S390_FP_D32_TO_F128:
      case S390_FP_D64_TO_F32:
      case S390_FP_D64_TO_F64:
      case S390_FP_D64_TO_F128:
      case S390_FP_D128_TO_F32:
      case S390_FP_D128_TO_F64:
      case S390_FP_D128_TO_F128: op = "v-d2f"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R,%R", op, fp_convert->dst_hi,
                   fp_convert->op_hi);
      break;
   }

   case S390_INSN_MFENCE:
      s390_sprintf(buf, "%M", "v-mfence");
      return buf;   /* avoid printing "size = ..." which is meaningless */

   case S390_INSN_MIMM:
      s390_sprintf(buf, "%M %A,%I", "v-mimm", insn->variant.mimm.dst,
                   insn->variant.mimm.value);
      break;

   case S390_INSN_MADD:
      s390_sprintf(buf, "%M %A += %I  (= %I)", "v-madd",
                   insn->variant.madd.dst,
                   (Long)(Char)insn->variant.madd.delta,
                   insn->variant.madd.value);
      break;

   case S390_INSN_SET_FPC_BFPRM:
      s390_sprintf(buf, "%M %R", "v-set-fpc-bfprm",
                   insn->variant.set_fpc_bfprm.mode);
      break;

   case S390_INSN_SET_FPC_DFPRM:
      s390_sprintf(buf, "%M %R", "v-set-fpc-dfprm",
                   insn->variant.set_fpc_dfprm.mode);
      break;

   case S390_INSN_EVCHECK:
      s390_sprintf(buf, "%M counter = %A, fail-addr = %A", "v-evcheck",
                   insn->variant.evcheck.counter,
                   insn->variant.evcheck.fail_addr);
      return buf;   /* avoid printing "size = ..." which is meaningless */

   case S390_INSN_PROFINC:
      s390_sprintf(buf, "%M", "v-profinc");
      return buf;   /* avoid printing "size = ..." which is meaningless */

   case S390_INSN_XDIRECT:
      s390_sprintf(buf, "%M if (%C) %A = %I  %s", "v-xdirect",
                   insn->variant.xdirect.cond,
                   insn->variant.xdirect.guest_IA,
                   insn->variant.xdirect.dst,
                   insn->variant.xdirect.to_fast_entry ? "fast" : "slow");
      return buf;   /* avoid printing "size = ..." which is meaningless */

   case S390_INSN_XINDIR:
      s390_sprintf(buf, "%M if (%C) %A = %R", "v-xindir",
                   insn->variant.xindir.cond,
                   insn->variant.xindir.guest_IA,
                   insn->variant.xindir.dst);
      return buf;   /* avoid printing "size = ..." which is meaningless */

   case S390_INSN_XASSISTED:
      s390_sprintf(buf, "%M if (%C) %J %A = %R", "v-xassisted",
                   insn->variant.xassisted.cond,
                   insn->variant.xassisted.kind,
                   insn->variant.xassisted.guest_IA,
                   insn->variant.xassisted.dst);
      return buf;   /* avoid printing "size = ..." which is meaningless */

   case S390_INSN_VEC_AMODEOP:
      switch (insn->variant.vec_amodeop.tag) {
      case S390_VEC_GET_ELEM:  op = "v-vgetelem";  break;
      case S390_VEC_ELEM_SHL_INT: op = "v-veshl"; break;
      case S390_VEC_ELEM_SHRA_INT: op = "v-veshra"; break;
      case S390_VEC_ELEM_SHRL_INT: op = "v-veshrl"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R, %R, %A", op, insn->variant.vec_amodeop.dst,
                   insn->variant.vec_amodeop.op1,
                   insn->variant.vec_amodeop.op2);
      break;

   case S390_INSN_VEC_AMODEINTOP:
      switch (insn->variant.vec_amodeintop.tag) {
      case S390_VEC_SET_ELEM:  op = "v-vsetelem";  break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R, %A, %R", op, insn->variant.vec_amodeintop.dst,
                   insn->variant.vec_amodeintop.op2,
                   insn->variant.vec_amodeintop.op3);
      break;

   case S390_INSN_VEC_BINOP:
      switch (insn->variant.vec_binop.tag) {
      case S390_VEC_PACK:           op = "v-vpack"; break;
      case S390_VEC_PACK_SATURS:    op = "v-vpacksaturs"; break;
      case S390_VEC_PACK_SATURU:    op = "v-vpacksaturu"; break;
      case S390_VEC_COMPARE_EQUAL:  op = "v-vcmpeq"; break;
      case S390_VEC_OR:             op = "v-vor"; break;
      case S390_VEC_ORC:            op = "v-vorc"; break;
      case S390_VEC_XOR:            op = "v-vxor";  break;
      case S390_VEC_AND:            op = "v-vand"; break;
      case S390_VEC_MERGEL:         op = "v-vmergel"; break;
      case S390_VEC_MERGEH:         op = "v-vmergeh"; break;
      case S390_VEC_NOR:            op = "v-vnor"; break;
      case S390_VEC_INT_ADD:        op = "v-vintadd"; break;
      case S390_VEC_INT_SUB:        op = "v-vintsub"; break;
      case S390_VEC_MAXU:           op = "v-vmaxu"; break;
      case S390_VEC_MAXS:           op = "v-vmaxs"; break;
      case S390_VEC_MINU:           op = "v-vminu"; break;
      case S390_VEC_MINS:           op = "v-vmins"; break;
      case S390_VEC_AVGU:           op = "v-vavgu"; break;
      case S390_VEC_AVGS:           op = "v-vavgs"; break;
      case S390_VEC_COMPARE_GREATERS: op = "v-vcmpgts"; break;
      case S390_VEC_COMPARE_GREATERU: op = "v-vcmpgtu"; break;
      case S390_VEC_INT_MUL_HIGHS:    op = "v-vintmulhis"; break;
      case S390_VEC_INT_MUL_HIGHU:    op = "v-vintmulhiu"; break;
      case S390_VEC_INT_MUL_LOW:      op = "v-vintmullo"; break;
      case S390_VEC_INT_MUL_EVENS:    op = "v-vintmulevens"; break;
      case S390_VEC_INT_MUL_EVENU:    op = "v-vintmulevenu"; break;
      case S390_VEC_ELEM_SHL_V:       op = "v-velemshl"; break;
      case S390_VEC_ELEM_SHRA_V:      op = "v-vshrav"; break;
      case S390_VEC_ELEM_SHRL_V:      op = "v-vshrlv"; break;
      case S390_VEC_ELEM_ROLL_V:      op = "v-vrollv"; break;
      case S390_VEC_SHL_BITS:         op = "v-vshlbits"; break;
      case S390_VEC_SHRL_BITS:        op = "v-vshrlbits"; break;
      case S390_VEC_SHRA_BITS:        op = "v-vshrabits"; break;
      case S390_VEC_SHL_BYTES:        op = "v-vshlbytes"; break;
      case S390_VEC_SHRL_BYTES:       op = "v-vshrlbytes"; break;
      case S390_VEC_SHRA_BYTES:       op = "v-vshrabytes"; break;
      case S390_VEC_PWSUM_W:          op = "v-vpwsumw"; break;
      case S390_VEC_PWSUM_DW:         op = "v-vpwsumdw"; break;
      case S390_VEC_PWSUM_QW:         op = "v-vpwsumqw"; break;
      case S390_VEC_INIT_FROM_GPRS:   op = "v-vinitfromgprs"; break;
      case S390_VEC_INIT_FROM_FPRS:   op = "v-vinitfromfprs"; break;
      case S390_VEC_FLOAT_ADD:        op = "v-vfloatadd"; break;
      case S390_VEC_FLOAT_SUB:        op = "v-vfloatsub"; break;
      case S390_VEC_FLOAT_MUL:        op = "v-vfloatmul"; break;
      case S390_VEC_FLOAT_DIV:        op = "v-vfloatdiv"; break;
      case S390_VEC_FLOAT_COMPARE_EQUAL: op = "v-vfloatcmpeq"; break;
      case S390_VEC_FLOAT_COMPARE_LESS_OR_EQUAL:  op = "v-vfloatcmple"; break;
      case S390_VEC_FLOAT_COMPARE_LESS: op = "v-vfloatcmpl"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R, %R, %R", op, insn->variant.vec_binop.dst,
                   insn->variant.vec_binop.op1, insn->variant.vec_binop.op2);
      break;

   case S390_INSN_VEC_TRIOP:
      switch (insn->variant.vec_triop.tag) {
      case S390_VEC_PERM:  op = "v-vperm";  break;
      case S390_VEC_FLOAT_MADD: op = "v-vfloatmadd"; break;
      case S390_VEC_FLOAT_MSUB: op = "v-vfloatmsub"; break;
      default: goto fail;
      }
      s390_sprintf(buf, "%M %R, %R, %R, %R", op, insn->variant.vec_triop.dst,
                   insn->variant.vec_triop.op1, insn->variant.vec_triop.op2,
                   insn->variant.vec_triop.op3);
      break;

   case S390_INSN_VEC_REPLICATE:
      s390_sprintf(buf, "%M %R, %R, %I", "v-vrep",
                   insn->variant.vec_replicate.dst,
                   insn->variant.vec_replicate.op1,
                   insn->variant.vec_replicate.idx);
      break;

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

   case S390_INSN_BFP_CONVERT:
      switch (insn->variant.bfp_convert.tag) {
      case S390_BFP_I32_TO_F32:
      case S390_BFP_I32_TO_F64:
      case S390_BFP_I32_TO_F128:
      case S390_BFP_U32_TO_F32:
      case S390_BFP_U32_TO_F64:
      case S390_BFP_U32_TO_F128:
      case S390_BFP_F32_TO_I32:
      case S390_BFP_F32_TO_I64:
      case S390_BFP_F32_TO_U32:
      case S390_BFP_F32_TO_U64:
      case S390_BFP_F32_TO_F64:
      case S390_BFP_F32_TO_F128: p += vex_sprintf(p, "4 -> "); goto common;
      case S390_BFP_I64_TO_F32:
      case S390_BFP_I64_TO_F64:
      case S390_BFP_I64_TO_F128:
      case S390_BFP_U64_TO_F32:
      case S390_BFP_U64_TO_F64:
      case S390_BFP_U64_TO_F128:
      case S390_BFP_F64_TO_I32:
      case S390_BFP_F64_TO_I64:
      case S390_BFP_F64_TO_U32:
      case S390_BFP_F64_TO_U64:
      case S390_BFP_F64_TO_F32:
      case S390_BFP_F64_TO_F128: p += vex_sprintf(p, "8 -> "); goto common;
      case S390_BFP_F128_TO_I32:
      case S390_BFP_F128_TO_I64:
      case S390_BFP_F128_TO_U32:
      case S390_BFP_F128_TO_U64:
      case S390_BFP_F128_TO_F32:
      case S390_BFP_F128_TO_F64: p += vex_sprintf(p, "16 -> "); goto common;
      default:
         goto common;
      }

   case S390_INSN_DFP_CONVERT:
      switch (insn->variant.dfp_convert.tag) {
      case S390_DFP_D32_TO_D64:
      case S390_DFP_I32_TO_D64:
      case S390_DFP_I32_TO_D128:
      case S390_DFP_U32_TO_D64:
      case S390_DFP_U32_TO_D128: p += vex_sprintf(p, "4 -> "); goto common;
      case S390_DFP_D64_TO_D32:
      case S390_DFP_D64_TO_D128:
      case S390_DFP_I64_TO_D64:
      case S390_DFP_I64_TO_D128:
      case S390_DFP_U64_TO_D64:
      case S390_DFP_U64_TO_D128:
      case S390_DFP_D64_TO_I32:
      case S390_DFP_D64_TO_I64:
      case S390_DFP_D64_TO_U32:
      case S390_DFP_D64_TO_U64:  p += vex_sprintf(p, "8 -> "); goto common;
      case S390_DFP_D128_TO_D64:
      case S390_DFP_D128_TO_I32:
      case S390_DFP_D128_TO_I64:
      case S390_DFP_D128_TO_U32:
      case S390_DFP_D128_TO_U64: p += vex_sprintf(p, "16 -> "); goto common;
      default:
         goto common;
      }

   case S390_INSN_FP_CONVERT: {
      s390_fp_convert *fp_convert = insn->variant.fp_convert.details;

      switch (fp_convert->tag) {
      case S390_FP_F32_TO_D32:
      case S390_FP_F32_TO_D64:
      case S390_FP_F32_TO_D128:
      case S390_FP_D32_TO_F32:
      case S390_FP_D32_TO_F64:
      case S390_FP_D32_TO_F128:  p += vex_sprintf(p, "4 -> "); goto common;
      case S390_FP_F64_TO_D32:
      case S390_FP_F64_TO_D64:
      case S390_FP_F64_TO_D128:
      case S390_FP_D64_TO_F32:
      case S390_FP_D64_TO_F64:
      case S390_FP_D64_TO_F128:  p += vex_sprintf(p, "8 -> "); goto common;
      case S390_FP_F128_TO_D32:
      case S390_FP_F128_TO_D64:
      case S390_FP_F128_TO_D128:
      case S390_FP_D128_TO_F32:
      case S390_FP_D128_TO_F64:
      case S390_FP_D128_TO_F128: p += vex_sprintf(p, "16 -> "); goto common;
      default:
         goto common;
      }
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
      case 1:  return s390_emit_IC(p, reg, x, b, d);
      case 2:  return s390_emit_LH(p, reg, x, b, d);
      case 4:  return s390_emit_L(p, reg, x, b, d);
      case 8:  return s390_emit_LG(p, reg, x, b, DISP20(d));
      case 16: return s390_emit_VL(p, reg, x, b, d);
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

   if (hregClass(insn->variant.store.src) == HRcVec128) {
      vassert(insn->size == 16);
      switch (dst->tag) {
      case S390_AMODE_B12:
      case S390_AMODE_BX12:
         return s390_emit_VST(buf, r, x, b, d);

      default:
         vpanic("s390_insn_store_emit: unknown dst->tag for HRcVec128");
      }
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
      if (dst_class == HRcVec128) {
         return s390_emit_VLR(buf, dst, src);
      }
   } else {
      if (dst_class == HRcFlt64 && src_class == HRcInt64) {
         if (insn->size == 4) {
            buf = s390_emit_SLLG(buf, R0, src, 0, DISP20(32)); /* r0 = src << 32 */
            return s390_emit_LDGRw(buf, dst, R0);
         } else {
            return s390_emit_LDGRw(buf, dst, src);
         }
      }
      if (dst_class == HRcInt64 && src_class == HRcFlt64) {
         if (insn->size == 4) {
            buf = s390_emit_LGDRw(buf, dst, src);
            return s390_emit_SRLG(buf, dst, dst, 0, DISP20(32)); /* dst >>= 32 */
         } else {
            return s390_emit_LGDRw(buf, dst, src);
         }
      }
      if (dst_class == HRcFlt64 && src_class == HRcVec128) {
         return s390_emit_VLR(buf, dst, src);
      }
      /* A move between floating point registers and general purpose
         registers of different size should never occur and indicates
         an error elsewhere. */
   }

   vpanic("s390_insn_move_emit");
}


static UChar *
s390_insn_memcpy_emit(UChar *buf, const s390_insn *insn)
{
   s390_amode *dst = insn->variant.memcpy.dst;
   s390_amode *src = insn->variant.memcpy.src;

   return s390_emit_MVC(buf, insn->size - 1, hregNumber(dst->b), dst->d,
                        hregNumber(src->b), src->d);
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


/* Insert low half of r2 into high half of dst. */
static UChar *
s390_emit_ilih(UChar *buf, UChar size, UChar dst, UChar r2)
{
   if (s390_host_has_gie)
      return s390_emit_RISBG(buf, dst, r2, 64 - 8 * size, 63 - 4 * size,
                             4 * size);

   /* Clear dst's upper half. */
   buf = s390_emit_SLLG(buf, dst, dst, 0, DISP20(64 - 4 * size));
   buf = s390_emit_SRLG(buf, dst, dst, 0, DISP20(64 - 4 * size));

   /* Shift r2 by appropriate amount and OR it into dst. */
   buf = s390_emit_SLLG(buf, R0, r2, 0, DISP20(4 * size));
   return s390_emit_OGR(buf, dst, R0);
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
         case S390_ALU_ILIH: return s390_emit_ilih(buf, insn->size, dst, r2);
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
         case S390_ALU_ILIH: return s390_emit_ilih(buf, 8, dst, r2);
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
         case S390_ALU_ILIH:
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

            case S390_ALU_ILIH:
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

            case S390_ALU_ILIH:
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
            case S390_ALU_ILIH:
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
            case S390_ALU_ILIH:
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
         case S390_ALU_ILIH:
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

         case S390_ALU_ILIH: ; /* avoid GCC warning */
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
         case S390_ALU_ILIH: ; /* avoid GCC warning */
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
         case S390_ALU_ILIH: ; /* avoid GCC warning */
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
               return s390_emit_LB(buf, r1, x, b, DISP20(d));
            else
               return s390_emit_LLCw(buf, r1, x, b, DISP20(d));
         }
         if (insn->size == 8) {
            if (sign_extend)
               return s390_emit_LGB(buf, r1, x, b, DISP20(d));
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
s390_vec_duplicate_emit(UChar *buf, const s390_insn *insn)
{
   UChar v1 = hregNumber(insn->variant.unop.dst);
   s390_opnd_RMI opnd = insn->variant.unop.src;
   UChar r2;

   switch (opnd.tag) {
   case S390_OPND_AMODE: {
      s390_amode* am = opnd.variant.am;
      UInt b = hregNumber(am->b);
      UInt x = hregNumber(am->x);
      UInt d = am->d;

      if (fits_unsigned_12bit(d)) {
         return s390_emit_VLREP(buf, v1, x, b, d,
                                s390_getM_from_size(insn->size));
      }
      buf = s390_emit_load_mem(buf, insn->size, R0, am);
      r2 = R0;
      goto duplicate_from_gpr;
   }

   case S390_OPND_IMMEDIATE: {
      ULong val = opnd.variant.imm;

      if (ulong_fits_signed_16bit(val)) {
         return s390_emit_VREPI(buf, v1, val, s390_getM_from_size(insn->size));
      }
      buf = s390_emit_load_64imm(buf, R0, val);
      r2 = R0;
      goto duplicate_from_gpr;
   }

   case S390_OPND_REG:
      r2 = hregNumber(opnd.variant.reg);

   duplicate_from_gpr:
      buf = s390_emit_VLVGP(buf, v1, r2, r2);
      if (insn->size != 8) {
         buf = s390_emit_VREP(buf, v1, v1, 8 / insn->size - 1,
                              s390_getM_from_size(insn->size));
      }
      return buf;
   }

   vpanic("s390_vec_duplicate_emit");
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
   case S390_VEC_FILL: {
      vassert(insn->variant.unop.src.tag == S390_OPND_IMMEDIATE);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UShort i2 = insn->variant.unop.src.variant.imm;
      return s390_emit_VGBM(buf, v1, i2);
      }
   case S390_VEC_DUPLICATE:  return s390_vec_duplicate_emit(buf, insn);
   case S390_VEC_UNPACKLOWS: {
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      vassert(insn->size < 8);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VUPH(buf, v1, v2, s390_getM_from_size(insn->size));
      }
   case S390_VEC_UNPACKLOWU: {
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      vassert(insn->size < 8);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VUPLH(buf, v1, v2, s390_getM_from_size(insn->size));
      }

   case S390_VEC_ABS:{
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VLP(buf, v1, v2, s390_getM_from_size(insn->size));
   }

   case S390_VEC_COUNT_LEADING_ZEROES:{
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VCLZ(buf, v1, v2, s390_getM_from_size(insn->size));
   }

   case S390_VEC_COUNT_TRAILING_ZEROES:{
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VCTZ(buf, v1, v2, s390_getM_from_size(insn->size));
   }

   case S390_VEC_COUNT_ONES:{
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VPOPCT(buf, v1, v2, s390_getM_from_size(insn->size));
   }

   case S390_VEC_FLOAT_NEG: {
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      vassert(insn->size >= 4);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VFPSO(buf, v1, v2, s390_getM_from_size(insn->size), 0, 0);
   }
   case S390_VEC_FLOAT_ABS: {
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      vassert(insn->size >= 4);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VFPSO(buf, v1, v2, s390_getM_from_size(insn->size), 0, 2);
   }
   case S390_VEC_FLOAT_NABS: {
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      vassert(insn->size >= 4);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VFPSO(buf, v1, v2, s390_getM_from_size(insn->size), 0, 1);
   }
   case S390_VEC_FLOAT_SQRT: {
      vassert(insn->variant.unop.src.tag == S390_OPND_REG);
      vassert(insn->size >= 4);
      UChar v1 = hregNumber(insn->variant.unop.dst);
      UChar v2 = hregNumber(insn->variant.unop.src.variant.reg);
      return s390_emit_VFSQ(buf, v1, v2, s390_getM_from_size(insn->size), 0);
   }
   default:
      vpanic("s390_insn_unop_emit");
   }
}


/* Test operand for zero. */
static UChar *
s390_insn_test_emit(UChar *buf, const s390_insn *insn)
{
   s390_opnd_RMI opnd;

   opnd = insn->variant.test.src;

   switch (opnd.tag) {
   case S390_OPND_REG: {
      UInt reg = hregNumber(opnd.variant.reg);

      switch (insn->size) {
      case 1:
         return s390_emit_TMLL(buf, reg, 0xff);

      case 2:
         return s390_emit_TMLL(buf, reg, 0xffff);

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
      case 1:
         switch (am->tag) {
         case S390_AMODE_B12:
            return s390_emit_TM(buf, 0xff, b, d);
         case S390_AMODE_B20:
            return s390_emit_TMY(buf, 0xff, b, DISP20(d));
         default:
            buf = s390_emit_LB(buf, R0, x, b, DISP20(d));
            return s390_emit_TMLL(buf, R0, 0xff);
         }

      case 2:
         switch (am->tag) {
         case S390_AMODE_B12:
            buf = s390_emit_LH(buf, R0, x, b, d);
            return s390_emit_TMLL(buf, R0, 0xffff);
         default:
            buf = s390_emit_LHY(buf, R0, x, b, DISP20(d));
            return s390_emit_TMLL(buf, R0, 0xffff);
         }

      case 4:
         return s390_emit_LTw(buf, R0, x, b, DISP20(d));

      case 8:
         return s390_emit_LTGw(buf, R0, x, b, DISP20(d));

      default:
         goto fail;
      }
   }

   case S390_OPND_IMMEDIATE: {
      if (opnd.variant.imm == 0)
         return s390_emit_CR(buf, R0, R0);
      else
         return s390_emit_OILL(buf, R0, 1);
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

   /* Make the destination register be -1 or 0, depending on whether
      the relevant condition holds. A 64-bit value is computed. */
   if (cond == S390_CC_ALWAYS)
      return s390_emit_LGHI(buf, r1, -1);  /* r1 = -1 */

   /* If LOCGHI is available, use it. */
   if (s390_host_has_lsc2) {
      /* Clear r1, then load immediate -1 on condition. */
      buf = s390_emit_LGHI(buf, r1, 0);
      if (cond != S390_CC_NEVER)
         buf = s390_emit_LOCGHI(buf, r1, -1, cond);
      return buf;
   }

   buf = s390_emit_load_cc(buf, r1);                 /* r1 = cc */
   buf = s390_emit_LGHI(buf, R0, cond);              /* r0 = mask */
   buf = s390_emit_SLLG(buf, r1, R0, r1, DISP20(60)); /* r1 = mask << (cc+60) */
   buf = s390_emit_SRAG(buf, r1, r1, 0,  DISP20(63)); /* r1 = r1 >> 63 */

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

   vassert(am->tag == S390_AMODE_B12 || am->tag == S390_AMODE_B20);

   switch (insn->size) {
   case 4:
      /* r1 must not be overwritten. So copy it to R0 and let CS clobber it */
      buf = s390_emit_LR(buf, R0, r1);
      if (am->tag == S390_AMODE_B12)
         buf = s390_emit_CS(buf, R0, r3, b, d);
      else
         buf = s390_emit_CSY(buf, R0, r3, b, DISP20(d));
      /* Now copy R0 which has the old memory value to OLD */
      return s390_emit_LR(buf, old, R0);

   case 8:
      /* r1 must not be overwritten. So copy it to R0 and let CS clobber it */
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


/* Only 4-byte and 8-byte operands are handled. */
static UChar *
s390_insn_cdas_emit(UChar *buf, const s390_insn *insn)
{
   UChar r1, r1p1, r3, /*r3p1,*/ b, old_high, old_low, scratch;
   Int d;
   s390_amode *am;
   s390_cdas *cdas = insn->variant.cdas.details;

   r1   = hregNumber(cdas->op1_high); /* expected value */
   r1p1 = hregNumber(cdas->op1_low);  /* expected value */
   r3   = hregNumber(cdas->op3_high);
   /* r3p1 = hregNumber(cdas->op3_low); */ /* unused */
   old_high = hregNumber(cdas->old_mem_high);
   old_low  = hregNumber(cdas->old_mem_low);
   scratch  = hregNumber(cdas->scratch);
   am = cdas->op2;
   b  = hregNumber(am->b);
   d  = am->d;

   vassert(scratch == 1);
   vassert(am->tag == S390_AMODE_B12 || am->tag == S390_AMODE_B20);

   switch (insn->size) {
   case 4:
      /* r1, r1+1 must not be overwritten. So copy them to R0,scratch
         and let CDS/CDSY clobber it */
      buf = s390_emit_LR(buf, R0, r1);
      buf = s390_emit_LR(buf, scratch, r1p1);

      if (am->tag == S390_AMODE_B12)
         buf = s390_emit_CDS(buf, R0, r3, b, d);
      else
         buf = s390_emit_CDSY(buf, R0, r3, b, DISP20(d));

      /* Now copy R0,scratch which has the old memory value to OLD */
      buf = s390_emit_LR(buf, old_high, R0);
      buf = s390_emit_LR(buf, old_low,  scratch);
      return buf;

   case 8:
      /* r1, r1+1 must not be overwritten. So copy them to R0,scratch
         and let CDSG clobber it */
      buf = s390_emit_LGR(buf, R0, r1);
      buf = s390_emit_LGR(buf, scratch, r1p1);

      buf = s390_emit_CDSG(buf, R0, r3, b, DISP20(d));

      /* Now copy R0,scratch which has the old memory value to OLD */
      buf = s390_emit_LGR(buf, old_high, R0);
      buf = s390_emit_LGR(buf, old_low,  scratch);
      return buf;

   default:
      goto fail;
   }

 fail:
   vpanic("s390_insn_cdas_emit");
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
         if (s390_host_has_eimm) {
            if (signed_comparison) {
               if (ulong_fits_signed_32bit(value))
                  return s390_emit_CGFI(buf, r1, value);
            } else {
               if (ulong_fits_unsigned_32bit(value))
                  return s390_emit_CLGFI(buf, r1, value);
            }
         }
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
   signed_multiply = insn->tag == S390_INSN_SMUL;

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
            return s390_emit_MGRK(buf, r1 + 1, r1, r2);
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
            return s390_emit_MG(buf, r1, x, b, DISP20(d));
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
            return s390_emit_MGRK(buf, r1 + 1, r1, R0);
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
   signed_divide = insn->tag == S390_INSN_SDIV;

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


/* Returns a value == BUF to denote failure, != BUF to denote success. */
static UChar *
s390_insn_helper_call_emit(UChar *buf, const s390_insn *insn)
{
   s390_cc_t cond;
   ULong target;
   Int delta;
   s390_helper_call *helper_call = insn->variant.helper_call.details;

   cond = helper_call->cond;
   target = helper_call->target;

   const Bool not_always = (cond != S390_CC_ALWAYS);
   const Bool not_void_return = (helper_call->rloc.pri != RLPri_None);

   /* We have this situation:
      ( *** code in this braces is for  not_always && not_void_return*** )
         ...
         before:
           brc{!cond} else
           call_helper
         preElse:
         ***  j after ***
         else:
         *** load_64imm $0x5555555555555555, %%r2  *** // e.g. for Int RetLoc
         after:
         ...
   */

   // before:
   UChar *pBefore = buf;
   if (not_always) {
      /* 4 bytes (a BRC insn) to be filled in here */
      buf += 4;
   }

   // call_helper
   /* Load the target address into a register, that
      (a) is not used for passing parameters to the helper and
      (b) can be clobbered by the callee
      (c) is not special to the BASR insn
      r1 is the only choice.
      Also, need to arrange for the return address be put into the
      link-register */
   buf = s390_emit_load_64imm(buf, 1, target);

   /* Stash away the client's FPC register because the helper might change it. */
   buf = s390_emit_STFPC(buf, S390_REGNO_STACK_POINTER, S390_OFFSET_SAVED_FPC_C);

   buf = s390_emit_BASR(buf, S390_REGNO_LINK_REGISTER, 1);      // call helper

   buf = s390_emit_LFPC(buf, S390_REGNO_STACK_POINTER,          // restore FPC
                        S390_OFFSET_SAVED_FPC_C);

   // preElse:
   UChar* pPreElse = buf;
   if (not_always && not_void_return) {
      /* 4 bytes (a BRC insn) to be filled in here */
      buf += 4;
   }

   // else:
   UChar* pElse = buf;
   if (not_always && not_void_return) {
      switch (helper_call->rloc.pri) {
      case RLPri_Int:
         buf = s390_emit_load_64imm(buf, S390_REGNO_RETURN_VALUE, 0x5555555555555555ULL);
         break;
      default:
         ppS390Instr(insn, True);
         vpanic("s390_insn_helper_call_emit: invalid conditional RetLoc.");
      }
   }

   // after:
   UChar* pAfter = buf;

   // fill "brc{!cond} else"
   if(not_always)
   {
      delta = pElse - pBefore;
      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(pBefore, s390_cc_invert(cond), delta);
   }

   // fill "brc{ALWAYS} after"
   if (not_always && not_void_return)
   {
      delta = pAfter - pPreElse;
      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(pPreElse, S390_CC_ALWAYS, delta);
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

   if (cond == S390_CC_NEVER) return buf;

   p = buf;

   if (s390_host_has_lsc && hregClass(dst) == HRcInt64) {
      /* LOCx is not the preferred way to implement an unconditional load. */
      if (cond == S390_CC_ALWAYS) goto use_branch_insn;

      switch (src.tag) {
      case S390_OPND_REG:
         return s390_emit_LOCGR(p, cond, hregNumber(dst),
                                hregNumber(src.variant.reg));

      case S390_OPND_AMODE: {
         const s390_amode *am = src.variant.am;

         /* We cannot use LOCx for loads less than 4 bytes. In that case
            load into R0 and then use LOCGR. Do the same if the amode uses
            an index register. */
         if (insn->size < 4 ||
             am->tag == S390_AMODE_BX12 || am->tag == S390_AMODE_BX20) {
            p = s390_emit_load_mem(p, insn->size, R0, am);
            p = s390_emit_LOCGR(p, cond, hregNumber(dst), R0);
            return p;
         }

         vassert(am->tag == S390_AMODE_B12 || am->tag == S390_AMODE_B20);
         vassert(insn->size == 4 || insn->size == 8);

         UInt b = hregNumber(am->b);
         UInt d = am->d;

         if (insn->size == 4) {
            return s390_emit_LOC(p, hregNumber(dst), cond, b, DISP20(d));
         }
         return s390_emit_LOCG(p, hregNumber(dst), cond, b, DISP20(d));
      }

      case S390_OPND_IMMEDIATE: {
         ULong value = src.variant.imm;

         /* If LOCGHI is available, use it. */
         if (s390_host_has_lsc2 && ulong_fits_signed_16bit(value)) {
            return s390_emit_LOCGHI(p, hregNumber(dst), value, cond);
         }

         /* Load value into R0, then use LOCGR */
         if (insn->size <= 4) {
            p = s390_emit_load_32imm(p, R0, value);
            return s390_emit_LOCGR(p, cond, hregNumber(dst), R0);
         }

         vassert(insn->size == 8);
         p = s390_emit_load_64imm(p, R0, value);
         return s390_emit_LOCGR(p, cond, hregNumber(dst), R0);
      }
      }
   }

use_branch_insn:
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
      switch (hregClass(dst)) {
      case HRcInt64:
         p = s390_emit_LGR(p, hregNumber(dst), hregNumber(src.variant.reg));
         break;
      case HRcFlt64:
         p = s390_emit_LDR(p, hregNumber(dst), hregNumber(src.variant.reg));
         break;
      case HRcVec128:
         p = s390_emit_VLR(p, hregNumber(dst), hregNumber(src.variant.reg));
         break;
      default:
         goto fail;
      }
      break;

   case S390_OPND_AMODE:
      if (hregClass(dst) != HRcInt64)
         goto fail;

      p = s390_emit_load_mem(p, insn->size, hregNumber(dst), src.variant.am);
      break;

   case S390_OPND_IMMEDIATE: {
      if (hregClass(dst) != HRcInt64)
         goto fail;

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


static UChar *
s390_insn_bfp_triop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1 = hregNumber(insn->variant.bfp_triop.dst);
   UInt r2 = hregNumber(insn->variant.bfp_triop.op2);
   UInt r3 = hregNumber(insn->variant.bfp_triop.op3);

   switch (insn->size) {
   case 4:
      switch (insn->variant.bfp_triop.tag) {
      case S390_BFP_MADD:  return s390_emit_MAEBR(buf, r1, r3, r2);
      case S390_BFP_MSUB:  return s390_emit_MSEBR(buf, r1, r3, r2);
      default:  goto fail;
      }
      break;

   case 8:
      switch (insn->variant.bfp_triop.tag) {
      case S390_BFP_MADD:  return s390_emit_MADBR(buf, r1, r3, r2);
      case S390_BFP_MSUB:  return s390_emit_MSDBR(buf, r1, r3, r2);
      default:  goto fail;
      }
      break;

   default:  goto fail;
   }

 fail:
   vpanic("s390_insn_bfp_triop_emit");
}


static UChar *
s390_insn_bfp_binop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1 = hregNumber(insn->variant.bfp_binop.dst_hi);
   UInt r2 = hregNumber(insn->variant.bfp_binop.op2_hi);

   switch (insn->size) {
   case 4:
      switch (insn->variant.bfp_binop.tag) {
      case S390_BFP_ADD:     return s390_emit_AEBR(buf, r1, r2);
      case S390_BFP_SUB:     return s390_emit_SEBR(buf, r1, r2);
      case S390_BFP_MUL:     return s390_emit_MEEBR(buf, r1, r2);
      case S390_BFP_DIV:     return s390_emit_DEBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   case 8:
      switch (insn->variant.bfp_binop.tag) {
      case S390_BFP_ADD:     return s390_emit_ADBR(buf, r1, r2);
      case S390_BFP_SUB:     return s390_emit_SDBR(buf, r1, r2);
      case S390_BFP_MUL:     return s390_emit_MDBR(buf, r1, r2);
      case S390_BFP_DIV:     return s390_emit_DDBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   case 16:
      switch (insn->variant.bfp_binop.tag) {
      case S390_BFP_ADD:     return s390_emit_AXBR(buf, r1, r2);
      case S390_BFP_SUB:     return s390_emit_SXBR(buf, r1, r2);
      case S390_BFP_MUL:     return s390_emit_MXBR(buf, r1, r2);
      case S390_BFP_DIV:     return s390_emit_DXBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   default:  goto fail;
   }

 fail:
   vpanic("s390_insn_bfp_binop_emit");
}


static UChar *
s390_insn_bfp_unop_emit(UChar *buf, const s390_insn *insn)
{
   UInt  r1 = hregNumber(insn->variant.bfp_unop.dst_hi);
   UInt  r2 = hregNumber(insn->variant.bfp_unop.op_hi);

   switch (insn->variant.bfp_unop.tag) {
   case S390_BFP_ABS:
      switch (insn->size) {
      case 4:   return s390_emit_LPEBR(buf, r1, r2);
      case 8:   return s390_emit_LPDBR(buf, r1, r2);
      case 16:  return s390_emit_LPXBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   case S390_BFP_NABS:
      switch (insn->size) {
      case 4:   return s390_emit_LNEBR(buf, r1, r2);
      case 8:   return s390_emit_LNDBR(buf, r1, r2);
      case 16:  return s390_emit_LNXBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   case S390_BFP_NEG:
      switch (insn->size) {
      case 4:   return s390_emit_LCEBR(buf, r1, r2);
      case 8:   return s390_emit_LCDBR(buf, r1, r2);
      case 16:  return s390_emit_LCXBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   case S390_BFP_SQRT:
      switch (insn->size) {
      case 4:   return s390_emit_SQEBR(buf, r1, r2);
      case 8:   return s390_emit_SQDBR(buf, r1, r2);
      case 16:  return s390_emit_SQXBR(buf, r1, r2);
      default:  goto fail;
      }
      break;

   default: goto fail;
   }

 fail:
   vpanic("s390_insn_bfp_unop_emit");
}


static UChar *
s390_insn_bfp_convert_emit(UChar *buf, const s390_insn *insn)
{
   UInt  r1 = hregNumber(insn->variant.bfp_convert.dst_hi);
   UInt  r2 = hregNumber(insn->variant.bfp_convert.op_hi);
   s390_bfp_round_t m3 = insn->variant.bfp_convert.rounding_mode;
   /* The IEEE-inexact-exception control is not modelled. So the
      m4 field is 0 (which is what GCC does, too) */
   const UInt m4 = 0;

   switch (insn->variant.bfp_convert.tag) {
      /* Convert to fixed */
   case S390_BFP_F32_TO_I32:  return s390_emit_CFEBR(buf, m3, r1, r2);
   case S390_BFP_F64_TO_I32:  return s390_emit_CFDBR(buf, m3, r1, r2);
   case S390_BFP_F128_TO_I32: return s390_emit_CFXBR(buf, m3, r1, r2);
   case S390_BFP_F32_TO_I64:  return s390_emit_CGEBR(buf, m3, r1, r2);
   case S390_BFP_F64_TO_I64:  return s390_emit_CGDBR(buf, m3, r1, r2);
   case S390_BFP_F128_TO_I64: return s390_emit_CGXBR(buf, m3, r1, r2);

      /* Convert to logical */
   case S390_BFP_F32_TO_U32:  return s390_emit_CLFEBR(buf, m3, m4, r1, r2);
   case S390_BFP_F64_TO_U32:  return s390_emit_CLFDBR(buf, m3, m4, r1, r2);
   case S390_BFP_F128_TO_U32: return s390_emit_CLFXBR(buf, m3, m4, r1, r2);
   case S390_BFP_F32_TO_U64:  return s390_emit_CLGEBR(buf, m3, m4, r1, r2);
   case S390_BFP_F64_TO_U64:  return s390_emit_CLGDBR(buf, m3, m4, r1, r2);
   case S390_BFP_F128_TO_U64: return s390_emit_CLGXBR(buf, m3, m4, r1, r2);

      /* Convert from fixed */
   case S390_BFP_I32_TO_F32:  return s390_emit_CEFBRA(buf, m3, m4, r1, r2);
   case S390_BFP_I32_TO_F64:  return s390_emit_CDFBRA(buf,  0, m4, r1, r2);
   case S390_BFP_I32_TO_F128: return s390_emit_CXFBRA(buf,  0, m4, r1, r2);
   case S390_BFP_I64_TO_F32:  return s390_emit_CEGBRA(buf, m3, m4, r1, r2);
   case S390_BFP_I64_TO_F64:  return s390_emit_CDGBRA(buf, m3, m4, r1, r2);
   case S390_BFP_I64_TO_F128: return s390_emit_CXGBRA(buf,  0, m4, r1, r2);

      /* Convert from logical */
   case S390_BFP_U32_TO_F32:  return s390_emit_CELFBR(buf, m3, m4, r1, r2);
   case S390_BFP_U32_TO_F64:  return s390_emit_CDLFBR(buf, m3, m4, r1, r2);
   case S390_BFP_U32_TO_F128: return s390_emit_CXLFBR(buf, m3, m4, r1, r2);
   case S390_BFP_U64_TO_F32:  return s390_emit_CELGBR(buf, m3, m4, r1, r2);
   case S390_BFP_U64_TO_F64:  return s390_emit_CDLGBR(buf, m3, m4, r1, r2);
   case S390_BFP_U64_TO_F128: return s390_emit_CXLGBR(buf, m3, m4, r1, r2);

      /* Load lengthened */
   case S390_BFP_F32_TO_F64:  return s390_emit_LDEBR(buf, r1, r2);
   case S390_BFP_F32_TO_F128: return s390_emit_LXEBR(buf, r1, r2);
   case S390_BFP_F64_TO_F128: return s390_emit_LXDBR(buf, r1, r2);

      /* Load rounded */
   case S390_BFP_F64_TO_F32:  return s390_emit_LEDBRA(buf, m3, m4, r1, r2);
   case S390_BFP_F128_TO_F32: return s390_emit_LEXBRA(buf, m3, m4, r1, r2);
   case S390_BFP_F128_TO_F64: return s390_emit_LDXBRA(buf, m3, m4, r1, r2);

      /* Load FP integer */
   case S390_BFP_F32_TO_F32I: return s390_emit_FIEBRA(buf, m3, m4, r1, r2);
   case S390_BFP_F64_TO_F64I: return s390_emit_FIDBRA(buf, m3, m4, r1, r2);
   case S390_BFP_F128_TO_F128I: return s390_emit_FIXBRA(buf, m3, m4, r1, r2);

   default: goto fail;
   }

 fail:
   vpanic("s390_insn_bfp_convert_emit");
}


static UChar *
s390_insn_bfp_compare_emit(UChar *buf, const s390_insn *insn)
{
   UInt dst = hregNumber(insn->variant.bfp_compare.dst);
   UInt r1  = hregNumber(insn->variant.bfp_compare.op1_hi);
   UInt r2  = hregNumber(insn->variant.bfp_compare.op2_hi);

   switch (insn->size) {
   case 4:  buf = s390_emit_CEBR(buf, r1, r2); break;
   case 8:  buf = s390_emit_CDBR(buf, r1, r2); break;
   case 16: buf = s390_emit_CXBR(buf, r1, r2); break;
   default:  goto fail;
   }

   return s390_emit_load_cc(buf, dst);  /* Load condition code into DST */

 fail:
   vpanic("s390_insn_bfp_compare_emit");
}


static UChar *
s390_insn_dfp_binop_emit(UChar *buf, const s390_insn *insn)
{
   s390_dfp_binop *dfp_binop = insn->variant.dfp_binop.details;

   UInt r1 = hregNumber(dfp_binop->dst_hi);
   UInt r2 = hregNumber(dfp_binop->op2_hi);
   UInt r3 = hregNumber(dfp_binop->op3_hi);
   s390_dfp_round_t m4 = dfp_binop->rounding_mode;

   switch (insn->size) {
   case 8:
      switch (dfp_binop->tag) {
      case S390_DFP_ADD: return s390_emit_ADTRA(buf, r3, m4, r1, r2);
      case S390_DFP_SUB: return s390_emit_SDTRA(buf, r3, m4, r1, r2);
      case S390_DFP_MUL: return s390_emit_MDTRA(buf, r3, m4, r1, r2);
      case S390_DFP_DIV: return s390_emit_DDTRA(buf, r3, m4, r1, r2);
      case S390_DFP_QUANTIZE: return s390_emit_QADTR(buf, r3, m4, r1, r2);
      default:  goto fail;
      }
      break;

   case 16:
      switch (dfp_binop->tag) {
      case S390_DFP_ADD:     return s390_emit_AXTRA(buf, r3, m4, r1, r2);
      case S390_DFP_SUB:     return s390_emit_SXTRA(buf, r3, m4, r1, r2);
      case S390_DFP_MUL:     return s390_emit_MXTRA(buf, r3, m4, r1, r2);
      case S390_DFP_DIV:     return s390_emit_DXTRA(buf, r3, m4, r1, r2);
      case S390_DFP_QUANTIZE: return s390_emit_QAXTR(buf, r3, m4, r1, r2);
      default:  goto fail;
      }
      break;

   default:  goto fail;
   }

 fail:
   vpanic("s390_insn_dfp_binop_emit");
}


static UChar *
s390_insn_dfp_reround_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1 = hregNumber(insn->variant.dfp_reround.dst_hi);
   UInt r2 = hregNumber(insn->variant.dfp_reround.op2);
   UInt r3 = hregNumber(insn->variant.dfp_reround.op3_hi);
   s390_dfp_round_t m4 = insn->variant.dfp_reround.rounding_mode;

   switch (insn->size) {
   case 8:
      return s390_emit_RRDTR(buf, r3, m4, r1, r2);

   case 16:
      return s390_emit_RRXTR(buf, r3, m4, r1, r2);

   default: goto fail;
   }
 fail:
   vpanic("s390_insn_dfp_reround_emit");
}


static UChar *
s390_insn_dfp_unop_emit(UChar *buf, const s390_insn *insn)
{
   UInt  r1 = hregNumber(insn->variant.dfp_unop.dst_hi);
   UInt  r2 = hregNumber(insn->variant.dfp_unop.op_hi);

   switch (insn->variant.dfp_unop.tag) {
   case S390_DFP_EXTRACT_EXP_D64:  return s390_emit_EEDTR(buf, r1, r2); break;
   case S390_DFP_EXTRACT_EXP_D128: return s390_emit_EEXTR(buf, r1, r2); break;
   case S390_DFP_EXTRACT_SIG_D64:  return s390_emit_ESDTR(buf, r1, r2); break;
   case S390_DFP_EXTRACT_SIG_D128: return s390_emit_ESXTR(buf, r1, r2); break;
   default: goto fail;
   }
 fail:
   vpanic("s390_insn_dfp_unop_emit");
}


static UChar *
s390_insn_dfp_intop_emit(UChar *buf, const s390_insn *insn)
{
   UInt r1 = hregNumber(insn->variant.dfp_intop.dst_hi);
   UInt r2 = hregNumber(insn->variant.dfp_intop.op2);
   UInt r3 = hregNumber(insn->variant.dfp_intop.op3_hi);

   switch (insn->size) {
   case 8:
      switch (insn->variant.dfp_intop.tag) {
      case S390_DFP_SHIFT_LEFT:  return s390_emit_SLDT(buf, r3, r1, r2);
      case S390_DFP_SHIFT_RIGHT: return s390_emit_SRDT(buf, r3, r1, r2);
      case S390_DFP_INSERT_EXP:  return s390_emit_IEDTR(buf, r3, r1, r2);
      default:  goto fail;
      }
      break;

   case 16:
      switch (insn->variant.dfp_intop.tag) {
      case S390_DFP_SHIFT_LEFT:  return s390_emit_SLXT(buf, r3, r1, r2);
      case S390_DFP_SHIFT_RIGHT: return s390_emit_SRXT(buf, r3, r1, r2);
      case S390_DFP_INSERT_EXP:  return s390_emit_IEXTR(buf, r3, r1, r2);
      default:  goto fail;
      }
      break;

   default: goto fail;
   }

 fail:
   vpanic("s390_insn_dfp_intop_emit");
}


static UChar *
s390_insn_dfp_compare_emit(UChar *buf, const s390_insn *insn)
{
   UInt dst = hregNumber(insn->variant.dfp_compare.dst);
   UInt r1  = hregNumber(insn->variant.dfp_compare.op1_hi);
   UInt r2  = hregNumber(insn->variant.dfp_compare.op2_hi);

   switch (insn->size) {
   case 8:
      switch(insn->variant.dfp_compare.tag) {
      case S390_DFP_COMPARE:     buf = s390_emit_CDTR(buf, r1, r2); break;
      case S390_DFP_COMPARE_EXP: buf = s390_emit_CEDTR(buf, r1, r2); break;
      default: goto fail;
      }
      break;

   case 16:
      switch(insn->variant.dfp_compare.tag) {
      case S390_DFP_COMPARE:     buf = s390_emit_CXTR(buf, r1, r2); break;
      case S390_DFP_COMPARE_EXP: buf = s390_emit_CEXTR(buf, r1, r2); break;
      default: goto fail;
      }
      break;

   default:  goto fail;
   }

   return s390_emit_load_cc(buf, dst);  /* Load condition code into DST */

 fail:
   vpanic("s390_insn_dfp_compare_emit");
}


static UChar *
s390_insn_dfp_convert_emit(UChar *buf, const s390_insn *insn)
{
   UInt  r1 = hregNumber(insn->variant.dfp_convert.dst_hi);
   UInt  r2 = hregNumber(insn->variant.dfp_convert.op_hi);
   s390_dfp_round_t m3 = insn->variant.dfp_convert.rounding_mode;
   /* The IEEE-inexact-exception control is not modelled. So the
      m4 field is 0 (which is what GCC does, too) */
   const UInt m4 = 0;

   switch (insn->variant.dfp_convert.tag) {

      /* Convert to fixed */
   case S390_DFP_D64_TO_I32:  return s390_emit_CFDTR(buf, m3, m4, r1, r2);
   case S390_DFP_D128_TO_I32: return s390_emit_CFXTR(buf, m3, m4, r1, r2);
   case S390_DFP_D64_TO_I64:  return s390_emit_CGDTR(buf, m3, m4, r1, r2);
   case S390_DFP_D128_TO_I64: return s390_emit_CGXTR(buf, m3, m4, r1, r2);

      /* Convert to logical */
   case S390_DFP_D64_TO_U32:  return s390_emit_CLFDTR(buf, m3, m4, r1, r2);
   case S390_DFP_D128_TO_U32: return s390_emit_CLFXTR(buf, m3, m4, r1, r2);
   case S390_DFP_D64_TO_U64:  return s390_emit_CLGDTR(buf, m3, m4, r1, r2);
   case S390_DFP_D128_TO_U64: return s390_emit_CLGXTR(buf, m3, m4, r1, r2);

      /* Convert from fixed */
   case S390_DFP_I32_TO_D64:  return s390_emit_CDFTR(buf, 0, m4, r1, r2);
   case S390_DFP_I32_TO_D128: return s390_emit_CXFTR(buf, 0, m4, r1, r2);
   case S390_DFP_I64_TO_D64:  return s390_emit_CDGTRA(buf, m3, m4, r1, r2);
   case S390_DFP_I64_TO_D128: return s390_emit_CXGTR(buf, 0, m4, r1, r2);

      /* Convert from logical */
   case S390_DFP_U32_TO_D64:  return s390_emit_CDLFTR(buf, m3, m4, r1, r2);
   case S390_DFP_U64_TO_D64:  return s390_emit_CDLGTR(buf, m3, m4, r1, r2);
   case S390_DFP_U32_TO_D128: return s390_emit_CXLFTR(buf, m3, m4, r1, r2);
   case S390_DFP_U64_TO_D128: return s390_emit_CXLGTR(buf, m3, m4, r1, r2);

      /* Load lengthened */
   case S390_DFP_D32_TO_D64:   return s390_emit_LDETR(buf, m4, r1, r2);
   case S390_DFP_D64_TO_D128:  return s390_emit_LXDTR(buf, m4, r1, r2);

      /* Load rounded */
   case S390_DFP_D64_TO_D32:   return s390_emit_LEDTR(buf, m3, m4, r1, r2);
   case S390_DFP_D128_TO_D64:  return s390_emit_LDXTR(buf, m3, m4, r1, r2);

   default: goto fail;
   }

 fail:
   vpanic("s390_insn_dfp_convert_emit");
}


static UChar *
s390_insn_fp_convert_emit(UChar *buf, const s390_insn *insn)
{
   UInt pfpo;
   s390_fp_convert *fp_convert = insn->variant.fp_convert.details;
   s390_dfp_round_t rm = fp_convert->rounding_mode;

   vassert(rm < 2 || rm > 7);

   switch (fp_convert->tag) {
   case S390_FP_F32_TO_D32:   pfpo = S390_PFPO_F32_TO_D32   << 8; break;
   case S390_FP_F32_TO_D64:   pfpo = S390_PFPO_F32_TO_D64   << 8; break;
   case S390_FP_F32_TO_D128:  pfpo = S390_PFPO_F32_TO_D128  << 8; break;
   case S390_FP_F64_TO_D32:   pfpo = S390_PFPO_F64_TO_D32   << 8; break;
   case S390_FP_F64_TO_D64:   pfpo = S390_PFPO_F64_TO_D64   << 8; break;
   case S390_FP_F64_TO_D128:  pfpo = S390_PFPO_F64_TO_D128  << 8; break;
   case S390_FP_F128_TO_D32:  pfpo = S390_PFPO_F128_TO_D32  << 8; break;
   case S390_FP_F128_TO_D64:  pfpo = S390_PFPO_F128_TO_D64  << 8; break;
   case S390_FP_F128_TO_D128: pfpo = S390_PFPO_F128_TO_D128 << 8; break;
   case S390_FP_D32_TO_F32:   pfpo = S390_PFPO_D32_TO_F32   << 8; break;
   case S390_FP_D32_TO_F64:   pfpo = S390_PFPO_D32_TO_F64   << 8; break;
   case S390_FP_D32_TO_F128:  pfpo = S390_PFPO_D32_TO_F128  << 8; break;
   case S390_FP_D64_TO_F32:   pfpo = S390_PFPO_D64_TO_F32   << 8; break;
   case S390_FP_D64_TO_F64:   pfpo = S390_PFPO_D64_TO_F64   << 8; break;
   case S390_FP_D64_TO_F128:  pfpo = S390_PFPO_D64_TO_F128  << 8; break;
   case S390_FP_D128_TO_F32:  pfpo = S390_PFPO_D128_TO_F32  << 8; break;
   case S390_FP_D128_TO_F64:  pfpo = S390_PFPO_D128_TO_F64  << 8; break;
   case S390_FP_D128_TO_F128: pfpo = S390_PFPO_D128_TO_F128 << 8; break;
   default: goto fail;
   }

   pfpo = pfpo | rm;
   buf = s390_emit_load_32imm(buf, R0, pfpo);
   buf = s390_emit_PFPO(buf);
   return buf;

 fail:
   vpanic("s390_insn_fp_convert_emit");
}


static UChar *
s390_insn_mfence_emit(UChar *buf, const s390_insn *insn)
{
   return s390_emit_BCR(buf, 0xF, 0x0);
}


static UChar *
s390_insn_mimm_emit(UChar *buf, const s390_insn *insn)
{
   s390_amode *am = insn->variant.mimm.dst;
   UChar b = hregNumber(am->b);
   Int   d = am->d;
   ULong value = insn->variant.mimm.value;

   if (value == 0) {
      return s390_emit_XC(buf, insn->size - 1, b, d, b, d);
   }

   if (insn->size == 1) {
      return s390_emit_MVI(buf, value & 0xFF, b, d);
   }

   if (s390_host_has_gie && ulong_fits_signed_16bit(value)) {
      value &= 0xFFFF;
      switch (insn->size) {
      case 2: return s390_emit_MVHHI(buf, b, d, value);
      case 4: return s390_emit_MVHI(buf,  b, d, value);
      case 8: return s390_emit_MVGHI(buf, b, d, value);
      }
   } else {
      // Load value to R0, then store.
      switch (insn->size) {
      case 2:
         buf = s390_emit_LHI(buf, R0, value & 0xFFFF);
         return s390_emit_STH(buf, R0, 0, b, d);
      case 4:
         buf = s390_emit_load_32imm(buf, R0, value);
         return s390_emit_ST(buf, R0, 0, b, d);
      case 8:
         buf = s390_emit_load_64imm(buf, R0, value);
         return s390_emit_STG(buf, R0, 0, b, DISP20(d));
      }
   }
   
   vpanic("s390_insn_mimm_emit");
}


static UChar *
s390_insn_madd_emit(UChar *buf, const s390_insn *insn)
{
   s390_amode *am = insn->variant.madd.dst;
   UChar b = hregNumber(am->b);
   Int   d = am->d;

   if (insn->size == 4) {
      return s390_emit_ASI(buf, insn->variant.madd.delta, b, DISP20(d));
   }

   return s390_emit_AGSI(buf, insn->variant.madd.delta, b, DISP20(d));
}


static UChar *
s390_insn_set_fpc_bfprm_emit(UChar *buf, const s390_insn *insn)
{
   UInt mode = hregNumber(insn->variant.set_fpc_bfprm.mode);

   /* Copy FPC from guest state to R0 and OR in the new rounding mode */
   buf = s390_emit_L(buf, R0, 0, S390_REGNO_GUEST_STATE_POINTER,
                     S390X_GUEST_OFFSET(guest_fpc));   // r0 = guest_fpc

   buf = s390_emit_NILL(buf, R0, 0xFFF8); /* Clear out right-most 3 bits */
   buf = s390_emit_OR(buf, R0, mode);     /* OR in the new rounding mode */
   buf = s390_emit_SFPC(buf, R0);         /* Load FPC register from R0 */

   return buf;
}


static UChar *
s390_insn_set_fpc_dfprm_emit(UChar *buf, const s390_insn *insn)
{
   UInt mode = hregNumber(insn->variant.set_fpc_dfprm.mode);

   /* Copy FPC from guest state to R0 and OR in the new rounding mode */
   buf = s390_emit_L(buf, R0, 0, S390_REGNO_GUEST_STATE_POINTER,
                     S390X_GUEST_OFFSET(guest_fpc));   // r0 = guest_fpc

   /* DFP rounding mode is set at bit position 25:27 in FPC register */
   buf = s390_emit_NILL(buf, R0, 0xFF8F); /* Clear out 25:27 bits */
   buf = s390_emit_SLL(buf, mode, 0, 4);  /* bring mode to 25:27 bits */
   buf = s390_emit_OR(buf, R0, mode);     /* OR in the new rounding mode */
   buf = s390_emit_SFPC(buf, R0);         /* Load FPC register from R0 */

   return buf;
}


/* Define convenience functions needed for translation chaining.
   Any changes need to be applied to the functions in concert. */

static __inline__ Bool
s390_insn_is_BRCL(const UChar *p, UChar condition)
{
   return p[0] == 0xc0 && p[1] == ((condition << 4) | 0x04);
}

static __inline__ Bool
s390_insn_is_BR(const UChar *p, UChar reg)
{
   return p[0] == 0x07 && p[1] == (0xF0 | reg);  /* BCR 15,reg */
}


/* The length of the BASR insn */
#define S390_BASR_LEN  2


/* Load the 64-bit VALUE into REG. Note that this function must NOT
   optimise the generated code by looking at the value. I.e. using
   LGHI if value == 0 would be very wrong. */
static UChar *
s390_tchain_load64(UChar *buf, UChar regno, ULong value)
{
   UChar *begin = buf;

   if (s390_host_has_eimm) {
      /* Do it in two steps: upper half [0:31] and lower half [32:63] */
      buf = s390_emit_IIHF(buf, regno, value >> 32);
      buf = s390_emit_IILF(buf, regno, value & 0xFFFFFFFF);
   } else {
      buf = s390_emit_IILL(buf, regno, value & 0xFFFF);
      value >>= 16;
      buf = s390_emit_IILH(buf, regno, value & 0xFFFF);
      value >>= 16;
      buf = s390_emit_IIHL(buf, regno, value & 0xFFFF);
      value >>= 16;
      buf = s390_emit_IIHH(buf, regno, value & 0xFFFF);
   }

   vassert(buf - begin == s390_tchain_load64_len());

   return buf;
}

/* Return number of bytes generated by s390_tchain_load64 */
static UInt
s390_tchain_load64_len(void)
{
   if (s390_host_has_eimm) {
      return 6 + 6;      /* IIHF + IILF */
   }
   return 4 + 4 + 4 + 4; /* IIHH + IIHL + IILH + IILL */
}

/* Verify that CODE is the code sequence generated by s390_tchain_load64
   to load VALUE into REGNO. Return pointer to the byte following the
   insn sequence. */
static const UChar *
s390_tchain_verify_load64(const UChar *code, UChar regno, ULong value)
{
   UInt regmask = regno << 4;
   UInt hw;

   if (s390_host_has_eimm) {
      /* Check for IIHF */
      vassert(code[0]  ==  0xC0);
      vassert(code[1]  == (0x08 | regmask));
      vassert(*(const UInt *)&code[2] == (value >> 32));
      /* Check for IILF */
      vassert(code[6]  ==  0xC0);
      vassert(code[7]  == (0x09 | regmask));
      vassert(*(const UInt *)&code[8] == (value & 0xFFFFFFFF));
   } else {
      /* Check for IILL */
      hw = value & 0xFFFF;
      vassert(code[0]  ==  0xA5);
      vassert(code[1]  == (0x03 | regmask));
      vassert(code[2]  == (hw >> 8));
      vassert(code[3]  == (hw & 0xFF));

      /* Check for IILH */
      hw = (value >> 16) & 0xFFFF;
      vassert(code[4]  ==  0xA5);
      vassert(code[5]  == (0x02 | regmask));
      vassert(code[6]  == (hw >> 8));
      vassert(code[7]  == (hw & 0xFF));

      /* Check for IIHL */
      hw = (value >> 32) & 0xFFFF;
      vassert(code[8]  ==  0xA5);
      vassert(code[9]  == (0x01 | regmask));
      vassert(code[10] == (hw >> 8));
      vassert(code[11] == (hw & 0xFF));

      /* Check for IIHH */
      hw = (value >> 48) & 0xFFFF;
      vassert(code[12] ==  0xA5);
      vassert(code[13] == (0x00 | regmask));
      vassert(code[14] == (hw >> 8));
      vassert(code[15] == (hw & 0xFF));
   }

   return code + s390_tchain_load64_len();
}

/* CODE points to the code sequence as generated by s390_tchain_load64.
   Change the loaded value to IMM64. Return pointer to the byte following
   the patched code sequence. */
static UChar *
s390_tchain_patch_load64(UChar *code, ULong imm64)
{
   if (s390_host_has_eimm) {
      /* Patch IIHF */
      *(UInt *)&code[2] = imm64 >> 32;
      /* Patch IILF */
      *(UInt *)&code[8] = imm64 & 0xFFFFFFFF;
   } else {
      code[3]  = imm64 & 0xFF; imm64 >>= 8;
      code[2]  = imm64 & 0xFF; imm64 >>= 8;
      code[7]  = imm64 & 0xFF; imm64 >>= 8;
      code[6]  = imm64 & 0xFF; imm64 >>= 8;
      code[11] = imm64 & 0xFF; imm64 >>= 8;
      code[10] = imm64 & 0xFF; imm64 >>= 8;
      code[15] = imm64 & 0xFF; imm64 >>= 8;
      code[14] = imm64 & 0xFF; imm64 >>= 8;
   }

   return code + s390_tchain_load64_len();
}


/* NB: what goes on here has to be very closely coordinated with the
   chainXDirect_S390 and unchainXDirect_S390 below. */
static UChar *
s390_insn_xdirect_emit(UChar *buf, const s390_insn *insn,
                       const void *disp_cp_chain_me_to_slowEP,
                       const void *disp_cp_chain_me_to_fastEP)
{
   /* We're generating chain-me requests here, so we need to be
      sure this is actually allowed -- no-redir translations can't
      use chain-me's.  Hence: */
   vassert(disp_cp_chain_me_to_slowEP != NULL);
   vassert(disp_cp_chain_me_to_fastEP != NULL);

   /* Use ptmp for backpatching conditional jumps. */
   UChar *ptmp = buf;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it. */
   s390_cc_t cond = insn->variant.xdirect.cond;

   if (cond != S390_CC_ALWAYS) {
      /* So we have something like this
         if (cond) do_xdirect;
         Y: ...
         We convert this into
         if (! cond) goto Y;        // BRC opcode; 4 bytes
         do_xdirect;
         Y:
      */
      /* 4 bytes (a BRC insn) to be filled in here */
      buf += 4;
   }

   /* Update the guest IA. */
   buf = s390_emit_load_64imm(buf, R0, insn->variant.xdirect.dst);

   const s390_amode *amode = insn->variant.xdirect.guest_IA;
   vassert(amode->tag == S390_AMODE_B12);
   UInt b = hregNumber(amode->b);
   UInt d = amode->d;

   buf = s390_emit_STG(buf, R0, 0, b, DISP20(d));

   /* Load the chosen entry point into the scratch reg */
   const void *disp_cp_chain_me;

   disp_cp_chain_me =
      insn->variant.xdirect.to_fast_entry ? disp_cp_chain_me_to_fastEP 
                                          : disp_cp_chain_me_to_slowEP;
   /* Get the address of the beginning of the load64 code sequence into %r1.
      Do not change the register! This is part of the protocol with the
      dispatcher. */
   buf = s390_emit_BASR(buf, 1, R0);

   /* --- FIRST PATCHABLE BYTE follows (must not modify %r1) --- */
   Addr64 addr = (Addr)disp_cp_chain_me;
   buf = s390_tchain_load64(buf, S390_REGNO_TCHAIN_SCRATCH, addr);

   /* goto *tchain_scratch */
   buf = s390_emit_BCR(buf, S390_CC_ALWAYS, S390_REGNO_TCHAIN_SCRATCH);

   /* --- END of PATCHABLE BYTES --- */

   /* Fix up the conditional jump, if there was one. */
   if (cond != S390_CC_ALWAYS) {
      Int delta = buf - ptmp;

      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(ptmp, s390_cc_invert(cond), delta);
   }

   return buf;
}

/* Return the number of patchable bytes from an xdirect insn. */
static UInt
s390_xdirect_patchable_len(void)
{
   return s390_tchain_load64_len() + S390_BASR_LEN;
}


static UChar *
s390_insn_xindir_emit(UChar *buf, const s390_insn *insn,
                      const void *disp_cp_xindir)
{
   /* We're generating transfers that could lead indirectly to a
      chain-me, so we need to be sure this is actually allowed --
      no-redir translations are not allowed to reach normal
      translations without going through the scheduler.  That means
      no XDirects or XIndirs out from no-redir translations.
      Hence: */
   vassert(disp_cp_xindir != NULL);

   /* Use ptmp for backpatching conditional jumps. */
   UChar *ptmp = buf;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it. */
   s390_cc_t cond = insn->variant.xdirect.cond;

   if (cond != S390_CC_ALWAYS) {
      /* So we have something like this
         if (cond) do_xdirect;
         Y: ...
         We convert this into
         if (! cond) goto Y;        // BRC opcode; 4 bytes
         do_xdirect;
         Y:
      */
      /* 4 bytes (a BRC insn) to be filled in here */
      buf += 4;
   }

   /* Update the guest IA with the address in xdirect.dst. */
   const s390_amode *amode = insn->variant.xindir.guest_IA;

   vassert(amode->tag == S390_AMODE_B12);
   UInt b = hregNumber(amode->b);
   UInt d = amode->d;
   UInt regno = hregNumber(insn->variant.xindir.dst);

   buf = s390_emit_STG(buf, regno, 0, b, DISP20(d));

   /* load tchain_scratch, #disp_indir */
   buf = s390_tchain_load64(buf, S390_REGNO_TCHAIN_SCRATCH,
                            (Addr)disp_cp_xindir);
   /* goto *tchain_direct */
   buf = s390_emit_BCR(buf, S390_CC_ALWAYS, S390_REGNO_TCHAIN_SCRATCH);

   /* Fix up the conditional jump, if there was one. */
   if (cond != S390_CC_ALWAYS) {
      Int delta = buf - ptmp;

      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(ptmp, s390_cc_invert(cond), delta);
   }

   return buf;
}

static UChar *
s390_insn_xassisted_emit(UChar *buf, const s390_insn *insn,
                         const void *disp_cp_xassisted)
{
   /* Use ptmp for backpatching conditional jumps. */
   UChar *ptmp = buf;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it. */
   s390_cc_t cond = insn->variant.xdirect.cond;

   if (cond != S390_CC_ALWAYS) {
      /* So we have something like this
         if (cond) do_xdirect;
         Y: ...
         We convert this into
         if (! cond) goto Y;        // BRC opcode; 4 bytes
         do_xdirect;
         Y:
      */
      /* 4 bytes (a BRC insn) to be filled in here */
      buf += 4;
   }

   /* Update the guest IA with the address in xassisted.dst. */
   const s390_amode *amode = insn->variant.xassisted.guest_IA;

   vassert(amode->tag == S390_AMODE_B12);
   UInt b = hregNumber(amode->b);
   UInt d = amode->d;
   UInt regno = hregNumber(insn->variant.xassisted.dst);

   buf = s390_emit_STG(buf, regno, 0, b, DISP20(d));

   UInt trcval = 0;

   switch (insn->variant.xassisted.kind) {
   case Ijk_ClientReq:   trcval = VEX_TRC_JMP_CLIENTREQ;   break;
   case Ijk_Sys_syscall: trcval = VEX_TRC_JMP_SYS_SYSCALL; break;
   case Ijk_Yield:       trcval = VEX_TRC_JMP_YIELD;       break;
   case Ijk_EmWarn:      trcval = VEX_TRC_JMP_EMWARN;      break;
   case Ijk_EmFail:      trcval = VEX_TRC_JMP_EMFAIL;      break;
   case Ijk_MapFail:     trcval = VEX_TRC_JMP_MAPFAIL;     break;
   case Ijk_NoDecode:    trcval = VEX_TRC_JMP_NODECODE;    break;
   case Ijk_InvalICache: trcval = VEX_TRC_JMP_INVALICACHE; break;
   case Ijk_NoRedir:     trcval = VEX_TRC_JMP_NOREDIR;     break;
   case Ijk_SigTRAP:     trcval = VEX_TRC_JMP_SIGTRAP;     break;
   case Ijk_SigFPE:      trcval = VEX_TRC_JMP_SIGFPE;      break;
   case Ijk_SigSEGV:     trcval = VEX_TRC_JMP_SIGSEGV;     break;
   case Ijk_Boring:      trcval = VEX_TRC_JMP_BORING;      break;
      /* We don't expect to see the following being assisted. */
   case Ijk_Ret:
   case Ijk_Call:
      /* fallthrough */
   default: 
      ppIRJumpKind(insn->variant.xassisted.kind);
      vpanic("s390_insn_xassisted_emit: unexpected jump kind");
   }

   vassert(trcval != 0);

   /* guest_state_pointer = trcval */
   buf = s390_emit_LGHI(buf, S390_REGNO_GUEST_STATE_POINTER, trcval);

   /* load tchain_scratch, #disp_assisted */
   buf = s390_tchain_load64(buf, S390_REGNO_TCHAIN_SCRATCH,
                            (Addr)disp_cp_xassisted);

   /* goto *tchain_direct */
   buf = s390_emit_BCR(buf, S390_CC_ALWAYS, S390_REGNO_TCHAIN_SCRATCH);

   /* Fix up the conditional jump, if there was one. */
   if (cond != S390_CC_ALWAYS) {
      Int delta = buf - ptmp;

      delta >>= 1;  /* immediate constant is #half-words */
      vassert(delta > 0 && delta < (1 << 16));
      s390_emit_BRC(ptmp, s390_cc_invert(cond), delta);
   }

   return buf;
}


/* Pseudo code:

   guest_state[host_EvC_COUNTER] -= 1;
   if (guest_state[host_EvC_COUNTER] >= 0) goto nofail;
   goto guest_state[host_EvC_FAILADDR];
   nofail: ;

   The dispatch counter is a 32-bit value. */
static UChar *
s390_insn_evcheck_emit(UChar *buf, const s390_insn *insn,
                       VexEndness endness_host)
{
   s390_amode *amode;
   UInt b, d;
   UChar *code_begin, *code_end;

   code_begin = buf;

   amode = insn->variant.evcheck.counter;
   vassert(amode->tag == S390_AMODE_B12);
   b = hregNumber(amode->b);
   d = amode->d;

   /* Decrement the dispatch counter in the guest state */
   if (s390_host_has_gie) {
      buf = s390_emit_ASI(buf, -1, b, DISP20(d));   /* 6 bytes */
   } else {
      buf = s390_emit_LHI(buf, R0, -1);             /* 4 bytes */
      buf = s390_emit_A(buf, R0, 0, b, d);          /* 4 bytes */
      buf = s390_emit_ST(buf, R0, 0, b, d);         /* 4 bytes */
   }

   /* Jump over the next insn if >= 0 */
   buf = s390_emit_BRC(buf, S390_CC_HE, (4 + 6 + 2) / 2);  /* 4 bytes */

   /* Computed goto to fail_address */
   amode = insn->variant.evcheck.fail_addr;
   b = hregNumber(amode->b);
   d = amode->d;
   buf = s390_emit_LG(buf, S390_REGNO_TCHAIN_SCRATCH, 0, b, DISP20(d));  /* 6 bytes */
   buf = s390_emit_BCR(buf, S390_CC_ALWAYS, S390_REGNO_TCHAIN_SCRATCH);  /* 2 bytes */

   code_end = buf;
   
   /* Make sure the size of the generated code is identical to the size
      returned by evCheckSzB_S390 */
   vassert(evCheckSzB_S390() == code_end - code_begin);

   return buf;
}


static UChar *
s390_insn_profinc_emit(UChar *buf,
                       const s390_insn *insn __attribute__((unused)))
{
   /* Generate a code template to increment a memory location whose
      address will be known later as an immediate value. This code
      template will be patched once the memory location is known.
      For now we do this with address == 0. */
   buf = s390_tchain_load64(buf, S390_REGNO_TCHAIN_SCRATCH, 0);
   if (s390_host_has_gie) {
      buf = s390_emit_AGSI(buf, 1, S390_REGNO_TCHAIN_SCRATCH, DISP20(0));
   } else {
      buf = s390_emit_LGHI(buf, R0, 1);
      buf = s390_emit_AG( buf, R0, 0, S390_REGNO_TCHAIN_SCRATCH, DISP20(0));
      buf = s390_emit_STG(buf, R0, 0, S390_REGNO_TCHAIN_SCRATCH, DISP20(0));
   }

   return buf;
}


static UChar *
s390_insn_vec_amodeop_emit(UChar *buf, const s390_insn *insn)
{
   UChar v1 = hregNumber(insn->variant.vec_amodeop.dst);
   UChar v2 = hregNumber(insn->variant.vec_amodeop.op1);
   s390_amode* op2 = insn->variant.vec_amodeop.op2;

   vassert(hregNumber(op2->x) == 0);
   vassert(fits_unsigned_12bit(op2->d));

   UChar b = hregNumber(op2->b);
   UShort d = op2->d;


   switch (insn->variant.vec_amodeop.tag) {
   case S390_VEC_GET_ELEM:
      return s390_emit_VLGV(buf, v1, b, d, v2, s390_getM_from_size(insn->size));

   case S390_VEC_ELEM_SHL_INT:
      return s390_emit_VESL(buf, v1, b, d, v2, s390_getM_from_size(insn->size));

   case S390_VEC_ELEM_SHRA_INT:
      return s390_emit_VESRA(buf, v1, b, d, v2, s390_getM_from_size(insn->size));

   case S390_VEC_ELEM_SHRL_INT:
      return s390_emit_VESRL(buf, v1, b, d, v2, s390_getM_from_size(insn->size));

   default:  goto fail;
   }

 fail:
   vpanic("s390_insn_vec_amodeop_emit");
}


static UChar *
s390_insn_vec_amodeintop_emit(UChar *buf, const s390_insn *insn)
{
   UChar v1 = hregNumber(insn->variant.vec_amodeintop.dst);
   s390_amode* op2 = insn->variant.vec_amodeintop.op2;
   UChar r3 = hregNumber(insn->variant.vec_amodeintop.op3);

   vassert(hregNumber(op2->x) == 0);
   UChar b = hregNumber(op2->b);
   UShort d = op2->d;

   switch (insn->variant.vec_amodeintop.tag) {
   case S390_VEC_SET_ELEM:
      return s390_emit_VLVG(buf, v1, b, d, r3, s390_getM_from_size(insn->size));
   default:  goto fail;
   }

 fail:
   vpanic("s390_insn_vec_amodeop_emit");
}


static UChar *
s390_insn_vec_binop_emit(UChar *buf, const s390_insn *insn)
{
   s390_vec_binop_t tag = insn->variant.vec_binop.tag;
   UChar size = insn->size;
   UChar v1 = hregNumber(insn->variant.vec_binop.dst);
   UChar v2 = hregNumber(insn->variant.vec_binop.op1);
   UChar v3 = hregNumber(insn->variant.vec_binop.op2);

   switch (tag) {
      case S390_VEC_PACK:
         return s390_emit_VPK(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_PACK_SATURU:
         return s390_emit_VPKLS(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_PACK_SATURS:
         return s390_emit_VPKS(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_COMPARE_EQUAL:
         return s390_emit_VCEQ(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_OR:
         return s390_emit_VO(buf, v1, v2, v3);
      case S390_VEC_ORC:
         return s390_emit_VOC(buf, v1, v2, v3);
      case S390_VEC_XOR:
         return s390_emit_VX(buf, v1, v2, v3);
      case S390_VEC_AND:
         return s390_emit_VN(buf, v1, v2, v3);
      case S390_VEC_MERGEL:
         return s390_emit_VMRL(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_MERGEH:
         return s390_emit_VMRH(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_NOR:
         return s390_emit_VNO(buf, v1, v2, v3);
      case S390_VEC_INT_ADD:
         return s390_emit_VA(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INT_SUB:
         return s390_emit_VS(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_MAXU:
         return s390_emit_VMXL(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_MAXS:
         return s390_emit_VMX(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_MINU:
         return s390_emit_VMNL(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_MINS:
         return s390_emit_VMN(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_AVGU:
         return s390_emit_VAVGL(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_AVGS:
         return s390_emit_VAVG(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_COMPARE_GREATERS:
         return s390_emit_VCH(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_COMPARE_GREATERU:
         return s390_emit_VCHL(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INT_MUL_HIGHS:
         return s390_emit_VMH(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INT_MUL_HIGHU:
         return s390_emit_VMLH(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INT_MUL_LOW:
         return s390_emit_VML(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INT_MUL_EVENS:
         return s390_emit_VME(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INT_MUL_EVENU:
         return s390_emit_VMLE(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_ELEM_SHL_V:
         return s390_emit_VESLV(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_ELEM_SHRA_V:
         return s390_emit_VESRAV(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_ELEM_SHRL_V:
         return s390_emit_VESRLV(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_ELEM_ROLL_V:
         return s390_emit_VERLLV(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_SHL_BITS:
         return s390_emit_VSL(buf, v1, v2, v3);
      case S390_VEC_SHRL_BITS:
         return s390_emit_VSRL(buf, v1, v2, v3);
      case S390_VEC_SHRA_BITS:
         return s390_emit_VSRA(buf, v1, v2, v3);
      case S390_VEC_SHL_BYTES:
         return s390_emit_VSLB(buf, v1, v2, v3);
      case S390_VEC_SHRL_BYTES:
         return s390_emit_VSRLB(buf, v1, v2, v3);
      case S390_VEC_SHRA_BYTES:
         return s390_emit_VSRAB(buf, v1, v2, v3);
      case S390_VEC_PWSUM_W:
         vassert((size == 1) || (size == 2));
         return s390_emit_VSUM(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_PWSUM_DW:
         vassert((size == 2) || (size == 4));
         return s390_emit_VSUMG(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_PWSUM_QW:
         vassert((size == 4) || (size == 8));
         return s390_emit_VSUMQ(buf, v1, v2, v3, s390_getM_from_size(size));
      case S390_VEC_INIT_FROM_GPRS:
         return s390_emit_VLVGP(buf, v1, v2, v3);
      case S390_VEC_INIT_FROM_FPRS:
         return s390_emit_VMRH(buf, v1, v2, v3, 3);
      case S390_VEC_FLOAT_ADD:
         return s390_emit_VFA(buf, v1, v2, v3, s390_getM_from_size(size), 0);
      case S390_VEC_FLOAT_SUB:
         return s390_emit_VFS(buf, v1, v2, v3, s390_getM_from_size(size), 0);
      case S390_VEC_FLOAT_MUL:
         return s390_emit_VFM(buf, v1, v2, v3, s390_getM_from_size(size), 0);
      case S390_VEC_FLOAT_DIV:
         return s390_emit_VFD(buf, v1, v2, v3, s390_getM_from_size(size), 0);
      case S390_VEC_FLOAT_COMPARE_EQUAL:
         return s390_emit_VFCE(buf, v1, v2, v3, s390_getM_from_size(size), 0, 0);
      case S390_VEC_FLOAT_COMPARE_LESS_OR_EQUAL:
         // PJF I assume that CHE is compare higher or equal so the order needs swapping
         // coverity[SWAPPED_ARGUMENTS:FALSE]
         return s390_emit_VFCHE(buf, v1, v3, v2, s390_getM_from_size(size), 0, 0);
      case S390_VEC_FLOAT_COMPARE_LESS:
         // PJF as above but this time compare higher
         // coverity[SWAPPED_ARGUMENTS:FALSE]
         return s390_emit_VFCH(buf, v1, v3, v2, s390_getM_from_size(size), 0, 0);

      default:
         goto fail;
   }

   fail:
      ppS390Instr(insn, True);
      vpanic("s390_insn_vec_binop_emit");

}


static UChar *
s390_insn_vec_triop_emit(UChar *buf, const s390_insn *insn)
{
   s390_vec_triop_t tag = insn->variant.vec_triop.tag;
   UChar size = insn->size;
   UChar v1 = hregNumber(insn->variant.vec_triop.dst);
   UChar v2 = hregNumber(insn->variant.vec_triop.op1);
   UChar v3 = hregNumber(insn->variant.vec_triop.op2);
   UChar v4 = hregNumber(insn->variant.vec_triop.op3);

   switch (tag) {
      case S390_VEC_PERM: {
         vassert(size == 16);
         return s390_emit_VPERM(buf, v1, v2, v3, v4);
      }
      case S390_VEC_FLOAT_MADD:
         return s390_emit_VFMA(buf, v1, v2, v3, v4, 0,
                               s390_getM_from_size(size));
      case S390_VEC_FLOAT_MSUB:
         return s390_emit_VFMS(buf, v1, v2, v3, v4, 0,
                               s390_getM_from_size(size));
      default:
         goto fail;
   }

   fail:
      vpanic("s390_insn_vec_triop_emit");

}


static UChar *
s390_insn_vec_replicate_emit(UChar *buf, const s390_insn *insn)
{
   UChar v1 = hregNumber(insn->variant.vec_replicate.dst);
   UChar v2 = hregNumber(insn->variant.vec_replicate.op1);
   UShort idx = (UShort) insn->variant.vec_replicate.idx;
   return s390_emit_VREP(buf, v1, v2, idx, s390_getM_from_size(insn->size));
}


Int
emit_S390Instr(Bool *is_profinc, UChar *buf, Int nbuf, const s390_insn *insn,
               Bool mode64, VexEndness endness_host,
               const void *disp_cp_chain_me_to_slowEP,
               const void *disp_cp_chain_me_to_fastEP,
               const void *disp_cp_xindir,
               const void *disp_cp_xassisted)
{
   UChar *end;

   /* Used to be 48 bytes. Make sure it stays low */
   vassert(sizeof(s390_insn) == 32);

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

   case S390_INSN_MEMCPY:
      end = s390_insn_memcpy_emit(buf, insn);
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

   case S390_INSN_SMUL:
   case S390_INSN_UMUL:
      end = s390_insn_mul_emit(buf, insn);
      break;

   case S390_INSN_SDIV:
   case S390_INSN_UDIV:
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

   case S390_INSN_CDAS:
      end = s390_insn_cdas_emit(buf, insn);
      break;

   case S390_INSN_COMPARE:
      end = s390_insn_compare_emit(buf, insn);
      break;

   case S390_INSN_HELPER_CALL:
      end = s390_insn_helper_call_emit(buf, insn);
      if (end == buf) goto fail;
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

   case S390_INSN_BFP_CONVERT:
      end = s390_insn_bfp_convert_emit(buf, insn);
      break;

   case S390_INSN_DFP_BINOP:
      end = s390_insn_dfp_binop_emit(buf, insn);
      break;

   case S390_INSN_DFP_UNOP:
      end = s390_insn_dfp_unop_emit(buf, insn);
      break;

   case S390_INSN_DFP_INTOP:
      end = s390_insn_dfp_intop_emit(buf, insn);
      break;

   case S390_INSN_DFP_COMPARE:
      end = s390_insn_dfp_compare_emit(buf, insn);
      break;

   case S390_INSN_DFP_CONVERT:
      end = s390_insn_dfp_convert_emit(buf, insn);
      break;

   case S390_INSN_DFP_REROUND:
      end = s390_insn_dfp_reround_emit(buf, insn);
      break;

   case S390_INSN_FP_CONVERT:
      end = s390_insn_fp_convert_emit(buf, insn);
      break;

   case S390_INSN_MFENCE:
      end = s390_insn_mfence_emit(buf, insn);
      break;

   case S390_INSN_MIMM:
      end = s390_insn_mimm_emit(buf, insn);
      break;

   case S390_INSN_MADD:
      end = s390_insn_madd_emit(buf, insn);
      break;

   case S390_INSN_SET_FPC_BFPRM:
      end = s390_insn_set_fpc_bfprm_emit(buf, insn);
      break;

   case S390_INSN_SET_FPC_DFPRM:
      end = s390_insn_set_fpc_dfprm_emit(buf, insn);
      break;

   case S390_INSN_PROFINC:
      end = s390_insn_profinc_emit(buf, insn);
      /* Tell the caller .. */
      vassert(*is_profinc == False);
      *is_profinc = True;
      break;

   case S390_INSN_EVCHECK:
      end = s390_insn_evcheck_emit(buf, insn, endness_host);
      break;

   case S390_INSN_XDIRECT:
      end = s390_insn_xdirect_emit(buf, insn, disp_cp_chain_me_to_slowEP,
                                   disp_cp_chain_me_to_fastEP);
      break;

   case S390_INSN_XINDIR:
      end = s390_insn_xindir_emit(buf, insn, disp_cp_xindir);
      break;

   case S390_INSN_XASSISTED:
      end = s390_insn_xassisted_emit(buf, insn, disp_cp_xassisted);
      break;

   case S390_INSN_VEC_AMODEOP:
      end = s390_insn_vec_amodeop_emit(buf, insn);
      break;

   case S390_INSN_VEC_AMODEINTOP:
      end = s390_insn_vec_amodeintop_emit(buf, insn);
      break;

   case S390_INSN_VEC_BINOP:
      end = s390_insn_vec_binop_emit(buf, insn);
      break;

   case S390_INSN_VEC_TRIOP:
      end = s390_insn_vec_triop_emit(buf, insn);
      break;

   case S390_INSN_VEC_REPLICATE:
      end = s390_insn_vec_replicate_emit(buf, insn);
      break;

   fail:
   default:
      vpanic("emit_S390Instr");
   }

   vassert(end - buf <= nbuf);

   return end - buf;
}


/* Return the number of bytes emitted for an S390_INSN_EVCHECK.
   See s390_insn_evcheck_emit */
Int
evCheckSzB_S390(void)
{
   return s390_host_has_gie ? 18 : 24;
}


/* Patch the counter address into CODE_TO_PATCH as previously
   generated by s390_insn_profinc_emit. */
VexInvalRange
patchProfInc_S390(VexEndness endness_host,
                  void *code_to_patch, const ULong *location_of_counter)
{
   vassert(sizeof(ULong *) == 8);

   s390_tchain_verify_load64(code_to_patch, S390_REGNO_TCHAIN_SCRATCH, 0);

   UChar *p = s390_tchain_patch_load64(code_to_patch,
                                       (Addr)location_of_counter);

   UInt len = p - (UChar *)code_to_patch;
   VexInvalRange vir = { (HWord)code_to_patch, len };
   return vir;
}


/* NB: what goes on here has to be very closely coordinated with the
   s390_insn_xdirect_emit code above. */
VexInvalRange
chainXDirect_S390(VexEndness endness_host,
                  void *place_to_chain,
                  const void *disp_cp_chain_me_EXPECTED,
                  const void *place_to_jump_to)
{
   vassert(endness_host == VexEndnessBE);

   /* What we're expecting to see @ PLACE_TO_CHAIN is:

        load  tchain_scratch, #disp_cp_chain_me_EXPECTED
        goto *tchain_scratch
   */
   const UChar *next;
   next = s390_tchain_verify_load64(place_to_chain, S390_REGNO_TCHAIN_SCRATCH,
                                    (Addr)disp_cp_chain_me_EXPECTED);
   vassert(s390_insn_is_BR(next, S390_REGNO_TCHAIN_SCRATCH));

   /* And what we want to change it to is either:
        (general case):

          load  tchain_scratch, #place_to_jump_to
          goto *tchain_scratch

      ---OR---

        in the case where the displacement is small enough

          BRCL delta       where delta is in half-words
          invalid opcodes

      In both cases the replacement has the same length as the original.
      To remain sane & verifiable,
      (1) limit the displacement for the short form to 
          (say) +/- one billion, so as to avoid wraparound
          off-by-ones
      (2) even if the short form is applicable, once every (say)
          1024 times use the long form anyway, so as to maintain
          verifiability
   */

   /* This is the delta we need to put into a BRCL insn. Note, that the
      offset in BRCL is in half-words. Hence division by 2. */
   Long delta =
      (Long)((const UChar *)place_to_jump_to - (const UChar *)place_to_chain) / 2;
   Bool shortOK = delta >= -1000*1000*1000 && delta < 1000*1000*1000;

   static UInt shortCTR = 0; /* DO NOT MAKE NON-STATIC */
   if (shortOK) {
      shortCTR++; // thread safety bleh
      if (0 == (shortCTR & 0x3FF)) {
         shortOK = False;
         if (0)
            vex_printf("QQQ chainXDirect_S390: shortCTR = %u, "
                       "using long jmp\n", shortCTR);
      }
   }

   /* And make the modifications. */
   UChar *p = (UChar *)place_to_chain;
   if (shortOK) {
      p = s390_emit_BRCL(p, S390_CC_ALWAYS, delta);  /* 6 bytes */

      /* Make sure that BRCL fits into the patchable part of an xdirect
         code sequence */
      vassert(6 <= s390_xdirect_patchable_len());

      /* Fill remaining bytes with 0x00 (invalid opcode) */
      Int i;
      for (i = 0; i < s390_xdirect_patchable_len() - 6; ++i)
         p[i] = 0x00;
   } else {
      /*
          load  tchain_scratch, #place_to_jump_to
          goto *tchain_scratch
      */
      Addr64 addr = (Addr)place_to_jump_to;
      p = s390_tchain_load64(p, S390_REGNO_TCHAIN_SCRATCH, addr);
      /* There is not need to emit a BCR here, as it is already there. */
   }

   UInt len = p - (UChar *)place_to_chain;
   VexInvalRange vir = { (HWord)place_to_chain, len };
   return vir;
}


/* NB: what goes on here has to be very closely coordinated with the
   s390_insn_xdirect_emit code above. */
VexInvalRange
unchainXDirect_S390(VexEndness endness_host,
                    void *place_to_unchain,
                    const void *place_to_jump_to_EXPECTED,
                    const void *disp_cp_chain_me)
{
   vassert(endness_host == VexEndnessBE);

   /* What we're expecting to see @ PLACE_TO_UNCHAIN:

          load  tchain_scratch, #place_to_jump_to_EXPECTED
          goto *tchain_scratch

      ---OR---
        in the case where the displacement falls within 32 bits

          BRCL delta
          invalid opcodes
   */
   UChar *p = place_to_unchain;

   Bool uses_short_form = False;

   if (s390_insn_is_BRCL(p, S390_CC_ALWAYS)) {
      /* Looks like the short form */
      Int num_hw = *(Int *)&p[2];
      Int delta = 2 *num_hw;

      vassert(p + delta == place_to_jump_to_EXPECTED);

      Int i;
      for (i = 0; i < s390_xdirect_patchable_len() - 6; ++i)
         vassert(p[6+i] == 0x00);
      uses_short_form = True;
   } else {
      /* Should be the long form */
      const UChar *next;

      next = s390_tchain_verify_load64(p, S390_REGNO_TCHAIN_SCRATCH,
                                       (Addr)place_to_jump_to_EXPECTED);
      /* Check for BR *tchain_scratch */
      vassert(s390_insn_is_BR(next, S390_REGNO_TCHAIN_SCRATCH));
   }

   /* And what we want to change it to is:

        load  tchain_scratch, #disp_cp_chain_me
        goto *tchain_scratch
   */

   /* Get the address of the beginning of the load64 code sequence into %r1.
      Do not change the register! This is part of the protocol with the
      dispatcher.
      Note: the incoming argument PLACE_TO_CHAIN points to the beginning of the
      load64 insn sequence. That sequence is prefixed with a BASR to get its
      address (see s390_insn_xdirect_emit).  */
   p = s390_emit_BASR(p - S390_BASR_LEN, 1, R0);

   Addr64 addr = (Addr)disp_cp_chain_me;
   p = s390_tchain_load64(p, S390_REGNO_TCHAIN_SCRATCH, addr);

   /* Emit the BCR in case the short form was used. In case of the long
      form, the BCR is already there. */
   if (uses_short_form)
      s390_emit_BCR(p, S390_CC_ALWAYS, S390_REGNO_TCHAIN_SCRATCH);

   UInt len = p - (UChar *)place_to_unchain;
   VexInvalRange vir = { (HWord)place_to_unchain, len };
   return vir;
}

/*---------------------------------------------------------------*/
/*--- end                                    host_s390_defs.c ---*/
/*---------------------------------------------------------------*/
