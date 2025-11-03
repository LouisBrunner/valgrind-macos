/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                     s390_disasm.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
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
#include "main_util.h"        // vassert
#include "main_globals.h"     // vex_traceflags
#include "s390_defs.h"        // S390_MAX_MNEMONIC_LEN
#include "s390_disasm.h"

static HChar *s390_disasm_aux(const s390_opnd *, const HChar *, HChar *,
                              Int (*)(UInt, UInt, UInt *));

/* Return the mnemonic padded with blanks to its right */
static const HChar *
padmnm(const HChar *mnm)
{
   vassert(vex_strlen(mnm) <= S390_MAX_MNEMONIC_LEN);

   static HChar buf[S390_MAX_MNEMONIC_LEN + 1];

   vex_sprintf(buf, "%-*s", S390_MAX_MNEMONIC_LEN, mnm);

   return buf;
}


/* Return the name of a general purpose register for dis-assembly purposes. */
static const HChar *
gpr_operand(UInt archreg)
{
   static const HChar names[16][5] = {
      "%r0", "%r1", "%r2", "%r3",
      "%r4", "%r5", "%r6", "%r7",
      "%r8", "%r9", "%r10", "%r11",
      "%r12", "%r13", "%r14", "%r15",
   };

   vassert(archreg < 16);

   return names[archreg];
}


/* Return the name of a floating point register for dis-assembly purposes. */
static const HChar *
fpr_operand(UInt archreg)
{
   static const HChar names[16][5] = {
      "%f0", "%f1", "%f2", "%f3",
      "%f4", "%f5", "%f6", "%f7",
      "%f8", "%f9", "%f10", "%f11",
      "%f12", "%f13", "%f14", "%f15",
   };

   vassert(archreg < 16);

   return names[archreg];
}


/* Return the name of an access register for dis-assembly purposes. */
static const HChar *
ar_operand(UInt archreg)
{
   static const HChar names[16][5] = {
      "%a0", "%a1", "%a2", "%a3",
      "%a4", "%a5", "%a6", "%a7",
      "%a8", "%a9", "%a10", "%a11",
      "%a12", "%a13", "%a14", "%a15",
   };

   vassert(archreg < 16);

   return names[archreg];
}


/* Return the name of a vector register for dis-assembly purposes. */
static const HChar *
vr_operand(UInt archreg)
{
   static const HChar names[32][5] = {
      "%v0", "%v1", "%v2", "%v3",
      "%v4", "%v5", "%v6", "%v7",
      "%v8", "%v9", "%v10", "%v11",
      "%v12", "%v13", "%v14", "%v15",
      "%v16", "%v17", "%v18", "%v19",
      "%v20", "%v21", "%v22", "%v23",
      "%v24", "%v25", "%v26", "%v27",
      "%v28", "%v29", "%v30", "%v31",
   };

   vassert(archreg < 32);

   return names[archreg];
}


/* Common function used to construct a mnemonic based on a condition code
   mask. */
static const HChar *
construct_mnemonic(const HChar *prefix, const HChar *suffix, UInt mask)
{
   static HChar buf[S390_MAX_MNEMONIC_LEN + 1];

   static HChar mask_id[16][4] = {
      "", /* 0 -> unused */
      "o", "h", "nle", "l", "nhe", "lh", "ne",
      "e", "nlh", "he", "nl", "le", "nh", "no",
      ""  /* 15 -> unused */
   };

   /* Guard against buffer overflow */
   vassert(vex_strlen(prefix) + vex_strlen(suffix) +
           sizeof mask_id[0] <= sizeof buf);

   HChar *p = buf;

   p += vex_sprintf(p, "%s%s%s", prefix, mask_id[mask], suffix);
   *p = '\0';

   return buf;
}


/* An operand with a base register, an index register, and a displacement.
   If the displacement is signed, the rightmost 20 bit of D need to be
   sign extended */
static HChar *
dxb_operand(HChar *p, UInt d, UInt x, UInt b, Bool displacement_is_signed)
{
   if (displacement_is_signed) {
      Int displ = (Int)(d << 12) >> 12;  /* sign extend */

      p += vex_sprintf(p, "%d", displ);
   } else {
      p += vex_sprintf(p, "%u", d);
   }
   if (x != 0) {
      p += vex_sprintf(p, "(%s,%s)", gpr_operand(x),
                       b != 0 ? gpr_operand(b) : "0");
   } else {
      if (b != 0) {
         p += vex_sprintf(p, "(%s)", gpr_operand(b));
      }
   }

   return p;
}


/* An operand with base register, unsigned length, and a 12-bit
   unsigned displacement */
static HChar *
udlb_operand(HChar *p, UInt d, UInt length, UInt b)
{
   p += vex_sprintf(p, "%u", d);
   p += vex_sprintf(p, "(%u", length + 1);  // actual length is +1
   p += vex_sprintf(p, ",%s", b != 0 ? gpr_operand(b) : "0");
   p += vex_sprintf(p, ")");

   return p;
}


/* An operand with a base register, an vector register, and a displacement.
   If the displacement is signed, the rightmost 20 bit of D need to be
   sign extended */
static HChar *
dvb_operand(HChar *p, UInt d, UInt v, UInt b, Bool displacement_is_signed)
{
   if (displacement_is_signed) {
      Int displ = (Int)(d << 12) >> 12;  /* sign extend */

      p += vex_sprintf(p, "%d", displ);
   } else {
      p += vex_sprintf(p, "%u", d);
   }
   p += vex_sprintf(p, "(%s", vr_operand(v));
   p += vex_sprintf(p, ",%s", b != 0 ? gpr_operand(b) : "0");
   p += vex_sprintf(p, ")");

   return p;
}


/* Return the number of MASK operands */
static UInt
mask_count(const s390_opnd *opnds)
{
   UInt num_masks = 0;

   for (UInt ix = 0; opnds[ix].kind != S390_OPND_DONE; ++ix) {
      if (opnds[ix].kind == S390_OPND_MASK) {
         ++num_masks;
      }
   }
   return num_masks;
}


/* Given the mask number return its index in the operands array.
   Mask numbers begin at 1. */
static UInt
get_mask_index(const s390_opnd *opnds, UInt mask_no)
{
   vassert(mask_no <= mask_count(opnds));

   UInt count = 0;

   for (UInt ix = 0; opnds[ix].kind != S390_OPND_DONE; ++ix) {
      if (opnds[ix].kind == S390_OPND_MASK) {
         ++count;
         if (count == mask_no)
            return ix;
      }
   }
   /* not reachable */
   return 0;
}


/* It is expected that OPNDS contains exactly one MASK operand. Return
   its index. Assert, if there is no mask or multiple mask fields. */
static UInt
unique_mask_index(const s390_opnd *opnds)
{
   vassert(mask_count(opnds) == 1);

   return get_mask_index(opnds, 1);
}


/* Special handling for the BCR opcode */
HChar *
bcr_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *xmnm;
   UInt mask = opnds[1].mask;

   if (mask == 0)
      xmnm = "nopr";
   else if (mask == 15)
      xmnm = "br";
   else
      xmnm = construct_mnemonic("b", "r", mask);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


/* Special handling for the BC opcode */
HChar *
bc_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *xmnm;
   UInt mask = opnds[1].mask;

   if (mask == 0) {
      xmnm = "nop";
      if (opnds[2].d == 0 && opnds[2].b == 0 && opnds[2].x == 0)
         return p += vex_sprintf(p, "nop");
   } else if (mask == 15)
      xmnm = "b";
   else
      xmnm = construct_mnemonic("b", "", mask);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


/* Special handling for the BRC opcode */
HChar *
brc_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *xmnm;
   UInt mask = opnds[1].mask;

   if (mask == 0)
      xmnm = "jnop";
   else if (mask == 15)
      xmnm = "j";
   else
      xmnm = construct_mnemonic("j", "", mask);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


/* Special handling for the BRCL opcode */
HChar *
brcl_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *xmnm;
   UInt mask = opnds[1].mask;

   if (mask == 0)
      xmnm = "jgnop";
   else if (mask == 15)
      xmnm = "jg";
   else
      xmnm = construct_mnemonic("jg", "", mask);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


/* Return 1, if mask should be printed. In *VALUE return the mask value
   that should be printed.  */
static Int
cabt_mh(UInt ix __attribute__((unused)), UInt mask, UInt *value)
{
   *value = mask;
   return (mask & 1) || mask == 0 || mask == 14;
}


/* Special handling for the various compare and branch / trap opcodes:
   CLFIT, CLGIT, C[G]IT, C[L][G]RJ, C[L][G]IJ, C[L][G]IB, C[L][G]RB,
   CL[G]T, C[L][G]RT
*/
HChar *
cabt_disasm(const s390_opnd *opnds, HChar *p)
{
   static HChar xmnm[S390_MAX_MNEMONIC_LEN + 1];

   static const HChar suffix[8][3] = {
      "", "h", "l", "ne", "e", "nl", "nh", ""
   };

   const HChar *base = opnds[0].xmnm.base;

   /* Guard against buffer overflow */
   vassert(vex_strlen(base) + sizeof suffix[0] <= sizeof xmnm);

   HChar *x  = xmnm;
   UInt mask = opnds[unique_mask_index(opnds)].mask;

   x += vex_sprintf(x, "%s", base);
   if (! (mask & 0x1)) {
      x += vex_sprintf(x, "%s", suffix[mask >> 1]);
   }
   *x = '\0';

   return s390_disasm_aux(opnds, xmnm, p, cabt_mh);
}


static Int
cls_mh(UInt ix __attribute__((unused)), UInt mask, UInt *value)
{
   *value = mask;
   return mask == 0 || mask == 15;
}


/* Special handling for the various conditional load / store opcodes:
   LOC[G]R, LOCFHR, LOC[G]HI, LOCHHI, LOC[G], LOCFH, STOC[G], STOFH
   Also used for SEL[G]R and SELFHR
*/
HChar *
cls_disasm(const s390_opnd *opnds, HChar *p)
{
   UInt  mask = opnds[unique_mask_index(opnds)].mask;
   const HChar *base = opnds[0].xmnm.base;
   const HChar *xmnm = construct_mnemonic(base, "", mask);

   return s390_disasm_aux(opnds, xmnm, p, cls_mh);
}


static Int
bic_mh(UInt ix __attribute__((unused)), UInt mask, UInt *value)
{
   *value = mask;
   return mask == 0;
}


/* Special handling for the BIC opcode */
HChar *
bic_disasm(const s390_opnd *opnds, HChar *p)
{
   UInt  mask = opnds[1].mask;
   const HChar *xmnm;

   if (mask == 0) {
      /* There is no special opcode when mask == 0. */
      xmnm = opnds[0].xmnm.base;
   } else {
      xmnm = construct_mnemonic("bi", "", mask);
   }

   return s390_disasm_aux(opnds, xmnm, p, bic_mh);
}


/* Special handling for the VGBM opcode */
HChar *
vgbm_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *xmnm;
   const HChar *v1 = vr_operand(opnds[1].regno);
   UInt i2 = opnds[2].u;

   if (i2 == 0) {
      xmnm = padmnm("vzero");
      p += vex_sprintf(p, "%s %s", xmnm, v1);
   } else if (i2 == 0xffff) {
      xmnm = padmnm("vone");
      p += vex_sprintf(p, "%s %s", xmnm, v1);
   } else {
      xmnm = padmnm(opnds[0].xmnm.base);
      p += vex_sprintf(p, "%s %s,%u", xmnm, v1, i2);
   }
   return p;
}


static Int
mask0_mh(UInt ix __attribute__((unused)), UInt mask, UInt *value)
{
   *value = mask;
   return mask != 0;
}


/* Write out the operands, except if the unique mask is 0 do not write it */
HChar *
mask0_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 1);

   const HChar *mnm = opnds[0].xmnm.base;

   if (vex_streq(mnm, "cu12") && opnds[unique_mask_index(opnds)].mask == 0)
      mnm = "cutfu";
   if (vex_streq(mnm, "cu21") && opnds[unique_mask_index(opnds)].mask == 0)
      mnm = "cuutf";

   return s390_disasm_aux(opnds, mnm, p, mask0_mh);
}


/* Opcode is expected to have a single MASK operand. Use its value
   to determine a single-character suffix to be appended to the base
   mnemonic. The mask itself is not printed. */
HChar *
va_like_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar suffix[] = { 'b', 'h', 'f', 'g', 'q' };
   const HChar *base = opnds[0].xmnm.base;
   UInt  mask = opnds[unique_mask_index(opnds)].mask;
   HChar xmnm[vex_strlen(base) + 2 + 1];
   HChar extra = '\0';

   vassert(mask < sizeof suffix);

   if (mask == 1 &&
       (vex_streq(base, "vupl") || vex_streq(base, "vmal") ||
        vex_streq(base, "vml"))) {
      extra = 'w';
   }
   vex_sprintf(xmnm, "%s%c%c", base, suffix[mask], extra);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


/* Opcode is expected to have two MASK operands. Use their values
   to determine a suffix to be appended to the base mnemonic. The
   masks themselves are not printed. */
HChar *
vch_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar suffix1[] = { 'b', 'h', 'f', 'g' };
   const HChar suffix2[] = { '\0', 's' };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m4 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m5 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + 2 + 1];

   vassert(m4 < sizeof suffix1);
   vassert(m5 < sizeof suffix2);

   vex_sprintf(xmnm, "%s%c%c", base, suffix1[m4], suffix2[m5]);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


static Int
always_mh(UInt ix __attribute__((unused)),
          UInt mask, UInt *value)
{
   *value = mask;
   return 1;
}


static Int
never_mh(UInt ix __attribute__((unused)),
         UInt mask, UInt *value)
{
   *value = mask;
   return 0;
}


HChar *
vfce_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 3);

   const HChar  suffix1[][3] = { "sb", "db", "xb" };
   const HChar  suffix2[] = { '\0', 's' };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m4 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m5 = opnds[get_mask_index(opnds, 2)].mask;
   UInt  m6 = opnds[get_mask_index(opnds, 3)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix1 - 1) + 1 + 1];

   if (m4 == 4 && (m5 == 0 || m5 == 4))
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m4 - 2 < sizeof suffix1 / sizeof *suffix1);
   vassert(m6 < sizeof suffix2);

   vex_sprintf(xmnm, "%s%s%c", base, suffix1[m4 - 2], suffix2[m6]);

   if (m5 & 0x8) xmnm[0] = 'w';
   if (m5 & 0x4) xmnm[2] = 'k';

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


HChar *
vfmix_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar  suffix[][3] = { "sb", "db", "xb" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m4 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m5 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix - 1) + 1];

   if (m4 == 4 && m5 == 0)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m4 - 2 < sizeof suffix / sizeof *suffix);

   vex_sprintf(xmnm, "%s%s", base, suffix[m4 - 2]);

   if (m5 & 0x8) xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


HChar *
vfa_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar  suffix[][3] = { "sb", "db", "xb" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m4 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m5 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix - 1) + 1];

   if (m4 == 4 && m5 == 0)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m4 - 2 < sizeof suffix / sizeof *suffix);

   vex_sprintf(xmnm, "%s%s", base, suffix[m4 - 2]);

   if (m5 & 0x8) xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


/* Also used for VFEE, VFENE, and VSTRS. But for those opcodes m5 < 8.
   Hence, the return value is 0 and no mask will be written for
   those opcodes. */
static Int
vfae_mh(UInt ix, UInt mask, UInt *value)
{
   *value = mask & 0xC;
   return (ix == 5) && (mask & 0xC);
}


HChar *
vfae_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar suffix[] = { 'b', 'h', 'f' };
   const HChar *base = opnds[0].xmnm.base;

   UInt  m4 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m5 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + 3 + 1];

   HChar s = (m5 & 0x1) ? 's' : '\0';
   HChar z = (m5 & 0x2) ? 'z' : '\0';

   vex_sprintf(xmnm, "%s%c%c%c", base, z, suffix[m4], s);

   return s390_disasm_aux(opnds, xmnm, p, vfae_mh);
}


HChar *
vfms_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar  suffix[][3] = { "sb", "db", "xb" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m5 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m6 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix - 1) + 1];

   if (m6 == 4 && m5 == 0)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m6 - 2 < sizeof suffix / sizeof *suffix);

   vex_sprintf(xmnm, "%s%s", base, suffix[m6 - 2]);

   if (m5 & 0x8) xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


static Int
vmsl_mh(UInt ix, UInt mask, UInt *value)
{
   *value = mask;
   return ix == 6;
}


HChar *
vmsl_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar  suffix[] = { 'g' };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m5 = opnds[get_mask_index(opnds, 1)].mask;
   /* m6 does not influence the extended mnemonic */
   HChar xmnm[vex_strlen(base) + 1 + 1];

   vassert(m5 - 3 < sizeof suffix);

   vex_sprintf(xmnm, "%s%c", base, suffix[m5 - 3]);

   return s390_disasm_aux(opnds, xmnm, p, vmsl_mh);
}


static Int
vstrc_mh(UInt ix, UInt mask, UInt *value)
{
   *value = mask & 0xC;
   return (ix == 6) && (mask & 0xC);
}


HChar *
vstrc_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar suffix[] = { 'b', 'h', 'f' };
   const HChar *base = opnds[0].xmnm.base;

   UInt  m5 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m6 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + 3 + 1];

   HChar s = (m6 & 0x1) ? 's' : '\0';
   HChar z = (m6 & 0x2) ? 'z' : '\0';

   vex_sprintf(xmnm, "%s%c%c%c", base, z, suffix[m5], s);

   return s390_disasm_aux(opnds, xmnm, p, vstrc_mh);
}


HChar *
vllebrz_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 1);

   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   const HChar *xmnm;

   switch (m3) {
   case 1: xmnm = "vllebrzh"; break;
   case 2: xmnm = "vllebrzf"; break;
   case 3: xmnm = "ldrv";     break;
   case 6: xmnm = "lerv";     break;
   default: vassert(0);
   }

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


HChar *
vstebrf_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 1);

   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   const HChar *xmnm;

   if (m3 == 0)
      xmnm = "sterv";
   else
      xmnm = opnds[0].xmnm.base;

   return s390_disasm_aux(opnds, xmnm, p, mask0_mh);
}


HChar *
vstebrg_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 1);

   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   const HChar *xmnm;

   if (m3 == 0)
      xmnm = "stdrv";
   else
      xmnm = opnds[0].xmnm.base;

   return s390_disasm_aux(opnds, xmnm, p, mask0_mh);
}


HChar *
vllez_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 1);

   const HChar suffix[][3] = { "b", "h", "f", "g", "", "", "lf" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix - 1) + 1];

   vassert(m3 < sizeof suffix / sizeof *suffix);
   vex_sprintf(xmnm, "%s%s", base, suffix[m3]);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


HChar *
wfc_like_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar  suffix[][3] = { "sb", "db", "xb" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix - 1) + 1];

   vassert(m3 - 2 < sizeof suffix / sizeof *suffix);

   vex_sprintf(xmnm, "%s%s", base, suffix[m3 - 2]);

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


HChar *
vfll_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar suffix[] = { 's', 'd' };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m4 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + 1 + 1];

   if (m3 == 3 && m4 == 0)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m3 - 2 < sizeof suffix);
   vex_sprintf(xmnm, "%s%c", base, suffix[m3 - 2]);

   if (m4 == 8)
      xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


static Int
always_m4(UInt ix, UInt mask, UInt *value)
{
   *value = mask & 0x4;
   return ix == 4;
}


HChar *
vflr_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar suffix[] = { 'd', 'x' };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m4 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + 1 + 1];

   if (m3 == 4 && m4 < 8)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m3 - 3 < sizeof suffix);
   vex_sprintf(xmnm, "%s%c", base, suffix[m3 - 3]);

   if (m4 & 0x8)
      xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, always_m4);
}


HChar *
vfi_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar  suffix[][3] = { "sb", "db", "xb" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m4 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(base) + (sizeof *suffix - 1) + 1];

   if (m3 == 4 && m4 < 8)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(m3 - 2 < sizeof suffix / sizeof *suffix);
   vex_sprintf(xmnm, "%s%s", base, suffix[m3 - 2]);

   if (m4 & 0x8)
      xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, always_m4);
}


HChar *
vfpso_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 3);

   const HChar  suffix1[] = { 'c', 'n', 'p' };
   const HChar  suffix2[][3] = { "sb", "db", "xb" };
   const HChar *base = opnds[0].xmnm.base;
   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m4 = opnds[get_mask_index(opnds, 2)].mask;
   UInt  m5 = opnds[get_mask_index(opnds, 3)].mask;
   HChar xmnm[vex_strlen(base) + 1 + (sizeof *suffix2 - 1) + 1];

   if (m3 == 4 && m4 == 0)
      return s390_disasm_aux(opnds, base, p, always_mh);

   vassert(vex_strlen("vfl") <= vex_strlen(base));
   vassert(m5 < sizeof suffix1);
   vassert(m3 - 2 < sizeof suffix2 / sizeof *suffix2);
   vex_sprintf(xmnm, "%s%c%s", "vfl", suffix1[m5], suffix2[m3 - 2]);

   if (m4 & 0x8)
      xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, NULL);
}


static Int
vcgd_mh(UInt ix, UInt mask, UInt *value)
{
   *value = mask;
   if (ix == 5)
      return 1;
   if (ix == 4) {
      if (mask >= 8)
         *value = mask - 8;
      return 1;
   }
   return 0;
}


static HChar *
vcgd_like_disasm(const s390_opnd *opnds, const HChar *mnm[2], HChar *p)
{
   vassert(mask_count(opnds) == 3);
   vassert(vex_strlen(mnm[0]) == vex_strlen(mnm[1]));

   UInt  m3 = opnds[get_mask_index(opnds, 1)].mask;
   UInt  m4 = opnds[get_mask_index(opnds, 2)].mask;
   HChar xmnm[vex_strlen(mnm[0]) + 1];

   vex_sprintf(xmnm, "%s", mnm[m3 - 2]);
   if (m4 & 0x8)
      xmnm[0] = 'w';

   return s390_disasm_aux(opnds, xmnm, p, vcgd_mh);
}


HChar *
vcgd_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *mnm[2] = { "vcfeb", "vcgdb" };

   return vcgd_like_disasm(opnds, mnm, p);
}


HChar *
vcdg_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *mnm[2] = { "vcefb", "vcdgb" };

   return vcgd_like_disasm(opnds, mnm, p);
}


HChar *
vclgd_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *mnm[2] = { "vclfeb", "vclgdb" };

   return vcgd_like_disasm(opnds, mnm, p);
}


HChar *
vcgld_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *mnm[2] = { "vcelfb", "vcdlgb" };

   return vcgd_like_disasm(opnds, mnm, p);
}


/* Used by BFP / DFP convert from fixed / logical opcodes

   1) Mnemonics ending in 'A', e.g. CEFBRA
      a) m3 == m4 == 0  --> CEFBR and no mask values written
      b) otherwise      --> CEFBRA and both mask values written

   2) Mnemonics for "logical" opcodes, e.g. CELFBR
      These do not end in 'A'
      --> mnemonic unchanged, both mask values written

   3) Neither #1 nor #2, e.g. CDFTR
      --> mnemonic unchanged, both mask values written
*/
HChar *
fp_convf_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar *base = opnds[0].xmnm.base;
   UInt m3 = opnds[get_mask_index(opnds, 1)].mask;
   UInt m4 = opnds[get_mask_index(opnds, 2)].mask;
   UInt len = vex_strlen(base);

   HChar xmnm[len + 1];

   vex_sprintf(xmnm, "%s", base);

   if (xmnm[len - 1] == 'a' && m3 + m4 == 0) {
      xmnm[len - 1] = '\0';
      return s390_disasm_aux(opnds, xmnm, p, never_mh);
   }
   return s390_disasm_aux(opnds, xmnm, p, always_mh);
}


/* Return 1, if mask should be printed. In *VALUE return the mask value
   that should be printed.  */
static Int
fp_convt_mh(UInt ix __attribute__((unused)), UInt mask, UInt *value)
{
   *value = mask;
   return (ix == 2 || mask != 0) ? 1 : 0;
}


/* Used by BFP / DFP convert to fixed / logical opcodes

   1) Mnemonics ending in 'A', e.g. CFEBRA
      a) m4 == 0     --> CFEBR and no mask values written
      b) otherwise   --> CFEBRA and both mask values written

   2) Mnemonics for "logical" opcodes, e.g. CLFEBR
      These do not end in 'A'
      --> mnemonic unchanged, both mask values written

   3) Neither #1 nor #2, e.g. CFDTR
      --> mnemonic unchanged, both mask values written
*/
HChar *
fp_convt_disasm(const s390_opnd *opnds, HChar *p)
{
   vassert(mask_count(opnds) == 2);

   const HChar *base = opnds[0].xmnm.base;
   UInt m4 = opnds[get_mask_index(opnds, 2)].mask;
   UInt len = vex_strlen(base);

   HChar xmnm[len + 1];

   vex_sprintf(xmnm, "%s", base);

   if (xmnm[len - 1] == 'a' && m4 == 0) {
      xmnm[len - 1] = '\0';
      return s390_disasm_aux(opnds, xmnm, p, fp_convt_mh);
   }
   return s390_disasm_aux(opnds, xmnm, p, always_mh);
}


HChar *
adtra_like_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *base = opnds[0].xmnm.base;
   UInt m4 = opnds[get_mask_index(opnds, 1)].mask;
   UInt len = vex_strlen(base);

   HChar xmnm[len + 1];

   vex_sprintf(xmnm, "%s", base);

   if (xmnm[len - 1] == 'a' && m4 == 0) {
      xmnm[len - 1] = '\0';
   }
   return s390_disasm_aux(opnds, xmnm, p, mask0_mh);
}


static Int
rotate_mh(UInt ix __attribute__((unused)), UInt mask, UInt *value)
{
   *value = mask;
   if (ix == 5 && mask == 0) return 0;
   if (ix == 3)   // rosbg, etc
      *value = mask & ~0x80;
   if (ix == 4)   // risbg
      *value = mask & ~0x80;
   return 1;
}


HChar *
rotate_disasm(const s390_opnd *opnds, HChar *p)
{
   const HChar *base = opnds[0].xmnm.base;
   UInt len = vex_strlen(base);
   HChar xmnm[len + 1];

   if (opnds[0].xmnm.base[1] == 'i')
      vex_sprintf(xmnm, "%s%c", base, (opnds[4].u & 0x80) ? 'z' : '\0');
   else
      vex_sprintf(xmnm, "%s%c", base, (opnds[3].u & 0x80) ? 't' : '\0');

   return s390_disasm_aux(opnds, xmnm, p, rotate_mh);
}


/* Write out OPNDS. MH is a mask handler. It decides whether or not a
   MASK operand is written and if so, massages the mask value as needed. */
static HChar *
s390_disasm_aux(const s390_opnd *opnds, const HChar *xmnm, HChar *p,
                Int (*mh)(UInt, UInt, UInt *))
{
   vassert(opnds[0].kind == S390_OPND_MNM ||
           opnds[0].kind == S390_OPND_XMNM);

   Int write_separator = 0;   // no separator after mnemonic

   for (UInt ix = 0; opnds[ix].kind != S390_OPND_DONE; ++ix) {
      const s390_opnd *opnd = opnds + ix;

      switch (opnd->kind) {
      case S390_OPND_MNM:
         p += vex_sprintf(p, "%s", padmnm(opnd->mnm));
         *p++ = ' ';
         break;

      case S390_OPND_XMNM:
         p += vex_sprintf(p, "%s", padmnm(xmnm));
         *p++ = ' ';
         break;

      case S390_OPND_GPR:
         p += vex_sprintf(p, "%s", gpr_operand(opnd->regno));
         break;

      case S390_OPND_FPR:
         p += vex_sprintf(p, "%s", fpr_operand(opnd->regno));
         break;

      case S390_OPND_AR:
         p += vex_sprintf(p, "%s", ar_operand(opnd->regno));
         break;

      case S390_OPND_VR:
         p += vex_sprintf(p, "%s", vr_operand(opnd->regno));
         break;

      case S390_OPND_MASK: {
         UInt value;
         if (mh && mh(ix, opnd->mask, &value))
            p += vex_sprintf(p, "%u", value);
         else
            write_separator = 0;
         break;
      }

      case S390_OPND_UINT:
         p += vex_sprintf(p, "%u", opnd->u);
         break;

      case S390_OPND_INT:
         p += vex_sprintf(p, "%d", opnd->i);
         break;

      case S390_OPND_PCREL: {
         Long offset = opnd->pcrel;

         /* Convert # halfwords to # bytes */
         offset <<= 1;

         if (offset < 0) {
            p += vex_sprintf(p, ".%lld", offset);
         } else {
            p += vex_sprintf(p, ".+%lld", offset);
         }
         break;
      }

      case S390_OPND_SDXB: {
         UInt d = opnd->d;
         UInt x = opnd->x;
         UInt b = opnd->b;

         p = dxb_operand(p, d, x, b, 1 /* signed_displacement */);
         break;
      }

      case S390_OPND_UDXB: {
         UInt d = opnd->d;
         UInt x = opnd->x;
         UInt b = opnd->b;

         p = dxb_operand(p, d, x, b, 0 /* signed_displacement */);
         break;
      }

      case S390_OPND_UDLB: {
         UInt d = opnd->d;
         UInt l = opnd->l;
         UInt b = opnd->b;

         p = udlb_operand(p, d, l, b);
         break;
      }

      case S390_OPND_UDVB: {
         UInt d = opnd->d;
         UInt v = opnd->v;
         UInt b = opnd->b;

         p = dvb_operand(p, d, v, b, 0 /* signed_displacement */);
         break;
      }

      case S390_OPND_DONE:  // silence GCC
         vassert(0);
         break;
      }

      if (write_separator)
         *p++ = ',';
      write_separator = 1;
   }

   if (p[-1] == ',')    // remove trailing separator, if any
      *--p = '\0';

   return p;
}


void
s390_disasm(const s390_opnd *opnds)
{
   HChar buf[128];  /* holds the disassembled insn */
   HChar *p = buf;

   if (opnds[0].kind == S390_OPND_MNM) {
      p = s390_disasm_aux(opnds, NULL, p, NULL);
   } else if (opnds[0].kind == S390_OPND_XMNM) {
      p = opnds[0].xmnm.handler(opnds, p);
   } else {
      vassert(0);
   }
   *p = '\0';

   vassert(p < buf + sizeof buf);  /* detect buffer overwrite */

   /* Finally, write out the disassembled insn */
   vex_printf("%s\n", buf);
}

/*---------------------------------------------------------------*/
/*--- end                                       s390_disasm.c ---*/
/*---------------------------------------------------------------*/
