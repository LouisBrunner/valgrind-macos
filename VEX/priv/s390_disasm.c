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
#include "main_util.h"        // vassert
#include "main_globals.h"     // vex_traceflags
#include "s390_defs.h"        // S390_MAX_MNEMONIC_LEN
#include "s390_disasm.h"

static HChar *s390_disasm_aux(const s390_opnd *, const HChar *, HChar *,
                              Int (*)(UInt, UInt));

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
      p += vex_sprintf(p, "(%s,%s)", gpr_operand(x), gpr_operand(b));
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
   p += vex_sprintf(p, ",%s", gpr_operand(b));
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
   if (v != 0) {
      p += vex_sprintf(p, "(%s", vr_operand(v));
      if (b != 0) {
         p += vex_sprintf(p, ",%s", gpr_operand(b));
      }
      p += vex_sprintf(p, ")");
   } else {
      if (b != 0) {
         p += vex_sprintf(p, "(%s)", gpr_operand(b));
      }
   }

   return p;
}


/* It is expected that OPNDS contains exactly one MASK operand. Return
   its index. Assert, if there is no mask or multiple mask fields. */
static UInt
unique_mask_index(const s390_opnd *opnds)
{
   UInt num_masks, mask_ix = 0;   // silence GCC

   num_masks = 0;
   for (UInt ix = 0; opnds[ix].kind != S390_OPND_DONE; ++ix) {
      if (opnds[ix].kind == S390_OPND_MASK) {
         ++num_masks;
         mask_ix = ix;
      }
   }
   vassert(num_masks == 1);
   return mask_ix;
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

   if (mask == 0)
      xmnm = "nop";
   else if (mask == 15)
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


/* Return 1, if mask should be printed */
static Int
cabt_mdf(UInt ix __attribute__((unused)), UInt mask)
{
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

   return s390_disasm_aux(opnds, xmnm, p, cabt_mdf);
}


static Int
cls_mdf(UInt ix __attribute__((unused)), UInt mask)
{
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

   return s390_disasm_aux(opnds, xmnm, p, cls_mdf);
}


static Int
bic_mdf(UInt ix __attribute__((unused)), UInt mask)
{
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

   return s390_disasm_aux(opnds, xmnm, p, bic_mdf);
}


/* Write out OPNDS. */
static HChar *
s390_disasm_aux(const s390_opnd *opnds, const HChar *xmnm, HChar *p,
                Int (*mdf)(UInt, UInt))
{
   vassert(opnds[0].kind == S390_OPND_MNM ||
           opnds[0].kind == S390_OPND_XMNM);

   Int separator = 0;

   for (UInt ix = 0; opnds[ix].kind != S390_OPND_DONE; ++ix) {
      const s390_opnd *opnd = opnds + ix;

      if (ix > 1 && separator)
         *p++ = ',';

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

      case S390_OPND_MASK:
         if (mdf && mdf(ix, opnd->mask))
            p += vex_sprintf(p, "%u", opnd->mask);
         else {
            if (ix != 1)
               (*--p) = '\0';   // overwrite the separator
            else
               separator = 0;
         }
         continue;  // *not* break

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

      separator = ',';
   }
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
