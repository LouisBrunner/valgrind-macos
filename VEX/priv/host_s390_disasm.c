/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                host_s390_disasm.c ---*/
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

#include <stdarg.h>
#include "libvex_basictypes.h"
#include "main_util.h"        // vassert
#include "main_globals.h"     // vex_traceflags
#include "host_s390_disasm.h"

/* The format that is used to write out a mnemonic.
   These should be declared as 'const HChar' but vex_printf needs
   to be changed for that first */
static HChar s390_mnm_fmt[] = "%-8s";


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


/* Build and return the extended mnemonic for the compare and branch
   opcodes as introduced by z10. See also the opcodes in file
   opcodes/s390-opc.txt (from binutils) that have a '$' in their name. */
static const HChar *
cab_operand(const HChar *base, UInt mask)
{
   HChar *to;
   const HChar *from;

   static HChar buf[10];   /* Maximum is 6 + 2 */

   static HChar *suffix[] = {
      "", "h", "l", "ne", "e", "nl", "nh", ""
   };

   /* strcpy(buf, from); */
   for (from = base, to = buf; *from; ++from, ++to) {
      *to = *from;
   }
   /* strcat(buf, suffix); */
   for (from = suffix[mask >> 1]; *from; ++from, ++to) {
      *to = *from;
   }
   *to = '\0';

   return buf;
}


/* Return the special mnemonic for the BCR opcode */
static const HChar *
bcr_operand(UInt m1)
{
   static const HChar mnemonic[16][6] = {
      /* 0 */ "nopr",  /* no operation */
      /* 1 */ "bor",   /* branch on overflow / if ones */
      /* 2 */ "bhr",   /* branch on high */
      /* 3 */ "bnler", /* branch on not low or equal */
      /* 4 */ "blr",   /* branch on low */
      /* 5 */ "bnher", /* branch on not high or equal */
      /* 6 */ "blhr",  /* branch on low or high */
      /* 7 */ "bner",  /* branch on not equal */
      /* 8 */ "ber",   /* branch on equal */
      /* 9 */ "bnlhr", /* branch on not low or high */
      /* a */ "bher",  /* branch on high or equal */
      /* b */ "bnlr",  /* branch on not low */
      /* c */ "bler",  /* brach on low or equal */
      /* d */ "bnhr",  /* branch on not high */
      /* e */ "bnor",  /* branch on not overflow / if not ones */
      /* f */ "br",    /* unconditional branch */
   };

   return mnemonic[m1];
}


/* Return the special mnemonic for the BC opcode */
static const HChar *
bc_operand(UInt m1)
{
   static const HChar mnemonic[16][5] = {
      /* 0 */ "nop",  // no operation
      /* 1 */ "bo",   // branch on overflow / if ones
      /* 2 */ "bh",   // branch on high
      /* 3 */ "bnle", // branch on not low or equal
      /* 4 */ "bl",   // branch on low
      /* 5 */ "bnhe", // branch on not high or equal
      /* 6 */ "blh",  // branch on low or high
      /* 7 */ "bne",  // branch on not equal
      /* 8 */ "be",   // branch on equal
      /* 9 */ "bnlh", // branch on not low or high
      /* a */ "bhe",  // branch on high or equal
      /* b */ "bnl",  // branch on not low
      /* c */ "ble",  // branch on low or equal
      /* d */ "bnh",  // branch on not high
      /* e */ "bno",  // branch on not overflow / if not ones
      /* f */ "b"     // unconditional branch
   };

   return mnemonic[m1];
}


/* Return the special mnemonic for the BRC opcode */
static const HChar *
brc_operand(UInt m1)
{
   static const HChar mnemonic[16][5] = {
      /* 0 */ "brc",  /* no special mnemonic */
      /* 1 */ "jo",   /* jump on overflow / if ones */
      /* 2 */ "jh",   /* jump on A high */
      /* 3 */ "jnle", /* jump on not low or equal */
      /* 4 */ "jl",   /* jump on A low */
      /* 5 */ "jnhe", /* jump on not high or equal */
      /* 6 */ "jlh",  /* jump on low or high */
      /* 7 */ "jne",  /* jump on A not equal B */
      /* 8 */ "je",   /* jump on A equal B */
      /* 9 */ "jnlh", /* jump on not low or high */
      /* a */ "jhe",  /* jump on high or equal */
      /* b */ "jnl",  /* jump on A not low */
      /* c */ "jle",  /* jump on low or equal */
      /* d */ "jnh",  /* jump on A not high */
      /* e */ "jno",  /* jump on not overflow / if not ones */
      /* f */ "j",    /* jump */
   };

   return mnemonic[m1];
}


/* Return the special mnemonic for the BRCL opcode */
static const HChar *
brcl_operand(UInt m1)
{
   static const HChar mnemonic[16][6] = {
      /* 0 */ "brcl",  /* no special mnemonic */
      /* 1 */ "jgo",   /* jump long on overflow / if ones */
      /* 2 */ "jgh",   /* jump long on high */
      /* 3 */ "jgnle", /* jump long on not low or equal */
      /* 4 */ "jgl",   /* jump long on low */
      /* 5 */ "jgnhe", /* jump long on not high or equal */
      /* 6 */ "jglh",  /* jump long on low or high */
      /* 7 */ "jgne",  /* jump long on not equal */
      /* 8 */ "jge",   /* jump long on equal */
      /* 9 */ "jgnlh", /* jump long on not low or high */
      /* a */ "jghe",  /* jump long on high or equal */
      /* b */ "jgnl",  /* jump long on not low */
      /* c */ "jgle",  /* jump long on low or equal */
      /* d */ "jgnh",  /* jump long on not high */
      /* e */ "jgno",  /* jump long on not overflow / if not ones */
      /* f */ "jg",    /* jump long */
   };

   return mnemonic[m1];
}


/* An operand with a base register, an index register, and a displacement.
   If the displacement is signed, the rightmost 20 bit of D need to be
   sign extended */
static HChar *
dxb_operand(HChar *p, UInt d, UInt x, UInt b, Bool displacement_is_signed)
{
   if (displacement_is_signed) {
      Int displ = ((Int)d << 12) >> 12;  /* sign extend */

      p += vex_sprintf(p, "%d", displ);
   } else {
      p += vex_sprintf(p, "%u", d);
   }
   if (x != 0) {
      p += vex_sprintf(p, "(%s", gpr_operand(x));
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


/* An operand with base register, unsigned length, and a 12-bit
   unsigned displacement */
static HChar *
udlb_operand(HChar *p, UInt d, UInt length, UInt b)
{
   p += vex_sprintf(p, "%u", d);
   p += vex_sprintf(p, "(%u", length + 1);  // actual length is +1
   if (b != 0) {
      p += vex_sprintf(p, ",%s", gpr_operand(b));
   }
   p += vex_sprintf(p, ")");

   return p;
}


/* The first argument is the command that says how to write the disassembled
   insn. It is understood that the mnemonic comes first and that arguments
   are separated by a ','. The command holds the arguments. Each argument is
   encoded using a 4-bit S390_ARG_xyz value. The first argument is placed
   in the least significant bits of the command and so on. There are at most
   5 arguments in an insn and a sentinel (S390_ARG_DONE) is needed to identify
   the end of the argument list. 6 * 4 = 24 bits are required for the
   command. */
void
s390_disasm(UInt command, ...)
{
   va_list  args;
   unsigned argkind;
   HChar buf[128];  /* holds the disassembled insn */
   HChar *p;
   HChar separator;

   va_start(args, command);

   p = buf;
   separator = 0;

   while (42) {
      argkind = command & 0xF;
      command >>= 4;

      if (argkind == S390_ARG_DONE) goto done;

      if (argkind == S390_ARG_CABM) separator = 0;  /* optional */

      /* Write out the separator */
      if (separator) *p++ = separator;

      /* argument */
      switch (argkind) {
      case S390_ARG_MNM:
         p += vex_sprintf(p, s390_mnm_fmt, va_arg(args, HChar *));
         separator = ' ';
         continue;

      case S390_ARG_XMNM: {
         UInt mask, kind;
         const HChar *mnm;

         kind = va_arg(args, UInt);

         separator = ' ';
         switch (kind) {
         case S390_XMNM_BC:
         case S390_XMNM_BCR:
            mask = va_arg(args, UInt);
            mnm = kind == S390_XMNM_BCR ? bcr_operand(mask) : bc_operand(mask);
            p  += vex_sprintf(p, s390_mnm_fmt, mnm);
            /* mask == 0 is a NOP and has no argument */
            if (mask == 0) goto done;
            break;

         case S390_XMNM_BRC:
         case S390_XMNM_BRCL:
            mask = va_arg(args, UInt);
            mnm = kind == S390_XMNM_BRC ? brc_operand(mask) : brcl_operand(mask);
            p  += vex_sprintf(p, s390_mnm_fmt, mnm);

            /* mask == 0 has no special mnemonic */
            if (mask == 0) {
               p += vex_sprintf(p, " 0");
               separator = ',';
            }
            break;

         case S390_XMNM_CAB:
            mnm  = va_arg(args, HChar *);
            mask = va_arg(args, UInt);
            p  += vex_sprintf(p, s390_mnm_fmt, cab_operand(mnm, mask));
            break;
         }
      }
      continue;

      case S390_ARG_GPR:
         p += vex_sprintf(p, "%s", gpr_operand(va_arg(args, UInt)));
         break;

      case S390_ARG_FPR:
         p += vex_sprintf(p, "%s", fpr_operand(va_arg(args, UInt)));
         break;

      case S390_ARG_AR:
         p += vex_sprintf(p, "%s", ar_operand(va_arg(args, UInt)));
         break;

      case S390_ARG_UINT:
         p += vex_sprintf(p, "%u", va_arg(args, UInt));
         break;

      case S390_ARG_INT:
         p += vex_sprintf(p, "%d", (Int)(va_arg(args, UInt)));
         break;

      case S390_ARG_PCREL: {
         Int offset = (Int)(va_arg(args, UInt));

         /* Convert # halfwords to # bytes */
         offset <<= 1;

         if (offset < 0) {
            p += vex_sprintf(p, ".%d", offset);
         } else {
            p += vex_sprintf(p, ".+%u", offset);
         }
         break;
      }

      case S390_ARG_SDXB: {
         UInt dh, dl, x, b;

         dh = va_arg(args, UInt);
         dl = va_arg(args, UInt);
         x  = va_arg(args, UInt);
         b  = va_arg(args, UInt);

         p = dxb_operand(p, (dh << 12) | dl, x, b, 1 /* signed_displacement */);
         break;
      }

      case S390_ARG_UDXB: {
         UInt d, x, b;

         d = va_arg(args, UInt);
         x = va_arg(args, UInt);
         b = va_arg(args, UInt);

         p = dxb_operand(p, d, x, b, 0 /* signed_displacement */);
         break;
      }

      case S390_ARG_UDLB: {
         UInt d, l, b;

         d = va_arg(args, UInt);
         l = va_arg(args, UInt);
         b = va_arg(args, UInt);

         p = udlb_operand(p, d, l, b);
         break;
      }

      case S390_ARG_CABM: {
         UInt mask;

         mask = va_arg(args, UInt) & 0xE;
         if (mask == 0 || mask == 14) {
            p += vex_sprintf(p, ",%u", mask);
         }
         break;
      }
      }

      separator = ',';
   }

 done:
   va_end(args);

   *p = '\0';

   vassert(p < buf + sizeof buf);  /* detect buffer overwrite */

   /* Finally, write out the disassembled insn */
   vex_printf("%s\n", buf);
}

/*---------------------------------------------------------------*/
/*--- end                                  host_s390_disasm.c ---*/
/*---------------------------------------------------------------*/
