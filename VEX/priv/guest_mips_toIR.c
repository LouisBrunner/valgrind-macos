
/*--------------------------------------------------------------------*/
/*--- begin                                      guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2017 RT-RK
      mips-valgrind@rt-rk.com

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

/* Translates MIPS code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_mips32.h"
#include "libvex_guest_mips64.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_mips_defs.h"
#include "mips_defs.h"

/*------------------------------------------------------------*/
/*---                      Globals                         ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly. CONST means does
   not change during translation of the instruction. */

/* CONST: what is the host's endianness?  This has to do with float vs
   double register accesses on VFP, but it's complex and not properly
   thought out. */
static VexEndness host_endness;

/* Pointer to the guest code area. */
const UChar *guest_code;

/* CONST: The guest address for the instruction currently being
   translated. */
#if defined(VGP_mips32_linux)
static Addr32 guest_PC_curr_instr;
#else
static Addr64 guest_PC_curr_instr;
#endif

/* MOD: The IRSB* into which we're generating code. */
IRSB *irsb;

/* Is our guest binary 32 or 64bit? Set at each call to
   disInstr_MIPS below. */
Bool mode64 = False;

/* CPU has FPU and 32 dbl. prec. FP registers. */
 static Bool fp_mode64 = False;

/* FPU works in FRE mode */
static Bool fp_mode64_fre = False;

/* CPU has MSA unit */
static Bool has_msa = False;

/* Define 1.0 in single and double precision. */
#define ONE_SINGLE 0x3F800000
#define ONE_DOUBLE 0x3FF0000000000000ULL

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- mips insn stream.                                    ---*/
/*------------------------------------------------------------*/

/* ---------------- Integer registers ---------------- */

static UInt integerGuestRegOffset(UInt iregNo)
{
   /* Do we care about endianness here?  We do if sub-parts of integer
      registers are accessed, but I don't think that ever happens on
      MIPS. */
   UInt ret;

   if (!mode64)
      switch (iregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS32State, guest_r0);
            break;

         case 1:
            ret = offsetof(VexGuestMIPS32State, guest_r1);
            break;

         case 2:
            ret = offsetof(VexGuestMIPS32State, guest_r2);
            break;

         case 3:
            ret = offsetof(VexGuestMIPS32State, guest_r3);
            break;

         case 4:
            ret = offsetof(VexGuestMIPS32State, guest_r4);
            break;

         case 5:
            ret = offsetof(VexGuestMIPS32State, guest_r5);
            break;

         case 6:
            ret = offsetof(VexGuestMIPS32State, guest_r6);
            break;

         case 7:
            ret = offsetof(VexGuestMIPS32State, guest_r7);
            break;

         case 8:
            ret = offsetof(VexGuestMIPS32State, guest_r8);
            break;

         case 9:
            ret = offsetof(VexGuestMIPS32State, guest_r9);
            break;

         case 10:
            ret = offsetof(VexGuestMIPS32State, guest_r10);
            break;

         case 11:
            ret = offsetof(VexGuestMIPS32State, guest_r11);
            break;

         case 12:
            ret = offsetof(VexGuestMIPS32State, guest_r12);
            break;

         case 13:
            ret = offsetof(VexGuestMIPS32State, guest_r13);
            break;

         case 14:
            ret = offsetof(VexGuestMIPS32State, guest_r14);
            break;

         case 15:
            ret = offsetof(VexGuestMIPS32State, guest_r15);
            break;

         case 16:
            ret = offsetof(VexGuestMIPS32State, guest_r16);
            break;

         case 17:
            ret = offsetof(VexGuestMIPS32State, guest_r17);
            break;

         case 18:
            ret = offsetof(VexGuestMIPS32State, guest_r18);
            break;

         case 19:
            ret = offsetof(VexGuestMIPS32State, guest_r19);
            break;

         case 20:
            ret = offsetof(VexGuestMIPS32State, guest_r20);
            break;

         case 21:
            ret = offsetof(VexGuestMIPS32State, guest_r21);
            break;

         case 22:
            ret = offsetof(VexGuestMIPS32State, guest_r22);
            break;

         case 23:
            ret = offsetof(VexGuestMIPS32State, guest_r23);
            break;

         case 24:
            ret = offsetof(VexGuestMIPS32State, guest_r24);
            break;

         case 25:
            ret = offsetof(VexGuestMIPS32State, guest_r25);
            break;

         case 26:
            ret = offsetof(VexGuestMIPS32State, guest_r26);
            break;

         case 27:
            ret = offsetof(VexGuestMIPS32State, guest_r27);
            break;

         case 28:
            ret = offsetof(VexGuestMIPS32State, guest_r28);
            break;

         case 29:
            ret = offsetof(VexGuestMIPS32State, guest_r29);
            break;

         case 30:
            ret = offsetof(VexGuestMIPS32State, guest_r30);
            break;

         case 31:
            ret = offsetof(VexGuestMIPS32State, guest_r31);
            break;

         default:
            vassert(0);
            break;
      }
   else
      switch (iregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS64State, guest_r0);
            break;

         case 1:
            ret = offsetof(VexGuestMIPS64State, guest_r1);
            break;

         case 2:
            ret = offsetof(VexGuestMIPS64State, guest_r2);
            break;

         case 3:
            ret = offsetof(VexGuestMIPS64State, guest_r3);
            break;

         case 4:
            ret = offsetof(VexGuestMIPS64State, guest_r4);
            break;

         case 5:
            ret = offsetof(VexGuestMIPS64State, guest_r5);
            break;

         case 6:
            ret = offsetof(VexGuestMIPS64State, guest_r6);
            break;

         case 7:
            ret = offsetof(VexGuestMIPS64State, guest_r7);
            break;

         case 8:
            ret = offsetof(VexGuestMIPS64State, guest_r8);
            break;

         case 9:
            ret = offsetof(VexGuestMIPS64State, guest_r9);
            break;

         case 10:
            ret = offsetof(VexGuestMIPS64State, guest_r10);
            break;

         case 11:
            ret = offsetof(VexGuestMIPS64State, guest_r11);
            break;

         case 12:
            ret = offsetof(VexGuestMIPS64State, guest_r12);
            break;

         case 13:
            ret = offsetof(VexGuestMIPS64State, guest_r13);
            break;

         case 14:
            ret = offsetof(VexGuestMIPS64State, guest_r14);
            break;

         case 15:
            ret = offsetof(VexGuestMIPS64State, guest_r15);
            break;

         case 16:
            ret = offsetof(VexGuestMIPS64State, guest_r16);
            break;

         case 17:
            ret = offsetof(VexGuestMIPS64State, guest_r17);
            break;

         case 18:
            ret = offsetof(VexGuestMIPS64State, guest_r18);
            break;

         case 19:
            ret = offsetof(VexGuestMIPS64State, guest_r19);
            break;

         case 20:
            ret = offsetof(VexGuestMIPS64State, guest_r20);
            break;

         case 21:
            ret = offsetof(VexGuestMIPS64State, guest_r21);
            break;

         case 22:
            ret = offsetof(VexGuestMIPS64State, guest_r22);
            break;

         case 23:
            ret = offsetof(VexGuestMIPS64State, guest_r23);
            break;

         case 24:
            ret = offsetof(VexGuestMIPS64State, guest_r24);
            break;

         case 25:
            ret = offsetof(VexGuestMIPS64State, guest_r25);
            break;

         case 26:
            ret = offsetof(VexGuestMIPS64State, guest_r26);
            break;

         case 27:
            ret = offsetof(VexGuestMIPS64State, guest_r27);
            break;

         case 28:
            ret = offsetof(VexGuestMIPS64State, guest_r28);
            break;

         case 29:
            ret = offsetof(VexGuestMIPS64State, guest_r29);
            break;

         case 30:
            ret = offsetof(VexGuestMIPS64State, guest_r30);
            break;

         case 31:
            ret = offsetof(VexGuestMIPS64State, guest_r31);
            break;

         default:
            vassert(0);
            break;
      }

   return ret;
}

#if defined(VGP_mips32_linux)
#define OFFB_PC     offsetof(VexGuestMIPS32State, guest_PC)
#else
#define OFFB_PC     offsetof(VexGuestMIPS64State, guest_PC)
#endif

/* ---------------- Floating point registers ---------------- */

static UInt floatGuestRegOffset(UInt fregNo)
{
   vassert(fregNo < 32);
   UInt ret;

   if (!mode64)
      switch (fregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS32State, guest_f0);
            break;

         case 1:
            ret = offsetof(VexGuestMIPS32State, guest_f1);
            break;

         case 2:
            ret = offsetof(VexGuestMIPS32State, guest_f2);
            break;

         case 3:
            ret = offsetof(VexGuestMIPS32State, guest_f3);
            break;

         case 4:
            ret = offsetof(VexGuestMIPS32State, guest_f4);
            break;

         case 5:
            ret = offsetof(VexGuestMIPS32State, guest_f5);
            break;

         case 6:
            ret = offsetof(VexGuestMIPS32State, guest_f6);
            break;

         case 7:
            ret = offsetof(VexGuestMIPS32State, guest_f7);
            break;

         case 8:
            ret = offsetof(VexGuestMIPS32State, guest_f8);
            break;

         case 9:
            ret = offsetof(VexGuestMIPS32State, guest_f9);
            break;

         case 10:
            ret = offsetof(VexGuestMIPS32State, guest_f10);
            break;

         case 11:
            ret = offsetof(VexGuestMIPS32State, guest_f11);
            break;

         case 12:
            ret = offsetof(VexGuestMIPS32State, guest_f12);
            break;

         case 13:
            ret = offsetof(VexGuestMIPS32State, guest_f13);
            break;

         case 14:
            ret = offsetof(VexGuestMIPS32State, guest_f14);
            break;

         case 15:
            ret = offsetof(VexGuestMIPS32State, guest_f15);
            break;

         case 16:
            ret = offsetof(VexGuestMIPS32State, guest_f16);
            break;

         case 17:
            ret = offsetof(VexGuestMIPS32State, guest_f17);
            break;

         case 18:
            ret = offsetof(VexGuestMIPS32State, guest_f18);
            break;

         case 19:
            ret = offsetof(VexGuestMIPS32State, guest_f19);
            break;

         case 20:
            ret = offsetof(VexGuestMIPS32State, guest_f20);
            break;

         case 21:
            ret = offsetof(VexGuestMIPS32State, guest_f21);
            break;

         case 22:
            ret = offsetof(VexGuestMIPS32State, guest_f22);
            break;

         case 23:
            ret = offsetof(VexGuestMIPS32State, guest_f23);
            break;

         case 24:
            ret = offsetof(VexGuestMIPS32State, guest_f24);
            break;

         case 25:
            ret = offsetof(VexGuestMIPS32State, guest_f25);
            break;

         case 26:
            ret = offsetof(VexGuestMIPS32State, guest_f26);
            break;

         case 27:
            ret = offsetof(VexGuestMIPS32State, guest_f27);
            break;

         case 28:
            ret = offsetof(VexGuestMIPS32State, guest_f28);
            break;

         case 29:
            ret = offsetof(VexGuestMIPS32State, guest_f29);
            break;

         case 30:
            ret = offsetof(VexGuestMIPS32State, guest_f30);
            break;

         case 31:
            ret = offsetof(VexGuestMIPS32State, guest_f31);
            break;

         default:
            vassert(0);
            break;
      }
   else
      switch (fregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS64State, guest_f0);
            break;

         case 1:
            ret = offsetof(VexGuestMIPS64State, guest_f1);
            break;

         case 2:
            ret = offsetof(VexGuestMIPS64State, guest_f2);
            break;

         case 3:
            ret = offsetof(VexGuestMIPS64State, guest_f3);
            break;

         case 4:
            ret = offsetof(VexGuestMIPS64State, guest_f4);
            break;

         case 5:
            ret = offsetof(VexGuestMIPS64State, guest_f5);
            break;

         case 6:
            ret = offsetof(VexGuestMIPS64State, guest_f6);
            break;

         case 7:
            ret = offsetof(VexGuestMIPS64State, guest_f7);
            break;

         case 8:
            ret = offsetof(VexGuestMIPS64State, guest_f8);
            break;

         case 9:
            ret = offsetof(VexGuestMIPS64State, guest_f9);
            break;

         case 10:
            ret = offsetof(VexGuestMIPS64State, guest_f10);
            break;

         case 11:
            ret = offsetof(VexGuestMIPS64State, guest_f11);
            break;

         case 12:
            ret = offsetof(VexGuestMIPS64State, guest_f12);
            break;

         case 13:
            ret = offsetof(VexGuestMIPS64State, guest_f13);
            break;

         case 14:
            ret = offsetof(VexGuestMIPS64State, guest_f14);
            break;

         case 15:
            ret = offsetof(VexGuestMIPS64State, guest_f15);
            break;

         case 16:
            ret = offsetof(VexGuestMIPS64State, guest_f16);
            break;

         case 17:
            ret = offsetof(VexGuestMIPS64State, guest_f17);
            break;

         case 18:
            ret = offsetof(VexGuestMIPS64State, guest_f18);
            break;

         case 19:
            ret = offsetof(VexGuestMIPS64State, guest_f19);
            break;

         case 20:
            ret = offsetof(VexGuestMIPS64State, guest_f20);
            break;

         case 21:
            ret = offsetof(VexGuestMIPS64State, guest_f21);
            break;

         case 22:
            ret = offsetof(VexGuestMIPS64State, guest_f22);
            break;

         case 23:
            ret = offsetof(VexGuestMIPS64State, guest_f23);
            break;

         case 24:
            ret = offsetof(VexGuestMIPS64State, guest_f24);
            break;

         case 25:
            ret = offsetof(VexGuestMIPS64State, guest_f25);
            break;

         case 26:
            ret = offsetof(VexGuestMIPS64State, guest_f26);
            break;

         case 27:
            ret = offsetof(VexGuestMIPS64State, guest_f27);
            break;

         case 28:
            ret = offsetof(VexGuestMIPS64State, guest_f28);
            break;

         case 29:
            ret = offsetof(VexGuestMIPS64State, guest_f29);
            break;

         case 30:
            ret = offsetof(VexGuestMIPS64State, guest_f30);
            break;

         case 31:
            ret = offsetof(VexGuestMIPS64State, guest_f31);
            break;

         default:
            vassert(0);
            break;
      }

   return ret;
}

/* ---------------- MIPS32 DSP ASE(r2) accumulators ---------------- */

UInt accumulatorGuestRegOffset(UInt acNo)
{
   vassert(!mode64);
   vassert(acNo <= 3);
   UInt ret;

   switch (acNo) {
      case 0:
         ret = offsetof(VexGuestMIPS32State, guest_ac0);
         break;

      case 1:
         ret = offsetof(VexGuestMIPS32State, guest_ac1);
         break;

      case 2:
         ret = offsetof(VexGuestMIPS32State, guest_ac2);
         break;

      case 3:
         ret = offsetof(VexGuestMIPS32State, guest_ac3);
         break;

      default:
         vassert(0);
         break;
   }

   return ret;
}

/* ---------------- MIPS32 MSA registers ---------------- */

static UInt msaGuestRegOffset(UInt msaRegNo)
{
   vassert(msaRegNo <= 31);
   UInt ret;

   if (mode64) {
      switch (msaRegNo) {
         case 0:
            ret = offsetof(VexGuestMIPS64State, guest_w0);
            break;

         case 1:
            ret = offsetof(VexGuestMIPS64State, guest_w1);
            break;

         case 2:
            ret = offsetof(VexGuestMIPS64State, guest_w2);
            break;

         case 3:
            ret = offsetof(VexGuestMIPS64State, guest_w3);
            break;

         case 4:
            ret = offsetof(VexGuestMIPS64State, guest_w4);
            break;

         case 5:
            ret = offsetof(VexGuestMIPS64State, guest_w5);
            break;

         case 6:
            ret = offsetof(VexGuestMIPS64State, guest_w6);
            break;

         case 7:
            ret = offsetof(VexGuestMIPS64State, guest_w7);
            break;

         case 8:
            ret = offsetof(VexGuestMIPS64State, guest_w8);
            break;

         case 9:
            ret = offsetof(VexGuestMIPS64State, guest_w9);
            break;

         case 10:
            ret = offsetof(VexGuestMIPS64State, guest_w10);
            break;

         case 11:
            ret = offsetof(VexGuestMIPS64State, guest_w11);
            break;

         case 12:
            ret = offsetof(VexGuestMIPS64State, guest_w12);
            break;

         case 13:
            ret = offsetof(VexGuestMIPS64State, guest_w13);
            break;

         case 14:
            ret = offsetof(VexGuestMIPS64State, guest_w14);
            break;

         case 15:
            ret = offsetof(VexGuestMIPS64State, guest_w15);
            break;

         case 16:
            ret = offsetof(VexGuestMIPS64State, guest_w16);
            break;

         case 17:
            ret = offsetof(VexGuestMIPS64State, guest_w17);
            break;

         case 18:
            ret = offsetof(VexGuestMIPS64State, guest_w18);
            break;

         case 19:
            ret = offsetof(VexGuestMIPS64State, guest_w19);
            break;

         case 20:
            ret = offsetof(VexGuestMIPS64State, guest_w20);
            break;

         case 21:
            ret = offsetof(VexGuestMIPS64State, guest_w21);
            break;

         case 22:
            ret = offsetof(VexGuestMIPS64State, guest_w22);
            break;

         case 23:
            ret = offsetof(VexGuestMIPS64State, guest_w23);
            break;

         case 24:
            ret = offsetof(VexGuestMIPS64State, guest_w24);
            break;

         case 25:
            ret = offsetof(VexGuestMIPS64State, guest_w25);
            break;

         case 26:
            ret = offsetof(VexGuestMIPS64State, guest_w26);
            break;

         case 27:
            ret = offsetof(VexGuestMIPS64State, guest_w27);
            break;

         case 28:
            ret = offsetof(VexGuestMIPS64State, guest_w28);
            break;

         case 29:
            ret = offsetof(VexGuestMIPS64State, guest_w29);
            break;

         case 30:
            ret = offsetof(VexGuestMIPS64State, guest_w30);
            break;

         case 31:
            ret = offsetof(VexGuestMIPS64State, guest_w31);
            break;

         default:
            vassert(0);
            break;
      }
   } else {
      switch (msaRegNo) {
         case 0:
            ret = offsetof(VexGuestMIPS32State, guest_w0);
            break;

         case 1:
            ret = offsetof(VexGuestMIPS32State, guest_w1);
            break;

         case 2:
            ret = offsetof(VexGuestMIPS32State, guest_w2);
            break;

         case 3:
            ret = offsetof(VexGuestMIPS32State, guest_w3);
            break;

         case 4:
            ret = offsetof(VexGuestMIPS32State, guest_w4);
            break;

         case 5:
            ret = offsetof(VexGuestMIPS32State, guest_w5);
            break;

         case 6:
            ret = offsetof(VexGuestMIPS32State, guest_w6);
            break;

         case 7:
            ret = offsetof(VexGuestMIPS32State, guest_w7);
            break;

         case 8:
            ret = offsetof(VexGuestMIPS32State, guest_w8);
            break;

         case 9:
            ret = offsetof(VexGuestMIPS32State, guest_w9);
            break;

         case 10:
            ret = offsetof(VexGuestMIPS32State, guest_w10);
            break;

         case 11:
            ret = offsetof(VexGuestMIPS32State, guest_w11);
            break;

         case 12:
            ret = offsetof(VexGuestMIPS32State, guest_w12);
            break;

         case 13:
            ret = offsetof(VexGuestMIPS32State, guest_w13);
            break;

         case 14:
            ret = offsetof(VexGuestMIPS32State, guest_w14);
            break;

         case 15:
            ret = offsetof(VexGuestMIPS32State, guest_w15);
            break;

         case 16:
            ret = offsetof(VexGuestMIPS32State, guest_w16);
            break;

         case 17:
            ret = offsetof(VexGuestMIPS32State, guest_w17);
            break;

         case 18:
            ret = offsetof(VexGuestMIPS32State, guest_w18);
            break;

         case 19:
            ret = offsetof(VexGuestMIPS32State, guest_w19);
            break;

         case 20:
            ret = offsetof(VexGuestMIPS32State, guest_w20);
            break;

         case 21:
            ret = offsetof(VexGuestMIPS32State, guest_w21);
            break;

         case 22:
            ret = offsetof(VexGuestMIPS32State, guest_w22);
            break;

         case 23:
            ret = offsetof(VexGuestMIPS32State, guest_w23);
            break;

         case 24:
            ret = offsetof(VexGuestMIPS32State, guest_w24);
            break;

         case 25:
            ret = offsetof(VexGuestMIPS32State, guest_w25);
            break;

         case 26:
            ret = offsetof(VexGuestMIPS32State, guest_w26);
            break;

         case 27:
            ret = offsetof(VexGuestMIPS32State, guest_w27);
            break;

         case 28:
            ret = offsetof(VexGuestMIPS32State, guest_w28);
            break;

         case 29:
            ret = offsetof(VexGuestMIPS32State, guest_w29);
            break;

         case 30:
            ret = offsetof(VexGuestMIPS32State, guest_w30);
            break;

         case 31:
            ret = offsetof(VexGuestMIPS32State, guest_w31);
            break;

         default:
            vassert(0);
            break;
      }
   }

   return ret;
}


/* Do a endian load of a 32-bit word, regardless of the endianness of the
   underlying host. */
static inline UInt getUInt(const UChar * p)
{
   UInt w = 0;
#if defined (_MIPSEL)
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
#elif defined (_MIPSEB)
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
#endif
   return w;
}

#define BITS2(_b1,_b0) \
   (((_b1) << 1) | (_b0))

#define BITS3(_b2,_b1,_b0)                      \
  (((_b2) << 2) | ((_b1) << 1) | (_b0))

#define BITS4(_b3,_b2,_b1,_b0) \
   (((_b3) << 3) | ((_b2) << 2) | ((_b1) << 1) | (_b0))

#define BITS5(_b4,_b3,_b2,_b1,_b0)  \
   (((_b4) << 4) | BITS4((_b3),(_b2),(_b1),(_b0)))

#define BITS6(_b5,_b4,_b3,_b2,_b1,_b0)  \
   ((BITS2((_b5),(_b4)) << 4) \
    | BITS4((_b3),(_b2),(_b1),(_b0)))

#define BITS8(_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   ((BITS4((_b7),(_b6),(_b5),(_b4)) << 4) \
    | BITS4((_b3),(_b2),(_b1),(_b0)))

#define LOAD_STORE_PATTERN \
   t1 = newTemp(mode64 ? Ity_I64 : Ity_I32); \
      if(!mode64) \
         assign(t1, binop(Iop_Add32, getIReg(rs), \
                                     mkU32(extend_s_16to32(imm)))); \
      else \
         assign(t1, binop(Iop_Add64, getIReg(rs), \
                                     mkU64(extend_s_16to64(imm)))); \

#define LOAD_STORE_PATTERN_MSA(imm) \
   t1 = newTemp(mode64 ? Ity_I64 : Ity_I32); \
      if (!mode64) \
         assign(t1, binop(Iop_Add32, getIReg(ws),  \
                                     mkU32(extend_s_10to32(imm)))); \
      else \
         assign(t1, binop(Iop_Add64, getIReg(ws), \
                                     mkU64(extend_s_10to64(imm)))); \

#define LOADX_STORE_PATTERN \
   t1 = newTemp(mode64 ? Ity_I64 : Ity_I32); \
      if(!mode64) \
         assign(t1, binop(Iop_Add32, getIReg(regRs), getIReg(regRt))); \
      else \
         assign(t1, binop(Iop_Add64, getIReg(regRs), getIReg(regRt)));

#define LWX_SWX_PATTERN64 \
   t2 = newTemp(Ity_I64); \
   assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFFCULL))); \
   t4 = newTemp(Ity_I32); \
   assign(t4, mkNarrowTo32( ty, binop(Iop_And64, \
                                      mkexpr(t1), mkU64(0x3))));

#define LWX_SWX_PATTERN64_1 \
   t2 = newTemp(Ity_I64); \
   assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFF8ULL))); \
   t4 = newTemp(Ity_I64); \
   assign(t4, binop(Iop_And64, mkexpr(t1), mkU64(0x7)));

#define LWX_SWX_PATTERN \
   t2 = newTemp(Ity_I32); \
   assign(t2, binop(Iop_And32, mkexpr(t1), mkU32(0xFFFFFFFC))); \
   t4 = newTemp(Ity_I32); \
   assign(t4, binop(Iop_And32, mkexpr(t1), mkU32(0x00000003)))

#define SXXV_PATTERN(op) \
   putIReg(rd, binop(op, \
         getIReg(rt), \
            unop(Iop_32to8, \
               binop(Iop_And32, \
                  getIReg(rs), \
                  mkU32(0x0000001F) \
               ) \
            ) \
         ) \
      )

#define SXXV_PATTERN64(op) \
   putIReg(rd, mkWidenFrom32(ty, binop(op, \
           mkNarrowTo32(ty, getIReg(rt)), \
             unop(Iop_32to8, \
                binop(Iop_And32, \
                   mkNarrowTo32(ty, getIReg(rs)), \
                   mkU32(0x0000001F) \
                ) \
             ) \
          ), True \
       ))

#define SXX_PATTERN(op) \
   putIReg(rd, binop(op, getIReg(rt), mkU8(sa)));

#define ALU_PATTERN(op) \
   putIReg(rd, binop(op, getIReg(rs), getIReg(rt)));

#define ALUI_PATTERN(op) \
   putIReg(rt, binop(op, getIReg(rs), mkU32(imm)));

#define ALUI_PATTERN64(op) \
   putIReg(rt, binop(op, getIReg(rs), mkU64(imm)));

#define ALU_PATTERN64(op) \
   putIReg(rd, mkWidenFrom32(ty, binop(op, \
                             mkNarrowTo32(ty, getIReg(rs)), \
                             mkNarrowTo32(ty, getIReg(rt))), True));

#define FP_CONDITIONAL_CODE \
   t3 = newTemp(Ity_I32);   \
   assign(t3, binop(Iop_And32, \
                 IRExpr_ITE( binop(Iop_CmpEQ32, mkU32(cc), mkU32(0)), \
                             binop(Iop_Shr32, getFCSR(), mkU8(23)), \
                             binop(Iop_Shr32, getFCSR(), mkU8(24+cc))), \
                 mkU32(0x1)));



#define ILLEGAL_INSTRUCTON \
   putPC(mkU32(guest_PC_curr_instr + 4)); \
   dres->jk_StopHere = Ijk_SigILL; \
   dres->whatNext    = Dis_StopHere;

#define LLADDR_INVALID \
   (mode64 ? mkU64(0xFFFFFFFFFFFFFFFFULL) : mkU32(0xFFFFFFFF))

/*------------------------------------------------------------*/
/*---                  Field helpers                       ---*/
/*------------------------------------------------------------*/

static Bool branch_or_jump(const UChar * addr)
{
   UInt fmt;
   UInt cins = getUInt(addr);

   UInt opcode = get_opcode(cins);
   UInt rt = get_rt(cins);
   UInt function = get_function(cins);

   /* bgtz, blez, bne, beq, jal */
   if (opcode == 0x07 || opcode == 0x06 || opcode == 0x05 || opcode == 0x04
       || opcode == 0x03 || opcode == 0x02) {
      return True;
   }

   /* bgez */
   if (opcode == 0x01 && rt == 0x01) {
      return True;
   }

   /* bgezal */
   if (opcode == 0x01 && rt == 0x11) {
      return True;
   }

   /* bltzal */
   if (opcode == 0x01 && rt == 0x10) {
      return True;
   }

   /* bltz */
   if (opcode == 0x01 && rt == 0x00) {
      return True;
   }

   /* jalr */
   if (opcode == 0x00 && function == 0x09) {
      return True;
   }

   /* jr */
   if (opcode == 0x00 && function == 0x08) {
      return True;
   }

   if (opcode == 0x11) {
      /* bc1f & bc1t */
      fmt = get_fmt(cins);

      if (fmt == 0x08) {
         return True;
      }

      /* MSA branches */
      /* bnz.df, bz.df */
      if (fmt >= 0x18) {
         return True;
      }

      /* bnz.v */
      if (fmt == 0x0f) {
         return True;
      }

      /* bz.v */
      if (fmt == 0x0b) {
         return True;
      }

      /* R6 branches */
      /* bc1eqz */
      if (fmt == 0x09) {
         return True;
      }

      /* bc1nez */
      if (fmt == 0x0D) {
         return True;
      }
   }

   /* bposge32 */
   if (opcode == 0x01 && rt == 0x1c) {
      return True;
   }

   /* Cavium Specific instructions. */
   if (opcode == 0x32 || opcode == 0x3A || opcode == 0x36 || opcode == 0x3E) {
      /* BBIT0, BBIT1, BBIT032, BBIT132 */
      return True;
   }

   return False;
}

static Bool is_Branch_or_Jump_and_Link(const UChar * addr)
{
   UInt cins = getUInt(addr);

   UInt opcode = get_opcode(cins);
   UInt rt = get_rt(cins);
   UInt function = get_function(cins);

   /* jal */
   if (opcode == 0x02) {
      return True;
   }

   /* bgezal or bal(r6) */
   if (opcode == 0x01 && rt == 0x11) {
      return True;
   }

   /* bltzal */
   if (opcode == 0x01 && rt == 0x10) {
      return True;
   }

   /* jalr */
   if (opcode == 0x00 && function == 0x09) {
      return True;
   }

   return False;
}

static Bool branch_or_link_likely(const UChar * addr)
{
   UInt cins = getUInt(addr);
   UInt opcode = get_opcode(cins);
   UInt rt = get_rt(cins);

   /* bgtzl, blezl, bnel, beql */
   if (opcode == 0x17 || opcode == 0x16 || opcode == 0x15 || opcode == 0x14)
      return True;

   /* bgezl */
   if (opcode == 0x01 && rt == 0x03)
      return True;

   /* bgezall */
   if (opcode == 0x01 && rt == 0x13)
      return True;

   /* bltzall */
   if (opcode == 0x01 && rt == 0x12)
      return True;

   /* bltzl */
   if (opcode == 0x01 && rt == 0x02)
      return True;

   return False;
}

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

/* Generate an expression for SRC rotated right by ROT. */
static IRExpr *genROR32(IRExpr * src, Int rot)
{
   vassert(rot >= 0 && rot < 32);

   if (rot == 0)
      return src;

   return binop(Iop_Or32, binop(Iop_Shl32, src, mkU8(32 - rot)),
                binop(Iop_Shr32, src, mkU8(rot)));
}

static IRExpr *genRORV32(IRExpr * src, IRExpr * rs)
{
   IRTemp t0 = newTemp(Ity_I8);
   IRTemp t1 = newTemp(Ity_I8);

   assign(t0, unop(Iop_32to8, binop(Iop_And32, rs, mkU32(0x0000001F))));
   assign(t1, binop(Iop_Sub8, mkU8(32), mkexpr(t0)));
   return binop(Iop_Or32, binop(Iop_Shl32, src, mkexpr(t1)),
                binop(Iop_Shr32, src, mkexpr(t0)));
}

static void jmp_lit32 ( /*MOD*/ DisResult* dres, IRJumpKind kind, Addr32 d32 )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt( IRStmt_Put( OFFB_PC, mkU32(d32) ) );
}

static void jmp_lit64 ( /*MOD*/ DisResult* dres, IRJumpKind kind, Addr64 d64 )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt(IRStmt_Put(OFFB_PC, mkU64(d64)));
}

/* Get value from accumulator (helper function for MIPS32 DSP ASE instructions).
   This function should be called before any other operation if widening
   multiplications are used. */
IRExpr *getAcc(UInt acNo)
{
   vassert(!mode64);
   vassert(acNo <= 3);
   return IRExpr_Get(accumulatorGuestRegOffset(acNo), Ity_I64);
}

/* Get value from DSPControl register (helper function for MIPS32 DSP ASE
   instructions). */
IRExpr *getDSPControl(void)
{
   vassert(!mode64);
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_DSPControl), Ity_I32);
}

/* Fetch a byte from the guest insn stream. */
static UChar getIByte(Int delta)
{
   return guest_code[delta];
}

IRExpr *getIReg(UInt iregNo)
{
   if (0 == iregNo) {
      return mode64 ? mkU64(0x0) : mkU32(0x0);
   } else {
      IRType ty = mode64 ? Ity_I64 : Ity_I32;
      vassert(iregNo < 32);
      return IRExpr_Get(integerGuestRegOffset(iregNo), ty);
   }
}

static IRExpr *getWReg(UInt wregNo)
{
   vassert(wregNo <= 31);
   return IRExpr_Get(msaGuestRegOffset(wregNo), Ity_V128);
}

static IRExpr *getHI(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_HI), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_HI), Ity_I32);
}

static IRExpr *getLO(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_LO), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LO), Ity_I32);
}

static IRExpr *getFCSR(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_FCSR), Ity_I32);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_FCSR), Ity_I32);
}

static IRExpr *getLLaddr(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_LLaddr), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LLaddr), Ity_I32);
}

static IRExpr *getLLdata(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_LLdata), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LLdata), Ity_I32);
}

static IRExpr *getMSACSR(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_MSACSR), Ity_I32);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_MSACSR), Ity_I32);
}

/* Get byte from register reg, byte pos from 0 to 3 (or 7 for MIPS64) . */
static IRExpr *getByteFromReg(UInt reg, UInt byte_pos)
{
   UInt pos = byte_pos * 8;

   if (mode64)
      return unop(Iop_64to8, binop(Iop_And64,
                                   binop(Iop_Shr64, getIReg(reg), mkU8(pos)),
                                   mkU64(0xFF)));
   else
      return unop(Iop_32to8, binop(Iop_And32,
                                   binop(Iop_Shr32, getIReg(reg), mkU8(pos)),
                                   mkU32(0xFF)));
}

static void putFCSR(IRExpr * e)
{
   if (mode64)
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_FCSR), e));
   else
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_FCSR), e));
}

static void putLLaddr(IRExpr * e)
{
   if (mode64)
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_LLaddr), e));
   else
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LLaddr), e));
}

static void putLLdata(IRExpr * e)
{
   if (mode64)
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_LLdata), e));
   else
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LLdata), e));
}

static void putMSACSR(IRExpr * e)
{
   if (mode64)
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_MSACSR), e));
   else
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_MSACSR), e));
}

/* fs   - fpu source register number.
   inst - fpu instruction that needs to be executed.
   sz32 - size of source register.
   opN  - number of operads:
          1 - unary operation.
          2 - binary operation. */
static void calculateFCSR(UInt fs, UInt ft, UInt inst, Bool sz32, UInt opN)
{
   IRDirty *d;
   IRTemp fcsr = newTemp(Ity_I32);

   /* IRExpr_GSPTR() => Need to pass pointer to guest state to helper. */
   if (fp_mode64)
      d = unsafeIRDirty_1_N(fcsr, 0,
                            "mips_dirtyhelper_calculate_FCSR_fp64",
                            &mips_dirtyhelper_calculate_FCSR_fp64,
                            mkIRExprVec_4(IRExpr_GSPTR(),
                                          mkU32(fs),
                                          mkU32(ft),
                                          mkU32(inst)));
   else
      d = unsafeIRDirty_1_N(fcsr, 0,
                            "mips_dirtyhelper_calculate_FCSR_fp32",
                            &mips_dirtyhelper_calculate_FCSR_fp32,
                            mkIRExprVec_4(IRExpr_GSPTR(),
                                          mkU32(fs),
                                          mkU32(ft),
                                          mkU32(inst)));

   if (opN == 1) {  /* Unary operation. */
      /* Declare we're reading guest state. */
      if (sz32 || fp_mode64)
         d->nFxState = 2;
      else
         d->nFxState = 3;

      vex_bzero(&d->fxState, sizeof(d->fxState));

      d->fxState[0].fx     = Ifx_Read;  /* read */

      if (mode64)
         d->fxState[0].offset = offsetof(VexGuestMIPS64State, guest_FCSR);
      else
         d->fxState[0].offset = offsetof(VexGuestMIPS32State, guest_FCSR);

      d->fxState[0].size   = sizeof(UInt);
      d->fxState[1].fx     = Ifx_Read;  /* read */
      d->fxState[1].offset = floatGuestRegOffset(fs);
      d->fxState[1].size   = sizeof(ULong);

      if (!(sz32 || fp_mode64)) {
         d->fxState[2].fx     = Ifx_Read;  /* read */
         d->fxState[2].offset = floatGuestRegOffset(fs + 1);
         d->fxState[2].size   = sizeof(ULong);
      }
   } else if (opN == 2) {  /* Binary operation. */
      /* Declare we're reading guest state. */
      if (sz32 || fp_mode64)
         d->nFxState = 3;
      else
         d->nFxState = 5;

      vex_bzero(&d->fxState, sizeof(d->fxState));

      d->fxState[0].fx     = Ifx_Read;  /* read */

      if (mode64)
         d->fxState[0].offset = offsetof(VexGuestMIPS64State, guest_FCSR);
      else
         d->fxState[0].offset = offsetof(VexGuestMIPS32State, guest_FCSR);

      d->fxState[0].size   = sizeof(UInt);
      d->fxState[1].fx     = Ifx_Read;  /* read */
      d->fxState[1].offset = floatGuestRegOffset(fs);
      d->fxState[1].size   = sizeof(ULong);
      d->fxState[2].fx     = Ifx_Read;  /* read */
      d->fxState[2].offset = floatGuestRegOffset(ft);
      d->fxState[2].size   = sizeof(ULong);

      if (!(sz32 || fp_mode64)) {
         d->fxState[3].fx     = Ifx_Read;  /* read */
         d->fxState[3].offset = floatGuestRegOffset(fs + 1);
         d->fxState[3].size   = sizeof(ULong);
         d->fxState[4].fx     = Ifx_Read;  /* read */
         d->fxState[4].offset = floatGuestRegOffset(ft + 1);
         d->fxState[4].size   = sizeof(ULong);
      }
   }

   stmt(IRStmt_Dirty(d));

   putFCSR(mkexpr(fcsr));
}

/* ws, wt - source MSA register numbers.
   inst   - MSA fp instruction that needs to be executed.
   opN    - number of operads:
             1 - unary operation.
             2 - binary operation. */
static void calculateMSACSR(UInt ws, UInt wt, UInt inst, UInt opN)
{
   IRDirty *d;
   IRTemp msacsr = newTemp(Ity_I32);
   /* IRExpr_BBPTR() => Need to pass pointer to guest state to helper. */
   d = unsafeIRDirty_1_N(msacsr, 0,
                         "mips_dirtyhelper_calculate_MSACSR",
                         &mips_dirtyhelper_calculate_MSACSR,
                         mkIRExprVec_4(IRExpr_GSPTR(),
                                       mkU32(ws),
                                       mkU32(wt),
                                       mkU32(inst)));

   if (opN == 1) {  /* Unary operation. */
      /* Declare we're reading guest state. */
      d->nFxState = 2;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;  /* read */

      if (mode64)
         d->fxState[0].offset = offsetof(VexGuestMIPS64State, guest_MSACSR);
      else
         d->fxState[0].offset = offsetof(VexGuestMIPS32State, guest_MSACSR);

      d->fxState[0].size   = sizeof(UInt);
      d->fxState[1].fx     = Ifx_Read;  /* read */
      d->fxState[1].offset = msaGuestRegOffset(ws);
      d->fxState[1].size   = sizeof(ULong);
   } else if (opN == 2) {  /* Binary operation. */
      /* Declare we're reading guest state. */
      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;  /* read */

      if (mode64)
         d->fxState[0].offset = offsetof(VexGuestMIPS64State, guest_MSACSR);
      else
         d->fxState[0].offset = offsetof(VexGuestMIPS32State, guest_MSACSR);

      d->fxState[0].size   = sizeof(UInt);
      d->fxState[1].fx     = Ifx_Read;  /* read */
      d->fxState[1].offset = msaGuestRegOffset(ws);
      d->fxState[1].size   = sizeof(ULong);
      d->fxState[2].fx     = Ifx_Read;  /* read */
      d->fxState[2].offset = msaGuestRegOffset(wt);
      d->fxState[2].size   = sizeof(ULong);
   }

   stmt(IRStmt_Dirty(d));
   putMSACSR(mkexpr(msacsr));
}

static IRExpr *getULR(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_ULR), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_ULR), Ity_I32);
}

void putIReg(UInt archreg, IRExpr * e)
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == ty);

   if (archreg != 0)
      stmt(IRStmt_Put(integerGuestRegOffset(archreg), e));
}

static void putWReg(UInt wregNo, IRExpr * e)
{
   vassert(wregNo <= 31);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_V128);
   stmt(IRStmt_Put(msaGuestRegOffset(wregNo), e));
   stmt(IRStmt_Put(floatGuestRegOffset(wregNo),
                   unop(Iop_ReinterpI64asF64, unop(Iop_V128to64, e))));
}

IRExpr *mkNarrowTo32(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to32, src) : src;
}

void putLO(IRExpr * e)
{
   if (mode64) {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_LO), e));
   } else {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LO), e));
      /* Add value to lower 32 bits of ac0 to maintain compatibility between
         regular MIPS32 instruction set and MIPS DSP ASE. Keep higher 32bits
         unchanged. */
      IRTemp t_lo = newTemp(Ity_I32);
      IRTemp t_hi = newTemp(Ity_I32);
      assign(t_lo, e);
      assign(t_hi, unop(Iop_64HIto32, getAcc(0)));
      stmt(IRStmt_Put(accumulatorGuestRegOffset(0),
                      binop(Iop_32HLto64, mkexpr(t_hi), mkexpr(t_lo))));
   }
}

void putHI(IRExpr * e)
{
   if (mode64) {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_HI), e));
   } else {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_HI), e));
      /* Add value to higher 32 bits of ac0 to maintain compatibility between
         regular MIPS32 instruction set and MIPS DSP ASE. Keep lower 32bits
         unchanged. */
      IRTemp t_lo = newTemp(Ity_I32);
      IRTemp t_hi = newTemp(Ity_I32);
      assign(t_hi, e);
      assign(t_lo, unop(Iop_64to32, getAcc(0)));
      stmt(IRStmt_Put(accumulatorGuestRegOffset(0),
                      binop(Iop_32HLto64, mkexpr(t_hi), mkexpr(t_lo))));
   }
}

static IRExpr *mkNarrowTo8 ( IRType ty, IRExpr * src )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to8, src) : unop(Iop_32to8, src);
}

static IRExpr *mkNarrowTo16 ( IRType ty, IRExpr * src )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to16, src) : unop(Iop_32to16, src);
}

static void putPC(IRExpr * e)
{
   stmt(IRStmt_Put(OFFB_PC, e));
}

static IRExpr *mkWidenFrom32(IRType ty, IRExpr * src, Bool sined)
{
   vassert(ty == Ity_I32 || ty == Ity_I64);

   if (ty == Ity_I32)
      return src;

   return (sined) ? unop(Iop_32Sto64, src) : unop(Iop_32Uto64, src);
}

/* Narrow 8/16/32 bit int expr to 8/16/32.  Clearly only some
   of these combinations make sense. */
static IRExpr *narrowTo(IRType dst_ty, IRExpr * e)
{
   IRType src_ty = typeOfIRExpr(irsb->tyenv, e);

   if (src_ty == dst_ty)
      return e;

   if (src_ty == Ity_I32 && dst_ty == Ity_I16)
      return unop(Iop_32to16, e);

   if (src_ty == Ity_I32 && dst_ty == Ity_I8)
      return unop(Iop_32to8, e);

   if (src_ty == Ity_I64 && dst_ty == Ity_I8) {
      vassert(mode64);
      return unop(Iop_64to8, e);
   }

   if (src_ty == Ity_I64 && dst_ty == Ity_I16) {
      vassert(mode64);
      return unop(Iop_64to16, e);
   }

   vpanic("narrowTo(mips)");
   return 0;
}

static IRExpr *getLoFromF64(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_F32 || ty == Ity_F64);

   if (ty == Ity_F64) {
      IRTemp t0, t1;
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I32);
      assign(t0, unop(Iop_ReinterpF64asI64, src));
      assign(t1, unop(Iop_64to32, mkexpr(t0)));
      return unop(Iop_ReinterpI32asF32, mkexpr(t1));
   } else
      return src;
}

static inline IRExpr *getHiFromF64(IRExpr * src)
{
   vassert(typeOfIRExpr(irsb->tyenv, src) == Ity_F64);
   return unop(Iop_ReinterpI32asF32, unop(Iop_64HIto32,
                                          unop(Iop_ReinterpF64asI64, src)));
}

static IRExpr *mkWidenFromF32(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_F32 || ty == Ity_F64);

   if (ty == Ity_F64) {
      IRTemp t0 = newTemp(Ity_I32);
      IRTemp t1 = newTemp(Ity_I64);
      assign(t0, unop(Iop_ReinterpF32asI32, src));
      assign(t1, binop(Iop_32HLto64, mkU32(0x0), mkexpr(t0)));
      return unop(Iop_ReinterpI64asF64, mkexpr(t1));
   } else
      return src;
}

/* Convenience function to move to next instruction on condition. */
static void mips_next_insn_if(IRExpr *condition)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   stmt(IRStmt_Exit(condition, Ijk_Boring,
                    mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                    IRConst_U32(guest_PC_curr_instr + 4),
                    OFFB_PC));
}

static IRExpr *dis_branch_likely(IRExpr * guard, UInt imm)
{
   ULong branch_offset;
   IRTemp t0;

   /* PC = PC + (SignExtend(signed_immed_24) << 2)
      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits)
      is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form
      a PC-relative effective target address. */
   if (mode64)
      branch_offset = extend_s_18to64(imm << 2);
   else
      branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);

   if (mode64)
      stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                       IRConst_U64(guest_PC_curr_instr + 8), OFFB_PC));
   else
      stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                       IRConst_U32(guest_PC_curr_instr + 8), OFFB_PC));

   irsb->jumpkind = Ijk_Boring;

   if (mode64)
      return mkU64(guest_PC_curr_instr + 4 + branch_offset);
   else
      return mkU32(guest_PC_curr_instr + 4 + branch_offset);
}

static void dis_branch(Bool link, IRExpr * guard, UInt imm, IRStmt ** set)
{
   ULong branch_offset;
   IRTemp t0;

   if (link) {  /* LR (GPR31) = addr of the 2nd instr after branch instr */
      if (mode64)
         putIReg(31, mkU64(guest_PC_curr_instr + 8));
      else
         putIReg(31, mkU32(guest_PC_curr_instr + 8));
   }

   /* PC = PC + (SignExtend(signed_immed_24) << 2)
      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits)
      is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form
      a PC-relative effective target address. */

   if (mode64)
      branch_offset = extend_s_18to64(imm << 2);
   else
      branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);

   if (mode64)
      *set = IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                         IRConst_U64(guest_PC_curr_instr + 4 + branch_offset),
                         OFFB_PC);
   else
      *set = IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                         IRConst_U32(guest_PC_curr_instr + 4 +
                                     (UInt) branch_offset), OFFB_PC);
}

static void dis_branch_compact(Bool link, IRExpr * guard, UInt imm,
                               DisResult *dres)
{
   ULong branch_offset;
   IRTemp t0;

   if (link) {  /* LR (GPR31) = addr of the instr after branch instr */
      if (mode64)
         putIReg(31, mkU64(guest_PC_curr_instr + 4));
      else
         putIReg(31, mkU32(guest_PC_curr_instr + 4));

      dres->jk_StopHere = Ijk_Call;
   } else {
      dres->jk_StopHere = Ijk_Boring;
   }

   dres->whatNext = Dis_StopHere;

   /* PC = PC + (SignExtend(signed_immed_24) << 2)
      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits)
      is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form
      a PC-relative effective target address. */

   if (mode64)
      branch_offset = extend_s_18to64(imm << 2);
   else
      branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);

   if (mode64) {
      stmt(IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                       IRConst_U64(guest_PC_curr_instr + 4 + branch_offset),
                       OFFB_PC));
      putPC(mkU64(guest_PC_curr_instr + 4));
   } else {
      stmt(IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                       IRConst_U32(guest_PC_curr_instr + 4 +
                                   (UInt) branch_offset), OFFB_PC));
      putPC(mkU32(guest_PC_curr_instr + 4));
   }
}

static IRExpr *getFReg(UInt fregNo)
{
   vassert(fregNo < 32);
   IRType ty = fp_mode64 ? Ity_F64 : Ity_F32;
   return IRExpr_Get(floatGuestRegOffset(fregNo), ty);
}

static IRExpr *getDReg(UInt dregNo)
{
   vassert(dregNo < 32);

   if (fp_mode64) {
      return IRExpr_Get(floatGuestRegOffset(dregNo), Ity_F64);
   } else {
      /* Read a floating point register pair and combine their contents into a
         64-bit value */
      IRTemp t0 = newTemp(Ity_F32);
      IRTemp t1 = newTemp(Ity_F32);
      IRTemp t2 = newTemp(Ity_F64);
      IRTemp t3 = newTemp(Ity_I32);
      IRTemp t4 = newTemp(Ity_I32);
      IRTemp t5 = newTemp(Ity_I64);

      assign(t0, getFReg(dregNo & (~1)));
      assign(t1, getFReg(dregNo | 1));

      assign(t3, unop(Iop_ReinterpF32asI32, mkexpr(t0)));
      assign(t4, unop(Iop_ReinterpF32asI32, mkexpr(t1)));
      assign(t5, binop(Iop_32HLto64, mkexpr(t4), mkexpr(t3)));
      assign(t2, unop(Iop_ReinterpI64asF64, mkexpr(t5)));

      return mkexpr(t2);
   }
}

static void putFReg(UInt dregNo, IRExpr * e)
{
   vassert(dregNo < 32);
   IRType ty = fp_mode64 ? Ity_F64 : Ity_F32;
   vassert(typeOfIRExpr(irsb->tyenv, e) == ty);

   if (fp_mode64_fre) {
      IRTemp t0 = newTemp(Ity_F32);
      assign(t0, getLoFromF64(ty, e));
#if defined (_MIPSEL)
      stmt(IRStmt_Put(floatGuestRegOffset(dregNo), mkexpr(t0)));

      if (dregNo & 1)
         stmt(IRStmt_Put(floatGuestRegOffset(dregNo) - 4, mkexpr(t0)));

#else
      stmt(IRStmt_Put(floatGuestRegOffset(dregNo) + 4, mkexpr(t0)));

      if (dregNo & 1)
         stmt(IRStmt_Put(floatGuestRegOffset(dregNo & (~1)), mkexpr(t0)));

#endif
   } else {
      stmt(IRStmt_Put(floatGuestRegOffset(dregNo), e));
   }

   if (has_msa && fp_mode64) {
      stmt(IRStmt_Put(msaGuestRegOffset(dregNo),
                      binop(Iop_64HLtoV128,
                            unop(Iop_ReinterpF64asI64, e),
                            unop(Iop_ReinterpF64asI64, e))));
   }
}

static void putDReg(UInt dregNo, IRExpr * e)
{
   if (fp_mode64) {
      vassert(dregNo < 32);
      IRType ty = Ity_F64;
      vassert(typeOfIRExpr(irsb->tyenv, e) == ty);
      stmt(IRStmt_Put(floatGuestRegOffset(dregNo), e));

      if (fp_mode64_fre) {
         IRTemp t0 = newTemp(Ity_F32);

         if (dregNo & 1) {
            assign(t0, getLoFromF64(ty, e));
#if defined (_MIPSEL)
            stmt(IRStmt_Put(floatGuestRegOffset(dregNo) - 4, mkexpr(t0)));
#else
            stmt(IRStmt_Put(floatGuestRegOffset(dregNo & (~1)), mkexpr(t0)));
#endif
         } else {
            assign(t0, getHiFromF64(e));
#if defined (_MIPSEL)
            stmt(IRStmt_Put(floatGuestRegOffset(dregNo | 1), mkexpr(t0)));
#else
            stmt(IRStmt_Put(floatGuestRegOffset(dregNo | 1) + 4, mkexpr(t0)));
#endif
         }
      }

      if (has_msa)
         stmt(IRStmt_Put(msaGuestRegOffset(dregNo),
                         binop(Iop_64HLtoV128,
                               unop(Iop_ReinterpF64asI64, e),
                               unop(Iop_ReinterpF64asI64, e))));
   } else {
      vassert(dregNo < 32);
      vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F64);
      IRTemp t1 = newTemp(Ity_F64);
      IRTemp t4 = newTemp(Ity_I32);
      IRTemp t5 = newTemp(Ity_I32);
      IRTemp t6 = newTemp(Ity_I64);
      assign(t1, e);
      assign(t6, unop(Iop_ReinterpF64asI64, mkexpr(t1)));
      assign(t4, unop(Iop_64HIto32, mkexpr(t6)));  /* hi */
      assign(t5, unop(Iop_64to32, mkexpr(t6)));    /* lo */
      putFReg(dregNo & (~1), unop(Iop_ReinterpI32asF32, mkexpr(t5)));
      putFReg(dregNo | 1, unop(Iop_ReinterpI32asF32, mkexpr(t4)));
   }
}

static void setFPUCondCode(IRExpr * e, UInt cc)
{
   if (cc == 0) {
      putFCSR(binop(Iop_And32, getFCSR(), mkU32(0xFF7FFFFF)));
      putFCSR(binop(Iop_Or32, getFCSR(), binop(Iop_Shl32, e, mkU8(23))));
   } else {
      putFCSR(binop(Iop_And32, getFCSR(), unop(Iop_Not32,
                    binop(Iop_Shl32, mkU32(0x01000000), mkU8(cc)))));
      putFCSR(binop(Iop_Or32, getFCSR(), binop(Iop_Shl32, e, mkU8(24 + cc))));
   }
}

static IRExpr* get_IR_roundingmode ( void )
{
   /*
      rounding mode | MIPS | IR
      ------------------------
      to nearest    | 00  | 00
      to zero       | 01  | 11
      to +infinity  | 10  | 10
      to -infinity  | 11  | 01
   */
   IRTemp rm_MIPS = newTemp(Ity_I32);
   /* Last two bits in FCSR are rounding mode. */

   if (mode64)
      assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS64State,
                            guest_FCSR), Ity_I32), mkU32(3)));
   else
      assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS32State,
                            guest_FCSR), Ity_I32), mkU32(3)));

   /* rm_IR = XOR( rm_MIPS32, (rm_MIPS32 << 1) & 2) */

   return binop(Iop_Xor32, mkexpr(rm_MIPS), binop(Iop_And32,
                binop(Iop_Shl32, mkexpr(rm_MIPS), mkU8(1)), mkU32(2)));
}

static IRExpr* get_IR_roundingmode_MSA ( void )
{
   /*
      rounding mode | MIPS | IR
      ------------------------
      to nearest    | 00  | 00
      to zero       | 01  | 11
      to +infinity  | 10  | 10
      to -infinity  | 11  | 01
   */
   IRTemp rm_MIPS = newTemp(Ity_I32);
   /* Last two bits in MSACSR are rounding mode. */

   if (mode64)
      assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS64State,
                            guest_MSACSR), Ity_I32), mkU32(3)));
   else
      assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS32State,
                            guest_MSACSR), Ity_I32), mkU32(3)));

   /* rm_IR = XOR( rm_MIPS32, (rm_MIPS32 << 1) & 2) */
   return binop(Iop_Xor32, mkexpr(rm_MIPS), binop(Iop_And32,
                binop(Iop_Shl32, mkexpr(rm_MIPS), mkU8(1)), mkU32(2)));
}

/* sz, ULong -> IRExpr */
static IRExpr *mkSzImm ( IRType ty, ULong imm64 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? mkU64(imm64) : mkU32((UInt) imm64);
}

static IRConst *mkSzConst ( IRType ty, ULong imm64 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return (ty == Ity_I64 ? IRConst_U64(imm64) : IRConst_U32((UInt) imm64));
}

/* Make sure we get valid 32 and 64bit addresses */
static Addr64 mkSzAddr ( IRType ty, Addr64 addr )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return (ty == Ity_I64 ? (Addr64) addr :
           (Addr64) extend_s_32to64(toUInt(addr)));
}

/* Shift and Rotate instructions for MIPS64 */
static Bool dis_instr_shrt ( UInt theInstr )
{
   UInt opc2 = get_function(theInstr);
   UChar regRs = get_rs(theInstr);
   UChar regRt = get_rt(theInstr);
   UChar regRd = get_rd(theInstr);
   UChar uImmsa = get_sa(theInstr);
   Long sImmsa = extend_s_16to64(uImmsa);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRTemp tmp = newTemp(ty);
   IRTemp tmpOr = newTemp(ty);
   IRTemp tmpRt = newTemp(ty);
   IRTemp tmpRs = newTemp(ty);
   IRTemp tmpRd = newTemp(ty);

   assign(tmpRs, getIReg(regRs));
   assign(tmpRt, getIReg(regRt));

   switch (opc2) {
      case 0x3A:
         if ((regRs & 0x01) == 0) {
            /* Doubleword Shift Right Logical - DSRL; MIPS64 */
            DIP("dsrl r%u, r%u, %lld", regRd, regRt, sImmsa);
            assign(tmpRd, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa)));
            putIReg(regRd, mkexpr(tmpRd));
         } else if ((regRs & 0x01) == 1) {
            /* Doubleword Rotate Right - DROTR; MIPS64r2 */
            vassert(mode64);
            DIP("drotr r%u, r%u, %lld", regRd, regRt, sImmsa);
            IRTemp tmpL = newTemp(ty);
            IRTemp tmpR = newTemp(ty);
            assign(tmpR, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa)));
            assign(tmp, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(63 - uImmsa)));
            assign(tmpL, binop(Iop_Shl64, mkexpr(tmp), mkU8(1)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpL), mkexpr(tmpR)));
            putIReg(regRd, mkexpr(tmpRd));
         } else
            return False;

         break;

      case 0x3E:
         if ((regRs & 0x01) == 0) {
            /* Doubleword Shift Right Logical Plus 32 - DSRL32; MIPS64 */
            DIP("dsrl32 r%u, r%u, %lld", regRd, regRt, sImmsa + 32);
            assign(tmpRd, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
            putIReg(regRd, mkexpr(tmpRd));
         } else if ((regRs & 0x01) == 1) {
            /* Doubleword Rotate Right Plus 32 - DROTR32; MIPS64r2 */
            DIP("drotr32 r%u, r%u, %lld", regRd, regRt, sImmsa);
            vassert(mode64);
            IRTemp tmpL = newTemp(ty);
            IRTemp tmpR = newTemp(ty);
            /* (tmpRt >> sa) | (tmpRt << (64 - sa)) */
            assign(tmpR, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
            assign(tmp, binop(Iop_Shl64, mkexpr(tmpRt),
                              mkU8(63 - (uImmsa + 32))));
            assign(tmpL, binop(Iop_Shl64, mkexpr(tmp), mkU8(1)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpL), mkexpr(tmpR)));
            putIReg(regRd, mkexpr(tmpRd));
         } else
            return False;

         break;

      case 0x16:
         if ((uImmsa & 0x01) == 0) {
            /* Doubleword Shift Right Logical Variable - DSRLV; MIPS64 */
            DIP("dsrlv r%u, r%u, r%u", regRd, regRt, regRs);
            IRTemp tmpRs8 = newTemp(Ity_I8);
            /* s = tmpRs[5..0] */
            assign(tmp, binop(Iop_And64, mkexpr(tmpRs), mkU64(63)));
            assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
            assign(tmpRd, binop(Iop_Shr64, mkexpr(tmpRt), mkexpr(tmpRs8)));
            putIReg(regRd, mkexpr(tmpRd));
         } else if ((uImmsa & 0x01) == 1) {
            /* Doubleword Rotate Right Variable - DROTRV; MIPS64r2 */
            DIP("drotrv r%u, r%u, r%u", regRd, regRt, regRs);
            IRTemp tmpL = newTemp(ty);
            IRTemp tmpR = newTemp(ty);
            IRTemp tmpRs8 = newTemp(Ity_I8);
            IRTemp tmpLs8 = newTemp(Ity_I8);
            IRTemp tmp64 = newTemp(ty);
            /* s = tmpRs[5...0]
               m = 64 - s
               (tmpRt << s) | (tmpRt >> m) */

            assign(tmp64, binop(Iop_And64, mkexpr(tmpRs), mkSzImm(ty, 63)));
            assign(tmp, binop(Iop_Sub64, mkU64(63), mkexpr(tmp64)));

            assign(tmpLs8, mkNarrowTo8(ty, mkexpr(tmp)));
            assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp64)));

            assign(tmpR, binop(Iop_Shr64, mkexpr(tmpRt), mkexpr(tmpRs8)));
            assign(tmpL, binop(Iop_Shl64, mkexpr(tmpRt), mkexpr(tmpLs8)));
            assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpL), mkU8(1)));
            assign(tmpOr, binop(Iop_Or64, mkexpr(tmpRd), mkexpr(tmpR)));

            putIReg(regRd, mkexpr(tmpOr));
         } else
            return False;

         break;

      case 0x38:  /* Doubleword Shift Left Logical - DSLL; MIPS64 */
         DIP("dsll r%u, r%u, %lld", regRd, regRt, sImmsa);
         vassert(mode64);
         assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(uImmsa)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x3C:  /* Doubleword Shift Left Logical Plus 32 - DSLL32; MIPS64 */
         DIP("dsll32 r%u, r%u, %lld", regRd, regRt, sImmsa);
         assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x14: {  /* Doubleword Shift Left Logical Variable - DSLLV; MIPS64 */
         DIP("dsllv r%u, r%u, r%u", regRd, regRt, regRs);
         IRTemp tmpRs8 = newTemp(Ity_I8);

         assign(tmp, binop(Iop_And64, mkexpr(tmpRs), mkSzImm(ty, 63)));
         assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
         assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpRt), mkexpr(tmpRs8)));
         putIReg(regRd, mkexpr(tmpRd));
         break;
      }

      case 0x3B:  /* Doubleword Shift Right Arithmetic - DSRA; MIPS64 */
         DIP("dsra r%u, r%u, %lld", regRd, regRt, sImmsa);
         assign(tmpRd, binop(Iop_Sar64, mkexpr(tmpRt), mkU8(uImmsa)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x3F:  /* Doubleword Shift Right Arithmetic Plus 32 - DSRA32;
                     MIPS64 */
         DIP("dsra32 r%u, r%u, %lld", regRd, regRt, sImmsa);
         assign(tmpRd, binop(Iop_Sar64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x17: {
         /* Doubleword Shift Right Arithmetic Variable - DSRAV;
                          MIPS64 */
         DIP("dsrav r%u, r%u, r%u", regRd, regRt, regRs);
         IRTemp tmpRs8 = newTemp(Ity_I8);
         assign(tmp, binop(Iop_And64, mkexpr(tmpRs), mkSzImm(ty, 63)));
         assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
         assign(tmpRd, binop(Iop_Sar64, mkexpr(tmpRt), mkexpr(tmpRs8)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      }

      default:
         return False;

   }

   return True;
}

static IROp mkSzOp ( IRType ty, IROp op8 )
{
   Int adj;
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I64);
   vassert(op8 == Iop_Add8 || op8 == Iop_Sub8 || op8 == Iop_Mul8
           || op8 == Iop_Or8 || op8 == Iop_And8 || op8 == Iop_Xor8
           || op8 == Iop_Shl8 || op8 == Iop_Shr8 || op8 == Iop_Sar8
           || op8 == Iop_CmpEQ8 || op8 == Iop_CmpNE8 || op8 == Iop_Not8);
   adj = ty == Ity_I8 ? 0 : (ty == Ity_I16 ? 1 : (ty == Ity_I32 ? 2 : 3));
   return adj + op8;
}

/*********************************************************/
/*---             Floating Point Compare              ---*/
/*********************************************************/
/* Function that returns a string that represent mips cond
   mnemonic for the input code. */
static const HChar* showCondCode(UInt code)
{
   const HChar* ret;

   switch (code) {
      case 0:
         ret = "f";
         break;

      case 1:
         ret = "un";
         break;

      case 2:
         ret = "eq";
         break;

      case 3:
         ret = "ueq";
         break;

      case 4:
         ret = "olt";
         break;

      case 5:
         ret = "ult";
         break;

      case 6:
         ret = "ole";
         break;

      case 7:
         ret = "ule";
         break;

      case 8:
         ret = "sf";
         break;

      case 9:
         ret = "ngle";
         break;

      case 10:
         ret = "seq";
         break;

      case 11:
         ret = "ngl";
         break;

      case 12:
         ret = "lt";
         break;

      case 13:
         ret = "nge";
         break;

      case 14:
         ret = "le";
         break;

      case 15:
         ret = "ngt";
         break;

      default:
         vpanic("showCondCode");
         break;
   }

   return ret;
}

static Bool dis_instr_CCondFmt ( UInt cins )
{
   IRTemp t0, t1, t2, t3, tmp5, tmp6;
   IRTemp ccIR = newTemp(Ity_I32);
   IRTemp ccMIPS = newTemp(Ity_I32);
   UInt FC = get_FC(cins);
   UInt fmt = get_fmt(cins);
   UInt fs = get_fs(cins);
   UInt ft = get_ft(cins);
   UInt cond = get_cond(cins);

   if (FC == 0x3) {  /* C.cond.fmt */
      UInt fpc_cc = get_fpc_cc(cins);

      switch (fmt) {
         case 0x10: {  /* C.cond.S */
            DIP("c.%s.s %u, f%u, f%u", showCondCode(cond), fpc_cc, fs, ft);

            if (fp_mode64) {
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);

               tmp5 = newTemp(Ity_F64);
               tmp6 = newTemp(Ity_F64);

               assign(tmp5, unop(Iop_F32toF64, getLoFromF64(Ity_F64,
                                 getFReg(fs))));
               assign(tmp6, unop(Iop_F32toF64, getLoFromF64(Ity_F64,
                                 getFReg(ft))));

               assign(ccIR, binop(Iop_CmpF64, mkexpr(tmp5), mkexpr(tmp6)));
               putHI(mkWidenFrom32(mode64 ? Ity_I64 : Ity_I32,
                                   mkexpr(ccIR), True));
               /* Map compare result from IR to MIPS
                  FP cmp result | MIPS | IR
                  --------------------------
                  UN            | 0x1 | 0x45
                  EQ            | 0x2 | 0x40
                  GT            | 0x4 | 0x00
                  LT            | 0x8 | 0x01
                */

               /* ccMIPS = Shl(1, (~(ccIR>>5) & 2) | ((ccIR ^ (ccIR>>6)) & 1) */
               assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                                    binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                                          binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))), mkU32(2)),
                                          binop(Iop_And32, binop(Iop_Xor32, mkexpr(ccIR),
                                                binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                                                mkU32(1))))));
               putLO(mkWidenFrom32(mode64 ? Ity_I64 : Ity_I32,
                                   mkexpr(ccMIPS), True));

               /* UN */
               assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));
               /* EQ */
               assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                                 mkU8(0x1)), mkU32(0x1)));
               /* NGT */
               assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                                                mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));
               /* LT */
               assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                                 mkU8(0x3)), mkU32(0x1)));

               switch (cond) {
                  case 0x0:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;

                  case 0x1:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;

                  case 0x2:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;

                  case 0x3:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0x4:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;

                  case 0x5:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                    fpc_cc);
                     break;

                  case 0x6:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0x7:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;

                  case 0x8:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;

                  case 0x9:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;

                  case 0xA:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;

                  case 0xB:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0xC:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;

                  case 0xD:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                    fpc_cc);
                     break;

                  case 0xE:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0xF:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;

                  default:
                     return False;
               }

            } else {
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);

               assign(ccIR, binop(Iop_CmpF64, unop(Iop_F32toF64, getFReg(fs)),
                                  unop(Iop_F32toF64, getFReg(ft))));
               /* Map compare result from IR to MIPS
                  FP cmp result | MIPS | IR
                  --------------------------
                  UN            | 0x1 | 0x45
                  EQ            | 0x2 | 0x40
                  GT            | 0x4 | 0x00
                  LT            | 0x8 | 0x01
                */

               /* ccMIPS = Shl(1, (~(ccIR>>5) & 2) | ((ccIR ^ (ccIR>>6)) & 1) */
               assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                                    binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                                          binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))),
                                          mkU32(2)), binop(Iop_And32,
                                                binop(Iop_Xor32, mkexpr(ccIR),
                                                      binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                                                mkU32(1))))));
               /* UN */
               assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));
               /* EQ */
               assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                                 mkU8(0x1)), mkU32(0x1)));
               /* NGT */
               assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                                                mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));
               /* LT */
               assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                                 mkU8(0x3)), mkU32(0x1)));

               switch (cond) {
                  case 0x0:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;

                  case 0x1:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;

                  case 0x2:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;

                  case 0x3:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0x4:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;

                  case 0x5:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                    fpc_cc);
                     break;

                  case 0x6:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0x7:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;

                  case 0x8:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;

                  case 0x9:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;

                  case 0xA:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;

                  case 0xB:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0xC:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;

                  case 0xD:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                    fpc_cc);
                     break;

                  case 0xE:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                    fpc_cc);
                     break;

                  case 0xF:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;

                  default:
                     return False;
               }
            }
         }
         break;

         case 0x11: {  /* C.cond.D */
            DIP("c.%s.d %u, f%u, f%u", showCondCode(cond), fpc_cc, fs, ft);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I32);
            assign(ccIR, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
            /* Map compare result from IR to MIPS
               FP cmp result | MIPS | IR
               --------------------------
               UN            | 0x1 | 0x45
               EQ            | 0x2 | 0x40
               GT            | 0x4 | 0x00
               LT            | 0x8 | 0x01
             */

            /* ccMIPS = Shl(1, (~(ccIR>>5) & 2) | ((ccIR ^ (ccIR>>6)) & 1) */
            assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                                 binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                                       binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))), mkU32(2)),
                                       binop(Iop_And32, binop(Iop_Xor32, mkexpr(ccIR),
                                             binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                                             mkU32(1))))));

            /* UN */
            assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));
            /* EQ */
            assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                              mkU8(0x1)), mkU32(0x1)));
            /* NGT */
            assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                                             mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));
            /* LT */
            assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                              mkU8(0x3)), mkU32(0x1)));

            switch (cond) {
               case 0x0:
                  setFPUCondCode(mkU32(0), fpc_cc);
                  break;

               case 0x1:
                  setFPUCondCode(mkexpr(t0), fpc_cc);
                  break;

               case 0x2:
                  setFPUCondCode(mkexpr(t1), fpc_cc);
                  break;

               case 0x3:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                 fpc_cc);
                  break;

               case 0x4:
                  setFPUCondCode(mkexpr(t3), fpc_cc);
                  break;

               case 0x5:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                 fpc_cc);
                  break;

               case 0x6:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                 fpc_cc);
                  break;

               case 0x7:
                  setFPUCondCode(mkexpr(t2), fpc_cc);
                  break;

               case 0x8:
                  setFPUCondCode(mkU32(0), fpc_cc);
                  break;

               case 0x9:
                  setFPUCondCode(mkexpr(t0), fpc_cc);
                  break;

               case 0xA:
                  setFPUCondCode(mkexpr(t1), fpc_cc);
                  break;

               case 0xB:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                 fpc_cc);
                  break;

               case 0xC:
                  setFPUCondCode(mkexpr(t3), fpc_cc);
                  break;

               case 0xD:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                 fpc_cc);
                  break;

               case 0xE:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                 fpc_cc);
                  break;

               case 0xF:
                  setFPUCondCode(mkexpr(t2), fpc_cc);
                  break;

               default:
                  return False;
            }
         }
         break;

         default:
            return False;
      }
   } else {
      return False;
   }

   return True;
}

/*********************************************************/
/*---        Branch Instructions for mips64           ---*/
/*********************************************************/
static Bool dis_instr_branch ( UInt theInstr, DisResult * dres, IRStmt ** set )
{
   UInt jmpKind = 0;
   UChar opc1 = get_opcode(theInstr);
   UChar regRs = get_rs(theInstr);
   UChar regRt = get_rt(theInstr);
   UInt offset = get_imm(theInstr);
   Long sOffset = extend_s_16to64(offset);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IROp opSlt = mode64 ? Iop_CmpLT64S : Iop_CmpLT32S;

   IRTemp tmp = newTemp(ty);
   IRTemp tmpRs = newTemp(ty);
   IRTemp tmpRt = newTemp(ty);
   IRTemp tmpLt = newTemp(ty);
   IRTemp tmpReg0 = newTemp(ty);

   UChar regLnk = 31;   /* reg 31 is link reg in MIPS */
   Addr64 addrTgt = 0;
   Addr64 cia = guest_PC_curr_instr;

   IRExpr *eConst0 = mkSzImm(ty, (UInt) 0);
   IRExpr *eNia = mkSzImm(ty, cia + 8);
   IRExpr *eCond = NULL;

   assign(tmpRs, getIReg(regRs));
   assign(tmpRt, getIReg(regRt));
   assign(tmpReg0, getIReg(0));

   eCond = binop(mkSzOp(ty, Iop_CmpNE8), mkexpr(tmpReg0), mkexpr(tmpReg0));

   switch (opc1) {
      case 0x01:
         switch (regRt) {
            case 0x00: {  /* BLTZ rs, offset */
               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));
               IRTemp tmpLtRes = newTemp(Ity_I1);

               assign(tmp, eConst0);
               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), mkexpr(tmp)));
               assign(tmpLt, mode64 ? unop(Iop_1Uto64, mkexpr(tmpLtRes)) :
                      unop(Iop_1Uto32, mkexpr(tmpLtRes)));

               eCond = binop(mkSzOp(ty, Iop_CmpNE8), mkexpr(tmpLt),
                             mkexpr(tmpReg0));

               jmpKind = Ijk_Boring;
               break;
            }

            case 0x01: {  /* BGEZ rs, offset */
               IRTemp tmpLtRes = newTemp(Ity_I1);
               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));

               assign(tmp, eConst0);
               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), mkexpr(tmp)));
               assign(tmpLt, mode64 ? unop(Iop_1Uto64, mkexpr(tmpLtRes)) :
                      unop(Iop_1Uto32, mkexpr(tmpLtRes)));
               eCond = binop(mkSzOp(ty, Iop_CmpEQ8), mkexpr(tmpLt),
                             mkexpr(tmpReg0));

               jmpKind = Ijk_Boring;
               break;
            }

            case 0x11: {  /* BGEZAL rs, offset */
               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));
               putIReg(regLnk, eNia);
               IRTemp tmpLtRes = newTemp(Ity_I1);

               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), eConst0));
               assign(tmpLt, mode64 ? unop(Iop_1Uto64, mkexpr(tmpLtRes)) :
                      unop(Iop_1Uto32, mkexpr(tmpLtRes)));

               eCond = binop(mkSzOp(ty, Iop_CmpEQ8), mkexpr(tmpLt),
                             mkexpr(tmpReg0));

               jmpKind = Ijk_Call;
               break;
            }

            case 0x10: {  /* BLTZAL rs, offset */
               IRTemp tmpLtRes = newTemp(Ity_I1);
               IRTemp tmpRes = newTemp(ty);

               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));
               putIReg(regLnk, eNia);

               assign(tmp, eConst0);
               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), mkexpr(tmp)));
               assign(tmpRes, mode64 ? unop(Iop_1Uto64,
                                            mkexpr(tmpLtRes)) : unop(Iop_1Uto32, mkexpr(tmpLtRes)));
               eCond = binop(mkSzOp(ty, Iop_CmpNE8), mkexpr(tmpRes),
                             mkexpr(tmpReg0));

               jmpKind = Ijk_Call;
               break;
            }

         }

         break;

      default:
         return False;
   }

   *set = IRStmt_Exit(eCond, jmpKind, mkSzConst(ty, addrTgt), OFFB_PC);
   return True;
}

/*********************************************************/
/*---         Cavium Specific Instructions            ---*/
/*********************************************************/

/* Convenience function to yield to thread scheduler */
static void jump_back(IRExpr *condition)
{
   stmt( IRStmt_Exit(condition,
                     Ijk_Yield,
                     IRConst_U64( guest_PC_curr_instr ),
                     OFFB_PC) );
}

/* Based on s390_irgen_load_and_add32. */
static void mips_load_store32(IRTemp op1addr, IRTemp new_val,
                              IRTemp expd, UChar rd, Bool putIntoRd)
{
   IRCAS *cas;
   IRTemp old_mem = newTemp(Ity_I32);
   IRType ty      = mode64 ? Ity_I64 : Ity_I32;

   cas = mkIRCAS(IRTemp_INVALID, old_mem,
#if defined (_MIPSEL)
                 Iend_LE, mkexpr(op1addr),
#else /* _MIPSEB */
                 Iend_BE, mkexpr(op1addr),
#endif
                 NULL, mkexpr(expd), /* expected value */
                 NULL, mkexpr(new_val)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* If old_mem contains the expected value, then the CAS succeeded.
      Otherwise, it did not */
   jump_back(binop(Iop_CmpNE32, mkexpr(old_mem), mkexpr(expd)));

   if (putIntoRd)
      putIReg(rd, mkWidenFrom32(ty, mkexpr(old_mem), True));
}

/* Based on s390_irgen_load_and_add64. */
static void mips_load_store64(IRTemp op1addr, IRTemp new_val,
                              IRTemp expd, UChar rd, Bool putIntoRd)
{
   IRCAS *cas;
   IRTemp old_mem = newTemp(Ity_I64);
   vassert(mode64);
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
#if defined (_MIPSEL)
                 Iend_LE, mkexpr(op1addr),
#else /* _MIPSEB */
                 Iend_BE, mkexpr(op1addr),
#endif
                 NULL, mkexpr(expd), /* expected value */
                 NULL, mkexpr(new_val)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* If old_mem contains the expected value, then the CAS succeeded.
      Otherwise, it did not */
   jump_back(binop(Iop_CmpNE64, mkexpr(old_mem), mkexpr(expd)));

   if (putIntoRd)
      putIReg(rd, mkexpr(old_mem));
}

static Bool dis_instr_CVM ( UInt theInstr )
{
   UChar  opc2     = get_function(theInstr);
   UChar  opc1     = get_opcode(theInstr);
   UChar  regRs    = get_rs(theInstr);
   UChar  regRt    = get_rt(theInstr);
   UChar  regRd    = get_rd(theInstr);
   /* MIPS trap instructions extract code from theInstr[15:6].
      Cavium OCTEON instructions SNEI, SEQI extract immediate operands
      from the same bit field [15:6]. */
   UInt   imm      = get_code(theInstr);
   UChar  lenM1    = get_msb(theInstr);
   UChar  p        = get_lsb(theInstr);
   IRType ty       = mode64 ? Ity_I64 : Ity_I32;
   IRTemp tmp      = newTemp(ty);
   IRTemp tmpRs    = newTemp(ty);
   IRTemp tmpRt    = newTemp(ty);
   IRTemp t1       = newTemp(ty);
   UInt size;
   assign(tmpRs, getIReg(regRs));

   switch (opc1) {
      case 0x1C: {
         switch (opc2) {
            case 0x03: {  /* DMUL rd, rs, rt */
               DIP("dmul r%u, r%u, r%u", regRd, regRs, regRt);
               IRTemp t0 = newTemp(Ity_I128);
               assign(t0, binop(Iop_MullU64, getIReg(regRs), getIReg(regRt)));
               putIReg(regRd, unop(Iop_128to64, mkexpr(t0)));
               break;
            }

            case 0x18: {  /* Store Atomic Add Word - SAA; Cavium OCTEON */
               DIP("saa r%u, (r%u)", regRt, regRs);
               IRTemp addr = newTemp(Ity_I64);
               IRTemp new_val = newTemp(Ity_I32);
               IRTemp old = newTemp(Ity_I32);
               assign(addr, getIReg(regRs));
               assign(old, load(Ity_I32, mkexpr(addr)));
               assign(new_val, binop(Iop_Add32,
                                     mkexpr(old),
                                     mkNarrowTo32(ty, getIReg(regRt))));
               mips_load_store32(addr, new_val, old, 0, False);
               break;
            }

            /* Store Atomic Add Doubleword - SAAD; Cavium OCTEON */
            case 0x19: {
               DIP( "saad r%u, (r%u)", regRt, regRs);
               IRTemp addr = newTemp(Ity_I64);
               IRTemp new_val = newTemp(Ity_I64);
               IRTemp old = newTemp(Ity_I64);
               assign(addr, getIReg(regRs));
               assign(old, load(Ity_I64, mkexpr(addr)));
               assign(new_val, binop(Iop_Add64,
                                     mkexpr(old),
                                     getIReg(regRt)));
               mips_load_store64(addr, new_val, old, 0, False);
               break;
            }

            /* LAI, LAID, LAD, LADD, LAS, LASD,
               LAC, LACD, LAA, LAAD, LAW, LAWD */
            case 0x1f: {
               UInt opc3 = get_sa(theInstr);
               IRTemp addr = newTemp(Ity_I64);

               switch (opc3) {
                  /* Load Atomic Increment Word - LAI; Cavium OCTEON2 */
                  case 0x02: {
                     DIP("lai r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I32);
                     IRTemp old = newTemp(Ity_I32);
                     assign(addr, getIReg(regRs));
                     assign(old, load(Ity_I32, mkexpr(addr)));
                     assign(new_val, binop(Iop_Add32,
                                           mkexpr(old),
                                           mkU32(1)));
                     mips_load_store32(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Increment Doubleword - LAID; Cavium OCTEON2 */
                  case 0x03: {
                     DIP("laid r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I64);
                     IRTemp old = newTemp(Ity_I64);
                     assign(addr, getIReg(regRs));
                     assign(old, load(Ity_I64, mkexpr(addr)));
                     assign(new_val, binop(Iop_Add64,
                                           mkexpr(old),
                                           mkU64(1)));
                     mips_load_store64(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Decrement Word - LAD; Cavium OCTEON2 */
                  case 0x06: {
                     DIP("lad r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I32);
                     IRTemp old = newTemp(Ity_I32);
                     assign(addr, getIReg(regRs));
                     assign(old, load(Ity_I32, mkexpr(addr)));
                     assign(new_val, binop(Iop_Sub32,
                                           mkexpr(old),
                                           mkU32(1)));
                     mips_load_store32(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Decrement Doubleword - LADD; Cavium OCTEON2 */
                  case 0x07: {
                     DIP("ladd r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I64);
                     IRTemp old = newTemp(Ity_I64);
                     assign(addr, getIReg(regRs));
                     assign(old, load(Ity_I64, mkexpr(addr)));
                     assign(new_val, binop(Iop_Sub64,
                                           mkexpr(old),
                                           mkU64(1)));
                     mips_load_store64(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Set Word - LAS; Cavium OCTEON2 */
                  case 0x0a: {
                     DIP("las r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I32);
                     IRTemp old = newTemp(Ity_I32);
                     assign(addr, getIReg(regRs));
                     assign(new_val, mkU32(0xffffffff));
                     assign(old, load(Ity_I32, mkexpr(addr)));
                     mips_load_store32(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Set Doubleword - LASD; Cavium OCTEON2 */
                  case 0x0b: {
                     DIP("lasd r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I64);
                     IRTemp old = newTemp(Ity_I64);
                     assign(addr, getIReg(regRs));
                     assign(new_val, mkU64(0xffffffffffffffffULL));
                     assign(old, load(Ity_I64, mkexpr(addr)));
                     mips_load_store64(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Clear Word - LAC; Cavium OCTEON2 */
                  case 0x0e: {
                     DIP("lac r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I32);
                     IRTemp old = newTemp(Ity_I32);
                     assign(addr, getIReg(regRs));
                     assign(new_val, mkU32(0));
                     assign(old, load(Ity_I32, mkexpr(addr)));
                     mips_load_store32(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Clear Doubleword - LACD; Cavium OCTEON2 */
                  case 0x0f: {
                     DIP("lacd r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I64);
                     IRTemp old = newTemp(Ity_I64);
                     assign(addr, getIReg(regRs));
                     assign(new_val, mkU64(0));
                     assign(old, load(Ity_I64, mkexpr(addr)));
                     mips_load_store64(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Add Word - LAA; Cavium OCTEON2 */
                  case 0x12: {
                     DIP("laa r%u,(r%u),r%u\n", regRd, regRs, regRt);
                     IRTemp new_val = newTemp(Ity_I32);
                     IRTemp old = newTemp(Ity_I32);
                     assign(addr, getIReg(regRs));
                     assign(old, load(Ity_I32, mkexpr(addr)));
                     assign(new_val, binop(Iop_Add32,
                                           mkexpr(old),
                                           mkNarrowTo32(ty, getIReg(regRt))));
                     mips_load_store32(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Add Doubleword - LAAD; Cavium OCTEON2 */
                  case 0x13: {
                     DIP("laad r%u,(r%u),r%u\n", regRd, regRs, regRt);
                     IRTemp new_val = newTemp(Ity_I64);
                     IRTemp old = newTemp(Ity_I64);
                     assign(addr, getIReg(regRs));
                     assign(old, load(Ity_I64, mkexpr(addr)));
                     assign(new_val, binop(Iop_Add64,
                                           load(Ity_I64, mkexpr(addr)),
                                           getIReg(regRt)));
                     mips_load_store64(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Swap Word - LAW; Cavium OCTEON2 */
                  case 0x16: {
                     DIP("law r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I32);
                     IRTemp old = newTemp(Ity_I32);
                     assign(addr, getIReg(regRs));
                     assign(new_val, mkNarrowTo32(ty, getIReg(regRt)));
                     assign(old, load(Ity_I32, mkexpr(addr)));
                     mips_load_store32(addr, new_val, old, regRd, True);
                     break;
                  }

                  /* Load Atomic Swap Doubleword - LAWD; Cavium OCTEON2 */
                  case 0x17: {
                     DIP("lawd r%u,(r%u)\n", regRd, regRs);
                     IRTemp new_val = newTemp(Ity_I64);
                     IRTemp old = newTemp(Ity_I64);
                     assign(addr, getIReg(regRs));
                     assign(new_val, getIReg(regRt));
                     assign(old, load(Ity_I64, mkexpr(addr)));
                     mips_load_store64(addr, new_val, old, regRd, True);
                     break;
                  }

                  default:
                     vex_printf("Unknown laxx instruction, opc3=0x%x\n", opc3);
                     vex_printf("Instruction=0x%08x\n", theInstr);
                     return False;
               }

               break;
            }

            /* Unsigned Byte Add - BADDU rd, rs, rt; Cavium OCTEON */
            case 0x28: {
               DIP("BADDU r%u, r%u, r%u", regRs, regRt, regRd);
               IRTemp t0 = newTemp(Ity_I8);

               assign(t0, binop(Iop_Add8,
                                mkNarrowTo8(ty, getIReg(regRs)),
                                mkNarrowTo8(ty, getIReg(regRt))));

               if (mode64)
                  putIReg(regRd, binop(mkSzOp(ty, Iop_And8),
                                       unop(Iop_8Uto64, mkexpr(t0)),
                                       mkSzImm(ty, 0xFF)));
               else
                  putIReg(regRd, binop(mkSzOp(ty, Iop_And8),
                                       unop(Iop_8Uto32, mkexpr(t0)),
                                       mkSzImm(ty, 0xFF)));

               break;
            }

            case 0x2c: {  /* Count Ones in a Word - POP; Cavium OCTEON */
               int i, shift[5];
               IRTemp mask[5];
               IRTemp old = newTemp(ty);
               IRTemp nyu = IRTemp_INVALID;
               assign(old, getIReg(regRs));
               DIP("pop r%u, r%u", regRd, regRs);

               for (i = 0; i < 5; i++) {
                  mask[i] = newTemp(ty);
                  shift[i] = 1 << i;
               }

               if (mode64) {
                  assign(mask[0], mkU64(0x0000000055555555));
                  assign(mask[1], mkU64(0x0000000033333333));
                  assign(mask[2], mkU64(0x000000000F0F0F0F));
                  assign(mask[3], mkU64(0x0000000000FF00FF));
                  assign(mask[4], mkU64(0x000000000000FFFF));

                  for (i = 0; i < 5; i++) {
                     nyu = newTemp(ty);
                     assign(nyu,
                            binop(Iop_Add64,
                                  binop(Iop_And64,
                                        mkexpr(old), mkexpr(mask[i])),
                                  binop(Iop_And64,
                                        binop(Iop_Shr64,
                                              mkexpr(old), mkU8(shift[i])),
                                        mkexpr(mask[i]))));
                     old = nyu;
                  }
               } else {
                  assign(mask[0], mkU32(0x55555555));
                  assign(mask[1], mkU32(0x33333333));
                  assign(mask[2], mkU32(0x0F0F0F0F));
                  assign(mask[3], mkU32(0x00FF00FF));
                  assign(mask[4], mkU32(0x0000FFFF));
                  assign(old, getIReg(regRs));

                  for (i = 0; i < 5; i++) {
                     nyu = newTemp(ty);
                     assign(nyu,
                            binop(Iop_Add32,
                                  binop(Iop_And32,
                                        mkexpr(old), mkexpr(mask[i])),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32,
                                              mkexpr(old), mkU8(shift[i])),
                                        mkexpr(mask[i]))));
                     old = nyu;
                  }
               }

               putIReg(regRd, mkexpr(nyu));
               break;
            }

            /* Count Ones in a Doubleword - DPOP; Cavium OCTEON */
            case 0x2d: {
               int i, shift[6];
               IRTemp mask[6];
               IRTemp old = newTemp(ty);
               IRTemp nyu = IRTemp_INVALID;
               DIP("dpop r%u, r%u", regRd, regRs);

               for (i = 0; i < 6; i++) {
                  mask[i] = newTemp(ty);
                  shift[i] = 1 << i;
               }

               vassert(mode64); /*Caution! Only for Mode 64*/
               assign(mask[0], mkU64(0x5555555555555555ULL));
               assign(mask[1], mkU64(0x3333333333333333ULL));
               assign(mask[2], mkU64(0x0F0F0F0F0F0F0F0FULL));
               assign(mask[3], mkU64(0x00FF00FF00FF00FFULL));
               assign(mask[4], mkU64(0x0000FFFF0000FFFFULL));
               assign(mask[5], mkU64(0x00000000FFFFFFFFULL));
               assign(old, getIReg(regRs));

               for (i = 0; i < 6; i++) {
                  nyu = newTemp(Ity_I64);
                  assign(nyu,
                         binop(Iop_Add64,
                               binop(Iop_And64,
                                     mkexpr(old), mkexpr(mask[i])),
                               binop(Iop_And64,
                                     binop(Iop_Shr64,
                                           mkexpr(old), mkU8(shift[i])),
                                     mkexpr(mask[i]))));
                  old = nyu;
               }

               putIReg(regRd, mkexpr(nyu));
               break;
            }

            case 0x32:  /* 5. CINS rd, rs, p, lenm1 */
               DIP("cins r%u, r%u, %u, %u\n", regRt, regRs, p, lenM1);
               assign ( tmp  , binop(Iop_Shl64, mkexpr(tmpRs),
                                     mkU8(64 - ( lenM1 + 1 ))));
               assign ( tmpRt, binop(Iop_Shr64, mkexpr( tmp ),
                                     mkU8(64 - (p + lenM1 + 1))));
               putIReg( regRt, mkexpr(tmpRt));
               break;

            case 0x33:  /* 6. CINS32 rd, rs, p+32, lenm1 */
               DIP("cins32 r%u, r%u, %d, %d\n", regRt, regRs, p + 32, lenM1);
               assign ( tmp  , binop(Iop_Shl64, mkexpr(tmpRs),
                                     mkU8(64 - ( lenM1 + 1 ))));
               assign ( tmpRt, binop(Iop_Shr64, mkexpr( tmp ),
                                     mkU8(32 - (p + lenM1 + 1))));
               putIReg( regRt, mkexpr(tmpRt));
               break;

            case 0x3A:  /* 3. EXTS rt, rs, p len */
               DIP("exts r%u, r%u, %d, %d\n", regRt, regRs, p, lenM1);
               size = lenM1 + 1;  /* lenm1+1 */
               UChar lsAmt = 64 - (p + size);  /* p+lenm1+1 */
               UChar rsAmt = 64 - size;  /* lenm1+1 */
               tmp = newTemp(Ity_I64);
               assign(tmp, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
               putIReg(regRt, binop(Iop_Sar64, mkexpr(tmp), mkU8(rsAmt)));
               break;

            case 0x3B:  /* 4. EXTS32 rt, rs, p len */
               DIP("exts32 r%u, r%u, %d, %d\n", regRt, regRs, p, lenM1);
               assign ( tmp  , binop(Iop_Shl64, mkexpr(tmpRs),
                                     mkU8(32 - (p + lenM1 + 1))));
               assign ( tmpRt, binop(Iop_Sar64, mkexpr(tmp),
                                     mkU8(64 - (lenM1 + 1))) );
               putIReg( regRt, mkexpr(tmpRt));
               break;

            case 0x2B:  /* 20. SNE rd, rs, rt */
               DIP("sne r%u, r%u, r%u", regRd, regRs, regRt);

               if (mode64)
                  putIReg(regRd, unop(Iop_1Uto64, binop(Iop_CmpNE64,
                                                        getIReg(regRs),
                                                        getIReg(regRt))));
               else
                  putIReg(regRd, unop(Iop_1Uto32, binop(Iop_CmpNE32,
                                                        getIReg(regRs),
                                                        getIReg(regRt))));

               break;

            case 0x2A:  /* Set Equals - SEQ; Cavium OCTEON */
               DIP("seq r%u, r%u, %d", regRd, regRs, regRt);

               if (mode64)
                  putIReg(regRd, unop(Iop_1Uto64,
                                      binop(Iop_CmpEQ64, getIReg(regRs),
                                            getIReg(regRt))));
               else
                  putIReg(regRd, unop(Iop_1Uto32,
                                      binop(Iop_CmpEQ32, getIReg(regRs),
                                            getIReg(regRt))));

               break;

            case 0x2E:  /* Set Equals Immediate - SEQI; Cavium OCTEON */
               DIP("seqi r%u, r%u, %u", regRt, regRs, imm);

               if (mode64)
                  putIReg(regRt, unop(Iop_1Uto64,
                                      binop(Iop_CmpEQ64, getIReg(regRs),
                                            mkU64(extend_s_10to64(imm)))));
               else
                  putIReg(regRt, unop(Iop_1Uto32,
                                      binop(Iop_CmpEQ32, getIReg(regRs),
                                            mkU32(extend_s_10to32(imm)))));

               break;

            case 0x2F:  /* Set Not Equals Immediate - SNEI; Cavium OCTEON */
               DIP("snei r%u, r%u, %u", regRt, regRs, imm);

               if (mode64)
                  putIReg(regRt, unop(Iop_1Uto64,
                                      binop(Iop_CmpNE64,
                                            getIReg(regRs),
                                            mkU64(extend_s_10to64(imm)))));
               else
                  putIReg(regRt, unop(Iop_1Uto32,
                                      binop(Iop_CmpNE32,
                                            getIReg(regRs),
                                            mkU32(extend_s_10to32(imm)))));

               break;

            default:
               return False;
         }

         break;
      } /* opc1 0x1C ends here*/

      case 0x1F: {
         switch (opc2) {
            case 0x0A: {  // lx - Load indexed instructions
               switch (get_sa(theInstr)) {
                  case 0x00: {  // LWX rd, index(base)
                     DIP("lwx r%u, r%u(r%u)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;
                     putIReg(regRd, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)),
                                                  True));
                     break;
                  }

                  case 0x04:  // LHX rd, index(base)
                     DIP("lhx r%u, r%u(r%u)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;

                     if (mode64)
                        putIReg(regRd, unop(Iop_16Sto64, load(Ity_I16,
                                                              mkexpr(t1))));
                     else
                        putIReg(regRd, unop(Iop_16Sto32, load(Ity_I16,
                                                              mkexpr(t1))));

                     break;

                  case 0x08: {  // LDX rd, index(base)
                     DIP("ldx r%u, r%u(r%u)", regRd, regRt, regRs);
                     vassert(mode64); /* Currently Implemented only for n64 */
                     LOADX_STORE_PATTERN;
                     putIReg(regRd, load(Ity_I64, mkexpr(t1)));
                     break;
                  }

                  case 0x06: {  // LBUX rd, index(base)
                     DIP("lbux r%u, r%u(r%u)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;

                     if (mode64)
                        putIReg(regRd, unop(Iop_8Uto64, load(Ity_I8,
                                                             mkexpr(t1))));
                     else
                        putIReg(regRd, unop(Iop_8Uto32, load(Ity_I8,
                                                             mkexpr(t1))));

                     break;
                  }

                  case 0x10: {  // LWUX rd, index(base) (Cavium OCTEON)
                     DIP("lwux r%u, r%u(r%u)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN; /* same for both 32 and 64 modes*/
                     putIReg(regRd, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)),
                                                  False));
                     break;
                  }

                  case 0x14: {  // LHUX rd, index(base) (Cavium OCTEON)
                     DIP("lhux r%u, r%u(r%u)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;

                     if (mode64)
                        putIReg(regRd,
                                unop(Iop_16Uto64, load(Ity_I16, mkexpr(t1))));
                     else
                        putIReg(regRd,
                                unop(Iop_16Uto32, load(Ity_I16, mkexpr(t1))));

                     break;
                  }

                  case 0x16: {  // LBX rd, index(base) (Cavium OCTEON)
                     DIP("lbx r%u, r%u(r%u)", regRd, regRs, regRt);
                     LOADX_STORE_PATTERN;

                     if (mode64)
                        putIReg(regRd,
                                unop(Iop_8Sto64, load(Ity_I8, mkexpr(t1))));
                     else
                        putIReg(regRd,
                                unop(Iop_8Sto32, load(Ity_I8, mkexpr(t1))));

                     break;
                  }

                  default:
                     vex_printf("\nUnhandled LX instruction opc3 = %x\n",
                                get_sa(theInstr));
                     return False;
               }

               break;
            }
         } /* opc1 = 0x1F & opc2 = 0xA (LX) ends here*/

         break;
      } /* opc1 = 0x1F ends here*/

      default:
         return False;
   } /* main opc1 switch ends here */

   return True;
}

static Int msa_I8_logical(UInt cins, UChar wd, UChar ws)
{
   IRTemp t1, t2;
   UShort operation;
   UChar i8;

   operation = (cins >> 24) & 3;
   i8 = (cins & 0x00FF0000) >> 16;

   switch (operation) {
      case 0x00: {  /* ANDI.B */
         DIP("ANDI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t1, getWReg(ws));
         assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         putWReg(wd, binop(Iop_AndV128, mkexpr(t1), mkexpr(t2)));
         break;
      }

      case 0x01: { /* ORI.B */
         DIP("ORI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t1, getWReg(ws));
         assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         putWReg(wd, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         break;
      }

      case 0x02: { /* NORI.B */
         DIP("NORI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t1, getWReg(ws));
         assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         putWReg(wd, unop(Iop_NotV128, binop(Iop_OrV128,
                                             mkexpr(t1), mkexpr(t2))));
         break;
      }

      case 0x03: {  /* XORI.B */
         DIP("XORI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t1, getWReg(ws));
         assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         putWReg(wd, binop(Iop_XorV128, mkexpr(t1), mkexpr(t2)));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_I8_branch(UInt cins, UChar wd, UChar ws)
{
   IRTemp t1, t2, t3, t4;
   UShort operation;
   UChar i8;

   operation = (cins >> 24) & 3;
   i8 = (cins & 0x00FF0000) >> 16;

   switch (operation) {
      case 0x00: { /* BMNZI.B */
         DIP("BMNZI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         t4 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t4, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         assign(t1, binop(Iop_AndV128, getWReg(ws), mkexpr(t4)));
         assign(t2, binop(Iop_AndV128, getWReg(wd),
                          unop(Iop_NotV128, mkexpr(t4))));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x01: { /* BMZI.B */
         DIP("BMZI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         t4 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t4, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         assign(t1, binop(Iop_AndV128, getWReg(wd), mkexpr(t4)));
         assign(t2, binop(Iop_AndV128, getWReg(ws),
                          unop(Iop_NotV128, mkexpr(t4))));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x02: { /* BSELI.B */
         DIP("BSELI.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         t4 = newTemp(Ity_V128);
         ULong tmp = i8;
         tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                (tmp << 32) | (tmp << 24) | (tmp << 16) |
                (tmp << 8);
         assign(t4, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         assign(t1, binop(Iop_AndV128, getWReg(wd), mkexpr(t4)));
         assign(t2, binop(Iop_AndV128, getWReg(ws),
                          unop(Iop_NotV128, getWReg(wd))));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_I8_shift(UInt cins, UChar wd, UChar ws)
{
   IRTemp t1, t2;
   UShort operation;
   UChar i8;

   operation = (cins >> 24) & 3;
   i8 = (cins & 0x00FF0000) >> 16;

   switch (operation) {
      case 0x00: { /* SHF.B */
         DIP("SHF.B w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(wd));
         assign(t2, getWReg(ws));
         Int i;
         IRTemp tmp[16];

         for (i = 0; i < 16; i++) {
            tmp[i] = newTemp(Ity_I8);
            assign(tmp[i],
                   binop(Iop_GetElem8x16, mkexpr(t2),
                         mkU8(i - (i % 4) +
                              ((i8 >> (i % 4) * 2) & 0x03))));
         }

         putWReg(wd, binop(Iop_64HLtoV128,
                           binop(Iop_32HLto64,
                                 binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[15]),
                                             mkexpr(tmp[14])),
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[13]),
                                             mkexpr(tmp[12]))),
                                 binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[11]),
                                             mkexpr(tmp[10])),
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[9]),
                                             mkexpr(tmp[8])))),
                           binop(Iop_32HLto64,
                                 binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[7]),
                                             mkexpr(tmp[6])),
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[5]),
                                             mkexpr(tmp[4]))),
                                 binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[3]),
                                             mkexpr(tmp[2])),
                                       binop(Iop_8HLto16,
                                             mkexpr(tmp[1]),
                                             mkexpr(tmp[0]))))));
         break;
      }

      case 0x01: { /* SHF.H */
         DIP("SHF.H w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(wd));
         assign(t2, getWReg(ws));
         Int i;
         IRTemp tmp[8];

         for (i = 0; i < 8; i++) {
            tmp[i] = newTemp(Ity_I16);
            assign(tmp[i],
                   binop(Iop_GetElem16x8, mkexpr(t2),
                         mkU8(i - (i % 4) +
                              ((i8 >> (i % 4) * 2) & 0x03))));
         }

         putWReg(wd, binop(Iop_64HLtoV128,
                           binop(Iop_32HLto64,
                                 binop(Iop_16HLto32,
                                       mkexpr(tmp[7]), mkexpr(tmp[6])),
                                 binop(Iop_16HLto32,
                                       mkexpr(tmp[5]), mkexpr(tmp[4]))),
                           binop(Iop_32HLto64,
                                 binop(Iop_16HLto32,
                                       mkexpr(tmp[3]), mkexpr(tmp[2])),
                                 binop(Iop_16HLto32,
                                       mkexpr(tmp[1]), mkexpr(tmp[0])))));
         break;
      }

      case 0x02: { /* SHF.W */
         DIP("SHF.W w%d, w%d, %d", wd, ws, i8);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(wd));
         assign(t2, getWReg(ws));
         Int i;
         IRTemp tmp[4];

         for (i = 0; i < 4; i++) {
            tmp[i] = newTemp(Ity_I32);
            assign(tmp[i],
                   binop(Iop_GetElem32x4, mkexpr(t2),
                         mkU8(i - (i % 4) +
                              ((i8 >> (i % 4) * 2) & 0x03))));
         }

         putWReg(wd, binop(Iop_64HLtoV128,
                           binop(Iop_32HLto64,
                                 mkexpr(tmp[3]), mkexpr(tmp[2])),
                           binop(Iop_32HLto64,
                                 mkexpr(tmp[1]), mkexpr(tmp[0]))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_I5_06(UInt cins, UChar wd, UChar ws)   /* I5 (0x06) */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* ADDVI */
         ULong tmp = wt;

         switch (df) {
            case 0x00: { /* ADDVI.B */
               DIP("ADDVI.B w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Add8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ADDVI.H */
               DIP("ADDVI.H w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Add16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ADDVI.W */
               DIP("ADDVI.W w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Add32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ADDVI.D */
               DIP("ADDVI.D w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Add64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x01: { /* SUBVI */
         ULong tmp = wt;

         switch (df) {
            case 0x00: { /* SUBVI.B */
               DIP("SUBVI.B w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Sub8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SUBVI.H */
               DIP("SUBVI.H w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Sub16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SUBVI.W */
               DIP("SUBVI.W w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Sub32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SUBVI.D */
               DIP("SUBVI.D w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Sub64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x02: { /* MAXI_S */
         ULong tmp = wt;

         switch (df) {
            case 0x00: { /* MAXI_S.B */
               DIP("MAXI_S.B w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               char stemp = ((int)tmp << 27) >> 27;
               tmp = (UChar)stemp;
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MAXI_S.H */
               DIP("MAXI_S.H w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               short stemp = ((int)tmp << 27) >> 27;
               tmp = (UShort)stemp;
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MAXI_S.W */
               DIP("MAXI_S.W w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               int stemp = ((int)tmp << 27) >> 27;
               tmp = (UInt)stemp;
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MAXI_S.D */
               DIP("MAXI_S.D w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               Long stemp = ((Long)tmp << 59) >> 59;
               tmp = stemp;
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max64Sx2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x03: { /* MAXI_U */
         ULong tmp = wt;

         switch (df) {
            case 0x00: { /* MAXI_U.B */
               DIP("MAXI_U.B w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MAXI_U.H */
               DIP("MAXI_U.H w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MAXI_U.W */
               DIP("MAXI_U.W w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MAXI_U.D */
               DIP("MAXI_U.D w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Max64Ux2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x04: { /* MINI_S */
         ULong tmp = wt;

         switch (df) {
            case 0x00: { /* MINI_S.B */
               DIP("MINI_S.B w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               char stemp = ((int)tmp << 27) >> 27;
               tmp = (UChar)stemp;
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MINI_S.H */
               DIP("MINI_S.H w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               short stemp = ((int)tmp << 27) >> 27;
               tmp = (UShort)stemp;
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MINI_S.W */
               DIP("MINI_S.W w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               int stemp = ((int)tmp << 27) >> 27;
               tmp = (UInt)stemp;
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MINI_S.D */
               DIP("MINI_S.D w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               Long stemp = ((Long)tmp << 59) >> 59;
               tmp = stemp;
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min64Sx2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x05: { /* MINI_U */
         ULong tmp = wt;

         switch (df) {
            case 0x00: { /* MINI_U.B */
               DIP("MINI_U.B w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MINI_U.H */
               DIP("MINI_U.H w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MINI_U.W */
               DIP("MINI_U.W w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MINI_U.D */
               DIP("MINI_U.D w%d, w%d, %d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_Min64Ux2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      default: {
         return -1;
      }
   }

   return 0;
}

static Int msa_I5_07(UInt cins, UChar wd, UChar ws)   /* I5 (0x07) / I10 */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar df, i5;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   i5 = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: {
         ULong tmp = i5;

         switch (df) {
            case 0x00: { /* CEQI.B */
               DIP("CEQI.B w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               char stemp = ((int)tmp << 27) >> 27;
               tmp = (UChar)stemp;
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpEQ8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CEQI.H */
               DIP("CEQI.H w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               short stemp = ((int)tmp << 27) >> 27;
               tmp = (UShort)stemp;
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpEQ16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CEQI.W */
               DIP("CEQI.W w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               int stemp = ((int)tmp << 27) >> 27;
               tmp = (UInt)stemp;
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpEQ32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CEQI.D */
               DIP("CEQI.D w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               Long stemp = ((Long)tmp << 59) >> 59;
               tmp = stemp;
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpEQ64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x02: { /* CLTI_S.df */
         ULong tmp = i5;

         switch (df) {
            case 0x00: { /* CLTI_S.B */
               DIP("CLTI_S.B w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               char stemp = ((int)tmp << 27) >> 27;
               tmp = (UChar)stemp;
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT8Sx16, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLTI_S.H */
               DIP("CLTI_S.H w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               short stemp = ((int)tmp << 27) >> 27;
               tmp = (UShort)stemp;
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT16Sx8, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLTI_S.W */
               DIP("CLTI_S.W w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               int stemp = ((int)tmp << 27) >> 27;
               tmp = (UInt)stemp;
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT32Sx4, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLTI_S.D */
               DIP("CLTI_S.D w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               Long stemp = ((Long)tmp << 59) >> 59;
               tmp = stemp;
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT64Sx2, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* CLTI_U.df */
         ULong tmp = i5;

         switch (df) {
            case 0x00: { /* CLTI_U.B */
               DIP("CLTI_U.B w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT8Ux16, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLTI_U.H */
               DIP("CLTI_U.H w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT16Ux8, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLTI_U.W */
               DIP("CLTI_U.W w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT32Ux4, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLTI_U.D */
               DIP("CLTI_U.D w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_CmpGT64Ux2, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x04: { /* CLEI_S.df */
         ULong tmp = i5;

         switch (df) {
            case 0x00: { /* CLEI_S.B */
               DIP("CLEI_S.B w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               char stemp = ((int)tmp << 27) >> 27;
               tmp = (UChar)stemp;
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128, binop(Iop_CmpGT8Sx16,
                                                  mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ8x16,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLEI_S.H */
               DIP("CLEI_S.H w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               short stemp = ((int)tmp << 27) >> 27;
               tmp = (UShort)stemp;
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128, binop(Iop_CmpGT16Sx8,
                                                  mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ16x8,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLEI_S.W */
               DIP("CLEI_S.W w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               int stemp = ((int)tmp << 27) >> 27;
               tmp = (UInt)stemp;
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT32Sx4,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ32x4,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLEI_S.D */
               DIP("CLEI_S.D w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               Long stemp = ((Long)tmp << 59) >> 59;
               tmp = stemp;
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT64Sx2,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ64x2,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* CLEI_U.df */
         ULong tmp = i5;

         switch (df) {
            case 0x00: { /* CLEI_U.B */
               DIP("CLEI_U.B w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT8Ux16,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ8x16,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLEI_U.H */
               DIP("CLEI_U.H w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT16Ux8,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ16x8,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLEI_U.W */
               DIP("CLEI_U.W w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               tmp |= (tmp << 32);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT32Ux4,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ32x4,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLEI_U.D */
               DIP("CLEI_U.D w%d, w%d, %d", wd, ws, i5);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT64Ux2,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ64x2,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x06: { /* LDI.df */
         ULong tmp;
         UShort s10;
         s10 = (cins & 0x001FF800) >> 11;

         switch (df) {
            case 0x00: /* LDI.B */
               DIP("LDI.B w%d, %d", wd, s10);
               tmp = s10 & 0xFFl;
               tmp = tmp | (tmp << 8) | (tmp << 16) | (tmp << 24)
                     | (tmp << 32) | (tmp << 40) | (tmp << 48) |
                     (tmp << 56);
               break;

            case 0x01: /* LDI.H */
               DIP("LDI.H w%d, %d", wd, s10);
               tmp = extend_s_10to16(s10);
               tmp = tmp | (tmp << 16) | (tmp << 32) | (tmp << 48);
               break;

            case 0x02: /* LDI.W */
               DIP("LDI.W w%d, %d", wd, s10);
               tmp = extend_s_10to32(s10);
               tmp = tmp | (tmp << 32);
               break;

            case 0x03: /* LDI.D */
               DIP("LDI.D w%d, %d", wd, s10);
               tmp = extend_s_10to64(s10);
               break;

            default:
               return -1;
         }

         putWReg(wd, binop(Iop_64HLtoV128, mkU64(tmp), mkU64(tmp)));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_BIT_09(UInt cins, UChar wd, UChar ws)   /* BIT (0x09) */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar df, m;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x007F0000) >> 16;

   if ((df & 0x70) == 0x70) {        // 111mmmm; b
      m = df & 0x07;
      df = 0;
   } else if ((df & 0x60) == 0x60) { // 110mmmm; h
      m = df & 0x0F;
      df = 1;
   } else if ((df & 0x40) == 0x40) { // 10mmmmm; w
      m = df & 0x1F;
      df = 2;
   } else if ((df & 0x00) == 0x00) { // 0mmmmmm; d
      m = df & 0x3F;
      df = 3;
   }

   switch (operation) {
      case 0x00: { /* SLLI.df */
         switch (df) {
            case 0x00: { /* SLLI.B */
               DIP("SLLI.B w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShlN8x16, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x01: { /* SLLI.H */
               DIP("SLLI.H w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShlN16x8, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x02: { /* SLLI.W */
               DIP("SLLI.W w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShlN32x4, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x03: { /* SLLI.D */
               DIP("SLLI.D w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShlN64x2, getWReg(ws), mkU8(m)));
               break;
            }
         }

         break;
      }

      case 0x01: { /* SRAI.df */
         switch (df) {
            case 0x00: { /* SRAI.B */
               DIP("SRAI.B w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_SarN8x16, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x01: { /* SRAI.H */
               DIP("SRAI.H w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_SarN16x8, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x02: { /* SRAI.W */
               DIP("SRAI.W w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_SarN32x4, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x03: { /* SRAI.D */
               DIP("SRAI.D w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_SarN64x2, getWReg(ws), mkU8(m)));
               break;
            }
         }

         break;
      }

      case 0x02: { /* SRLI.df */
         switch (df) {
            case 0x00: { /* SRLI.B */
               DIP("SRLI.B w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShrN8x16, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x01: { /* SRLI.H */
               DIP("SRLI.H w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShrN16x8, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x02: { /* SRLI.W */
               DIP("SRLI.W w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShrN32x4, getWReg(ws), mkU8(m)));
               break;
            }

            case 0x03: { /* SRLI.D */
               DIP("SRLI.D w%d, w%d, %d", wd, ws, m);
               putWReg(wd, binop(Iop_ShrN64x2, getWReg(ws), mkU8(m)));
               break;
            }
         }

         break;
      }

      case 0x03: { /* BCLRI.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         ULong tmp = 1;
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* BCLRI.B */
               DIP("BCLRI.B w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t2, binop(Iop_ShlN8x16,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x01: { /* BCLRI.H */
               DIP("BCLRI.H w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t2, binop(Iop_ShlN16x8,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x02: { /* BCLRI.W */
               DIP("BCLRI.W w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 32);
               assign(t2, binop(Iop_ShlN32x4,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x03: { /* BCLRI.D */
               DIP("BCLRI.D w%d, w%d, %d", wd, ws, m);
               assign(t2, binop(Iop_ShlN64x2,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }
         }

         assign(t3, binop(Iop_AndV128,
                          mkexpr(t1), unop(Iop_NotV128, mkexpr(t2))));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x04: { /* BSETI */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         ULong tmp = 1;
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* BSETI.B */
               DIP("BSETI.B w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t2, binop(Iop_ShlN8x16,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x01: { /* BSETI.H */
               DIP("BSETI.H w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t2, binop(Iop_ShlN16x8,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x02: { /* BSETI.W */
               DIP("BSETI.W w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 32);
               assign(t2, binop(Iop_ShlN32x4,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x03: { /* BSETI.D */
               DIP("BSETI.D w%d, w%d, %d", wd, ws, m);
               assign(t2, binop(Iop_ShlN64x2,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }
         }

         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x05: { /* BNEGI.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         ULong tmp = 1;
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* BNEGI.B */
               DIP("BNEGI.B w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t2, binop(Iop_ShlN8x16,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x01: { /* BNEGI.H */
               DIP("BNEGI.H w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t2, binop(Iop_ShlN16x8,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x02: { /* BNEGI.W */
               DIP("BNEGI.W w%d, w%d, %d", wd, ws, m);
               tmp |= (tmp << 32);
               assign(t2, binop(Iop_ShlN32x4,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }

            case 0x03: { /* BNEGI.D */
               DIP("BNEGI.D w%d, w%d, %d", wd, ws, m);
               assign(t2, binop(Iop_ShlN64x2,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               break;
            }
         }

         assign(t3, binop(Iop_XorV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x06: { /* BINSLI.df */
         switch (df) {
            case 0x00: { /* BINSLI.B */
               DIP("BINSLI.B w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8080808080808080ULL;
               assign(t1, binop(Iop_SarN8x16,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)), mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)), getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x01: { /* BINSLI.H */
               DIP("BINSLI.H w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8000800080008000ULL;
               assign(t1,
                      binop(Iop_SarN16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)), mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)), getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x02: { /* BINSLI.W */
               DIP("BINSLI.W w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8000000080000000ULL;
               assign(t1,
                      binop(Iop_SarN32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)), mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)), getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x03: { /* BINSLI.D */
               DIP("BINSLI.D w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8000000000000000ULL;
               assign(t1,
                      binop(Iop_SarN64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)), mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)), getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: {
         switch (df) {
            case 0x00: { /* BINSRI.B */
               DIP("BINSRI.B w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0xFEFEFEFEFEFEFEFEULL;
               assign(t1,
                      binop(Iop_ShlN8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)), mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)), getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x01: { /* BINSRI.H */
               DIP("BINSRI.H w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0xFFFEFFFEFFFEFFFEULL;
               assign(t1,
                      binop(Iop_ShlN16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x02: { /* BINSRI.W */
               DIP("BINSRI.W w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0xFFFFFFFEFFFFFFFEULL;
               assign(t1,
                      binop(Iop_ShlN32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x03: { /* BINSRI.D */
               DIP("BINSRI.D w%d, w%d, w%d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = -2;
               assign(t1,
                      binop(Iop_ShlN64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_BIT_0A(UInt cins, UChar wd, UChar ws)   /* BIT (0x0A) */
{
   IRTemp t1, t2;
   UShort operation;
   UChar df, m;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x007F0000) >> 16;

   if ((df & 0x70) == 0x70) {        // 111mmmm; b
      m = df & 0x07;
      df = 0;
   } else if ((df & 0x60) == 0x60) { // 110mmmm; h
      m = df & 0x0F;
      df = 1;
   } else if ((df & 0x40) == 0x40) { // 10mmmmm; w
      m = df & 0x1F;
      df = 2;
   } else if ((df & 0x00) == 0x00) { // 0mmmmmm; d
      m = df & 0x3F;
      df = 3;
   }

   switch (operation) {
      case 0x00: { /* SAT_S.df */
         switch (df) {
            case 0x00: { /* SAT_S.B */
               DIP("SAT_S.B w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN8x16, getWReg(ws), mkU8(7)));

               if (m == 0) {
                  putWReg(wd, mkexpr(t1));
               } else {
                  t2 = newTemp(Ity_V128);
                  assign(t2,
                         binop(Iop_SarN8x16, getWReg(ws), mkU8(m)));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_OrV128,
                                      binop(Iop_AndV128,
                                            binop(Iop_CmpEQ8x16,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            getWReg(ws)),
                                      binop(Iop_ShlN8x16,
                                            binop(Iop_CmpGT8Sx16,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(m))),
                                binop(Iop_ShrN8x16,
                                      binop(Iop_CmpGT8Sx16,
                                            mkexpr(t2),
                                            mkexpr(t1)),
                                      mkU8(8 - m))));
               }

               break;
            }

            case 0x01: { /* SAT_S.H */
               DIP("SAT_S.H w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN16x8, getWReg(ws), mkU8(15)));

               if (m == 0) {
                  putWReg(wd, mkexpr(t1));
               } else {
                  t2 = newTemp(Ity_V128);
                  assign(t2,
                         binop(Iop_SarN16x8,
                               getWReg(ws),
                               mkU8(m)));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_OrV128,
                                      binop(Iop_AndV128,
                                            binop(Iop_CmpEQ16x8,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            getWReg(ws)),
                                      binop(Iop_ShlN16x8,
                                            binop(Iop_CmpGT16Sx8,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(m))),
                                binop(Iop_ShrN16x8,
                                      binop(Iop_CmpGT16Sx8,
                                            mkexpr(t2),
                                            mkexpr(t1)),
                                      mkU8(16 - m))));
               }

               break;
            }

            case 0x02: { /* SAT_S.W */
               DIP("SAT_S.W w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN32x4, getWReg(ws), mkU8(31)));

               if (m == 0) {
                  putWReg(wd, mkexpr(t1));
               } else {
                  t2 = newTemp(Ity_V128);
                  assign(t2,
                         binop(Iop_SarN32x4,
                               getWReg(ws),
                               mkU8(m)));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_OrV128,
                                      binop(Iop_AndV128,
                                            binop(Iop_CmpEQ32x4,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            getWReg(ws)),
                                      binop(Iop_ShlN32x4,
                                            binop(Iop_CmpGT32Sx4,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(m))),
                                binop(Iop_ShrN32x4,
                                      binop(Iop_CmpGT32Sx4,
                                            mkexpr(t2),
                                            mkexpr(t1)),
                                      mkU8(32 - m))));
               }

               break;
            }

            case 0x03: { /* SAT_S.D */
               DIP("SAT_S.D w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN64x2, getWReg(ws), mkU8(63)));

               if (m == 0) {
                  putWReg(wd, mkexpr(t1));
               } else {
                  t2 = newTemp(Ity_V128);
                  assign(t2,
                         binop(Iop_SarN64x2,
                               getWReg(ws),
                               mkU8(m)));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_OrV128,
                                      binop(Iop_AndV128,
                                            binop(Iop_CmpEQ64x2,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            getWReg(ws)),
                                      binop(Iop_ShlN64x2,
                                            binop(Iop_CmpGT64Sx2,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(m))),
                                binop(Iop_ShrN64x2,
                                      binop(Iop_CmpGT64Sx2,
                                            mkexpr(t2),
                                            mkexpr(t1)),
                                      mkU8(64 - m))));
               }

               break;
            }
         }

         break;
      }

      case 0x01: { /* SAT_U.df */
         switch (df) {
            case 0x00: { /* SAT_U.B */
               DIP("SAT_U.B w%d, w%d, %d", wd, ws, m);

               if (m == 7) {
                  putWReg(wd, getWReg(ws));
               } else {
                  t1 = newTemp(Ity_V128);
                  assign(t1,
                         binop(Iop_CmpEQ8x16,
                               binop(Iop_ShrN8x16,
                                     getWReg(ws),
                                     mkU8(m + 1)),
                               binop(Iop_64HLtoV128,
                                     mkU64(0), mkU64(0))));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                binop(Iop_ShrN8x16,
                                      unop(Iop_NotV128,
                                           mkexpr(t1)),
                                      mkU8(7 - m))));
               }

               break;
            }

            case 0x01: { /* SAT_U.H */
               DIP("SAT_U.H w%d, w%d, %d", wd, ws, m);

               if (m == 15) {
                  putWReg(wd, getWReg(ws));
               } else {
                  t1 = newTemp(Ity_V128);
                  assign(t1,
                         binop(Iop_CmpEQ16x8,
                               binop(Iop_ShrN16x8,
                                     getWReg(ws),
                                     mkU8(m + 1)),
                               binop(Iop_64HLtoV128,
                                     mkU64(0), mkU64(0))));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                binop(Iop_ShrN16x8,
                                      unop(Iop_NotV128,
                                           mkexpr(t1)),
                                      mkU8(15 - m))));
               }

               break;
            }

            case 0x02: { /* SAT_U.W */
               DIP("SAT_U.W w%d, w%d, %d", wd, ws, m);

               if (m == 31) {
                  putWReg(wd, getWReg(ws));
               } else {
                  t1 = newTemp(Ity_V128);
                  assign(t1,
                         binop(Iop_CmpEQ32x4,
                               binop(Iop_ShrN32x4,
                                     getWReg(ws),
                                     mkU8(m + 1)),
                               binop(Iop_64HLtoV128,
                                     mkU64(0), mkU64(0))));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t1), \
                                      getWReg(ws)),
                                binop(Iop_ShrN32x4,
                                      unop(Iop_NotV128,
                                           mkexpr(t1)),
                                      mkU8(31 - m))));
               }

               break;
            }

            case 0x03: { /* SAT_U.D */
               DIP("SAT_U.D w%d, w%d, %d", wd, ws, m);

               if (m == 63) {
                  putWReg(wd, getWReg(ws));
               } else {
                  t1 = newTemp(Ity_V128);
                  assign(t1,
                         binop(Iop_CmpEQ64x2,
                               binop(Iop_ShrN64x2,
                                     getWReg(ws),
                                     mkU8(m + 1)),
                               binop(Iop_64HLtoV128,
                                     mkU64(0), mkU64(0))));
                  putWReg(wd,
                          binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                binop(Iop_ShrN64x2,
                                      unop(Iop_NotV128,
                                           mkexpr(t1)),
                                      mkU8(63 - m))));
               }

               break;
            }
         }

         break;
      }

      case 0x02: { /* SRARI.df */
         switch (df) {
            case 0x00: { /* SRARI.B */
               DIP("SRARI.B w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_SarN8x16,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN8x16,
                            binop(Iop_ShlN8x16,
                                  getWReg(ws),
                                  mkU8(8 - m)),
                            mkU8(7)));

               if (m) putWReg(wd, binop(Iop_Add8x16,
                                           mkexpr(t1),
                                           mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }

            case 0x01: { /* SRARI.H */
               DIP("SRARI.H w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_SarN16x8,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN16x8,
                            binop(Iop_ShlN16x8,
                                  getWReg(ws),
                                  mkU8(16 - m)),
                            mkU8(15)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add16x8,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }

            case 0x02: { /* SRARI.W */
               DIP("SRARI.W w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_SarN32x4,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN32x4,
                            binop(Iop_ShlN32x4,
                                  getWReg(ws),
                                  mkU8(32 - m)),
                            mkU8(31)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add32x4,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }

            case 0x03: { /* SRARI.D */
               DIP("SRARI.D w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_SarN64x2,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN64x2,
                            binop(Iop_ShlN64x2,
                                  getWReg(ws),
                                  mkU8(64 - m)),
                            mkU8(63)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add64x2,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }
         }

         break;
      }

      case 0x03: { /* SRLRI.df */
         switch (df) {
            case 0x00: { /* SRLRI.B */
               DIP("SRLRI.B w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_ShrN8x16,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN8x16,
                            binop(Iop_ShlN8x16,
                                  getWReg(ws),
                                  mkU8(8 - m)),
                            mkU8(7)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add8x16,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }

            case 0x01: { /* SRLRI.H */
               DIP("SRLRI.H w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_ShrN16x8,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN16x8,
                            binop(Iop_ShlN16x8,
                                  getWReg(ws),
                                  mkU8(16 - m)),
                            mkU8(15)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add16x8,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }

            case 0x02: { /* SRLRI.W */
               DIP("SRLRI.W w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_ShrN32x4,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN32x4,
                            binop(Iop_ShlN32x4,
                                  getWReg(ws),
                                  mkU8(32 - m)),
                            mkU8(31)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add32x4,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }

            case 0x03: { /* SRLRI.D */
               DIP("SRLRI.D w%d, w%d, %d", wd, ws, m);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_ShrN64x2,
                            getWReg(ws),
                            mkU8(m)));
               assign(t2,
                      binop(Iop_ShrN64x2,
                            binop(Iop_ShlN64x2,
                                  getWReg(ws),
                                  mkU8(64 - m)),
                            mkU8(63)));

               if (m)
                  putWReg(wd,
                          binop(Iop_Add64x2,
                                mkexpr(t1), mkexpr(t2)));
               else putWReg(wd, mkexpr(t1));

               break;
            }
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_0D(UInt cins, UChar wd, UChar ws)   /* 3R (0x0D) */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* SLL.df */
         switch (df) {
            case 0x00: { /* SLL.B */
               DIP("SLL.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shl8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SLL.H */
               DIP("SLL.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shl16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SLL.W */
               DIP("SLL.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shl32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SLL.D */
               DIP("SLL.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shl64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* SRA.df */
         switch (df) {
            case 0x00: { /* SRA.B */
               DIP("SRA.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sar8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SRA.H */
               DIP("SRA.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sar16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SRA.W */
               DIP("SRA.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sar32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SRA.D */
               DIP("SRA.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sar64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* SRL.df */
         switch (df) {
            case 0x00: { /* SRL.B */
               DIP("SRL.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shr8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SRL.H */
               DIP("SRL.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shr16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SRL.W */
               DIP("SRL.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shr32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SRL.D */
               DIP("SRL.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Shr64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* BCLR.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         ULong tmp = 1;
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* BCLR.B */
               DIP("BCLR.B w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t2, binop(Iop_Shl8x16,
                                binop(Iop_64HLtoV128,
                                      mkU64(tmp), mkU64(tmp)),
                                getWReg(wt)));
               break;
            }

            case 0x01: { /* BCLR.H */
               DIP("BCLR.H w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t2,
                      binop(Iop_Shl16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x02: { /* BCLR.W */
               DIP("BCLR.W w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 32);
               assign(t2,
                      binop(Iop_Shl32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x03: { /* BCLR.D */
               DIP("BCLR.D w%d, w%d, w%d", wd, ws, wt);
               assign(t2,
                      binop(Iop_Shl64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }
         }

         assign(t3,
                binop(Iop_AndV128,
                      mkexpr(t1), unop(Iop_NotV128, mkexpr(t2))));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x04: { /* BSET.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         ULong tmp = 1;
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* BSET.B */
               DIP("BSET.B w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t2,
                      binop(Iop_Shl8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x01: { /* BSET.H */
               DIP("BSET.H w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t2,
                      binop(Iop_Shl16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x02: { /* BSET.W */
               DIP("BSET.W w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 32);
               assign(t2,
                      binop(Iop_Shl32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x03: { /* BSET.D */
               DIP("BSET.D w%d, w%d, w%d", wd, ws, wt);
               assign(t2,
                      binop(Iop_Shl64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }
         }

         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x05: { /* BNEG.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         ULong tmp = 1;
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* BNEG.B */
               DIP("BNEG.B w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 56) | (tmp << 48) | (tmp << 40) |
                      (tmp << 32) | (tmp << 24) | (tmp << 16) |
                      (tmp << 8);
               assign(t2,
                      binop(Iop_Shl8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x01: { /* BNEG.H */
               DIP("BNEG.H w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 48) | (tmp << 32) | (tmp << 16);
               assign(t2,
                      binop(Iop_Shl16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x02: { /* BNEG.W */
               DIP("BNEG.W w%d, w%d, w%d", wd, ws, wt);
               tmp |= (tmp << 32);
               assign(t2,
                      binop(Iop_Shl32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }

            case 0x03: { /* BNEG.D */
               DIP("BNEG.D w%d, w%d, w%d", wd, ws, wt);
               assign(t2,
                      binop(Iop_Shl64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               break;
            }
         }

         assign(t3, binop(Iop_XorV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x06: { /* BINSL.df */
         switch (df) {
            case 0x00: { /* BINSL.B */
               DIP("BINSL.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8080808080808080ULL;
               assign(t1,
                      binop(Iop_Sar8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x01: { /* BINSL.H */
               DIP("BINSL.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8000800080008000ULL;
               assign(t1,
                      binop(Iop_Sar16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x02: { /* BINSL.W */
               DIP("BINSL.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8000000080000000ULL;
               assign(t1,
                      binop(Iop_Sar32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x03: { /* BINSL.D */
               DIP("BINSL.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0x8000000000000000ULL;
               assign(t1,
                      binop(Iop_Sar64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(wd)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(ws)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* BINSR.df */
         switch (df) {
            case 0x00: { /* BINSR.B */
               DIP("BINSR.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0xFEFEFEFEFEFEFEFEULL;
               assign(t1,
                      binop(Iop_Shl8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x01: { /* BINSR.H */
               DIP("BINSR.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0xFFFEFFFEFFFEFFFEULL;
               assign(t1,
                      binop(Iop_Shl16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x02: { /* BINSR.W */
               DIP("BINSR.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = 0xFFFFFFFEFFFFFFFEULL;
               assign(t1,
                      binop(Iop_Shl32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x03: { /* BINSR.D */
               DIP("BINSR.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               ULong tmp = -2;
               assign(t1,
                      binop(Iop_Shl64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(tmp), mkU64(tmp)),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128, mkexpr(t1)),
                            getWReg(ws)));
               assign(t3,
                      binop(Iop_AndV128,
                            mkexpr(t1), getWReg(wd)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_0E(UInt cins, UChar wd, UChar ws)   /* 3R (0x0E) */
{
   IRTemp t1, t2, t3, t4;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* ADDV.df */
         switch (df) {
            case 0x00: { /* ADDV.B */
               DIP("ADDV.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ADDV.H */
               DIP("ADDV.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ADDV.W */
               DIP("ADDV.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ADDV.D */
               DIP("ADDV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* SUBV.df */
         switch (df) {
            case 0x00: { /* SUBV.B */
               DIP("SUBV.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sub8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SUBV.H */
               DIP("SUBV.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sub16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SUBV.W */
               DIP("SUBV.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sub32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SUBV.D */
               DIP("SUBV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Sub64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* MAX_S.df */
         switch (df) {
            case 0x00: { /* MAX_S.B */
               DIP("MAX_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MAX_S.H */
               DIP("MAX_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MAX_S.W */
               DIP("MAX_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MAX_S.D */
               DIP("MAX_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max64Sx2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* MAX_U.df */
         switch (df) {
            case 0x00: { /* MAX_U.B */
               DIP("MAX_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MAX_U.H */
               DIP("MAX_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MAX_U.W */
               DIP("MAX_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MAX_U.D */
               DIP("MAX_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Max64Ux2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* MIN_S.df */
         switch (df) {
            case 0x00: { /* MIN_S.B */
               DIP("MIN_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MIN_S.H */
               DIP("MIN_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MIN_S.W */
               DIP("MIN_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MIN_S.D */
               DIP("MIN_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min64Sx2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* MIN_U.df */
         switch (df) {
            case 0x00: { /* MIN_U.B */
               DIP("MIN_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MIN_U.H */
               DIP("MIN_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MIN_U.W */
               DIP("MIN_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MIN_U.D */
               DIP("MIN_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Min64Ux2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x06: { /* MAX_A.df */
         switch (df) {
            case 0x00: { /* MAX_A.B */
               DIP("MAX_A.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs8x16, getWReg(ws)));
               assign(t2, unop(Iop_Abs8x16, getWReg(wt)));
               assign(t4, binop(Iop_CmpGT8Ux16, mkexpr(t1), mkexpr(t2)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(ws)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(wt))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MAX_A.H */
               DIP("MAX_A.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs16x8, getWReg(ws)));
               assign(t2, unop(Iop_Abs16x8, getWReg(wt)));
               assign(t4, binop(Iop_CmpGT16Ux8, mkexpr(t1), mkexpr(t2)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(ws)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(wt))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MAX_A.W */
               DIP("MAX_A.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs32x4, getWReg(ws)));
               assign(t2, unop(Iop_Abs32x4, getWReg(wt)));
               assign(t4, binop(Iop_CmpGT32Ux4, mkexpr(t1), mkexpr(t2)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(ws)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(wt))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MAX_A.D */
               DIP("MAX_A.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs64x2, getWReg(ws)));
               assign(t2, unop(Iop_Abs64x2, getWReg(wt)));
               assign(t4, binop(Iop_CmpGT64Ux2, mkexpr(t1), mkexpr(t2)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(ws)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(wt))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* MIN_A.df */
         switch (df) {
            case 0x00: { /* MIN_A.B */
               DIP("MIN_A.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs8x16, getWReg(ws)));
               assign(t2, unop(Iop_Abs8x16, getWReg(wt)));
               assign(t4, binop(Iop_OrV128,
                                binop(Iop_CmpGT8Ux16,
                                      mkexpr(t1), mkexpr(t2)),
                                binop(Iop_CmpEQ8x16,
                                      mkexpr(t1), mkexpr(t2))));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(wt)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(ws))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MIN_A.H */
               DIP("MIN_A.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs16x8, getWReg(ws)));
               assign(t2, unop(Iop_Abs16x8, getWReg(wt)));
               assign(t4, binop(Iop_OrV128,
                                binop(Iop_CmpGT16Ux8,
                                      mkexpr(t1), mkexpr(t2)),
                                binop(Iop_CmpEQ16x8,
                                      mkexpr(t1), mkexpr(t2))));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(wt)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(ws))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* MIN_A.W */
               DIP("MIN_A.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs32x4, getWReg(ws)));
               assign(t2, unop(Iop_Abs32x4, getWReg(wt)));
               assign(t4, binop(Iop_OrV128,
                                binop(Iop_CmpGT32Ux4,
                                      mkexpr(t1), mkexpr(t2)),
                                binop(Iop_CmpEQ32x4,
                                      mkexpr(t1), mkexpr(t2))));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(wt)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(ws))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* MIN_A.D */
               DIP("MIN_A.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs64x2, getWReg(ws)));
               assign(t2, unop(Iop_Abs64x2, getWReg(wt)));
               assign(t4, binop(Iop_OrV128,
                                binop(Iop_CmpGT64Ux2,
                                      mkexpr(t1), mkexpr(t2)),
                                binop(Iop_CmpEQ64x2,
                                      mkexpr(t1), mkexpr(t2))));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_AndV128,
                                      mkexpr(t4),
                                      getWReg(wt)),
                                binop(Iop_AndV128,
                                      unop(Iop_NotV128, mkexpr(t4)),
                                      getWReg(ws))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_0F(UInt cins, UChar wd, UChar ws)   /* 3R (0x0F) */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* CEQ.df */
         switch (df) {
            case 0x00: { /* CEQ.B */
               DIP("CEQ.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpEQ8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CEQ.H */
               DIP("CEQ.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpEQ16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CEQ.W */
               DIP("CEQ.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpEQ32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CEQ.D */
               DIP("CEQ.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpEQ64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* CLT_S.df */
         switch (df) {
            case 0x00: { /* CLT_S.B */
               DIP("CLT_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT8Sx16, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLT_S.H */
               DIP("CLT_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT16Sx8, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLT_S.W */
               DIP("CLT_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT32Sx4, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLT_S.D */
               DIP("CLT_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT64Sx2, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* CLT_U.df */
         switch (df) {
            case 0x00: { /* CLT_U.B */
               DIP("CLT_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT8Ux16, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLT_U.H */
               DIP("CLT_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT16Ux8, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLT_U.W */
               DIP("CLT_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT32Ux4, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLT_U.D */
               DIP("CLT_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_CmpGT64Ux2, mkexpr(t2), mkexpr(t1)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* CLE_S.df */
         switch (df) {
            case 0x00: { /* CLE_S.B */
               DIP("CLE_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT8Sx16,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ8x16,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLE_S.H */
               DIP("CLE_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT16Sx8,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ16x8,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLE_S.W */
               DIP("CLE_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT32Sx4,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ32x4,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLE_S.D */
               DIP("CLE_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT64Sx2,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ64x2,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* CLE_U.df */
         switch (df) {
            case 0x00: { /* CLE_U.B */
               DIP("CLE_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT8Ux16,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ8x16,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* CLE_U.H */
               DIP("CLE_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT16Ux8,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ16x8,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* CLE_U.W */
               DIP("CLE_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT32Ux4,
                                      mkexpr(t2), mkexpr(t1)),
                                binop(Iop_CmpEQ32x4,
                                      mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* CLE_U.D */
               DIP("CLE_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_OrV128,
                            binop(Iop_CmpGT64Ux2,
                                  mkexpr(t2), mkexpr(t1)),
                            binop(Iop_CmpEQ64x2,
                                  mkexpr(t1), mkexpr(t2))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_10(UInt cins, UChar wd, UChar ws)   /* 3R (0x10) */
{
   IRTemp t1, t2, t3, t4;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* ADD_A.df */
         switch (df) {
            case 0x00: { /* ADD_A.B */
               DIP("ADD_A.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs8x16, getWReg(ws)));
               assign(t2, unop(Iop_Abs8x16, getWReg(wt)));
               assign(t3, binop(Iop_Add8x16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ADD_A.H */
               DIP("ADD_A.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs16x8, getWReg(ws)));
               assign(t2, unop(Iop_Abs16x8, getWReg(wt)));
               assign(t3, binop(Iop_Add16x8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ADD_A.W */
               DIP("ADD_A.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs32x4, getWReg(ws)));
               assign(t2, unop(Iop_Abs32x4, getWReg(wt)));
               assign(t3, binop(Iop_Add32x4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ADD_A.D */
               DIP("ADD_A.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs64x2, getWReg(ws)));
               assign(t2, unop(Iop_Abs64x2, getWReg(wt)));
               assign(t3, binop(Iop_Add64x2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* ADDS_A.df */
         switch (df) {
            case 0x00: { /* ADDS_A.B */
               DIP("ADDS_A.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs8x16, getWReg(ws)));
               assign(t2, unop(Iop_Abs8x16, getWReg(wt)));
               assign(t3, binop(Iop_SarN8x16,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                mkU8(7)));
               assign(t4, binop(Iop_SarN8x16,
                                binop(Iop_AndV128,
                                      mkexpr(t2),
                                      getWReg(wt)),
                                mkU8(7)));
               putWReg(wd, binop(Iop_QAdd8Sx16,
                                 binop(Iop_OrV128,
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t3)),
                                             mkexpr(t1)),
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t1)),
                                             mkexpr(t3))),
                                 binop(Iop_OrV128,
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t4)),
                                             mkexpr(t2)),
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t2)),
                                             mkexpr(t4)))));
               break;
            }

            case 0x01: { /* ADDS_A.H */
               DIP("ADDS_A.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs16x8, getWReg(ws)));
               assign(t2, unop(Iop_Abs16x8, getWReg(wt)));
               assign(t3, binop(Iop_SarN16x8,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                mkU8(15)));
               assign(t4, binop(Iop_SarN16x8,
                                binop(Iop_AndV128,
                                      mkexpr(t2),
                                      getWReg(wt)),
                                mkU8(15)));
               putWReg(wd, binop(Iop_QAdd16Sx8,
                                 binop(Iop_OrV128,
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t3)),
                                             mkexpr(t1)),
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t1)),
                                             mkexpr(t3))),
                                 binop(Iop_OrV128,
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t4)),
                                             mkexpr(t2)),
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t2)),
                                             mkexpr(t4)))));
               break;
            }

            case 0x02: { /* ADDS_A.W */
               DIP("ADDS_A.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs32x4, getWReg(ws)));
               assign(t2, unop(Iop_Abs32x4, getWReg(wt)));
               assign(t3, binop(Iop_SarN32x4,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                mkU8(31)));
               assign(t4, binop(Iop_SarN32x4,
                                binop(Iop_AndV128,
                                      mkexpr(t2),
                                      getWReg(wt)),
                                mkU8(31)));
               putWReg(wd, binop(Iop_QAdd32Sx4,
                                 binop(Iop_OrV128,
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t3)),
                                             mkexpr(t1)),
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t1)),
                                             mkexpr(t3))),
                                 binop(Iop_OrV128,
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t4)),
                                             mkexpr(t2)),
                                       binop(Iop_AndV128,
                                             unop(Iop_NotV128,
                                                  mkexpr(t2)),
                                             mkexpr(t4)))));
               break;
            }

            case 0x03: { /* ADDS_A.D */
               DIP("ADDS_A.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Abs64x2, getWReg(ws)));
               assign(t2, unop(Iop_Abs64x2, getWReg(wt)));
               assign(t3, binop(Iop_SarN64x2,
                                binop(Iop_AndV128,
                                      mkexpr(t1),
                                      getWReg(ws)),
                                mkU8(63)));
               assign(t4, binop(Iop_SarN64x2,
                                binop(Iop_AndV128,
                                      mkexpr(t2),
                                      getWReg(wt)),
                                mkU8(63)));
               putWReg(wd,
                       binop(Iop_QAdd64Sx2,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              mkexpr(t3)),
                                         mkexpr(t1)),
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              mkexpr(t1)),
                                         mkexpr(t3))),
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              mkexpr(t4)),
                                         mkexpr(t2)),
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              mkexpr(t2)),
                                         mkexpr(t4)))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* ADDS_S.df */
         switch (df) {
            case 0x00: { /* ADDS_S.B */
               DIP("ADDS_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ADDS_S.H */
               DIP("ADDS_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ADDS_S.W */
               DIP("ADDS_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ADDS_S.D */
               DIP("ADDS_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd64Sx2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* ADDS_U.df */
         switch (df) {
            case 0x00: { /* ADDS_U.B */
               DIP("ADDS_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ADDS_U.H */
               DIP("ADDS_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ADDS_U.W */
               DIP("ADDS_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ADDS_U.D */
               DIP("ADDS_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QAdd64Ux2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* AVE_S.df */
         switch (df) {
            case 0x00: { /* AVE_S.B */
               DIP("AVE_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add8x16,
                                binop(Iop_Add8x16,
                                      binop(Iop_SarN8x16,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_SarN8x16,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN8x16,
                                      binop(Iop_ShlN8x16,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(7)),
                                      mkU8(7))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* AVE_S.H */
               DIP("AVE_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add16x8,
                            binop(Iop_Add16x8,
                                  binop(Iop_SarN16x8,
                                        mkexpr(t1), mkU8(1)),
                                  binop(Iop_SarN16x8,
                                        mkexpr(t2), mkU8(1))),
                            binop(Iop_ShrN16x8,
                                  binop(Iop_ShlN16x8,
                                        binop(Iop_AndV128,
                                              mkexpr(t1),
                                              mkexpr(t2)),
                                        mkU8(15)),
                                  mkU8(15))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* AVE_S.W */
               DIP("AVE_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add32x4,
                                binop(Iop_Add32x4,
                                      binop(Iop_SarN32x4,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_SarN32x4,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN32x4,
                                      binop(Iop_ShlN32x4,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(31)),
                                      mkU8(31))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* AVE_S.D */
               DIP("AVE_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add64x2,
                                binop(Iop_Add64x2,
                                      binop(Iop_SarN64x2,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_SarN64x2,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN64x2,
                                      binop(Iop_ShlN64x2,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(63)),
                                      mkU8(63))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* AVE_U.df */
         switch (df) {
            case 0x00: { /* AVE_U.B */
               DIP("AVE_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add16x8,
                                binop(Iop_Add8x16,
                                      binop(Iop_ShrN8x16,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_ShrN8x16,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN8x16,
                                      binop(Iop_ShlN8x16,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(7)),
                                      mkU8(7))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* AVE_U.H */
               DIP("AVE_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add16x8,
                                binop(Iop_Add16x8,
                                      binop(Iop_ShrN16x8,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_ShrN16x8,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN16x8,
                                      binop(Iop_ShlN16x8,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(15)),
                                      mkU8(15))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* AVE_U.W */
               DIP("AVE_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add32x4,
                                binop(Iop_Add32x4,
                                      binop(Iop_ShrN32x4,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_ShrN32x4,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN32x4,
                                      binop(Iop_ShlN32x4,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(31)),
                                      mkU8(31))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* AVE_U.D */
               DIP("AVE_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add64x2,
                                binop(Iop_Add64x2,
                                      binop(Iop_ShrN64x2,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_ShrN64x2,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN64x2,
                                      binop(Iop_ShlN64x2,
                                            binop(Iop_AndV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(63)),
                                      mkU8(63))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x06: { /* AVER_S.df */
         switch (df) {
            case 0x00: { /* AVER_S.B */
               DIP("AVER_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Avg8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* AVER_S.H */
               DIP("AVER_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Avg16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* AVER_S.W */
               DIP("AVER_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Avg32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* AVER_S.D */
               DIP("AVER_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add64x2,
                                binop(Iop_Add64x2,
                                      binop(Iop_SarN64x2,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_SarN64x2,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN64x2,
                                      binop(Iop_ShlN64x2,
                                            binop(Iop_OrV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(63)),
                                      mkU8(63))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* AVER_U.df */
         switch (df) {
            case 0x00: { /* AVER_U.B */
               DIP("AVER_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Avg8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* AVER_U.H */
               DIP("AVER_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Avg16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* AVER_U.W */
               DIP("AVER_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Avg32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* AVER_U.D */
               DIP("AVER_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_Add64x2,
                                binop(Iop_Add64x2,
                                      binop(Iop_ShrN64x2,
                                            mkexpr(t1), mkU8(1)),
                                      binop(Iop_ShrN64x2,
                                            mkexpr(t2), mkU8(1))),
                                binop(Iop_ShrN64x2,
                                      binop(Iop_ShlN64x2,
                                            binop(Iop_OrV128,
                                                  mkexpr(t1),
                                                  mkexpr(t2)),
                                            mkU8(63)),
                                      mkU8(63))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_11(UInt cins, UChar wd, UChar ws)   /* 3R (0x11) */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* SUBS_S.df */
         switch (df) {
            case 0x00: { /* SUBS_S.B */
               DIP("SUBS_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub8Sx16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SUBS_S.H */
               DIP("SUBS_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub16Sx8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SUBS_S.W */
               DIP("SUBS_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub32Sx4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SUBS_S.D */
               DIP("SUBS_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub64Sx2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* SUBS_U.df */
         switch (df) {
            case 0x00: { /* SUBS_U.B */
               DIP("SUBS_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub8Ux16, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* SUBS_U.H */
               DIP("SUBS_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub16Ux8, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* SUBS_U.W */
               DIP("SUBS_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub32Ux4, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* SUBS_U.D */
               DIP("SUBS_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QSub64Ux2, mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* SUBSUS_U.df */
         switch (df) {
            case 0x00: { /* SUBSUS_U.B */
               DIP("SUBSUS_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub8x16, getWReg(ws), getWReg(wt)));
               assign(t2, binop(Iop_SarN8x16, getWReg(wt), mkU8(7)));
               assign(t3, binop(Iop_OrV128,
                                binop(Iop_CmpGT8Ux16,
                                      getWReg(ws),
                                      getWReg(wt)),
                                binop(Iop_CmpEQ8x16,
                                      getWReg(ws),
                                      getWReg(wt))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   binop(Iop_XorV128,
                                         mkexpr(t3),
                                         mkexpr(t2)))));
               break;
            }

            case 0x01: { /* SUBSUS_U.H */
               DIP("SUBSUS_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub16x8, getWReg(ws), getWReg(wt)));
               assign(t2, binop(Iop_SarN16x8, getWReg(wt), mkU8(15)));
               assign(t3,
                      binop(Iop_OrV128,
                            binop(Iop_CmpGT16Ux8,
                                  getWReg(ws),
                                  getWReg(wt)),
                            binop(Iop_CmpEQ16x8,
                                  getWReg(ws),
                                  getWReg(wt))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   binop(Iop_XorV128,
                                         mkexpr(t3),
                                         mkexpr(t2)))));
               break;
            }

            case 0x02: { /* SUBSUS_U.W */
               DIP("SUBSUS_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub32x4, getWReg(ws), getWReg(wt)));
               assign(t2, binop(Iop_SarN32x4, getWReg(wt), mkU8(31)));
               assign(t3,
                      binop(Iop_OrV128,
                            binop(Iop_CmpGT32Ux4,
                                  getWReg(ws),
                                  getWReg(wt)),
                            binop(Iop_CmpEQ32x4,
                                  getWReg(ws),
                                  getWReg(wt))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   binop(Iop_XorV128,
                                         mkexpr(t3),
                                         mkexpr(t2)))));
               break;
            }

            case 0x03: { /* SUBSUS_U.D */
               DIP("SUBSUS_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub64x2, getWReg(ws), getWReg(wt)));
               assign(t2, binop(Iop_SarN64x2, getWReg(wt), mkU8(63)));
               assign(t3,
                      binop(Iop_OrV128,
                            binop(Iop_CmpGT64Ux2,
                                  getWReg(ws),
                                  getWReg(wt)),
                            binop(Iop_CmpEQ64x2,
                                  getWReg(ws),
                                  getWReg(wt))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   binop(Iop_XorV128,
                                         mkexpr(t3),
                                         mkexpr(t2)))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* SUBSUU_S.df */
         switch (df) {
            case 0x00: { /* SUBSUU_S.B */
               DIP("SUBSUU_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub8x16, getWReg(ws), getWReg(wt)));
               assign(t2,
                      binop(Iop_SarN8x16,
                            binop (Iop_AndV128,
                                   binop(Iop_XorV128,
                                         getWReg(ws),
                                         getWReg(wt)),
                                   binop(Iop_XorV128,
                                         mkexpr(t1),
                                         getWReg(wt))),
                            mkU8(7)));
               assign(t3,
                      binop(Iop_AndV128,
                            binop(Iop_SarN8x16,
                                  getWReg(ws), mkU8(7)),
                            mkexpr(t2)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   unop(Iop_NotV128,
                                        mkexpr(t2))),
                             binop(Iop_XorV128,
                                   binop(Iop_ShlN8x16,
                                         mkexpr(t2), mkU8(7)),
                                   mkexpr(t3))));
               break;
            }

            case 0x01: { /* SUBSUU_S.H */
               DIP("SUBSUU_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub16x8, getWReg(ws), getWReg(wt)));
               assign(t2,
                      binop(Iop_SarN16x8,
                            binop (Iop_AndV128,
                                   binop(Iop_XorV128,
                                         getWReg(ws),
                                         getWReg(wt)),
                                   binop(Iop_XorV128,
                                         mkexpr(t1),
                                         getWReg(wt))),
                            mkU8(15)));
               assign(t3,
                      binop(Iop_AndV128,
                            binop(Iop_SarN16x8,
                                  getWReg(ws),
                                  mkU8(15)),
                            mkexpr(t2)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   unop(Iop_NotV128,
                                        mkexpr(t2))),
                             binop(Iop_XorV128,
                                   binop(Iop_ShlN16x8,
                                         mkexpr(t2), mkU8(15)),
                                   mkexpr(t3))));
               break;
            }

            case 0x02: { /* SUBSUU_S.W */
               DIP("SUBSUU_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub32x4, getWReg(ws), getWReg(wt)));
               assign(t2,
                      binop(Iop_SarN32x4,
                            binop (Iop_AndV128,
                                   binop(Iop_XorV128,
                                         getWReg(ws),
                                         getWReg(wt)),
                                   binop(Iop_XorV128,
                                         mkexpr(t1),
                                         getWReg(wt))),
                            mkU8(31)));
               assign(t3,
                      binop(Iop_AndV128,
                            binop(Iop_SarN32x4,
                                  getWReg(ws),
                                  mkU8(31)),
                            mkexpr(t2)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   unop(Iop_NotV128,
                                        mkexpr(t2))),
                             binop(Iop_XorV128,
                                   binop(Iop_ShlN32x4,
                                         mkexpr(t2),
                                         mkU8(31)),
                                   mkexpr(t3))));
               break;
            }

            case 0x03: { /* SUBSUU_S.D */
               DIP("SUBSUU_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_Sub64x2, getWReg(ws), getWReg(wt)));
               assign(t2,
                      binop(Iop_SarN64x2,
                            binop (Iop_AndV128,
                                   binop(Iop_XorV128,
                                         getWReg(ws),
                                         getWReg(wt)),
                                   binop(Iop_XorV128,
                                         mkexpr(t1),
                                         getWReg(wt))),
                            mkU8(63)));
               assign(t3,
                      binop(Iop_AndV128,
                            binop(Iop_SarN64x2,
                                  getWReg(ws),
                                  mkU8(63)),
                            mkexpr(t2)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t1),
                                   unop(Iop_NotV128,
                                        mkexpr(t2))),
                             binop(Iop_XorV128,
                                   binop(Iop_ShlN64x2,
                                         mkexpr(t2), mkU8(63)),
                                   mkexpr(t3))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* ASUB_S.df */
         switch (df) {
            case 0x00: { /* ASUB_S.B */
               DIP("ASUB_S.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN8x16, getWReg(ws), mkU8(7)));
               assign(t2, binop(Iop_SarN8x16, getWReg(wt), mkU8(7)));
               assign(t3, binop(Iop_Sub8x16, getWReg(ws), getWReg(wt)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         binop(Iop_AndV128,
                                               unop(Iop_NotV128,
                                                    mkexpr(t1)),
                                               mkexpr(t2)),
                                         mkexpr(t3)),
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              binop(Iop_XorV128,
                                                    mkexpr(t1),
                                                    mkexpr(t2))),
                                         unop(Iop_Abs8x16,
                                              mkexpr(t3)))),
                             binop(Iop_AndV128,
                                   binop(Iop_AndV128,
                                         mkexpr(t1),
                                         unop(Iop_NotV128,
                                              mkexpr(t2))),
                                   binop(Iop_Sub8x16,
                                         getWReg(wt),
                                         getWReg(ws)))));
               break;
            }

            case 0x01: { /* ASUB_S.H */
               DIP("ASUB_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN16x8, getWReg(ws), mkU8(15)));
               assign(t2, binop(Iop_SarN16x8, getWReg(wt), mkU8(15)));
               assign(t3, binop(Iop_Sub16x8, getWReg(ws), getWReg(wt)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         binop(Iop_AndV128,
                                               unop(Iop_NotV128,
                                                    mkexpr(t1)),
                                               mkexpr(t2)),
                                         mkexpr(t3)),
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              binop(Iop_XorV128,
                                                    mkexpr(t1),
                                                    mkexpr(t2))),
                                         unop(Iop_Abs16x8,
                                              mkexpr(t3)))),
                             binop(Iop_AndV128,
                                   binop(Iop_AndV128,
                                         mkexpr(t1),
                                         unop(Iop_NotV128,
                                              mkexpr(t2))),
                                   binop(Iop_Sub16x8,
                                         getWReg(wt),
                                         getWReg(ws)))));
               break;
            }

            case 0x02: { /* ASUB_S.W */
               DIP("ASUB_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN32x4, getWReg(ws), mkU8(31)));
               assign(t2, binop(Iop_SarN32x4, getWReg(wt), mkU8(31)));
               assign(t3, binop(Iop_Sub32x4, getWReg(ws), getWReg(wt)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         binop(Iop_AndV128,
                                               unop(Iop_NotV128,
                                                    mkexpr(t1)),
                                               mkexpr(t2)),
                                         mkexpr(t3)),
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              binop(Iop_XorV128,
                                                    mkexpr(t1),
                                                    mkexpr(t2))),
                                         unop(Iop_Abs32x4,
                                              mkexpr(t3)))),
                             binop(Iop_AndV128,
                                   binop(Iop_AndV128,
                                         mkexpr(t1),
                                         unop(Iop_NotV128,
                                              mkexpr(t2))),
                                   binop(Iop_Sub32x4,
                                         getWReg(wt),
                                         getWReg(ws)))));
               break;
            }

            case 0x03: { /* ASUB_S.D */
               DIP("ASUB_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, binop(Iop_SarN64x2, getWReg(ws), mkU8(63)));
               assign(t2, binop(Iop_SarN64x2, getWReg(wt), mkU8(63)));
               assign(t3, binop(Iop_Sub64x2, getWReg(ws), getWReg(wt)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         binop(Iop_AndV128,
                                               unop(Iop_NotV128,
                                                    mkexpr(t1)),
                                               mkexpr(t2)),
                                         mkexpr(t3)),
                                   binop(Iop_AndV128,
                                         unop(Iop_NotV128,
                                              binop(Iop_XorV128,
                                                    mkexpr(t1),
                                                    mkexpr(t2))),
                                         unop(Iop_Abs64x2,
                                              mkexpr(t3)))),
                             binop(Iop_AndV128,
                                   binop(Iop_AndV128,
                                         mkexpr(t1),
                                         unop(Iop_NotV128,
                                              mkexpr(t2))),
                                   binop(Iop_Sub64x2,
                                         getWReg(wt),
                                         getWReg(ws)))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* ASUB_U.df */
         switch (df) {
            case 0x00: { /* ASUB_U.B */
               DIP("ASUB_U.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_SarN8x16,
                            binop(Iop_XorV128,
                                  mkexpr(t1), mkexpr(t2)),
                            mkU8(7)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128, mkexpr(t3)),
                                   unop(Iop_Abs8x16,
                                        binop(Iop_Sub8x16,
                                              mkexpr(t1),
                                              mkexpr(t2)))),
                             binop(Iop_AndV128, mkexpr(t3),
                                   binop(Iop_Sub8x16,
                                         binop(Iop_Max8Ux16,
                                               mkexpr(t1),
                                               mkexpr(t2)),
                                         binop(Iop_Min8Ux16,
                                               mkexpr(t1),
                                               mkexpr(t2))))));
               break;
            }

            case 0x01: { /* ASUB_U.H */
               DIP("ASUB_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_SarN16x8,
                            binop(Iop_XorV128,
                                  mkexpr(t1), mkexpr(t2)),
                            mkU8(15)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128,
                                        mkexpr(t3)),
                                   unop(Iop_Abs16x8,
                                        binop(Iop_Sub16x8,
                                              mkexpr(t1),
                                              mkexpr(t2)))),
                             binop(Iop_AndV128,
                                   mkexpr(t3),
                                   binop(Iop_Sub16x8,
                                         binop(Iop_Max16Ux8,
                                               mkexpr(t1),
                                               mkexpr(t2)),
                                         binop(Iop_Min16Ux8,
                                               mkexpr(t1),
                                               mkexpr(t2))))));
               break;
            }

            case 0x02: { /* ASUB_U.W */
               DIP("ASUB_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_SarN32x4,
                            binop(Iop_XorV128,
                                  mkexpr(t1), mkexpr(t2)),
                            mkU8(31)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128, mkexpr(t3)),
                                   unop(Iop_Abs32x4,
                                        binop(Iop_Sub32x4,
                                              mkexpr(t1),
                                              mkexpr(t2)))),
                             binop(Iop_AndV128,
                                   mkexpr(t3),
                                   binop(Iop_Sub32x4,
                                         binop(Iop_Max32Ux4,
                                               mkexpr(t1),
                                               mkexpr(t2)),
                                         binop(Iop_Min32Ux4,
                                               mkexpr(t1),
                                               mkexpr(t2))))));
               break;
            }

            case 0x03: { /* ASUB_U.D */
               DIP("ASUB_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_SarN64x2,
                            binop(Iop_XorV128,
                                  mkexpr(t1), mkexpr(t2)),
                            mkU8(63)));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128, mkexpr(t3)),
                                   unop(Iop_Abs64x2,
                                        binop(Iop_Sub64x2,
                                              mkexpr(t1),
                                              mkexpr(t2)))),
                             binop(Iop_AndV128,
                                   mkexpr(t3),
                                   binop(Iop_Sub64x2,
                                         binop(Iop_Max64Ux2,
                                               mkexpr(t1),
                                               mkexpr(t2)),
                                         binop(Iop_Min64Ux2,
                                               mkexpr(t1),
                                               mkexpr(t2))))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_12(UInt cins, UChar wd, UChar ws)   /* 3R (0x12) */
{
   IRTemp t1, t2, t3, t4, t5, t6;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* MULV.df */
         switch (df) {
            case 0x00: { /* MULV.B */
               DIP("MULV.B w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd, binop(Iop_Mul8x16, getWReg(ws), getWReg(wt)));
               break;
            }

            case 0x01: { /* MULV.H */
               DIP("MULV.H w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd, binop(Iop_Mul16x8, getWReg(ws), getWReg(wt)));
               break;
            }

            case 0x02: { /* MULV.W */
               DIP("MULV.W w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd, binop(Iop_Mul32x4, getWReg(ws), getWReg(wt)));
               break;
            }

            case 0x03: { /* MULV.D */
               DIP("MULV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_Mul64,
                                   unop(Iop_V128HIto64,
                                        mkexpr(t1)),
                                   unop(Iop_V128HIto64,
                                        mkexpr(t2))),
                             binop(Iop_Mul64,
                                   unop(Iop_V128to64,
                                        mkexpr(t1)),
                                   unop(Iop_V128to64,
                                        mkexpr(t2)))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* MADDV.df */
         switch (df) {
            case 0x00: { /* MADDV.B */
               DIP("MADDV.B w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_Add8x16,
                             getWReg(wd),
                             binop(Iop_Mul8x16,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* MADDV.H */
               DIP("MADDV.H w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_Add16x8,
                             getWReg(wd),
                             binop(Iop_Mul16x8,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x02: { /* MADDV.W */
               DIP("MADDV.W w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_Add32x4,
                             getWReg(wd),
                             binop(Iop_Mul32x4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x03: { /* MADDV.D */
               DIP("MADDV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               putWReg(wd,
                       binop(Iop_Add64x2,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_Mul64,
                                         unop(Iop_V128HIto64,
                                              mkexpr(t1)),
                                         unop(Iop_V128HIto64,
                                              mkexpr(t2))),
                                   binop(Iop_Mul64,
                                         unop(Iop_V128to64,
                                              mkexpr(t1)),
                                         unop(Iop_V128to64,
                                              mkexpr(t2))))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* MSUBV.df */
         switch (df) {
            case 0x00: { /* MSUBV.B */
               DIP("MSUBV.B w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_Sub8x16,
                             getWReg(wd),
                             binop(Iop_Mul8x16,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* MSUBV.H */
               DIP("MSUBV.H w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_Sub16x8,
                             getWReg(wd),
                             binop(Iop_Mul16x8,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x02: { /* MSUBV.W */
               DIP("MSUBV.W w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_Sub32x4,
                             getWReg(wd),
                             binop(Iop_Mul32x4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x03: { /* MSUBV.D */
               DIP("MSUBV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               putWReg(wd,
                       binop(Iop_Sub64x2,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_Mul64,
                                         unop(Iop_V128HIto64,
                                              mkexpr(t1)),
                                         unop(Iop_V128HIto64,
                                              mkexpr(t2))),
                                   binop(Iop_Mul64,
                                         unop(Iop_V128to64,
                                              mkexpr(t1)),
                                         unop(Iop_V128to64,
                                              mkexpr(t2))))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* DIV_S.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x00: { /* DIV_S.B */
               DIP("DIV_S.B w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[16];
               Int i;

               for (i = 0; i < 16; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFF),
                                     binop(Iop_DivS32,
                                           unop(Iop_8Sto32,
                                                binop(Iop_GetElem8x16,
                                                      mkexpr(t1),
                                                      mkU8(i))),
                                           unop(Iop_8Sto32,
                                                binop(Iop_GetElem8x16,
                                                      mkexpr(t2),
                                                      mkU8(i))))),
                               mkU8((i & 3) << 3)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[15]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[14]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[13]),
                                                     mkexpr(tmp[12])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[11]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[10]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[9]),
                                                     mkexpr(tmp[8]))))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[6]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[5]),
                                                     mkexpr(tmp[4])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[2]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[1]),
                                                     mkexpr(tmp[0]))))))
                      );
               break;
            }

            case 0x01: { /* DIV_S.H */
               DIP("DIV_S.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFFFF),
                                     binop(Iop_DivS32,
                                           unop(Iop_16Sto32,
                                                binop(Iop_GetElem16x8,
                                                      mkexpr(t1),
                                                      mkU8(i))),
                                           unop(Iop_16Sto32,
                                                binop(Iop_GetElem16x8,
                                                      mkexpr(t2),
                                                      mkU8(i))))),
                               mkU8((i & 1) << 4)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* DIV_S.W */
               DIP("DIV_S.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_DivS32,
                               binop(Iop_GetElem32x4,
                                     mkexpr(t1), mkU8(i)),
                               binop(Iop_GetElem32x4,
                                     mkexpr(t2), mkU8(i))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128, \
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* DIV_S.D */
               DIP("DIV_S.D w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_DivS64,
                                   unop(Iop_V128HIto64,
                                        mkexpr(t1)),
                                   unop(Iop_V128HIto64,
                                        mkexpr(t2))),
                             binop(Iop_DivS64,
                                   unop(Iop_V128to64,
                                        mkexpr(t1)),
                                   unop(Iop_V128to64,
                                        mkexpr(t2)))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* DIV_U.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x00: { /* DIV_U.B */
               DIP("DIV_U.B w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[16];
               Int i;

               for (i = 0; i < 16; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFF),
                                     binop(Iop_DivU32,
                                           unop(Iop_8Uto32,
                                                binop(Iop_GetElem8x16,
                                                      mkexpr(t1),
                                                      mkU8(i))),
                                           unop(Iop_8Uto32,
                                                binop(Iop_GetElem8x16,
                                                      mkexpr(t2),
                                                      mkU8(i))))),
                               mkU8((i & 3) << 3)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[15]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[14]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[13]),
                                                     mkexpr(tmp[12])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[11]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[10]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[9]),
                                                     mkexpr(tmp[8]))))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[6]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[5]),
                                                     mkexpr(tmp[4])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[2]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[1]),
                                                     mkexpr(tmp[0]))))))
                      );
               break;
            }

            case 0x01: { /* DIV_U.H */
               DIP("DIV_U.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFFFF),
                                     binop(Iop_DivU32,
                                           unop(Iop_16Uto32,
                                                binop(Iop_GetElem16x8,
                                                      mkexpr(t1),
                                                      mkU8(i))),
                                           unop(Iop_16Uto32,
                                                binop(Iop_GetElem16x8,
                                                      mkexpr(t2),
                                                      mkU8(i))))),
                               mkU8((i & 1) << 4)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* DIV_U.W */
               DIP("DIV_U.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_DivU32,
                               binop(Iop_GetElem32x4,
                                     mkexpr(t1), mkU8(i)),
                               binop(Iop_GetElem32x4,
                                     mkexpr(t2), mkU8(i))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* DIV_U.D */
               DIP("DIV_U.D w%d, w%d, w%d", wd, ws, wt);
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_DivU64,
                                   unop(Iop_V128HIto64,
                                        mkexpr(t1)),
                                   unop(Iop_V128HIto64,
                                        mkexpr(t2))),
                             binop(Iop_DivU64,
                                   unop(Iop_V128to64,
                                        mkexpr(t1)),
                                   unop(Iop_V128to64,
                                        mkexpr(t2)))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x06: { /* MOD_S.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x00: { /* MOD_S.B */
               DIP("MOD_S.B w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[16];
               Int i;

               for (i = 0; i < 16; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFF),
                                     unop(Iop_64HIto32,
                                          binop(Iop_DivModS32to32,
                                                unop(Iop_8Sto32,
                                                     binop(Iop_GetElem8x16,
                                                           mkexpr(t1),
                                                           mkU8(i))),
                                                unop(Iop_8Sto32,
                                                     binop(Iop_GetElem8x16,
                                                           mkexpr(t2),
                                                           mkU8(i)))))),
                               mkU8((i & 3) << 3)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[15]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[14]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[13]),
                                                     mkexpr(tmp[12])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[11]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[10]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[9]),
                                                     mkexpr(tmp[8]))))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[6]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[5]),
                                                     mkexpr(tmp[4])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[2]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[1]),
                                                     mkexpr(tmp[0])))))));
               break;
            }

            case 0x01: { /* MOD_S.H */
               DIP("MOD_S.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFFFF),
                                     unop(Iop_64HIto32,
                                          binop(Iop_DivModS32to32,
                                                unop(Iop_16Sto32,
                                                     binop(Iop_GetElem16x8,
                                                           mkexpr(t1),
                                                           mkU8(i))),
                                                unop(Iop_16Sto32,
                                                     binop(Iop_GetElem16x8,
                                                           mkexpr(t2),
                                                           mkU8(i)))))),
                               mkU8((i & 1) << 4)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* MOD_S.W */
               DIP("MOD_S.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         unop(Iop_64HIto32,
                              binop(Iop_DivModS32to32,
                                    binop(Iop_GetElem32x4,
                                          mkexpr(t1),
                                          mkU8(i)),
                                    binop(Iop_GetElem32x4,
                                          mkexpr(t2),
                                          mkU8(i)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* MOD_S.D */
               DIP("MOD_S.D w%d, w%d, w%d", wd, ws, wt);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I64);
               t5 = newTemp(Ity_I64);
               t6 = newTemp(Ity_I64);
               assign(t3, unop(Iop_V128HIto64, mkexpr(t1)));
               assign(t4, unop(Iop_V128HIto64, mkexpr(t2)));
               assign(t5, unop(Iop_V128to64, mkexpr(t1)));
               assign(t6, unop(Iop_V128to64, mkexpr(t2)));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_Sub64,
                                   mkexpr(t3),
                                   binop(Iop_Mul64,
                                         mkexpr(t4),
                                         binop(Iop_DivS64,
                                               mkexpr(t3),
                                               mkexpr(t4)))),
                             binop(Iop_Sub64,
                                   mkexpr(t5),
                                   binop(Iop_Mul64,
                                         mkexpr(t6),
                                         binop(Iop_DivS64,
                                               mkexpr(t5),
                                               mkexpr(t6))))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* MOD_U.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x00: { /* MOD_U.B */
               DIP("MOD_U.B w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[16];
               Int i;

               for (i = 0; i < 16; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFF),
                                     unop(Iop_64HIto32,
                                          binop(Iop_DivModU32to32,
                                                unop(Iop_8Uto32,
                                                     binop(Iop_GetElem8x16,
                                                           mkexpr(t1),
                                                           mkU8(i))),
                                                unop(Iop_8Uto32,
                                                     binop(Iop_GetElem8x16,
                                                           mkexpr(t2),
                                                           mkU8(i)))))),
                               mkU8((i & 3) << 3)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[15]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[14]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[13]),
                                                     mkexpr(tmp[12])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[11]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[10]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[9]),
                                                     mkexpr(tmp[8]))))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[6]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[5]),
                                                     mkexpr(tmp[4])))),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         binop(Iop_Or32,
                                               mkexpr(tmp[2]),
                                               binop(Iop_Or32,
                                                     mkexpr(tmp[1]),
                                                     mkexpr(tmp[0])))))));
               break;
            }

            case 0x01: { /* MOD_U.H */
               DIP("MOD_U.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Shl32,
                               binop(Iop_And32,
                                     mkU32(0xFFFF),
                                     unop(Iop_64HIto32,
                                          binop(Iop_DivModU32to32,
                                                unop(Iop_16Uto32,
                                                     binop(Iop_GetElem16x8,
                                                           mkexpr(t1),
                                                           mkU8(i))),
                                                unop(Iop_16Uto32,
                                                     binop(Iop_GetElem16x8,
                                                           mkexpr(t2),
                                                           mkU8(i)))))),
                               mkU8((i & 1) << 4)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_Or32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_Or32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* MOD_U.W */
               DIP("MOD_U.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         unop(Iop_64HIto32,
                              binop(Iop_DivModU32to32,
                                    binop(Iop_GetElem32x4,
                                          mkexpr(t1),
                                          mkU8(i)),
                                    binop(Iop_GetElem32x4,
                                          mkexpr(t2),
                                          mkU8(i)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* MOD_U.D */
               DIP("MOD_U.D w%d, w%d, w%d", wd, ws, wt);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I64);
               t5 = newTemp(Ity_I64);
               t6 = newTemp(Ity_I64);
               assign(t3, unop(Iop_V128HIto64, mkexpr(t1)));
               assign(t4, unop(Iop_V128HIto64, mkexpr(t2)));
               assign(t5, unop(Iop_V128to64, mkexpr(t1)));
               assign(t6, unop(Iop_V128to64, mkexpr(t2)));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_Sub64,
                                   mkexpr(t3),
                                   binop(Iop_Mul64,
                                         mkexpr(t4),
                                         binop(Iop_DivU64,
                                               mkexpr(t3),
                                               mkexpr(t4)))),
                             binop(Iop_Sub64,
                                   mkexpr(t5),
                                   binop(Iop_Mul64,
                                         mkexpr(t6),
                                         binop(Iop_DivU64,
                                               mkexpr(t5),
                                               mkexpr(t6))))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_13(UInt cins, UChar wd, UChar ws)   /* 3R (0x13) */
{
   IRTemp t1, t2;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* DOTP_S.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x01: { /* DOTP_S.H */
               DIP("DOTP_S.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_Add16,
                               binop(Iop_MullS8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* DOTP_S.W */
               DIP("DOTP_S.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Add32,
                               binop(Iop_MullS16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* DOTP_S.D */
               DIP("DOTP_S.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_Add64,
                               binop(Iop_MullS32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(tmp[1]), mkexpr(tmp[0])));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* DOTP_U.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x01: { /* DOTP_U.H */
               DIP("DOTP_U.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_Add16,
                               binop(Iop_MullU8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* DOTP_U.W */
               DIP("DOTP_U.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Add32,
                               binop(Iop_MullU16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* DOTP_U.D */
               DIP("DOTP_U.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_Add64,
                               binop(Iop_MullU32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(tmp[1]), mkexpr(tmp[0])));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* DPADD_S.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x01: { /* DPADD_S.H */
               DIP("DPADD_S.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_Add16,
                               binop(Iop_MullS8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Add16x8,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0]))))));
               break;
            }

            case 0x02: { /* DPADD_S.W */
               DIP("DPADD_S.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Add32,
                               binop(Iop_MullS16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Add32x4,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x03: { /* DPADD_S.D */
               DIP("DPADD_S.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_Add64,
                               binop(Iop_MullS32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Add64x2,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* DPADD_U.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x01: { /* DPADD_U.H */
               DIP("DPADD_U.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_Add16,
                               binop(Iop_MullU8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Add16x8,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0]))))));
               break;
            }

            case 0x02: { /* DPADD_U.W */
               DIP("DPADD_U.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Add32,
                               binop(Iop_MullU16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Add32x4,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x03: { /* DPADD_U.D */
               DIP("DPADD_U.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_Add64,
                               binop(Iop_MullU32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Add64x2,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* DPSUB_S.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x01: { /* DPSUB_S.H */
               DIP("DPSUB_S.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_Add16,
                               binop(Iop_MullS8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Sub16x8,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0]))))));
               break;
            }

            case 0x02: { /* DPSUB_S.W */
               DIP("DPSUB_S.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Add32,
                               binop(Iop_MullS16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Sub32x4,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x03: { /* DPSUB_S.D */
               DIP("DPSUB_S.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_Add64,
                               binop(Iop_MullS32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullS32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Sub64x2,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* DPSUB_U.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));

         switch (df) {
            case 0x01: { /* DPSUB_U.H */
               DIP("DPSUB_U.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_Add16,
                               binop(Iop_MullU8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU8,
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Sub16x8,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0]))))));
               break;
            }

            case 0x02: { /* DPSUB_U.W */
               DIP("DPSUB_U.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_Add32,
                               binop(Iop_MullU16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU16,
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem16x8,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Sub32x4,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x03: { /* DPSUB_U.D */
               DIP("DPSUB_U.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_Add64,
                               binop(Iop_MullU32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i))),
                               binop(Iop_MullU32,
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t1),
                                           mkU8(2 * i + 1)),
                                     binop(Iop_GetElem32x4,
                                           mkexpr(t2),
                                           mkU8(2 * i + 1)))));
               }

               putWReg(wd,
                       binop(Iop_Sub64x2,
                             getWReg(wd),
                             binop(Iop_64HLtoV128,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_14(UInt cins, UChar wd, UChar ws)   /* 3R (0x14) */
{
   IRTemp t1, t2, t3, t4;
   IRType ty;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;
   ty = mode64 ? Ity_I64 : Ity_I32;

   switch (operation) {
      case 0x00: { /* SLD.df */
         switch (df) {
            case 0x00: {
               DIP("SLD.B w%d, w%d[%d]", wd, ws, wt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shl32,
                            binop(Iop_And32,
                                  mkNarrowTo32(ty,
                                               getIReg(wt)),
                                  mkU32(15)),
                            mkU8(3)));
               assign(t2,
                      binop(Iop_ShrV128,
                            getWReg(ws),
                            unop(Iop_32to8, mkexpr(t1))));
               assign(t3,
                      binop(Iop_ShlV128,
                            getWReg(wd),
                            unop(Iop_32to8,
                                 binop(Iop_Sub32,
                                       mkU32(128),
                                       mkexpr(t1)))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t2), mkexpr(t3)));
               break;
            }

            case 0x01: {/* SLD.H */
               DIP("SLD.H w%d, w%d[%d]", wd, ws, wt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shl32,
                            binop(Iop_And32,
                                  mkNarrowTo32(ty,
                                               getIReg(wt)),
                                  mkU32(7)),
                            mkU8(3)));
               assign(t2,
                      binop(Iop_32HLto64, mkU32(0), mkexpr(t1)));
               assign(t3,
                      binop(Iop_Shr64x2,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkexpr(t2), mkexpr(t2))));
               assign(t4,
                      binop(Iop_Shl64x2,
                            getWReg(wd),
                            binop(Iop_Sub64x2,
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x40ul),
                                        mkU64(0x40ul)),
                                  binop(Iop_64HLtoV128,
                                        mkexpr(t2),
                                        mkexpr(t2)))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3),
                             IRExpr_ITE(
                                binop(Iop_CmpNE32,
                                      mkexpr(t1), mkU32(0)),
                                mkexpr(t4),
                                binop(Iop_64HLtoV128,
                                      mkU64(0), mkU64(0)))));
               break;
            }

            case 0x02: {/* SLD.W */
               DIP("SLD.W w%d, w%d[%d]", wd, ws, wt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shl32,
                            binop(Iop_And32,
                                  mkNarrowTo32(ty,
                                               getIReg(wt)),
                                  mkU32(3)),
                            mkU8(3)));
               assign(t2,
                      binop(Iop_32HLto64,
                            mkexpr(t1), mkexpr(t1)));
               assign(t3,
                      binop(Iop_Shr32x4,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkexpr(t2), mkexpr(t2))));
               assign(t4,
                      binop(Iop_Shl32x4,
                            getWReg(wd),
                            binop(Iop_Sub32x4,
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x2000000020ul),
                                        mkU64(0x2000000020ul)),
                                  binop(Iop_64HLtoV128,
                                        mkexpr(t2),
                                        mkexpr(t2)))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3),
                             IRExpr_ITE(
                                binop(Iop_CmpNE32,
                                      mkexpr(t1), mkU32(0)),
                                mkexpr(t4),
                                binop(Iop_64HLtoV128,
                                      mkU64(0), mkU64(0)))));
               break;
            }

            case 0x03: { /* SLD.D */
               DIP("SLD.D w%d, w%d[%d]", wd, ws, wt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shl32,
                            binop(Iop_And32,
                                  mkNarrowTo32(ty,
                                               getIReg(wt)),
                                  mkU32(1)),
                            mkU8(3)));
               assign(t2,
                      binop(Iop_32HLto64,
                            binop(Iop_Or32,
                                  mkexpr(t1),
                                  binop(Iop_Shl32,
                                        mkexpr(t1), mkU8(16))),
                            binop(Iop_Or32,
                                  mkexpr(t1),
                                  binop(Iop_Shl32,
                                        mkexpr(t1), mkU8(16)))));
               assign(t3,
                      binop(Iop_Shr16x8,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkexpr(t2), mkexpr(t2))));
               assign(t4,
                      binop(Iop_Shl16x8,
                            getWReg(wd),
                            binop(Iop_Sub16x8,
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x10001000100010ul),
                                        mkU64(0x10001000100010ul)),
                                  binop(Iop_64HLtoV128,
                                        mkexpr(t2),
                                        mkexpr(t2)))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3),
                             IRExpr_ITE(
                                binop(Iop_CmpNE32,
                                      mkexpr(t1), mkU32(0)),
                                mkexpr(t4),
                                binop(Iop_64HLtoV128,
                                      mkU64(0), mkU64(0)))));
               break;
            }
         }

         break;
      }

      case 0x01: { /* SPLAT.df */
         switch (df) {
               Int i;

            case 0x00: { /* SPLAT.B */
               DIP("SPLAT.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I32);
               assign(t1, getWReg(ws));
               assign(t2,
                      mkNarrowTo32(ty, getIReg(wt)));
               IRTemp tmp[16];

               for (i = 0; i < 16; i++) {
                  tmp[i] = newTemp(Ity_I8);
                  assign(tmp[i],
                         binop(Iop_GetElem8x16,
                               mkexpr(t1),
                               unop(Iop_32to8, mkexpr(t2))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[15]),
                                               mkexpr(tmp[14])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[13]),
                                               mkexpr(tmp[12]))),
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[11]),
                                               mkexpr(tmp[10])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[9]),
                                               mkexpr(tmp[8])))),
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0]))))));
               break;
            }

            case 0x01: { /* SPLAT.H */
               DIP("SPLAT.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I32);
               assign(t1, getWReg(ws));
               assign(t2,
                      mkNarrowTo32(ty, getIReg(wt)));
               IRTemp tmp[8];

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         binop(Iop_GetElem16x8,
                               mkexpr(t1),
                               unop(Iop_32to8, mkexpr(t2))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* SPLAT.W */
               DIP("SPLAT.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I32);
               assign(t1, getWReg(ws));
               assign(t2,
                      mkNarrowTo32(ty, getIReg(wt)));
               IRTemp tmp[4];

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         binop(Iop_GetElem32x4,
                               mkexpr(t1),
                               unop(Iop_32to8, mkexpr(t2))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* SPLAT.D */
               DIP("SPLAT.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I32);
               assign(t1, getWReg(ws));
               assign(t2,
                      mkNarrowTo32(ty, getIReg(wt)));
               IRTemp tmp[2];

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         binop(Iop_GetElem64x2,
                               mkexpr(t1),
                               unop(Iop_32to8, mkexpr(t2))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(tmp[1]), mkexpr(tmp[0])));
               break;
            }
         }

         break;
      }

      case 0x02: { /* PCKEV.df */
         switch (df) {
            case 0x00: { /* PCKEV.B */
               DIP("PCKEV.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_PackEvenLanes8x16,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* PCKEV.H */
               DIP("PCKEV.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_PackEvenLanes16x8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* PCKEV.W */
               DIP("PCKEV.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_PackEvenLanes32x4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* PCKEV.D */
               DIP("PCKEV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveLO64x2,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* PCKOD.df */
         switch (df) {
            case 0x00: { /* PCKOD.B */
               DIP("PCKOD.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_PackOddLanes8x16,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* PCKOD.H */
               DIP("PCKOD.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_PackOddLanes16x8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* PCKOD.W */
               DIP("PCKOD.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_PackOddLanes32x4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* PCKOD.D */
               DIP("PCKOD.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveHI64x2,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* ILVL.df */
         switch (df) {
            case 0x00: { /* ILVL.B */
               DIP("ILVL.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveHI8x16,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ILVL.H */
               DIP("ILVL.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveHI16x8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ILVL.W */
               DIP("ILVL.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveHI32x4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ILVL.D */
               DIP("ILVL.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveHI64x2,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* ILVR.df */
         switch (df) {
            case 0x00: { /* ILVL.B */
               DIP("ILVL.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveLO8x16,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ILVL.H */
               DIP("ILVL.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveLO16x8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ILVL.W */
               DIP("ILVL.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveLO32x4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ILVL.D */
               DIP("ILVL.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveLO64x2,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }
         }

         break;
      }

      case 0x06: { /* ILVEV.df */
         switch (df) {
            case 0x00: { /* ILVEV.B */
               DIP("ILVEV.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveEvenLanes8x16,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ILVEV.H */
               DIP("ILVEV.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveEvenLanes16x8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ILVEV.W */
               DIP("ILVEV.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveEvenLanes32x4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ILVEV.D */
               DIP("ILVEV.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveLO64x2,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* ILVOD.df */
         switch (df) {
            case 0x00: { /* ILVOD.B */
               DIP("ILVOD.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveOddLanes8x16,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* ILVOD.H */
               DIP("ILVOD.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveOddLanes16x8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* ILVOD.W */
               DIP("ILVOD.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveOddLanes32x4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* ILVOD.D */
               DIP("ILVOD.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_InterleaveHI64x2,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_15(UInt cins, UChar wd, UChar ws)   /* 3R (0x15) */
{
   IRTemp t1, t2, t3, t4;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03800000) >> 23;
   df = (cins & 0x00600000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* VSHF.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1, getWReg(wd));
         assign(t2, getWReg(ws));
         assign(t3, getWReg(wt));

         switch (df) {
            case 0x00: { /* VSHF.B */
               DIP("VSHF.B w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[16];
               Int i;

               for (i = 0; i < 16; i++) {
                  tmp[i] = newTemp(Ity_I8);
                  assign(tmp[i],
                         IRExpr_ITE(
                            binop(Iop_CmpEQ8,
                                  binop(Iop_And8,
                                        binop(Iop_GetElem8x16,
                                              mkexpr(t1),
                                              mkU8(i)),
                                        mkU8(0xC0)),
                                  mkU8(0x0)),
                            IRExpr_ITE(
                               binop(Iop_CmpEQ8,
                                     binop(Iop_And8,
                                           binop(Iop_GetElem8x16,
                                                 mkexpr(t1),
                                                 mkU8(i)),
                                           mkU8(0x10)),
                                     mkU8(0x0)),
                               binop(Iop_GetElem8x16,
                                     mkexpr(t3),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(i))),
                               binop(Iop_GetElem8x16,
                                     mkexpr(t2),
                                     binop(Iop_GetElem8x16,
                                           mkexpr(t1),
                                           mkU8(i)))),
                            mkU8(0x0)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[15]),
                                               mkexpr(tmp[14])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[13]),
                                               mkexpr(tmp[12]))),
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[11]),
                                               mkexpr(tmp[10])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[9]),
                                               mkexpr(tmp[8])))),
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_8HLto16,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0]))))));
               break;
            }

            case 0x01: { /* VSHF.H */
               DIP("VSHF.H w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[8];
               Int i;

               for (i = 0; i < 8; i++) {
                  tmp[i] = newTemp(Ity_I16);
                  assign(tmp[i],
                         IRExpr_ITE(
                            binop(Iop_CmpEQ16,
                                  binop(Iop_And16,
                                        binop(Iop_GetElem16x8,
                                              mkexpr(t1),
                                              mkU8(i)),
                                        mkU16(0xC0)),
                                  mkU16(0x0)),
                            IRExpr_ITE(
                               binop(Iop_CmpEQ16,
                                     binop(Iop_And16,
                                           binop(Iop_GetElem16x8,
                                                 mkexpr(t1),
                                                 mkU8(i)),
                                           mkU16(0x08)),
                                     mkU16(0x0)),
                               binop(Iop_GetElem16x8,
                                     mkexpr(t3),
                                     unop(Iop_16to8,
                                          binop(Iop_GetElem16x8,
                                                mkexpr(t1),
                                                mkU8(i)))),
                               binop(Iop_GetElem16x8,
                                     mkexpr(t2),
                                     unop(Iop_16to8,
                                          binop(Iop_GetElem16x8,
                                                mkexpr(t1),
                                                mkU8(i))))),
                            mkU16(0x0)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[7]),
                                         mkexpr(tmp[6])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[5]),
                                         mkexpr(tmp[4]))),
                             binop(Iop_32HLto64,
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_16HLto32,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0])))));
               break;
            }

            case 0x02: { /* VSHF.W */
               DIP("VSHF.W w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         IRExpr_ITE(
                            binop(Iop_CmpEQ32,
                                  binop(Iop_And32,
                                        binop(Iop_GetElem32x4,
                                              mkexpr(t1),
                                              mkU8(i)),
                                        mkU32(0xC0)),
                                  mkU32(0x0)),
                            IRExpr_ITE(
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32,
                                           binop(Iop_GetElem32x4,
                                                 mkexpr(t1),
                                                 mkU8(i)),
                                           mkU32(0x04)),
                                     mkU32(0x0)),
                               binop(Iop_GetElem32x4,
                                     mkexpr(t3),
                                     unop(Iop_32to8,
                                          binop(Iop_GetElem32x4,
                                                mkexpr(t1),
                                                mkU8(i)))),
                               binop(Iop_GetElem32x4,
                                     mkexpr(t2),
                                     unop(Iop_32to8,
                                          binop(Iop_GetElem32x4,
                                                mkexpr(t1),
                                                mkU8(i))))),
                            mkU32(0x0)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[3]),
                                   mkexpr(tmp[2])),
                             binop(Iop_32HLto64,
                                   mkexpr(tmp[1]),
                                   mkexpr(tmp[0]))));
               break;
            }

            case 0x03: { /* VSHF.D */
               DIP("VSHF.D w%d, w%d, w%d", wd, ws, wt);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         IRExpr_ITE(
                            binop(Iop_CmpEQ64,
                                  binop(Iop_And64,
                                        binop(Iop_GetElem64x2,
                                              mkexpr(t1),
                                              mkU8(i)),
                                        mkU64(0xC0)),
                                  mkU64(0x0)),
                            IRExpr_ITE(
                               binop(Iop_CmpEQ64,
                                     binop(Iop_And64,
                                           binop(Iop_GetElem64x2,
                                                 mkexpr(t1),
                                                 mkU8(i)),
                                           mkU64(0x02)),
                                     mkU64(0x0)),
                               binop(Iop_GetElem64x2,
                                     mkexpr(t3),
                                     unop(Iop_64to8,
                                          binop(Iop_GetElem64x2,
                                                mkexpr(t1),
                                                mkU8(i)))),
                               binop(Iop_GetElem64x2,
                                     mkexpr(t2),
                                     unop(Iop_64to8,
                                          binop(Iop_GetElem64x2,
                                                mkexpr(t1),
                                                mkU8(i))))),
                            mkU64(0x0)));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(tmp[1]), mkexpr(tmp[0])));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* SRAR.df */
         switch (df) {
            case 0x00: { /* SRAR.B */
               DIP("SRAR.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Sar8x16,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(0x808080808080808ull),
                                  mkU64(0x808080808080808ull)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ8x16,
                                 binop(Iop_ShlN8x16,
                                       getWReg(wt),
                                       mkU8(5)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN8x16,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl8x16,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(7)));
               putWReg(wd,
                       binop(Iop_Add8x16,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            case 0x01: { /* SRAR.H */
               DIP("SRAR.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Sar16x8,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(0x10001000100010ul),
                                  mkU64(0x10001000100010ul)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ16x8,
                                 binop(Iop_ShlN16x8,
                                       getWReg(wt),
                                       mkU8(12)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN16x8,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl16x8,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(15)));
               putWReg(wd,
                       binop(Iop_Add16x8,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            case 0x02: { /* SRAR.W */
               DIP("SRAR.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128); // shifted
               t2 = newTemp(Ity_V128); // 32 - wt
               t3 = newTemp(Ity_V128); // rv
               t4 = newTemp(Ity_V128); // wt % 32 == 0
               assign(t1,
                      binop(Iop_Sar32x4,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(0x2000000020ul),
                                  mkU64(0x2000000020ul)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ32x4,
                                 binop(Iop_ShlN32x4,
                                       getWReg(wt),
                                       mkU8(27)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN32x4,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl32x4,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(31)));
               putWReg(wd,
                       binop(Iop_Add32x4,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            case 0x03: { /* SRAR.D */
               DIP("SRAR.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Sar64x2,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(64ul), mkU64(64ul)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ64x2,
                                 binop(Iop_ShlN64x2,
                                       getWReg(wt),
                                       mkU8(58)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN64x2,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl64x2,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(63)));
               putWReg(wd,
                       binop(Iop_Add64x2,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* SRLR.df */
         switch (df) {
            case 0x00: { /* SRLR.B */
               DIP("SRLR.B w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shr8x16,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub8x16,
                            binop(Iop_64HLtoV128,
                                  mkU64(0x808080808080808ull),
                                  mkU64(0x808080808080808ull)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ8x16,
                                 binop(Iop_ShlN8x16,
                                       getWReg(wt),
                                       mkU8(5)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN8x16,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl8x16,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(7)));
               putWReg(wd,
                       binop(Iop_Add8x16,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            case 0x01: { /* SRLR.H */
               DIP("SRLR.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shr16x8,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub16x8,
                            binop(Iop_64HLtoV128,
                                  mkU64(0x10001000100010ul),
                                  mkU64(0x10001000100010ul)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ16x8,
                                 binop(Iop_ShlN16x8,
                                       getWReg(wt),
                                       mkU8(12)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN16x8,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl16x8,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(15)));
               putWReg(wd,
                       binop(Iop_Add16x8,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            case 0x02: { /* SRLR.W */
               DIP("SRLR.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shr32x4,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub32x4,
                            binop(Iop_64HLtoV128,
                                  mkU64(0x2000000020ul),
                                  mkU64(0x2000000020ul)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ32x4,
                                 binop(Iop_ShlN32x4,
                                       getWReg(wt),
                                       mkU8(27)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN32x4,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl32x4,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(31)));
               putWReg(wd,
                       binop(Iop_Add32x4,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            case 0x03: { /* SRLR.D */
               DIP("SRLR.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_Shr64x2,
                            getWReg(ws),
                            getWReg(wt)));
               assign(t2,
                      binop(Iop_Sub64x2,
                            binop(Iop_64HLtoV128,
                                  mkU64(64ul), mkU64(64ul)),
                            getWReg(wt)));
               assign(t4,
                      unop(Iop_NotV128,
                           binop(Iop_CmpEQ64x2,
                                 binop(Iop_ShlN64x2,
                                       getWReg(wt),
                                       mkU8(58)),
                                 binop(Iop_64HLtoV128,
                                       mkU64(0), mkU64(0)))));
               assign(t3,
                      binop(Iop_ShrN64x2,
                            binop(Iop_AndV128,
                                  binop(Iop_Shl64x2,
                                        getWReg(ws),
                                        mkexpr(t2)),
                                  mkexpr(t4)),
                            mkU8(63)));
               putWReg(wd,
                       binop(Iop_Add64x2,
                             mkexpr(t1), mkexpr(t3)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* HADD_S.df */
         switch (df) {
            case 0x01: { /* HADD_S.H */
               DIP("HADD_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add16x8,
                            binop(Iop_SarN16x8,
                                  mkexpr(t1), mkU8(8)),
                            binop(Iop_SarN16x8,
                                  binop(Iop_ShlN16x8,
                                        mkexpr(t2), mkU8(8)),
                                  mkU8(8))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* HADD_S.W */
               DIP("HADD_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add32x4,
                            binop(Iop_SarN32x4,
                                  mkexpr(t1), mkU8(16)),
                            binop(Iop_SarN32x4,
                                  binop(Iop_ShlN32x4,
                                        mkexpr(t2), mkU8(16)),
                                  mkU8(16))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* HADD_S.D */
               DIP("HADD_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add64x2,
                            binop(Iop_SarN64x2,
                                  mkexpr(t1), mkU8(32)),
                            binop(Iop_SarN64x2,
                                  binop(Iop_ShlN64x2,
                                        mkexpr(t2), mkU8(32)),
                                  mkU8(32))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* HADD_U.df */
         switch (df) {
            case 0x01: { /* HADD_U.H */
               DIP("HADD_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add16x8,
                            binop(Iop_ShrN16x8,
                                  mkexpr(t1), mkU8(8)),
                            binop(Iop_ShrN16x8,
                                  binop(Iop_ShlN16x8,
                                        mkexpr(t2), mkU8(8)),
                                  mkU8(8))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* HADD_U.W */
               DIP("HADD_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add32x4,
                            binop(Iop_ShrN32x4,
                                  mkexpr(t1), mkU8(16)),
                            binop(Iop_ShrN32x4,
                                  binop(Iop_ShlN32x4,
                                        mkexpr(t2), mkU8(16)),
                                  mkU8(16))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* HADD_U.D */
               DIP("HADD_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Add64x2,
                            binop(Iop_ShrN64x2,
                                  mkexpr(t1), mkU8(32)),
                            binop(Iop_ShrN64x2,
                                  binop(Iop_ShlN64x2,
                                        mkexpr(t2), mkU8(32)),
                                  mkU8(32))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x06: { /* HSUB_S.df */
         switch (df) {
            case 0x01: { /* HSUB_S.H */
               DIP("HSUB_S.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Sub16x8,
                            binop(Iop_SarN16x8,
                                  mkexpr(t1), mkU8(8)),
                            binop(Iop_SarN16x8,
                                  binop(Iop_ShlN16x8,
                                        mkexpr(t2), mkU8(8)),
                                  mkU8(8))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* HSUB_S.W */
               DIP("HSUB_S.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Sub32x4,
                            binop(Iop_SarN32x4,
                                  mkexpr(t1), mkU8(16)),
                            binop(Iop_SarN32x4,
                                  binop(Iop_ShlN32x4,
                                        mkexpr(t2), mkU8(16)),
                                  mkU8(16))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* HSUB_S.D */
               DIP("HSUB_S.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Sub64x2,
                            binop(Iop_SarN64x2,
                                  mkexpr(t1), mkU8(32)),
                            binop(Iop_SarN64x2,
                                  binop(Iop_ShlN64x2,
                                        mkexpr(t2), mkU8(32)),
                                  mkU8(32))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* HSUB_U.df */
         switch (df) {
            case 0x01: { /* HSUB_U.H */
               DIP("HSUB_U.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Sub16x8,
                            binop(Iop_ShrN16x8,
                                  mkexpr(t1), mkU8(8)),
                            binop(Iop_ShrN16x8,
                                  binop(Iop_ShlN16x8,
                                        mkexpr(t2), mkU8(8)),
                                  mkU8(8))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x02: { /* HSUB_U.W */
               DIP("HSUB_U.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Sub32x4,
                            binop(Iop_ShrN32x4,
                                  mkexpr(t1), mkU8(16)),
                            binop(Iop_ShrN32x4,
                                  binop(Iop_ShlN32x4,
                                        mkexpr(t2), mkU8(16)),
                                  mkU8(16))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x03: { /* HSUB_U.D */
               DIP("HSUB_U.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_Sub64x2,
                            binop(Iop_ShrN64x2,
                                  mkexpr(t1), mkU8(32)),
                            binop(Iop_ShrN64x2,
                                  binop(Iop_ShlN64x2,
                                        mkexpr(t2), mkU8(32)),
                                  mkU8(32))));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_1A(UInt cins, UChar wd, UChar ws)   /* 3R (0x1A) */
{
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03C00000) >> 22;
   df = (cins & 0x00200000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* FCAF.df */
         switch (df) {
            case 0x00: { /* FCAF.W */
               DIP("FCAF.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCAFW, 2);
               putWReg(wd, binop(Iop_64HLtoV128, mkU64(0ul), mkU64(0ul)));
               break;
            }

            case 0x01: { /* FCAF.D */
               DIP("FCAF.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCAFD, 2);
               putWReg(wd, binop(Iop_64HLtoV128, mkU64(0ul), mkU64(0ul)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* FCUN.df */
         switch (df) {
            case 0x00: { /* FCUN.W */
               DIP("FCUN.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCUNW, 2);
               putWReg(wd, binop(Iop_CmpUN32Fx4,
                                 getWReg(ws),
                                 getWReg(wt)));
               break;
            }

            case 0x01: { /* FCUN.D */
               DIP("FCUN.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCUND, 2);
               putWReg(wd, binop(Iop_CmpUN64Fx2,
                                 getWReg(ws),
                                 getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* FCEQ.df */
         switch (df) {
            case 0x00: { /* FCEQ.W */
               DIP("FCEQ.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCEQW, 2);
               putWReg(wd, binop(Iop_CmpEQ32Fx4,
                                 getWReg(ws),
                                 getWReg(wt)));
               break;
            }

            case 0x01: { /* FCEQ.D */
               DIP("FCEQ.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCEQD, 2);
               putWReg(wd, binop(Iop_CmpEQ64Fx2,
                                 getWReg(ws),
                                 getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* FCUEQ.df */
         switch (df) {
            case 0x00: { /* FCUEQ.W */
               DIP("FCUEQ.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCUEQW, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpEQ32Fx4,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FCUEQ.D */
               DIP("FCUEQ.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCUEQD, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpEQ64Fx2,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* FCLT.df */
         switch (df) {
            case 0x00: { /* FCLT.W */
               DIP("FCLT.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCLTW, 2);
               putWReg(wd,
                       binop(Iop_CmpLT32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FCLT.D */
               DIP("FCLT.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCLTD, 2);
               putWReg(wd,
                       binop(Iop_CmpLT64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* FCULT.df */
         switch (df) {
            case 0x00: { /* FCULT.W */
               DIP("FCULT.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCULTW, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLT32Fx4,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FCULT.D */
               DIP("FCULT.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCULTD, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLT64Fx2,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x06: { /* FCLE.df */
         switch (df) {
            case 0x00: { /* FCLE.W */
               DIP("FCLE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCLEW, 2);
               putWReg(wd,
                       binop(Iop_CmpLE32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FCLE.D */
               DIP("FCLE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCLED, 2);
               putWReg(wd,
                       binop(Iop_CmpLE64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* FCULE.df */
         switch (df) {
            case 0x00: { /* FCULE.W */
               DIP("FCULE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCULEW, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLE32Fx4,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FCULE.D */
               DIP("FCULE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCULED, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLE64Fx2,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x08: { /* FSAF.df */
         switch (df) {
            case 0x00: { /* FSAF.W */
               DIP("FSAF.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSAFW, 2);
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkU64(0ul), mkU64(0ul)));
               break;
            }

            case 0x01: { /* FSAF.D */
               DIP("FSAF.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSAFD, 2);
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkU64(0ul), mkU64(0ul)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x09: { /* FSUN.df */
         switch (df) {
            case 0x00: { /* FSUN.W */
               DIP("FSUN.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUNW, 2);
               putWReg(wd,
                       binop(Iop_CmpUN32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FSUN.D */
               DIP("FSUN.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUND, 2);
               putWReg(wd,
                       binop(Iop_CmpUN64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0A: { /* FSEQ.df */
         switch (df) {
            case 0x00: { /* FSEQ.W */
               DIP("FSEQ.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSEQW, 2);
               putWReg(wd,
                       binop(Iop_CmpEQ32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FSEQ.D */
               DIP("FSEQ.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSEQD, 2);
               putWReg(wd,
                       binop(Iop_CmpEQ64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0B: { /* FSUEQ.df */
         switch (df) {
            case 0x00: { /* FSUEQ.W */
               DIP("FSUEQ.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUEQW, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpEQ32Fx4,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FSUEQ.D */
               DIP("FSUEQ.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUEQD, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpEQ64Fx2,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0C: { /* FSLT.df */
         switch (df) {
            case 0x00: { /* FSLT.W */
               DIP("FSLT.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSLTW, 2);
               putWReg(wd,
                       binop(Iop_CmpLT32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FSLT.D */
               DIP("FSLT.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSLTD, 2);
               putWReg(wd,
                       binop(Iop_CmpLT64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0D: { /* FSULT.df */
         switch (df) {
            case 0x00: { /* FSULT.W */
               DIP("FSULT.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSULTW, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLT32Fx4,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FSULT.D */
               DIP("FSULT.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSULTD, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLT64Fx2,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0E: { /* FSLE.df */
         switch (df) {
            case 0x00: { /* FSLE.W */
               DIP("FSLE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSLEW, 2);
               putWReg(wd,
                       binop(Iop_CmpLE32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FSLE.D */
               DIP("FSLE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSLED, 2);
               putWReg(wd,
                       binop(Iop_CmpLE64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0F: { /* FSULE.df */
         switch (df) {
            case 0x00: { /* FSULE.W */
               DIP("FSULE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSULEW, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLE32Fx4,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FSULE.D */
               DIP("FSULE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSULED, 2);
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_CmpLE64Fx2,
                                   getWReg(ws),
                                   getWReg(wt)),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_1B(UInt cins, UChar wd, UChar ws)   /* 3R (0x1B) */
{
   IRTemp t1, t2, t3, t4;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03C00000) >> 22;
   df = (cins & 0x00200000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* FADD.df */
         switch (df) {
            case 0x00: { /* FADD.W */
               DIP("FADD.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FADDW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Add32Fx4, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FADD.D */
               DIP("FADD.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FADDD, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Add64Fx2, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x01: { /* FSUB.df */
         switch (df) {
            case 0x00: { /* FSUB.W */
               DIP("FSUB.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUBW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Sub32Fx4, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FSUB.D */
               DIP("FSUB.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUBD, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Sub64Fx2, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* FMUL.df */
         switch (df) {
            case 0x00: { /* FMUL.W */
               DIP("FMUL.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMULW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Mul32Fx4, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FMUL.D */
               DIP("FMUL.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMULW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Mul64Fx2, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* FDIV.df */
         switch (df) {
            case 0x00: { /* FDIV.W */
               DIP("FDIV.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FDIVW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Div32Fx4, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FDIV.D */
               DIP("FDIV.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FDIVD, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Div64Fx2, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* FMADD.df */
         switch (df) {
            case 0x00: { /* FMADD.W */
               DIP("FMADD.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMADDW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_F32);
                  assign(tmp[i],
                         qop(Iop_MAddF32, rm,
                             unop(Iop_ReinterpI32asF32,
                                  binop(Iop_GetElem32x4,
                                        getWReg(ws),
                                        mkU8(i))),
                             unop(Iop_ReinterpI32asF32,
                                  binop(Iop_GetElem32x4,
                                        getWReg(wt),
                                        mkU8(i))),
                             unop(Iop_ReinterpI32asF32,
                                  binop(Iop_GetElem32x4,
                                        getWReg(wd),
                                        mkU8(i)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[3])),
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[2]))),
                             binop(Iop_32HLto64,
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[1])),
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[0])))));
               break;
            }

            case 0x01: { /* FMADD.D */
               DIP("FMADD.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMADDW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_F64);
                  assign(tmp[i],
                         qop(Iop_MAddF64, rm,
                             unop(Iop_ReinterpI64asF64,
                                  binop(Iop_GetElem64x2,
                                        getWReg(ws),
                                        mkU8(i))),
                             unop(Iop_ReinterpI64asF64,
                                  binop(Iop_GetElem64x2,
                                        getWReg(wt),
                                        mkU8(i))),
                             unop(Iop_ReinterpI64asF64,
                                  binop(Iop_GetElem64x2,
                                        getWReg(wd),
                                        mkU8(i)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             unop(Iop_ReinterpF64asI64,
                                  mkexpr(tmp[1])),
                             unop(Iop_ReinterpF64asI64,
                                  mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* FMSUB.df */
         switch (df) {
            case 0x00: { /* FMSUB.W */
               DIP("FMSUB.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMADDW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_F32);
                  assign(tmp[i],
                         qop(Iop_MSubF32, rm,
                             unop(Iop_ReinterpI32asF32,
                                  binop(Iop_GetElem32x4,
                                        getWReg(ws),
                                        mkU8(i))),
                             unop(Iop_ReinterpI32asF32,
                                  binop(Iop_GetElem32x4,
                                        getWReg(wt),
                                        mkU8(i))),
                             unop(Iop_ReinterpI32asF32,
                                  binop(Iop_GetElem32x4,
                                        getWReg(wd),
                                        mkU8(i)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[3])),
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[2]))),
                             binop(Iop_32HLto64,
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[1])),
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[0])))));
               break;
            }

            case 0x01: { /* FMSUB.D */
               DIP("FMSUB.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMADDD, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_F64);
                  assign(tmp[i],
                         qop(Iop_MSubF64, rm,
                             unop(Iop_ReinterpI64asF64,
                                  binop(Iop_GetElem64x2,
                                        getWReg(ws),
                                        mkU8(i))),
                             unop(Iop_ReinterpI64asF64,
                                  binop(Iop_GetElem64x2,
                                        getWReg(wt),
                                        mkU8(i))),
                             unop(Iop_ReinterpI64asF64,
                                  binop(Iop_GetElem64x2,
                                        getWReg(wd),
                                        mkU8(i)))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             unop(Iop_ReinterpF64asI64,
                                  mkexpr(tmp[1])),
                             unop(Iop_ReinterpF64asI64,
                                  mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x07: { /* FEXP2.df */
         switch (df) {
            case 0x00: { /* FEXP2.W */
               DIP("FEXP2.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FEXP2W, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Scale2_32Fx4, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FEXP2.D */
               DIP("FEXP2.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FEXP2D, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_Scale2_64Fx2, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x08: { /* FEXDO.df */
         switch (df) {
            case 0x00: { /* FEXDO.H */
               DIP("FEXDO.H w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FEXDOH, 2);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               assign(t1,
                      unop(Iop_F32toF16x4_DEP,
                           getWReg(ws)));
               assign(t2,
                      unop(Iop_F32toF16x4_DEP,
                           getWReg(wt)));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t1), mkexpr(t2)));
               break;
            }

            case 0x01: { /* FEXDO.W */
               DIP("FEXDO.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FEXDOW, 2);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               t4 = newTemp(Ity_I32);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      unop(Iop_ReinterpF32asI32,
                           binop(Iop_F64toF32, rm,
                                 unop(Iop_ReinterpI64asF64,
                                      unop(Iop_V128to64,
                                           getWReg(ws))))));
               assign(t2,
                      unop(Iop_ReinterpF32asI32,
                           binop(Iop_F64toF32, rm,
                                 unop(Iop_ReinterpI64asF64,
                                      unop(Iop_V128HIto64,
                                           getWReg(ws))))));
               assign(t3,
                      unop(Iop_ReinterpF32asI32,
                           binop(Iop_F64toF32, rm,
                                 unop(Iop_ReinterpI64asF64,
                                      unop(Iop_V128to64,
                                           getWReg(wt))))));
               assign(t4,
                      unop(Iop_ReinterpF32asI32,
                           binop(Iop_F64toF32, rm,
                                 unop(Iop_ReinterpI64asF64,
                                      unop(Iop_V128HIto64,
                                           getWReg(wt))))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   mkexpr(t2), mkexpr(t1)),
                             binop(Iop_32HLto64,
                                   mkexpr(t4), mkexpr(t3))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0A: { /* FTQ.df */
         switch (df) {
            case 0x00: { /* FTQ.H */
               DIP("FTQ.H w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FTQH, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_F32x4_2toQ16x8, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FTQ.W */
               DIP("FTQ.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FTQW, 2);
               IRExpr *rm = get_IR_roundingmode_MSA();
               putWReg(wd,
                       triop(Iop_F64x2_2toQ32x4, rm,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0C: { /* FMIN.df */
         switch (df) {
            case 0x00: { /* FMIN.W */
               DIP("FMIN.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMINW, 2);
               putWReg(wd,
                       binop(Iop_Min32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FMIN.D */
               DIP("FMIN.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMINW, 2);
               putWReg(wd,
                       binop(Iop_Min64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0D: { /* FMIN_A.df */
         switch (df) {
            case 0x00: { /* FMIN_A.W */
               DIP("FMIN_A.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMINAW, 2);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_AndV128,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFF7FFFFFFF),
                                  mkU64(0x7FFFFFFF7FFFFFFF))));
               assign(t2,
                      binop(Iop_AndV128,
                            getWReg(wt),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFF7FFFFFFF),
                                  mkU64(0x7FFFFFFF7FFFFFFF))));
               assign(t3,
                      binop(Iop_Min32Fx4,
                            mkexpr(t2), mkexpr(t1)));
               assign(t4,
                      binop(Iop_AndV128,
                            binop(Iop_AndV128,
                                  unop(Iop_NotV128,
                                       binop(Iop_CmpUN32Fx4,
                                             mkexpr(t3),
                                             mkexpr(t3))),
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128,
                                              binop(Iop_CmpEQ32Fx4,
                                                    mkexpr(t1),
                                                    mkexpr(t2)),
                                              binop(Iop_OrV128,
                                                    getWReg(ws),
                                                    getWReg(wt))),
                                        binop(Iop_OrV128,
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN32Fx4,
                                                                mkexpr(t1),
                                                                mkexpr(t1)),
                                                          binop(Iop_CmpLT32Fx4,
                                                                mkexpr(t3),
                                                                mkexpr(t1))),
                                                    getWReg(wt)),
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN32Fx4,
                                                                mkexpr(t2),
                                                                mkexpr(t2)),
                                                          binop(Iop_CmpLT32Fx4,
                                                                mkexpr(t3),
                                                                mkexpr(t2))),
                                                    getWReg(ws))))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x8000000080000000),
                                  mkU64(0x8000000080000000))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3), mkexpr(t4)));
               break;
            }

            case 0x01: { /* FMIN_A.D */
               DIP("FMIN_A.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMINAD, 2);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_AndV128,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFFFFFFFFFF),
                                  mkU64(0x7FFFFFFFFFFFFFFF))));
               assign(t2,
                      binop(Iop_AndV128,
                            getWReg(wt),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFFFFFFFFFF),
                                  mkU64(0x7FFFFFFFFFFFFFFF))));
               assign(t3,
                      binop(Iop_Min64Fx2,
                            mkexpr(t2), mkexpr(t1)));
               assign(t4,
                      binop(Iop_AndV128,
                            binop(Iop_AndV128,
                                  unop(Iop_NotV128,
                                       binop(Iop_CmpUN64Fx2,
                                             mkexpr(t3),
                                             mkexpr(t3))),
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128,
                                              binop(Iop_CmpEQ64Fx2,
                                                    mkexpr(t1),
                                                    mkexpr(t2)),
                                              binop(Iop_OrV128,
                                                    getWReg(ws),
                                                    getWReg(wt))),
                                        binop(Iop_OrV128,
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN64Fx2,
                                                                mkexpr(t1),
                                                                mkexpr(t1)),
                                                          binop(Iop_CmpLT64Fx2,
                                                                mkexpr(t3),
                                                                mkexpr(t1))),
                                                    getWReg(wt)),
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN64Fx2,
                                                                mkexpr(t2),
                                                                mkexpr(t2)),
                                                          binop(Iop_CmpLT64Fx2,
                                                                mkexpr(t3),
                                                                mkexpr(t2))),
                                                    getWReg(ws))))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x8000000000000000),
                                  mkU64(0x8000000000000000))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3), mkexpr(t4)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0E: { /* FMAX.df */
         switch (df) {
            case 0x00: { /* FMAX.W */
               DIP("FMAX.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMAXW, 2);
               putWReg(wd,
                       binop(Iop_Max32Fx4,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            case 0x01: { /* FMAX.D */
               DIP("FMAX.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMAXW, 2);
               putWReg(wd,
                       binop(Iop_Max64Fx2,
                             getWReg(ws),
                             getWReg(wt)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0F: { /* FMAX_A.df */
         switch (df) {
            case 0x00: { /* FMAX_A.W */
               DIP("FMAX_A.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMAXAW, 2);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_AndV128,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFF7FFFFFFF),
                                  mkU64(0x7FFFFFFF7FFFFFFF))));
               assign(t2,
                      binop(Iop_AndV128,
                            getWReg(wt),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFF7FFFFFFF),
                                  mkU64(0x7FFFFFFF7FFFFFFF))));
               assign(t3,
                      binop(Iop_Max32Fx4,
                            mkexpr(t2), mkexpr(t1)));
               assign(t4,
                      binop(Iop_AndV128,
                            binop(Iop_AndV128,
                                  unop(Iop_NotV128,
                                       binop(Iop_CmpUN32Fx4,
                                             mkexpr(t3),
                                             mkexpr(t3))),
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128,
                                              binop(Iop_CmpEQ32Fx4,
                                                    mkexpr(t1),
                                                    mkexpr(t2)),
                                              binop(Iop_AndV128,
                                                    getWReg(ws),
                                                    getWReg(wt))),
                                        binop(Iop_OrV128,
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN32Fx4,
                                                                mkexpr(t1),
                                                                mkexpr(t1)),
                                                          binop(Iop_CmpLT32Fx4,
                                                                mkexpr(t1),
                                                                mkexpr(t3))),
                                                    getWReg(wt)),
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN32Fx4,
                                                                mkexpr(t2),
                                                                mkexpr(t2)),
                                                          binop(Iop_CmpLT32Fx4,
                                                                mkexpr(t2),
                                                                mkexpr(t3))),
                                                    getWReg(ws))))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x8000000080000000),
                                  mkU64(0x8000000080000000))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3), mkexpr(t4)));
               break;
            }

            case 0x01: { /* FMAX_A.D */
               DIP("FMAX_A.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FMAXAD, 2);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               assign(t1,
                      binop(Iop_AndV128,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFFFFFFFFFF),
                                  mkU64(0x7FFFFFFFFFFFFFFF))));
               assign(t2,
                      binop(Iop_AndV128,
                            getWReg(wt),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FFFFFFFFFFFFFFF),
                                  mkU64(0x7FFFFFFFFFFFFFFF))));
               assign(t3,
                      binop(Iop_Max64Fx2,
                            mkexpr(t2), mkexpr(t1)));
               assign(t4,
                      binop(Iop_AndV128,
                            binop(Iop_AndV128,
                                  unop(Iop_NotV128,
                                       binop(Iop_CmpUN64Fx2,
                                             mkexpr(t3),
                                             mkexpr(t3))),
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128,
                                              binop(Iop_CmpEQ64Fx2,
                                                    mkexpr(t1),
                                                    mkexpr(t2)),
                                              binop(Iop_AndV128,
                                                    getWReg(ws),
                                                    getWReg(wt))),
                                        binop(Iop_OrV128,
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN64Fx2,
                                                                mkexpr(t1),
                                                                mkexpr(t1)),
                                                          binop(Iop_CmpLT64Fx2,
                                                                mkexpr(t1),
                                                                mkexpr(t3))),
                                                    getWReg(wt)),
                                              binop(Iop_AndV128,
                                                    binop(Iop_OrV128,
                                                          binop(Iop_CmpUN64Fx2,
                                                                mkexpr(t2),
                                                                mkexpr(t2)),
                                                          binop(Iop_CmpLT64Fx2,
                                                                mkexpr(t2),
                                                                mkexpr(t3))),
                                                    getWReg(ws))))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x8000000000000000),
                                  mkU64(0x8000000000000000))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t3), mkexpr(t4)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_3R_1C(UInt cins, UChar wd, UChar ws)   /* 3R (0x1C) */
{
   IRTemp t1, t2, t3, t4, t5, t6;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03C00000) >> 22;
   df = (cins & 0x00200000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x01: { /* FCOR.df */
         switch (df) {
            case 0x00: { /* FCOR.W */
               DIP("FCOR.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCORW, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpUN32Fx4,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            case 0x01: { /* FCOR.D */
               DIP("FCOR.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCORD, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpUN64Fx2,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x02: { /* FCUNE.df */
         switch (df) {
            case 0x00: { /* FCUNE.W */
               DIP("FCUNE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCUNEW, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpEQ32Fx4,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            case 0x01: { /* FCUNE.D */
               DIP("FCUNE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCUNED, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpEQ64Fx2,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x03: { /* FCNE.df */
         switch (df) {
            case 0x00: { /* FCNE.W */
               DIP("FCNE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCNEW, 2);
               putWReg(wd,
                       binop(Iop_XorV128,
                             unop(Iop_NotV128,
                                  binop(Iop_CmpEQ32Fx4,
                                        getWReg(ws),
                                        getWReg(wt))),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FCNE.D */
               DIP("FCNE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FCNED, 2);
               putWReg(wd,
                       binop(Iop_XorV128,
                             unop(Iop_NotV128,
                                  binop(Iop_CmpEQ64Fx2,
                                        getWReg(ws),
                                        getWReg(wt))),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x04: { /* MUL_Q.df */
         switch (df) {
            case 0x00: { /* MUL_Q.H */
               DIP("MUL_Q.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_QDMulHi16Sx8,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MUL_Q.W */
               DIP("MUL_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3,
                      binop(Iop_QDMulHi32Sx4,
                            mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x05: { /* MADD_Q.df */
         switch (df) {
            case 0x00: { /* MADD_Q.W */
               DIP("MADD_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(16)));
               assign(t2, // odd
                      binop(Iop_SarN32x4,
                            getWReg(ws), mkU8(16)));
               assign(t3, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(16)));
               assign(t4, // odd
                      binop(Iop_SarN32x4,
                            getWReg(wt), mkU8(16)));
               assign(t5,
                      binop(Iop_Add32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        binop(Iop_InterleaveEvenLanes16x8,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t1), mkexpr(t3))));
               assign(t6,
                      binop(Iop_Add32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        getWReg(wd),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t2), mkexpr(t4))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes16x8,
                             binop(Iop_QandQSarNnarrow32Sto16Sx4,
                                   mkexpr(t6), mkU8(15)),
                             binop(Iop_QandQSarNnarrow32Sto16Sx4,
                                   mkexpr(t5), mkU8(15))));
               break;
            }

            case 0x01: { /* MADD_Q.W */
               DIP("MADD_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(32)));
               assign(t2, // odd
                      binop(Iop_SarN64x2,
                            getWReg(ws), mkU8(32)));
               assign(t3, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(32)));
               assign(t4, // odd
                      binop(Iop_SarN64x2,
                            getWReg(wt), mkU8(32)));
               assign(t5,
                      binop(Iop_Add64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        binop(Iop_InterleaveEvenLanes32x4,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t1)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t3))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t1)),
                                        unop(Iop_V128to64,
                                             mkexpr(t3))))));
               assign(t6,
                      binop(Iop_Add64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        getWReg(wd),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t2)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t4))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t2)),
                                        unop(Iop_V128to64,
                                             mkexpr(t4))))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes32x4,
                             binop(Iop_QandQSarNnarrow64Sto32Sx2,
                                   mkexpr(t6), mkU8(31)),
                             binop(Iop_QandQSarNnarrow64Sto32Sx2,
                                   mkexpr(t5), mkU8(31))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x06: { /* MSUB_Q.df */
         switch (df) {
            case 0x00: { /* MSUB_Q.H */
               DIP("MSUB_Q.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(16)));
               assign(t2, // odd
                      binop(Iop_SarN32x4,
                            getWReg(ws), mkU8(16)));
               assign(t3, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(16)));
               assign(t4, // odd
                      binop(Iop_SarN32x4,
                            getWReg(wt), mkU8(16)));
               assign(t5,
                      binop(Iop_Sub32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        binop(Iop_InterleaveEvenLanes16x8,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t1), mkexpr(t3))));
               assign(t6,
                      binop(Iop_Sub32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        getWReg(wd),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t2), mkexpr(t4))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes16x8,
                             binop(Iop_QandQSarNnarrow32Sto16Sx4,
                                   mkexpr(t6), mkU8(15)),
                             binop(Iop_QandQSarNnarrow32Sto16Sx4,
                                   mkexpr(t5), mkU8(15))));
               break;
            }

            case 0x01: { /* MSUB_Q.W */
               DIP("MSUB_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(32)));
               assign(t2, // odd
                      binop(Iop_SarN64x2,
                            getWReg(ws), mkU8(32)));
               assign(t3, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(32)));
               assign(t4, // odd
                      binop(Iop_SarN64x2,
                            getWReg(wt), mkU8(32)));
               assign(t5,
                      binop(Iop_Sub64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        binop(Iop_InterleaveEvenLanes32x4,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t1)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t3))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t1)),
                                        unop(Iop_V128to64,
                                             mkexpr(t3))))));
               assign(t6,
                      binop(Iop_Sub64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        getWReg(wd),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t2)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t4))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t2)),
                                        unop(Iop_V128to64,
                                             mkexpr(t4))))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes32x4,
                             binop(Iop_QandQSarNnarrow64Sto32Sx2,
                                   mkexpr(t6), mkU8(31)),
                             binop(Iop_QandQSarNnarrow64Sto32Sx2,
                                   mkexpr(t5), mkU8(31))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x09: { /* FSOR.df */
         switch (df) {
            case 0x00: { /* FSOR.W */
               DIP("FSOR.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSORW, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpUN32Fx4,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            case 0x01: { /* FSOR.D */
               DIP("FSOR.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSORD, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpUN64Fx2,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0A: { /* FSUNE.df */
         switch (df) {
            case 0x00: { /* FSUNE.W */
               DIP("FSUNE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUNEW, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpEQ32Fx4,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            case 0x01: { /* FSUNE.D */
               DIP("FSUNE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSUNED, 2);
               putWReg(wd,
                       unop(Iop_NotV128,
                            binop(Iop_CmpEQ64Fx2,
                                  getWReg(ws),
                                  getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0B: { /* FSNE.df */
         switch (df) {
            case 0x00: { /* FSNE.W */
               DIP("FSNE.W w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSNEW, 2);
               putWReg(wd,
                       binop(Iop_XorV128,
                             unop(Iop_NotV128,
                                  binop(Iop_CmpEQ32Fx4,
                                        getWReg(ws),
                                        getWReg(wt))),
                             binop(Iop_CmpUN32Fx4,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            case 0x01: { /* FSNE.D */
               DIP("FSNE.D w%d, w%d, w%d", wd, ws, wt);
               calculateMSACSR(ws, wt, FSNED, 2);
               putWReg(wd,
                       binop(Iop_XorV128,
                             unop(Iop_NotV128,
                                  binop(Iop_CmpEQ64Fx2,
                                        getWReg(ws),
                                        getWReg(wt))),
                             binop(Iop_CmpUN64Fx2,
                                   getWReg(ws),
                                   getWReg(wt))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0C: { /* MULR_Q.df */
         switch (df) {
            case 0x00: { /* MULR_Q.H */
               DIP("MULR_Q.H w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QRDMulHi16Sx8,
                                mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            case 0x01: { /* MULR_Q.W */
               DIP("MULR_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, getWReg(ws));
               assign(t2, getWReg(wt));
               assign(t3, binop(Iop_QRDMulHi32Sx4,
                                mkexpr(t1), mkexpr(t2)));
               putWReg(wd, mkexpr(t3));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0D: { /* MADDR_Q.df */
         switch (df) {
            case 0x00: { /* MADDR_Q.W */
               DIP("MADDR_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(16)));
               assign(t2, // odd
                      binop(Iop_SarN32x4,
                            getWReg(ws), mkU8(16)));
               assign(t3, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(16)));
               assign(t4, // odd
                      binop(Iop_SarN32x4,
                            getWReg(wt), mkU8(16)));
               assign(t5,
                      binop(Iop_Add32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        binop(Iop_InterleaveEvenLanes16x8,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t1), mkexpr(t3))));
               assign(t6,
                      binop(Iop_Add32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        getWReg(wd),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t2), mkexpr(t4))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes16x8,
                             binop(Iop_QandQRSarNnarrow32Sto16Sx4,
                                   mkexpr(t6), mkU8(15)),
                             binop(Iop_QandQRSarNnarrow32Sto16Sx4,
                                   mkexpr(t5), mkU8(15))));
               break;
            }

            case 0x01: { /* MADDR_Q.D */
               DIP("MADDR_Q.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(32)));
               assign(t2, // odd
                      binop(Iop_SarN64x2,
                            getWReg(ws), mkU8(32)));
               assign(t3, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(32)));
               assign(t4, // odd
                      binop(Iop_SarN64x2,
                            getWReg(wt), mkU8(32)));
               assign(t5,
                      binop(Iop_Add64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        binop(Iop_InterleaveEvenLanes32x4,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t1)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t3))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t1)),
                                        unop(Iop_V128to64,
                                             mkexpr(t3))))));
               assign(t6,
                      binop(Iop_Add64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        getWReg(wd),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t2)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t4))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t2)),
                                        unop(Iop_V128to64,
                                             mkexpr(t4))))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes32x4,
                             binop(Iop_QandQRSarNnarrow64Sto32Sx2,
                                   mkexpr(t6), mkU8(31)),
                             binop(Iop_QandQRSarNnarrow64Sto32Sx2,
                                   mkexpr(t5), mkU8(31))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x0E: { /* MSUBR_Q.df */
         switch (df) {
            case 0x00: { /* MSUBR_Q.W */
               DIP("MSUBR_Q.W w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(16)));
               assign(t2, // odd
                      binop(Iop_SarN32x4,
                            getWReg(ws), mkU8(16)));
               assign(t3, // even
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveEvenLanes16x8,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(16)));
               assign(t4, // odd
                      binop(Iop_SarN32x4,
                            getWReg(wt), mkU8(16)));
               assign(t5,
                      binop(Iop_Sub32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        binop(Iop_InterleaveEvenLanes16x8,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t1), mkexpr(t3))));
               assign(t6,
                      binop(Iop_Sub32x4,
                            binop(Iop_ShlN32x4,
                                  binop(Iop_SarN32x4,
                                        getWReg(wd),
                                        mkU8(16)),
                                  mkU8(15)),
                            binop(Iop_Mul32x4,
                                  mkexpr(t2), mkexpr(t4))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes16x8,
                             binop(Iop_QandQRSarNnarrow32Sto16Sx4,
                                   mkexpr(t6), mkU8(15)),
                             binop(Iop_QandQRSarNnarrow32Sto16Sx4,
                                   mkexpr(t5), mkU8(15))));
               break;
            }

            case 0x01: { /* MSUBR_Q.D */
               DIP("MSUBR_Q.D w%d, w%d, w%d", wd, ws, wt);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               t5 = newTemp(Ity_V128);
               t6 = newTemp(Ity_V128);
               assign(t1, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(32)));
               assign(t2, // odd
                      binop(Iop_SarN64x2,
                            getWReg(ws), mkU8(32)));
               assign(t3, // even
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveEvenLanes32x4,
                                  getWReg(wt),
                                  getWReg(wt)),
                            mkU8(32)));
               assign(t4, // odd
                      binop(Iop_SarN64x2,
                            getWReg(wt), mkU8(32)));
               assign(t5,
                      binop(Iop_Sub64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        binop(Iop_InterleaveEvenLanes32x4,
                                              getWReg(wd),
                                              getWReg(wd)),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t1)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t3))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t1)),
                                        unop(Iop_V128to64,
                                             mkexpr(t3))))));
               assign(t6,
                      binop(Iop_Sub64x2,
                            binop(Iop_ShlN64x2,
                                  binop(Iop_SarN64x2,
                                        getWReg(wd),
                                        mkU8(32)),
                                  mkU8(31)),
                            binop(Iop_64HLtoV128,
                                  binop(Iop_Mul64,
                                        unop(Iop_V128HIto64,
                                             mkexpr(t2)),
                                        unop(Iop_V128HIto64,
                                             mkexpr(t4))),
                                  binop(Iop_Mul64,
                                        unop(Iop_V128to64,
                                             mkexpr(t2)),
                                        unop(Iop_V128to64,
                                             mkexpr(t4))))));
               putWReg(wd,
                       binop(Iop_InterleaveEvenLanes32x4,
                             binop(Iop_QandQRSarNnarrow64Sto32Sx2,
                                   mkexpr(t6), mkU8(31)),
                             binop(Iop_QandQRSarNnarrow64Sto32Sx2,
                                   mkexpr(t5), mkU8(31))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_ELM(UInt cins, UChar wd, UChar ws)   /* ELM (0x19) */
{
   IRTemp t1, t2, t3, t4, t5;
   IRType ty;
   UShort operation;
   UChar df, n;

   operation = (cins & 0x03C00000) >> 22;
   ty = mode64 ? Ity_I64 : Ity_I32;

   switch ((cins & 0x03FF0000) >> 16) {
      case 0x07E: /* CFCMSA */
         DIP("CFCMSA r%d, c%d", wd, ws);

         switch (ws) {
            case 0: { /* MSAIR */
               IRDirty *d;
               t1 = newTemp(Ity_I32);
               /* IRExpr_BBPTR() =>
                                    Need to pass pointer to
                                          guest state to helper. */
               d = unsafeIRDirty_1_N(t1, 0,
                                     "mips_dirtyhelper_get_MSAIR",
                                     &mips_dirtyhelper_get_MSAIR,
                                     mkIRExprVec_0());
               /* d->nFxState = 0; */
               stmt(IRStmt_Dirty(d));
               putIReg(wd,
                       mkWidenFrom32(ty, mkexpr(t1), True));
               break;
            }

            case 1: /* MSACSR */
               putIReg(wd,
                       mkWidenFrom32(ty, getMSACSR(), True));
               break;

            default:
               putIReg(wd,
                       mkWidenFrom32(ty, mkU32(0), False));
               break;
         }

         break;

      case 0x03E: /* CTCMSA */
         DIP("CTCMSA r%d, c%d", ws, wd);

         if (wd == 1) { /* MSACSR */
            putMSACSR(
               binop(Iop_And32, mkNarrowTo32(ty, getIReg(ws)),
                     mkU32(0x1FFFFFF)));
         }

         break;

      case 0x0BE: /* MOVE.V */
         DIP("MOVE.V w%d, w%d", ws, wd);
         putWReg(wd, getWReg(ws));
         break;

      default:
         df = (cins & 0x003F0000) >> 16;

         if ((df & 0x38) == 0x38) {        // 11100n; dw
            n = df & 0x01;
            df = 0x38;
         } else if ((df & 0x30) == 0x30) { // 1100nn; w
            n = df & 0x03;
            df = 0x30;
         } else if ((df & 0x20) == 0x20) { // 100nnn; hw
            n = df & 0x07;
            df = 0x20;
         } else if ((df & 0x00) == 0x00) { // 00nnnn; b
            n = df & 0x0F;
            df = 0x00;
         }

         switch (operation) {
            case 0x00: /* SLDI.df */
               switch (df) {
                  case 0x00: /* SLDI.B */
                     DIP("SLDI.B w%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     assign(t1,
                            binop(Iop_ShrV128,
                                  getWReg(ws),
                                  mkU8(n << 3)));
                     assign(t2,
                            binop(Iop_ShlV128,
                                  getWReg(wd),
                                  mkU8(n ?
                                       (16 - n) << 3 : 0)));
                     putWReg(wd,
                             binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
                     break;

                  case 0x20: /* SLDI.H */
                     DIP("SLDI.H w%d, w%d[%d]", wd, ws, n);

                     if (n == 0) {
                        putWReg(wd, getWReg(ws));
                     } else {
                        t1 = newTemp(Ity_V128);
                        t2 = newTemp(Ity_V128);
                        assign(t1,
                               binop(Iop_ShrN64x2,
                                     getWReg(ws),
                                     mkU8(n << 3)));
                        assign(t2,
                               binop(Iop_ShlN64x2,
                                     getWReg(wd),
                                     mkU8((8 - n) << 3)));
                        putWReg(wd,
                                binop(Iop_OrV128,
                                      mkexpr(t1),
                                      mkexpr(t2)));
                     }

                     break;

                  case 0x30: /* SLDI.W */
                     DIP("SLDI.W w%d, w%d[%d]", wd, ws, n);

                     if (n == 0) {
                        putWReg(wd, getWReg(ws));
                     } else {
                        t1 = newTemp(Ity_V128);
                        t2 = newTemp(Ity_V128);
                        assign(t1,
                               binop(Iop_ShrN32x4,
                                     getWReg(ws),
                                     mkU8(n << 3)));
                        assign(t2,
                               binop(Iop_ShlN32x4,
                                     getWReg(wd),
                                     mkU8((4 - n) << 3)));
                        putWReg(wd,
                                binop(Iop_OrV128,
                                      mkexpr(t1),
                                      mkexpr(t2)));
                     }

                     break;

                  case 0x38:  /* SLDI.D */
                     DIP("SLDI.D w%d, w%d[%d]", wd, ws, n);

                     if (n == 0) {
                        putWReg(wd, getWReg(ws));
                     } else {
                        t1 = newTemp(Ity_V128);
                        t2 = newTemp(Ity_V128);
                        assign(t1,
                               binop(Iop_ShrN16x8,
                                     getWReg(ws),
                                     mkU8(n << 3)));
                        assign(t2,
                               binop(Iop_ShlN16x8,
                                     getWReg(wd),
                                     mkU8((2 - n) << 3)));
                        putWReg(wd,
                                binop(Iop_OrV128,
                                      mkexpr(t1),
                                      mkexpr(t2)));
                     }

                     break;

                  default:
                     return -1;
               }

               break;

            case 0x01: /* SPLATI.df */
               switch (df) {
                  case 0x00: { /* SPLATI.B */
                     DIP("SPLATI.B w%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     t3 = newTemp(Ity_V128);
                     t4 = newTemp(Ity_V128);

                     if (n & 1)
                        assign(t1,
                               binop(Iop_InterleaveOddLanes8x16,
                                     getWReg(ws),
                                     getWReg(ws)));
                     else
                        assign(t1,
                               binop(Iop_InterleaveEvenLanes8x16,
                                     getWReg(ws),
                                     getWReg(ws)));

                     n /= 2;

                     if (n & 1)
                        assign(t2,
                               binop(Iop_InterleaveOddLanes16x8,
                                     mkexpr(t1), mkexpr(t1)));
                     else
                        assign(t2,
                               binop(Iop_InterleaveEvenLanes16x8,
                                     mkexpr(t1), mkexpr(t1)));

                     n /= 2;

                     if (n & 1)
                        assign(t3,
                               binop(Iop_InterleaveOddLanes32x4,
                                     mkexpr(t2), mkexpr(t2)));
                     else
                        assign(t3,
                               binop(Iop_InterleaveEvenLanes32x4,
                                     mkexpr(t2), mkexpr(t2)));

                     n /= 2;

                     if (n & 1)
                        assign(t4,
                               binop(Iop_InterleaveHI64x2,
                                     mkexpr(t3), mkexpr(t3)));
                     else
                        assign(t4,
                               binop(Iop_InterleaveLO64x2,
                                     mkexpr(t3), mkexpr(t3)));

                     putWReg(wd, mkexpr(t4));
                     break;
                  }

                  case 0x20: { /* SPLATI.H */
                     DIP("SPLATI.H w%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     t3 = newTemp(Ity_V128);

                     if (n & 1)
                        assign(t1,
                               binop(Iop_InterleaveOddLanes16x8,
                                     getWReg(ws),
                                     getWReg(ws)));
                     else
                        assign(t1,
                               binop(Iop_InterleaveEvenLanes16x8,
                                     getWReg(ws),
                                     getWReg(ws)));

                     n /= 2;

                     if (n & 1)
                        assign(t2,
                               binop(Iop_InterleaveOddLanes32x4,
                                     mkexpr(t1), mkexpr(t1)));
                     else
                        assign(t2,
                               binop(Iop_InterleaveEvenLanes32x4,
                                     mkexpr(t1), mkexpr(t1)));

                     n /= 2;

                     if (n & 1)
                        assign(t3,
                               binop(Iop_InterleaveHI64x2,
                                     mkexpr(t2), mkexpr(t2)));
                     else
                        assign(t3,
                               binop(Iop_InterleaveLO64x2,
                                     mkexpr(t2), mkexpr(t2)));

                     putWReg(wd, mkexpr(t3));
                     break;
                  }

                  case 0x30: { /* SPLATI.W */
                     DIP("SPLATI.W w%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     t3 = newTemp(Ity_V128);
                     assign(t1, getWReg(ws));

                     if (n & 1)
                        assign(t2,
                               binop(Iop_InterleaveOddLanes32x4,
                                     mkexpr(t1), mkexpr(t1)));
                     else
                        assign(t2,
                               binop(Iop_InterleaveEvenLanes32x4,
                                     mkexpr(t1), mkexpr(t1)));

                     n /= 2;

                     if (n & 1)
                        assign(t3,
                               binop(Iop_InterleaveHI64x2,
                                     mkexpr(t2), mkexpr(t2)));
                     else
                        assign(t3,
                               binop(Iop_InterleaveLO64x2,
                                     mkexpr(t2), mkexpr(t2)));

                     putWReg(wd, mkexpr(t3));
                     break;
                  }

                  case 0x38: /* SPLATI.D */
                     DIP("SPLATI.D w%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_V128);
                     t3 = newTemp(Ity_V128);
                     assign(t1, getWReg(ws));

                     if (n)
                        assign(t3,
                               binop(Iop_InterleaveHI64x2,
                                     mkexpr(t1), mkexpr(t1)));
                     else
                        assign(t3,
                               binop(Iop_InterleaveLO64x2,
                                     mkexpr(t1), mkexpr(t1)));

                     putWReg(wd, mkexpr(t3));
                     break;

                  default:
                     return -1;
               }

               break;

            case 0x02: /* COPY_S.df */
               switch (df) {
                  case 0x00: /* COPY_S.B */
                     DIP("COPY_S.B r%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_I8);

                     switch (n) {
                        case 0:
                           assign(t1,
                                  unop(Iop_32to8,
                                       unop(Iop_V128to32,
                                            getWReg(ws))));
                           break;

                        case 1:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_V128to32,
                                                 getWReg(ws)))));
                           break;

                        case 2:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 3:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 4:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 5:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 6:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 7:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 8:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 9:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 10:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 11:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 12:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 13:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 14:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 15:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;
                     }

                     putIReg(wd,
                             unop(mode64 ? Iop_8Sto64 : Iop_8Sto32,
                                  mkexpr(t1)));
                     break;

                  case 0x20: /* COPY_S.H */
                     DIP("COPY_S.H r%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_I16);

                     switch (n) {
                        case 0:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 1:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 2:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 3:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 4:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;

                        case 5:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;

                        case 6:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;

                        case 7:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;
                     }

                     putIReg(wd,
                             unop(mode64 ? Iop_16Sto64 : Iop_16Sto32,
                                  mkexpr(t1)));
                     break;

                  case 0x30: /* COPY_S.W */
                     DIP("COPY_S.W r%d, w%d[%d]", wd, ws, n);

                     switch (n) {
                        case 0:
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_V128to32,
                                                      getWReg(ws)),
                                                 True));
                           break;

                        case 1:
                           t2 = newTemp(Ity_I64);
                           assign(t2,
                                  unop(Iop_V128to64, getWReg(ws)));
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_64HIto32,
                                                      mkexpr(t2)),
                                                 True));
                           break;

                        case 2:
                           t2 = newTemp(Ity_I64);
                           assign(t2,
                                  unop(Iop_V128HIto64,
                                       getWReg(ws)));
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_64to32,
                                                      mkexpr(t2)),
                                                 True));
                           break;

                        case 3:
                           t2 = newTemp(Ity_I64);
                           assign(t2,
                                  unop(Iop_V128HIto64,
                                       getWReg(ws)));
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_64HIto32,
                                                      mkexpr(t2)),
                                                 True));
                           break;

                        default:
                           break;
                     }

                     break;

                  case 0x38: /* COPY_S.D */
                     if (mode64) {
                        DIP("COPY_S.D r%d, w%d[%d]", wd, ws, n);

                        switch (n) {
                           case 0:
                              putIReg(wd,
                                      unop(Iop_V128to64,
                                           getWReg(ws)));
                              break;

                           case 1:
                              putIReg(wd,
                                      unop(Iop_V128HIto64,
                                           getWReg(ws)));
                              break;
                        }
                     } else {
                        return -2;
                     }

                     break;

                  default:
                     return -1;
               }

               break;

            case 0x03: { /* COPY_U.df */
               switch (df) {
                  case 0x00: /* COPY_U.B */
                     DIP("COPY_U.B r%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_I8);

                     switch (n) {
                        case 0:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 1:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 2:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 3:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 4:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 5:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 6:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 7:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128to64,
                                                      getWReg(ws))))));
                           break;

                        case 8:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 9:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 10:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 11:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64to32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 12:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 13:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32to16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 14:
                           assign(t1,
                                  unop(Iop_16to8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;

                        case 15:
                           assign(t1,
                                  unop(Iop_16HIto8,
                                       unop(Iop_32HIto16,
                                            unop(Iop_64HIto32,
                                                 unop(Iop_V128HIto64,
                                                      getWReg(ws))))));
                           break;
                     }

                     putIReg(wd,
                             unop(mode64 ? Iop_8Uto64 : Iop_8Uto32,
                                  mkexpr(t1)));
                     break;

                  case 0x20: /* COPY_U.H */
                     DIP("COPY_U.H r%d, w%d[%d]", wd, ws, n);
                     t1 = newTemp(Ity_I16);

                     switch (n) {
                        case 0:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 1:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 2:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 3:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128to64,
                                                 getWReg(ws)))));
                           break;

                        case 4:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;

                        case 5:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64to32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;

                        case 6:
                           assign(t1,
                                  unop(Iop_32to16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;

                        case 7:
                           assign(t1,
                                  unop(Iop_32HIto16,
                                       unop(Iop_64HIto32,
                                            unop(Iop_V128HIto64,
                                                 getWReg(ws)))));
                           break;
                     }

                     putIReg(wd,
                             unop(mode64 ? Iop_16Uto64 : Iop_16Uto32,
                                  mkexpr(t1)));
                     break;

                  case 0x30: /* COPY_U.W */
                     DIP("COPY_U.W r%d, w%d[%d]", wd, ws, n);

                     switch (n) {
                        case 0:
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_V128to32,
                                                      getWReg(ws)),
                                                 False));
                           break;

                        case 1:
                           t2 = newTemp(Ity_I64);
                           assign(t2,
                                  unop(Iop_V128to64,
                                       getWReg(ws)));
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_64HIto32,
                                                      mkexpr(t2)),
                                                 False));
                           break;

                        case 2:
                           t2 = newTemp(Ity_I64);
                           assign(t2,
                                  unop(Iop_V128HIto64,
                                       getWReg(ws)));
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_64to32,
                                                      mkexpr(t2)),
                                                 False));
                           break;

                        case 3:
                           t2 = newTemp(Ity_I64);
                           assign(t2,
                                  unop(Iop_V128HIto64,
                                       getWReg(ws)));
                           putIReg(wd,
                                   mkWidenFrom32(ty,
                                                 unop(Iop_64HIto32,
                                                      mkexpr(t2)),
                                                 False));
                           break;

                        default:
                           break;
                     }

                     break;

                  default:
                     return -1;
               }

               break;
            }

            case 0x04: { /* INSERT.df */
               t5 = newTemp(Ity_I64);
               UInt hi = 1;
               ULong mask;
               IRTemp *src, *dst;
               assign(t5, mode64 ? getIReg(ws) :
                      unop(Iop_32Uto64, getIReg(ws)));

               if (df == 0x38) { /* INSERT.D */
                  if (mode64) {
                     DIP("INSERT.D w%d[%d], r%d", wd, n, ws);

                     if (n == 0) {
                        putWReg(wd,
                                binop(Iop_64HLtoV128,
                                      unop(Iop_V128HIto64,
                                           getWReg(wd)),
                                      mkexpr(t5)));
                     } else {
                        putWReg(wd,
                                binop(Iop_64HLtoV128,
                                      mkexpr(t5),
                                      unop(Iop_V128to64,
                                           getWReg(wd))));
                     }

                     break;
                  } else {
                     return -2;
                  }
               } else {
                  t1 = newTemp(Ity_I64);
                  t2 = newTemp(Ity_I64);
                  assign(t1, unop(Iop_V128to64, getWReg(wd)));
                  assign(t2, unop(Iop_V128HIto64, getWReg(wd)));
               }

               switch (df) {
                  case 0x00: /* INSERT.B */
                     DIP("INSERT.B w%d[%d], r%d", wd, n, ws);

                     if (n >= 8) {
                        n -= 8;
                     } else {
                        hi = 0;
                     }

                     n <<= 3;
                     mask = 0xFFull;
                     break;

                  case 0x20: /* INSERT.H */
                     DIP("INSERT.H w%d[%d], r%d", wd, n, ws);

                     if (n >= 4) {
                        n -= 4;
                     } else {
                        hi = 0;
                     }

                     n <<= 4;
                     mask = 0xFFFFull;
                     break;

                  case 0x30: /* INSERT.W */
                     DIP("INSERT.W w%d[%d], r%d", wd, n, ws);

                     if (n >= 2) {
                        n -= 2;
                     } else {
                        hi = 0;
                     }

                     n <<= 5;
                     mask = 0xFFFFFFFFull;
                     break;

                  default:
                     return -1;
               }

               if (hi) {
                  t4 = newTemp(Ity_I64);
                  src = &t2;
                  dst = &t4;
                  t3 = t1;
               } else {
                  t3 = newTemp(Ity_I64);
                  src = &t1;
                  dst = &t3;
                  t4 = t2;
               }

               mask <<= n;
               assign(*dst,
                      binop(Iop_Or64,
                            binop(Iop_And64, mkexpr(*src), mkU64(~mask)),
                            binop(Iop_And64,
                                  binop(Iop_Shl64, mkexpr(t5), mkU8(n)),
                                  mkU64(mask))));
               putWReg(wd,
                       binop(Iop_64HLtoV128, mkexpr(t4), mkexpr(t3)));
               break;
            }

            case 0x05: { /* INSVE.df */
               switch (df) {
                  case 0x00: { /* INSVE.B */
                     DIP("INSVE.B w%d[%d], w%d[0]", wd, n, ws);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     assign(t1, getWReg(wd));
                     assign(t2, getWReg(ws));
                     Int i;
                     IRTemp tmp[16];

                     for (i = 0; i < 16; i++) {
                        tmp[i] = newTemp(Ity_I8);

                        if (n == i)
                           assign(tmp[i],
                                  binop(Iop_GetElem8x16,
                                        mkexpr(t2), mkU8(0x0)));
                        else
                           assign(tmp[i],
                                  binop(Iop_GetElem8x16,
                                        mkexpr(t1), mkU8(i)));
                     }

                     putWReg(wd,
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[15]),
                                                     mkexpr(tmp[14])),
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[13]),
                                                     mkexpr(tmp[12]))),
                                         binop(Iop_16HLto32,
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[11]),
                                                     mkexpr(tmp[10])),
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[9]),
                                                     mkexpr(tmp[8])))),
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[7]),
                                                     mkexpr(tmp[6])),
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[5]),
                                                     mkexpr(tmp[4]))),
                                         binop(Iop_16HLto32,
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[3]),
                                                     mkexpr(tmp[2])),
                                               binop(Iop_8HLto16,
                                                     mkexpr(tmp[1]),
                                                     mkexpr(tmp[0]))))));
                     break;
                  }

                  case 0x20: { /* INSVE.H */
                     DIP("INSVE.H w%d[%d], r%d[0]", wd, n, ws);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     assign(t1, getWReg(wd));
                     assign(t2, getWReg(ws));
                     Int i;
                     IRTemp tmp[8];

                     for (i = 0; i < 8; i++) {
                        tmp[i] = newTemp(Ity_I16);

                        if (n == i)
                           assign(tmp[i],
                                  binop(Iop_GetElem16x8,
                                        mkexpr(t2), mkU8(0x0)));
                        else
                           assign(tmp[i],
                                  binop(Iop_GetElem16x8,
                                        mkexpr(t1), mkU8(i)));
                     }

                     putWReg(wd,
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[7]),
                                               mkexpr(tmp[6])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[5]),
                                               mkexpr(tmp[4]))),
                                   binop(Iop_32HLto64,
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[3]),
                                               mkexpr(tmp[2])),
                                         binop(Iop_16HLto32,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0])))));
                     break;
                  }

                  case 0x30: { /* INSVE.W */
                     DIP("INSVE.W w%d[%d], r%d[0]", wd, n, ws);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     assign(t1, getWReg(wd));
                     assign(t2, getWReg(ws));
                     Int i;
                     IRTemp tmp[4];

                     for (i = 0; i < 4; i++) {
                        tmp[i] = newTemp(Ity_I32);

                        if (n == i)
                           assign(tmp[i],
                                  binop(Iop_GetElem32x4,
                                        mkexpr(t2), mkU8(0x0)));
                        else
                           assign(tmp[i],
                                  binop(Iop_GetElem32x4,
                                        mkexpr(t1), mkU8(i)));
                     }

                     putWReg(wd,
                             binop(Iop_64HLtoV128,
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[3]),
                                         mkexpr(tmp[2])),
                                   binop(Iop_32HLto64,
                                         mkexpr(tmp[1]),
                                         mkexpr(tmp[0]))));
                     break;
                  }

                  case 0x38: { /* INSVE.D */
                     DIP("INSVE.D w%d[%d], r%d[0]", wd, n, ws);
                     t1 = newTemp(Ity_V128);
                     t2 = newTemp(Ity_V128);
                     assign(t1, getWReg(wd));
                     assign(t2, getWReg(ws));
                     Int i;
                     IRTemp tmp[2];

                     for (i = 0; i < 2; i++) {
                        tmp[i] = newTemp(Ity_I64);

                        if (n == i)
                           assign(tmp[i],
                                  binop(Iop_GetElem64x2,
                                        mkexpr(t2), mkU8(0x0)));
                        else
                           assign(tmp[i],
                                  binop(Iop_GetElem64x2,
                                        mkexpr(t1), mkU8(i)));
                     }

                     putWReg(wd,
                             binop(Iop_64HLtoV128,
                                   mkexpr(tmp[1]), mkexpr(tmp[0])));
                     break;
                  }
               }

               break;
            }

            default:
               return -1;
         }
   }

   return 0;
}

static Int msa_VEC(UInt cins, UChar wd, UChar ws)   /* VEC */
{
   IRTemp t1, t2, t3;
   UShort operation;
   UChar wt;

   vassert((cins & 0x03000000) == 0);

   operation = (cins & 0x03E00000) >> 21;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {
      case 0x00: { /* AND.V */
         DIP("AND.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));
         assign(t3, binop(Iop_AndV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x01: { /* OR.V */
         DIP("OR.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x02: { /* NOR.V */
         DIP("NOR.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));
         assign(t3,
                unop(Iop_NotV128,
                     binop(Iop_OrV128, mkexpr(t1), mkexpr(t2))));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x03: { /* XOR.V */
         DIP("XOR.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         assign(t2, getWReg(wt));
         assign(t3, binop(Iop_XorV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x04: { /* BMNZ  (ws AND wt) OR (wd AND NOT wt) */
         DIP("BMNZ.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1,
                binop(Iop_AndV128,
                      getWReg(ws), getWReg(wt)));
         assign(t2,
                binop(Iop_AndV128,
                      getWReg(wd),
                      unop(Iop_NotV128, getWReg(wt))));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x05: { /* BMZ.V (ws AND NOT wt) OR (wd AND wt) */
         DIP("BMZ.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1,
                binop(Iop_AndV128,
                      getWReg(wd), getWReg(wt)));
         assign(t2,
                binop(Iop_AndV128,
                      getWReg(ws),
                      unop(Iop_NotV128, getWReg(wt))));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      case 0x06: { /* BSEL (ws AND NOT wd) OR (wt AND wd) */
         DIP("BSEL.V w%d, w%d, w%d", wd, ws, wt);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         assign(t1,
                binop(Iop_AndV128,
                      getWReg(wd), getWReg(wt)));
         assign(t2,
                binop(Iop_AndV128,
                      getWReg(ws),
                      unop(Iop_NotV128, getWReg(wd))));
         assign(t3, binop(Iop_OrV128, mkexpr(t1), mkexpr(t2)));
         putWReg(wd, mkexpr(t3));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_2R(UInt cins, UChar wd, UChar ws)   /* 2R */
{
   IRTemp t1, t2, t3, t4;
   IRType ty;
   UShort operation;
   UChar df;

   vassert((cins & 0x00200000) == 0);

   operation = (cins & 0x03FC0000) >> 18;
   df = (cins & 0x00030000) >> 16;
   ty = mode64 ? Ity_I64 : Ity_I32;

   switch (operation) {
      case 0xC0: { /* FILL.df */
         t1 = newTemp(Ity_I64);

         switch (df) {
            case 0x00: /* FILL.B */
               DIP("FILL.B w%d, r%d", wd, ws);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I16);
               t4 = newTemp(Ity_I8);
               assign(t4, mkNarrowTo8(ty, getIReg(ws)));
               assign(t3,
                      binop(Iop_8HLto16, mkexpr(t4), mkexpr(t4)));
               assign(t2,
                      binop(Iop_16HLto32, mkexpr(t3), mkexpr(t3)));
               assign(t1,
                      binop(Iop_32HLto64, mkexpr(t2), mkexpr(t2)));
               break;

            case 0x01: /* FILL.H */
               DIP("FILL.H w%d, r%d", wd, ws);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I16);
               assign(t3, mkNarrowTo16(ty, getIReg(ws)));
               assign(t2,
                      binop(Iop_16HLto32, mkexpr(t3), mkexpr(t3)));
               assign(t1,
                      binop(Iop_32HLto64, mkexpr(t2), mkexpr(t2)));
               break;

            case 0x02: /* FILL.W */
               DIP("FILL.W w%d, r%d", wd, ws);
               t2 = newTemp(Ity_I32);
               assign(t2, mkNarrowTo32(ty, getIReg(ws)));
               assign(t1,
                      binop(Iop_32HLto64, mkexpr(t2), mkexpr(t2)));
               break;

            case 0x03: /* FILL.D */
               if (mode64) {
                  DIP("FILL.W w%d, r%d", wd, ws);
                  t2 = newTemp(Ity_I32);
                  assign(t1, getIReg(ws));
               } else {
                  return -2;
               }

               break;

            default:
               return -1;
         }

         putWReg(wd,
                 binop(Iop_64HLtoV128, mkexpr(t1), mkexpr(t1)));
         break;
      }

      case 0xC1: { /* PCNT.df */
         switch (df) {
            case 0x00: /* PCNT.B */
               DIP("PCNT.B w%d, r%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Cnt8x16, getWReg(ws)));
               break;

            case 0x01: /* PCNT.H */
               DIP("PCNT.H w%d, r%d", wd, ws);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Cnt8x16, getWReg(ws)));
               assign(t2,
                      binop(Iop_Add16x8,
                            binop(Iop_AndV128,
                                  mkexpr(t1),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00FF00FF00FF00FFULL),
                                        mkU64(0x00FF00FF00FF00FFULL))),
                            binop(Iop_AndV128,
                                  binop(Iop_ShrN16x8,
                                        mkexpr(t1), mkU8(8)),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00FF00FF00FF00FFULL),
                                        mkU64(0x00FF00FF00FF00FFULL)))));
               putWReg(wd, mkexpr(t2));
               break;

            case 0x02: /* PCNT.W */
               DIP("PCNT.W w%d, r%d", wd, ws);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               assign(t1, unop(Iop_Cnt8x16, getWReg(ws)));
               assign(t2,
                      binop(Iop_Add32x4,
                            binop(Iop_AndV128,
                                  mkexpr(t1),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00FF00FF00FF00FFULL),
                                        mkU64(0x00FF00FF00FF00FFULL))),
                            binop(Iop_AndV128,
                                  binop(Iop_ShrN32x4,
                                        mkexpr(t1), mkU8(8)),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00FF00FF00FF00FFULL),
                                        mkU64(0x00FF00FF00FF00FFULL)))));
               assign(t3,
                      binop(Iop_Add32x4,
                            binop(Iop_AndV128,
                                  mkexpr(t2),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0000FFFF0000FFFFULL),
                                        mkU64(0x0000FFFF0000FFFFULL))),
                            binop(Iop_AndV128,
                                  binop(Iop_ShrN32x4,
                                        mkexpr(t2), mkU8(16)),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0000FFFF0000FFFFULL),
                                        mkU64(0x0000FFFF0000FFFFULL)))));
               putWReg(wd, mkexpr(t3));
               break;

            case 0x03: /* PCNT.D */
               DIP("PCNT.D w%d, r%d", wd, ws);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_V128);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);;
               assign(t1, unop(Iop_Cnt8x16, getWReg(ws)));
               assign(t2,
                      binop(Iop_Add64x2,
                            binop(Iop_AndV128,
                                  mkexpr(t1),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00FF00FF00FF00FFULL),
                                        mkU64(0x00FF00FF00FF00FFULL))),
                            binop(Iop_AndV128,
                                  binop(Iop_ShrN64x2,
                                        mkexpr(t1), mkU8(8)),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00FF00FF00FF00FFULL),
                                        mkU64(0x00FF00FF00FF00FFULL)))));
               assign(t3,
                      binop(Iop_Add64x2,
                            binop(Iop_AndV128,
                                  mkexpr(t2),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0000FFFF0000FFFFULL),
                                        mkU64(0x0000FFFF0000FFFFULL))),
                            binop(Iop_AndV128,
                                  binop(Iop_ShrN64x2,
                                        mkexpr(t2), mkU8(16)),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0000FFFF0000FFFFULL),
                                        mkU64(0x0000FFFF0000FFFFULL)))));
               assign(t4,
                      binop(Iop_Add64x2,
                            binop(Iop_AndV128,
                                  mkexpr(t3),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00000000FFFFFFFFULL),
                                        mkU64(0x00000000FFFFFFFFULL))),
                            binop(Iop_AndV128,
                                  binop(Iop_ShrN64x2,
                                        mkexpr(t3), mkU8(32)),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x00000000FFFFFFFFULL),
                                        mkU64(0x00000000FFFFFFFFULL)))));
               putWReg(wd, mkexpr(t4));
               break;

            default:
               return -1;
         }

         break;
      }

      case 0xC2: { /* NLOC.df */
         switch (df) {
            case 0x00: /* NLOC.B */
               DIP("NLOC.B w%d, w%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Cls8x16, getWReg(ws)));
               break;

            case 0x01: /* NLOC.H */
               DIP("NLOC.H w%d, w%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Cls16x8, getWReg(ws)));
               break;

            case 0x02: /* NLOC.W */
               DIP("NLOC.W w%d, w%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Cls32x4, getWReg(ws)));
               break;

            case 0x03: /* NLOC.D */
               DIP("NLOC.D w%d, w%d", wd, ws);
               t1 = newTemp(Ity_V128);
               assign(t1, unop(Iop_NotV128, getWReg(ws)));
               putWReg(wd, unop(Iop_Clz64x2, mkexpr(t1)));
               break;

            default:
               return -1;
         }

         break;
      }

      case 0xC3: { /* NLZC.df */
         switch (df) {
            case 0x00: /* NLZC.B */
               DIP("NLZC.W w%d, w%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Clz8x16, getWReg(ws)));
               break;

            case 0x01: /* NLZC.H */
               DIP("NLZC.H w%d, w%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Clz16x8, getWReg(ws)));
               break;

            case 0x02: /* NLZC.W */
               DIP("NLZC.W w%d, w%d", wd, ws);
               putWReg(wd,
                       unop(Iop_Clz32x4, getWReg(ws)));
               break;

            case 0x03: {/* NLZC.D */
               putWReg(wd,
                       unop(Iop_Clz64x2, getWReg(ws)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_2RF(UInt cins, UChar wd, UChar ws)   /* 2RF */
{
   IRTemp t1, t2, t3, t4, t5;
   UShort operation;
   UChar df, wt;

   operation = (cins & 0x03FE0000) >> 17;
   df = (cins & 0x00010000) >> 16;
   wt = (cins & 0x001F0000) >> 16;

   switch (operation) {

      case 0x190: { /* FCLASS.df */
         IRTemp t0 = newTemp(Ity_V128);
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         t4 = newTemp(Ity_V128);
         t5 = newTemp(Ity_V128);

         switch (df) {
            case 0x00: { /* FCLASS.W */
               DIP("FCLASS.W w%d, w%d", wd, ws);
               assign(t0,
                      binop(Iop_CmpEQ32x4,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x7F8000007F800000ull),
                                        mkU64(0x7F8000007F800000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0ull), mkU64(0ull))));
               assign(t1,
                      binop(Iop_CmpEQ32x4,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x7F8000007F800000ull),
                                        mkU64(0x7F8000007F800000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7F8000007F800000ull),
                                  mkU64(0x7F8000007F800000ull))));
               assign(t2,
                      binop(Iop_SarN32x4,
                            getWReg(ws), mkU8(31)));
               assign(t3,
                      binop(Iop_CmpEQ32x4,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0040000000400000ull),
                                        mkU64(0x0040000000400000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x0040000000400000ull),
                                  mkU64(0x0040000000400000ull))));
               assign(t4,
                      binop(Iop_CmpEQ32x4,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x007FFFFF007FFFFFULL),
                                        mkU64(0x007FFFFF007FFFFFULL))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0ull), mkU64(0ull))));
               assign(t5,
                      binop(Iop_Shl32x4,
                            binop(Iop_OrV128,
                                  binop(Iop_AndV128,
                                        mkexpr(t1),
                                        binop(Iop_AndV128,
                                              mkexpr(t4),
                                              binop(Iop_64HLtoV128,
                                                    mkU64(0x100000001ull),
                                                    mkU64(0x100000001ull)))),
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128,
                                              mkexpr(t0),
                                              binop(Iop_OrV128,
                                                    binop(Iop_AndV128,
                                                          mkexpr(t4),
                                                          binop(Iop_64HLtoV128,
                                                                mkU64(0x800000008ull),
                                                                mkU64(0x800000008ull))),
                                                    binop(Iop_AndV128,
                                                          unop(Iop_NotV128,
                                                                mkexpr(t4)),
                                                          binop(Iop_64HLtoV128,
                                                                mkU64(0x400000004ull),
                                                                mkU64(0x400000004ull))))),
                                        binop(Iop_AndV128,
                                              unop(Iop_NotV128,
                                                   mkexpr(t1)),
                                              binop(Iop_AndV128,
                                                    unop(Iop_NotV128,
                                                          mkexpr(t0)),
                                                    binop(Iop_64HLtoV128,
                                                          mkU64(0x200000002ull),
                                                          mkU64(0x200000002ull)))))),
                            binop(Iop_OrV128,
                                  binop(Iop_AndV128,
                                        mkexpr(t2),
                                        binop(Iop_64HLtoV128,
                                              mkU64(0x200000002ull),
                                              mkU64(0x200000002ull))),
                                  binop(Iop_AndV128,
                                        unop(Iop_NotV128,
                                             mkexpr(t2)),
                                        binop(Iop_64HLtoV128,
                                              mkU64(0x600000006ull),
                                              mkU64(0x600000006ull))))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t5),
                             binop(Iop_AndV128,
                                   binop(Iop_CmpEQ32x4,
                                         mkexpr(t5),
                                         binop(Iop_64HLtoV128,
                                               mkU64(0ull),
                                               mkU64(0ull))),
                                   binop(Iop_OrV128,
                                         binop(Iop_AndV128,
                                               mkexpr(t3),
                                               binop(Iop_64HLtoV128,
                                                     mkU64(0x100000001ull),
                                                     mkU64(0x100000001ull))),
                                         binop(Iop_AndV128,
                                               unop(Iop_NotV128, mkexpr(t3)),
                                               binop(Iop_64HLtoV128,
                                                     mkU64(0x200000002ull),
                                                     mkU64(0x200000002ull)))))));
               break;
            }

            case 0x01: { /* FCLASS.D */
               DIP("FCLASS.D w%d, w%d", wd, ws);
               assign(t0,
                      binop(Iop_CmpEQ64x2,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x7FF0000000000000ull),
                                        mkU64(0x7FF0000000000000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0ull), mkU64(0ull))));
               assign(t1,
                      binop(Iop_CmpEQ64x2,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x7FF0000000000000ull),
                                        mkU64(0x7FF0000000000000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x7FF0000000000000ull),
                                  mkU64(0x7FF0000000000000ull))));
               assign(t2,
                      binop(Iop_SarN64x2,
                            getWReg(ws), mkU8(63)));
               assign(t3,
                      binop(Iop_CmpEQ64x2,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0008000000000000ull),
                                        mkU64(0x0008000000000000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x0008000000000000ull),
                                  mkU64(0x0008000000000000ull))));
               assign(t4,
                      binop(Iop_CmpEQ64x2,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x000FFFFFFFFFFFFFULL),
                                        mkU64(0x000FFFFFFFFFFFFFULL))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0ull), mkU64(0ull))));
               assign(t5,
                      binop(Iop_Shl64x2,
                            binop(Iop_OrV128,
                                  binop(Iop_AndV128,
                                        mkexpr(t1),
                                        binop(Iop_AndV128,
                                              mkexpr(t4),
                                              binop(Iop_64HLtoV128,
                                                    mkU64(1ull),
                                                    mkU64(1ull)))),
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128,
                                              mkexpr(t0),
                                              binop(Iop_OrV128,
                                                    binop(Iop_AndV128,
                                                          mkexpr(t4),
                                                          binop(Iop_64HLtoV128,
                                                                mkU64(8ull),
                                                                mkU64(8ull))),
                                                    binop(Iop_AndV128,
                                                          unop(Iop_NotV128,
                                                                mkexpr(t4)),
                                                          binop(Iop_64HLtoV128,
                                                                mkU64(4ull),
                                                                mkU64(4ull))))),
                                        binop(Iop_AndV128,
                                              unop(Iop_NotV128,
                                                   mkexpr(t1)),
                                              binop(Iop_AndV128,
                                                    unop(Iop_NotV128,
                                                          mkexpr(t0)),
                                                    binop(Iop_64HLtoV128,
                                                          mkU64(2ull),
                                                          mkU64(2ull)))))),
                            binop(Iop_OrV128,
                                  binop(Iop_AndV128,
                                        mkexpr(t2),
                                        binop(Iop_64HLtoV128,
                                              mkU64(2ull),
                                              mkU64(2ull))),
                                  binop(Iop_AndV128,
                                        unop(Iop_NotV128,
                                             mkexpr(t2)),
                                        binop(Iop_64HLtoV128,
                                              mkU64(6ull),
                                              mkU64(6ull))))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             mkexpr(t5),
                             binop(Iop_AndV128,
                                   binop(Iop_CmpEQ64x2,
                                         mkexpr(t5),
                                         binop(Iop_64HLtoV128,
                                               mkU64(0ull),
                                               mkU64(0ull))),
                                   binop(Iop_OrV128,
                                         binop(Iop_AndV128,
                                               mkexpr(t3),
                                               binop(Iop_64HLtoV128,
                                                     mkU64(1ull),
                                                     mkU64(1ull))),
                                         binop(Iop_AndV128,
                                               unop(Iop_NotV128,
                                                    mkexpr(t3)),
                                               binop(Iop_64HLtoV128,
                                                     mkU64(2ull),
                                                     mkU64(2ull)))))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x191: { /* FTRUNC_S.df */
         switch (df) {
            case 0x00: { /* FTRUNC_S.W */
               DIP("FTRUNC_S.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTRUNCSW, 1);
               putWReg(wd, unop(Iop_F32toI32Sx4_RZ, getWReg(ws)));
               break;
            }

            case 0x01: { /* FTRUNC_S.D */
               DIP("FTRUNC_S.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTRUNCSD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               assign(t3,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128,
                                 binop(Iop_CmpUN64Fx2,
                                       getWReg(ws),
                                       getWReg(ws))),
                            binop(Iop_Max64Fx2,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0xC3E0000000000000),
                                        mkU64(0xC3E0000000000000)))));
               assign(t1,
                      binop(Iop_F64toI64S, mkU32(0x3),
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128to64, mkexpr(t3)))));
               assign(t2,
                      binop(Iop_F64toI64S, mkU32(0x3),
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128HIto64, mkexpr(t3)))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x192: { /* FTRUNC_U.df */
         switch (df) {
            case 0x00: {  /* FTRUNC_U.W */
               DIP("FTRUNC_U.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTRUNCUW, 1);
               putWReg(wd, unop(Iop_F32toI32Ux4_RZ, getWReg(ws)));
               break;
            }

            case 0x01: { /* FTRUNC_U.D */
               DIP("FTRUNC_U.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTRUNCUD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               assign(t1,
                      binop(Iop_F64toI64U,
                            mkU32(0x3),
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128to64,
                                      getWReg(ws)))));
               assign(t2,
                      binop(Iop_F64toI64U,
                            mkU32(0x3),
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128HIto64,
                                      getWReg(ws)))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x193: { /* FSQRT.df */
         switch (df) {
            case 0x00: { /* FSQRT.W */
               DIP("FSQRT.W w%d, w%d", wd, ws);
               IRExpr *rm = get_IR_roundingmode_MSA();
               calculateMSACSR(ws, wd, FSQRTW, 1);
               putWReg(wd, binop(Iop_Sqrt32Fx4, rm, getWReg(ws)));
               break;
            }

            case 0x01: { /* FSQRT.D */
               DIP("FSQRT.D w%d, w%d", wd, ws);
               IRExpr *rm = get_IR_roundingmode_MSA();
               calculateMSACSR(ws, wd, FSQRTD, 1);
               putWReg(wd, binop(Iop_Sqrt64Fx2, rm, getWReg(ws)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x194: { /* FRSQRT.df */
         switch (df) {
            case 0x00: { /* FRSQRT.W */
               DIP("FRSQRT.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FRSQRTW, 1);
               putWReg(wd, unop(Iop_RSqrtEst32Fx4, getWReg(ws)));
               break;
            }

            case 0x01: { /* FRSQRT.D */
               DIP("FRSQRT.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FRSQRTD, 1);
               putWReg(wd, unop(Iop_RSqrtEst64Fx2, getWReg(ws)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x195: { /* FRCP.df */
         switch (df) { /* FRCP.W */
            case 0x00: {
               DIP("FRCP.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FRCPW, 1);
               putWReg(wd, unop(Iop_RecipEst32Fx4, getWReg(ws)));
               break;
            }

            case 0x01: { /* FRCP.D */
               DIP("FRCP.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FRCPD, 1);
               putWReg(wd, unop(Iop_RecipEst64Fx2, getWReg(ws)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x196: { /* FRINT.df */
         t1 = newTemp(Ity_V128);
         t2 = newTemp(Ity_V128);
         t3 = newTemp(Ity_V128);
         t4 = newTemp(Ity_V128);
         IRExpr *rm = get_IR_roundingmode_MSA();
         assign(t1, getWReg(ws));

         switch (df) {
            case 0x00: { /* FRINT.W */
               DIP("FRINT.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FRINTW, 1);
               assign(t2,
                      binop(Iop_OrV128,
                            binop(Iop_CmpLT32Fx4,
                                  mkexpr(t1),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0xCF000000CF000000ull),
                                        mkU64(0xCF000000CF000000ull))),
                            binop(Iop_CmpLT32Fx4,
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x4F0000004F000000ull),
                                        mkU64(0x4F0000004F000000ull)),
                                  mkexpr(t1))));
               assign(t3,
                      binop(Iop_CmpEQ32x4,
                            binop(Iop_AndV128,
                                  mkexpr(t1),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0040000000400000ull),
                                        mkU64(0x0040000000400000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x0040000000400000ull),
                                  mkU64(0x0040000000400000ull))));
               assign(t4,
                      binop(Iop_CmpUN32Fx4,
                            mkexpr(t1), mkexpr(t1)));
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_I32);
                  assign(tmp[i],
                         unop(Iop_ReinterpF32asI32,
                              binop(Iop_RoundF32toInt, rm,
                                    unop(Iop_ReinterpI32asF32,
                                         binop(Iop_GetElem32x4,
                                               mkexpr(t1), mkU8(i))))));
               }

               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         binop(Iop_OrV128,
                                               mkexpr(t2),
                                               binop(Iop_AndV128,
                                                     mkexpr(t4),
                                                     unop(Iop_NotV128,
                                                           mkexpr(t3)))),
                                         mkexpr(t1)),
                                   binop(Iop_AndV128,
                                         binop(Iop_AndV128,
                                               mkexpr(t4),
                                               mkexpr(t3)),
                                         binop(Iop_64HLtoV128,
                                               mkU64(0x7FBFFFFF7FBFFFFF),
                                               mkU64(0x7FBFFFFF7FBFFFFF)))),
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128,
                                        binop(Iop_OrV128,
                                              mkexpr(t2),
                                              mkexpr(t4))),
                                   binop(Iop_OrV128,
                                         binop(Iop_64HLtoV128,
                                               binop(Iop_32HLto64,
                                                     mkexpr(tmp[3]),
                                                     mkexpr(tmp[2])),
                                               binop(Iop_32HLto64,
                                                     mkexpr(tmp[1]),
                                                     mkexpr(tmp[0]))),
                                         binop(Iop_AndV128,
                                               mkexpr(t1),
                                               binop(Iop_64HLtoV128,
                                                     mkU64(0x8000000080000000ull),
                                                     mkU64(0x8000000080000000ull)))
                                        ))));
               break;
            }

            case 0x01: { /* FRINT.D */
               DIP("FRINT.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FRINTD, 1);
               assign(t2,
                      binop(Iop_OrV128,
                            binop(Iop_CmpLT64Fx2,
                                  mkexpr(t1),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0xC3E0000000000000ull),
                                        mkU64(0xC3E0000000000000ull))),
                            binop(Iop_CmpLT64Fx2,
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x43E0000000000000ull),
                                        mkU64(0x43E0000000000000ull)),
                                  mkexpr(t1))));
               assign(t3,
                      binop(Iop_CmpEQ64x2,
                            binop(Iop_AndV128,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0x0008000000000000ull),
                                        mkU64(0x0008000000000000ull))),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x0008000000000000ull),
                                  mkU64(0x0008000000000000ull))));
               assign(t4,
                      binop(Iop_CmpUN64Fx2,
                            mkexpr(t1), mkexpr(t1)));
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_I64);
                  assign(tmp[i],
                         unop(Iop_ReinterpF64asI64,
                              binop(Iop_RoundF64toInt, rm,
                                    unop(Iop_ReinterpI64asF64,
                                         binop(Iop_GetElem64x2,
                                               mkexpr(t1), mkU8(i))))));
               }

               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128,
                                   binop(Iop_AndV128,
                                         binop(Iop_OrV128,
                                               mkexpr(t2),
                                               binop(Iop_AndV128,
                                                     mkexpr(t4),
                                                     unop(Iop_NotV128,
                                                           mkexpr(t3)))),
                                         mkexpr(t1)),
                                   binop(Iop_AndV128,
                                         binop(Iop_AndV128,
                                               mkexpr(t4),
                                               mkexpr(t3)),
                                         binop(Iop_64HLtoV128,
                                               mkU64(0x7FF7FFFFFFFFFFFF),
                                               mkU64(0x7FF7FFFFFFFFFFFF)))),
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128,
                                        binop(Iop_OrV128,
                                              mkexpr(t2),
                                              mkexpr(t4))),
                                   binop(Iop_OrV128,
                                         binop(Iop_64HLtoV128,
                                               mkexpr(tmp[1]),
                                               mkexpr(tmp[0])),
                                         binop(Iop_AndV128,
                                               mkexpr(t1),
                                               binop(Iop_64HLtoV128,
                                                     mkU64(0x8000000000000000ull),
                                                     mkU64(0x8000000000000000ull))
                                              )))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x197: { /* FLOG2.df */

         switch (df) {
            case 0x00: { /* FLOG2.W */
               DIP("FLOG2.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FLOG2W, 1);
               putWReg(wd, unop(Iop_Log2_32Fx4, getWReg(ws)));
               break;
            }

            case 0x01: { /* FLOG2.D */
               DIP("FLOG2.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FLOG2D, 1);
               putWReg(wd, unop(Iop_Log2_64Fx2, getWReg(ws)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x198: { /* FEXUPL.df */
         switch (df) {
            case 0x00: { /* FEXUPL.W */
               DIP("FEXUPL.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FEXUPLW, 1);
               putWReg(wd,
                       unop(Iop_F16toF32x4,
                            unop(Iop_V128HIto64,
                                 getWReg(ws))));
               break;
            }

            case 0x01: { /* FEXUPL.D */
               DIP("FEXUPL.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FEXUPLD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               assign(t1,
                      unop(Iop_ReinterpF64asI64,
                           unop(Iop_F32toF64,
                                unop(Iop_ReinterpI32asF32,
                                     unop(Iop_64to32,
                                          unop(Iop_V128HIto64,
                                               getWReg(ws)))))));
               assign(t2,
                      unop(Iop_ReinterpF64asI64,
                           unop(Iop_F32toF64,
                                unop(Iop_ReinterpI32asF32,
                                     unop(Iop_64HIto32,
                                          unop(Iop_V128HIto64,
                                               getWReg(ws)))))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x199: { /* FEXUPR.df */
         switch (df) {
            case 0x00: { /* FEXUPR.W */
               DIP("FEXUPR.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FEXUPRW, 1);
               putWReg(wd,
                       unop(Iop_F16toF32x4,
                            unop(Iop_V128to64,
                                 getWReg(ws))));
               break;
            }

            case 0x01: { /* FEXUPR.D */
               DIP("FEXUPR.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FEXUPRD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               assign(t1,
                      unop(Iop_ReinterpF64asI64,
                           unop(Iop_F32toF64,
                                unop(Iop_ReinterpI32asF32,
                                     unop(Iop_64to32,
                                          unop(Iop_V128to64,
                                               getWReg(ws)))))));
               assign(t2,
                      unop(Iop_ReinterpF64asI64,
                           unop(Iop_F32toF64,
                                unop(Iop_ReinterpI32asF32,
                                     unop(Iop_64HIto32,
                                          unop(Iop_V128to64,
                                               getWReg(ws)))))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x19A: { /* FFQL.df */
         switch (df) {
            case 0x00: { /* FFQL.W */
               DIP("FFQL.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFQLW, 1);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveHI16x8,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(16)));
               assign(t2,
                      binop(Iop_32HLto64,
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(1)))),
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(0))))));
               assign(t3,
                      binop(Iop_32HLto64,
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(3)))),
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(2))))));
               putWReg(wd,
                       triop(Iop_Div32Fx4, rm,
                             binop(Iop_64HLtoV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_64HLtoV128,
                                   mkU64(0x4700000047000000),
                                   mkU64(0x4700000047000000))));
               break;
            }

            case 0x01: { /* FFQL.D */
               DIP("FFQL.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFQLD, 1);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveHI32x4,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(32)));
               assign(t2,
                      unop(Iop_ReinterpF64asI64,
                           binop(Iop_I64StoF64, rm,
                                 unop(Iop_V128to64,
                                      mkexpr(t1)))));
               assign(t3,
                      unop(Iop_ReinterpF64asI64,
                           binop(Iop_I64StoF64, rm,
                                 unop(Iop_V128HIto64,
                                      mkexpr(t1)))));
               putWReg(wd,
                       triop(Iop_Div64Fx2, rm,
                             binop(Iop_64HLtoV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_64HLtoV128,
                                   mkU64(0x41E0000000000000),
                                   mkU64(0x41E0000000000000))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x19B: { /* FFQR.df */
         switch (df) {
            case 0x00: { /* FFQR.W */
               DIP("FFQR.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFQRW, 1);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_SarN32x4,
                            binop(Iop_InterleaveLO16x8,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(16)));
               assign(t2,
                      binop(Iop_32HLto64,
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(1)))),
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(0))))));
               assign(t3,
                      binop(Iop_32HLto64,
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(3)))),
                            unop(Iop_ReinterpF32asI32,
                                 binop(Iop_I32StoF32, rm,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t1),
                                             mkU8(2))))));
               putWReg(wd,
                       triop(Iop_Div32Fx4, rm,
                             binop(Iop_64HLtoV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_64HLtoV128,
                                   mkU64(0x4700000047000000),
                                   mkU64(0x4700000047000000))));
               break;
            }

            case 0x01: { /* FFQR.D */
               DIP("FFQR.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFQRD, 1);
               t1 = newTemp(Ity_V128);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_SarN64x2,
                            binop(Iop_InterleaveLO32x4,
                                  getWReg(ws),
                                  getWReg(ws)),
                            mkU8(32)));
               assign(t2,
                      unop(Iop_ReinterpF64asI64,
                           binop(Iop_I64StoF64, rm,
                                 unop(Iop_V128to64,
                                      mkexpr(t1)))));
               assign(t3,
                      unop(Iop_ReinterpF64asI64,
                           binop(Iop_I64StoF64, rm,
                                 unop(Iop_V128HIto64,
                                      mkexpr(t1)))));
               putWReg(wd,
                       triop(Iop_Div64Fx2, rm,
                             binop(Iop_64HLtoV128,
                                   mkexpr(t3), mkexpr(t2)),
                             binop(Iop_64HLtoV128,
                                   mkU64(0x41E0000000000000),
                                   mkU64(0x41E0000000000000))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x19C: { /* FTINT_S.df */
         switch (df) { /* FTINT_S.W */
            case 0x00: {
               DIP("FTINT_S.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTINT_SW, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_I32);
               assign(t3,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128,
                                 binop(Iop_CmpUN32Fx4,
                                       getWReg(ws),
                                       getWReg(ws))),
                            binop(Iop_Max32Fx4,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0xCF000000CF000000),
                                        mkU64(0xCF000000CF000000)))));
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_32HLto64,
                            binop(Iop_F32toI32S, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t3), mkU8(1)))),
                            binop(Iop_F32toI32S, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t3), mkU8(0))))));
               assign(t2,
                      binop(Iop_32HLto64,
                            binop(Iop_F32toI32S, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t3), mkU8(3)))),
                            binop(Iop_F32toI32S, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             mkexpr(t3), mkU8(2))))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            case 0x01: {  /* FTINT_S.D */
               DIP("FTINT_S.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTINT_SD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               assign(t3,
                      binop(Iop_AndV128,
                            unop(Iop_NotV128,
                                 binop(Iop_CmpUN64Fx2,
                                       getWReg(ws),
                                       getWReg(ws))),
                            binop(Iop_Max64Fx2,
                                  getWReg(ws),
                                  binop(Iop_64HLtoV128,
                                        mkU64(0xC3E0000000000000),
                                        mkU64(0xC3E0000000000000)))));
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_F64toI64S, rm,
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128to64, mkexpr(t3)))));
               assign(t2,
                      binop(Iop_F64toI64S, rm,
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128HIto64, mkexpr(t3)))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x19D: {/* FTINT_U.df */
         switch (df) { /* FTINT_U.W */
            case 0x00: {
               DIP("FTINT_U.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTINT_UW, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_V128);
               t4 = newTemp(Ity_V128);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_32HLto64,
                            binop(Iop_F32toI32U, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             getWReg(ws), mkU8(1)))),
                            binop(Iop_F32toI32U, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             getWReg(ws), mkU8(0))))));
               assign(t2,
                      binop(Iop_32HLto64,
                            binop(Iop_F32toI32U, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             getWReg(ws), mkU8(3)))),
                            binop(Iop_F32toI32U, rm,
                                  unop(Iop_ReinterpI32asF32,
                                       binop(Iop_GetElem32x4,
                                             getWReg(ws), mkU8(2))))));
               assign(t3,
                      unop(Iop_NotV128,
                           binop(Iop_SarN32x4,
                                 getWReg(ws),
                                 mkU8(31))));
               assign(t4,
                      binop(Iop_CmpLT32Fx4,
                            getWReg(ws),
                            binop(Iop_64HLtoV128,
                                  mkU64(0x4EFFFFFF4EFFFFFF),
                                  mkU64(0x4EFFFFFF4EFFFFFF))));
               putWReg(wd,
                       binop(Iop_OrV128,
                             binop(Iop_AndV128,
                                   mkexpr(t4),
                                   binop(Iop_AndV128,
                                         binop(Iop_64HLtoV128,
                                               mkexpr(t2),
                                               mkexpr(t1)),
                                         mkexpr(t3))),
                             binop(Iop_AndV128,
                                   unop(Iop_NotV128, mkexpr(t4)),
                                   unop(Iop_F32toI32Ux4_RZ,
                                        getWReg(ws)))));
               break;
            }

            case 0x01: {  /* FTINT_U.D */
               DIP("FTINT_U.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wd, FTINT_UD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               IRExpr *rm = get_IR_roundingmode_MSA();
               assign(t1,
                      binop(Iop_F64toI64U, rm,
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128to64,
                                      getWReg(ws)))));
               assign(t2,
                      binop(Iop_F64toI64U, rm,
                            unop(Iop_ReinterpI64asF64,
                                 unop(Iop_V128HIto64,
                                      getWReg(ws)))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x19E: { /* FFINT_S.df */
         t1 = newTemp(Ity_V128);
         assign(t1, getWReg(ws));
         IRExpr *rm = get_IR_roundingmode_MSA();

         switch (df) {
            case 0x00: { /* FFINT_S.W */
               DIP("FFINT_S.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFINTSW, 1);
               IRTemp tmp[4];
               Int i;

               for (i = 0; i < 4; i++) {
                  tmp[i] = newTemp(Ity_F32);
                  assign(tmp[i],
                         binop(Iop_I32StoF32, rm,
                               binop(Iop_GetElem32x4,
                                     mkexpr(t1), mkU8(i))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             binop(Iop_32HLto64,
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[3])),
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[2]))),
                             binop(Iop_32HLto64,
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[1])),
                                   unop(Iop_ReinterpF32asI32,
                                        mkexpr(tmp[0])))));
               break;
            }

            case 0x01: { /* FFINT_S.D */
               DIP("FFINT_S.D w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFINTSD, 1);
               IRTemp tmp[2];
               Int i;

               for (i = 0; i < 2; i++) {
                  tmp[i] = newTemp(Ity_F64);
                  assign(tmp[i],
                         binop(Iop_I64StoF64, rm,
                               binop(Iop_GetElem64x2,
                                     mkexpr(t1), mkU8(i))));
               }

               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             unop(Iop_ReinterpF64asI64,
                                  mkexpr(tmp[1])),
                             unop(Iop_ReinterpF64asI64,
                                  mkexpr(tmp[0]))));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      case 0x19F: { /* FFINT_U.df */
         IRExpr *rm = get_IR_roundingmode_MSA();

         switch (df) {
            case 0x00: { /* FFINT_U.W */
               DIP("FFINT_U.W w%d, w%d", wd, ws);
               calculateMSACSR(ws, wt, FFINT_UW, 1);
               putWReg(wd, unop(Iop_I32UtoF32x4_DEP, getWReg(ws)));
               break;
            }

            case 0x01: { /* FFINT_U.D */
               DIP("FFINT_U.D w%d, w%d",
                   wd, ws);
               calculateMSACSR(ws, wt,
                               FFINT_UD, 1);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               assign(t1,
                      unop(Iop_ReinterpF64asI64,
                           binop(Iop_I64UtoF64, rm,
                                 unop(Iop_V128to64,
                                      getWReg(ws)))));
               assign(t2,
                      unop(Iop_ReinterpF64asI64,
                           binop(Iop_I64UtoF64, rm,
                                 unop(Iop_V128HIto64,
                                      getWReg(ws)))));
               putWReg(wd,
                       binop(Iop_64HLtoV128,
                             mkexpr(t2), mkexpr(t1)));
               break;
            }

            default:
               return -1;
         }

         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_MI10_load(UInt cins, UChar wd, UChar ws)   /* MI10 (0x20) */
{
   IRTemp t1;
   UShort i10;
   UChar df;

   i10 = (cins & 0x03FF0000) >> 16;
   df = cins & 0x00000003;

   switch (df) {
      case 0x00: { /* LD.B */
         DIP("LD.B w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10);
         putWReg(wd, load(Ity_V128, mkexpr(t1)));
         break;
      }

      case 0x01: { /* LD.H */
         DIP("LD.H w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10 << 1);
#if defined (_MIPSEL)
         putWReg(wd, load(Ity_V128, mkexpr(t1)));
#elif defined (_MIPSEB)
         putWReg(wd,
                 unop(Iop_Reverse8sIn16_x8,
                      load(Ity_V128, mkexpr(t1))));
#endif
         break;
      }

      case 0x02: { /* LD.W */
         DIP("LD.W w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10 << 2);
#if defined (_MIPSEL)
         putWReg(wd, load(Ity_V128, mkexpr(t1)));
#elif defined (_MIPSEB)
         putWReg(wd,
                 unop(Iop_Reverse8sIn32_x4,
                      load(Ity_V128, mkexpr(t1))));
#endif
         break;
      }

      case 0x03: { /* LD.D */
         DIP("LD.D w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10 << 3);
#if defined (_MIPSEL)
         putWReg(wd, load(Ity_V128, mkexpr(t1)));
#elif defined (_MIPSEB)
         putWReg(wd,
                 unop(Iop_Reverse8sIn64_x2,
                      load(Ity_V128, mkexpr(t1))));
#endif
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static Int msa_MI10_store(UInt cins, UChar wd, UChar ws)   /* MI10 (0x24) */
{
   IRTemp t1;
   UShort i10;
   UChar df;

   df = cins & 0x00000003;
   i10 = (cins & 0x03FF0000) >> 16;

   switch (df) {
      case 0x00: { /* ST.B */
         DIP("ST.B w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10);
         store(mkexpr(t1), getWReg(wd));
         break;
      }

      case 0x01: { /* ST.H */
         DIP("ST.H w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10 << 1);
#if defined (_MIPSEL)
         store(mkexpr(t1), getWReg(wd));
#elif defined (_MIPSEB)
         store(mkexpr(t1),
               unop(Iop_Reverse8sIn16_x8, getWReg(wd)));
#endif
         break;
      }

      case 0x02: { /* ST.W */
         DIP("ST.W w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10 << 2);
#if defined (_MIPSEL)
         store(mkexpr(t1), getWReg(wd));
#elif defined (_MIPSEB)
         store(mkexpr(t1),
               unop(Iop_Reverse8sIn32_x4, getWReg(wd)));
#endif
         break;
      }

      case 0x03: { /* ST.D */
         DIP("ST.D w%d, %d(r%d)", wd, ws, i10);
         LOAD_STORE_PATTERN_MSA(i10 << 3);
#if defined (_MIPSEL)
         store(mkexpr(t1), getWReg(wd));
#elif defined (_MIPSEB)
         store(mkexpr(t1),
               unop(Iop_Reverse8sIn64_x2, getWReg(wd)));
#endif
         break;
      }

      default:
         return -1;
   }

   return 0;
}

/*------------------------------------------------------------*/
/*---   Disassemble a single MIPS MSA (SIMD) instruction   ---*/
/*---   Return values:                                     ---*/
/*---       0: Success                                     ---*/
/*---      -1: Decode failure (unknown instruction)        ---*/
/*---      -2: Illegal instruction                         ---*/
/*------------------------------------------------------------*/
static Int disMSAInstr_MIPS_WRK ( UInt cins )
{
   UChar minor_opcode, wd, ws;

   vassert(has_msa);
   vassert((cins & 0xFC000000) == 0x78000000);

   minor_opcode = (cins & 0x20) > 0 ? (cins & 0x3C) : (cins & 0x3F);
   wd = (cins & 0x000007C0) >> 6;
   ws = (cins & 0x0000F800) >> 11;

   switch (minor_opcode) {
      case 0x0:
         return msa_I8_logical(cins, wd, ws);

      case 0x01:
         return msa_I8_branch(cins, wd, ws);

      case 0x02:
         return msa_I8_shift(cins, wd, ws);

      case 0x06:
         return msa_I5_06(cins, wd, ws);

      case 0x07:
         return msa_I5_07(cins, wd, ws);

      case 0x09:
         return msa_BIT_09(cins, wd, ws);

      case 0x0A:
         return msa_BIT_0A(cins, wd, ws);

      case 0x0D:
         return msa_3R_0D(cins, wd, ws);

      case 0x0E:
         return msa_3R_0E(cins, wd, ws);

      case 0x0F:
         return msa_3R_0F(cins, wd, ws);

      case 0x10:
         return msa_3R_10(cins, wd, ws);

      case 0x11:
         return msa_3R_11(cins, wd, ws);

      case 0x12:
         return msa_3R_12(cins, wd, ws);

      case 0x13:
         return msa_3R_13(cins, wd, ws);

      case 0x14:
         return msa_3R_14(cins, wd, ws);

      case 0x15:
         return msa_3R_15(cins, wd, ws);

      case 0x19:
         return msa_ELM(cins, wd, ws);

      case 0x1A:
         return msa_3R_1A(cins, wd, ws);

      case 0x1B:
         return msa_3R_1B(cins, wd, ws);

      case 0x1C:
         return msa_3R_1C(cins, wd, ws);

      case 0x1E:
         if ((cins & 0x03000000) == 0)
            return msa_VEC(cins, wd, ws);
         else if ((cins & 0x00200000) == 0)
            return msa_2R(cins, wd, ws);
         else
            return msa_2RF(cins, wd, ws);

      case 0x20:
         return msa_MI10_load(cins, wd, ws);

      case 0x24:
         return msa_MI10_store(cins, wd, ws);
   }

   return -1;
}

/*------------------------------------------------------------*/
/*---              DSP to IR function                      ---*/
/*------------------------------------------------------------*/

extern UInt disDSPInstr_MIPS_WRK ( UInt );

/*------------------------------------------------------------*/
/*---          Disassemble a single instruction            ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR. The instruction is
   located in host memory at guest_instr, and has guest IP of
   guest_PC_curr_instr, which will have been set before the call
   here. */


static UInt disInstr_MIPS_WRK_Special(UInt cins, const VexArchInfo* archinfo,
                                      const VexAbiInfo*  abiinfo, DisResult* dres,
                                      IRStmt** bstmt, IRExpr** lastn)
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5;
   UInt rs, rt, rd, sa, tf, function, trap_code, imm, instr_index, rot, sel;
   /* Additional variables for instruction fields in DSP ASE insructions */
   UInt ac;

   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);
   tf = get_tf(cins);
   sel = get_sel(cins);
   instr_index = get_instr_index(cins);
   trap_code = get_code(cins);
   function = get_function(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   ac = get_acNo(cins);

   switch (function) {
      case 0x00: {  /* SLL */
         DIP("sll r%u, r%u, %u", rd, rt, sa);
         IRTemp tmpRt32 = newTemp(Ity_I32);
         IRTemp tmpSh32 = newTemp(Ity_I32);
         IRTemp tmpRd = newTemp(Ity_I64);

         if (mode64) {
            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Shl32, mkexpr(tmpRt32), mkU8(sa)));
            assign(tmpRd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
            putIReg(rd, mkexpr(tmpRd));
         } else
            SXX_PATTERN(Iop_Shl32);

         break;
      }

      case 0x01: {  /* MOVCI */
         UInt mov_cc = get_mov_cc(cins);

         if (tf == 0) {  /* MOVF */
            DIP("movf r%u, r%u, %u", rd, rs, mov_cc);
            t1 = newTemp(Ity_I1);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I1);

            assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
            assign(t2, IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(23)),
                                        mkU32(0x1)),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(24 + mov_cc)),
                                        mkU32(0x1))
                                 ));
            assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
            putIReg(rd, IRExpr_ITE(mkexpr(t3), getIReg(rs), getIReg(rd)));
         } else if (tf == 1) {  /* MOVT */
            DIP("movt r%u, r%u, %u", rd, rs, mov_cc);
            t1 = newTemp(Ity_I1);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I1);

            assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
            assign(t2, IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(23)),
                                        mkU32(0x1)),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(24 + mov_cc)),
                                        mkU32(0x1))
                                 ));
            assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
            putIReg(rd, IRExpr_ITE(mkexpr(t3), getIReg(rs), getIReg(rd)));
         }

         break;
      }

      case 0x02: {  /* SRL */
         rot = get_rot(cins);

         if (rot) {
            DIP("rotr r%u, r%u, %u", rd, rt, sa);
            putIReg(rd, mkWidenFrom32(ty, genROR32(mkNarrowTo32(ty,
                                                   getIReg(rt)), sa), True));
         } else {
            DIP("srl r%u, r%u, %u", rd, rt, sa);

            if (mode64) {
               IRTemp tmpSh32 = newTemp(Ity_I32);
               IRTemp tmpRt32 = newTemp(Ity_I32);

               assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
               assign(tmpSh32, binop(Iop_Shr32, mkexpr(tmpRt32), mkU8(sa)));
               putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
            } else {
               SXX_PATTERN(Iop_Shr32);
            }
         }

         break;
      }

      case 0x03:  /* SRA */
         DIP("sra r%u, r%u, %u", rd, rt, sa);

         if (mode64) {
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpSh32 = newTemp(Ity_I32);

            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);
            t3 = newTemp(Ity_I64);

            assign(t1, binop(Iop_And64, getIReg(rt),  /* hi */
                             mkU64(0xFFFFFFFF00000000ULL)));

            assign(t2, binop(Iop_Sar64, mkexpr(t1), mkU8(sa)));

            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Sar32, mkexpr(tmpRt32), mkU8(sa)));

            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
         } else {
            SXX_PATTERN(Iop_Sar32);
         }

         break;

      case 0x04: {  /* SLLV */
         DIP("sllv r%u, r%u, r%u", rd, rt, rs);

         if (mode64) {
            IRTemp tmpRs8 = newTemp(Ity_I8);
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpSh32 = newTemp(Ity_I32);
            IRTemp tmp = newTemp(ty);
            assign(tmp, binop(mkSzOp(ty, Iop_And8), getIReg(rs),
                              mkSzImm(ty, 31)));
            assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Shl32, mkexpr(tmpRt32), mkexpr(tmpRs8)));
            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
         } else {
            SXXV_PATTERN(Iop_Shl32);
         }

         break;
      }

      case 0x05: { /* LSA */
         UInt imm2 = (imm & 0xC0) >> 6;

         if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) || has_msa) {
            DIP("lsa r%u, r%u, r%u, imm: 0x%x", rd, rs, rt, imm2);

            if (mode64) {
               DIP("lsa r%u, r%u, r%u, imm: 0x%x", rd, rs, rt, imm2);
               putIReg(rd, unop(Iop_32Sto64,
                                binop(Iop_Add32,
                                      binop(Iop_Shl32,
                                            unop(Iop_64to32, getIReg(rs)),
                                            mkU8(imm2 + 1)),
                                      unop(Iop_64to32, getIReg(rt)))));
               break;
            } else {
               DIP("lsa r%u, r%u, r%u, imm: 0x%x", rd, rs, rt, imm2);
               putIReg(rd, binop(Iop_Add32,
                                 binop(Iop_Shl32,
                                       getIReg(rs), mkU8(imm2 + 1)), getIReg(rt)));
               break;
            }
         } else {
            ILLEGAL_INSTRUCTON;
            break;
         }
      }

      case 0x06: { /* SRLV */
         rot = get_rotv(cins);

         if (rot) {
            DIP("rotrv r%u, r%u, r%u", rd, rt, rs);
            putIReg(rd, mkWidenFrom32(ty, genRORV32(mkNarrowTo32(ty,
                                                    getIReg(rt)), mkNarrowTo32(ty, getIReg(rs))), True));
            break;
         } else {  /* SRLV */
            DIP("srlv r%u, r%u, r%u", rd, rt, rs);

            if (mode64) {
               SXXV_PATTERN64(Iop_Shr32);
            } else {
               SXXV_PATTERN(Iop_Shr32);
            }

            break;
         }
      }

      case 0x07:  /* SRAV */
         DIP("srav r%u, r%u, r%u", rd, rt, rs);

         if (mode64) {
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpSh32 = newTemp(Ity_I32);

            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I8);

            assign(t4, unop(Iop_32to8, binop(Iop_And32,
                                             mkNarrowTo32(ty, getIReg(rs)), mkU32(0x0000001F))));

            assign(t1, binop(Iop_And64, getIReg(rt),  /* hi */
                             mkU64(0xFFFFFFFF00000000ULL)));

            assign(t2, binop(Iop_Sar64, mkexpr(t1), mkexpr(t4)));

            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Sar32, mkexpr(tmpRt32), mkexpr(t4)));

            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
         } else {
            SXXV_PATTERN(Iop_Sar32);
         }

         break;

      case 0x08:  /* JR */
         DIP("jr r%u", rs);
         t0 = newTemp(ty);
         assign(t0, getIReg(rs));
         *lastn = mkexpr(t0);
         break;

      case 0x09:  /* JALR */
         DIP("jalr r%u r%u", rd, rs);

         if (mode64) {
            putIReg(rd, mkU64(guest_PC_curr_instr + 8));
            t0 = newTemp(Ity_I64);
            assign(t0, getIReg(rs));
            *lastn = mkexpr(t0);
         } else {
            putIReg(rd, mkU32(guest_PC_curr_instr + 8));
            t0 = newTemp(Ity_I32);
            assign(t0, getIReg(rs));
            *lastn = mkexpr(t0);
         }

         break;

      case 0x0A: {  /* MOVZ */
         DIP("movz r%u, r%u, r%u", rd, rs, rt);
         t1 = newTemp(ty);
         t2 = newTemp(ty);

         if (mode64) {
            assign(t1, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpEQ64,
                                              getIReg(rt), mkU64(0x0)))));
            assign(t2, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpNE64,
                                              getIReg(rt), mkU64(0x0)))));
            putIReg(rd, binop(Iop_Add64, binop(Iop_And64, getIReg(rs),
                                               mkexpr(t1)), binop(Iop_And64, getIReg(rd), mkexpr(t2))));
         } else {
            assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, getIReg(rt),
                                              mkU32(0x0))));
            assign(t2, unop(Iop_1Sto32, binop(Iop_CmpNE32, getIReg(rt),
                                              mkU32(0x0))));
            putIReg(rd, binop(Iop_Add32, binop(Iop_And32, getIReg(rs),
                                               mkexpr(t1)), binop(Iop_And32, getIReg(rd),
                                                     mkexpr(t2))));
         }

         break;
      }

      case 0x0B: {  /* MOVN */
         DIP("movn r%u, r%u, r%u", rd, rs, rt);
         t1 = newTemp(ty);
         t2 = newTemp(ty);

         if (mode64) {
            assign(t1, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpEQ64,
                                              getIReg(rt), mkU64(0x0)))));
            assign(t2, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpNE64,
                                              getIReg(rt), mkU64(0x0)))));
            putIReg(rd, binop(Iop_Add64, binop(Iop_And64, getIReg(rs),
                                               mkexpr(t2)), binop(Iop_And64, getIReg(rd),
                                                     mkexpr(t1))));
         } else {
            assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, getIReg(rt),
                                              mkU32(0x0))));
            assign(t2, unop(Iop_1Sto32, binop(Iop_CmpNE32, getIReg(rt),
                                              mkU32(0x0))));
            putIReg(rd, binop(Iop_Add32, binop(Iop_And32, getIReg(rs),
                                               mkexpr(t2)), binop(Iop_And32, getIReg(rd),
                                                     mkexpr(t1))));
         }

         break;
      }

      case 0x0C:  /* SYSCALL */
         DIP("syscall");

         if (mode64)
            putPC(mkU64(guest_PC_curr_instr + 4));
         else
            putPC(mkU32(guest_PC_curr_instr + 4));

         dres->jk_StopHere = Ijk_Sys_syscall;
         dres->whatNext    = Dis_StopHere;
         break;

      case 0x0D:  /* BREAK */
         DIP("break 0x%x", trap_code);

         if (mode64)
            jmp_lit64(dres, Ijk_SigTRAP, (guest_PC_curr_instr + 4));
         else
            jmp_lit32(dres, Ijk_SigTRAP, (guest_PC_curr_instr + 4));

         vassert(dres->whatNext == Dis_StopHere);
         break;

      case 0x0F:  /* SYNC */
         DIP("sync 0x%x", sel);
         /* Just ignore it. */
         break;

      case 0x10: {  /* MFHI, CLZ R6 */
         if (((instr_index >> 6) & 0x1f) == 1) { /* CLZ */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("clz r%u, r%u", rd, rs);

               if (mode64) {
                  IRTemp tmpClz32 = newTemp(Ity_I32);
                  IRTemp tmpRs32 = newTemp(Ity_I32);

                  assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
                  assign(tmpClz32, unop(Iop_Clz32, mkexpr(tmpRs32)));
                  putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpClz32), True));
               } else {
                  t1 = newTemp(Ity_I1);
                  assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0)));
                  putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                         mkU32(0x00000020),
                                         unop(Iop_Clz32, getIReg(rs))));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MFHI */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );

            if (0 != retVal ) {
               return -1;
            }

            break;
         } else {
            DIP("mfhi r%u", rd);
            putIReg(rd, getHI());
            break;
         }
      }

      case 0x11:  {  /* MTHI, CLO R6 */
         if (((instr_index >> 6) & 0x1f) == 1) { /* CLO */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("clo r%u, r%u", rd, rs);

               if (mode64) {
                  IRTemp tmpClo32 = newTemp(Ity_I32);
                  IRTemp tmpRs32 = newTemp(Ity_I32);
                  assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));

                  t1 = newTemp(Ity_I1);
                  assign(t1, binop(Iop_CmpEQ32, mkexpr(tmpRs32), mkU32(0xffffffff)));
                  assign(tmpClo32, IRExpr_ITE(mkexpr(t1),
                                              mkU32(0x00000020),
                                              unop(Iop_Clz32, unop(Iop_Not32, mkexpr(tmpRs32)))));

                  putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpClo32), True));
                  break;
               } else {
                  t1 = newTemp(Ity_I1);
                  assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0xffffffff)));
                  putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                         mkU32(0x00000020),
                                         unop(Iop_Clz32,
                                              unop(Iop_Not32, getIReg(rs)))));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MTHI */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );

            if (0 != retVal ) {
               return -1;
            }

            break;
         } else {
            DIP("mthi r%u", rs);
            putHI(getIReg(rs));
            break;
         }
      }

      case 0x12:  {  /* MFLO */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MFLO */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );

            if (0 != retVal ) {
               return -1;
            }

            break;
         } else {
            switch (sa) {
               case 0:
                  DIP("mflo r%u", rd);

                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                        !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                     ILLEGAL_INSTRUCTON
                  }

                  putIReg(rd, getLO());
                  break;

               case 1:
                  DIP("dclz r%u, r%u", rd, rs);
                  t1 = newTemp(Ity_I1);
                  assign(t1, binop(Iop_CmpEQ64, getIReg(rs), mkU64(0)));
                  putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                         mkU64(0x00000040),
                                         unop(Iop_Clz64, getIReg(rs))));
                  break;
            }

            break;
         }
      }

      case 0x13:  {  /* MTLO */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MTLO */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );

            if (0 != retVal ) {
               return -1;
            }

            break;
         } else {
            switch (sa) {
               case 0:
                  DIP("mtlo r%u", rs);

                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                        !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                     ILLEGAL_INSTRUCTON
                  }

                  putLO(getIReg(rs));
                  break;

               case 1:
                  DIP("dclo r%u, r%u", rd, rs);
                  t1 = newTemp(Ity_I1);
                  assign(t1, binop(Iop_CmpEQ64, getIReg(rs),
                                   mkU64(0xffffffffffffffffULL)));
                  putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                         mkU64(0x40),
                                         unop(Iop_Clz64, unop(Iop_Not64,
                                               getIReg(rs)))));
                  break;
            }

            break;
         }
      }

      case 0x15: { /* DLSA */
         UInt imm2 = (imm & 0xC0) >> 6;

         if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) || has_msa) {
            DIP("dlsa r%u, r%u, r%u, imm: 0x%x", rd, rs, rt, imm2);
            putIReg(rd, binop(Iop_Add64,
                              binop(Iop_Shl64, getIReg(rs), mkU8(imm2 + 1)),
                              getIReg(rt)));
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;
      }

      case 0x18:  {  /* MULT */
         switch (sa & 0x3) {
            case 0: {
               if ((1 <= ac) && ( 3 >= ac)) {
                  if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                     /* If DSP is present -> DSP ASE MULT */
                     UInt retVal = disDSPInstr_MIPS_WRK(cins);

                     if (0 != retVal) {
                        return -2;
                     }

                     break;
                  } else {
                     return -2;
                  }
               } else {
                  DIP("mult r%u, r%u", rs, rt);

                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                        !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                     ILLEGAL_INSTRUCTON
                  }

                  t2 = newTemp(Ity_I64);

                  assign(t2, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                                   mkNarrowTo32(ty, getIReg(rt))));

                  putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
                  putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
                  break;
               }
            }

            case 2: { /* MUL R6 */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("mul r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Sto64,
                                      unop(Iop_64to32,
                                           binop(Iop_MullS32,
                                                 unop(Iop_64to32, getIReg(rs)),
                                                 unop(Iop_64to32, getIReg(rt))))));
                  } else {
                     putIReg(rd, unop(Iop_64to32,
                                      binop(Iop_MullS32,
                                            getIReg(rs), getIReg(rt))));
                  }
               } else {
                  ILLEGAL_INSTRUCTON;
               }

               break;
            }

            case 3: {  /* MUH R6 */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("muh r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Sto64,
                                      unop(Iop_64HIto32,
                                           binop(Iop_MullS32,
                                                 unop(Iop_64to32, getIReg(rs)),
                                                 unop(Iop_64to32, getIReg(rt))))));
                  } else {
                     putIReg(rd, unop(Iop_64HIto32,
                                      binop(Iop_MullS32,
                                            getIReg(rs), getIReg(rt))));
                  }
               } else {
                  ILLEGAL_INSTRUCTON;
               }

               break;
            }
         }

         break;
      }

      case 0x19:  {  /* MULTU */
         switch (sa & 0x3) {
            case 0: {
               if ((1 <= ac) && ( 3 >= ac)) {
                  if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                     /* If DSP is present -> DSP ASE MULTU */
                     UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                     if (0 != retVal) {
                        return -2;
                     }

                     break;
                  } else {
                     return -2;
                  }
               } else {
                  DIP("multu r%u, r%u", rs, rt);

                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                        !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                     ILLEGAL_INSTRUCTON
                  }

                  t2 = newTemp(Ity_I64);

                  assign(t2, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                                   mkNarrowTo32(ty, getIReg(rt))));

                  putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
                  putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
                  break;
               }
            }

            case 2: {  /* MULU R6 */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("mulu r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Uto64,
                                      unop(Iop_64to32,
                                           binop(Iop_MullU32,
                                                 unop(Iop_64to32, getIReg(rs)),
                                                 unop(Iop_64to32, getIReg(rt))))));
                  } else {
                     putIReg(rd, unop(Iop_64to32,
                                      binop(Iop_MullU32,
                                            getIReg(rs), getIReg(rt))));
                  }
               } else {
                  ILLEGAL_INSTRUCTON;
               }

               break;
            }

            case 3: {  /* MUHU R6 */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("muhu r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Uto64,
                                      unop(Iop_64HIto32,
                                           binop(Iop_MullU32,
                                                 unop(Iop_64to32, getIReg(rs)),
                                                 unop(Iop_64to32, getIReg(rt))))));
                  } else {
                     putIReg(rd, unop(Iop_64HIto32,
                                      binop(Iop_MullU32,
                                            getIReg(rs), getIReg(rt))));
                  }
               } else {
                  ILLEGAL_INSTRUCTON;
               }

               break;
            }
         }

         break;
      }

      case 0x1A:  /* DIV */
         switch (sa & 0x3) {
            case 0:
               DIP("div r%u, r%u", rs, rt);

               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                     !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                  ILLEGAL_INSTRUCTON
               }

               if (mode64) {
                  t2 = newTemp(Ity_I64);

                  assign(t2, binop(Iop_DivModS32to32,
                                   mkNarrowTo32(ty, getIReg(rs)),
                                   mkNarrowTo32(ty, getIReg(rt))));

                  putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
                  putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
               } else {
                  t1 = newTemp(Ity_I64);

                  assign(t1, binop(Iop_DivModS32to32, getIReg(rs), getIReg(rt)));

                  putHI(unop(Iop_64HIto32, mkexpr(t1)));
                  putLO(unop(Iop_64to32, mkexpr(t1)));
               }

               break;

            case 2:
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("div r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Sto64,
                                      binop(Iop_DivS32,
                                            unop(Iop_64to32, getIReg(rs)),
                                            unop(Iop_64to32, getIReg(rt)))));
                  } else {
                     putIReg(rd, binop(Iop_DivS32, getIReg(rs), getIReg(rt)));
                  }
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 3:
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("mod r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Sto64,
                                      unop(Iop_64HIto32,
                                           binop(Iop_DivModS32to32,
                                                 unop(Iop_64to32, getIReg(rs)),
                                                 unop(Iop_64to32, getIReg(rt))))));
                  } else {
                     t1 = newTemp(Ity_I64);

                     assign(t1, binop(Iop_DivModS32to32, getIReg(rs), getIReg(rt)));
                     putIReg(rd, unop(Iop_64HIto32, mkexpr(t1)));
                  }
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
         }

         break;

      case 0x1B:  /* DIVU */
         switch (sa & 0x3) {
            case 0:
               DIP("divu r%u, r%u", rs, rt);

               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                     !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                  ILLEGAL_INSTRUCTON
               }

               if (mode64) {
                  t1 = newTemp(Ity_I64);

                  assign(t1, binop(Iop_DivModU32to32,
                                   mkNarrowTo32(ty, getIReg(rs)),
                                   mkNarrowTo32(ty, getIReg(rt))));

                  putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t1)), True));
                  putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t1)), True));
               } else {
                  t1 = newTemp(Ity_I64);

                  assign(t1, binop(Iop_DivModU32to32, getIReg(rs), getIReg(rt)));
                  putHI(unop(Iop_64HIto32, mkexpr(t1)));
                  putLO(unop(Iop_64to32, mkexpr(t1)));
               }

               break;

            case 2:
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("divu r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Sto64,
                                      binop(Iop_DivU32,
                                            unop(Iop_64to32, getIReg(rs)),
                                            unop(Iop_64to32, getIReg(rt)))));
                  } else {
                     putIReg(rd, binop(Iop_DivU32, getIReg(rs), getIReg(rt)));
                  }

                  break;
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 3:
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("modu r%u, r%u, r%u", rs, rt, rd);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Uto64,
                                      unop(Iop_64HIto32,
                                           binop(Iop_DivModU32to32,
                                                 unop(Iop_64to32, getIReg(rs)),
                                                 unop(Iop_64to32, getIReg(rt))))));
                  } else {
                     t1 = newTemp(Ity_I64);

                     assign(t1, binop(Iop_DivModU32to32, getIReg(rs), getIReg(rt)));
                     putIReg(rd, unop(Iop_64HIto32, mkexpr(t1)));
                  }
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
         }

         break;

      case 0x1C:  /* Doubleword Multiply - DMULT; MIPS64 */
         switch (sa) {
            case 0:
               DIP("dmult r%u, r%u", rs, rt);

               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                     !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                  ILLEGAL_INSTRUCTON
               }

               t0 = newTemp(Ity_I128);

               assign(t0, binop(Iop_MullS64, getIReg(rs), getIReg(rt)));

               putHI(unop(Iop_128HIto64, mkexpr(t0)));
               putLO(unop(Iop_128to64, mkexpr(t0)));
               break;

            case 2: /* DMUL */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dmul r%u, r%u, r%u", rd, rs, rt);
                  putIReg(rd, unop(Iop_128to64,
                                   binop(Iop_MullS64, getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 3: /* DMUH */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dmuh r%u, r%u, r%u", rd, rs, rt);
                  putIReg(rd, unop(Iop_128HIto64,
                                   binop(Iop_MullS64, getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
         }

         break;

      case 0x1D:  /* Doubleword Multiply Unsigned - DMULTU; MIPS64 */
         switch (sa) {
            case 0:
               DIP("dmultu r%u, r%u", rs, rt);

               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                     !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                  ILLEGAL_INSTRUCTON
               }

               t0 = newTemp(Ity_I128);

               assign(t0, binop(Iop_MullU64, getIReg(rs), getIReg(rt)));

               putHI(unop(Iop_128HIto64, mkexpr(t0)));
               putLO(unop(Iop_128to64, mkexpr(t0)));
               break;

            case 2: /* DMULU */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dmulu r%u, r%u, r%u", rd, rs, rt);
                  putIReg(rd, unop(Iop_128to64,
                                   binop(Iop_MullU64, getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 3: /* DMUHU */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dmuhu r%u, r%u, r%u", rd, rs, rt);
                  putIReg(rd, unop(Iop_128HIto64,
                                   binop(Iop_MullU64, getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
         }

         break;

      case 0x1E:  /* Doubleword Divide DDIV; MIPS64 */
         switch (sa) {
            case 0:
               DIP("ddiv r%u, r%u", rs, rt);

               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                     !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                  ILLEGAL_INSTRUCTON
               }

               t1 = newTemp(Ity_I128);

               assign(t1, binop(Iop_DivModS64to64, getIReg(rs), getIReg(rt)));

               putHI(unop(Iop_128HIto64, mkexpr(t1)));
               putLO(unop(Iop_128to64, mkexpr(t1)));
               break;

            case 2: /* DDIV r6 */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("ddiv r%u, r%u, r%u", rs, rt, rd);
                  putIReg(rd, unop(Iop_128to64,
                                   binop(Iop_DivModS64to64,
                                         getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 3: /* DMOD r6 */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dmod r%u, r%u, r%u", rs, rt, rd);
                  t2 = newTemp(Ity_I128);
                  assign(t2, binop(Iop_DivModS64to64, getIReg(rs), getIReg(rt)));
                  putIReg(rd, unop(Iop_128HIto64, mkexpr(t2)));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
         }

         break;

      case 0x1F:  /* Doubleword Divide Unsigned DDIVU; MIPS64 check this */
         switch (sa) {
            case 0:
               DIP("ddivu r%u, r%u", rs, rt);

               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) &&
                     !VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps)) {
                  ILLEGAL_INSTRUCTON
               }

               t1 = newTemp(Ity_I128);

               assign(t1, binop(Iop_DivModU64to64, getIReg(rs), getIReg(rt)));

               putHI(unop(Iop_128HIto64, mkexpr(t1)));
               putLO(unop(Iop_128to64, mkexpr(t1)));
               break;

            case 2:
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("ddivu r%u, r%u, r%u", rs, rt, rd);
                  putIReg(rd, unop(Iop_128to64, binop(Iop_DivModU64to64,
                                                      getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 3:
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dmodu r%u, r%u, r%u", rs, rt, rd);
                  putIReg(rd, unop(Iop_128HIto64, binop(Iop_DivModU64to64,
                                                        getIReg(rs), getIReg(rt))));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
         }

         break;

      case 0x20: {  /* ADD */
         DIP("add r%u, r%u, r%u", rd, rs, rt);
         IRTemp tmpRs32 = newTemp(Ity_I32);
         IRTemp tmpRt32 = newTemp(Ity_I32);

         assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
         assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         /* dst = src0 + src1
               if (sign(src0 ) != sign(src1 ))
               goto no overflow;
               if (sign(dst) == sign(src0 ))
               goto no overflow;
               we have overflow! */

         assign(t0, binop(Iop_Add32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
         assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
         assign(t2, unop(Iop_1Uto32,
                         binop(Iop_CmpEQ32,
                               binop(Iop_And32, mkexpr(t1), mkU32(0x80000000)),
                               mkU32(0x80000000))));

         assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
         assign(t4, unop(Iop_1Uto32,
                         binop(Iop_CmpNE32,
                               binop(Iop_And32, mkexpr(t3), mkU32(0x80000000)),
                               mkU32(0x80000000))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ32,
                                binop(Iop_Or32, mkexpr(t2), mkexpr(t4)),
                                mkU32(0)),
                          Ijk_SigFPE_IntOvf,
                          mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                          IRConst_U32(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd,  mkWidenFrom32(ty, mkexpr(t0), True));
         break;
      }

      case 0x21:  /* ADDU */
         DIP("addu r%u, r%u, r%u", rd, rs, rt);

         if (mode64) {
            ALU_PATTERN64(Iop_Add32);
         } else {
            ALU_PATTERN(Iop_Add32);
         }

         break;

      case 0x22: {  /* SUB */
         DIP("sub r%u, r%u, r%u", rd, rs, rt);
         IRTemp tmpRs32 = newTemp(Ity_I32);
         IRTemp tmpRt32 = newTemp(Ity_I32);

         assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
         assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         /* dst = src0 + (-1 * src1)
               if(sign(src0 ) != sign((-1 * src1) ))
               goto no overflow;
               if(sign(dst) == sign(src0 ))
               goto no overflow;
               we have overflow! */

         assign(t5, binop(Iop_Mul32, mkexpr(tmpRt32), mkU32(-1)));
         assign(t0, binop(Iop_Add32, mkexpr(tmpRs32), mkexpr(t5)));
         assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32), mkexpr(t5)));
         assign(t2, unop(Iop_1Sto32, binop(Iop_CmpEQ32, binop(Iop_And32,
                                           mkexpr(t1), mkU32(0x80000000)), mkU32(0x80000000))));

         assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
         assign(t4, unop(Iop_1Sto32, binop(Iop_CmpNE32, binop(Iop_And32,
                                           mkexpr(t3), mkU32(0x80000000)), mkU32(0x80000000))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ32, binop(Iop_Or32, mkexpr(t2),
                                mkexpr(t4)), mkU32(0)), Ijk_SigFPE_IntOvf,
                          mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                          IRConst_U32(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd, mkWidenFrom32(ty, mkexpr(t0), True));
         break;
      }

      case 0x23:  /* SUBU */
         DIP("subu r%u, r%u, r%u", rd, rs, rt);

         if (mode64) {
            ALU_PATTERN64(Iop_Sub32);
         } else {
            ALU_PATTERN(Iop_Sub32);
         }

         break;

      case 0x24:  /* AND */
         DIP("and r%u, r%u, r%u", rd, rs, rt);

         if (mode64) {
            ALU_PATTERN(Iop_And64);
         } else {
            ALU_PATTERN(Iop_And32);
         }

         break;

      case 0x25:  /* OR */
         DIP("or r%u, r%u, r%u", rd, rs, rt);

         if (mode64) {
            ALU_PATTERN(Iop_Or64);
         } else {
            ALU_PATTERN(Iop_Or32);
         }

         break;

      case 0x26:  /* XOR */
         DIP("xor r%u, r%u, r%u", rd, rs, rt);

         if (mode64) {
            ALU_PATTERN(Iop_Xor64);
         } else {
            ALU_PATTERN(Iop_Xor32);
         }

         break;

      case 0x27:  /* NOR */
         DIP("nor r%u, r%u, r%u", rd, rs, rt);

         if (mode64)
            putIReg(rd, unop(Iop_Not64, binop(Iop_Or64, getIReg(rs),
                                              getIReg(rt))));
         else
            putIReg(rd, unop(Iop_Not32, binop(Iop_Or32, getIReg(rs),
                                              getIReg(rt))));

         break;

      case 0x2A:  /* SLT */
         DIP("slt r%u, r%u, r%u", rd, rs, rt);

         if (mode64)
            putIReg(rd, unop(Iop_1Uto64, binop(Iop_CmpLT64S, getIReg(rs),
                                               getIReg(rt))));
         else
            putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                               getIReg(rt))));

         break;

      case 0x2B:  /* SLTU */
         DIP("sltu r%u, r%u, r%u", rd, rs, rt);

         if (mode64)
            putIReg(rd, unop(Iop_1Uto64, binop(Iop_CmpLT64U, getIReg(rs),
                                               getIReg(rt))));
         else
            putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                               getIReg(rt))));

         break;

      case 0x2C: {  /* Doubleword Add - DADD; MIPS64 */
         DIP("dadd r%u, r%u, r%u", rd, rs, rt);
         IRTemp tmpRs64 = newTemp(Ity_I64);
         IRTemp tmpRt64 = newTemp(Ity_I64);

         assign(tmpRs64, getIReg(rs));
         assign(tmpRt64, getIReg(rt));

         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I64);
         /* dst = src0 + src1
               if(sign(src0 ) != sign(src1 ))
               goto no overflow;
               if(sign(dst) == sign(src0 ))
               goto no overflow;
               we have overflow! */

         assign(t0, binop(Iop_Add64, mkexpr(tmpRs64), mkexpr(tmpRt64)));
         assign(t1, binop(Iop_Xor64, mkexpr(tmpRs64), mkexpr(tmpRt64)));
         assign(t2, unop(Iop_1Uto64,
                         binop(Iop_CmpEQ64,
                               binop(Iop_And64, mkexpr(t1),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         assign(t3, binop(Iop_Xor64, mkexpr(t0), mkexpr(tmpRs64)));
         assign(t4, unop(Iop_1Uto64,
                         binop(Iop_CmpNE64,
                               binop(Iop_And64, mkexpr(t3),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ64,
                                binop(Iop_Or64, mkexpr(t2), mkexpr(t4)),
                                mkU64(0)),
                          Ijk_SigFPE_IntOvf,
                          IRConst_U64(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd,  mkexpr(t0));
         break;
      }

      case 0x2D:  /* Doubleword Add Unsigned - DADDU; MIPS64 */
         DIP("daddu r%u, r%u, r%u", rd, rs, rt);
         ALU_PATTERN(Iop_Add64);
         break;

      case 0x2E: {  /* Doubleword Subtract - DSUB; MIPS64 */
         DIP("dsub r%u, r%u, r%u", rd, rs, rt);
         IRTemp tmpRs64 = newTemp(Ity_I64);
         IRTemp tmpRt64 = newTemp(Ity_I64);

         assign(tmpRs64, getIReg(rs));
         assign(tmpRt64, getIReg(rt));
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         /* dst = src0 + (-1 * src1)
               if(sign(src0 ) != sign((-1 * src1) ))
               goto no overflow;
               if(sign(dst) == sign(src0 ))
               goto no overflow;
               we have overflow! */

         assign(t5, binop(Iop_Mul64,
                          mkexpr(tmpRt64),
                          mkU64(0xffffffffffffffffULL)));
         assign(t0, binop(Iop_Add64, mkexpr(tmpRs64), mkexpr(t5)));
         assign(t1, binop(Iop_Xor64, mkexpr(tmpRs64), mkexpr(t5)));
         assign(t2, unop(Iop_1Sto64,
                         binop(Iop_CmpEQ64,
                               binop(Iop_And64,
                                     mkexpr(t1),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         assign(t3, binop(Iop_Xor64, mkexpr(t0), mkexpr(tmpRs64)));
         assign(t4, unop(Iop_1Sto64,
                         binop(Iop_CmpNE64,
                               binop(Iop_And64,
                                     mkexpr(t3),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ64, binop(Iop_Or64, mkexpr(t2),
                                mkexpr(t4)), mkU64(0)), Ijk_SigFPE_IntOvf,
                          IRConst_U64(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd, binop(Iop_Sub64, getIReg(rs), getIReg(rt)));
         break;
      }

      case 0x2F:  /* Doubleword Subtract Unsigned - DSUBU; MIPS64 */
         DIP("dsub r%u, r%u,r%u", rd, rt, rt);
         ALU_PATTERN(Iop_Sub64);
         break;

      case 0x30: {  /* TGE */
         DIP("tge r%u, r%u %u", rs, rt, trap_code);

         if (mode64) {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         }

         break;
      }

      case 0x31: {  /* TGEU */
         DIP("tgeu r%u, r%u %u", rs, rt, trap_code);

         if (mode64) {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         }

         break;
      }

      case 0x32: {  /* TLT */
         DIP("tlt r%u, r%u %u", rs, rt, trap_code);

         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT64S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }

         break;
      }

      case 0x33: {  /* TLTU */
         DIP("tltu r%u, r%u %u", rs, rt, trap_code);

         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT64U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg (rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }

         break;
      }

      case 0x34: {  /* TEQ */
         DIP("teq r%u, r%u, %u", rs, rt, trap_code);

         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpEQ64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }

         break;
      }

      case 0x35: {   /* SELEQZ */
         if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            DIP("seleqz r%u, r%u, r%u", rd, rs, rt);

            if (mode64) {
               putIReg(rd, binop(Iop_And64,
                                 unop(Iop_Not64,
                                      unop(Iop_CmpwNEZ64, getIReg(rt))),
                                 getIReg(rs)));
            } else {
               putIReg(rd, binop(Iop_And32,
                                 unop(Iop_Not32,
                                      unop(Iop_CmpwNEZ32, getIReg(rt))),
                                 getIReg(rs)));
            }
         } else {
            ILLEGAL_INSTRUCTON;
         }

         break;
      }

      case 0x36: {  /* TNE */
         DIP("tne r%u, r%u %u", rs, rt, trap_code);

         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpNE64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpNE64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpNE64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }

         break;
      }

      case 0x37:  { /* SELNEZ */
         if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            DIP("selnez r%u, r%u, r%u", rd, rs, rt);

            if (mode64) {
               putIReg(rd, binop(Iop_And64,
                                 unop(Iop_CmpwNEZ64, getIReg(rt)), getIReg(rs)));
            } else {
               putIReg(rd, binop(Iop_And32,
                                 unop(Iop_CmpwNEZ32, getIReg(rt)), getIReg(rs)));
            }
         } else {
            ILLEGAL_INSTRUCTON;
         }

         break;
      }

      case 0x14:
      case 0x16:
      case 0x17:  /* DSLLV, DROTRV:DSRLV, DSRAV */
      case 0x38:
      case 0x3A:
      case 0x3B:  /* DSLL, DROTL:DSRL, DSRA  */
      case 0x3C:
      case 0x3E:
      case 0x3F:  /* DSLL32, DROTR32:DSRL32, DSRA32 */
         if (dis_instr_shrt(cins))
            break;

         return -1;

      default:
         return -1;
   }

   return 0;
}

static UInt disInstr_MIPS_WRK_Special2(UInt cins, const VexArchInfo* archinfo,
                                       const VexAbiInfo*  abiinfo, DisResult* dres,
                                       IRStmt** bstmt, IRExpr** lastn)
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6;
   UInt rs, rt, rd, function;
   /* Additional variables for instruction fields in DSP ASE insructions */
   UInt ac;

   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   function = get_function(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   ac = get_acNo(cins);

   switch (function) {
         /* Cavium Specific instructions */
      case 0x03:
      case 0x32:
      case 0x33:  /* DMUL, CINS , CINS32 */
      case 0x3A:
      case 0x3B:
      case 0x2B:  /* EXT,  EXT32, SNE    */

         /* CVM Compare Instructions */
      case 0x2A:
      case 0x2E:
      case 0x2F:  /* SEQ,  SEQI,  SNEI   */

         /* CPU Load, Store, Memory, and Control Instructions */
      case 0x18:
      case 0x19:             /* SAA, SAAD */
      case 0x1F:                        /* LAA, LAAD, LAI, LAID */
      case 0x28:
      case 0x2C:
      case 0x2D:  /* BADDU, POP, DPOP */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            if (dis_instr_CVM(cins))
               break;

            return -1;
         } else {
            return -1;
         }

         break;

      case 0x02: {  /* MUL */
         DIP("mul r%u, r%u, r%u", rd, rs, rt);

         if (mode64) {
            IRTemp tmpRs32 = newTemp(Ity_I32);
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpRes = newTemp(Ity_I32);

            assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpRes, binop(Iop_Mul32,
                                 mkexpr(tmpRs32), mkexpr(tmpRt32)));
            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpRes), True));
         } else
            putIReg(rd, binop(Iop_Mul32, getIReg(rs), getIReg(rt)));

         break;
      }

      case 0x00: {  /* MADD */
         if (mode64) {
            DIP("madd r%u, r%u", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                             mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Add64, mkexpr(t3), mkexpr(t4)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MADD */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }
            } else {
               DIP("madd r%u, r%u", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I32);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));

               assign(t4, binop(Iop_Add32, mkexpr(t2), unop(Iop_64to32,
                                mkexpr(t3))));

               assign(t5, unop(Iop_1Uto32, binop(Iop_CmpLT32U, mkexpr(t4),
                                                 unop(Iop_64to32, mkexpr(t3)))));
               assign(t6, binop(Iop_Add32, mkexpr(t5), mkexpr(t1)));

               putHI(binop(Iop_Add32, mkexpr(t6), unop(Iop_64HIto32,
                                                       mkexpr(t3))));
               putLO(mkexpr(t4));
               break;
            }
         }

         break;
      }

      case 0x01: {  /* MADDU */
         if (mode64) {
            DIP("maddu r%u, r%u", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                             mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Add64, mkexpr(t3), mkexpr(t4)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MADDU */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }
            } else {
               DIP("maddu r%u, r%u", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I32);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));

               assign(t4, binop(Iop_Add32, mkexpr(t2), unop(Iop_64to32,
                                mkexpr(t3))));
               assign(t5, unop(Iop_1Uto32, binop(Iop_CmpLT32U, mkexpr(t4),
                                                 unop(Iop_64to32, mkexpr(t3)))));
               assign(t6, binop(Iop_Add32, mkexpr(t5), mkexpr(t1)));

               putHI(binop(Iop_Add32, mkexpr(t6), unop(Iop_64HIto32,
                                                       mkexpr(t3))));
               putLO(mkexpr(t4));
               break;
            }
         }

         break;
      }

      case 0x04: {  /* MSUB */
         if (mode64) {
            DIP("msub r%u, r%u", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                             mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Sub64, mkexpr(t4), mkexpr(t3)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MSUB */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }
            } else {
               DIP("msub r%u, r%u", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I1);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
               assign(t4, unop(Iop_64to32, mkexpr(t3)));  /* new lo */

               /* if lo<lo(mul) hi = hi - 1 */
               assign(t5, binop(Iop_CmpLT32U,
                                mkexpr(t2),
                                mkexpr(t4)));

               assign(t6, IRExpr_ITE(mkexpr(t5),
                                     binop(Iop_Sub32, mkexpr(t1), mkU32(0x1)),
                                     mkexpr(t1)));

               putHI(binop(Iop_Sub32, mkexpr(t6), unop(Iop_64HIto32,
                                                       mkexpr(t3))));
               putLO(binop(Iop_Sub32, mkexpr(t2), mkexpr(t4)));
               break;
            }
         }

         break;
      }

      case 0x05: {  /* MSUBU */
         if (mode64) {
            DIP("msubu r%u, r%u", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                             mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Sub64, mkexpr(t4), mkexpr(t3)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MSUBU */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }
            } else {
               DIP("msubu r%u, r%u", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I1);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
               assign(t4, unop(Iop_64to32, mkexpr(t3)));  /* new lo */

               /* if lo<lo(mul) hi = hi - 1 */
               assign(t5, binop(Iop_CmpLT32U,
                                mkexpr(t2),
                                mkexpr(t4)));

               assign(t6, IRExpr_ITE(mkexpr(t5),
                                     binop(Iop_Sub32,
                                           mkexpr(t1),
                                           mkU32(0x1)),
                                     mkexpr(t1)));

               putHI(binop(Iop_Sub32, mkexpr(t6), unop(Iop_64HIto32,
                                                       mkexpr(t3))));
               putLO(binop(Iop_Sub32, mkexpr(t2), mkexpr(t4)));
               break;
            }
         }

         break;
      }

      case 0x6:  /* dmul MIPS64 - Netlogic */
         DIP("dmul r%u, r%u, r%u", rd, rs, rt);
         t0 = newTemp(Ity_I128);

         assign(t0, binop(Iop_MullU64, getIReg(rs), getIReg(rt)));

         putIReg(rd, unop(Iop_128to64, mkexpr(t0)));
         break;

      case 0x10:  /* LDADDW - Swap Word - Netlogic */
         DIP("ldaddw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);

         /* v = GPR[rt] */
         assign(t0, mkNarrowTo32(ty, getIReg(rt)));

         /* GPR[rt] = memory[base]; */
         assign(t1, load(Ity_I32, getIReg(rs)));
         putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));

         /* memory[base] = memory[base] + v; */
         store(getIReg(rs), binop(Iop_Add32, mkexpr(t0), mkexpr(t1)));
         break;

      case 0x12:  /* LDADDD - Swap Word - Netlogic */
         DIP("ldaddw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);

         /*  v = GPR[rt] */
         assign(t0, getIReg(rt));

         /* GPR[rt] = memory[base]; */
         assign(t1, load(Ity_I64, getIReg(rs)));
         putIReg(rt, mkexpr(t1));

         /* memory[base] = memory[base] + v; */
         store(getIReg(rs), binop(Iop_Add64, mkexpr(t0), mkexpr(t1)));
         break;

      case 0x14:  /* SWAPW - Swap Word - Netlogic */
         DIP("swapw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         assign(t0, mkNarrowTo32(ty, getIReg(rt)));
         assign(t1, load(Ity_I32, getIReg(rs)));
         putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));
         store(getIReg(rs), mkexpr(t0));
         break;

      case 0x16:  /* SWAPD - Swap Double - Netlogic */
         DIP("swapw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         assign(t0, getIReg(rt));
         assign(t1, load(Ity_I64, getIReg(rs)));
         putIReg(rt, mkexpr(t1));
         store(getIReg(rs), mkexpr(t0));
         break;

      case 0x20: {  /* CLZ */
         DIP("clz r%u, r%u", rd, rs);

         if (mode64) {
            IRTemp tmpClz32 = newTemp(Ity_I32);
            IRTemp tmpRs32 = newTemp(Ity_I32);

            assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
            assign(tmpClz32, unop(Iop_Clz32, mkexpr(tmpRs32)));
            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpClz32), True));
         } else {
            t1 = newTemp(Ity_I1);
            assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0)));
            putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                   mkU32(0x00000020),
                                   unop(Iop_Clz32, getIReg(rs))));
         }

         break;
      }

      case 0x21: {  /* CLO */
         DIP("clo r%u, r%u", rd, rs);

         if (mode64) {
            IRTemp tmpClo32 = newTemp(Ity_I32);
            IRTemp tmpRs32 = newTemp(Ity_I32);
            assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));

            t1 = newTemp(Ity_I1);
            assign(t1, binop(Iop_CmpEQ32, mkexpr(tmpRs32), mkU32(0xffffffff)));
            assign(tmpClo32, IRExpr_ITE(mkexpr(t1),
                                        mkU32(0x00000020),
                                        unop(Iop_Clz32, unop(Iop_Not32, mkexpr(tmpRs32)))));

            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpClo32), True));
            break;
         } else {
            t1 = newTemp(Ity_I1);
            assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0xffffffff)));
            putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                   mkU32(0x00000020),
                                   unop(Iop_Clz32,
                                        unop(Iop_Not32, getIReg(rs)))));
            break;
         }
      }

      case 0x24:  /* Count Leading Zeros in Doubleword - DCLZ; MIPS64 */
         DIP("dclz r%u, r%u", rd, rs);
         t1 = newTemp(Ity_I1);
         assign(t1, binop(Iop_CmpEQ64, getIReg(rs), mkU64(0)));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                mkU64(0x00000040),
                                unop(Iop_Clz64, getIReg(rs))));
         break;

      case 0x25:  /* Count Leading Ones in Doubleword - DCLO; MIPS64 */
         DIP("dclo r%u, r%u", rd, rs);
         t1 = newTemp(Ity_I1);
         assign(t1, binop(Iop_CmpEQ64, getIReg(rs),
                          mkU64(0xffffffffffffffffULL)));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                mkU64(0x40),
                                unop(Iop_Clz64, unop(Iop_Not64,
                                      getIReg(rs)))));
         break;

      default:
         return -1;
   }

   return 0;
}

static UInt disInstr_MIPS_WRK_Special3(UInt cins, const VexArchInfo* archinfo,
                                       const VexAbiInfo*  abiinfo, DisResult* dres,
                                       IRStmt** bstmt, IRExpr** lastn)

{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6;
   UInt rs, rt, rd, sa, function, imm, instr_index, msb, lsb, size;
   /* Additional variables for instruction fields in DSP ASE insructions */

   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);
   instr_index = get_instr_index(cins);
   function = get_function(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   switch (function) {
      case 0x01: {  /* Doubleword Extract Bit Field - DEXTM; MIPS64r2 */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         UInt srcPos = lsb;
         UInt dstSz = msb + 33;
         t1 = newTemp(Ity_I64);
         DIP("dextm r%u, r%u, %u, %u", rt, rs, lsb, msb + 1);

         UChar lsAmt = 64 - (srcPos + dstSz);  /* left shift amount; */
         UChar rsAmt = 64 - dstSz;  /* right shift amount; */

         assign(t1, binop(Iop_Shl64, getIReg(rs), mkU8(lsAmt)));
         putIReg(rt, binop(Iop_Shr64, mkexpr(t1), mkU8(rsAmt)));

         break;
      }

      case 0x02: {  /* Doubleword Extract Bit Field Upper - DEXTU; MIPS64r2 */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         UInt srcPos = lsb + 32;
         UInt dstSz = msb + 1;
         DIP("dextu r%u, r%u, %u, %u", rt, rs, srcPos, dstSz);
         t1 = newTemp(Ity_I64);

         vassert(srcPos >= 32 && srcPos < 64);
         vassert(dstSz > 0 && dstSz <= 32);
         vassert((srcPos + dstSz) > 32 && (srcPos + dstSz) <= 64);

         UChar lsAmt = 64 - (srcPos + dstSz);  /* left shift amount; */
         UChar rsAmt = 64 - dstSz;  /* right shift amount; */

         assign(t1, binop(Iop_Shl64, getIReg(rs), mkU8(lsAmt)));
         putIReg(rt, binop(Iop_Shr64, mkexpr(t1), mkU8(rsAmt)));
         break;
      }

      case 0x05: {  /* Doubleword Insert Bit Field Middle - DINSM; MIPS64r2 */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         UInt dstPos = lsb;
         UInt srcSz = msb - lsb + 33;
         t1 = newTemp(ty);
         t2 = newTemp(ty);
         t3 = newTemp(ty);
         t4 = newTemp(ty);
         IRTemp tmpT1 = newTemp(ty);
         IRTemp tmpT2 = newTemp(ty);
         IRTemp tmpT3 = newTemp(ty);
         IRTemp tmpT4 = newTemp(ty);
         IRTemp tmpT5 = newTemp(ty);
         IRTemp tmpT6 = newTemp(ty);
         IRTemp tmpT7 = newTemp(ty);
         IRTemp tmpRs = newTemp(ty);
         IRTemp tmpRt = newTemp(ty);
         IRTemp tmpRd = newTemp(ty);

         assign(tmpRs, getIReg(rs));
         assign(tmpRt, getIReg(rt));
         DIP("dinsm r%u, r%u, %u, %u", rt, rs, lsb, msb);

         UChar lsAmt = dstPos + srcSz - 1;   /* left shift amount; */
         UChar rsAmt = dstPos + srcSz - 1;   /* right shift amount; */

         assign(t1, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(rsAmt)));
         assign(tmpT1, binop(Iop_Shr64, mkexpr(t1), mkU8(1)));
         assign(t2, binop(Iop_Shl64, mkexpr(tmpT1), mkU8(lsAmt)));
         assign(tmpT2, binop(Iop_Shl64, mkexpr(t2), mkU8(1)));

         lsAmt = 63 - dstPos; /* left shift amount; */
         rsAmt = 63 - dstPos; /* right shift amount; */

         assign(t3, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(lsAmt)));
         assign(tmpT3, binop(Iop_Shl64, mkexpr(t3), mkU8(1)));
         assign(t4, binop(Iop_Shr64, mkexpr(tmpT3), mkU8(rsAmt)));
         assign(tmpT4, binop(Iop_Shr64, mkexpr(t4), mkU8(1)));

         /* extract size from src register */
         lsAmt = 64 - srcSz;  /* left shift amount; */
         rsAmt = 64 - (lsb + srcSz);   /* right shift amount; */

         assign(tmpT5, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
         assign(tmpT6, binop(Iop_Shr64, mkexpr(tmpT5), mkU8(rsAmt)));

         assign(tmpT7, binop(Iop_Or64, mkexpr(tmpT2), mkexpr(tmpT4)));
         assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT6), mkexpr(tmpT7)));
         putIReg(rt, mkexpr(tmpRd));
         break;
      }

      case 0x06: {  /* Doubleword Insert Bit Field Upper - DINSU; MIPS64r2 */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         UInt dstPos = lsb + 32;
         UInt srcSz = msb - lsb + 1;
         IRTemp tmpT1 = newTemp(ty);
         IRTemp tmpT2 = newTemp(ty);
         IRTemp tmpT3 = newTemp(ty);
         IRTemp tmpT4 = newTemp(ty);
         IRTemp tmpT5 = newTemp(ty);
         IRTemp tmpT6 = newTemp(ty);
         IRTemp tmpT7 = newTemp(ty);
         IRTemp tmpT8 = newTemp(ty);
         IRTemp tmpT9 = newTemp(ty);
         IRTemp tmpRs = newTemp(ty);
         IRTemp tmpRt = newTemp(ty);
         IRTemp tmpRd = newTemp(ty);

         assign(tmpRs, getIReg(rs));
         assign(tmpRt, getIReg(rt));
         DIP("dinsu r%u, r%u, %u, %u", rt, rs, lsb, msb);

         UChar lsAmt = 64 - srcSz;  /* left shift amount; */
         UChar rsAmt = 64 - (dstPos + srcSz);  /* right shift amount; */
         assign(tmpT1, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
         assign(tmpT2, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(rsAmt)));

         lsAmt = 64 - dstPos;  /* left shift amount; */
         rsAmt = 64 - dstPos;  /* right shift amount; */
         assign(tmpT3, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(lsAmt)));
         assign(tmpT4, binop(Iop_Shr64, mkexpr(tmpT3), mkU8(rsAmt)));

         lsAmt = dstPos;  /* left shift amount; */
         rsAmt = srcSz;  /* right shift amount; */
         assign(tmpT5, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(rsAmt)));
         assign(tmpT6, binop(Iop_Shr64, mkexpr(tmpT5), mkU8(lsAmt)));

         assign(tmpT7, binop(Iop_Shl64, mkexpr(tmpT6), mkU8(rsAmt)));
         assign(tmpT8, binop(Iop_Shl64, mkexpr(tmpT7), mkU8(lsAmt)));

         assign(tmpT9, binop(Iop_Or64, mkexpr(tmpT8), mkexpr(tmpT4)));
         assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT2), mkexpr(tmpT9)));
         putIReg(rt, mkexpr(tmpRd));
         break;
      }

      case 0x07: {  /* Doubleword Insert Bit Field - DINS; MIPS64r2 */
         IRTemp tmp1 = newTemp(ty);
         IRTemp tmpT1 = newTemp(ty);
         IRTemp tmpT2 = newTemp(ty);
         IRTemp tmpT3 = newTemp(ty);
         IRTemp tmpT4 = newTemp(ty);
         IRTemp tmpT5 = newTemp(ty);
         IRTemp tmpT6 = newTemp(ty);
         IRTemp tmpT7 = newTemp(ty);
         IRTemp tmpT8 = newTemp(ty);
         IRTemp tmpT9 = newTemp(ty);
         IRTemp tmp = newTemp(ty);
         IRTemp tmpRs = newTemp(ty);
         IRTemp tmpRt = newTemp(ty);
         IRTemp tmpRd = newTemp(ty);

         assign(tmpRs, getIReg(rs));
         assign(tmpRt, getIReg(rt));

         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         DIP("dins r%u, r%u, %u, %u", rt, rs, lsb,
             msb - lsb + 1);
         UChar lsAmt = 63 - lsb;  /* left shift amount; */
         UChar rsAmt = 63 - lsb;  /* right shift amount; */
         assign(tmp, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(lsAmt)));
         assign(tmpT1, binop(Iop_Shl64, mkexpr(tmp), mkU8(1)));
         assign(tmp1, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(rsAmt)));
         assign(tmpT2, binop(Iop_Shr64, mkexpr(tmp1), mkU8(1)));

         lsAmt = msb;  /* left shift amount; */
         rsAmt = 1;  /*right shift amount; */
         assign(tmpT3, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(rsAmt)));
         assign(tmpT4, binop(Iop_Shr64, mkexpr(tmpT3), mkU8(lsAmt)));
         assign(tmpT5, binop(Iop_Shl64, mkexpr(tmpT4), mkU8(rsAmt)));
         assign(tmpT6, binop(Iop_Shl64, mkexpr(tmpT5), mkU8(lsAmt)));

         lsAmt = 64 - (msb - lsb + 1);  /* left shift amount; */
         rsAmt = 64 - (msb + 1);  /* right shift amount; */
         assign(tmpT7, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
         assign(tmpT8, binop(Iop_Shr64, mkexpr(tmpT7), mkU8(rsAmt)));

         assign(tmpT9, binop(Iop_Or64, mkexpr(tmpT2), mkexpr(tmpT8)));
         assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT6), mkexpr(tmpT9)));
         putIReg(rt, mkexpr(tmpRd));
         break;
      }

      case 0x24:  /* DBSHFL */
         lsb = get_lsb(cins);
         IRTemp tmpRs = newTemp(ty);
         IRTemp tmpRt = newTemp(ty);
         IRTemp tmpRd = newTemp(ty);
         assign(tmpRs, getIReg(rs));
         assign(tmpRt, getIReg(rt));

         switch (lsb) {
            case 0x02: {  /* DSBH */
               DIP("dsbh r%u, r%u", rd, rt);
               IRTemp tmpT1 = newTemp(ty);
               IRTemp tmpT2 = newTemp(ty);
               IRTemp tmpT3 = newTemp(ty);
               IRTemp tmpT4 = newTemp(ty);
               IRTemp tmpT5 = newTemp(Ity_I64);
               IRTemp tmpT6 = newTemp(ty);
               assign(tmpT5, mkU64(0xFF00FF00FF00FF00ULL));
               assign(tmpT6, mkU64(0x00FF00FF00FF00FFULL));
               assign(tmpT1, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT5)));
               assign(tmpT2, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(8)));
               assign(tmpT3, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT6)));
               assign(tmpT4, binop(Iop_Shl64, mkexpr(tmpT3), mkU8(8)));
               assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT4), mkexpr(tmpT2)));
               putIReg(rd, mkexpr(tmpRd));
               break;
            }

            case 0x05: {  /* DSHD */
               DIP("dshd r%u, r%u\n", rd, rt);
               IRTemp tmpT1 = newTemp(ty);
               IRTemp tmpT2 = newTemp(ty);
               IRTemp tmpT3 = newTemp(ty);
               IRTemp tmpT4 = newTemp(ty);
               IRTemp tmpT5 = newTemp(Ity_I64);
               IRTemp tmpT6 = newTemp(ty);
               IRTemp tmpT7 = newTemp(ty);
               IRTemp tmpT8 = newTemp(ty);
               IRTemp tmpT9 = newTemp(ty);
               assign(tmpT5, mkU64(0xFFFF0000FFFF0000ULL));
               assign(tmpT6, mkU64(0x0000FFFF0000FFFFULL));
               assign(tmpT1, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT5)));
               assign(tmpT2, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(16)));
               assign(tmpT3, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT6)));
               assign(tmpT4, binop(Iop_Shl64, mkexpr(tmpT3), mkU8(16)));
               assign(tmpT7, binop(Iop_Or64, mkexpr(tmpT4), mkexpr(tmpT2)));
               assign(tmpT8, binop(Iop_Shl64, mkexpr(tmpT7), mkU8(32)));
               assign(tmpT9, binop(Iop_Shr64, mkexpr(tmpT7), mkU8(32)));
               assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT8), mkexpr(tmpT9)));
               putIReg(rd, mkexpr(tmpRd));
               break;
            }

            case 0x08 ... 0x0f: { /* DALIGN */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dalign r%u, r%u, r%u, %u", rd, rs, rt, lsb & 0x7);
                  UInt bp = (lsb & 0x7) << 3;

                  if (bp) {
                     putIReg(rd, binop(Iop_Or64,
                                       binop(Iop_Shl64, getIReg(rt), mkU8(bp)),
                                       binop(Iop_Shr64,
                                             getIReg(rs), mkU8(64 - bp))));
                  } else
                     putIReg(rd, getIReg(rt));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
            }

            case 0: /* DBITSWAP */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dbitswap r%u, r%u", rd, rt);
                  putIReg(rd, qop(Iop_Rotx64, getIReg(rt), mkU8(7), mkU8(8), mkU8(1)));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            default:
               return -1;;
         }

         break;

      case 0x3B: /* RDHWR */
         DIP("rdhwr r%u, r%u", rt, rd);

         if (VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps) ||
               VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps) ||
               (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_BROADCOM)) {
            if (rd == 29) {
               putIReg(rt, getULR());
            } else if (rd <= 3
                       || (rd == 31
                           && VEX_MIPS_COMP_ID(archinfo->hwcaps)
                           == VEX_PRID_COMP_CAVIUM)) {
               IRExpr** arg = mkIRExprVec_1(mkU32(rd));
               IRTemp   val  = newTemp(ty);
               IRDirty *d = unsafeIRDirty_1_N(val,
                                              0,
                                              "mips_dirtyhelper_rdhwr",
                                              &mips_dirtyhelper_rdhwr,
                                              arg);
               stmt(IRStmt_Dirty(d));
               putIReg(rt, mkexpr(val));
            } else
               return -1;
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x04:  /* INS */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb - lsb + 1;
         DIP("ins size:%u msb:%u lsb:%u", size, msb, lsb);

         vassert(lsb + size <= 32);
         vassert(lsb + size > 0);

         /* put size bits from rs at the pos in temporary */
         t0 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         /* shift left for 32 - size to clear leading bits and get zeros
            at the end */
         assign(t0, binop(Iop_Shl32, mkNarrowTo32(ty, getIReg(rs)),
                          mkU8(32 - size)));
         /* now set it at pos */
         t1 = newTemp(Ity_I32);
         assign(t1, binop(Iop_Shr32, mkexpr(t0), mkU8(32 - size - lsb)));

         if (lsb > 0) {
            t2 = newTemp(Ity_I32);
            /* clear everything but lower pos bits from rt */
            assign(t2, binop(Iop_Shl32, mkNarrowTo32(ty, getIReg(rt)),
                             mkU8(32 - lsb)));
            assign(t3, binop(Iop_Shr32, mkexpr(t2), mkU8(32 - lsb)));
         } else
            assign(t3, mkU32(0));

         if (msb < 31) {
            t4 = newTemp(Ity_I32);
            /* clear everything but upper msb + 1 bits from rt */
            assign(t4, binop(Iop_Shr32, mkNarrowTo32(ty, getIReg(rt)),
                             mkU8(msb + 1)));
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_Shl32, mkexpr(t4), mkU8(msb + 1)));

            /* now combine these registers */
            if (lsb > 0) {
               t6 = newTemp(Ity_I32);
               assign(t6, binop(Iop_Or32, mkexpr(t5), mkexpr(t1)));
               putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t6),
                                                   mkexpr(t3)), True));
            } else {
               putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t1),
                                                   mkexpr(t5)), True));
            }
         } else {
            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t1),
                                                mkexpr(t3)), True));
         }

         break;

      case 0x00:  /* EXT */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         DIP("ext size:%u msb:%u lsb:%u", size, msb, lsb);
         vassert(lsb + size <= 32);
         vassert(lsb + size > 0);

         /* put size bits from rs at the top of in temporary */
         if (lsb + size < 32) {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Shl32, mkNarrowTo32(ty, getIReg(rs)),
                             mkU8(32 - lsb - size)));

            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Shr32, mkexpr(t0),
                                                mkU8(32 - size)), True));
         } else {
            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Shr32,
                                                mkNarrowTo32(ty, getIReg(rs)),
                                                mkU8(32 - size)), True));
         }

         break;

      case 0x03:  /* Doubleword Extract Bit Field - DEXT; MIPS64r2 */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         DIP("dext r%u, r%u, %u, %u", rt, rs, lsb, msb + 1);
         t1 = newTemp(Ity_I64);
         vassert(lsb >= 0 && lsb < 32);
         vassert(size > 0 && size <= 32);
         vassert((lsb + size) > 0 && (lsb + size) <= 63);

         UChar lsAmt = 63 - (lsb + msb);  /* left shift amount; */
         UChar rsAmt = 63 - msb;  /* right shift amount; */

         assign(t1, binop(Iop_Shl64, getIReg(rs), mkU8(lsAmt)));
         putIReg(rt, binop(Iop_Shr64, mkexpr(t1), mkU8(rsAmt)));

         break;

      case 0x20:  /* BSHFL */
         switch (sa) {
            case 0x0: /* BITSWAP */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("bitswap r%u, r%u", rd, rt);

                  if (mode64) {
                     putIReg(rd, unop(Iop_32Uto64, qop(Iop_Rotx32, unop(Iop_64to32, getIReg(rt)),
                                                       mkU8(7), mkU8(8), mkU8(1))));
                  } else {
                     putIReg(rd, qop(Iop_Rotx32, getIReg(rt), mkU8(7),
                                     mkU8(8), mkU8(1)));
                  }
               } else {
                  ILLEGAL_INSTRUCTON;
               }

               break;

            case 0x02:  /* WSBH */
               DIP("wsbh r%u, r%u", rd, rt);
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               assign(t0, binop(Iop_Shl32, binop(Iop_And32, mkNarrowTo32(ty,
                                                 getIReg(rt)), mkU32(0x00FF0000)),
                                mkU8(0x8)));
               assign(t1, binop(Iop_Shr32, binop(Iop_And32, mkNarrowTo32(ty,
                                                 getIReg(rt)), mkU32(0xFF000000)), mkU8(0x8)));
               assign(t2, binop(Iop_Shl32, binop(Iop_And32, mkNarrowTo32(ty,
                                                 getIReg(rt)), mkU32(0x000000FF)), mkU8(0x8)));
               assign(t3, binop(Iop_Shr32, binop(Iop_And32, mkNarrowTo32(ty,
                                                 getIReg(rt)), mkU32(0x0000FF00)), mkU8(0x8)));
               putIReg(rd, mkWidenFrom32(ty, binop(Iop_Or32, binop(Iop_Or32,
                                                   mkexpr(t0), mkexpr(t1)),
                                                   binop(Iop_Or32, mkexpr(t2),
                                                         mkexpr(t3))), True));
               break;

            case 0x10:  /* SEB */
               DIP("seb r%u, r%u", rd, rt);

               if (mode64)
                  putIReg(rd, unop(Iop_8Sto64, unop(Iop_64to8, getIReg(rt))));
               else
                  putIReg(rd, unop(Iop_8Sto32, unop(Iop_32to8, getIReg(rt))));

               break;

            case 0x18:  /* SEH */
               DIP("seh r%u, r%u", rd, rt);

               if (mode64)
                  putIReg(rd, unop(Iop_16Sto64, unop(Iop_64to16, getIReg(rt))));
               else
                  putIReg(rd, unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rt))));

               break;

            case 0x08 ... 0x0b: /* ALIGN */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  if (mode64) {
                     UInt bp = (sa & 0x3) << 3;

                     if (bp) {
                        putIReg(rd, unop(Iop_32Sto64,
                                         binop(Iop_Or32,
                                               binop(Iop_Shl32,
                                                     unop(Iop_64to32,
                                                          getIReg(rt)),
                                                     mkU8(bp)),
                                               binop(Iop_Shr32,
                                                     unop(Iop_64to32,
                                                          getIReg(rs)),
                                                     mkU8(32 - bp)))));
                     } else
                        putIReg(rd, getIReg(rt));
                  } else {
                     UInt bp = (sa & 0x3) << 3;

                     if (bp) {
                        putIReg(rd, binop(Iop_Or32,
                                          binop(Iop_Shl32,
                                                getIReg(rt), mkU8(bp)),
                                          binop(Iop_Shr32,
                                                getIReg(rs), mkU8(32 - bp))));
                     } else
                        putIReg(rd, getIReg(rt));
                  }
               } else {
                  ILLEGAL_INSTRUCTON;
               }

               break;

            default:
               return -1;

         }

         break;  /* BSHFL */

         /* --- MIPS32(r2) DSP ASE(r2) / Cavium Specfic (LX) instructions --- */
      case 0xA:  /* LX */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            if (dis_instr_CVM(cins))
               break;

            return -1;
         }
         /* fallthrough */
      case 0xC:  /* INSV */
      case 0x38: {  /* EXTR.W */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );

            if (0 != retVal ) {
               return -2;
            }

            break;
         } else {
            return -2;
         }

         break;
      }

      case 0x10: {  /* ADDU.QB */
         switch (sa) {
            case  0xC:  /* SUBU_S.PH */
            case  0xD:  /* ADDU_S.PH */
            case 0x1E: {  /* MULQ_S.PH */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }

            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }
         }

         break;
      }

      case 0x11: {  /* CMPU.EQ.QB */
         switch (sa) {
            case 0x18:  /* CMPGDU.EQ.QB */
            case 0x19:  /* CMPGDU.LT.QB */
            case 0x1A:  /* CMPGDU.LE.QB */
            case 0x0D:  /* PRECR.QB.PH */
            case 0x1E:  /* PRECR_SRA.PH.W */
            case 0x1F: {  /* PRECR_SRA_R.PH.W */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }

            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }
         }

         break;
      }

      case 0x12: {  /* ABSQ_S.PH */
         switch (sa) {
            case 0x1: {  /* ABSQ_S.QB */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }

            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }
         }

         break;
      }

      case 0x13: {  /* SHLL.QB */
         switch (sa) {
            case 0x04:  /* SHRA.QB */
            case 0x05:  /* SHRA_R.QB */
            case 0x06:  /* SHRAV.QB */
            case 0x07:  /* SHRAV_R.QB */
            case 0x19:  /* SHLR.PH */
            case 0x1B: {  /* SHLRV.PH */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }

            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }
         }

         break;
      }

      case 0x30: {  /* DPAQ.W.PH */
         switch (sa) {
            case  0x0:  /* DPA.W.PH */
            case 0x18:  /* DPAQX_S.W.PH */
            case 0x1A:  /* DPAQX_SA.W.PH */
            case  0x8:  /* DPAX.W.PH */
            case  0x1:  /* DPS.W.PH */
            case 0x19:  /* DPSQX_S.W.PH */
            case 0x1B:  /* DPSQX_SA.W.PH */
            case  0x9:  /* DPSX.W.PH */
            case  0x2: {  /* MULSA.W.PH */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }

            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );

                  if (0 != retVal ) {
                     return -2;
                  }

                  break;
               } else {
                  return -2;
               }

               break;
            }
         }

         break;
      }

      case 0x18:  /* ADDUH.QB/MUL.PH */
      case 0x31: {  /* APPEND */
         if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );

            if (0 != retVal ) {
               return -2;
            }

            break;
         } else {
            return -2;
         }
      }

      case 0x35: { /* PREF r6*/
         DIP("pref");
         break;
      }

      case 0x36: { /* LL */
         imm = extend_s_9to16((instr_index >> 7) & 0x1ff);
         DIP("ll r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         t2 = newTemp(ty);
         assign(t2, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), True));
         putLLaddr(mkexpr(t1));
         putLLdata(mkexpr(t2));
         putIReg(rt, mkexpr(t2));
         break;
      }

      case 0x26: { /* SC */
         imm = extend_s_9to16((instr_index >> 7) & 0x1ff);
         DIP("sc r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I32);
         assign(t2, binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                          mkexpr(t1), getLLaddr()));
         assign(t3, mkNarrowTo32(ty, getIReg(rt)));
         putLLaddr(LLADDR_INVALID);
         putIReg(rt, getIReg(0));

         mips_next_insn_if(mkexpr(t2));

         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);

         assign(t5, mkNarrowTo32(ty, getLLdata()));

         stmt(IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t4, /* old_mem */
                                 MIPS_IEND, mkexpr(t1),                 /* addr */
                                 NULL, mkexpr(t5),                      /* expected value */
                                 NULL, mkexpr(t3)                       /* new value */)));

         putIReg(rt, unop(mode64 ? Iop_1Uto64 : Iop_1Uto32,
                          binop(Iop_CmpEQ32, mkexpr(t4), mkexpr(t5))));
         break;
      }

      case 0x37: { /* LLD */
         imm = extend_s_9to16((instr_index >> 7) & 0x1ff);
         DIP("lld r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         t2 = newTemp(Ity_I64);
         assign(t2, load(Ity_I64, mkexpr(t1)));
         putLLaddr(mkexpr(t1));
         putLLdata(mkexpr(t2));
         putIReg(rt, mkexpr(t2));
         break;
      }

      case 0x27: { /* SCD */
         imm = extend_s_9to16((instr_index >> 7) & 0x1ff);
         DIP("sdc r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I64);
         assign(t2, binop(Iop_CmpNE64, mkexpr(t1), getLLaddr()));
         assign(t3, getIReg(rt));
         putLLaddr(LLADDR_INVALID);
         putIReg(rt, getIReg(0));

         mips_next_insn_if(mkexpr(t2));

         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);

         assign(t5, getLLdata());

         stmt(IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t4, /* old_mem */
                                 MIPS_IEND, mkexpr(t1),                 /* addr */
                                 NULL, mkexpr(t5),                      /* expected value */
                                 NULL, mkexpr(t3)                       /* new value */)));

         putIReg(rt, unop(Iop_1Uto64,
                          binop(Iop_CmpEQ64, mkexpr(t4), mkexpr(t5))));
         break;
      }

      default:
         return -1;
   }

   return 0;
}

static UInt disInstr_MIPS_WRK_00(UInt cins, const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo, DisResult* dres,
                                 IRStmt** bstmt, IRExpr** lastn)
{
   IRTemp t0;
   UInt opcode, rs, rt, trap_code, imm, instr_index, p;
   /* Additional variables for instruction fields in DSP ASE insructions */

   opcode = get_opcode(cins);
   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   instr_index = get_instr_index(cins);
   trap_code = get_code(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   switch (opcode & 0x0F) {
      case 0x00:  /* Special */
         return disInstr_MIPS_WRK_Special(cins, archinfo, abiinfo,
                                          dres, bstmt, lastn);

      case 0x01:  /* Regimm */
         switch (rt) {
            case 0x00:  /* BLTZ */
               DIP("bltz r%u, %u", rs, imm);

               if (mode64) {
                  if (!dis_instr_branch(cins, dres, bstmt))
                     return -1;
               } else
                  dis_branch(False, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                                          mkU32(0x80000000)), mkU32(0x80000000)), imm, bstmt);

               break;

            case 0x01:  /* BGEZ */
               DIP("bgez r%u, %u", rs, imm);

               if (mode64) {
                  if (!dis_instr_branch(cins, dres, bstmt))
                     return -1;
               } else
                  dis_branch(False, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                                          mkU32(0x80000000)), mkU32(0x0)), imm, bstmt);

               break;

            case 0x02:  /* BLTZL */
               DIP("bltzl r%u, %u", rs, imm);
               *lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                                                binop(mode64 ? Iop_And64 : Iop_And32, getIReg(rs),
                                                      mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                                                mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                                          imm);
               break;

            case 0x03:  /* BGEZL */
               DIP("bgezl r%u, %u", rs, imm);
               *lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                                                binop(mode64 ? Iop_And64 : Iop_And32, getIReg(rs),
                                                      mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                                                mode64 ? mkU64(0x0) : mkU32(0x0)), imm);
               break;

            case 0x06: { /* DAHI */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dahi  r%u,  %x", rs, imm);
                  putIReg(rs, binop(Iop_Add64,
                                    getIReg(rs), mkU64(extend_s_16to64 (imm) << 32)));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
            }

            case 0x08:  /* TGEI */
               DIP("tgei r%u, %u %u", rs, imm, trap_code);

               if (mode64) {
                  stmt (IRStmt_Exit (unop (Iop_Not1,
                                           binop (Iop_CmpLT64S,
                                                  getIReg (rs),
                                                  mkU64 (extend_s_16to64 (imm)))),
                                     Ijk_SigTRAP,
                                     IRConst_U64(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               } else {
                  stmt (IRStmt_Exit (unop (Iop_Not1,
                                           binop (Iop_CmpLT32S,
                                                  getIReg (rs),
                                                  mkU32 (extend_s_16to32 (imm)))),
                                     Ijk_SigTRAP,
                                     IRConst_U32(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               }

               break;

            case 0x09: {  /* TGEIU */
               DIP("tgeiu r%u, %u %u", rs, imm, trap_code);

               if (mode64) {
                  stmt (IRStmt_Exit (unop (Iop_Not1,
                                           binop (Iop_CmpLT64U,
                                                  getIReg (rs),
                                                  mkU64 (extend_s_16to64 (imm)))),
                                     Ijk_SigTRAP,
                                     IRConst_U64(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               } else {
                  stmt (IRStmt_Exit (unop (Iop_Not1,
                                           binop (Iop_CmpLT32U,
                                                  getIReg (rs),
                                                  mkU32 (extend_s_16to32 (imm)))),
                                     Ijk_SigTRAP,
                                     IRConst_U32(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               }

               break;
            }

            case 0x0A: {  /* TLTI */
               DIP("tlti r%u, %u %u", rs, imm, trap_code);

               if (mode64) {
                  stmt (IRStmt_Exit (binop (Iop_CmpLT64S, getIReg (rs),
                                            mkU64 (extend_s_16to64 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U64(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               } else {
                  stmt (IRStmt_Exit (binop (Iop_CmpLT32S, getIReg (rs),
                                            mkU32 (extend_s_16to32 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U32(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               }

               break;
            }

            case 0x0B: {  /* TLTIU */
               DIP("tltiu r%u, %u %u", rs, imm, trap_code);

               if (mode64) {
                  stmt (IRStmt_Exit (binop (Iop_CmpLT64U, getIReg (rs),
                                            mkU64 (extend_s_16to64 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U64(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               } else {
                  stmt (IRStmt_Exit (binop (Iop_CmpLT32U, getIReg (rs),
                                            mkU32 (extend_s_16to32 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U32(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               }

               break;
            }

            case 0x0C: {  /* TEQI */
               DIP("teqi r%u, %u %u", rs, imm, trap_code);

               if (mode64) {
                  stmt (IRStmt_Exit (binop (Iop_CmpEQ64, getIReg (rs),
                                            mkU64 (extend_s_16to64 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U64(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               } else {
                  stmt (IRStmt_Exit (binop (Iop_CmpEQ32, getIReg (rs),
                                            mkU32 (extend_s_16to32 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U32(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               }

               break;
            }

            case 0x0E: {  /* TNEI */
               DIP("tnei r%u, %u %u", rs, imm, trap_code);

               if (mode64) {
                  stmt (IRStmt_Exit (binop (Iop_CmpNE64, getIReg (rs),
                                            mkU64 (extend_s_16to64 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U64(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               } else {
                  stmt (IRStmt_Exit (binop (Iop_CmpNE32, getIReg (rs),
                                            mkU32 (extend_s_16to32 (imm))),
                                     Ijk_SigTRAP,
                                     IRConst_U32(guest_PC_curr_instr + 4),
                                     OFFB_PC));
               }

               break;
            }

            case 0x10:  /* BLTZAL */
               DIP("bltzal r%u, %u", rs, imm);

               if (mode64) {
                  if (!dis_instr_branch(cins, dres, bstmt))
                     return -1;
               } else
                  dis_branch(True, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                                         mkU32(0x80000000)), mkU32(0x80000000)), imm, bstmt);

               break;

            case 0x11:  /* BGEZAL */
               DIP("bgezal r%u, %u", rs, imm);

               if (mode64) {
                  if (!dis_instr_branch(cins, dres, bstmt))
                     return -1;
               } else
                  dis_branch(True, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                                         mkU32(0x80000000)), mkU32(0x0)), imm, bstmt);

               break;

            case 0x12:  /* BLTZALL */
               DIP("bltzall r%u, %u", rs, imm);
               putIReg(31, mode64 ? mkU64(guest_PC_curr_instr + 8) :
                       mkU32(guest_PC_curr_instr + 8));
               *lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                                                binop(mode64 ? Iop_And64 : Iop_And32, getIReg(rs),
                                                      mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                                                mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                                          imm);
               break;

            case 0x13:  /* BGEZALL */
               DIP("bgezall r%u, %u", rs, imm);

               if (mode64) {
                  putIReg(31, mkU64(guest_PC_curr_instr + 8));
                  *lastn = dis_branch_likely(binop(Iop_CmpNE64,
                                                   binop(Iop_And64,
                                                         getIReg(rs),
                                                         mkU64(0x8000000000000000ULL)),
                                                   mkU64(0x0)),
                                             imm);
               } else {
                  putIReg(31, mkU32(guest_PC_curr_instr + 8));
                  *lastn = dis_branch_likely(binop(Iop_CmpNE32, binop(Iop_And32,
                                                   getIReg(rs), mkU32(0x80000000)),
                                                   mkU32(0x0)), imm);
               }

               break;

            case 0x1C: {  /* BPOSGE32 */
               DIP("bposge32 %u", imm);
               vassert(!mode64);
               t0 = newTemp(Ity_I32);
               /* Get pos field from DSPControl register. */
               assign(t0, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));
               dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLT32U, mkexpr(t0),
                                                      mkU32(32))), imm, bstmt);
               break;
            }

            case 0x1E: { /* DATI */
               if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                  DIP("dati  r%u,  %x", rs, imm);
                  putIReg(rs, binop(Iop_Add64,
                                    getIReg(rs), mkU64((long long)imm << 48)));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;
            }

            case 0x1F: /* SYNCI */
               /* Just ignore it */
               break;

            default:
               return -1;
         }

         break;

      case 0x02:  /* J */
         DIP("j 0x%x", instr_index);
         t0 = newTemp(ty);

         if (mode64)
            assign(t0, mkU64((guest_PC_curr_instr & 0xFFFFFFFFF0000000ULL) |
                             (instr_index << 2)));
         else
            assign(t0, mkU32((guest_PC_curr_instr & 0xF0000000) |
                             (instr_index << 2)));

         *lastn = mkexpr(t0);
         break;

      case 0x03:  /* JAL */
         DIP("jal 0x%x", instr_index);

         if (mode64) {
            putIReg(31, mkU64(guest_PC_curr_instr + 8));
            t0 = newTemp(ty);
            assign(t0, mkU64((guest_PC_curr_instr & 0xFFFFFFFFF0000000ULL) |
                             (instr_index << 2)));
         } else {
            putIReg(31, mkU32(guest_PC_curr_instr + 8));
            t0 = newTemp(ty);
            assign(t0, mkU32((guest_PC_curr_instr & 0xF0000000) |
                             (instr_index << 2)));
         }

         *lastn = mkexpr(t0);
         break;

      case 0x04:  /* BEQ */
         DIP("beq r%u, r%u, %u", rs, rt, imm);

         if (mode64)
            dis_branch(False, binop(Iop_CmpEQ64, getIReg(rs), getIReg(rt)),
                       imm, bstmt);
         else
            dis_branch(False, binop(Iop_CmpEQ32, getIReg(rs), getIReg(rt)),
                       imm, bstmt);

         break;

      case 0x05:  /* BNE */
         DIP("bne r%u, r%u, %u", rs, rt, imm);

         if (mode64)
            dis_branch(False, binop(Iop_CmpNE64, getIReg(rs), getIReg(rt)),
                       imm, bstmt);
         else
            dis_branch(False, binop(Iop_CmpNE32, getIReg(rs), getIReg(rt)),
                       imm, bstmt);

         break;

      case 0x06:  /* BLEZ, BLEZALC, BGEZALC, BGEUC */
         if (rt == 0) { /* BLEZ */
            DIP("blez r%u, %u", rs, imm);

            if (mode64)
               dis_branch(False, binop(Iop_CmpLE64S, getIReg(rs), mkU64(0x0)),
                          imm, bstmt);
            else
               dis_branch(False, binop(Iop_CmpLE32S, getIReg(rs), mkU32(0x0)), imm,
                          bstmt);
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            if (rs == 0) { /* BLEZALC */
               DIP("blezalc r%u, %u", rt, imm);

               if (mode64)
                  dis_branch_compact(True,
                                     binop(Iop_CmpLE64S, getIReg(rt), mkU64(0x0)),
                                     imm, dres);
               else
                  dis_branch_compact(True,
                                     binop(Iop_CmpLE32S, getIReg(rt), mkU32(0x0)),
                                     imm, dres);
            } else if (rt == rs) { /* BGEZALC */
               DIP("bgezalc r%u, %u", rt, imm);

               if (mode64)
                  dis_branch_compact(True,
                                     binop(Iop_CmpLE64S, mkU64(0x0), getIReg(rt)),
                                     imm, dres);
               else
                  dis_branch_compact(True,
                                     binop(Iop_CmpLE32S, mkU32(0x0), getIReg(rt)),
                                     imm, dres);
            } else { /* BGEUC */
               DIP("bgeuc r%u, r%u, %u", rt, rs, imm);

               if (mode64)
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLT64U,
                                                getIReg(rs), getIReg(rt))),
                                     imm, dres);
               else
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLT32U,
                                                getIReg(rs), getIReg(rt))),
                                     imm, dres);
            }
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x07:  /* BGTZ, BGTZALC, BLTZALC, BLTUC */
         if (rt == 0) { /* BGTZ */
            DIP("bgtz r%u, %u", rs, imm);

            if (mode64)
               dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLE64S, getIReg(rs),
                                                      mkU64(0x00))), imm, bstmt);
            else
               dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLE32S, getIReg(rs),
                                                      mkU32(0x00))), imm, bstmt);
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            if (rs == 0) { /* BGTZALC */
               DIP("bgtzalc r%u, %u", rt, imm);

               if (mode64) {
                  dis_branch_compact(True,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE64S,
                                                getIReg(rt), mkU64(0x0))),
                                     imm, dres);
               } else {
                  dis_branch_compact(True,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE32S,
                                                getIReg(rt), mkU32(0x0))),
                                     imm, dres);
               }
            } else if (rs == rt) { /* BLTZALC */
               DIP("bltzalc r%u, %u", rt, imm);

               if (mode64) {
                  dis_branch_compact(True,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE64S,
                                                mkU64(0x0), getIReg(rt))),
                                     imm, dres);
               } else {
                  dis_branch_compact(True,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE32S,
                                                mkU32(0x0), getIReg(rt))),
                                     imm, dres);
               }
            } else { /* BLTUC */
               DIP("bltuc r%u, r%u, %u", rt, rs, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLT64U, getIReg(rs), getIReg(rt)),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLT32U, getIReg(rs), getIReg(rt)),
                                     imm, dres);
               }
            }
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

#if defined(__mips__) && ((defined(__mips_isa_rev) && __mips_isa_rev < 6))

      case 0x08: {  /* ADDI */
         DIP("addi r%u, r%u, %u", rt, rs, imm);
         IRTemp tmpRs32, t1, t2, t3, t4;
         tmpRs32 = newTemp(Ity_I32);
         assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         /* dst = src0 + sign(imm)
         if(sign(src0 ) != sign(imm ))
         goto no overflow;
         if(sign(dst) == sign(src0 ))
         goto no overflow;
         we have overflow! */

         assign(t0, binop(Iop_Add32, mkexpr(tmpRs32),
                          mkU32(extend_s_16to32(imm))));
         assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32),
                          mkU32(extend_s_16to32(imm))));
         assign(t2, unop(Iop_1Sto32, binop(Iop_CmpEQ32, binop(Iop_And32,
                                           mkexpr(t1), mkU32(0x80000000)), mkU32(0x80000000))));

         assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
         assign(t4, unop(Iop_1Sto32, binop(Iop_CmpNE32, binop(Iop_And32,
                                           mkexpr(t3), mkU32(0x80000000)), mkU32(0x80000000))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ32, binop(Iop_Or32, mkexpr(t2),
                                mkexpr(t4)), mkU32(0)), Ijk_SigFPE_IntOvf,
                          mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                          IRConst_U32(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rt,  mkWidenFrom32(ty, mkexpr(t0), True));
         break;
      }

#elif defined(__mips__) && ((defined(__mips_isa_rev) && __mips_isa_rev >= 6))

      case 0x08: { /* BEQZALC, BEQC, BOVC */
         IRTemp t1, t2, t3, t4;
         if (rs == 0) { /* BEQZALC */
            DIP("beqzalc r%u, %u", rt, imm);

            if (mode64) {
               dis_branch_compact(True,
                                  binop(Iop_CmpEQ64, getIReg(rt), mkU64(0x0)),
                                  imm, dres);
            } else {
               dis_branch_compact(True,
                                  binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x0)),
                                  imm, dres);
            }
         } else  if (rs < rt) { /* BEQC */
            DIP("beqc r%u, r%u, %u", rs, rt, imm);

            if (mode64) {
               dis_branch_compact(False,
                                  binop(Iop_CmpEQ64, getIReg(rt), getIReg(rs)),
                                  imm, dres);
            } else {
               dis_branch_compact(False,
                                  binop(Iop_CmpEQ32, getIReg(rt), getIReg(rs)),
                                  imm, dres);
            }
         } else { /* BOVC */
            DIP("bovc r%u, r%u, %u", rs, rt, imm);

            if (mode64) {
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               assign(t0, IRExpr_ITE(binop(Iop_CmpLT64S,
                                           getIReg(rt),
                                           mkU64(0xffffffff80000000ULL)),
                                     mkU32(1),
                                     IRExpr_ITE(binop(Iop_CmpLT64S,
                                                      getIReg(rt),
                                                      mkU64(0x7FFFFFFFULL)),
                                                mkU32(0), mkU32(1))));
               assign(t1, IRExpr_ITE(binop(Iop_CmpLT64S,
                                           getIReg(rs),
                                           mkU64(0xffffffff80000000ULL)),
                                     mkU32(1),
                                     IRExpr_ITE(binop(Iop_CmpLT64S,
                                                      getIReg(rs),
                                                      mkU64(0x7FFFFFFFULL)),
                                                mkU32(0), mkU32(1))));
               assign(t2, IRExpr_ITE(binop(Iop_CmpLT64S,
                                           binop(Iop_Add64,
                                                 getIReg(rt), getIReg(rs)),
                                           mkU64(0xffffffff80000000ULL)),
                                     mkU32(1),
                                     IRExpr_ITE(binop(Iop_CmpLT64S,
                                                      binop(Iop_Add64,
                                                            getIReg(rt),
                                                            getIReg(rs)),
                                                      mkU64(0x7FFFFFFFULL)),
                                                mkU32(0), mkU32(1))));
               assign(t3, binop(Iop_Add32,
                                mkexpr(t0),
                                binop(Iop_Add32, mkexpr(t1), mkexpr(t2))));
               dis_branch_compact(False,
                                  binop(Iop_CmpNE32, mkexpr(t3), mkU32(0)),
                                  imm, dres);
            } else {
               IRTemp tmpRs32 = newTemp(Ity_I32);
               IRTemp tmpRt32 = newTemp(Ity_I32);
               assign(tmpRs32, getIReg(rs));
               assign(tmpRt32, getIReg(rt));

               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               t4 = newTemp(Ity_I32);
               /* dst = src0 + src1
               if (sign(src0 ) != sign(src1 ))
               goto no overflow;
               if (sign(dst) == sign(src0 ))
               goto no overflow;
               we have overflow! */

               assign(t0, binop(Iop_Add32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
               assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
               assign(t2, unop(Iop_1Uto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32, mkexpr(t1), mkU32(0x80000000)),
                                     mkU32(0x80000000))));

               assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
               assign(t4, unop(Iop_1Uto32,
                               binop(Iop_CmpNE32,
                                     binop(Iop_And32, mkexpr(t3), mkU32(0x80000000)),
                                     mkU32(0x80000000))));

               dis_branch_compact(False, binop(Iop_CmpEQ32,
                                               binop(Iop_Or32, mkexpr(t2), mkexpr(t4)),
                                               mkU32(0)), imm, dres);
            }
         }

         break;
         /* In documentation for BEQC stands rs > rt and for BOVC stands rs >= rt! */
      }

#endif

      case 0x09:  /* ADDIU */
         DIP("addiu r%u, r%u, %u", rt, rs, imm);

         if (mode64) {
            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Add32,
                                                mkNarrowTo32(ty, getIReg(rs)), mkU32(extend_s_16to32(imm))),
                                      True));
         } else
            putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));

         break;

      case 0x0A:  /* SLTI */
         DIP("slti r%u, r%u, %u", rt, rs, imm);

         if (mode64)
            putIReg(rt, unop(Iop_1Uto64, binop(Iop_CmpLT64S, getIReg(rs),
                                               mkU64(extend_s_16to64(imm)))));
         else
            putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                               mkU32(extend_s_16to32(imm)))));

         break;

      case 0x0B:  /* SLTIU */
         DIP("sltiu r%u, r%u, %u", rt, rs, imm);

         if (mode64)
            putIReg(rt, unop(Iop_1Uto64, binop(Iop_CmpLT64U, getIReg(rs),
                                               mkU64(extend_s_16to64(imm)))));
         else
            putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                               mkU32(extend_s_16to32(imm)))));

         break;

      case 0x0C:  /* ANDI */
         DIP("andi r%u, r%u, %u", rt, rs, imm);

         if (mode64) {
            ALUI_PATTERN64(Iop_And64);
         } else {
            ALUI_PATTERN(Iop_And32);
         }

         break;

      case 0x0D:  /* ORI */
         DIP("ori r%u, r%u, %u", rt, rs, imm);

         if (mode64) {
            ALUI_PATTERN64(Iop_Or64);
         } else {
            ALUI_PATTERN(Iop_Or32);
         }

         break;

      case 0x0E:  /* XORI */
         DIP("xori r%u, r%u, %u", rt, rs, imm);

         if (mode64) {
            ALUI_PATTERN64(Iop_Xor64);
         } else {
            ALUI_PATTERN(Iop_Xor32);
         }

         break;

      case 0x0F:  /* LUI */
         if (rs == 0) {
            p = (imm << 16);
            DIP("lui r%u, imm: 0x%x", rt, imm);

            if (mode64)
               putIReg(rt, mkU64(extend_s_32to64(p)));
            else
               putIReg(rt, mkU32(p));

            break;
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) { /* AUI */
            DIP("aui r%u, imm: 0x%x", rt, imm);

            if (mode64) {
               putIReg(rt, unop(Iop_32Sto64,
                                unop(Iop_64to32,
                                     binop(Iop_Add64,
                                           getIReg(rs),
                                           mkU64(extend_s_32to64(imm << 16))))));
            } else {
               putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(imm << 16)));
            }

         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      default:
         return -1;
   }

   return 0;
}

static UInt disInstr_MIPS_WRK_10(UInt cins, const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo, DisResult* dres,
                                 IRStmt** bstmt, IRExpr** lastn)
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7;
   UInt opcode, rs, rt, ft, fs, fd, fmt, tf, nd, function, imm;
   /* Additional variables for instruction fields in DSP ASE insructions */

   opcode = get_opcode(cins);
   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   fs = get_fs(cins);
   fd = get_fd(cins);
   ft = get_ft(cins);
   tf = get_tf(cins);
   nd = get_nd(cins);
   fmt = get_fmt(cins);
   function = get_function(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRType tyF = fp_mode64 ? Ity_F64 : Ity_F32;

   switch (opcode & 0x0F) {
      case 0x01: {  /* COP1 */
         if (fmt == 0x3 && fd == 0 && function == 0) {  /* MFHC1 */
            DIP("mfhc1 r%u, f%u", rt, fs);

            if (VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps) ||
                  VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               if (fp_mode64) {
                  t0 = newTemp(Ity_I64);
                  t1 = newTemp(Ity_I32);
                  assign(t0, unop(Iop_ReinterpF64asI64, getDReg(fs)));
                  assign(t1, unop(Iop_64HIto32, mkexpr(t0)));
                  putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));
               } else {
                  putIReg(rt, mkWidenFrom32(ty, unop(Iop_ReinterpF32asI32,
                                                     getFReg(fs | 1)), True));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if (fmt == 0x7 && fd == 0 && function == 0) {  /* MTHC1 */
            DIP("mthc1 r%u, f%u", rt, fs);

            if (VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps) ||
                  VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               if (fp_mode64) {
                  t0 = newTemp(Ity_I64);
                  assign(t0, binop(Iop_32HLto64, mkNarrowTo32(ty, getIReg(rt)),
                                   unop(Iop_ReinterpF32asI32,
                                        getLoFromF64(Ity_F64, getDReg(fs)))));
                  putDReg(fs, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
               } else {
                  putFReg(fs | 1, unop(Iop_ReinterpI32asF32,
                                       mkNarrowTo32(ty, getIReg(rt))));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if (fmt == 0x8) {  /* BC */
            /* FcConditionalCode(bc1_cc) */
            UInt bc1_cc = get_bc1_cc(cins);
            t1 = newTemp(Ity_I1);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I1);

            assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(bc1_cc)));
            assign(t2, IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(), mkU8(23)),
                                        mkU32(0x1)),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(24 + bc1_cc)),
                                        mkU32(0x1))));

            if (tf == 1 && nd == 0) {
               /* branch on true */
               DIP("bc1t %u, %u", bc1_cc, imm);
               assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
               dis_branch(False, mkexpr(t3), imm, bstmt);
               break;
            } else if (tf == 0 && nd == 0) {
               /* branch on false */
               DIP("bc1f %u, %u", bc1_cc, imm);
               assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
               dis_branch(False, mkexpr(t3), imm, bstmt);
               break;
            } else if (nd == 1 && tf == 0) {
               DIP("bc1fl %u, %u", bc1_cc, imm);
               *lastn = dis_branch_likely(binop(Iop_CmpNE32, mkexpr(t2),
                                                mkU32(0x0)), imm);
               break;
            } else if (nd == 1 && tf == 1) {
               DIP("bc1tl %u, %u", bc1_cc, imm);
               *lastn = dis_branch_likely(binop(Iop_CmpEQ32, mkexpr(t2),
                                                mkU32(0x0)), imm);
               break;
            } else
               return -1;
         } else if (fmt >= 0x1c && has_msa) { /* BNZ.df */
            Int df = fmt & 3;
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_V128);
            t2 = newTemp(Ity_V128);
            t3 = newTemp(Ity_V128);
            assign(t1, getWReg(ft));
            assign(t2, binop(Iop_64HLtoV128, mkU64(0), mkU64(0)));

            switch (df) {
               case 0x00: { /* BNZ.B */
                  DIP("BNZ.B w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ8x16, mkexpr(t1), mkexpr(t2)));
                  break;
               }

               case 0x01: { /* BNZ.H */
                  DIP("BNZ.H w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ16x8, mkexpr(t1), mkexpr(t2)));
                  break;
               }

               case 0x02: { /* BNZ.W */
                  DIP("BNZ.W w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ32x4, mkexpr(t1), mkexpr(t2)));
                  break;
               }

               case 0x03: { /* BNZ.D */
                  DIP("BNZ.D w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ64x2, mkexpr(t1), mkexpr(t2)));
                  break;
               }
            }

            assign(t0,
                   binop(Iop_Or32,
                         binop(Iop_Or32,
                               unop(Iop_V128to32, mkexpr(t3)),
                               unop(Iop_64HIto32, unop(Iop_V128to64, mkexpr(t3)))),
                         binop(Iop_Or32,
                               unop(Iop_64to32,
                                    unop(Iop_V128HIto64, mkexpr(t3))),
                               unop(Iop_64HIto32,
                                    unop(Iop_V128HIto64, mkexpr(t3))))));
            dis_branch(False,
                       binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0)), imm, bstmt);
         } else if (fmt == 0x0F && has_msa) { /* BNZ.V */
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_V128);
            assign(t1, getWReg(ft));
            assign(t0,
                   binop(Iop_Or32,
                         binop(Iop_Or32,
                               unop(Iop_V128to32, mkexpr(t1)),
                               unop(Iop_64HIto32, unop(Iop_V128to64, mkexpr(t1)))),
                         binop(Iop_Or32,
                               unop(Iop_64to32, unop(Iop_V128HIto64, mkexpr(t1))),
                               unop(Iop_64HIto32,
                                    unop(Iop_V128HIto64, mkexpr(t1))))));
            dis_branch(False,
                       binop(Iop_CmpNE32, mkexpr(t0), mkU32(0)), imm, bstmt);
         } else if (fmt >= 0x18 && has_msa) { /* BZ.df */
            Int df = fmt & 3;
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_V128);
            t2 = newTemp(Ity_V128);
            t3 = newTemp(Ity_V128);
            assign(t1, getWReg(ft));
            assign(t2, binop(Iop_64HLtoV128, mkU64(0), mkU64(0)));

            switch (df) {
               case 0x00: { /* BZ.B */
                  DIP("BZ.B w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ8x16, mkexpr(t1), mkexpr(t2)));
                  break;
               }

               case 0x01: { /* BZ.H */
                  DIP("BZ.H w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ16x8, mkexpr(t1), mkexpr(t2)));
                  break;
               }

               case 0x02: { /* BZ.W */
                  DIP("BZ.W w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ32x4, mkexpr(t1), mkexpr(t2)));
                  break;
               }

               case 0x03: { /* BZ.D */
                  DIP("BZ.D w%u, %u", ft, imm);
                  assign(t3, binop(Iop_CmpEQ64x2, mkexpr(t1), mkexpr(t2)));
                  break;
               }
            }

            assign(t0,
                   binop(Iop_Or32,
                         binop(Iop_Or32,
                               unop(Iop_V128to32, mkexpr(t3)),
                               unop(Iop_64HIto32, unop(Iop_V128to64, mkexpr(t3)))),
                         binop(Iop_Or32,
                               unop(Iop_64to32, unop(Iop_V128HIto64, mkexpr(t3))),
                               unop(Iop_64HIto32,
                                    unop(Iop_V128HIto64, mkexpr(t3))))));
            dis_branch(False,
                       binop(Iop_CmpNE32, mkexpr(t0), mkU32(0)), imm, bstmt);
         } else if (fmt == 0x0B && has_msa) { /* BZ.V */
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_V128);
            assign(t1, getWReg(ft));
            assign(t0,
                   binop(Iop_Or32,
                         binop(Iop_Or32,
                               unop(Iop_V128to32, mkexpr(t1)),
                               unop(Iop_64HIto32, unop(Iop_V128to64, mkexpr(t1)))),
                         binop(Iop_Or32,
                               unop(Iop_64to32, unop(Iop_V128HIto64, mkexpr(t1))),
                               unop(Iop_64HIto32,
                                    unop(Iop_V128HIto64, mkexpr(t1))))));
            dis_branch(False,
                       binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0)), imm, bstmt);
         } else if (fmt == 0x09) { /* BC1EQZ */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("bc1eqz f%u, %u", ft, imm);
               t1 = newTemp(Ity_I1);

               if (mode64) {
                  assign(t1, binop(Iop_CmpEQ64,
                                   binop(Iop_And64,
                                         unop(Iop_ReinterpF64asI64, getDReg(ft)),
                                         mkU64(1)),
                                   mkU64(0)));
               } else {
                  assign(t1, binop(Iop_CmpEQ32,
                                   binop(Iop_And32,
                                         unop(Iop_64to32,
                                              unop(Iop_ReinterpF64asI64,
                                                   getDReg(ft))),
                                         mkU32(1)),
                                   mkU32(0)));
               }

               dis_branch(False, mkexpr(t1), imm, bstmt);
            } else {
               ILLEGAL_INSTRUCTON
            }
         } else if (fmt == 0x0D) { /* BC1NEZ */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("bc1nez f%u, %u", ft, imm);
               t1 = newTemp(Ity_I1);

               if (mode64) {
                  assign(t1, binop(Iop_CmpNE64,
                                   binop(Iop_And64,
                                         unop(Iop_ReinterpF64asI64, getDReg(ft)),
                                         mkU64(1)),
                                   mkU64(0)));
               } else {
                  assign(t1, binop(Iop_CmpNE32,
                                   binop(Iop_And32,
                                         unop(Iop_64to32,
                                              unop(Iop_ReinterpF64asI64, getDReg(ft))),
                                         mkU32(1)),
                                   mkU32(0)));
               }

               dis_branch(False, mkexpr(t1), imm, bstmt);
            } else {
               ILLEGAL_INSTRUCTON;
               break;
            }
         } else {
            if (fmt == 0x15) { /* CMP.cond.d */
               Bool comparison = True;
               UInt signaling = CMPAFD;
               DIP("cmp.cond.d f%u, f%u, f%u, cond %u", fd, fs, ft, function);
               t0 = newTemp(Ity_I32);

               /* Conditions starting with S should signal exception on QNaN inputs. */
               switch (function) {
                  case 0x08:  /* SAF */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x00: /* AF */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             binop(Iop_I64StoF64,
                                   get_IR_roundingmode(), mkU64(0)));
                     break;

                  case 0x09: /* SUN */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x01: /* UN */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x45)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        binop(Iop_I64StoF64,
                                              get_IR_roundingmode(), mkU64(0))));
                     break;

                  case 0x19: /* SOR */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x11: /* OR */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x45)),
                                        binop(Iop_I64StoF64,
                                              get_IR_roundingmode(), mkU64(0)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL))));
                     break;

                  case 0x0A: /* SEQ */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x02: /* EQ */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x40)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        binop(Iop_I64StoF64,
                                              get_IR_roundingmode(), mkU64(0))));
                     break;

                  case 0x1A: /* SNEQ */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x12: /* NEQ */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x40)),
                                        binop(Iop_I64StoF64,
                                              get_IR_roundingmode(), mkU64(0)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL))));
                     break;

                  case 0x0B: /* SUEQ */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x03: /* UEQ */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x40)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x45)),
                                                   unop(Iop_ReinterpI64asF64,
                                                        mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                                   binop(Iop_I64StoF64,
                                                         get_IR_roundingmode(),
                                                         mkU64(0)))));
                     break;

                  case 0x1B:  /* SNEQ */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x13:  /* NEQ */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x00)),
                                                   unop(Iop_ReinterpI64asF64,
                                                        mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                                   binop(Iop_I64StoF64,
                                                         get_IR_roundingmode(),
                                                         mkU64(0)))));
                     break;

                  case 0x0C: /* SLT */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x04: /* LT */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        binop(Iop_I64StoF64,
                                              get_IR_roundingmode(), mkU64(0))));
                     break;

                  case 0x0D: /* SULT */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x05: /* ULT */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x45)),
                                                   unop(Iop_ReinterpI64asF64,
                                                        mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                                   binop(Iop_I64StoF64,
                                                         get_IR_roundingmode(),
                                                         mkU64(0)))));
                     break;

                  case 0x0E: /* SLE */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x06: /* LE */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x40)),
                                                   unop(Iop_ReinterpI64asF64,
                                                        mkU64(0xFFFFFFFFFFFFFFFFULL)),
                                                   binop(Iop_I64StoF64,
                                                         get_IR_roundingmode(),
                                                         mkU64(0)))));
                     break;

                  case 0x0F: /* SULE */
                     signaling = CMPSAFD; /* fallthrough */

                  case 0x07: /* ULE */
                     assign(t0, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
                     calculateFCSR(fs, ft, signaling, False, 2);
                     putDReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)),
                                        binop(Iop_I64StoF64,
                                              get_IR_roundingmode(), mkU64(0)),
                                        unop(Iop_ReinterpI64asF64,
                                             mkU64(0xFFFFFFFFFFFFFFFFULL))));
                     break;

                  default:
                     comparison = False;
               }

               if (comparison) {
                  if (!VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     ILLEGAL_INSTRUCTON
                  }

                  break;
               }

            } else if (fmt == 0x14) {
               Bool comparison = True;
               UInt signaling = CMPAFS;
               DIP("cmp.cond.s f%u, f%u, f%u, cond %u", fd, fs, ft, function);
               t0 = newTemp(Ity_I32);

               /* Conditions starting with S should signal exception on QNaN inputs. */
               switch (function) {
                  case 0x08:  /* SAF */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x00: /* AF */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             mkWidenFromF32(tyF,
                                            binop(Iop_I32StoF32,
                                                  get_IR_roundingmode(), mkU32(0))));
                     break;

                  case 0x09: /* SUN */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x01: /* UN */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x45)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        mkWidenFromF32(tyF,
                                                       binop(Iop_I32StoF32,
                                                             get_IR_roundingmode(),
                                                             mkU32(0)))));
                     break;

                  case 0x19: /* SOR */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x11: /* OR */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x45)),
                                        mkWidenFromF32(tyF,
                                                       binop(Iop_I32StoF32,
                                                             get_IR_roundingmode(),
                                                             mkU32(0))),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU)))));
                     break;

                  case 0x0A: /* SEQ */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x02: /* EQ */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x40)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        mkWidenFromF32(tyF,
                                                       binop(Iop_I32StoF32,
                                                             get_IR_roundingmode(),
                                                             mkU32(0)))));
                     break;

                  case 0x1A: /* SNEQ */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x12: /* NEQ */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x40)),
                                        mkWidenFromF32(tyF,
                                                       binop(Iop_I32StoF32,
                                                             get_IR_roundingmode(),
                                                             mkU32(0))),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU)))));
                     break;

                  case 0x0B: /* SUEQ */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x03: /* UEQ */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x40)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x45)),
                                                   mkWidenFromF32(tyF,
                                                         unop(Iop_ReinterpI32asF32,
                                                               mkU32(0xFFFFFFFFU))),
                                                   mkWidenFromF32(tyF,
                                                         binop(Iop_I32StoF32,
                                                               get_IR_roundingmode(),
                                                               mkU32(0))))));
                     break;

                  case 0x1B:  /* SNEQ */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x13:  /* NEQ */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x00)),
                                                   mkWidenFromF32(tyF,
                                                         unop(Iop_ReinterpI32asF32,
                                                               mkU32(0xFFFFFFFFU))),
                                                   mkWidenFromF32(tyF,
                                                         binop(Iop_I32StoF32,
                                                               get_IR_roundingmode(),
                                                               mkU32(0))))));
                     break;

                  case 0x0C: /* SLT */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x04: /* LT */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        mkWidenFromF32(tyF,
                                                       binop(Iop_I32StoF32,
                                                             get_IR_roundingmode(),
                                                             mkU32(0)))));
                     break;

                  case 0x0D: /* SULT */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x05: /* ULT */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x45)),
                                                   mkWidenFromF32(tyF,
                                                         unop(Iop_ReinterpI32asF32,
                                                               mkU32(0xFFFFFFFFU))),
                                                   mkWidenFromF32(tyF,
                                                         binop(Iop_I32StoF32,
                                                               get_IR_roundingmode(),
                                                               mkU32(0))))));
                     break;

                  case 0x0E: /* SLE */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x06: /* LE */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x01)),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU))),
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         mkexpr(t0), mkU32(0x40)),
                                                   mkWidenFromF32(tyF,
                                                         unop(Iop_ReinterpI32asF32,
                                                               mkU32(0xFFFFFFFFU))),
                                                   mkWidenFromF32(tyF,
                                                         binop(Iop_I32StoF32,
                                                               get_IR_roundingmode(),
                                                               mkU32(0))))));
                     break;

                  case 0x0F: /* SULE */
                     signaling = CMPSAFS; /* fallthrough */

                  case 0x07: /* ULE */
                     assign(t0, binop(Iop_CmpF32,
                                      getLoFromF64(Ity_F64, getFReg(fs)),
                                      getLoFromF64(Ity_F64, getFReg(ft))));
                     calculateFCSR(fs, ft, signaling, True, 2);
                     putFReg(fd,
                             IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)),
                                        mkWidenFromF32(tyF,
                                                       binop(Iop_I32StoF32,
                                                             get_IR_roundingmode(),
                                                             mkU32(0))),
                                        mkWidenFromF32(tyF,
                                                       unop(Iop_ReinterpI32asF32,
                                                            mkU32(0xFFFFFFFFU)))));
                     break;

                  default:
                     comparison = False;
               }

               if (comparison) {
                  if (!VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     ILLEGAL_INSTRUCTON
                  }

                  break;
               }
            }

            switch (function) {
               case 0x04: {  /* SQRT.fmt */
                  switch (fmt) {
                     case 0x10: {  /* S */
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, binop(Iop_SqrtF32, rm,
                                                              getLoFromF64(tyF, getFReg(fs)))));
                        break;
                     }

                     case 0x11: {  /* D */
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, binop(Iop_SqrtF64, rm, getDReg(fs)));
                        break;
                     }

                     default:
                        return -1;
                  }
               }
               break;    /* SQRT.fmt */

               case 0x05:  /* ABS.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("abs.s f%u, f%u", fd, fs);
                        putFReg(fd, mkWidenFromF32(tyF, unop(Iop_AbsF32,
                                                             getLoFromF64(tyF, getFReg(fs)))));
                        break;

                     case 0x11:  /* D  */
                        DIP("abs.d f%u, f%u", fd, fs);
                        putDReg(fd, unop(Iop_AbsF64, getDReg(fs)));
                        break;

                     default:
                        return -1;
                  }

                  break;  /* ABS.fmt */

               case 0x02:  /* MUL.fmt */
                  switch (fmt) {
                     case 0x11: {  /* D */
                        DIP("mul.d f%u, f%u, f%u", fd, fs, ft);
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, triop(Iop_MulF64, rm, getDReg(fs),
                                          getDReg(ft)));
                        break;
                     }

                     case 0x10: {  /* S */
                        DIP("mul.s f%u, f%u, f%u", fd, fs, ft);
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, triop(Iop_MulF32, rm,
                                                              getLoFromF64(tyF, getFReg(fs)),
                                                              getLoFromF64(tyF, getFReg(ft)))));
                        break;
                     }

                     default:
                        return -1;
                  }

                  break;  /* MUL.fmt */

               case 0x03:  /* DIV.fmt */
                  switch (fmt) {
                     case 0x11: {  /* D */
                        DIP("div.d f%u, f%u, f%u", fd, fs, ft);
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, triop(Iop_DivF64, rm, getDReg(fs),
                                          getDReg(ft)));
                        break;
                     }

                     case 0x10: {  /* S */
                        DIP("div.s f%u, f%u, f%u", fd, fs, ft);
                        calculateFCSR(fs, ft, DIVS, False, 2);
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32, rm,
                                                              getLoFromF64(tyF, getFReg(fs)),
                                                              getLoFromF64(tyF, getFReg(ft)))));
                        break;
                     }

                     default:
                        return -1;
                  }

                  break;  /* DIV.fmt */

               case 0x01:  /* SUB.fmt */
                  switch (fmt) {
                     case 0x11: {  /* D */
                        DIP("sub.d f%u, f%u, f%u", fd, fs, ft);
                        calculateFCSR(fs, ft, SUBD, False, 2);
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, triop(Iop_SubF64, rm, getDReg(fs),
                                          getDReg(ft)));
                        break;
                     }

                     case 0x10: {  /* S */
                        DIP("sub.s f%u, f%u, f%u", fd, fs, ft);
                        calculateFCSR(fs, ft, SUBS, True, 2);
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, triop(Iop_SubF32, rm,
                                                              getLoFromF64(tyF, getFReg(fs)),
                                                              getLoFromF64(tyF, getFReg(ft)))));
                        break;
                     }

                     default:
                        return -1;
                  }

                  break;  /* SUB.fmt */

               case 0x06:  /* MOV.fmt */
                  switch (fmt) {
                     case 0x11:  /* D */
                        DIP("mov.d f%u, f%u", fd, fs);

                        if (fp_mode64) {
                           putDReg(fd, getDReg(fs));
                        } else {
                           putFReg(fd, getFReg(fs));
                           putFReg(fd + 1, getFReg(fs + 1));
                        }

                        break;

                     case 0x10:  /* S */
                        DIP("mov.s f%u, f%u", fd, fs);
                        putFReg(fd, getFReg(fs));
                        break;

                     default:
                        return -1;
                  }

                  break;  /* MOV.fmt */

               case 0x07:  /* NEG.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("neg.s f%u, f%u", fd, fs);
                        putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32,
                                                             getLoFromF64(tyF, getFReg(fs)))));
                        break;

                     case 0x11:  /* D */
                        DIP("neg.d f%u, f%u", fd, fs);
                        putDReg(fd, unop(Iop_NegF64, getDReg(fs)));
                        break;

                     default:
                        return -1;
                  }

                  break;  /* NEG.fmt */

               case 0x08:  /* ROUND.L.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("round.l.s f%u, f%u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, ROUNDLS, True, 1);
                           t0 = newTemp(Ity_I64);

                           assign(t0, binop(Iop_F32toI64S, mkU32(0x0),
                                            getLoFromF64(Ity_F64, getFReg(fs))));

                           putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     case 0x11:  /* D */
                        DIP("round.l.d f%u, f%u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, ROUNDLD, False, 1);
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            binop(Iop_F64toI64S,
                                                  mkU32(0x0),
                                                  getDReg(fs))));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     default:
                        return -1;

                  }

                  break;  /* ROUND.L.fmt */

               case 0x09:  /* TRUNC.L.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("trunc.l.s f%u, f%u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, TRUNCLS, True, 1);
                           t0 = newTemp(Ity_I64);
                           assign(t0, binop(Iop_F32toI64S, mkU32(0x3),
                                            getLoFromF64(Ity_F64, getFReg(fs))));

                           putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     case 0x11:  /* D */
                        DIP("trunc.l.d f%u, f%u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, TRUNCLD, False, 1);
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            binop(Iop_F64toI64S,
                                                  mkU32(0x3),
                                                  getDReg(fs))));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     default:
                        return -1;
                  }

                  break;  /* TRUNC.L.fmt */

               case 0x15:  /* RECIP.fmt */
                  switch (fmt) {
                     case 0x10: {  /* S */
                        DIP("recip.s f%u, f%u", fd, fs);
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32,
                                                              rm, unop(Iop_ReinterpI32asF32,
                                                                    mkU32(ONE_SINGLE)), getLoFromF64(tyF,
                                                                          getFReg(fs)))));
                        break;
                     }

                     case 0x11: {  /* D */
                        DIP("recip.d f%u, f%u", fd, fs);
                        IRExpr *rm = get_IR_roundingmode();
                        /* putDReg(fd, 1.0/getDreg(fs)); */
                        putDReg(fd, triop(Iop_DivF64, rm,
                                          unop(Iop_ReinterpI64asF64,
                                               mkU64(ONE_DOUBLE)), getDReg(fs)));
                        break;
                     }

                     default:
                        return -1;

                  }

                  break;  /* RECIP.fmt */

               case 0x13:  /* MOVN.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("movn.s f%u, f%u, r%u", fd, fs, rt);
                        t1 = newTemp(Ity_I1);

                        if (mode64)
                           assign(t1, binop(Iop_CmpNE64, mkU64(0), getIReg(rt)));
                        else
                           assign(t1, binop(Iop_CmpNE32, mkU32(0), getIReg(rt)));

                        putFReg(fd, IRExpr_ITE(mkexpr(t1), getFReg(fs), getFReg(fd)));
                        break;

                     case 0x11:  /* D */
                        DIP("movn.d f%u, f%u, r%u", fd, fs, rt);
                        t1 = newTemp(Ity_I1);

                        if (mode64)
                           assign(t1, binop(Iop_CmpNE64, mkU64(0), getIReg(rt)));
                        else
                           assign(t1, binop(Iop_CmpNE32, mkU32(0), getIReg(rt)));

                        putDReg(fd, IRExpr_ITE(mkexpr(t1), getDReg(fs), getDReg(fd)));
                        break;

                     default:
                        return -1;
                  }

                  break;  /* MOVN.fmt */

               case 0x12:  /* MOVZ.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("movz.s f%u, f%u, r%u", fd, fs, rt);
                        t1 = newTemp(Ity_I1);

                        if (mode64)
                           assign(t1, binop(Iop_CmpEQ64, mkU64(0), getIReg(rt)));
                        else
                           assign(t1, binop(Iop_CmpEQ32, mkU32(0), getIReg(rt)));

                        putFReg(fd, IRExpr_ITE(mkexpr(t1), getFReg(fs), getFReg(fd)));
                        break;

                     case 0x11:  /* D */
                        DIP("movz.d f%u, f%u, r%u", fd, fs, rt);
                        t1 = newTemp(Ity_I1);

                        if (mode64)
                           assign(t1, binop(Iop_CmpEQ64, mkU64(0), getIReg(rt)));
                        else
                           assign(t1, binop(Iop_CmpEQ32, mkU32(0), getIReg(rt)));

                        putDReg(fd, IRExpr_ITE(mkexpr(t1), getDReg(fs), getDReg(fd)));
                        break;

                     default:
                        return -1;
                  }

                  break;  /* MOVZ.fmt */

               case 0x11:  /* MOVT.fmt */
                  if (tf == 1) {
                     UInt mov_cc = get_mov_cc(cins);

                     switch (fmt) {  /* MOVCF = 010001 */
                        case 0x11:  /* D */
                           DIP("movt.d f%u, f%u, %u", fd, fs, mov_cc);
                           t1 = newTemp(Ity_I1);
                           t2 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_I1);
                           t4 = newTemp(Ity_F64);

                           assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                           assign(t2, IRExpr_ITE(mkexpr(t1),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(23)),
                                                       mkU32(0x1)),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(24 + mov_cc)),
                                                       mkU32(0x1))
                                                ));

                           assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
                           assign(t4, IRExpr_ITE(mkexpr(t3),
                                                 getDReg(fs), getDReg(fd)));
                           putDReg(fd, mkexpr(t4));
                           break;

                        case 0x10:  /* S */
                           DIP("movt.s f%u, f%u, %u", fd, fs, mov_cc);
                           t1 = newTemp(Ity_I1);
                           t2 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_I1);
                           t4 = newTemp(Ity_F64);
                           t5 = newTemp(Ity_F64);
                           t6 = newTemp(Ity_F64);
                           t7 = newTemp(Ity_I64);

                           if (fp_mode64) {
                              assign(t5, getFReg(fs));
                              assign(t6, getFReg(fd));
                           } else {
                              assign(t5, unop(Iop_F32toF64, getFReg(fs)));
                              assign(t6, unop(Iop_F32toF64, getFReg(fd)));
                           }

                           assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                           assign(t2, IRExpr_ITE(mkexpr(t1),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(23)),
                                                       mkU32(0x1)),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(24 + mov_cc)),
                                                       mkU32(0x1))
                                                ));

                           assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
                           assign(t4, IRExpr_ITE(mkexpr(t3),
                                                 mkexpr(t5), mkexpr(t6)));

                           if (fp_mode64) {
                              IRTemp f = newTemp(Ity_F64);
                              IRTemp fd_hi = newTemp(Ity_I32);
                              assign(f, getFReg(fd));
                              assign(fd_hi, unop(Iop_64HIto32,
                                                 unop(Iop_ReinterpF64asI64, mkexpr(f))));
                              assign(t7, mkWidenFrom32(Ity_I64, unop(Iop_64to32,
                                                                     unop(Iop_ReinterpF64asI64, mkexpr(t4))),
                                                       True));

                              putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t7)));
                           } else
                              putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                                mkexpr(t4)));

                           break;

                        default:
                           return -1;
                     }
                  } else if (tf == 0) { /* MOVF.fmt */
                     UInt mov_cc = get_mov_cc(cins);

                     switch (fmt) { /* MOVCF = 010001 */
                        case 0x11:  /* D */
                           DIP("movf.d f%u, f%u, %u", fd, fs, mov_cc);
                           t1 = newTemp(Ity_I1);
                           t2 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_I1);
                           t4 = newTemp(Ity_F64);

                           assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                           assign(t2, IRExpr_ITE(mkexpr(t1),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(23)),
                                                       mkU32(0x1)),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(24 + mov_cc)),
                                                       mkU32(0x1))
                                                ));

                           assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
                           assign(t4, IRExpr_ITE(mkexpr(t3),
                                                 getDReg(fs), getDReg(fd)));
                           putDReg(fd, mkexpr(t4));
                           break;

                        case 0x10:  /* S */
                           DIP("movf.s f%u, f%u, %u", fd, fs, mov_cc);
                           t1 = newTemp(Ity_I1);
                           t2 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_I1);
                           t4 = newTemp(Ity_F64);
                           t5 = newTemp(Ity_F64);
                           t6 = newTemp(Ity_F64);

                           if (fp_mode64) {
                              assign(t5, getFReg(fs));
                              assign(t6, getFReg(fd));
                           } else {
                              assign(t5, unop(Iop_F32toF64, getFReg(fs)));
                              assign(t6, unop(Iop_F32toF64, getFReg(fd)));
                           }

                           assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                           assign(t2, IRExpr_ITE(mkexpr(t1),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(23)),
                                                       mkU32(0x1)),
                                                 binop(Iop_And32,
                                                       binop(Iop_Shr32, getFCSR(),
                                                             mkU8(24 + mov_cc)),
                                                       mkU32(0x1))
                                                ));

                           assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
                           assign(t4, IRExpr_ITE(mkexpr(t3),
                                                 mkexpr(t5), mkexpr(t6)));

                           if (fp_mode64) {
                              IRTemp f = newTemp(Ity_F64);
                              IRTemp fd_hi = newTemp(Ity_I32);
                              t7 = newTemp(Ity_I64);
                              assign(f, getFReg(fd));
                              assign(fd_hi, unop(Iop_64HIto32,
                                                 unop(Iop_ReinterpF64asI64, mkexpr(f))));
                              assign(t7, mkWidenFrom32(Ity_I64, unop(Iop_64to32,
                                                                     unop(Iop_ReinterpF64asI64, mkexpr(t4))),
                                                       True));

                              putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t7)));
                           } else
                              putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                                mkexpr(t4)));

                           break;

                        default:
                           return -1;
                     }
                  }

                  break;  /* MOVT.fmt */

               case 0x00:  /* ADD.fmt */
                  switch (fmt) {
                     case 0x10: {  /* S */
                        DIP("add.s f%u, f%u, f%u", fd, fs, ft);
                        calculateFCSR(fs, ft, ADDS, True, 2);
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, triop(Iop_AddF32, rm,
                                                              getLoFromF64(tyF, getFReg(fs)),
                                                              getLoFromF64(tyF, getFReg(ft)))));
                        break;
                     }

                     case 0x11: {  /* D */
                        DIP("add.d f%u, f%u, f%u", fd, fs, ft);
                        calculateFCSR(fs, ft, ADDD, False, 2);
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, triop(Iop_AddF64, rm, getDReg(fs), getDReg(ft)));
                        break;
                     }

                     case 0x04:  /* MTC1 (Move Word to Floating Point) */
                        DIP("mtc1 r%u, f%u", rt, fs);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I32);
                           t1 = newTemp(Ity_F32);
                           assign(t0, mkNarrowTo32(ty, getIReg(rt)));
                           assign(t1, unop(Iop_ReinterpI32asF32, mkexpr(t0)));

                           putFReg(fs, mkWidenFromF32(tyF, mkexpr(t1)));
                        } else
                           putFReg(fs, unop(Iop_ReinterpI32asF32,
                                            mkNarrowTo32(ty, getIReg(rt))));

                        break;

                     case 0x05:  /* Doubleword Move to Floating Point DMTC1; MIPS64 */
                        DIP("dmtc1 r%u, f%u", rt, fs);
                        vassert(mode64);
                        putDReg(fs, unop(Iop_ReinterpI64asF64, getIReg(rt)));
                        break;

                     case 0x00:  /* MFC1 */
                        DIP("mfc1 r%u, f%u", rt, fs);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I64);
                           t1 = newTemp(Ity_I32);
                           assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));
                           assign(t1, unop(Iop_64to32, mkexpr(t0)));
                           putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));
                        } else
                           putIReg(rt, mkWidenFrom32(ty,
                                                     unop(Iop_ReinterpF32asI32, getFReg(fs)),
                                                     True));

                        break;

                     case 0x01:  /* Doubleword Move from Floating Point DMFC1;
                           MIPS64 */
                        DIP("dmfc1 r%u, f%u", rt, fs);
                        putIReg(rt, unop(Iop_ReinterpF64asI64, getDReg(fs)));
                        break;

                     case 0x06:  /* CTC1 */
                        DIP("ctc1 r%u, f%u", rt, fs);
                        t0 = newTemp(Ity_I32);
                        t1 = newTemp(Ity_I32);
                        t2 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_I32);
                        t4 = newTemp(Ity_I32);
                        t5 = newTemp(Ity_I32);
                        t6 = newTemp(Ity_I32);
                        assign(t0, mkNarrowTo32(ty, getIReg(rt)));

                        if (fs == 25) {  /* FCCR */
                           assign(t1, binop(Iop_Shl32, binop(Iop_And32, mkexpr(t0),
                                                             mkU32(0x000000FE)), mkU8(24)));
                           assign(t2, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x01000000)));
                           assign(t3, binop(Iop_Shl32, binop(Iop_And32, mkexpr(t0),
                                                             mkU32(0x00000001)), mkU8(23)));
                           assign(t4, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x007FFFFF)));
                           putFCSR(binop(Iop_Or32, binop(Iop_Or32, mkexpr(t1),
                                                         mkexpr(t2)), binop(Iop_Or32, mkexpr(t3),
                                                               mkexpr(t4))));
                        } else if (fs == 26) {  /* FEXR */
                           assign(t1, binop(Iop_And32, getFCSR(), mkU32(0xFFFC0000)));
                           assign(t2, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x0003F000)));
                           assign(t3, binop(Iop_And32, getFCSR(), mkU32(0x00000F80)));
                           assign(t4, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x0000007C)));
                           assign(t5, binop(Iop_And32, getFCSR(), mkU32(0x00000003)));
                           putFCSR(binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32,
                                                         mkexpr(t1), mkexpr(t2)), binop(Iop_Or32,
                                                               mkexpr(t3), mkexpr(t4))), mkexpr(t5)));
                        } else if (fs == 28) {
                           assign(t1, binop(Iop_And32, getFCSR(), mkU32(0xFE000000)));
                           assign(t2, binop(Iop_Shl32, binop(Iop_And32, mkexpr(t0),
                                                             mkU32(0x00000002)), mkU8(22)));
                           assign(t3, binop(Iop_And32, getFCSR(), mkU32(0x00FFF000)));
                           assign(t4, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x00000F80)));
                           assign(t5, binop(Iop_And32, getFCSR(), mkU32(0x0000007C)));
                           assign(t6, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x00000003)));
                           putFCSR(binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32,
                                                         mkexpr(t1), mkexpr(t2)), binop(Iop_Or32,
                                                               mkexpr(t3), mkexpr(t4))), binop(Iop_Or32,
                                                                     mkexpr(t5), mkexpr(t6))));
                        } else if (fs == 31) {
                           putFCSR(mkexpr(t0));
                        }

                        break;

                     case 0x02:  /* CFC1 */
                        DIP("cfc1 r%u, f%u", rt, fs);
                        t0 = newTemp(Ity_I32);
                        t1 = newTemp(Ity_I32);
                        t2 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_I32);
                        t4 = newTemp(Ity_I32);
                        t5 = newTemp(Ity_I32);
                        t6 = newTemp(Ity_I32);
                        assign(t0, getFCSR());

                        if (fs == 0) {
                           putIReg(rt, mkWidenFrom32(ty,
                                                     IRExpr_Get(offsetof(VexGuestMIPS32State,
                                                                guest_FIR),
                                                                Ity_I32),
                                                     False));
                        } else if (fs == 25) {
                           assign(t1, mkU32(0x000000FF));
                           assign(t2, binop(Iop_Shr32, binop(Iop_And32, mkexpr(t0),
                                                             mkU32(0xFE000000)), mkU8(25)));
                           assign(t3, binop(Iop_Shr32, binop(Iop_And32, mkexpr(t0),
                                                             mkU32(0x00800000)), mkU8(23)));
                           putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32,
                                                               binop(Iop_Or32, mkexpr(t1), mkexpr(t2)),
                                                               mkexpr(t3)), False));
                        } else if (fs == 26) {
                           assign(t1, mkU32(0xFFFFF07C));
                           assign(t2, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x0003F000)));
                           assign(t3, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x0000007C)));
                           putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32,
                                                               binop(Iop_Or32, mkexpr(t1), mkexpr(t2)),
                                                               mkexpr(t3)), False));
                        } else if (fs == 28) {
                           assign(t1, mkU32(0x00000F87));
                           assign(t2, binop(Iop_And32, mkexpr(t0),
                                            mkU32(0x00000F83)));
                           assign(t3, binop(Iop_Shr32, binop(Iop_And32, mkexpr(t0),
                                                             mkU32(0x01000000)), mkU8(22)));
                           putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32,
                                                               binop(Iop_Or32, mkexpr(t1), mkexpr(t2)),
                                                               mkexpr(t3)), False));
                        } else if (fs == 31) {
                           putIReg(rt, mkWidenFrom32(ty, getFCSR(), False));
                        }

                        break;

                     default:
                        return -1;
                  }

                  break;

               case 0x21:  /* CVT.D */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("cvt.d.s f%u, f%u", fd, fs);
                        calculateFCSR(fs, 0, CVTDS, True, 1);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I64);
                           t1 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_F32);
                           t4 = newTemp(Ity_F32);
                           /* get lo half of FPR */
                           assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                           assign(t1, unop(Iop_64to32, mkexpr(t0)));

                           assign(t3, unop(Iop_ReinterpI32asF32, mkexpr(t1)));

                           putFReg(fd, unop(Iop_F32toF64, mkexpr(t3)));
                        } else
                           putDReg(fd, unop(Iop_F32toF64, getFReg(fs)));

                        break;

                     case 0x14:  /* W */
                        DIP("cvt.d.w %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CVTDW, True, 1);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I64);
                           t1 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_F32);
                           t4 = newTemp(Ity_F32);
                           /* get lo half of FPR */
                           assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                           assign(t1, unop(Iop_64to32, mkexpr(t0)));
                           putDReg(fd, unop(Iop_I32StoF64, mkexpr(t1)));
                           break;
                        } else {
                           t0 = newTemp(Ity_I32);
                           assign(t0, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                           putDReg(fd, unop(Iop_I32StoF64, mkexpr(t0)));
                           break;
                        }

                     case 0x15: {  /* L */
                        if (fp_mode64) {
                           DIP("cvt.d.l %u, %u", fd, fs);
                           calculateFCSR(fs, 0, CVTDL, False, 1);
                           t0 = newTemp(Ity_I64);
                           assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                           putFReg(fd, binop(Iop_I64StoF64,
                                             get_IR_roundingmode(), mkexpr(t0)));
                           break;
                        } else
                           return -1;
                     }

                     default:
                        return -1;
                  }

                  break;  /* CVT.D */

               case 0x20:  /* CVT.s */
                  switch (fmt) {
                     case 0x14:  /* W */
                        DIP("cvt.s.w %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CVTSW, True, 1);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I64);
                           t1 = newTemp(Ity_I32);
                           t3 = newTemp(Ity_F32);
                           t4 = newTemp(Ity_F32);
                           /* get lo half of FPR */
                           assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                           assign(t1, unop(Iop_64to32, mkexpr(t0)));
                           putFReg(fd, mkWidenFromF32(tyF, binop(Iop_I32StoF32,
                                                                 get_IR_roundingmode(), mkexpr(t1))));
                        } else {
                           t0 = newTemp(Ity_I32);
                           assign(t0, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                           putFReg(fd, binop(Iop_I32StoF32, get_IR_roundingmode(),
                                             mkexpr(t0)));
                        }

                        break;

                     case 0x11:  /* D */
                        DIP("cvt.s.d %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CVTSD, False, 1);
                        t0 = newTemp(Ity_F32);
                        assign(t0, binop(Iop_F64toF32, get_IR_roundingmode(),
                                         getDReg(fs)));
                        putFReg(fd, mkWidenFromF32(tyF, mkexpr(t0)));
                        break;

                     case 0x15:  /* L */
                        DIP("cvt.s.l %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, CVTSL, False, 1);
                           t0 = newTemp(Ity_I64);
                           assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                           putFReg(fd, mkWidenFromF32(tyF, binop(Iop_I64StoF32,
                                                                 get_IR_roundingmode(), mkexpr(t0))));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     default:
                        return -1;
                  }

                  break;  /* CVT.s */

               case 0x24:  /* CVT.w */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("cvt.w.s %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CVTWS, True, 1);
                        putFReg(fd,
                                mkWidenFromF32(tyF,
                                               unop(Iop_ReinterpI32asF32,
                                                    binop(Iop_F32toI32S,
                                                          get_IR_roundingmode(),
                                                          getLoFromF64(tyF,
                                                                getFReg(fs))))));
                        break;

                     case 0x11:  /* D */
                        DIP("cvt.w.d %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CVTWD, False, 1);
                        t0 = newTemp(Ity_I32);
                        t1 = newTemp(Ity_F32);
                        assign(t0, binop(Iop_F64toI32S, get_IR_roundingmode(),
                                         getDReg(fs)));
                        assign(t1, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                        putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
                        break;

                     default:
                        return -1;

                  }

                  break;

               case 0x25:  /* CVT.l */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("cvt.l.s %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, CVTLS, True, 1);
                           t0 = newTemp(Ity_I64);

                           assign(t0, binop(Iop_F32toI64S, get_IR_roundingmode(),
                                            getLoFromF64(tyF, getFReg(fs))));

                           putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     case 0x11: {  /* D */
                        DIP("cvt.l.d %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, CVTLD, False, 1);
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            binop(Iop_F64toI64S,
                                                  get_IR_roundingmode(),
                                                  getDReg(fs))));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;
                     }

                     default:
                        return -1;
                  }

                  break;

               case 0x0B:  /* FLOOR.L.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("floor.l.s %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, FLOORLS, True, 1);
                           t0 = newTemp(Ity_I64);

                           assign(t0, binop(Iop_F32toI64S, mkU32(0x1),
                                            getLoFromF64(tyF, getFReg(fs))));

                           putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     case 0x11:  /* D */
                        DIP("floor.l.d %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, FLOORLD, False, 1);
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            binop(Iop_F64toI64S,
                                                  mkU32(0x01),
                                                  getDReg(fs))));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     default:
                        return -1;
                  }

                  break;

               case 0x0C:  /* ROUND.W.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("round.w.s f%u, f%u", fd, fs);
                        calculateFCSR(fs, 0, ROUNDWS, True, 1);
                        putFReg(fd,
                                mkWidenFromF32(tyF,
                                               unop(Iop_ReinterpI32asF32,
                                                    binop(Iop_F32toI32S,
                                                          mkU32(0x0),
                                                          getLoFromF64(tyF,
                                                                getFReg(fs))))));
                        break;

                     case 0x11:  /* D */
                        DIP("round.w.d f%u, f%u", fd, fs);
                        calculateFCSR(fs, 0, ROUNDWD, False, 1);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I32);
                           assign(t0, binop(Iop_F64toI32S, mkU32(0x0),
                                            getDReg(fs)));
                           putFReg(fd, mkWidenFromF32(tyF,
                                                      unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                        } else {
                           t0 = newTemp(Ity_I32);

                           assign(t0, binop(Iop_F64toI32S, mkU32(0x0),
                                            getDReg(fs)));

                           putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                        }

                        break;

                     default:
                        return -1;

                  }

                  break;  /* ROUND.W.fmt */

               case 0x0F:  /* FLOOR.W.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("floor.w.s f%u, f%u", fd, fs);
                        calculateFCSR(fs, 0, FLOORWS, True, 1);
                        putFReg(fd,
                                mkWidenFromF32(tyF,
                                               unop(Iop_ReinterpI32asF32,
                                                    binop(Iop_F32toI32S,
                                                          mkU32(0x1),
                                                          getLoFromF64(tyF,
                                                                getFReg(fs))))));
                        break;

                     case 0x11:  /* D */
                        DIP("floor.w.d f%u, f%u", fd, fs);
                        calculateFCSR(fs, 0, FLOORWD, False, 1);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I32);
                           assign(t0, binop(Iop_F64toI32S, mkU32(0x1),
                                            getDReg(fs)));
                           putFReg(fd, mkWidenFromF32(tyF,
                                                      unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                           break;
                        } else {
                           t0 = newTemp(Ity_I32);

                           assign(t0, binop(Iop_F64toI32S, mkU32(0x1),
                                            getDReg(fs)));

                           putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                           break;
                        }

                     default:
                        return -1;

                  }

                  break;  /* FLOOR.W.fmt */

               case 0x0D:  /* TRUNC.W */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("trunc.w.s %u, %u", fd, fs);
                        calculateFCSR(fs, 0, TRUNCWS, True, 1);
                        putFReg(fd,
                                mkWidenFromF32(tyF,
                                               unop(Iop_ReinterpI32asF32,
                                                    binop(Iop_F32toI32S,
                                                          mkU32(0x3),
                                                          getLoFromF64(tyF,
                                                                getFReg(fs))))));
                        break;

                     case 0x11:  /* D */
                        DIP("trunc.w.d %u, %u", fd, fs);
                        calculateFCSR(fs, 0, TRUNCWD, False, 1);

                        if (fp_mode64) {
                           t0 = newTemp(Ity_I32);

                           assign(t0, binop(Iop_F64toI32S, mkU32(0x3),
                                            getFReg(fs)));

                           putFReg(fd, mkWidenFromF32(tyF,
                                                      unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                        } else {
                           t0 = newTemp(Ity_I32);

                           assign(t0, binop(Iop_F64toI32S, mkU32(0x3),
                                            getDReg(fs)));

                           putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                        }

                        break;

                     default:
                        return -1;

                  }

                  break;

               case 0x0E:  /* CEIL.W.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("ceil.w.s %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CEILWS, True, 1);
                        putFReg(fd,
                                mkWidenFromF32(tyF,
                                               unop(Iop_ReinterpI32asF32,
                                                    binop(Iop_F32toI32S,
                                                          mkU32(0x2),
                                                          getLoFromF64(tyF,
                                                                getFReg(fs))))));
                        break;

                     case 0x11:  /* D */
                        DIP("ceil.w.d %u, %u", fd, fs);
                        calculateFCSR(fs, 0, CEILWD, False, 1);

                        if (!fp_mode64) {
                           t0 = newTemp(Ity_I32);
                           assign(t0, binop(Iop_F64toI32S, mkU32(0x2),
                                            getDReg(fs)));
                           putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                        } else {
                           t0 = newTemp(Ity_I32);
                           assign(t0, binop(Iop_F64toI32S, mkU32(0x2),
                                            getDReg(fs)));
                           putFReg(fd, mkWidenFromF32(tyF,
                                                      unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                        }

                        break;

                     default:
                        return -1;

                  }

                  break;

               case 0x0A:  /* CEIL.L.fmt */
                  switch (fmt) {
                     case 0x10:  /* S */
                        DIP("ceil.l.s %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, CEILLS, True, 1);
                           t0 = newTemp(Ity_I64);

                           assign(t0, binop(Iop_F32toI64S, mkU32(0x2),
                                            getLoFromF64(tyF, getFReg(fs))));

                           putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     case 0x11:  /* D */
                        DIP("ceil.l.d %u, %u", fd, fs);

                        if (fp_mode64) {
                           calculateFCSR(fs, 0, CEILLD, False, 1);
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            binop(Iop_F64toI64S,
                                                  mkU32(0x2),
                                                  getDReg(fs))));
                        } else {
                           ILLEGAL_INSTRUCTON
                        }

                        break;

                     default:
                        return -1;

                  }

                  break;

               case 0x16:  /* RSQRT.fmt */
                  switch (fmt) {
                     case 0x10: {  /* S */
                        DIP("rsqrt.s %u, %u", fd, fs);
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32, rm,
                                                              unop(Iop_ReinterpI32asF32, mkU32(ONE_SINGLE)),
                                                              binop(Iop_SqrtF32, rm, getLoFromF64(tyF,
                                                                    getFReg(fs))))));
                        break;
                     }

                     case 0x11: {  /* D */
                        DIP("rsqrt.d %u, %u", fd, fs);
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, triop(Iop_DivF64, rm,
                                          unop(Iop_ReinterpI64asF64,
                                               mkU64(ONE_DOUBLE)),
                                          binop(Iop_SqrtF64, rm, getDReg(fs))));
                        break;
                     }

                     default:
                        return -1;

                  }

                  break;

               case 0x18: /* MADDF.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("maddf.d f%u, f%u, f%u", fd, fs, ft);
                           IRExpr *rm = get_IR_roundingmode();
                           putDReg(fd, qop(Iop_MAddF64, rm, getDReg(fs), getDReg(ft),
                                           getDReg(fd)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("maddf.s f%u, f%u, f%u", fd, fs, ft);
                           IRExpr *rm = get_IR_roundingmode();
                           t1 = newTemp(Ity_F32);
                           assign(t1, qop(Iop_MAddF32, rm,
                                          getLoFromF64(tyF, getFReg(fs)),
                                          getLoFromF64(tyF, getFReg(ft)),
                                          getLoFromF64(tyF, getFReg(fd))));
                           putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
                           break;
                        }

                        default:
                           return -1;
                     }
                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x19: /* MSUBF.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("msubf.d f%u, f%u, f%u", fd, fs, ft);
                           IRExpr *rm = get_IR_roundingmode();
                           putDReg(fd, qop(Iop_MSubF64, rm, getDReg(fs),
                                           getDReg(ft), getDReg(fd)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("msubf.s f%u, f%u, f%u", fd, fs, ft);
                           IRExpr *rm = get_IR_roundingmode();
                           t1 = newTemp(Ity_F32);
                           assign(t1, qop(Iop_MSubF32, rm,
                                          getLoFromF64(tyF, getFReg(fs)),
                                          getLoFromF64(tyF, getFReg(ft)),
                                          getLoFromF64(tyF, getFReg(fd))));
                           putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
                           break;
                        }

                        default:
                           return -1;
                     }
                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x1E: /* MAX.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("max.d f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MAXD, False, 2);
                           putDReg(fd, binop(Iop_MaxNumF64, getDReg(fs), getDReg(ft)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("max.s f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MAXS, True, 2);
                           putFReg(fd, mkWidenFromF32(tyF, binop(Iop_MaxNumF32,
                                                                 getLoFromF64(Ity_F64,
                                                                       getFReg(fs)),
                                                                 getLoFromF64(Ity_F64,
                                                                       getFReg(ft)))));
                           break;
                        }

                        default:
                           return -1;
                     }
                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x1C: /* MIN.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("min.d f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MIND, False, 2);
                           putDReg(fd, binop(Iop_MinNumF64, getDReg(fs), getDReg(ft)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("min.s f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MINS, True, 2);
                           putFReg(fd, mkWidenFromF32(tyF, binop(Iop_MinNumF32,
                                                                 getLoFromF64(Ity_F64,
                                                                       getFReg(fs)),
                                                                 getLoFromF64(Ity_F64,
                                                                       getFReg(ft)))));
                           break;
                        }

                        default:
                           return -1;
                     }
                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x1F: /* MAXA.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("maxa.d f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MAXAD, False, 2);
                           t1 = newTemp(Ity_F64);
                           t2 = newTemp(Ity_F64);
                           t3 = newTemp(Ity_F64);
                           t4 = newTemp(Ity_I1);
                           assign(t1, unop(Iop_AbsF64, getFReg(fs)));
                           assign(t2, unop(Iop_AbsF64, getFReg(ft)));
                           assign(t3, binop(Iop_MaxNumF64, mkexpr(t1), mkexpr(t2)));
                           assign(t4, binop(Iop_CmpEQ32,
                                            binop(Iop_CmpF64, mkexpr(t3), mkexpr(t1)),
                                            mkU32(0x40)));
                           putFReg(fd, IRExpr_ITE(mkexpr(t4),
                                                  getFReg(fs), getFReg(ft)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("maxa.s f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MAXAS, True, 2);
                           t1 = newTemp(Ity_F32);
                           t2 = newTemp(Ity_F32);
                           t3 = newTemp(Ity_F32);
                           t4 = newTemp(Ity_I1);
                           assign(t1, unop(Iop_AbsF32, getLoFromF64(Ity_F64,
                                           getFReg(fs))));
                           assign(t2, unop(Iop_AbsF32, getLoFromF64(Ity_F64,
                                           getFReg(ft))));
                           assign(t3, binop(Iop_MaxNumF32, mkexpr(t1), mkexpr(t2)));
                           assign(t4, binop(Iop_CmpEQ32,
                                            binop(Iop_CmpF32, mkexpr(t3), mkexpr(t1)),
                                            mkU32(0x40)));
                           putFReg(fd, IRExpr_ITE(mkexpr(t4),
                                                  getFReg(fs), getFReg(ft)));
                           break;
                        }

                        default:
                           return -1;
                     }

                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x1D: /* MINA.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("mina.d f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MINAD, False, 2);
                           t1 = newTemp(Ity_F64);
                           t2 = newTemp(Ity_F64);
                           t3 = newTemp(Ity_F64);
                           t4 = newTemp(Ity_I1);
                           assign(t1, unop(Iop_AbsF64, getFReg(fs)));
                           assign(t2, unop(Iop_AbsF64, getFReg(ft)));
                           assign(t3, binop(Iop_MinNumF64, mkexpr(t1), mkexpr(t2)));
                           assign(t4, binop(Iop_CmpEQ32,
                                            binop(Iop_CmpF64, mkexpr(t3), mkexpr(t1)),
                                            mkU32(0x40)));
                           putFReg(fd, IRExpr_ITE(mkexpr(t4),
                                                  getFReg(fs), getFReg(ft)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("mina.s f%u, f%u, f%u", fd, fs, ft);
                           calculateFCSR(fs, ft, MINAS, True, 2);
                           t1 = newTemp(Ity_F32);
                           t2 = newTemp(Ity_F32);
                           t3 = newTemp(Ity_F32);
                           t4 = newTemp(Ity_I1);
                           assign(t1, unop(Iop_AbsF32, getLoFromF64(Ity_F64,
                                           getFReg(fs))));
                           assign(t2, unop(Iop_AbsF32, getLoFromF64(Ity_F64,
                                           getFReg(ft))));
                           assign(t3, binop(Iop_MinNumF32, mkexpr(t1), mkexpr(t2)));
                           assign(t4, binop(Iop_CmpEQ32,
                                            binop(Iop_CmpF32, mkexpr(t3), mkexpr(t1)),
                                            mkU32(0x40)));
                           putFReg(fd, IRExpr_ITE(mkexpr(t4),
                                                  getFReg(fs), getFReg(ft)));
                           break;
                        }

                        default:
                           return -1;
                     }
                  }

                  break;

               case 0x1A: /* RINT.fmt */
                  if (ft == 0) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("rint.d f%u, f%u", fd, fs);
                           calculateFCSR(fs, 0, RINTS, True, 1);
                           IRExpr *rm = get_IR_roundingmode();
                           putDReg(fd, binop(Iop_RoundF64toInt, rm, getDReg(fs)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("rint.s f%u, f%u", fd, fs);
                           calculateFCSR(fs, 0, RINTD, True, 1);
                           IRExpr *rm = get_IR_roundingmode();
                           putFReg(fd,
                                   mkWidenFromF32(tyF,
                                                  binop(Iop_RoundF32toInt, rm,
                                                        getLoFromF64(tyF,
                                                              getFReg(fs)))));
                           break;
                        }

                        default:
                           return -1;
                     }

                  }

                  break;

               case 0x10: /* SEL.fmt */
                  switch (fmt) {
                     case 0x11: {  /* D */
                        if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                           DIP("sel.d f%u, f%u, f%u", fd, fs, ft);
                           t1 = newTemp(Ity_I1);

                           if (mode64) {
                              assign(t1, binop(Iop_CmpNE64,
                                               binop(Iop_And64,
                                                     unop(Iop_ReinterpF64asI64,
                                                          getDReg(fd)),
                                                     mkU64(1)),
                                               mkU64(0)));
                           } else {
                              assign(t1, binop(Iop_CmpNE32,
                                               binop(Iop_And32,
                                                     unop(Iop_64to32,
                                                          unop(Iop_ReinterpF64asI64,
                                                               getDReg(fd))),
                                                     mkU32(1)),
                                               mkU32(0)));
                           }

                           putDReg(fd, IRExpr_ITE(mkexpr(t1),
                                                  getDReg(ft), getDReg(fs)));
                           break;
                        } else {
                           ILLEGAL_INSTRUCTON;
                           break;
                        }

                     }

                     case 0x10: {  /* S */
                        DIP("sel.s f%u, f%u, f%u", fd, fs, ft);
                        t1 = newTemp(Ity_I1);
                        assign(t1, binop(Iop_CmpNE32,
                                         binop(Iop_And32,
                                               unop(Iop_ReinterpF32asI32,
                                                    getLoFromF64(tyF, getFReg(fd))),
                                               mkU32(1)),
                                         mkU32(0)));
                        putFReg(fd, IRExpr_ITE( mkexpr(t1),
                                                getFReg(ft), getFReg(fs)));
                        break;
                     }

                     default:
                        return -1;
                  }

                  break;

               case 0x14: /* SELEQZ.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) { /* SELEQZ.df */
                        case 0x11: {  /* D */
                           DIP("seleqz.d f%u, f%u, f%u", fd, fs, ft);
                           t1 = newTemp(Ity_I1);

                           if (mode64) {
                              assign(t1, binop(Iop_CmpNE64,
                                               binop(Iop_And64,
                                                     unop(Iop_ReinterpF64asI64,
                                                          getDReg(ft)),
                                                     mkU64(1)),
                                               mkU64(0)));
                           } else {
                              assign(t1, binop(Iop_CmpNE32,
                                               binop(Iop_And32,
                                                     unop(Iop_64to32,
                                                          unop(Iop_ReinterpF64asI64,
                                                               getDReg(ft))),
                                                     mkU32(1)),
                                               mkU32(0)));
                           }

                           putDReg(fd, IRExpr_ITE( mkexpr(t1),
                                                   binop(Iop_I64StoF64,
                                                         get_IR_roundingmode(), mkU64(0)),
                                                   getDReg(fs)));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("seleqz.s f%u, f%u, f%u", fd, fs, ft);
                           t1 = newTemp(Ity_I1);
                           assign(t1, binop(Iop_CmpNE32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(ft))),
                                                  mkU32(1)),
                                            mkU32(0)));
                           putFReg(fd, IRExpr_ITE(mkexpr(t1),
                                                  mkWidenFromF32(tyF,
                                                                 binop(Iop_I32StoF32,
                                                                       get_IR_roundingmode(),
                                                                       mkU32(0))),
                                                  getFReg(fs)));
                           break;
                        }

                        default:
                           return -1;
                     }
                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x17: /* SELNEZ.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("selnez.d f%u, f%u, f%u", fd, fs, ft);
                           t1 = newTemp(Ity_I1);

                           if (mode64) {
                              assign(t1, binop(Iop_CmpNE64,
                                               binop(Iop_And64,
                                                     unop(Iop_ReinterpF64asI64,
                                                          getDReg(ft)),
                                                     mkU64(1)),
                                               mkU64(0)));
                           } else {
                              assign(t1, binop(Iop_CmpNE32,
                                               binop(Iop_And32,
                                                     unop(Iop_64to32,
                                                          unop(Iop_ReinterpF64asI64,
                                                               getDReg(ft))),
                                                     mkU32(1)),
                                               mkU32(0)));
                           }

                           putDReg(fd, IRExpr_ITE( mkexpr(t1),
                                                   getDReg(fs),
                                                   binop(Iop_I64StoF64,
                                                         get_IR_roundingmode(),
                                                         mkU64(0))));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("selnez.s f%u, f%u, f%u", fd, fs, ft);
                           t1 = newTemp(Ity_I1);
                           assign(t1, binop(Iop_CmpNE32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(ft))),
                                                  mkU32(1)),
                                            mkU32(0)));
                           putFReg(fd, IRExpr_ITE(mkexpr(t1),
                                                  getFReg(fs),
                                                  mkWidenFromF32(tyF,
                                                                 binop(Iop_I32StoF32,
                                                                       get_IR_roundingmode(),
                                                                       mkU32(0)))));
                           break;
                        }

                        default:
                           return -1;

                     }

                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               case 0x1B: /* CLASS.fmt */
                  if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
                     t0 = newTemp(Ity_I1); // exp zero
                     t1 = newTemp(Ity_I1); // exp max
                     t2 = newTemp(Ity_I1); // sign
                     t3 = newTemp(Ity_I1); // first
                     t4 = newTemp(Ity_I1); // val not zero
                     t5 = newTemp(Ity_I32);

                     switch (fmt) {
                        case 0x11: {  /* D */
                           DIP("class.d f%u, f%u", fd, fs);
                           assign(t0, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_64HIto32,
                                                       unop(Iop_ReinterpF64asI64,
                                                            getDReg(fs))),
                                                  mkU32(0x7ff00000)),
                                            mkU32(0)));
                           assign(t1, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_64HIto32,
                                                       unop(Iop_ReinterpF64asI64,
                                                            getDReg(fs))),
                                                  mkU32(0x7ff00000)),
                                            mkU32(0x7ff00000)));
                           assign(t2, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_64HIto32,
                                                       unop(Iop_ReinterpF64asI64,
                                                            getDReg(fs))),
                                                  mkU32(0x80000000)),
                                            mkU32(0x80000000)));
                           assign(t3, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_64HIto32,
                                                       unop(Iop_ReinterpF64asI64,
                                                            getDReg(fs))),
                                                  mkU32(0x00080000)),
                                            mkU32(0x00080000)));

                           if (mode64) assign(t4, binop(Iop_CmpNE64,
                                                           binop(Iop_And64,
                                                                 unop(Iop_ReinterpF64asI64,
                                                                      getDReg(fs)),
                                                                 mkU64(0x000fffffffffffffULL)),
                                                           mkU64(0)));
                           else assign(t4, binop(Iop_CmpNE32,
                                                    binop(Iop_Or32,
                                                          binop(Iop_And32,
                                                                unop(Iop_64HIto32,
                                                                     unop(Iop_ReinterpF64asI64,
                                                                           getDReg(fs))),
                                                                mkU32(0x000fffff)),
                                                          unop(Iop_64to32,
                                                               unop(Iop_ReinterpF64asI64,
                                                                    getDReg(fs)))),
                                                    mkU32(0)));

                           assign(t5, binop(Iop_Shl32,
                                            IRExpr_ITE(mkexpr(t1),
                                                       IRExpr_ITE(mkexpr(t4),
                                                                  mkU32(0), mkU32(1)),
                                                       IRExpr_ITE(mkexpr(t0),
                                                                  IRExpr_ITE(mkexpr(t4),
                                                                        mkU32(0x4),
                                                                        mkU32(0x8)),
                                                                  mkU32(2))),
                                            IRExpr_ITE(mkexpr(t2), mkU8(2), mkU8(6))));
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            unop(Iop_32Uto64,
                                                 IRExpr_ITE(binop(Iop_CmpNE32,
                                                                  mkexpr(t5), mkU32(0)),
                                                            mkexpr(t5),
                                                            IRExpr_ITE(mkexpr(t3),
                                                                  mkU32(2),
                                                                  mkU32(1))))));
                           break;
                        }

                        case 0x10: {  /* S */
                           DIP("class.s f%u, f%u", fd, fs);
                           assign(t0, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(fs))),
                                                  mkU32(0x7f800000)),
                                            mkU32(0)));
                           assign(t1, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(fs))),
                                                  mkU32(0x7f800000)),
                                            mkU32(0x7f800000)));
                           assign(t2, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(fs))),
                                                  mkU32(0x80000000)),
                                            mkU32(0x80000000)));
                           assign(t3, binop(Iop_CmpEQ32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(fs))),
                                                  mkU32(0x00400000)),
                                            mkU32(0x00400000)));
                           assign(t4, binop(Iop_CmpNE32,
                                            binop(Iop_And32,
                                                  unop(Iop_ReinterpF32asI32,
                                                       getLoFromF64(tyF, getFReg(fs))),
                                                  mkU32(0x007fffff)),
                                            mkU32(0)));
                           assign(t5, binop(Iop_Shl32,
                                            IRExpr_ITE(mkexpr(t1),
                                                       IRExpr_ITE(mkexpr(t4),
                                                                  mkU32(0), mkU32(1)),
                                                       IRExpr_ITE(mkexpr(t0),
                                                                  IRExpr_ITE(mkexpr(t4),
                                                                        mkU32(0x4),
                                                                        mkU32(0x8)), //zero or subnorm
                                                                  mkU32(2))),
                                            IRExpr_ITE(mkexpr(t2), mkU8(2), mkU8(6))));
                           putDReg(fd, unop(Iop_ReinterpI64asF64,
                                            unop(Iop_32Uto64,
                                                 IRExpr_ITE(binop(Iop_CmpNE32,
                                                                  mkexpr(t5), mkU32(0)),
                                                            mkexpr(t5),
                                                            IRExpr_ITE(mkexpr(t3),
                                                                  mkU32(2),
                                                                  mkU32(1))))));
                           break;
                        }

                        default:
                           return -1;
                     }
                  } else {
                     ILLEGAL_INSTRUCTON;
                  }

                  break;

               default:
                  if (dis_instr_CCondFmt(cins))
                     break;

                  return -1;

            }

         }
      }
      break;  /* COP1 */

      case 0x03:  /* COP1X */
         switch (function) {
            case 0x0: {  /* LWXC1 */
               /* Load Word  Indexed to Floating Point - LWXC1 (MIPS32r2) */
               DIP("lwxc1 f%u, r%u(r%u)", fd, rt, rs);
               t2 = newTemp(ty);
               assign(t2, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                                getIReg(rt)));

               if (fp_mode64) {
                  t0 = newTemp(Ity_I64);
                  t1 = newTemp(Ity_I32);
                  t3 = newTemp(Ity_F32);
                  t4 = newTemp(Ity_I64);
                  assign(t3, load(Ity_F32, mkexpr(t2)));

                  assign(t4, mkWidenFrom32(Ity_I64, unop(Iop_ReinterpF32asI32,
                                                         mkexpr(t3)), True));

                  putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t4)));
               } else {
                  putFReg(fd, load(Ity_F32, mkexpr(t2)));
               }

               break;
            }

            case 0x1: {  /* LDXC1 */
               /* Load Doubleword  Indexed to Floating Point
                  LDXC1 (MIPS32r2 and MIPS64) */
               DIP("ldxc1 f%u, r%u(r%u)", fd, rt, rs);
               t0 = newTemp(ty);
               assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                                getIReg(rt)));
               putDReg(fd, load(Ity_F64, mkexpr(t0)));
               break;
            }

            case 0x5:  /* Load Doubleword Indexed Unaligned to Floating Point - LUXC1;
                     MIPS32r2 and MIPS64 */
               DIP("luxc1 f%u, r%u(r%u)", fd, rt, rs);

               if ((mode64 || VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps))
                     && fp_mode64) {
                  t0 = newTemp(ty);
                  t1 = newTemp(ty);
                  assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32,
                                   getIReg(rs), getIReg(rt)));
                  assign(t1, binop(mode64 ? Iop_And64 : Iop_And32,
                                   mkexpr(t0),
                                   mode64 ? mkU64(0xfffffffffffffff8ULL)
                                   : mkU32(0xfffffff8ULL)));
                  putFReg(fd, load(Ity_F64, mkexpr(t1)));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 0x8: {  /* Store Word Indexed from Floating Point - SWXC1 */
               DIP("swxc1 f%u, r%u(r%u)", ft, rt, rs);
               t0 = newTemp(ty);
               assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                                getIReg(rt)));

               if (fp_mode64) {
                  store(mkexpr(t0), getLoFromF64(tyF, getFReg(fs)));
               } else {
                  store(mkexpr(t0), getFReg(fs));
               }

               break;
            }

            case 0x9: {  /* Store Doubleword Indexed from Floating Point - SDXC1 */
               DIP("sdxc1 f%u, r%u(r%u)", fs, rt, rs);
               t0 = newTemp(ty);
               assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                                getIReg(rt)));
               store(mkexpr(t0), getDReg(fs));
               break;
            }

            case 0xD:  /* Store Doubleword Indexed Unaligned from Floating Point -
                     SUXC1; MIPS64 MIPS32r2 */
               DIP("suxc1 f%u, r%u(r%u)", fd, rt, rs);

               if ((mode64 || VEX_MIPS_CPU_HAS_MIPS32R2(archinfo->hwcaps))
                     && fp_mode64) {
                  t0 = newTemp(ty);
                  t1 = newTemp(ty);
                  assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32,
                                   getIReg(rs), getIReg(rt)));
                  assign(t1, binop(mode64 ? Iop_And64 : Iop_And32,
                                   mkexpr(t0),
                                   mode64 ? mkU64(0xfffffffffffffff8ULL)
                                   : mkU32(0xfffffff8ULL)));
                  store(mkexpr(t1), getFReg(fs));
               } else {
                  ILLEGAL_INSTRUCTON
               }

               break;

            case 0x0F: {
               DIP("prefx");
               break;
            }

            case 0x20:  {  /* MADD.S */
               DIP("madd.s f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               t1 = newTemp(Ity_F32);
               assign(t1, triop(Iop_AddF32, rm, getLoFromF64(tyF, getFReg(fmt)),
                                triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                                      getLoFromF64(tyF, getFReg(ft)))));
               putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
               break;  /* MADD.S */
            }

            case 0x21: {  /* MADD.D */
               DIP("madd.d f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               putDReg(fd, triop(Iop_AddF64, rm, getDReg(fmt),
                                 triop(Iop_MulF64, rm, getDReg(fs),
                                       getDReg(ft))));
               break;  /* MADD.D */
            }

            case 0x28: {  /* MSUB.S */
               DIP("msub.s f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               t1 = newTemp(Ity_F32);
               assign(t1, triop(Iop_SubF32, rm,
                                triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                                      getLoFromF64(tyF, getFReg(ft))),
                                getLoFromF64(tyF, getFReg(fmt))));
               putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
               break;  /* MSUB.S */
            }

            case 0x29: {  /* MSUB.D */
               DIP("msub.d f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               putDReg(fd, triop(Iop_SubF64, rm, triop(Iop_MulF64, rm, getDReg(fs),
                                                       getDReg(ft)), getDReg(fmt)));
               break;  /* MSUB.D */
            }

            case 0x30: {  /* NMADD.S */
               DIP("nmadd.s f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               t1 = newTemp(Ity_F32);
               assign(t1, triop(Iop_AddF32, rm, getLoFromF64(tyF, getFReg(fmt)),
                                triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                                      getLoFromF64(tyF, getFReg(ft)))));
               putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32, mkexpr(t1))));
               break;  /* NMADD.S */
            }

            case 0x31: {  /* NMADD.D */
               DIP("nmadd.d f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               t1 = newTemp(Ity_F64);
               assign(t1, triop(Iop_AddF64, rm, getDReg(fmt),
                                triop(Iop_MulF64, rm, getDReg(fs),
                                      getDReg(ft))));
               putDReg(fd, unop(Iop_NegF64, mkexpr(t1)));
               break;  /* NMADD.D */
            }

            case 0x38: {  /* NMSUBB.S */
               DIP("nmsub.s f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               t1 = newTemp(Ity_F32);
               assign(t1, triop(Iop_SubF32, rm,
                                triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                                      getLoFromF64(tyF, getFReg(ft))),
                                getLoFromF64(tyF, getFReg(fmt))));
               putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32, mkexpr(t1))));
               break;  /* NMSUBB.S */
            }

            case 0x39: {  /* NMSUBB.D */
               DIP("nmsub.d f%u, f%u, f%u, f%u", fd, fmt, fs, ft);
               IRExpr *rm = get_IR_roundingmode();
               t1 = newTemp(Ity_F64);
               assign(t1, triop(Iop_SubF64, rm, triop(Iop_MulF64, rm, getDReg(fs),
                                                      getDReg(ft)), getDReg(fmt)));
               putDReg(fd, unop(Iop_NegF64, mkexpr(t1)));
               break;  /* NMSUBB.D */
            }

            default:
               return -1;
         }

         break;

      case 0x04:  /* BEQL */
         DIP("beql r%u, r%u, %u", rs, rt, imm);
         *lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                                          getIReg(rs), getIReg(rt)), imm);
         break;

      case 0x05:  /* BNEL */
         DIP("bnel r%u, r%u, %u", rs, rt, imm);
         *lastn = dis_branch_likely(binop(mode64 ? Iop_CmpEQ64 : Iop_CmpEQ32,
                                          getIReg(rs), getIReg(rt)), imm);
         break;

      case 0x06:  /* 0x16 ??? BLEZL, BLEZC, BGEZC, BGEC */
         if (rt == 0) { /* BLEZL */
            DIP("blezl r%u, %u", rs, imm);
            *lastn = dis_branch_likely(unop(Iop_Not1, (binop(mode64 ? Iop_CmpLE64S :
                                            Iop_CmpLE32S, getIReg(rs), mode64 ?
                                            mkU64(0x0) : mkU32(0x0)))), imm);
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            if (rs == 0) { /* BLEZC */
               DIP("blezc r%u, %u", rt, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLE64S, getIReg(rt), mkU64(0x0)),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLE32S, getIReg(rt), mkU32(0x0)),
                                     imm, dres);
               }
            } else if (rt == rs) { /* BGEZC */
               DIP("bgezc r%u, %u", rt, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLE64S, mkU64(0x0), getIReg(rt)),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLE32S, mkU32(0x0), getIReg(rt)),
                                     imm, dres);
               }
            } else { /* BGEC */
               DIP("bgec r%u, r%u, %u", rs, rt, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLE64S, getIReg(rt), getIReg(rs)),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     binop(Iop_CmpLE32S, getIReg(rt), getIReg(rs)),
                                     imm, dres);
               }
            }
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x07:  /* BGTZL, BGTZC, BLTZC, BLTC */
         if (rt == 0) { /* BGTZL */
            DIP("bgtzl r%u, %u", rs, imm);

            if (mode64)
               *lastn = dis_branch_likely(binop(Iop_CmpLE64S, getIReg(rs),
                                                mkU64(0x00)), imm);
            else
               *lastn = dis_branch_likely(binop(Iop_CmpLE32S, getIReg(rs),
                                                mkU32(0x00)), imm);
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            if (rs == 0) { /* BGTZC */
               DIP("bgtzc r%u, %u", rt, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE64S,
                                                getIReg(rt), mkU64(0x0))),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE32S,
                                                getIReg(rt), mkU32(0x0))),
                                     imm, dres);
               }
            } else if (rs == rt) { /* BLTZC */
               DIP("bltzc r%u, %u", rt, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE64S,
                                                mkU64(0x0), getIReg(rt))),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE32S,
                                                mkU32(0x0), getIReg(rt))),
                                     imm, dres);
               }
            } else { /* BLTC */
               DIP("bltc r%u, r%u, %u", rs, rt, imm);

               if (mode64) {
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE64S,
                                                getIReg(rt), getIReg(rs))),
                                     imm, dres);
               } else {
                  dis_branch_compact(False,
                                     unop(Iop_Not1,
                                          binop(Iop_CmpLE32S,
                                                getIReg(rt), getIReg(rs))),
                                     imm, dres);
               }
            }
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

#if defined(__mips__) && ((defined(__mips_isa_rev) && __mips_isa_rev < 6))

      case 0x08: {  /* Doubleword Add Immidiate - DADDI; MIPS64 */
         DIP("daddi r%u, r%u, %u", rt, rs, imm);
         IRTemp tmpRs64 = newTemp(Ity_I64);
         assign(tmpRs64, getIReg(rs));

         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I64);
         /* dst = src0 + sign(imm)
            if(sign(src0 ) != sign(imm ))
            goto no overflow;
            if(sign(dst) == sign(src0 ))
            goto no overflow;
            we have overflow! */

         assign(t0, binop(Iop_Add64, mkexpr(tmpRs64),
                          mkU64(extend_s_16to64(imm))));
         assign(t1, binop(Iop_Xor64, mkexpr(tmpRs64),
                          mkU64(extend_s_16to64(imm))));
         assign(t2, unop(Iop_1Sto64, binop(Iop_CmpEQ64, binop(Iop_And64,
                                           mkexpr(t1), mkU64(0x8000000000000000ULL)),
                                           mkU64(0x8000000000000000ULL))));

         assign(t3, binop(Iop_Xor64, mkexpr(t0), mkexpr(tmpRs64)));
         assign(t4, unop(Iop_1Sto64, binop(Iop_CmpNE64, binop(Iop_And64,
                                           mkexpr(t3), mkU64(0x8000000000000000ULL)),
                                           mkU64(0x8000000000000000ULL))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ64, binop(Iop_Or64, mkexpr(t2),
                                mkexpr(t4)), mkU64(0)), Ijk_SigFPE_IntOvf,
                          IRConst_U64(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rt,  mkexpr(t0));
         break;
      }

#elif defined(__mips__) && ((defined(__mips_isa_rev) && __mips_isa_rev >= 6))

      case 0x08: { /* BNEZALC, BNEC, BNVC */
         if (rs == 0) { /* BNEZALC */
            DIP("bnezalc r%u, %u", rt, imm);

            if (mode64) {
               dis_branch_compact(True,
                                  unop(Iop_Not1,
                                       binop(Iop_CmpEQ64, getIReg(rt), mkU64(0x0))),
                                  imm, dres);
            } else {
               dis_branch_compact(True,
                                  unop(Iop_Not1,
                                       binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x0))),
                                  imm, dres);
            }
         } else if (rs < rt) { /* BNEC */
            DIP("bnec r%u, %u", rt, imm);

            if (mode64) {
               dis_branch_compact(False,
                                  unop(Iop_Not1,
                                       binop(Iop_CmpEQ64,
                                             getIReg(rt), getIReg(rs))),
                                  imm, dres);
            } else {
               dis_branch_compact(False,
                                  unop(Iop_Not1,
                                       binop(Iop_CmpEQ32,
                                             getIReg(rt), getIReg(rs))),
                                  imm, dres);
            }
         } else { /* BNVC */
            DIP("bnvc r%u, r%u, %u", rs, rt, imm);

            if (mode64) {
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               assign(t0, IRExpr_ITE(binop(Iop_CmpLT64S,
                                           getIReg(rt),
                                           mkU64(0xffffffff80000000ULL)),
                                     mkU32(1),
                                     IRExpr_ITE(binop(Iop_CmpLT64S,
                                                      getIReg(rt),
                                                      mkU64(0x7FFFFFFFULL)),
                                                mkU32(0), mkU32(1))));
               assign(t1, IRExpr_ITE(binop(Iop_CmpLT64S,
                                           getIReg(rs),
                                           mkU64(0xffffffff80000000ULL)),
                                     mkU32(1),
                                     IRExpr_ITE(binop(Iop_CmpLT64S,
                                                      getIReg(rs),
                                                      mkU64(0x7FFFFFFFULL)),
                                                mkU32(0), mkU32(1))));
               assign(t2, IRExpr_ITE(binop(Iop_CmpLT64S,
                                           binop(Iop_Add64,
                                                 getIReg(rt), getIReg(rs)),
                                           mkU64(0xffffffff80000000ULL)),
                                     mkU32(1),
                                     IRExpr_ITE(binop(Iop_CmpLT64S,
                                                      binop(Iop_Add64,
                                                            getIReg(rt),
                                                            getIReg(rs)),
                                                      mkU64(0x7FFFFFFFULL)),
                                                mkU32(0), mkU32(1))));
               assign(t3, binop(Iop_Add32,
                                mkexpr(t0),
                                binop(Iop_Add32, mkexpr(t1), mkexpr(t2))));
               dis_branch_compact(False,
                                  binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0)),
                                  imm, dres);
            } else {
               IRTemp tmpRs32 = newTemp(Ity_I32);
               IRTemp tmpRt32 = newTemp(Ity_I32);

               assign(tmpRs32, getIReg(rs));
               assign(tmpRt32, getIReg(rt));
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               t4 = newTemp(Ity_I32);
               /* dst = src0 + src1
                  if (sign(src0 ) != sign(src1 ))
                  goto no overflow;
                  if (sign(dst) == sign(src0 ))
                  goto no overflow;
                  we have overflow! */

               assign(t0, binop(Iop_Add32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
               assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
               assign(t2, unop(Iop_1Uto32,
                               binop(Iop_CmpEQ32,
                                     binop(Iop_And32, mkexpr(t1), mkU32(0x80000000)),
                                     mkU32(0x80000000))));

               assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
               assign(t4, unop(Iop_1Uto32,
                               binop(Iop_CmpNE32,
                                     binop(Iop_And32, mkexpr(t3), mkU32(0x80000000)),
                                     mkU32(0x80000000))));

               dis_branch_compact(False, binop(Iop_CmpNE32 ,
                                               binop(Iop_Or32, mkexpr(t2), mkexpr(t4)),
                                               mkU32(0)), imm, dres);
            }
         }

         break;
      }

#endif

      case 0x09:  /* Doubleword Add Immidiate Unsigned - DADDIU; MIPS64 */
         DIP("daddiu r%u, r%u, %u", rt, rs, imm);
         putIReg(rt, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
         break;

      case 0x0A: {  /* LDL */
         /* Load Doubleword Left - LDL; MIPS64 */
         vassert(mode64);
         DIP("ldl r%u, %u(r%u)", rt, imm, rs);
         /* t1 = addr */
#if defined (_MIPSEL)
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
#elif defined (_MIPSEB)
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Xor64, mkU64(0x7), binop(Iop_Add64, getIReg(rs),
                          mkU64(extend_s_16to64(imm)))));
#endif
         /* t2 = word addr */
         /* t4 = addr mod 8 */
         LWX_SWX_PATTERN64_1;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I64);
         assign(t3, binop(Iop_Shl64, load(Ity_I64, mkexpr(t2)),
                          narrowTo(Ity_I8, binop(Iop_Shl64, binop(Iop_Sub64, mkU64(0x07),
                                                 mkexpr(t4)), mkU8(3)))));

         /* rt content  - adjusted */
         t5 = newTemp(Ity_I64);
         t6 = newTemp(Ity_I64);
         t7 = newTemp(Ity_I64);

         assign(t5, binop(Iop_Mul64, mkexpr(t4), mkU64(0x8)));

         assign(t6, binop(Iop_Shr64, mkU64(0x00FFFFFFFFFFFFFFULL),
                          narrowTo(Ity_I8, mkexpr(t5))));

         assign(t7, binop(Iop_And64, getIReg(rt), mkexpr(t6)));

         putIReg(rt, binop(Iop_Or64, mkexpr(t7), mkexpr(t3)));
         break;
      }

      case 0x0B: {  /* LDR */
         /* Load Doubleword Right - LDR; MIPS64 */
         vassert(mode64);
         DIP("ldr r%u,%u(r%u)", rt, imm, rs);
         /* t1 = addr */
#if defined (_MIPSEL)
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
#elif defined (_MIPSEB)
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Xor64, mkU64(0x7), binop(Iop_Add64, getIReg(rs),
                          mkU64(extend_s_16to64(imm)))));
#endif
         /* t2 = word addr */
         /* t4 = addr mod 8 */
         LWX_SWX_PATTERN64_1;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I64);
         assign(t3, binop(Iop_Shr64, load(Ity_I64, mkexpr(t2)),
                          narrowTo(Ity_I8, binop(Iop_Shl64, mkexpr(t4), mkU8(3)))));

         /* rt content  - adjusted */
         t5 = newTemp(Ity_I64);
         assign(t5, binop(Iop_And64, getIReg(rt), unop(Iop_Not64,
                          binop(Iop_Shr64, mkU64(0xFFFFFFFFFFFFFFFFULL),
                                narrowTo(Ity_I8, binop(Iop_Shl64, mkexpr(t4), mkU8(0x3)))))));

         putIReg(rt, binop(Iop_Or64, mkexpr(t5), mkexpr(t3)));
         break;
      }

      case 0x0C:  /* Special2 */
         return disInstr_MIPS_WRK_Special2(cins, archinfo, abiinfo,
                                           dres, bstmt, lastn);

      case 0x0D: /* DAUI */
         if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            DIP("daui  r%u, r%u, %x", rt, rs, imm);
            putIReg(rt, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_32to64(imm << 16))));
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x0E: /* MIPS MSA (SIMD) */
         if (has_msa) {
            Int retVal = disMSAInstr_MIPS_WRK(cins);

            if (retVal == 0) {
               break;
            } else if (retVal == -2) {
               ILLEGAL_INSTRUCTON
               break;
            }
         }

         vex_printf("Error occured while trying to decode MIPS MSA "
                    "instruction.\nYour platform probably doesn't support "
                    "MIPS MSA (SIMD) ASE.\n");
         return -1;

      case 0x0F:  /* Special3 */
         return disInstr_MIPS_WRK_Special3(cins, archinfo, abiinfo,
                                           dres, bstmt, lastn);

      default:
         return -1;
   }

   return 0;
}

static UInt disInstr_MIPS_WRK_20(UInt cins)
{
   IRTemp t1 = 0, t2, t3, t4, t5;
   UInt opcode, rs, rt, imm;

   opcode = get_opcode(cins);
   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   switch (opcode & 0x0F) {
      case 0x00:  /* LB */
         DIP("lb r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         if (mode64)
            putIReg(rt, unop(Iop_8Sto64, load(Ity_I8, mkexpr(t1))));
         else
            putIReg(rt, unop(Iop_8Sto32, load(Ity_I8, mkexpr(t1))));

         break;

      case 0x01:  /* LH */
         DIP("lh r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         if (mode64)
            putIReg(rt, unop(Iop_16Sto64, load(Ity_I16, mkexpr(t1))));
         else
            putIReg(rt, unop(Iop_16Sto32, load(Ity_I16, mkexpr(t1))));

         break;

      case 0x02:  /* LWL */
         DIP("lwl r%u, %u(r%u)", rt, imm, rs);

         if (mode64) {
            /* t1 = addr */
            t1 = newTemp(Ity_I64);
#if defined (_MIPSEL)
            assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
#elif defined (_MIPSEB)
            assign(t1, binop(Iop_Xor64,
                             mkU64(0x03),
                             binop(Iop_Add64,
                                   getIReg(rs),
                                   mkU64(extend_s_16to64(imm)))));
#endif
            /* t2 = word addr */
            /* t4 = addr mod 4 */
            LWX_SWX_PATTERN64;

            /* t3 = word content - shifted */
            t3 = newTemp(Ity_I32);
            assign(t3, binop(Iop_Shl32,
                             load(Ity_I32, mkexpr(t2)),
                             narrowTo(Ity_I8,
                                      binop(Iop_Shl32,
                                            binop(Iop_Sub32,
                                                  mkU32(0x03),
                                                  mkexpr(t4)),
                                            mkU8(3)))));

            /* rt content - adjusted */
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_And32,
                             mkNarrowTo32(ty, getIReg(rt)),
                             binop(Iop_Shr32,
                                   mkU32(0x00FFFFFF),
                                   narrowTo(Ity_I8, binop(Iop_Mul32,
                                            mkU32(0x08),
                                            mkexpr(t4))))));

            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t5),
                                                mkexpr(t3)), True));
         } else {
            /* t1 = addr */
            t1 = newTemp(Ity_I32);
#if defined (_MIPSEL)
            assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));
#elif defined (_MIPSEB)
            assign(t1, binop(Iop_Xor32, mkU32(0x3), binop(Iop_Add32, getIReg(rs),
                             mkU32(extend_s_16to32(imm)))));
#endif

            /* t2 = word addr */
            /* t4 = addr mod 4 */
            LWX_SWX_PATTERN;

            /* t3 = word content - shifted */
            t3 = newTemp(Ity_I32);
            assign(t3, binop(Iop_Shl32, load(Ity_I32, mkexpr(t2)), narrowTo(Ity_I8,
                             binop(Iop_Shl32, binop(Iop_Sub32, mkU32(0x03), mkexpr(t4)),
                                   mkU8(3)))));

            /* rt content  - adjusted */
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_And32,
                             getIReg(rt),
                             binop(Iop_Shr32,
                                   mkU32(0x00FFFFFF),
                                   narrowTo(Ity_I8, binop(Iop_Mul32,
                                            mkU32(0x08),
                                            mkexpr(t4))))));

            putIReg(rt, binop(Iop_Or32, mkexpr(t5), mkexpr(t3)));
         }

         break;

      case 0x03:  /* LW */
         DIP("lw r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         putIReg(rt, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), True));
         break;

      case 0x04:  /* LBU */
         DIP("lbu r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         if (mode64)
            putIReg(rt, unop(Iop_8Uto64, load(Ity_I8, mkexpr(t1))));
         else
            putIReg(rt, unop(Iop_8Uto32, load(Ity_I8, mkexpr(t1))));

         break;

      case 0x05:  /* LHU */
         DIP("lhu r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         if (mode64)
            putIReg(rt, unop(Iop_16Uto64, load(Ity_I16, mkexpr(t1))));
         else
            putIReg(rt, unop(Iop_16Uto32, load(Ity_I16, mkexpr(t1))));

         break;

      case 0x06:  /* LWR */
         DIP("lwr r%u, %u(r%u)", rt, imm, rs);

         if (mode64) {
            /* t1 = addr */
            t1 = newTemp(Ity_I64);

#if defined (_MIPSEL)
            assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
#elif defined (_MIPSEB)
            assign(t1, binop(Iop_Xor64,
                             mkU64(0x3),
                             binop(Iop_Add64,
                                   getIReg(rs),
                                   mkU64(extend_s_16to64(imm)))));
#endif
            /* t2 = word addr */
            /* t4 = addr mod 4 */
            LWX_SWX_PATTERN64;

            /* t3 = word content - shifted */
            t3 = newTemp(Ity_I32);
            assign(t3, binop(Iop_Shr32,
                             load(Ity_I32, mkexpr(t2)),
                             narrowTo(Ity_I8,
                                      binop(Iop_Shl32, mkexpr(t4), mkU8(0x03)))));

            /* rt content  - adjusted */
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_And32, mkNarrowTo32(ty, getIReg(rt)),
                             unop(Iop_Not32, binop(Iop_Shr32, mkU32(0xFFFFFFFF),
                                                   narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4), mkU8(0x3)))))));

            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t5),
                                                mkexpr(t3)), True));

         } else {
            /* t1 = addr */
            t1 = newTemp(Ity_I32);
#if defined (_MIPSEL)
            assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));
#elif defined (_MIPSEB)
            assign(t1, binop(Iop_Xor32, mkU32(0x3), binop(Iop_Add32, getIReg(rs),
                             mkU32(extend_s_16to32(imm)))));
#endif

            /* t2 = word addr */
            /* t4 = addr mod 4 */
            LWX_SWX_PATTERN;

            /* t3 = word content - shifted */
            t3 = newTemp(Ity_I32);
            assign(t3, binop(Iop_Shr32, load(Ity_I32, mkexpr(t2)),
                             narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4),
                                                    mkU8(3)))));

            /* rt content  - adjusted */
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_And32, getIReg(rt), unop(Iop_Not32,
                             binop(Iop_Shr32, mkU32(0xFFFFFFFF), narrowTo(Ity_I8,
                                   binop(Iop_Shl32, mkexpr(t4), mkU8(0x3)))))));

            putIReg(rt, binop(Iop_Or32, mkexpr(t5), mkexpr(t3)));
         }

         break;

      case 0x07:  /* Load Word unsigned - LWU; MIPS64 */
         DIP("lwu r%u,%u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         putIReg(rt, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), False));
         break;

      case 0x08:  /* SB */
         DIP("sb r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         store(mkexpr(t1), narrowTo(Ity_I8, getIReg(rt)));
         break;

      case 0x09:  /* SH */
         DIP("sh r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         store(mkexpr(t1), narrowTo(Ity_I16, getIReg(rt)));
         break;

      case 0x0A:  /* SWL */
         DIP("swl r%u, %u(r%u)", rt, imm, rs);

         if (mode64) {
            IRTemp E_byte = newTemp(Ity_I8);
            IRTemp F_byte = newTemp(Ity_I8);
            IRTemp G_byte = newTemp(Ity_I8);
            IRTemp H_byte = newTemp(Ity_I8);
            IRTemp F_pos  = newTemp(Ity_I64);
            IRTemp G_pos  = newTemp(Ity_I64);

            /* H byte */
            assign(H_byte, getByteFromReg(rt, 0));
            /* G byte */
            assign(G_byte, getByteFromReg(rt, 1));
            /* F byte */
            assign(F_byte, getByteFromReg(rt, 2));
            /* E byte */
            assign(E_byte, getByteFromReg(rt, 3));

            /* t1 = addr */
            t1 = newTemp(Ity_I64);
            assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

            /* t2 = word addr */
            t2 = newTemp(Ity_I64);
            assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFFCULL)));

            /* t3 = addr mod 4 */
            t3 = newTemp(Ity_I64);
            assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x3)));

#if defined (_MIPSEL)
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                     mkU64(0x0),
                                     mkU64(0x1)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                     mkU64(0x1),
                                     mkU64(0x0)));

            /* Store X_byte on the right place. */
            store(mkexpr(t2), mkexpr(H_byte));
            store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
            store(binop(Iop_Sub64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
            store(mkexpr(t1), mkexpr(E_byte));

#else    /* _MIPSEB */
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                     mkU64(0x0),
                                     mkU64(0x1)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                     mkU64(0x2),
                                     mkU64(0x3)));

            store(binop(Iop_Add64, mkexpr(t2), mkU64(3)), mkexpr(H_byte));
            store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
            store(binop(Iop_Add64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
            store(mkexpr(t1), mkexpr(E_byte));

#endif
         } else {
            IRTemp E_byte = newTemp(Ity_I8);
            IRTemp F_byte = newTemp(Ity_I8);
            IRTemp G_byte = newTemp(Ity_I8);
            IRTemp H_byte = newTemp(Ity_I8);
            IRTemp F_pos  = newTemp(Ity_I32);
            IRTemp G_pos  = newTemp(Ity_I32);

            /* H byte */
            assign(H_byte, getByteFromReg(rt, 0));
            /* G byte */
            assign(G_byte, getByteFromReg(rt, 1));
            /* F byte */
            assign(F_byte, getByteFromReg(rt, 2));
            /* E byte */
            assign(E_byte, getByteFromReg(rt, 3));

            /* t1 = addr */
            t1 = newTemp(Ity_I32);
            assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));

            /* t2 = word addr */
            t2 = newTemp(Ity_I32);
            assign(t2, binop(Iop_And32, mkexpr(t1), mkU32(0xFFFFFFFCULL)));

            /* t3 = addr mod 4 */
            t3 = newTemp(Ity_I32);
            assign(t3, binop(Iop_And32, mkexpr(t1), mkU32(0x3)));

#if defined (_MIPSEL)
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                     mkU32(0x0),
                                     mkU32(0x1)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                     mkU32(0x1),
                                     mkU32(0x0)));

            /* Store X_byte on the right place. */
            store(mkexpr(t2), mkexpr(H_byte));
            store(binop(Iop_Add32, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
            store(binop(Iop_Sub32, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
            store(mkexpr(t1), mkexpr(E_byte));

#else    /* _MIPSEB */
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                     mkU32(0x0),
                                     mkU32(0x1)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                     mkU32(0x2),
                                     mkU32(0x3)));

            store(binop(Iop_Add32, mkexpr(t2), mkU32(3)), mkexpr(H_byte));
            store(binop(Iop_Add32, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
            store(binop(Iop_Add32, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
            store(mkexpr(t1), mkexpr(E_byte));

#endif
         }

         break;

      case 0x0B:  /* SW */
         DIP("sw r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         store(mkexpr(t1), mkNarrowTo32(ty, getIReg(rt)));
         break;

      case 0x0C: {  /* SDL rt, offset(base) MIPS64 */
         DIP("sdl r%u, %u(r%u)", rt, imm, rs);
         vassert(mode64);
         IRTemp A_byte = newTemp(Ity_I8);
         IRTemp B_byte = newTemp(Ity_I8);
         IRTemp C_byte = newTemp(Ity_I8);
         IRTemp D_byte = newTemp(Ity_I8);
         IRTemp E_byte = newTemp(Ity_I8);
         IRTemp F_byte = newTemp(Ity_I8);
         IRTemp G_byte = newTemp(Ity_I8);
         IRTemp H_byte = newTemp(Ity_I8);
         IRTemp B_pos  = newTemp(Ity_I64);
         IRTemp C_pos  = newTemp(Ity_I64);
         IRTemp D_pos  = newTemp(Ity_I64);
         IRTemp E_pos  = newTemp(Ity_I64);
         IRTemp F_pos  = newTemp(Ity_I64);
         IRTemp G_pos  = newTemp(Ity_I64);

         /* H byte */
         assign(H_byte, getByteFromReg(rt, 0));
         /* G byte */
         assign(G_byte, getByteFromReg(rt, 1));
         /* F byte */
         assign(F_byte, getByteFromReg(rt, 2));
         /* E byte */
         assign(E_byte, getByteFromReg(rt, 3));
         /* D byte */
         assign(D_byte, getByteFromReg(rt, 4));
         /* C byte */
         assign(C_byte, getByteFromReg(rt, 5));
         /* B byte */
         assign(B_byte, getByteFromReg(rt, 6));
         /* A byte */
         assign(A_byte, getByteFromReg(rt, 7));

         /* t1 = addr */
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

         /* t2 = word addr */
         t2 = newTemp(Ity_I64);
         assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFF8ULL)));

         /* t3 = addr mod 7 */
         t3 = newTemp(Ity_I64);
         assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x7)));

#if defined (_MIPSEL)
         /* Calculate X_byte position. */
         assign(B_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x1)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x2)),
                                  mkU64(0x0),
                                  mkU64(0x2)));

         assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x3)),
                                  mkU64(0x0),
                                  mkU64(0x3)));

         assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x4)),
                                  mkU64(0x0),
                                  mkU64(0x4)));

         assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x5)),
                                  mkU64(0x0),
                                  mkU64(0x5)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x7)),
                                  mkU64(0x1),
                                  mkU64(0x0)));

         /* Store X_byte on the right place. */
         store(mkexpr(t2), mkexpr(H_byte));
         store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
         store(mkexpr(t1), mkexpr(A_byte));

#else /* _MIPSEB */
         /* Calculate X_byte position. */
         assign(B_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x7)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x6)),
                                  mkU64(0x2),
                                  mkU64(0x0)));

         assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x5)),
                                  mkU64(0x3),
                                  mkU64(0x0)));

         assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x4)),
                                  mkU64(0x4),
                                  mkU64(0x0)));

         assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x3)),
                                  mkU64(0x5),
                                  mkU64(0x0)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                  mkU64(0x6),
                                  mkU64(0x7)));

         /* Store X_byte on the right place. */
         store(binop(Iop_Add64, mkexpr(t2), mkU64(0x7)), mkexpr(H_byte));
         store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
         store(mkexpr(t1), mkexpr(A_byte));
#endif

         break;
      }

      case 0x0D: {
         /* SDR rt, offset(base) - MIPS64 */
         vassert(mode64);
         DIP("sdr r%u, %u(r%u)", rt, imm, rs);
         IRTemp A_byte = newTemp(Ity_I8);
         IRTemp B_byte = newTemp(Ity_I8);
         IRTemp C_byte = newTemp(Ity_I8);
         IRTemp D_byte = newTemp(Ity_I8);
         IRTemp E_byte = newTemp(Ity_I8);
         IRTemp F_byte = newTemp(Ity_I8);
         IRTemp G_byte = newTemp(Ity_I8);
         IRTemp H_byte = newTemp(Ity_I8);
         IRTemp B_pos  = newTemp(Ity_I64);
         IRTemp C_pos  = newTemp(Ity_I64);
         IRTemp D_pos  = newTemp(Ity_I64);
         IRTemp E_pos  = newTemp(Ity_I64);
         IRTemp F_pos  = newTemp(Ity_I64);
         IRTemp G_pos  = newTemp(Ity_I64);

         /* H byte */
         assign(H_byte, getByteFromReg(rt, 0));
         /* G byte */
         assign(G_byte, getByteFromReg(rt, 1));
         /* F byte */
         assign(F_byte, getByteFromReg(rt, 2));
         /* E byte */
         assign(E_byte, getByteFromReg(rt, 3));
         /* D byte */
         assign(D_byte, getByteFromReg(rt, 4));
         /* C byte */
         assign(C_byte, getByteFromReg(rt, 5));
         /* B byte */
         assign(B_byte, getByteFromReg(rt, 6));
         /* A byte */
         assign(A_byte, getByteFromReg(rt, 7));

         /* t1 = addr */
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

         /* t2 = word addr */
         t2 = newTemp(Ity_I64);
         assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFF8ULL)));

         /* t3 = addr mod 7 */
         t3 = newTemp(Ity_I64);
         assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x7)));

#if defined (_MIPSEL)
         /* Calculate X_byte position. */
         assign(B_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x1), mkexpr(t3)),
                                  mkU64(0x0),
                                  mkU64(0x6)));

         assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x2), mkexpr(t3)),
                                  mkU64(0x0),
                                  mkU64(0x5)));

         assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x3), mkexpr(t3)),
                                  mkU64(0x0),
                                  mkU64(0x4)));

         assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x4), mkexpr(t3)),
                                  mkU64(0x0),
                                  mkU64(0x3)));

         assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x5), mkexpr(t3)),
                                  mkU64(0x0),
                                  mkU64(0x2)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x7)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         /* Store X_byte on the right place. */
         store(binop(Iop_Add64, mkexpr(t2), mkU64(0x7)), mkexpr(A_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
         store(mkexpr(t1), mkexpr(H_byte));

#else /* _MIPSEB */
         /* Calculate X_byte position. */
         assign(B_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x5), mkexpr(t3)),
                                  mkU64(0x6),
                                  mkU64(0x0)));

         assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x4), mkexpr(t3)),
                                  mkU64(0x5),
                                  mkU64(0x0)));

         assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x3), mkexpr(t3)),
                                  mkU64(0x4),
                                  mkU64(0x0)));

         assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x2), mkexpr(t3)),
                                  mkU64(0x3),
                                  mkU64(0x0)));

         assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x1), mkexpr(t3)),
                                  mkU64(0x2),
                                  mkU64(0x0)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         /* Store X_byte on the right place. */
         store(mkexpr(t2), mkexpr(A_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
         store(mkexpr(t1), mkexpr(H_byte));
#endif
         break;
      }

      case 0x0E:  /* SWR */
         DIP("swr r%u, %u(r%u)", rt, imm, rs);

         if (mode64) {
            IRTemp E_byte = newTemp(Ity_I8);
            IRTemp F_byte = newTemp(Ity_I8);
            IRTemp G_byte = newTemp(Ity_I8);
            IRTemp H_byte = newTemp(Ity_I8);
            IRTemp F_pos  = newTemp(Ity_I64);
            IRTemp G_pos  = newTemp(Ity_I64);

            /* H byte */
            assign(H_byte, getByteFromReg(rt, 0));
            /* G byte */
            assign(G_byte, getByteFromReg(rt, 1));
            /* F byte */
            assign(F_byte, getByteFromReg(rt, 2));
            /* E byte */
            assign(E_byte, getByteFromReg(rt, 3));

            /* t1 = addr */
            t1 = newTemp(Ity_I64);
            assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

            /* t2 = word addr */
            t2 = newTemp(Ity_I64);
            assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFFCULL)));

            /* t3 = addr mod 4 */
            t3 = newTemp(Ity_I64);
            assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x3)));

#if defined (_MIPSEL)
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                     mkU64(0x2),
                                     mkU64(0x3)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                     mkU64(0x0),
                                     mkU64(0x1)));

            /* Store X_byte on the right place. */
            store(binop(Iop_Add64, mkexpr(t2), mkU64(0x3)), mkexpr(E_byte));
            store(binop(Iop_Add64, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
            store(binop(Iop_Add64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
            store(mkexpr(t1), mkexpr(H_byte));

#else    /* _MIPSEB */
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                     mkU64(0x1),
                                     mkU64(0x0)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                     mkU64(0x0),
                                     mkU64(0x1)));

            /* Store X_byte on the right place. */
            store(mkexpr(t2), mkexpr(E_byte));
            store(binop(Iop_Add64, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
            store(binop(Iop_Sub64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
            store(mkexpr(t1), mkexpr(H_byte));
#endif
         } else {
            IRTemp E_byte = newTemp(Ity_I8);
            IRTemp F_byte = newTemp(Ity_I8);
            IRTemp G_byte = newTemp(Ity_I8);
            IRTemp H_byte = newTemp(Ity_I8);
            IRTemp F_pos  = newTemp(Ity_I32);
            IRTemp G_pos  = newTemp(Ity_I32);

            /* H byte */
            assign(H_byte, getByteFromReg(rt, 0));
            /* G byte */
            assign(G_byte, getByteFromReg(rt, 1));
            /* F byte */
            assign(F_byte, getByteFromReg(rt, 2));
            /* E byte */
            assign(E_byte, getByteFromReg(rt, 3));

            /* t1 = addr */
            t1 = newTemp(Ity_I32);
            assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));

            /* t2 = word addr */
            t2 = newTemp(Ity_I32);
            assign(t2, binop(Iop_And32, mkexpr(t1), mkU32(0xFFFFFFFCULL)));

            /* t3 = addr mod 4 */
            t3 = newTemp(Ity_I32);
            assign(t3, binop(Iop_And32, mkexpr(t1), mkU32(0x3)));

#if defined (_MIPSEL)
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                     mkU32(0x2),
                                     mkU32(0x3)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                     mkU32(0x0),
                                     mkU32(0x1)));

            /* Store X_byte on the right place. */
            store(binop(Iop_Add32, mkexpr(t2), mkU32(0x3)), mkexpr(E_byte));
            store(binop(Iop_Add32, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
            store(binop(Iop_Add32, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
            store(mkexpr(t1), mkexpr(H_byte));

#else    /* _MIPSEB */
            /* Calculate X_byte position. */
            assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                     mkU32(0x1),
                                     mkU32(0x0)));

            assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                     mkU32(0x0),
                                     mkU32(0x1)));

            /* Store X_byte on the right place. */
            store(mkexpr(t2), mkexpr(E_byte));
            store(binop(Iop_Add32, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
            store(binop(Iop_Sub32, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
            store(mkexpr(t1), mkexpr(H_byte));
#endif
         }

         break;
   }

   return 0;
}

static UInt disInstr_MIPS_WRK_30(UInt cins, const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo, DisResult* dres,
                                 IRStmt** bstmt)
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5;
   UInt opcode, rs, rt, rd, ft, function, imm, instr_index;

   opcode = get_opcode(cins);
   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   ft = get_ft(cins);

   instr_index = get_instr_index(cins);
   function = get_function(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   switch (opcode & 0x0F) {
      case 0x00:  /* LL */
         DIP("ll r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;

         if (abiinfo->guest__use_fallback_LLSC) {
            t2 = newTemp(ty);
            assign(t2, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), True));
            putLLaddr(mkexpr(t1));
            putLLdata(mkexpr(t2));
            putIReg(rt, mkexpr(t2));
         } else {
            t2 = newTemp(Ity_I32);
            stmt(IRStmt_LLSC(MIPS_IEND, t2, mkexpr(t1), NULL));
            putIReg(rt, mkWidenFrom32(ty, mkexpr(t2), True));
         }

         break;

      case 0x01:  /* LWC1 */
         /* Load Word to Floating Point - LWC1 (MIPS32) */
         DIP("lwc1 f%u, %u(r%u)", ft, imm, rs);
         LOAD_STORE_PATTERN;

         if (fp_mode64) {
            t0 = newTemp(Ity_F32);
            t2 = newTemp(Ity_I64);
            assign(t0, load(Ity_F32, mkexpr(t1)));
            assign(t2, mkWidenFrom32(Ity_I64, unop(Iop_ReinterpF32asI32,
                                                   mkexpr(t0)), True));
            putDReg(ft, unop(Iop_ReinterpI64asF64, mkexpr(t2)));
         } else {
            putFReg(ft, load(Ity_F32, mkexpr(t1)));
         }

         break;

      case 0x02:  /* Branch on Bit Clear - BBIT0; Cavium OCTEON */

         /* Cavium Specific instructions. */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            DIP("bbit0 r%u, 0x%x, %x", rs, rt, imm);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            assign(t0, mkU32(0x1));
            assign(t1, binop(Iop_Shl32, mkexpr(t0), mkU8(rt)));
            dis_branch(False, binop(Iop_CmpEQ32,
                                    binop(Iop_And32,
                                          mkexpr(t1),
                                          mkNarrowTo32(ty, getIReg(rs))),
                                    mkU32(0x0)),
                       imm, bstmt);
         } else if (archinfo->hwcaps & VEX_MIPS_CPU_ISA_M32R6) { /* BC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("bc  %x", instr_index & 0x3FFFFFF);

               if (mode64) {
                  t0 = newTemp(Ity_I64);
                  assign(t0, mkU64(guest_PC_curr_instr +
                                   ((extend_s_26to64(instr_index & 0x3FFFFFF) + 1 ) << 2)));
               } else {
                  t0 = newTemp(Ity_I32);
                  assign(t0, mkU32(guest_PC_curr_instr +
                                   ((extend_s_26to32(instr_index & 0x3FFFFFF) + 1) << 2)));
               }

               putPC(mkexpr(t0));
               dres->whatNext = Dis_StopHere;
               dres->jk_StopHere = Ijk_Boring;
            } else {
               ILLEGAL_INSTRUCTON;
               break;
            }
         } else {
            return -1;
         }

         break;

      case 0x03:  /* PREF */
         DIP("pref");
         break;

      case 0x04:  /* Load Linked Doubleword - LLD; MIPS64 */
         DIP("lld r%u, %u(r%u)", rt, imm, rs);

         if (mode64) {
            LOAD_STORE_PATTERN;
            t2 = newTemp(Ity_I64);

            if (abiinfo->guest__use_fallback_LLSC) {
               assign(t2, load(Ity_I64, mkexpr(t1)));
               putLLaddr(mkexpr(t1));
               putLLdata(mkexpr(t2));
            } else {
               stmt(IRStmt_LLSC(MIPS_IEND, t2, mkexpr(t1), NULL));
            }

            putIReg(rt, mkexpr(t2));
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x05:  /* Load Doubleword to Floating Point - LDC1 (MIPS32) */
         DIP("ldc1 f%u, %u(%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         putDReg(ft, load(Ity_F64, mkexpr(t1)));
         break;

      case 0x06:  /* Branch on Bit Clear Plus 32 - BBIT032; Cavium OCTEON */

         /* Cavium Specific instructions. */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            DIP("bbit032 r%u, 0x%x, %x", rs, rt, imm);
            t0 = newTemp(Ity_I64);
            t1 = newTemp(Ity_I8);  /* Shift. */
            t2 = newTemp(Ity_I64);
            assign(t0, mkU64(0x1));
            assign(t1, binop(Iop_Add8, mkU8(rt), mkU8(32)));
            assign(t2, binop(Iop_Shl64, mkexpr(t0), mkexpr(t1)));
            dis_branch(False, binop(Iop_CmpEQ64,
                                    binop(Iop_And64,
                                          mkexpr(t2),
                                          getIReg(rs)),
                                    mkU64(0x0)),
                       imm, bstmt);
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            if (rs == 0) { /* JIC */
               DIP("jic r%u, %u", rt, instr_index & 0xFFFF);

               if (mode64) {
                  t0 = newTemp(Ity_I64);
                  assign(t0, binop(Iop_Add64, getIReg(rt),
                                   mkU64(extend_s_16to64((instr_index & 0xFFFF)))));
               } else {
                  t0 = newTemp(Ity_I32);
                  assign(t0, binop(Iop_Add32, getIReg(rt),
                                   mkU32(extend_s_16to32((instr_index & 0xFFFF)))));
               }

               putPC(mkexpr(t0));
               dres->whatNext = Dis_StopHere;
               dres->jk_StopHere = Ijk_Boring;
            } else { /* BEQZC */
               DIP("beqzc r%u, %u", rs, imm);
               dres->jk_StopHere = Ijk_Boring;
               dres->whatNext = Dis_StopHere;
               ULong branch_offset;
               t0 = newTemp(Ity_I1);

               if (mode64) {
                  branch_offset = extend_s_23to64((instr_index & 0x1fffff) << 2);
                  assign(t0, binop(Iop_CmpEQ64, getIReg(rs), mkU64(0x0)));
                  stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                                   IRConst_U64(guest_PC_curr_instr + 4 + branch_offset),
                                   OFFB_PC));
                  putPC(mkU64(guest_PC_curr_instr + 4));
               } else {
                  branch_offset = extend_s_23to32((instr_index & 0x1fffff) << 2);
                  assign(t0, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0x0)));
                  stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                                   IRConst_U32(guest_PC_curr_instr + 4 +
                                               (UInt) branch_offset), OFFB_PC));
                  putPC(mkU32(guest_PC_curr_instr + 4));
               }
            }
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x07:  /* Load Doubleword - LD; MIPS64 */
         DIP("ld r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         putIReg(rt, load(Ity_I64, mkexpr(t1)));
         break;

      case 0x08:  /* SC */
         DIP("sc r%u, %u(r%u)", rt, imm, rs);
         t2 = newTemp(Ity_I1);
         LOAD_STORE_PATTERN;

         if (abiinfo->guest__use_fallback_LLSC) {
            t3 = newTemp(Ity_I32);
            assign(t2, binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                             mkexpr(t1), getLLaddr()));
            assign(t3, mkNarrowTo32(ty, getIReg(rt)));
            putLLaddr(LLADDR_INVALID);
            putIReg(rt, getIReg(0));

            mips_next_insn_if(mkexpr(t2));

            t4 = newTemp(Ity_I32);
            t5 = newTemp(Ity_I32);

            assign(t5, mkNarrowTo32(ty, getLLdata()));

            stmt(IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t4, /* old_mem */
                                    MIPS_IEND, mkexpr(t1),                 /* addr */
                                    NULL, mkexpr(t5),                      /* expected value */
                                    NULL, mkexpr(t3)                       /* new value */)));

            putIReg(rt, unop(mode64 ? Iop_1Uto64 : Iop_1Uto32,
                             binop(Iop_CmpEQ32, mkexpr(t4), mkexpr(t5))));
         } else {
            stmt(IRStmt_LLSC(MIPS_IEND, t2, mkexpr(t1),
                             mkNarrowTo32(ty, getIReg(rt))));
            putIReg(rt, unop(mode64 ? Iop_1Uto64 : Iop_1Uto32, mkexpr(t2)));
         }

         break;

      case 0x09:  /* SWC1 */
         DIP("swc1 f%u, %u(r%u)", ft, imm, rs);

         if (fp_mode64) {
            t0 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I32);
            LOAD_STORE_PATTERN;
            assign(t0, unop(Iop_ReinterpF64asI64, getFReg(ft)));
            assign(t2, unop(Iop_64to32, mkexpr(t0)));
            store(mkexpr(t1), unop(Iop_ReinterpI32asF32, mkexpr(t2)));
         } else {
            LOAD_STORE_PATTERN;
            store(mkexpr(t1), getFReg(ft));
         }

         break;

      case 0x0A:  /* Branch on Bit Set - BBIT1; Cavium OCTEON */

         /* Cavium Specific instructions. */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            DIP("bbit1 r%u, 0x%x, %x", rs, rt, imm);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            assign(t0, mkU32(0x1));
            assign(t1, binop(Iop_Shl32, mkexpr(t0), mkU8(rt)));
            dis_branch(False, binop(Iop_CmpNE32,
                                    binop(Iop_And32,
                                          mkexpr(t1),
                                          mkNarrowTo32(ty, getIReg(rs))),
                                    mkU32(0x0)),
                       imm, bstmt);
         } else if (archinfo->hwcaps & VEX_MIPS_CPU_ISA_M32R6) {/* BALC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("balc  %x", instr_index & 0x3FFFFFF);

               if (mode64) {
                  t0 = newTemp(Ity_I64);
                  assign(t0, mkU64(guest_PC_curr_instr + ((extend_s_26to64(
                        instr_index & 0x3FFFFFF) + 1) << 2)));
                  putIReg(31, mkU64(guest_PC_curr_instr + 4));
               } else {
                  t0 = newTemp(Ity_I32);
                  assign(t0, mkU32(guest_PC_curr_instr + ((extend_s_26to32(
                        instr_index & 0x3FFFFFF) + 1) << 2)));
                  putIReg(31, mkU32(guest_PC_curr_instr + 4));
               }

               putPC(mkexpr(t0));
               dres->whatNext = Dis_StopHere;
               dres->jk_StopHere = Ijk_Call;
            } else {
               ILLEGAL_INSTRUCTON;
               break;
            }
         } else {
            return -1;
         }

         break;

      case 0x0B:  /* PCREL */
         if (rt == 0x1E) { /* AUIPC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("auipc r%u, %u", rs, imm);

               if (mode64) {
                  putIReg(rs, mkU64(guest_PC_curr_instr + (imm << 16)));
               } else {
                  putIReg(rs, mkU32(guest_PC_curr_instr + (imm << 16)));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if (rt == 0x1F) { /* ALUIPC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("aluipc r%u, %u", rs, imm);

               if (mode64) {
                  putIReg(rs, mkU64((~0x0FFFFULL) &
                                    (guest_PC_curr_instr + extend_s_32to64(imm << 16))));
               } else {
                  putIReg(rs, mkU32((~0x0FFFFULL) &
                                    (guest_PC_curr_instr + (imm << 16))));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if ((rt & 0x18) == 0) { /* ADDIUPC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("addiupc r%u, %u", rs, instr_index & 0x7FFFF);

               if (mode64) {
                  putIReg(rs, mkU64(guest_PC_curr_instr +
                                    (extend_s_19to64(instr_index & 0x7FFFF) << 2)));
               } else {
                  putIReg(rs, mkU32(guest_PC_curr_instr +
                                    (extend_s_19to32(instr_index & 0x7FFFF) << 2)));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if ((rt & 0x18) == 8) { /* LWPC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("lwpc r%u, %x", rs, instr_index & 0x7FFFF);

               if (mode64) {
                  t1 = newTemp(Ity_I64);
                  assign(t1, mkU64(guest_PC_curr_instr +
                                   (extend_s_19to64(instr_index & 0x7FFFF) << 2)));
                  putIReg(rs, unop(Iop_32Sto64, load(Ity_I32, mkexpr(t1))));
               } else {
                  t1 = newTemp(Ity_I32);
                  assign(t1, mkU32(guest_PC_curr_instr +
                                   (extend_s_19to32(instr_index & 0x7FFFF) << 2)));
                  putIReg(rs, load(Ity_I32, mkexpr(t1)));
               }
            } else {
               ILLEGAL_INSTRUCTON;
            }

            break;
         } else if ((rt & 0x18) == 16) { /* LWUPC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("lwupc r%u, %x", rs, instr_index & 0x7FFFF);

               if (mode64) {
                  t1 = newTemp(Ity_I64);
                  assign(t1, mkU64(guest_PC_curr_instr +
                                   (extend_s_19to64(instr_index & 0x7FFFF) << 2)));
                  putIReg(rs, unop(Iop_32Uto64, load(Ity_I32, mkexpr(t1))));
               } else {
                  t1 = newTemp(Ity_I32);
                  assign(t1, mkU32(guest_PC_curr_instr +
                                   (extend_s_19to32(instr_index & 0x7FFFF) << 2)));
                  putIReg(rs, load(Ity_I32, mkexpr(t1)));
               }
            } else {
               ILLEGAL_INSTRUCTON
            }

            break;
         } else if ((rt & 0x1C) == 0x18) { /* LDPC */
            if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
               DIP("ldpc r%u, %x", rs, instr_index & 0x3FFFF);
               t1 = newTemp(Ity_I64);
               assign(t1, mkU64(guest_PC_curr_instr +
                                (extend_s_18to64(instr_index & 0x3FFFF) << 3)));
               putIReg(rs, load(Ity_I64, mkexpr(t1)));
            } else {
               ILLEGAL_INSTRUCTON
            }

            break;
         } else {
            return -1;
         }

         if (0x3B == function &&
               (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_BROADCOM)) {
            /*RDHWR*/
            DIP("rdhwr r%u, r%u", rt, rd);

            if (rd == 29) {
               putIReg(rt, getULR());
            } else
               return -1;

            break;
         } else {
            return -1;
         }

      case 0x0C:  /* Store Conditional Doubleword - SCD; MIPS64 */
         DIP("scd r%u, %u(r%u)", rt, imm, rs);

         if (mode64) {
            t2 = newTemp(Ity_I1);
            LOAD_STORE_PATTERN;

            if (abiinfo->guest__use_fallback_LLSC) {
               t3 = newTemp(Ity_I64);
               assign(t2, binop(Iop_CmpNE64, mkexpr(t1), getLLaddr()));
               assign(t3, getIReg(rt));
               putLLaddr(LLADDR_INVALID);
               putIReg(rt, getIReg(0));

               mips_next_insn_if(mkexpr(t2));

               t4 = newTemp(Ity_I64);
               t5 = newTemp(Ity_I64);

               assign(t5, getLLdata());

               stmt(IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t4, /* old_mem */
                                       MIPS_IEND, mkexpr(t1),                 /* addr */
                                       NULL, mkexpr(t5),                      /* expected value */
                                       NULL, mkexpr(t3)                       /* new value */)));

               putIReg(rt, unop(Iop_1Uto64,
                                binop(Iop_CmpEQ64, mkexpr(t4), mkexpr(t5))));
            } else {
               stmt(IRStmt_LLSC(MIPS_IEND, t2, mkexpr(t1), getIReg(rt)));
               putIReg(rt, unop(Iop_1Uto64, mkexpr(t2)));
            }
         } else {
            ILLEGAL_INSTRUCTON
         }

         break;

      case 0x0D:  /* Store Doubleword from Floating Point - SDC1 */
         DIP("sdc1 f%u, %u(%u)", ft, imm, rs);
         LOAD_STORE_PATTERN;
         store(mkexpr(t1), getDReg(ft));
         break;

      case 0x0E:  /* Branch on Bit Set Plus 32 - BBIT132; Cavium OCTEON */

         /* Cavium Specific instructions. */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            DIP("bbit132 r%u, 0x%x, %x", rs, rt, imm);
            t0 = newTemp(Ity_I64);
            t1 = newTemp(Ity_I8);  /* Shift. */
            t2 = newTemp(Ity_I64);
            assign(t0, mkU64(0x1));
            assign(t1, binop(Iop_Add8, mkU8(rt), mkU8(32)));
            assign(t2, binop(Iop_Shl64, mkexpr(t0), mkexpr(t1)));
            dis_branch(False, binop(Iop_CmpNE64,
                                    binop(Iop_And64,
                                          mkexpr(t2),
                                          getIReg(rs)),
                                    mkU64(0x0)),
                       imm, bstmt);
         } else if (VEX_MIPS_CPU_HAS_MIPSR6(archinfo->hwcaps)) {
            if (rs == 0) {/* JIALC */
               DIP("jialc r%u, %u", rt, instr_index & 0xFFFF);

               if (rs) return -1;

               if (mode64) {
                  t0 = newTemp(Ity_I64);
                  assign(t0, binop(Iop_Add64, getIReg(rt),
                                   mkU64(extend_s_16to64((instr_index & 0xFFFF)))));
                  putIReg(31, mkU64(guest_PC_curr_instr + 4));
               } else {
                  t0 = newTemp(Ity_I32);
                  assign(t0, binop(Iop_Add32, getIReg(rt),
                                   mkU32(extend_s_16to32((instr_index & 0xFFFF)))));
                  putIReg(31, mkU32(guest_PC_curr_instr + 4));
               }

               putPC(mkexpr(t0));
               dres->whatNext = Dis_StopHere;
               dres->jk_StopHere = Ijk_Call;
            } else { /* BNEZC */
               DIP("bnezc r%u, %u", rs, imm);
               dres->jk_StopHere = Ijk_Boring;
               dres->whatNext = Dis_StopHere;
               ULong branch_offset;
               t0 = newTemp(Ity_I1);

               if (mode64) {
                  branch_offset = extend_s_23to64((instr_index & 0x1fffff) << 2);
                  assign(t0, unop(Iop_Not1, binop(Iop_CmpEQ64, getIReg(rs), mkU64(0x0))));
                  stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                                   IRConst_U64(guest_PC_curr_instr + 4 + branch_offset),
                                   OFFB_PC));
                  putPC(mkU64(guest_PC_curr_instr + 4));
               } else {
                  branch_offset = extend_s_23to32((instr_index & 0x1fffff) << 2);
                  assign(t0, unop(Iop_Not1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0x0))));
                  stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                                   IRConst_U32(guest_PC_curr_instr + 4 +
                                               (UInt) branch_offset), OFFB_PC));
                  putPC(mkU32(guest_PC_curr_instr + 4));
               }
            }
         } else {
            return -1;
         }

         break;

      case 0x0F:  /* Store Doubleword - SD; MIPS64 */
         DIP("sd r%u, %u(r%u)", rt, imm, rs);
         LOAD_STORE_PATTERN;
         store(mkexpr(t1), getIReg(rt));
         break;

      default:
         return -1;
   }

   return 0;
}

static DisResult disInstr_MIPS_WRK ( Long         delta64,
                                     const VexArchInfo* archinfo,
                                     const VexAbiInfo*  abiinfo,
                                     Bool         sigill_diag )
{

   UInt opcode, cins, result;

   DisResult dres;

   static IRExpr *lastn = NULL;  /* last jump addr */
   static IRStmt *bstmt = NULL;  /* branch (Exit) stmt */

   /* The running delta */
   Int delta = (Int) delta64;

   /* Holds eip at the start of the insn, so that we can print
      consistent error messages for unimplemented insns. */
   Int delta_start = delta;

   /* Are we in a delay slot ? */
   Bool delay_slot_branch, likely_delay_slot, delay_slot_jump;

   /* Set result defaults. */
   dres.whatNext = Dis_Continue;
   dres.len = 0;
   dres.jk_StopHere = Ijk_INVALID;
   dres.hint        = Dis_HintNone;

   delay_slot_branch = likely_delay_slot = delay_slot_jump = False;

   const UChar *code = guest_code + delta;
   cins = getUInt(code);
   opcode = get_opcode(cins);
   DIP("\t0x%llx:\t0x%08x\t", (Addr64)guest_PC_curr_instr, cins);

   if (delta != 0) {
      if (branch_or_jump(guest_code + delta - 4)) {
         if (lastn == NULL && bstmt == NULL) {
            vassert(0);
         } else {
            dres.whatNext = Dis_StopHere;

            if (lastn != NULL) {
               delay_slot_jump = True;
            } else if (bstmt != NULL) {
               delay_slot_branch = True;
            }
         }
      }

      if (branch_or_link_likely(guest_code + delta - 4)) {
         likely_delay_slot = True;
      }
   }

   /* Spot "Special" instructions (see comment at top of file). */
   {
      /* Spot the 16-byte preamble:
       ****mips32****
       "srl $0, $0, 13
       "srl $0, $0, 29
       "srl $0, $0, 3
       "srl $0, $0, 19

       ****mips64****
       dsll $0, $0, 3
       dsll $0, $0, 13
       dsll $0, $0, 29
       dsll $0, $0, 19 */

      UInt word1 = mode64 ? 0xF8  : 0x342;
      UInt word2 = mode64 ? 0x378 : 0x742;
      UInt word3 = mode64 ? 0x778 : 0xC2;
      UInt word4 = mode64 ? 0x4F8 : 0x4C2;

      if (getUInt(code + 0) == word1 && getUInt(code + 4) == word2 &&
            getUInt(code + 8) == word3 && getUInt(code + 12) == word4) {
         /* Got a "Special" instruction preamble. Which one is it? */
         if (getUInt(code + 16) == 0x01ad6825 /* or $13, $13, $13 */ ) {
            /* $11 = client_request ( $12 ) */
            DIP("$11 = client_request ( $12 )");

            if (mode64)
               putPC(mkU64(guest_PC_curr_instr + 20));
            else
               putPC(mkU32(guest_PC_curr_instr + 20));

            dres.jk_StopHere = Ijk_ClientReq;
            dres.whatNext    = Dis_StopHere;

            goto decode_success;
         } else if (getUInt(code + 16) == 0x01ce7025 /* or $14, $14, $14 */ ) {
            /* $11 = guest_NRADDR */
            DIP("$11 = guest_NRADDR");
            dres.len = 20;
            delta += 20;

            if (mode64)
               putIReg(11, IRExpr_Get(offsetof(VexGuestMIPS64State,
                                               guest_NRADDR), Ity_I64));
            else
               putIReg(11, IRExpr_Get(offsetof(VexGuestMIPS32State,
                                               guest_NRADDR), Ity_I32));

            goto decode_success;
         } else if (getUInt(code + 16) == 0x01ef7825 /* or $15, $15, $15 */ ) {
            /*  branch-and-link-to-noredir $25 */
            DIP("branch-and-link-to-noredir $25");

            if (mode64)
               putIReg(31, mkU64(guest_PC_curr_instr + 20));
            else
               putIReg(31, mkU32(guest_PC_curr_instr + 20));

            putPC(getIReg(25));
            dres.jk_StopHere = Ijk_NoRedir;
            dres.whatNext    = Dis_StopHere;
            goto decode_success;
         } else if (getUInt(code + 16) == 0x016b5825 /* or $11,$11,$11 */ ) {
            /* IR injection */
            DIP("IR injection");
#if defined (_MIPSEL)
            vex_inject_ir(irsb, Iend_LE);
#elif defined (_MIPSEB)
            vex_inject_ir(irsb, Iend_BE);
#endif

            if (mode64) {
               stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_CMSTART),
                               mkU64(guest_PC_curr_instr)));
               stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_CMLEN),
                               mkU64(20)));

               putPC(mkU64(guest_PC_curr_instr + 20));
            } else {
               stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_CMSTART),
                               mkU32(guest_PC_curr_instr)));
               stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_CMLEN),
                               mkU32(20)));

               putPC(mkU32(guest_PC_curr_instr + 20));
            }

            dres.whatNext    = Dis_StopHere;
            dres.jk_StopHere = Ijk_InvalICache;
            dres.len = 20;
            delta += 20;
            goto decode_success;
         }

         /* We don't know what it is.  Set opc1/opc2 so decode_failure
            can print the insn following the Special-insn preamble. */
         delta += 16;
         goto decode_failure;
         /*NOTREACHED*/
      }
   }

   switch (opcode & 0x30) {
      case 0x00:
         result = disInstr_MIPS_WRK_00(cins, archinfo, abiinfo,
                                       &dres, &bstmt, &lastn);

         if (result == -1) goto decode_failure;

         if (result == -2) goto decode_failure_dsp;

         break;

      case 0x10:

         result = disInstr_MIPS_WRK_10(cins, archinfo, abiinfo,
                                       &dres, &bstmt, &lastn);

         if (result == -1) goto decode_failure;

         if (result == -2) goto decode_failure_dsp;

         break;

      case 0x20:
         result = disInstr_MIPS_WRK_20(cins);

         if (result == -1) goto decode_failure;

         if (result == -2) goto decode_failure_dsp;

         break;

      case 0x30:
         result = disInstr_MIPS_WRK_30(cins, archinfo, abiinfo, &dres, &bstmt);

         if (result == -1) goto decode_failure;

         if (result == -2) goto decode_failure_dsp;

         break;

decode_failure_dsp:
         vex_printf("Error occured while trying to decode MIPS32 DSP "
                    "instruction.\nYour platform probably doesn't support "
                    "MIPS32 DSP ASE.\n");
decode_failure:

         /* All decode failures end up here. */
         if (sigill_diag)
            vex_printf("vex mips->IR: unhandled instruction bytes: "
                       "0x%x 0x%x 0x%x 0x%x\n",
                       (UInt) getIByte(delta_start + 0),
                       (UInt) getIByte(delta_start + 1),
                       (UInt) getIByte(delta_start + 2),
                       (UInt) getIByte(delta_start + 3));

         /* Tell the dispatcher that this insn cannot be decoded, and so has
            not been executed, and (is currently) the next to be executed.
            EIP should be up-to-date since it made so at the start bnezof each
            insn, but nevertheless be paranoid and update it again right
            now. */
         if (mode64) {
            stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_PC),
                            mkU64(guest_PC_curr_instr)));
            jmp_lit64(&dres, Ijk_NoDecode, guest_PC_curr_instr);
         } else {
            stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_PC),
                            mkU32(guest_PC_curr_instr)));
            jmp_lit32(&dres, Ijk_NoDecode, guest_PC_curr_instr);
         }

         dres.whatNext = Dis_StopHere;
         dres.len = 0;
         return dres;
   }  /* switch (opc) for the main (primary) opcode switch. */

   /* All MIPS insn have 4 bytes */

   if (delay_slot_branch) {
      delay_slot_branch = False;
      stmt(bstmt);
      bstmt = NULL;

      if (mode64)
         putPC(mkU64(guest_PC_curr_instr + 4));
      else
         putPC(mkU32(guest_PC_curr_instr + 4));

      dres.jk_StopHere = is_Branch_or_Jump_and_Link(guest_code + delta - 4) ?
                         Ijk_Call : Ijk_Boring;
   }

   if (likely_delay_slot) {
      dres.jk_StopHere = Ijk_Boring;
      dres.whatNext = Dis_StopHere;
      putPC(lastn);
      lastn = NULL;
   }

   if (delay_slot_jump) {
      putPC(lastn);
      lastn = NULL;
      dres.jk_StopHere = is_Branch_or_Jump_and_Link(guest_code + delta - 4) ?
                         Ijk_Call : Ijk_Boring;
   }

decode_success:

   /* All decode successes end up here. */
   switch (dres.whatNext) {
      case Dis_Continue:
         if (mode64)
            putPC(mkU64(guest_PC_curr_instr + 4));
         else
            putPC(mkU32(guest_PC_curr_instr + 4));

         break;

      case Dis_StopHere:
         break;

      default:
         vassert(0);
         break;
   }

   /* On MIPS we need to check if the last instruction in block is branch or
      jump. */
   if (((vex_control.guest_max_insns - 1) == (delta + 4) / 4)
         &&  (dres.whatNext != Dis_StopHere))
      if (branch_or_jump(guest_code + delta + 4)) {
         dres.whatNext = Dis_StopHere;
         dres.jk_StopHere = Ijk_Boring;
         if (mode64)
            putPC(mkU64(guest_PC_curr_instr + 4));
         else
            putPC(mkU32(guest_PC_curr_instr + 4));
      }
   dres.len = 4;

   DIP("\n");

   return dres;

}

/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */
DisResult disInstr_MIPS( IRSB*        irsb_IN,
                         const UChar* guest_code_IN,
                         Long         delta,
                         Addr         guest_IP,
                         VexArch      guest_arch,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo*  abiinfo,
                         VexEndness   host_endness_IN,
                         Bool         sigill_diag_IN )
{
   DisResult dres;
   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchMIPS32 || guest_arch == VexArchMIPS64);

   mode64 = guest_arch != VexArchMIPS32;
   fp_mode64 = abiinfo->guest_mips_fp_mode & 1;
   fp_mode64_fre = abiinfo->guest_mips_fp_mode & 2;
   has_msa = VEX_MIPS_PROC_MSA(archinfo->hwcaps);

   vassert(VEX_MIPS_HOST_FP_MODE(archinfo->hwcaps) >= fp_mode64);

   guest_code = guest_code_IN;
   irsb = irsb_IN;
   host_endness = host_endness_IN;
#if defined(VGP_mips32_linux)
   guest_PC_curr_instr = (Addr32)guest_IP;
#elif defined(VGP_mips64_linux)
   guest_PC_curr_instr = (Addr64)guest_IP;
#endif

   dres = disInstr_MIPS_WRK(delta, archinfo, abiinfo, sigill_diag_IN);

   return dres;
}

/*--------------------------------------------------------------------*/
/*--- end                                        guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/
