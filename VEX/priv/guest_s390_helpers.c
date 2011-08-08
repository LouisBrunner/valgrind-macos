/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                              guest_s390_helpers.c ---*/
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
#include "libvex_emwarn.h"
#include "libvex_guest_s390x.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_s390x_common.h"

#include "main_util.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_s390_defs.h"

void
LibVEX_GuestS390X_initialise(VexGuestS390XState *state)
{
/*------------------------------------------------------------*/
/*--- Initialise ar registers                              ---*/
/*------------------------------------------------------------*/

   state->guest_a0 = 0;
   state->guest_a1 = 0;
   state->guest_a2 = 0;
   state->guest_a3 = 0;
   state->guest_a4 = 0;
   state->guest_a5 = 0;
   state->guest_a6 = 0;
   state->guest_a7 = 0;
   state->guest_a8 = 0;
   state->guest_a9 = 0;
   state->guest_a10 = 0;
   state->guest_a11 = 0;
   state->guest_a12 = 0;
   state->guest_a13 = 0;
   state->guest_a14 = 0;
   state->guest_a15 = 0;

/*------------------------------------------------------------*/
/*--- Initialise fpr registers                             ---*/
/*------------------------------------------------------------*/

   state->guest_f0 = 0;
   state->guest_f1 = 0;
   state->guest_f2 = 0;
   state->guest_f3 = 0;
   state->guest_f4 = 0;
   state->guest_f5 = 0;
   state->guest_f6 = 0;
   state->guest_f7 = 0;
   state->guest_f8 = 0;
   state->guest_f9 = 0;
   state->guest_f10 = 0;
   state->guest_f11 = 0;
   state->guest_f12 = 0;
   state->guest_f13 = 0;
   state->guest_f14 = 0;
   state->guest_f15 = 0;

/*------------------------------------------------------------*/
/*--- Initialise gpr registers                             ---*/
/*------------------------------------------------------------*/

   state->guest_r0 = 0;
   state->guest_r1 = 0;
   state->guest_r2 = 0;
   state->guest_r3 = 0;
   state->guest_r4 = 0;
   state->guest_r5 = 0;
   state->guest_r6 = 0;
   state->guest_r7 = 0;
   state->guest_r8 = 0;
   state->guest_r9 = 0;
   state->guest_r10 = 0;
   state->guest_r11 = 0;
   state->guest_r12 = 0;
   state->guest_r13 = 0;
   state->guest_r14 = 0;
   state->guest_r15 = 0;

/*------------------------------------------------------------*/
/*--- Initialise S390 miscellaneous registers              ---*/
/*------------------------------------------------------------*/

   state->guest_counter = 0;
   state->guest_fpc = 0;
   state->guest_IA = 0;

/*------------------------------------------------------------*/
/*--- Initialise S390 pseudo registers                     ---*/
/*------------------------------------------------------------*/

   state->guest_SYSNO = 0;

/*------------------------------------------------------------*/
/*--- Initialise generic pseudo registers                  ---*/
/*------------------------------------------------------------*/

   state->guest_NRADDR = 0;
   state->guest_TISTART = 0;
   state->guest_TILEN = 0;
   state->guest_IP_AT_SYSCALL = 0;
   state->guest_EMWARN = EmWarn_NONE;

/*------------------------------------------------------------*/
/*--- Initialise thunk                                     ---*/
/*------------------------------------------------------------*/

   state->guest_CC_OP = 0;
   state->guest_CC_DEP1 = 0;
   state->guest_CC_DEP2 = 0;
   state->guest_CC_NDEP = 0;
}


/* Figure out if any part of the guest state contained in minoff
   .. maxoff requires precise memory exceptions.  If in doubt return
   True (but this is generates significantly slower code).  */
Bool
guest_s390x_state_requires_precise_mem_exns(Int minoff, Int maxoff)
{
   Int lr_min = S390X_GUEST_OFFSET(guest_LR);
   Int lr_max = lr_min + 8 - 1;
   Int sp_min = S390X_GUEST_OFFSET(guest_SP);
   Int sp_max = sp_min + 8 - 1;
   Int fp_min = S390X_GUEST_OFFSET(guest_FP);
   Int fp_max = fp_min + 8 - 1;
   Int ia_min = S390X_GUEST_OFFSET(guest_IA);
   Int ia_max = ia_min + 8 - 1;

   if (maxoff < lr_min || minoff > lr_max) {
      /* No overlap with LR */
   } else {
      return True;
   }

   if (maxoff < sp_min || minoff > sp_max) {
      /* No overlap with SP */
   } else {
      return True;
   }

   if (maxoff < fp_min || minoff > fp_max) {
      /* No overlap with FP */
   } else {
      return True;
   }

   if (maxoff < ia_min || minoff > ia_max) {
      /* No overlap with IA */
   } else {
      return True;
   }

   return False;
}


#define ALWAYSDEFD(field)                             \
    { S390X_GUEST_OFFSET(field),            \
      (sizeof ((VexGuestS390XState*)0)->field) }

VexGuestLayout s390xGuest_layout = {

   /* Total size of the guest state, in bytes. */
   .total_sizeB = sizeof(VexGuestS390XState),

   /* Describe the stack pointer. */
   .offset_SP = S390X_GUEST_OFFSET(guest_SP),
   .sizeof_SP = 8,

   /* Describe the frame pointer. */
   .offset_FP = S390X_GUEST_OFFSET(guest_FP),
   .sizeof_FP = 8,

   /* Describe the instruction pointer. */
   .offset_IP = S390X_GUEST_OFFSET(guest_IA),
   .sizeof_IP = 8,

   /* Describe any sections to be regarded by Memcheck as
      'always-defined'. */
   .n_alwaysDefd = 9,

   /* Flags thunk: OP and NDEP are always defined, whereas DEP1
      and DEP2 have to be tracked.  See detailed comment in
      gdefs.h on meaning of thunk fields. */
   .alwaysDefd = {
      /*  0 */ ALWAYSDEFD(guest_CC_OP),     /* generic */
      /*  1 */ ALWAYSDEFD(guest_CC_NDEP),   /* generic */
      /*  2 */ ALWAYSDEFD(guest_EMWARN),    /* generic */
      /*  3 */ ALWAYSDEFD(guest_TISTART),   /* generic */
      /*  4 */ ALWAYSDEFD(guest_TILEN),     /* generic */
      /*  5 */ ALWAYSDEFD(guest_IP_AT_SYSCALL), /* generic */
      /*  6 */ ALWAYSDEFD(guest_IA),        /* control reg */
      /*  7 */ ALWAYSDEFD(guest_fpc),       /* control reg */
      /*  8 */ ALWAYSDEFD(guest_counter),   /* internal usage register */
   }
};

/*------------------------------------------------------------*/
/*--- Dirty helper for invalid opcode 00                   ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
void
s390x_dirtyhelper_00(VexGuestS390XState *guest_state)
{
   /* Avoid infinite loop in case SIGILL is caught. See also
      none/tests/s390x/op_exception.c */
   guest_state->guest_IA += 2;

   asm volatile(".hword 0\n");
}
#else
void s390x_dirtyhelper_00(VexGuestS390XState *guest_state) { }
#endif

/*------------------------------------------------------------*/
/*--- Dirty helper for EXecute                             ---*/
/*------------------------------------------------------------*/
void
s390x_dirtyhelper_EX(ULong torun)
{
   last_execute_target = torun;
}


/*------------------------------------------------------------*/
/*--- Dirty helper for Clock instructions                  ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
ULong s390x_dirtyhelper_STCK(ULong *addr)
{
   int cc;

   asm volatile("stck %0\n"
                "ipm %1\n"
                "srl %1,28\n"
                : "+Q" (*addr), "=d" (cc) : : "cc");
   return cc;
}

ULong s390x_dirtyhelper_STCKE(ULong *addr)
{
   int cc;

   asm volatile("stcke %0\n"
                "ipm %1\n"
                "srl %1,28\n"
                : "+Q" (*addr), "=d" (cc) : : "cc");
   return cc;
}

ULong s390x_dirtyhelper_STCKF(ULong *addr)
{
   int cc;

   asm volatile(".insn s,0xb27c0000,%0\n"
                "ipm %1\n"
                "srl %1,28\n"
                : "+Q" (*addr), "=d" (cc) : : "cc");
   return cc;
}
#else
ULong s390x_dirtyhelper_STCK(ULong *addr)  {return 3;}
ULong s390x_dirtyhelper_STCKF(ULong *addr) {return 3;}
ULong s390x_dirtyhelper_STCKE(ULong *addr) {return 3;}
#endif /* VGA_s390x */

/*------------------------------------------------------------*/
/*--- Dirty helper for Store Facility instruction          ---*/
/*------------------------------------------------------------*/
#if defined(VGA_s390x)
ULong
s390x_dirtyhelper_STFLE(VexGuestS390XState *guest_state, HWord addr)
{
   ULong hoststfle[S390_NUM_FACILITY_DW], cc, num_dw, i;
   register ULong reg0 asm("0") = guest_state->guest_r0 & 0xF;  /* r0[56:63] */

   /* We cannot store more than S390_NUM_FACILITY_DW
      (and it makes not much sense to do so anyhow) */
   if (reg0 > S390_NUM_FACILITY_DW - 1)
      reg0 = S390_NUM_FACILITY_DW - 1;

   num_dw = reg0 + 1;  /* number of double words written */

   asm volatile(" .insn s,0xb2b00000,%0\n"   /* stfle */
                "ipm    %2\n"
                "srl    %2,28\n"
                : "=m" (hoststfle), "+d"(reg0), "=d"(cc) : : "cc", "memory");

   /* Update guest register 0  with what STFLE set r0 to */
   guest_state->guest_r0 = reg0;

   for (i = 0; i < num_dw; ++i)
      ((ULong *)addr)[i] = hoststfle[i];

   return cc;
}

#else

ULong
s390x_dirtyhelper_STFLE(VexGuestS390XState *guest_state, HWord addr)
{
   return 3;
}
#endif /* VGA_s390x */

/*------------------------------------------------------------*/
/*--- Helper for condition code.                           ---*/
/*------------------------------------------------------------*/

#define S390_CC_FOR_BINARY(opcode,cc_dep1,cc_dep2) \
({ \
   __asm__ volatile ( \
        opcode " %[op1],%[op2]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+d"(cc_dep1) \
                                   : [op2] "d"(cc_dep2) \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_TERNARY_SUBB(opcode,cc_dep1,cc_dep2,cc_ndep) \
({ \
   /* Recover the original DEP2 value. See comment near s390_cc_thunk_put3 \
      for rationale. */ \
   cc_dep2 = cc_dep2 ^ cc_ndep; \
   __asm__ volatile ( \
	"lghi 0,1\n\t" \
	"sr 0,%[op3]\n\t" /* borrow to cc */ \
        opcode " %[op1],%[op2]\n\t" /* then redo the op */\
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+&d"(cc_dep1) \
                                   : [op2] "d"(cc_dep2), [op3] "d"(cc_ndep) \
                                   : "0", "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_TERNARY_ADDC(opcode,cc_dep1,cc_dep2,cc_ndep) \
({ \
   /* Recover the original DEP2 value. See comment near s390_cc_thunk_put3 \
      for rationale. */ \
   cc_dep2 = cc_dep2 ^ cc_ndep; \
   __asm__ volatile ( \
	"lgfr 0,%[op3]\n\t" /* first load cc_ndep */ \
	"aghi 0,0\n\t" /* and convert it into a cc */ \
        opcode " %[op1],%[op2]\n\t" /* then redo the op */\
        "ipm %[psw]\n\t"           : [psw] "=d"(psw), [op1] "+&d"(cc_dep1) \
                                   : [op2] "d"(cc_dep2), [op3] "d"(cc_ndep) \
                                   : "0", "cc");\
   psw >> 28;   /* cc */ \
})


#define S390_CC_FOR_BFP_RESULT(opcode,cc_dep1) \
({ \
   __asm__ volatile ( \
        opcode " 0,%[op]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw) \
                                   : [op]  "f"(cc_dep1) \
                                   : "cc", "f0");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_RESULT(hi,lo) \
({ \
   __asm__ volatile ( \
        "ldr   4,%[high]\n\t" \
        "ldr   6,%[low]\n\t" \
        "ltxbr 0,4\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw) \
                                   : [high] "f"(hi), [low] "f"(lo) \
                                   : "cc", "f0", "f2", "f4", "f6");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP_CONVERT(opcode,cc_dep1) \
({ \
   __asm__ volatile ( \
        opcode " 0,0,%[op]\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw) \
                                   : [op]  "f"(cc_dep1) \
                                   : "cc", "r0");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_CONVERT(opcode,hi,lo) \
({ \
   __asm__ volatile ( \
        "ldr   4,%[high]\n\t" \
        "ldr   6,%[low]\n\t" \
        opcode " 0,0,4\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw) \
                                   : [high] "f"(hi), [low] "f"(lo) \
                                   : "cc", "r0", "f4", "f6");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP_TDC(opcode,cc_dep1,cc_dep2) \
({ \
   __asm__ volatile ( \
        opcode " %[value],0(%[class])\n\t" \
        "ipm %[psw]\n\t"           : [psw] "=d"(psw) \
                                   : [value] "f"(cc_dep1), \
                                     [class] "a"(cc_dep2)  \
                                   : "cc");\
   psw >> 28;   /* cc */ \
})

#define S390_CC_FOR_BFP128_TDC(cc_dep1,cc_dep2,cc_ndep) \
({ \
   /* Recover the original DEP2 value. See comment near s390_cc_thunk_put1f128Z \
      for rationale. */ \
   cc_dep2 = cc_dep2 ^ cc_ndep; \
   __asm__ volatile ( \
        "ldr  4,%[high]\n\t" \
        "ldr  6,%[low]\n\t" \
        "tcxb 4,0(%[class])\n\t" \
        "ipm  %[psw]\n\t"          : [psw] "=d"(psw) \
                                   : [high] "f"(cc_dep1), [low] "f"(cc_dep2), \
                                     [class] "a"(cc_ndep)  \
                                   : "cc", "f4", "f6");\
   psw >> 28;   /* cc */ \
})


/* Return the value of the condition code from the supplied thunk parameters.
   This is not the value of the PSW. It is the value of the 2 CC bits within
   the PSW. The returned value is thusly in the interval [0:3]. */
UInt
s390_calculate_cc(ULong cc_op, ULong cc_dep1, ULong cc_dep2, ULong cc_ndep)
{
#if defined(VGA_s390x)
   UInt psw;

   switch (cc_op) {

   case S390_CC_OP_BITWISE:
      return S390_CC_FOR_BINARY("ogr", cc_dep1, (ULong)0);

   case S390_CC_OP_SIGNED_COMPARE:
      return S390_CC_FOR_BINARY("cgr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_COMPARE:
      return S390_CC_FOR_BINARY("clgr", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_ADD_64:
      return S390_CC_FOR_BINARY("agr", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_ADD_32:
      return S390_CC_FOR_BINARY("ar", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_SUB_64:
      return S390_CC_FOR_BINARY("sgr", cc_dep1, cc_dep2);

   case S390_CC_OP_SIGNED_SUB_32:
      return S390_CC_FOR_BINARY("sr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_ADD_64:
      return S390_CC_FOR_BINARY("algr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_ADD_32:
      return S390_CC_FOR_BINARY("alr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_ADDC_64:
      return S390_CC_FOR_TERNARY_ADDC("alcgr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_UNSIGNED_ADDC_32:
      return S390_CC_FOR_TERNARY_ADDC("alcr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_UNSIGNED_SUB_64:
      return S390_CC_FOR_BINARY("slgr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_SUB_32:
      return S390_CC_FOR_BINARY("slr", cc_dep1, cc_dep2);

   case S390_CC_OP_UNSIGNED_SUBB_64:
      return S390_CC_FOR_TERNARY_SUBB("slbgr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_UNSIGNED_SUBB_32:
      return S390_CC_FOR_TERNARY_SUBB("slbr", cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_LOAD_AND_TEST:
      /* Like signed comparison with 0 */
      return S390_CC_FOR_BINARY("cgr", cc_dep1, (Long)0);

   case S390_CC_OP_TEST_AND_SET:
      /* Shift the sign bit into the LSB. Note, that the tested value is an
         8-bit value which has been zero-extended to 32/64 bit. */
      return cc_dep1 >> 7;

   case S390_CC_OP_LOAD_POSITIVE_32:
      __asm__ volatile (
           "lpr  %[result],%[op]\n\t"
           "ipm  %[psw]\n\t"            : [psw] "=d"(psw), [result] "=d"(cc_dep1)
                                        : [op] "d"(cc_dep1)
                                        : "cc");
      return psw >> 28;   /* cc */

   case S390_CC_OP_LOAD_POSITIVE_64:
      __asm__ volatile (
           "lpgr %[result],%[op]\n\t"
           "ipm  %[psw]\n\t"            : [psw] "=d"(psw), [result] "=d"(cc_dep1)
                                        : [op] "d"(cc_dep1)
                                        : "cc");
      return psw >> 28;   /* cc */

   case S390_CC_OP_TEST_UNDER_MASK_8: {
      UChar value  = cc_dep1;
      UChar mask   = cc_dep2;

      __asm__ volatile (
           "bras %%r2,1f\n\t"             /* %r2 = address of next insn */
           "tm %[value],0\n\t"            /* this is skipped, then EXecuted */
           "1: ex %[mask],0(%%r2)\n\t"    /* EXecute TM after modifying mask */
           "ipm %[psw]\n\t"             : [psw] "=d"(psw)
                                        : [value] "m"(value), [mask] "a"(mask)
                                        : "r2", "cc");
      return psw >> 28;   /* cc */
   }

   case S390_CC_OP_TEST_UNDER_MASK_16: {
      /* Create a TMLL insn with the mask as given by cc_dep2 */
      UInt insn  = (0xA701 << 16) | cc_dep2;
      UInt value = cc_dep1;

      __asm__ volatile (
           "lr   1,%[value]\n\t"
           "lhi  2,0x10\n\t"
           "ex   2,%[insn]\n\t"
           "ipm  %[psw]\n\t"       : [psw] "=d"(psw)
                                   : [value] "d"(value), [insn] "m"(insn)
                                   : "r1", "r2", "cc");
      return psw >> 28;   /* cc */
   }

   case S390_CC_OP_SHIFT_LEFT_32:
      __asm__ volatile (
           "sla  %[op],0(%[amount])\n\t"
           "ipm  %[psw]\n\t"            : [psw] "=d"(psw), [op] "+d"(cc_dep1)
                                        : [amount] "a"(cc_dep2)
                                        : "cc");
      return psw >> 28;   /* cc */

   case S390_CC_OP_SHIFT_LEFT_64: {
      Int high = (Int)(cc_dep1 >> 32);
      Int low  = (Int)(cc_dep1 & 0xFFFFFFFF);

      __asm__ volatile (
           "lr   2,%[high]\n\t"
           "lr   3,%[low]\n\t"
           "slda 2,0(%[amount])\n\t"
           "ipm %[psw]\n\t"             : [psw] "=d"(psw), [high] "+d"(high), [low] "+d"(low)
                                        : [amount] "a"(cc_dep2)
                                        : "cc", "r2", "r3");
      return psw >> 28;   /* cc */
   }

   case S390_CC_OP_INSERT_CHAR_MASK_32: {
      Int inserted = 0;
      Int msb = 0;

      if (cc_dep2 & 1) {
         inserted |= cc_dep1 & 0xff;
         msb = 0x80;
      }
      if (cc_dep2 & 2) {
         inserted |= cc_dep1 & 0xff00;
         msb = 0x8000;
      }
      if (cc_dep2 & 4) {
         inserted |= cc_dep1 & 0xff0000;
         msb = 0x800000;
      }
      if (cc_dep2 & 8) {
         inserted |= cc_dep1 & 0xff000000;
         msb = 0x80000000;
      }

      if (inserted & msb)  // MSB is 1
         return 1;
      if (inserted > 0)
         return 2;
      return 0;
   }

   case S390_CC_OP_BFP_RESULT_32:
      return S390_CC_FOR_BFP_RESULT("ltebr", cc_dep1);

   case S390_CC_OP_BFP_RESULT_64:
      return S390_CC_FOR_BFP_RESULT("ltdbr", cc_dep1);

   case S390_CC_OP_BFP_RESULT_128:
      return S390_CC_FOR_BFP128_RESULT(cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_32_TO_INT_32:
      return S390_CC_FOR_BFP_CONVERT("cfebr", cc_dep1);

   case S390_CC_OP_BFP_64_TO_INT_32:
      return S390_CC_FOR_BFP_CONVERT("cfdbr", cc_dep1);

   case S390_CC_OP_BFP_128_TO_INT_32:
      return S390_CC_FOR_BFP128_CONVERT("cfxbr", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_32_TO_INT_64:
      return S390_CC_FOR_BFP_CONVERT("cgebr", cc_dep1);

   case S390_CC_OP_BFP_64_TO_INT_64:
      return S390_CC_FOR_BFP_CONVERT("cgdbr", cc_dep1);

   case S390_CC_OP_BFP_128_TO_INT_64:
      return S390_CC_FOR_BFP128_CONVERT("cgxbr", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_TDC_32:
      return S390_CC_FOR_BFP_TDC("tceb", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_TDC_64:
      return S390_CC_FOR_BFP_TDC("tcdb", cc_dep1, cc_dep2);

   case S390_CC_OP_BFP_TDC_128:
      return S390_CC_FOR_BFP128_TDC(cc_dep1, cc_dep2, cc_ndep);

   case S390_CC_OP_SET:
      return cc_dep1;

   default:
      break;
   }
#endif
   vpanic("s390_calculate_cc");
}


UInt
s390_calculate_icc(ULong op, ULong dep1, ULong dep2)
{
   return s390_calculate_cc(op, dep1, dep2, 0 /* unused */);
}


/* Note that this does *not* return a Boolean value. The result needs to be
   explicitly tested against zero. */
UInt
s390_calculate_cond(ULong mask, ULong op, ULong dep1, ULong dep2, ULong ndep)
{
   UInt cc = s390_calculate_cc(op, dep1, dep2, ndep);

   return ((mask << cc) & 0x8);
}

/*------------------------------------------------------------*/
/*--- spechelper for performance                           ---*/
/*------------------------------------------------------------*/


/* Convenience macros */
#define unop(op,a1) IRExpr_Unop((op),(a1))
#define binop(op,a1,a2) IRExpr_Binop((op),(a1),(a2))
#define mkU64(v) IRExpr_Const(IRConst_U64(v))
#define mkU32(v) IRExpr_Const(IRConst_U32(v))
#define mkU8(v)  IRExpr_Const(IRConst_U8(v))


static inline Bool
isC64(IRExpr *expr)
{
   return expr->tag == Iex_Const && expr->Iex.Const.con->tag == Ico_U64;
}


/* The returned expression is NULL if no specialization was found. In that
   case the helper function will be called. Otherwise, the expression has
   type Ity_I32 and a Boolean value. */
IRExpr *
guest_s390x_spechelper(HChar *function_name, IRExpr **args,
                       IRStmt **precedingStmts, Int n_precedingStmts)
{
   UInt i, arity = 0;

   for (i = 0; args[i]; i++)
      arity++;

#  if 0
   vex_printf("spec request:\n");
   vex_printf("   %s  ", function_name);
   for (i = 0; i < arity; i++) {
      vex_printf("  ");
      ppIRExpr(args[i]);
   }
   vex_printf("\n");
#  endif

   /* --------- Specialising "s390_calculate_cond" --------- */

   if (vex_streq(function_name, "s390_calculate_cond")) {
      IRExpr *cond_expr, *cc_op_expr, *cc_dep1, *cc_dep2;
      ULong cond, cc_op;

      vassert(arity == 5);

      cond_expr  = args[0];
      cc_op_expr = args[1];

      /* The necessary requirement for all optimizations here is that the
         condition and the cc_op are constant. So check that upfront. */
      if (! isC64(cond_expr))  return NULL;
      if (! isC64(cc_op_expr)) return NULL;

      cond    = cond_expr->Iex.Const.con->Ico.U64;
      cc_op   = cc_op_expr->Iex.Const.con->Ico.U64;

      vassert(cond <= 15);

      /*
        +------+---+---+---+---+
        | cc   | 0 | 1 | 2 | 3 |
        | cond | 8 | 4 | 2 | 1 |
        +------+---+---+---+---+
      */
      cc_dep1 = args[2];
      cc_dep2 = args[3];

      /* S390_CC_OP_SIGNED_COMPARE */
      if (cc_op == S390_CC_OP_SIGNED_COMPARE) {
         /*
            cc == 0  --> cc_dep1 == cc_dep2   (cond == 8)
            cc == 1  --> cc_dep1 <  cc_dep2   (cond == 4)
            cc == 2  --> cc_dep1 >  cc_dep2   (cond == 2)

            Because cc == 3 cannot occur the rightmost bit of cond is
            a don't care.
         */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, cc_dep2));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, cc_dep2));
         }
         if (cond == 4 || cond == 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, cc_dep1, cc_dep2));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, cc_dep1, cc_dep2));
         }
         /* cc_dep1 > cc_dep2  ---->  cc_dep2 < cc_dep1 */
         if (cond == 2 || cond == 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_UNSIGNED_COMPARE */
      if (cc_op == S390_CC_OP_UNSIGNED_COMPARE) {
         /*
            cc == 0  --> cc_dep1 == cc_dep2   (cond == 8)
            cc == 1  --> cc_dep1 <  cc_dep2   (cond == 4)
            cc == 2  --> cc_dep1 >  cc_dep2   (cond == 2)

            Because cc == 3 cannot occur the rightmost bit of cond is
            a don't care.
         */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, cc_dep2));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, cc_dep2));
         }
         if (cond == 4 || cond == 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep1, cc_dep2));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep1, cc_dep2));
         }
         /* cc_dep1 > cc_dep2  ---->  cc_dep2 < cc_dep1 */
         if (cond == 2 || cond == 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep2, cc_dep1));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_LOAD_AND_TEST */
      if (cc_op == S390_CC_OP_LOAD_AND_TEST) {
         /*
            cc == 0  --> cc_dep1 == 0   (cond == 8)
            cc == 1  --> cc_dep1 <  0   (cond == 4)
            cc == 2  --> cc_dep1 >  0   (cond == 2)

            Because cc == 3 cannot occur the rightmost bit of cond is
            a don't care.
         */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, mkU64(0)));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, mkU64(0)));
         }
         if (cond == 4 || cond == 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, cc_dep1, mkU64(0)));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, cc_dep1, mkU64(0)));
         }
         /* cc_dep1 > 0  ---->  0 < cc_dep1 */
         if (cond == 2 || cond == 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLT64S, mkU64(0), cc_dep1));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE64S, mkU64(0), cc_dep1));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_BITWISE */
      if (cc_op == S390_CC_OP_BITWISE) {
         /*
            cc_dep1 is the result of the boolean operation.

            cc == 0  --> cc_dep1 == 0   (cond == 8)
            cc == 1  --> cc_dep1 != 0   (cond == 4)

            Because cc == 2 and cc == 3 cannot occur the two rightmost bits of
            cond are don't cares. Therefore:

            cond == 00xx  -> always false
            cond == 01xx  -> not equal
            cond == 10xx  -> equal
            cond == 11xx  -> always true
         */
         if ((cond & (8 + 4)) == 8 + 4) {
            return mkU32(1);
         }
         if (cond & 8) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, mkU64(0)));
         }
         if (cond & 4) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, mkU64(0)));
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_INSERT_CHAR_MASK_32
         Since the mask comes from an immediate field in the opcode, we
         expect the mask to be a constant here. That simplifies matters. */
      if (cc_op == S390_CC_OP_INSERT_CHAR_MASK_32) {
         ULong mask;
         UInt imask = 0, shift = 0;
         IRExpr *word;

         if (! isC64(cc_dep2)) goto missed;

         mask = cc_dep2->Iex.Const.con->Ico.U64;

         /* Extract the 32-bit value from the thunk */

         word = unop(Iop_64to32, cc_dep1);

         switch (mask) {
         case 0:  shift =  0; imask = 0x00000000; break;
         case 1:  shift = 24; imask = 0x000000FF; break;
         case 2:  shift = 16; imask = 0x0000FF00; break;
         case 3:  shift = 16; imask = 0x0000FFFF; break;
         case 4:  shift =  8; imask = 0x00FF0000; break;
         case 5:  shift =  8; imask = 0x00FF00FF; break;
         case 6:  shift =  8; imask = 0x00FFFF00; break;
         case 7:  shift =  8; imask = 0x00FFFFFF; break;
         case 8:  shift =  0; imask = 0xFF000000; break;
         case 9:  shift =  0; imask = 0xFF0000FF; break;
         case 10: shift =  0; imask = 0xFF00FF00; break;
         case 11: shift =  0; imask = 0xFF00FFFF; break;
         case 12: shift =  0; imask = 0xFFFF0000; break;
         case 13: shift =  0; imask = 0xFFFF00FF; break;
         case 14: shift =  0; imask = 0xFFFFFF00; break;
         case 15: shift =  0; imask = 0xFFFFFFFF; break;
         }

         /* Select the bits that were inserted */
         word = binop(Iop_And32, word, mkU32(imask));

         /* cc == 0  --> all inserted bits zero or mask == 0   (cond == 8)
            cc == 1  --> leftmost inserted bit is one          (cond == 4)
            cc == 2  --> leftmost inserted bit is zero and not (cond == 2)
                         all inserted bits are zero

            Because cc == 0,1,2 the rightmost bit of the mask is a don't care */
         if (cond == 8 || cond == 8 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ32, word, mkU32(0)));
         }
         if (cond == 4 + 2 || cond == 4 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE32, word, mkU32(0)));
         }

         /* Sign extend */
         if (shift != 0) {
            word = binop(Iop_Sar32, binop(Iop_Shl32, word, mkU8(shift)),
                         mkU8(shift));
         }

         if (cond == 4 || cond == 4 + 1) {  /* word < 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT32S, word, mkU32(0)));
         }
         if (cond == 2 || cond == 2 + 1) {  /* word > 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT32S, mkU32(0), word));
         }
         if (cond == 8 + 4 || cond == 8 + 4 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE32S, word, mkU32(0)));
         }
         if (cond == 8 + 2 || cond == 8 + 2 + 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpLE32S, mkU32(0), word));
         }
         if (cond == 8 + 4 + 2 || cond == 8 + 4 + 2 + 1) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_TEST_UNDER_MASK_8
         Since the mask comes from an immediate field in the opcode, we
         expect the mask to be a constant here. That simplifies matters. */
      if (cc_op == S390_CC_OP_TEST_UNDER_MASK_8) {
         ULong mask16;

         if (! isC64(cc_dep2)) goto missed;

         mask16 = cc_dep2->Iex.Const.con->Ico.U64;

         /* Get rid of the mask16 == 0 case first. Some of the simplifications
            below (e.g. for OVFL) only hold if mask16 == 0.  */
         if (mask16 == 0) {   /* cc == 0 */
            if (cond & 0x8) return mkU32(1);
            return mkU32(0);
         }

         /* cc == 2 is a don't care */
         if (cond == 8 || cond == 8 + 2) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7 || cond == 7 - 2) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 1 || cond == 1 + 2) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          cc_dep2));
         }
         if (cond == 14 || cond == 14 - 2) {  /* ! OVFL */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          cc_dep2));
         }
         goto missed;
      }

      /* S390_CC_OP_TEST_UNDER_MASK_16
         Since the mask comes from an immediate field in the opcode, we
         expect the mask to be a constant here. That simplifies matters. */
      if (cc_op == S390_CC_OP_TEST_UNDER_MASK_16) {
         ULong mask16;
         UInt msb;

         if (! isC64(cc_dep2)) goto missed;

         mask16 = cc_dep2->Iex.Const.con->Ico.U64;

         /* Get rid of the mask16 == 0 case first. Some of the simplifications
            below (e.g. for OVFL) only hold if mask16 == 0.  */
         if (mask16 == 0) {   /* cc == 0 */
            if (cond & 0x8) return mkU32(1);
            return mkU32(0);
         }

         if (cond == 8) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7) {
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 1) {
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(mask16)));
         }
         if (cond == 14) {  /* ! OVFL */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_And64, cc_dep1, cc_dep2),
                                          mkU64(mask16)));
         }

         /* Find MSB in mask */
         msb = 0x8000;
         while (msb > mask16)
            msb >>= 1;

         if (cond == 2) {  /* cc == 2 */
            IRExpr *c1, *c2;

            /* (cc_dep & msb) != 0 && (cc_dep & mask16) != mask16 */
            c1 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, mkU64(msb)), mkU64(0));
            c2 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(mask16));
            return binop(Iop_And32, unop(Iop_1Uto32, c1),
                         unop(Iop_1Uto32, c2));
         }

         if (cond == 4) {  /* cc == 1 */
            IRExpr *c1, *c2;

            /* (cc_dep & msb) == 0 && (cc_dep & mask16) != 0 */
            c1 = binop(Iop_CmpEQ64,
                       binop(Iop_And64, cc_dep1, mkU64(msb)), mkU64(0));
            c2 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(0));
            return binop(Iop_And32, unop(Iop_1Uto32, c1),
                         unop(Iop_1Uto32, c2));
         }

         if (cond == 11) {  /* cc == 0,2,3 */
            IRExpr *c1, *c2;

            c1 = binop(Iop_CmpNE64,
                       binop(Iop_And64, cc_dep1, mkU64(msb)), mkU64(0));
            c2 = binop(Iop_CmpEQ64,
                       binop(Iop_And64, cc_dep1, cc_dep2),
                       mkU64(0));
            return binop(Iop_Or32, unop(Iop_1Uto32, c1),
                         unop(Iop_1Uto32, c2));
         }

         if (cond == 3) {  /* cc == 2 || cc == 3 */
            return unop(Iop_1Uto32,
                        binop(Iop_CmpNE64,
                              binop(Iop_And64, cc_dep1, mkU64(msb)),
                              mkU64(0)));
         }
         if (cond == 12) { /* cc == 0 || cc == 1 */
            return unop(Iop_1Uto32,
                        binop(Iop_CmpEQ64,
                              binop(Iop_And64, cc_dep1, mkU64(msb)),
                              mkU64(0)));
         }
         // vex_printf("TUM mask = 0x%llx\n", mask16);
         goto missed;
      }

      /* S390_CC_OP_UNSIGNED_SUB_64/32 */
      if (cc_op == S390_CC_OP_UNSIGNED_SUB_64 ||
          cc_op == S390_CC_OP_UNSIGNED_SUB_32) {
         /*
            cc_dep1, cc_dep2 are the zero extended left and right operands

            cc == 1  --> result != 0, borrow    (cond == 4)
            cc == 2  --> result == 0, no borrow (cond == 2)
            cc == 3  --> result != 0, no borrow (cond == 1)

            cc = (cc_dep1 == cc_dep2) ? 2
                                      : (cc_dep1 > cc_dep2) ? 3 : 1;

            Because cc == 0 cannot occur the leftmost bit of cond is
            a don't care.
         */
         if (cond == 1 || cond == 1 + 8) {  /* cc == 3   op2 < op1 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep2, cc_dep1));
         }
         if (cond == 2 || cond == 2 + 8) {  /* cc == 2 */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64, cc_dep1, cc_dep2));
         }
         if (cond == 4 || cond == 4 + 8) {  /* cc == 1 */
            return unop(Iop_1Uto32, binop(Iop_CmpLT64U, cc_dep1, cc_dep2));
         }
         if (cond == 3 || cond == 3 + 8) {  /* cc == 2 || cc == 3 */
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep2, cc_dep1));
         }
         if (cond == 6 || cond == 6 + 8) {  /* cc == 2 || cc == 1 */
            return unop(Iop_1Uto32, binop(Iop_CmpLE64U, cc_dep1, cc_dep2));
         }

         if (cond == 5 || cond == 5 + 8) {  /* cc == 3 || cc == 1 */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64, cc_dep1, cc_dep2));
         }
         if (cond == 7 || cond == 7 + 8) {
            return mkU32(1);
         }
         /* Remaining case */
         return mkU32(0);
      }

      /* S390_CC_OP_UNSIGNED_ADD_64 */
      if (cc_op == S390_CC_OP_UNSIGNED_ADD_64) {
         /*
            cc_dep1, cc_dep2 are the zero extended left and right operands

            cc == 0  --> result == 0, no carry  (cond == 8)
            cc == 1  --> result != 0, no carry  (cond == 4)
            cc == 2  --> result == 0, carry     (cond == 2)
            cc == 3  --> result != 0, carry     (cond == 1)
         */
         if (cond == 8) { /* cc == 0 */
            /* Both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7) { /* cc == 1,2,3 */
            /* Not both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 8 + 2) {  /* cc == 0,2  -> result is zero */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_Add64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 4 + 1) {  /* cc == 1,3  -> result is not zero */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_Add64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         goto missed;
      }

      /* S390_CC_OP_UNSIGNED_ADD_32 */
      if (cc_op == S390_CC_OP_UNSIGNED_ADD_32) {
         /*
            cc_dep1, cc_dep2 are the zero extended left and right operands

            cc == 0  --> result == 0, no carry  (cond == 8)
            cc == 1  --> result != 0, no carry  (cond == 4)
            cc == 2  --> result == 0, carry     (cond == 2)
            cc == 3  --> result != 0, carry     (cond == 1)
         */
         if (cond == 8) { /* cc == 0 */
            /* Both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 7) { /* cc == 1,2,3 */
            /* Not both inputs are 0 */
            return unop(Iop_1Uto32, binop(Iop_CmpNE64,
                                          binop(Iop_Or64, cc_dep1, cc_dep2),
                                          mkU64(0)));
         }
         if (cond == 8 + 2) {  /* cc == 0,2  -> result is zero */
            return unop(Iop_1Uto32, binop(Iop_CmpEQ32,
                                          binop(Iop_Add32,
                                                unop(Iop_64to32, cc_dep1),
                                                unop(Iop_64to32, cc_dep2)),
                                          mkU32(0)));
         }
         if (cond == 4 + 1) {  /* cc == 1,3  -> result is not zero */
            return unop(Iop_1Uto32, binop(Iop_CmpNE32,
                                          binop(Iop_Add32,
                                                unop(Iop_64to32, cc_dep1),
                                                unop(Iop_64to32, cc_dep2)),
                                          mkU32(0)));
         }
         goto missed;
      }

      /* S390_CC_OP_SET */
      if (cc_op == S390_CC_OP_SET) {
         /* cc_dep1 is the condition code

            Return 1, if ((cond << cc_dep1) & 0x8) != 0 */

        return unop(Iop_1Uto32,
                    binop(Iop_CmpNE64,
                          binop(Iop_And64,
                                binop(Iop_Shl64, cond_expr,
                                      unop(Iop_64to8, cc_dep1)),
                                mkU64(8)),
                          mkU64(0)));
      }

      /* S390_CC_OP_TEST_AND_SET */
      if (cc_op == S390_CC_OP_TEST_AND_SET) {
         /* cc_dep1 is the zero-extended loaded value

            cc == 0  --> leftmost bit is zero  (cond == 8)
            cc == 1  --> leftmost bit is one   (cond == 4)

            As cc is either 0 or 1, only the two leftmost bits of the mask
            are relevant. */
         IRExpr *bit = binop(Iop_Shr64, cc_dep1, mkU8(7));

         switch (cond & (8 + 4)) {
         case 0:     return mkU32(0);
         case 4:     return unop(Iop_1Uto32, binop(Iop_CmpNE64, bit, mkU64(0)));
         case 8:     return unop(Iop_1Uto32, binop(Iop_CmpEQ64, bit, mkU64(0)));
         case 8 + 4: return mkU32(1);
         }
         /* not reached */
      }

missed:
      ;
   }

   return NULL;
}

/*---------------------------------------------------------------*/
/*--- end                                guest_s390_helpers.c ---*/
/*---------------------------------------------------------------*/
