/* HOW TO COMPILE:
 * 64bit build:
 *    gcc -Winline -Wall -g -O -mregnames -maltivec -m64
 */

/*
 * test_isa_3_0.c:
 * Copyright (C) 2016-2017 Carl Love <cel@us.ibm.com>
 * Copyright (C) 2016-2017 Will Schmidt <will_schmidt@vnet.ibm.com>
 *
 * This testfile contains tests for the ISA 3.0 instructions.
 * The framework of this test file was based on the framework
 * of the jm-insns.c testfile, whose original author was
 * Jocelyn Mayer.
 */

/*
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License V2
 *   as published by the Free Software Foundation
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Theory of operations:
 * a few registers are reserved for the test program:
 * r14...r18
 * f14...f18
 * - pre-load test values in r14 through r17
 * - patch the test opcode if any immediate operands are
 *   required
 * - execute the tested opcode.
 * CR and FPSCR are cleared before every test.
 * results in {r,f}17
 * check FPSCR for floating point operations.
 */

/*
 * Operation details
 * -----------------
 * The 'test' functions (via all_tests[]) are wrappers of
 * single __asm__ instructions
 *
 * The 'loops' (e.g. int_loops) do the actual work:
 *  - loops over as many arguments as the instruction needs (regs | imms)
 *     - sets up the environment (reset cr, assign src regs...)
 *     - maybe modifies the asm instruction to test different immediate args
 *     - call the test function
 *     - retrieve relevant register data (rD,cr,...)
 *     - prints argument and result data.
 *
 * all_tests[i] holds instruction tests
 *  - of which each holds: {instn_test_arr[], description, flags}
 *
 * flags hold 3 instruction classifiers: {family, type, arg_type}
 *
 * // The main test loop:
 * do_tests( user_ctl_flags ) {
 *    foreach(curr_test = all_test[i]) {
 *
 *       // flags are used to control what tests are run:
 *       if (curr_test->flags && !user_ctl_flags)
 *          continue;
 *
 *       // a 'loop_family_arr' is chosen based on the 'family' flag...
 *       switch(curr_test->flags->family) {
 *       case x: loop_family_arr = int_loops;
 *      ...
 *       }
 *
 *       // ...and the actual test_loop to run is found by indexing into
 *       // the loop_family_arr with the 'arg_type' flag:
 *       test_loop = loop_family[curr_test->flags->arg_type]
 *
 *       // finally, loop over all instn tests for this test:
 *       foreach (instn_test = curr_test->instn_test_arr[i]) {
 *
 *          // and call the test_loop with the current instn_test function,name
 *          test_loop( instn_test->func, instn_test->name )
 *       }
 *    }
 * }
 */

#include <stdio.h>
#include <stdint.h>

/* This test is only valid on machines that support IBM POWER ISA 3.0. */
#ifdef HAS_ISA_3_00

#include <assert.h>
#include <ctype.h>     // isspace
#include <stdlib.h>
#include <string.h>
#include <unistd.h>    // getopt
#include <altivec.h> // vector

#undef DEBUG_VECTOR_PERMUTE
static int verbose = 0;

#include "../ppc64/ppc64_helpers.h" // SET_CR() and friends.

#define VERBOSE_FUNCTION_CALLOUT \
   if (verbose) \
      printf("Test Harness Function: %s\n", __FUNCTION__);

/* generic out-of-range reporting.
 * Note: results are typically 'undefined' in these cases, so rather than
 * pushing through and getting potentially random results, avoid the check.
 * The caller should suppress output when insert_extract_error is set. */
#define vinsertextract_err    \
   insert_extract_error = 1;  \
   if (verbose > 1)           \
      printf("Expected error - index out of range in %s (%d)\n", \
             __FUNCTION__, x_index);

#define MAX(x, y) (x > y ? x : y)

/* Used in do_tests, indexed by flags->nb_args
   Elements correspond to enum test_flags::num args
*/

/* XXXX these must all be callee-save regs! */
register HWord_t r14 __asm__ ("r14");
register HWord_t r15 __asm__ ("r15");
register HWord_t r16 __asm__ ("r16");
register HWord_t r17 __asm__ ("r17");
register double  f14 __asm__ ("fr14");
register double  f15 __asm__ ("fr15");

/* globals used for vector tests */
static vector unsigned long vec_xa, vec_xb, vec_xc, vec_xt;

/* globals for the condition register fields.  These are used to
 * capture the condition register values immediately after the
 * instruction under test is tested.
 * This is to help prevent other test overhead, switch statements,
 * compares, what-not from interfering.
 */
unsigned long local_cr;
unsigned long local_fpscr;
unsigned long local_xer;
volatile unsigned int cr_value;

/* global for holding the DFP values */
dfp_val_t dfp_value;

/* individual instruction tests */
typedef void (*test_func_t) (void);
struct test_list_t {
   test_func_t func;
   const char *name;
};
typedef struct test_list_t test_list_t;

/* global variable, used to pass shift info down to the test functions.*/
volatile int x_shift;

/* Indicator for DCMX (Data Class Mask) matches. */
volatile int dcmx_match;

/* Error indicator to determine of the UIN (Unsigned Immediate) field
 * from insert/extract was out of range */
volatile int insert_extract_error;

/* vector splat value */
volatile int x_splat;
volatile int dfp_significance;

/* global variable, ... vector insert functions. */
volatile int x_index;

/* global variable, used to pass shift info down to the test functions.*/
/* Also used to control DRM,RM values for mffs* functions. */
volatile int x_shift;

/* groups of instruction tests, calling individual tests */
typedef void (*test_group_t) (const char *name, test_func_t func,
                              unsigned int test_flags);

enum test_flags {
   /* Nb arguments */
   PPC_ONE_ARG        = 0x00000001,
   PPC_TWO_ARGS       = 0x00000002,
   PPC_THREE_ARGS     = 0x00000003,
   PPC_FOUR_ARGS      = 0x00000004,
   PPC_COMPARE_ARGS   = 0x00000005,
   PPC_LD_ARGS        = 0x00000006,
   PPC_ST_ARGS        = 0x00000007,
   PPC_ONE_IMM        = 0x00000008,
   PPC_ONE_GPR_ONE_VEC = 0x00000009,
   PPC_NB_ARGS_MASK   = 0x0000000F,

   /* Type */
   PPC_ARITH          = 0x00000100,
   PPC_LOGICAL        = 0x00000200,
   PPC_COMPARE        = 0x00000300,
   PPC_LDST           = 0x00000400,
   PPC_POPCNT         = 0x00000500,
   PPC_INSERTEXTRACT  = 0x00000600,
   PPC_PERMUTE        = 0x00000700,
   PPC_ROUND          = 0x00000800,
   PPC_TYPE_MASK      = 0x00000F00,

   /* Family */
   PPC_INTEGER        = 0x00010000,
   PPC_ALTIVEC        = 0x00030000,
   PPC_ALTIVEC_QUAD   = 0x00040000,
   PPC_ALTIVEC_DOUBLE = 0x00050000,
   PPC_DFP            = 0x00060000,
   PPC_BCD            = 0x00070000,
   PPC_MISC           = 0x00080000,
   PPC_NO_OP          = 0x00090000,
   PPC_PC_IMMEDIATE   = 0x000A0000,
   PPC_MFFS           = 0x000B0000,
   PPC_FAMILY_MASK    = 0x000F0000,

   /* Flags: these may be combined, so use separate bit-fields. */
   PPC_CR             = 0x01000000,
   PPC_XER_CA         = 0x02000000,
};

static void test_cnttzw (void)
{
   __asm__ __volatile__ ("cnttzw       17, 14");
}

static void test_cnttzd (void)
{
   __asm__ __volatile__ ("cnttzd       17, 14");
}

static void test_dotted_cnttzw (void)
{
   __asm__ __volatile__ ("cnttzw.      17, 14");
}

static void test_dotted_cnttzd (void)
{
   __asm__ __volatile__ ("cnttzd.      17, 14");
}

static test_list_t testgroup_logical_one[] = {
   { &test_cnttzw       , "cnttzw"  },
   { &test_cnttzd       , "cnttzd"  },
   { &test_dotted_cnttzw, "cnttzw." },
   { &test_dotted_cnttzd, "cnttzd." },
   { NULL               , NULL      },
};

static void test_modsw (void)
{
   __asm__ __volatile__ ("modsw          17, 14, 15");
}

static void test_moduw (void)
{
   __asm__ __volatile__ ("moduw          17, 14, 15");
}

static void test_modsd (void)
{
   __asm__ __volatile__ ("modsd          17, 14, 15");
}

static void test_modud (void)
{
   __asm__ __volatile__ ("modud          17, 14, 15");
}

static void test_addex(void) {
   /* addex RT,RA,RB,CY  # at the time of this writing, only CY=0 is valid.  CY values of 1,2,3 are reserved. */
   __asm__ __volatile__ ("addex   %0, %1, %2, 0" : "=r" (r17) : "r" (r14), "r" (r15) );
}

static test_list_t testgroup_ia_ops_two[] = {
   { &test_modsw, "modsw" },
   { &test_moduw, "moduw" },
   { &test_modsd, "modsd" },
   { &test_modud, "modud" },
   { &test_addex, "addex" },
   { NULL       , NULL             },
};

static void test_dotted_extswsli (void)
{
   switch(x_shift) {
   case SH_0:
      __asm__ __volatile__ ("extswsli. %0, %1, %2" : "=r" (r17) : "r" (r14), "i" (SH_0) );
      break;

   case SH_1:
      __asm__ __volatile__ ("extswsli. %0, %1, %2" : "=r" (r17) : "r" (r14), "i" (SH_1) );
      break;

   case SH_2:
      __asm__ __volatile__ ("extswsli. %0, %1, %2" : "=r" (r17) : "r" (r14), "i" (SH_2) );
      break;

   case SH_3:
      __asm__ __volatile__ ("extswsli. %0, %1, %2" : "=r" (r17) : "r" (r14), "i" (SH_3) );
      break;

   default:
      printf("Unhandled shift value for extswsli. %d\n", x_shift);
   }
}

static void test_extswsli (void)
{
   switch(x_shift) {
   case SH_0:
      __asm__ __volatile__ ("extswsli %0, %1, %2" : "=r" (r17) : "r" (r14), "i"(SH_0));
      break;

   case SH_1:
      __asm__ __volatile__ ("extswsli %0, %1, %2" : "=r" (r17) : "r" (r14), "i"(SH_1));
      break;

   case SH_2:
      __asm__ __volatile__ ("extswsli %0, %1, %2" : "=r" (r17) : "r" (r14), "i"(SH_2));
      break;

   case SH_3:
      __asm__ __volatile__ ("extswsli %0, %1, %2" : "=r" (r17) : "r" (r14), "i"(SH_3));
      break;

   default:
      printf("Unhandled shift value for extswsli %d\n", x_shift);
   }
}

static test_list_t testgroup_shifted_one[] = {
      { &test_extswsli       , "extswsli " },
      { &test_dotted_extswsli, "extswsli." },
      { NULL                 , NULL        },
};

static void test_maddhd (void)
{
   __asm__ __volatile__ ("maddhd 17, 14, 15, 16");
}
static void test_maddhdu (void)
{
   __asm__ __volatile__ ("maddhdu 17, 14, 15, 16");
}
static void test_maddld (void)
{
   __asm__ __volatile__ ("maddld 17, 14, 15, 16");
}

static test_list_t testgroup_three_args[] = {
   { &test_maddhd , "maddhd " },
   { &test_maddhdu, "maddhdu" },
   { &test_maddld , "maddld " },
   { NULL         , NULL      },
};

/* VSX vector permutes. */
static void test_xxperm (void)
{
   __asm__ __volatile__ ("xxperm     %x0, %x1, %x2" : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

/* VSX vector permute, right indexed. */
static void test_xxpermr (void)
{
   __asm__ __volatile__ ("xxpermr    %x0, %x1, %x2" : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static test_list_t testgroup_vsx_xxpermute[] = {
   { &test_xxperm , "xxperm"  },
   { &test_xxpermr, "xxpermr" },
   { NULL         , NULL      },
};

static void test_vabsdub(void) {
   __asm__ __volatile__ ("vabsdub    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vabsduh(void) {
   __asm__ __volatile__ ("vabsduh    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vabsduw(void) {
   __asm__ __volatile__ ("vabsduw    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vcmpneb(void) {
   __asm__ __volatile__ ("vcmpneb    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_dotted_vcmpneb(void) {
   __asm__ __volatile__ ("vcmpneb.    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vcmpnezb(void) {
   __asm__ __volatile__ ("vcmpnezb    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_dotted_vcmpnezb(void) {
   __asm__ __volatile__ ("vcmpnezb.    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vcmpneh(void) {
   __asm__ __volatile__ ("vcmpneh    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_dotted_vcmpneh(void) {
   __asm__ __volatile__ ("vcmpneh.    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vcmpnezh(void) {
   __asm__ __volatile__ ("vcmpnezh    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_dotted_vcmpnezh(void) {
   __asm__ __volatile__ ("vcmpnezh.    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vcmpnew(void) {
   __asm__ __volatile__ ("vcmpnew    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_dotted_vcmpnew(void) {
   __asm__ __volatile__ ("vcmpnew.    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vcmpnezw(void) {
   __asm__ __volatile__ ("vcmpnezw    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_dotted_vcmpnezw(void) {
   __asm__ __volatile__ ("vcmpnezw.    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vrlwmi(void) {
   __asm__ __volatile__ ("vrlwmi    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vrldmi(void) {
   __asm__ __volatile__ ("vrldmi    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vbpermd(void) {
/* vector bit permute doubleword */
    __asm__ __volatile__ ("vbpermd   %0, %1, %2 " : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vrlwnm(void) {
   __asm__ __volatile__ ("vrlwnm    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_vrldnm(void) {
   __asm__ __volatile__ ("vrldnm    %0, %1, %2" : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb));
}

static void test_xviexpdp(void) {
   __asm__ __volatile__ ("xviexpdp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static void test_xviexpsp(void) {
   __asm__ __volatile__ ("xviexpsp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static test_list_t testgroup_vsx_absolute[] = {
   { &test_vabsdub        , "vabsdub"   },
   { &test_vabsduh        , "vabsduh"   },
   { &test_vabsduw        , "vabsduw"   },
   { &test_vcmpneb        , "vcmpneb"   },
   { &test_dotted_vcmpneb , "vcmpneb."  },
   { &test_vcmpnezb       , "vcmpnezb"  },
   { &test_dotted_vcmpnezb, "vcmpnezb." },
   { &test_vcmpneh        , "vcmpneh"   },
   { &test_dotted_vcmpneh , "vcmpneh."  },
   { &test_vcmpnezh       , "vcmpnezh"  },
   { &test_dotted_vcmpnezh, "vcmpnezh." },
   { &test_vcmpnew        , "vcmpnew"   },
   { &test_dotted_vcmpnew , "vcmpnew."  },
   { &test_vcmpnezw       , "vcmpnezw"  },
   { &test_dotted_vcmpnezw, "vcmpnezw." },
   { &test_vrlwnm         , "vrlwnm"    },
   { &test_vrlwmi         , "vrlwmi"    },
   { &test_vrldnm         , "vrldnm"    },
   { &test_vrldmi         , "vrldmi"    },
   { &test_vbpermd        , "vbpermd"   },
   { &test_xviexpdp       , "xviexpdp"  },
   { &test_xviexpsp       , "xviexpsp"  },
   { NULL                 , NULL        },
};

static void test_vpermr(void)
{ /* vector permute right-indexed */
    __asm__ __volatile__ ("vpermr   %0, %1, %2, %3 " : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb), "v" (vec_xc));
}

static void test_vmsumudm(void)
{ /* vector multiply-sum unsigned byte modulo. */
    __asm__ __volatile__ ("vmsumudm  %0, %1, %2, %3 " : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb), "v" (vec_xc));
}

/* vector, 3->1 unique; four arguments. xt, xa, xb, xc (xc = permute) */
static test_list_t testgroup_vector_four[] = {
   { &test_vpermr,   "vpermr" },
   { &test_vmsumudm, "vmsumudm" },
   { NULL        , NULL     },
};

/* vector insert instructions */
#define VINSERTB(X)    __asm__ __volatile__ ("vinsertb    %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VINSERTH(X)    __asm__ __volatile__ ("vinserth    %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VINSERTW(X)    __asm__ __volatile__ ("vinsertw    %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VINSERTD(X)    __asm__ __volatile__ ("vinsertd    %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VEXTRACTUB(X)  __asm__ __volatile__ ("vextractub  %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VEXTRACTUH(X)  __asm__ __volatile__ ("vextractuh  %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VEXTRACTUW(X)  __asm__ __volatile__ ("vextractuw  %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define VEXTRACTD(X)   __asm__ __volatile__ ("vextractd   %0, %1, %2" : "+v" (vec_xt) : "v" (vec_xb), "i"(X));

#define XXINSERTW(X)   __asm__ __volatile__ ("xxinsertw   %x0, %x1, %2" : "+wa" (vec_xt) : "wa" (vec_xb), "i"(X));

#define XXEXTRACTUW(X) __asm__ __volatile__ ("xxextractuw %x0, %x1, %2" : "+wa" (vec_xt) : "wa" (vec_xb), "i"(X));

static void test_vinsertb (void)
{
   switch(x_index) {
   case  0: VINSERTB( 0); break;
   case  1: VINSERTB( 1); break;
   case  2: VINSERTB( 2); break;
   case  3: VINSERTB( 3); break;
   case  4: VINSERTB( 4); break;
   case  5: VINSERTB( 5); break;
   case  6: VINSERTB( 6); break;
   case  7: VINSERTB( 7); break;
   case  8: VINSERTB( 8); break;
   case  9: VINSERTB( 9); break;
   case 10: VINSERTB(10); break;
   case 11: VINSERTB(11); break;
   case 12: VINSERTB(12); break;
   case 13: VINSERTB(13); break;
   case 14: VINSERTB(14); break;
   case 15: VINSERTB(15); break;
   default:
      vinsertextract_err;
      break;
   }
}

static void test_vinserth (void)
{
   switch(x_index) {
   case 0:  VINSERTH(0); break;
   case 1:  VINSERTH(1); break;
   case 2:  VINSERTH(2); break;
   case 3:  VINSERTH(3); break;
   case 4:  VINSERTH(4); break;
   case 5:  VINSERTH(5); break;
   case 6:  VINSERTH(6); break;
   case 7:  VINSERTH(7); break;
   case 8:  VINSERTH(8); break;
   case 9:  VINSERTH(9); break;
   case 10: VINSERTH(10); break;
   case 11: VINSERTH(11); break;
   case 12: VINSERTH(12); break;
   case 13: VINSERTH(13); break;
   case 14: VINSERTH(14); break;
   default:
      vinsertextract_err;
      break;
   }
}

static void test_vinsertw (void)
{
   switch(x_index) {
   case 0:  VINSERTW(0); break;
   case 1:  VINSERTW(1); break;
   case 2:  VINSERTW(2); break;
   case 3:  VINSERTW(3); break;
   case 4:  VINSERTW(4); break;
   case 5:  VINSERTW(5); break;
   case 6:  VINSERTW(6); break;
   case 7:  VINSERTW(7); break;
   case 8:  VINSERTW(8); break;
   case 9:  VINSERTW(9); break;
   case 10: VINSERTW(10); break;
   case 11: VINSERTW(11); break;
   case 12: VINSERTW(12); break;
   default:
      vinsertextract_err;
      break;
   }
}

static void test_vinsertd (void)
{
   switch(x_index) {
   case 0:  VINSERTD(0); break;
   case 1:  VINSERTD(1); break;
   case 2:  VINSERTD(2); break;
   case 3:  VINSERTD(3); break;
   case 4:  VINSERTD(4); break;
   case 5:  VINSERTD(5); break;
   case 6:  VINSERTD(6); break;
   case 7:  VINSERTD(7); break;
   case 8:  VINSERTD(8); break;
   default:
      vinsertextract_err;
      break;
   }
}

/* extracts */
static void test_vextractub (void)
{
   switch(x_index) {
      case  0: VEXTRACTUB( 0); break;
      case  1: VEXTRACTUB( 1); break;
      case  2: VEXTRACTUB( 2); break;
      case  3: VEXTRACTUB( 3); break;
      case  4: VEXTRACTUB( 4); break;
      case  5: VEXTRACTUB( 5); break;
      case  6: VEXTRACTUB( 6); break;
      case  7: VEXTRACTUB( 7); break;
      case  8: VEXTRACTUB( 8); break;
      case  9: VEXTRACTUB( 9); break;
      case 10: VEXTRACTUB(10); break;
      case 11: VEXTRACTUB(11); break;
      case 12: VEXTRACTUB(12); break;
      case 13: VEXTRACTUB(13); break;
      case 14: VEXTRACTUB(14); break;
      case 15: VEXTRACTUB(15); break;
      default:
         vinsertextract_err;
         break;
   }
}

static void test_vextractuh (void)
{
   switch(x_index) {
      case  0: VEXTRACTUH( 0); break;
      case  1: VEXTRACTUH( 1); break;
      case  2: VEXTRACTUH( 2); break;
      case  3: VEXTRACTUH( 3); break;
      case  4: VEXTRACTUH( 4); break;
      case  5: VEXTRACTUH( 5); break;
      case  6: VEXTRACTUH( 6); break;
      case  7: VEXTRACTUH( 7); break;
      case  8: VEXTRACTUH( 8); break;
      case  9: VEXTRACTUH( 9); break;
      case 10: VEXTRACTUH(10); break;
      case 11: VEXTRACTUH(11); break;
      case 12: VEXTRACTUH(12); break;
      case 13: VEXTRACTUH(13); break;
      case 14: VEXTRACTUH(14); break;
      default:
         vinsertextract_err;
         break;
   }
}

static void test_vextractuw (void)
{
   switch(x_index) {
      case  0: VEXTRACTUW( 0); break;
      case  1: VEXTRACTUW( 1); break;
      case  2: VEXTRACTUW( 2); break;
      case  3: VEXTRACTUW( 3); break;
      case  4: VEXTRACTUW( 4); break;
      case  5: VEXTRACTUW( 5); break;
      case  6: VEXTRACTUW( 6); break;
      case  7: VEXTRACTUW( 7); break;
      case  8: VEXTRACTUW( 8); break;
      case  9: VEXTRACTUW( 9); break;
      case 10: VEXTRACTUW(10); break;
      case 11: VEXTRACTUW(11); break;
      default:
         vinsertextract_err;
         break;
   }
}

static void test_vextractd (void)
{
   switch(x_index) {
      case  0: VEXTRACTD( 0); break;
      case  1: VEXTRACTD( 1); break;
      case  2: VEXTRACTD( 2); break;
      case  3: VEXTRACTD( 3); break;
      case  4: VEXTRACTD( 4); break;
      case  5: VEXTRACTD( 5); break;
      case  6: VEXTRACTD( 6); break;
      case  7: VEXTRACTD( 7); break;
      case  8: VEXTRACTD( 8); break;
      default:
         vinsertextract_err;
         break;
   }
}

static void test_xxinsertw (void)
{
   switch(x_index) {
      case  0: XXINSERTW( 0); break;
      case  1: XXINSERTW( 1); break;
      case  2: XXINSERTW( 2); break;
      case  3: XXINSERTW( 3); break;
      case  4: XXINSERTW( 4); break;
      case  5: XXINSERTW( 5); break;
      case  6: XXINSERTW( 6); break;
      case  7: XXINSERTW( 7); break;
      case  8: XXINSERTW( 8); break;
      case  9: XXINSERTW( 9); break;
      case 10: XXINSERTW(10); break;
      case 11: XXINSERTW(11); break;
      case 12: XXINSERTW(12); break;
      default:
         vinsertextract_err;
         break;
   }
}

static void test_xxextractuw (void)
{
   switch(x_index) {
      case  0: XXEXTRACTUW( 0); break;
      case  1: XXEXTRACTUW( 1); break;
      case  2: XXEXTRACTUW( 2); break;
      case  3: XXEXTRACTUW( 3); break;
      case  4: XXEXTRACTUW( 4); break;
      case  5: XXEXTRACTUW( 5); break;
      case  6: XXEXTRACTUW( 6); break;
      case  7: XXEXTRACTUW( 7); break;
      case  8: XXEXTRACTUW( 8); break;
      case  9: XXEXTRACTUW( 9); break;
      case 10: XXEXTRACTUW(10); break;
      case 11: XXEXTRACTUW(11); break;
      case 12: XXEXTRACTUW(12); break;
      default:
         vinsertextract_err;
         break;
   }
}

static test_list_t testgroup_vector_inserts[] = {
   { &test_vinsertb   , "vinsertb   " },
   { &test_vinserth   , "vinserth   " },
   { &test_vinsertw   , "vinsertw   " },
   { &test_vinsertd   , "vinsertd   " },
   { &test_vextractub , "vextractub " },
   { &test_vextractuh , "vextractuh " },
   { &test_vextractuw , "vextractuw " },
   { &test_vextractd  , "vextractd  " },
   { &test_xxinsertw  , "xxinsertw  " },
   { &test_xxextractuw, "xxextractuw" },
   { NULL             , NULL          },
};

static void test_xxspltib(void)
{ /* vector splat byte */
   switch(x_splat) {
      case SPLAT0:     __asm__ __volatile__ ("xxspltib  %x0, %1 " : "=wa"(vec_xt) : "i"(SPLAT0)); break;

      case SPLAT1:     __asm__ __volatile__ ("xxspltib  %x0, %1 " : "=wa"(vec_xt) : "i"(SPLAT1)); break;

      case SPLAT2:     __asm__ __volatile__ ("xxspltib  %x0, %1 " : "=wa"(vec_xt) : "i"(SPLAT2)); break;

      case SPLAT3:     __asm__ __volatile__ ("xxspltib  %x0, %1 " : "=wa"(vec_xt) : "i"(SPLAT3)); break;

      case SPLAT4:     __asm__ __volatile__ ("xxspltib  %x0, %1 " : "=wa"(vec_xt) : "i"(SPLAT4)); break;

      default:
         printf("Unhandled splat value for %s %d\n", __FUNCTION__, x_splat);
   }
};

static test_list_t testgroup_vector_immediate[] = {
   { &test_xxspltib, "xxspltib" },
   { NULL          , NULL       },
};

/* vector reverse bytes ... */
static void test_xxbrh(void)
{ /* vector reverse byte halfword*/
   __asm__ __volatile__ ("xxbrh %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xxbrw(void)
{ /* vector reverse byte word*/
   __asm__ __volatile__ ("xxbrw %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xxbrd(void)
{ /* vector reverse byte double*/
   __asm__ __volatile__ ("xxbrd %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xxbrq(void)
{ /* vector reverse byte */
   __asm__ __volatile__ ("xxbrq %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xvxexpdp(void) {
   __asm__ __volatile__ ("xvxexpdp %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xvxexpsp(void) {
   __asm__ __volatile__ ("xvxexpsp %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xvxsigdp(void) {
   __asm__ __volatile__ ("xvxsigdp %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static void test_xvxsigsp(void) {
   __asm__ __volatile__ ("xvxsigsp %x0, %x1 " : "=wa" (vec_xt) : "wa" (vec_xa));
}

static test_list_t testgroup_vector_logical_one[] = {
   { &test_xxbrh   , "xxbrh"    },
   { &test_xxbrw   , "xxbrw"    },
   { &test_xxbrd   , "xxbrd"    },
   { &test_xxbrq   , "xxbrq"    },
   { &test_xvxexpdp, "xvxexpdp" },
   { &test_xvxexpsp, "xvxexpsp" },
   { &test_xvxsigdp, "xvxsigdp" },
   { &test_xvxsigsp, "xvxsigsp" },
   { NULL          , NULL       },
};

static void test_xsxexpdp(void) {
   __asm__ __volatile__ ("xsxexpdp %0, %x1 " : "=r" (r17) : "wa" (vec_xa));
}

static void test_xsxsigdp(void) {
   __asm__ __volatile__ ("xsxsigdp %0, %x1 " : "=r" (r17) : "wa" (vec_xa));
}

static test_list_t testgroup_gpr_vector_logical_one[] = {
   { &test_xsxexpdp, "xsxexpdp" },
   { &test_xsxsigdp, "xsxsigdp" },
   { NULL          , NULL       },
};

static void test_lxvx(void)     {
   __asm__ __volatile__ ("lxvx %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_lxvwsx(void)   {
   __asm__ __volatile__ ("lxvwsx %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_lxvh8x(void)   {
   __asm__ __volatile__ ("lxvh8x %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_lxvb16x(void)  {
   __asm__ __volatile__ ("lxvb16x %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_stxvx(void)    {
  __asm__ __volatile__ ("stxvx %x0, 14, 15" :: "wa" (vec_xt));
}

static void test_stxvh8x(void)  {
  __asm__ __volatile__ ("stxvh8x %x0, 14, 15" :: "wa" (vec_xt));
}

static void test_stxvb16x(void) {
  __asm__ __volatile__ ("stxvb16x %x0, 14, 15" :: "wa" (vec_xt));
}

static test_list_t testgroup_vector_loadstore[] = {
   { &test_lxvx    , "lxvx"     },
   { &test_lxvwsx  , "lxvwsx"   },
   { &test_lxvh8x  , "lxvh8x"   },
   { &test_lxvb16x , "lxvb16x"  },
   { &test_stxvx   , "stxvx"    },
   { &test_stxvh8x , "stxvh8x"  },
   { &test_stxvb16x, "stxvb16x" },
   { NULL          , NULL       },
};

static void test_lxvl(void) {
   __asm__ __volatile__ ("lxvl %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_stxvl(void) {
  __asm__ __volatile__ ("stxvl %x0, 14, 15" :: "wa" (vec_xt));
}

static void test_lxvll(void) {
  __asm__ __volatile__ ("lxvll %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_stxvll(void) {
  __asm__ __volatile__ ("stxvll %x0, 14, 15" :: "wa" (vec_xt));
}

static void test_lxsibzx(void) {
   __asm__ __volatile__ ("lxsibzx %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_lxsihzx(void) {
   __asm__ __volatile__ ("lxsihzx %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_stxsibx(void) {
  __asm__ __volatile__ ("stxsibx %x0, 14, 15" :: "wa" (vec_xt));
}

static void test_stxsihx(void) {
  __asm__ __volatile__ ("stxsihx %x0, 14, 15" :: "wa" (vec_xt));
}

/* d-form vsx load/store */
static void test_lxsd_0(void) {
   __asm__ __volatile__ ("lxsd %0, 0(%1) " : "=v"(vec_xt) : "r"(r14));
}

static void test_stxsd_0(void) {
  __asm__ __volatile__ ("stxsd %0, 0(%1)" : : "v"(vec_xt), "r"(r14));
}

static void test_lxsd_16(void) {
   __asm__ __volatile__ ("lxsd %0, 16(%1)" : "=v"(vec_xt) : "r"(r14));
}

static void test_stxsd_16(void) {
   __asm__ __volatile__ ("stxsd %0, 16(%1)" : : "v"(vec_xt), "r"(r14));
}

static void test_lxssp_0(void) {
   __asm__ __volatile__ ("lxssp %0, 0(%1)" : "=v"(vec_xt) : "r"(r14));
}

static void test_stxssp_0(void) {
  __asm__ __volatile__ ("stxssp %0, 0(%1)" : : "v"(vec_xt), "r"(r14));
}

static void test_lxssp_16(void) {
   __asm__ __volatile__ ("lxssp %0, 16(%1)" : "=v"(vec_xt) : "r"(r14));
}

static void test_stxssp_16(void) {
  __asm__ __volatile__ ("stxssp %0, 16(%1)" : : "v"(vec_xt), "r"(r14));
}

static void test_lxv_0(void) {
   __asm__ __volatile__ ("lxv %x0, 0(%1)" : "=wa"(vec_xt) : "r"(r14));
}

static void test_stxv_0(void) {
   __asm__ __volatile__ ("stxv %x0, 0(%1)" : : "wa"(vec_xt), "r"(r14));
}

static void test_lxv_16(void) {
   __asm__ __volatile__ ("lxv %x0, 16(%1)" : "=wa"(vec_xt) : "r"(r14));
}

static void test_stxv_16(void) {
   __asm__ __volatile__ ("stxv %x0, 16(%1)" : : "wa"(vec_xt), "r"(r14));
}

static test_list_t testgroup_vector_scalar_loadstore_length[] = {
   { &test_lxvl     , "lxvl     " },
   { &test_lxvll    , "lxvll    " },
   { &test_lxsibzx  , "lxsibzx  " },
   { &test_lxsihzx  , "lxsihzx  " },
   { &test_stxvl    , "stxvl    " },
   { &test_stxvll   , "stxvll   " },
   { &test_stxsibx  , "stxsibx  " },
   { &test_stxsihx  , "stxsihx  " },
   { &test_lxsd_0   , "lxsd 0   " },
   { &test_stxsd_0  , "stxsd 0  " },
   { &test_lxsd_16  , "lxsd 16  " },
   { &test_stxsd_16 , "stxsd 16 " },
   { &test_lxssp_0  , "lxssp 0  " },
   { &test_stxssp_0 , "stxssp 0 " },
   { &test_lxssp_16 , "lxssp 16 " },
   { &test_stxssp_16, "stxssp 16" },
   { &test_lxv_0    , "lxv 0    " },
   { &test_stxv_0   , "stxv 0   " },
   { &test_lxv_16   , "lxv 16   " },
   { &test_stxv_16  , "stxv 16  " },
   { NULL           , NULL        },
};

/* move from/to VSR */
static void test_mfvsrld (void)
{
   __asm__ __volatile__ ("mfvsrld %0, %x1" : "=r" (r14) : "wa" (vec_xt));
};

static void test_mtvsrdd (void)
{
   __asm__ __volatile__ ("mtvsrdd %x0, 14, 15" : "=wa" (vec_xt));
};

static void test_mtvsrws (void)
{ /* To fit in better with the caller for the mfvsrdd test, use r15
   * instead of r14 as input here.
   */
   __asm__ __volatile__ ("mtvsrws %x0, 15" : "=wa" (vec_xt));
};

static test_list_t testgroup_vectorscalar_move_tofrom[] = {
   { &test_mfvsrld, "mfvsrld" }, /* RA, XS */
   { &test_mtvsrdd, "mtvsrdd" }, /* XT, RA, RB */
   { &test_mtvsrws, "mtvsrws" }, /* XT, RA */
   { NULL         , NULL      },
};

/* vector count {leading, trailing} zero least-significant bits byte.
 * roughly...  how many leading/trailing bytes are even. */
static void test_vclzlsbb(void) {
   __asm__ __volatile__ ("vclzlsbb %0, %1" : "=r"(r14) : "v"(vec_xb));
}

static void test_vctzlsbb(void) {
   __asm__ __volatile__ ("vctzlsbb %0, %1" : "=r"(r14) : "v"(vec_xb));
}

static test_list_t testgroup_vector_count_bytes[] = {
   { &test_vclzlsbb, "vclzlsbb" },
   { &test_vctzlsbb, "vctzlsbb" },
   { NULL          , NULL       },
};

static void test_vextsb2w(void) {
   __asm__ __volatile__ ("vextsb2w %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vextsb2d(void) {
   __asm__ __volatile__ ("vextsb2d %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vextsh2w(void) {
   __asm__ __volatile__ ("vextsh2w %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vextsh2d(void) {
   __asm__ __volatile__ ("vextsh2d %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vextsw2d(void) {
   __asm__ __volatile__ ("vextsw2d %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vnegw(void) {
   __asm__ __volatile__ ("vnegw %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vnegd(void) {
   __asm__ __volatile__ ("vnegd %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vprtybw(void) {
   __asm__ __volatile__ ("vprtybw %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vprtybd(void) {
   __asm__ __volatile__ ("vprtybd %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vprtybq(void) {
   __asm__ __volatile__ ("vprtybq %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vctzb(void) {
   __asm__ __volatile__ ("vctzb %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vctzh(void) {
   __asm__ __volatile__ ("vctzh %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vctzw(void) {
   __asm__ __volatile__ ("vctzw %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vctzd(void) {
   __asm__ __volatile__ ("vctzd %0, %1" : "=v"(vec_xt) : "v"(vec_xb));
}

static test_list_t testgroup_vector_extend_sign[] = {
   { &test_vextsb2w, "vextsb2w" },
   { &test_vextsb2d, "vextsb2d" },
   { &test_vextsh2w, "vextsh2w" },
   { &test_vextsh2d, "vextsh2d" },
   { &test_vextsw2d, "vextsw2d" },
   { &test_vnegw   , "vnegw   " },
   { &test_vnegd   , "vnegd   " },
   { &test_vprtybw , "vprtybw " },
   { &test_vprtybd , "vprtybd " },
   { &test_vprtybq , "vprtybq " },
   { &test_vctzb   , "vctzb   " },
   { &test_vctzh   , "vctzh   " },
   { &test_vctzw   , "vctzw   " },
   { &test_vctzd   , "vctzd   " },
   { NULL          , NULL       },
};

static void test_vextublx(void) {
   __asm__ __volatile__ ("vextublx %0, %1, %2" : "=r"(r14) : "r" (r15), "v" (vec_xb));
}

static void test_vextubrx(void) {
   __asm__ __volatile__ ("vextubrx %0, %1, %2" : "=r"(r14) : "r"(r15), "v"(vec_xb));
}

static void test_vextuhlx(void) {
   if (r15 > 14) return; // limit to 14 halfwords
   __asm__ __volatile__ ("vextuhlx %0, %1, %2" : "=r"(r14) : "r"(r15), "v"(vec_xb));
}

static void test_vextuhrx(void) {
   if (r15 > 14) return; // limit to 14 halfwords
   __asm__ __volatile__ ("vextuhrx %0, %1, %2" : "=r"(r14) : "r"(r15), "v"(vec_xb));
}

static void test_vextuwlx(void) {
   if (r15 > 12) return; // limit to 12 words
   __asm__ __volatile__ ("vextuwlx %0, %1, %2" : "=r"(r14) : "r"(r15), "v"(vec_xb));
}

static void test_vextuwrx(void) {
   if (r15 > 12) return; // limit to 12 words
   __asm__ __volatile__ ("vextuwrx %0, %1, %2" : "=r"(r14) : "r"(r15), "v"(vec_xb));
}

static test_list_t testgroup_vector_extract[] = {
   { &test_vextublx, "vextublx" },
   { &test_vextubrx, "vextubrx" },
   { &test_vextuhlx, "vextuhlx" },
   { &test_vextuhrx, "vextuhrx" },
   { &test_vextuwlx, "vextuwlx" },
   { &test_vextuwrx, "vextuwrx" },
   { NULL          , NULL       },
};

#define XSCMPEXPDP(x)                                                 \
   SET_FPSCR_ZERO                                                     \
   SET_CR_ZERO                                                        \
   __asm__ __volatile__                                               \
      ("xscmpexpdp %0, %x1, %x2"::"i"(x), "wa"(vec_xa), "wa"(vec_xb));\
   GET_CR(local_cr);                                                  \
   GET_FPSCR(local_fpscr);

static void test_xscmpexpdp(void) {
   switch(x_index) {
   case 0: XSCMPEXPDP(0); break;
   case 1: XSCMPEXPDP(1); break;
   case 2: XSCMPEXPDP(2); break;
   case 3: XSCMPEXPDP(3); break;
   case 4: XSCMPEXPDP(4); break;
   case 5: XSCMPEXPDP(5); break;
   case 6: XSCMPEXPDP(6); break;
   case 7: XSCMPEXPDP(7); break;
   default:
      printf("Unhandled shift value for %s %x\n", __FUNCTION__, x_index);
   };
}

static void test_xscmpeqdp(void) {
   __asm__ __volatile__ ("xscmpeqdp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static void test_xscmpgtdp(void) {
   __asm__ __volatile__ ("xscmpgtdp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static void test_xscmpgedp(void) {
   __asm__ __volatile__ ("xscmpgedp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static void test_xsmincdp(void) {
   __asm__ __volatile__ ("xsmincdp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static void test_xsmaxcdp(void) {
   __asm__ __volatile__ ("xsmaxcdp   %x0, %x1, %x2 " : "+wa" (vec_xt): "wa" (vec_xa), "wa" (vec_xb));
}

static test_list_t testgroup_vector_scalar_compare_double[] = {
   { &test_xscmpexpdp , "xscmpexpdp " },
   { &test_xscmpeqdp , "xscmpeqdp " },
   { &test_xscmpgtdp , "xscmpgtdp " },
   { &test_xscmpgedp , "xscmpgedp " },
   { &test_xsmincdp , "xsmincdp " },
   { &test_xsmaxcdp , "xsmaxcdp " },
   { NULL             , NULL          },
};

#define XSTSTDCQP(R,DCMX)                                                \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO                                                           \
   __asm__ __volatile__                                                  \
      ("xststdcqp %0, %1, %2":: "i"(R), "v"(vec_xb), "i"(DCMX));         \
   GET_CR(local_cr);                                                     \
   GET_FPSCR(local_fpscr);                                               \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO

#define XSTSTDCDP(R,DCMX)                                                \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO                                                           \
   __asm__ __volatile__                                                  \
      ("xststdcdp %0, %x1, %2":: "i"(R), "wa"(vec_xb), "i"(DCMX));       \
   GET_CR(local_cr);                                                     \
   GET_FPSCR(local_fpscr);                                               \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO

#define XSTSTDCSP(R,DCMX)                                                \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO                                                           \
   __asm__ __volatile__                                                  \
      ("xststdcsp %0, %x1, %2":: "i"(R), "wa"(vec_xb), "i"(DCMX));       \
   GET_CR(local_cr);                                                     \
   GET_FPSCR(local_fpscr);                                               \
   SET_CR_ZERO

#define XVTSTDCDP(R,DCMX)                                                \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO                                                           \
   __asm__ __volatile__                                                  \
      ("xvtstdcdp %x0, %x1, %2": "=wa"(vec_xt) : "wa"(vec_xb), "i"(DCMX)); \
   GET_CR(local_cr);                                                     \
   GET_FPSCR(local_fpscr);                                               \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO

#define XVTSTDCSP(R,DCMX)                                                \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO                                                           \
   __asm__ __volatile__                                                  \
      ("xvtstdcsp %x0, %x1, %2": "=wa"(vec_xt) : "wa"(vec_xb), "i"(DCMX)); \
   GET_CR(local_cr);                                                     \
   GET_FPSCR(local_fpscr);                                               \
   SET_FPSCR_ZERO                                                        \
   SET_CR_ZERO

static void test_xststdcqp(void) {
   switch(x_index) {
   case 1: XSTSTDCQP(3, 0x01); break; /* NaN */
   case 2: XSTSTDCQP(3, 0x02); break; /* +inf */
   case 3: XSTSTDCQP(3, 0x04); break; /* -inf */
   case 4: XSTSTDCQP(3, 0x08); break; /* +zero */
   case 5: XSTSTDCQP(3, 0x10); break; /* -zero */
   case 6: XSTSTDCQP(3, 0x20); break; /* +denormal */
   case 7: XSTSTDCQP(3, 0x40); break; /* -denormal */
   case 0: XSTSTDCQP(3, 0x7f); break; /* all of the above */
   }
}

static void test_xststdcdp(void) {
   switch(x_index) {
   case 1: XSTSTDCDP(3, 0x01); break; /* NaN */
   case 2: XSTSTDCDP(3, 0x02); break; /* +inf */
   case 3: XSTSTDCDP(3, 0x04); break; /* -inf */
   case 4: XSTSTDCDP(3, 0x08); break; /* +zero */
   case 5: XSTSTDCDP(3, 0x10); break; /* -zero */
   case 6: XSTSTDCDP(3, 0x20); break; /* +denormal */
   case 7: XSTSTDCDP(3, 0x40); break; /* -denormal */
   case 0: XSTSTDCDP(3, 0x7f); break; /* all of the above */
   }
}

static void test_xststdcsp(void) {
   switch(x_index) {
   case 1: XSTSTDCSP(3, 0x01); break; /* NaN */
   case 2: XSTSTDCSP(3, 0x02); break; /* +inf */
   case 3: XSTSTDCSP(3, 0x04); break; /* -inf */
   case 4: XSTSTDCSP(3, 0x08); break; /* +zero */
   case 5: XSTSTDCSP(3, 0x10); break; /* -zero */
   case 6: XSTSTDCSP(3, 0x20); break; /* +denormal */
   case 7: XSTSTDCSP(3, 0x40); break; /* -denormal */
   case 0: XSTSTDCSP(3, 0x7f); break; /* all of the above */
   }
}

static void test_xvtstdcdp(void) {
   switch(x_index) {
   case 1: XVTSTDCDP(3, 0x01); break; /* NaN */
   case 2: XVTSTDCDP(3, 0x02); break; /* +inf */
   case 3: XVTSTDCDP(3, 0x04); break; /* -inf */
   case 4: XVTSTDCDP(3, 0x08); break; /* +zero */
   case 5: XVTSTDCDP(3, 0x10); break; /* -zero */
   case 6: XVTSTDCDP(3, 0x20); break; /* +denormal */
   case 7: XVTSTDCDP(3, 0x40); break; /* -denormal */
   case 0: XVTSTDCDP(3, 0x7f); break; /* all of the above */
   }
}

/* Note: Due to the test groupings, the input for the xvtstdcsp test is
 * actually 'double'.  It is good enough for this test, but may wish to break
 * this one out eventually.
 */
static void test_xvtstdcsp(void) {
   switch(x_index) {
   case 1: XVTSTDCSP(3, 0x01); break; /* NaN */
   case 2: XVTSTDCSP(3, 0x02); break; /* +inf */
   case 3: XVTSTDCSP(3, 0x04); break; /* -inf */
   case 4: XVTSTDCSP(3, 0x08); break; /* +zero */
   case 5: XVTSTDCSP(3, 0x10); break; /* -zero */
   case 6: XVTSTDCSP(3, 0x20); break; /* +denormal */
   case 7: XVTSTDCSP(3, 0x40); break; /* -denormal */
   case 0: XVTSTDCSP(3, 0x7f); break; /* all of the above */
   }
}

static test_list_t testgroup_vector_scalar_data_class[] = {
   { &test_xststdcqp, "xststdcqp " },
   { &test_xststdcdp, "xststdcdp " },
   { &test_xststdcsp, "xststdcsp " },
   { &test_xvtstdcsp, "xvtstdcsp " },
   { &test_xvtstdcdp, "xvtstdcdp " },
   { NULL           , NULL         },
};

static void test_setb (void) {
   /* setb RT,BFA
    * BFA is a 3-bit field. */

   switch(x_index) {
   case 0:SET_CR0_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 0" : "=r" (r14)); break;

   case 1:SET_CR1_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 1" : "=r" (r14)); break;

   case 2:SET_CR2_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 2" : "=r" (r14)); break;

   case 3:SET_CR3_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 3" : "=r" (r14)); break;

   case 4:SET_CR4_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 4" : "=r" (r14)); break;

   case 5:SET_CR5_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 5" : "=r" (r14)); break;

   case 6:SET_CR6_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 6" : "=r" (r14)); break;

   case 7:SET_CR7_FIELD(cr_value);
      __asm__ __volatile__ ("setb %0, 7" : "=r" (r14)); break;
    }

    GET_CR(local_cr);

    if (verbose) {
       dissect_cr(local_cr);
    }
}

static test_list_t testgroup_set_boolean[] = {
   { &test_setb, "setb"},
   { NULL      , NULL  },
};

/* cmprb l = 0:
 * compares r14 = src with range specified by values in r15
 *      bits (48:55 - 56:63)].
 *
 *  cmprb l=1:
 *  compares r14 = src with ranges between two pairs of values, as above and
 *  also in r15 bits (32:39 - 40:47 .
 */
static void test_cmprb_l0() {
   /* Clear condition code reg (CR) immediately before test
    * instruction, read CR and clear immediately after test
    * instruction.  Otherwise, the CR gets corrupted and depending
    * on optimization level, strange loop control flow issues
    * occur because CR has been messed with.
    */
   switch(x_index) {
   case 0: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 0, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 1: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 1, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 2: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 2, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 3: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 3, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 4: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 4, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 5: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 5, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 6: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 6, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 7: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 7, 0, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;
    }
}

static void test_cmprb_l1() {
   /* Clear condition code reg (CR) immediately before test
    * instruction, read CR and clear immediately after test
    * instruction.  Otherwise, the CR gets corrupted and depending
    * on optimization level, strange loop control flow issues
    * occur because CR has been messed with.
    */
   switch(x_index) {
   case 0: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 0, 1 ,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 1: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 1, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 2: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 2, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 3: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 3, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 4: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 4, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 5: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 5, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 6: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 6, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 7: SET_CR_ZERO;
      __asm__ __volatile__ ("cmprb 7, 1, %0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;
    }
}

static void test_cmpeqb() {
   /* Clear condition code reg (CR) immediately before test
    * instruction, read CR and clear immediately after test
    * instruction.  Otherwise, the CR gets corrupted and depending
    * on optimization level, strange loop control flow issues
    * occur because CR has been messed with.
    */
   switch(x_index) {
   case 0: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 0,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 1: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 1,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 2: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 2,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 3: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 3,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 4: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 4,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 5: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 5,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 6: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 6,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;

   case 7: SET_CR_ZERO;
      __asm__ __volatile__ ("cmpeqb 7,%0, %1" : : "r"(r14), "r"(r15));
      GET_CR(local_cr); SET_CR_ZERO; break;
    }
}

static test_list_t testgroup_char_compare[] = {
   { &test_cmprb_l0, "cmprb l=0" },
   { &test_cmprb_l1, "cmprb l=1" },
   { &test_cmpeqb  , "cmpeqb"    },
   { NULL          , NULL        },
};

static void test_bcdtrunc_p0(void) {
   __asm__ __volatile__ ("bcdtrunc.  %0, %1, %2, 0": "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdtrunc_p1(void) {
   __asm__ __volatile__ ("bcdtrunc.  %0, %1, %2, 1": "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdutrunc(void) {
   __asm__ __volatile__ ("bcdutrunc. %0, %1, %2  ": "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdadd_p0(void) {
   __asm__ __volatile__ ("bcdadd.   %0, %1, %2, 0" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdadd_p1(void) {
   __asm__ __volatile__ ("bcdadd.   %0, %1, %2, 1" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdsub_p0(void) {
   __asm__ __volatile__ ("bcdsub.   %0, %1, %2, 0" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdsub_p1(void) {
   __asm__ __volatile__ ("bcdsub.   %0, %1, %2, 1" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdcpsgn(void)  {
   __asm__ __volatile__ ("bcdcpsgn.  %0, %1, %2  ": "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdctz_p0(void) {
   __asm__ __volatile__ ("bcdctz.   %0, %1, 0   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdctz_p1(void) {
   __asm__ __volatile__ ("bcdctz.   %0, %1, 1   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdsetsgn_p0(void) {
   __asm__ __volatile__ ("bcdsetsgn. %0, %1, 0   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdsetsgn_p1(void) {
   __asm__ __volatile__ ("bcdsetsgn. %0, %1, 1   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcds_p0(void) {
   __asm__ __volatile__ ("bcds.     %0, %1, %2, 0" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcds_p1(void) {
   __asm__ __volatile__ ("bcds.     %0, %1, %2, 1" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdus(void) {
   __asm__ __volatile__ ("bcdus.    %0, %1, %2  " : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdsr_p0(void) {
   __asm__ __volatile__ ("bcdsr.    %0, %1, %2, 0" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdsr_p1(void) {
   __asm__ __volatile__ ("bcdsr.    %0, %1, %2, 1" : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdcfn_p0(void) {
   __asm__ __volatile__ ("bcdcfn.   %0, %1, 0   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdcfn_p1(void) {
   __asm__ __volatile__ ("bcdcfn.   %0, %1, 1   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdcfz_p0(void) {
   __asm__ __volatile__ ("bcdcfz.   %0, %1, 0   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdcfz_p1(void) {
   __asm__ __volatile__ ("bcdcfz.   %0, %1, 1   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdctn(void) {
   __asm__ __volatile__ ("bcdctn.   %0, %1     " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_vmul10uq(void) {
   __asm__ __volatile__ ("vmul10uq   %0, %1     " : "=v"(vec_xt) : "v"(vec_xa));
}

static void test_vmul10cuq(void) {
   __asm__ __volatile__ ("vmul10cuq  %0, %1     " : "=v"(vec_xt) : "v"(vec_xa));
}

static void test_vmul10euq(void) {
   __asm__ __volatile__ ("vmul10euq  %0, %1, %2  " : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_vmul10ecuq(void) {
   __asm__ __volatile__ ("vmul10ecuq %0, %1, %2  " : "=v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_bcdctsq(void) {
   __asm__ __volatile__ ("bcdctsq.   %0, %1     " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdcfsq_p0(void) {
   __asm__ __volatile__ ("bcdcfsq.   %0, %1, 0   " : "=v"(vec_xt) : "v"(vec_xb));
}

static void test_bcdcfsq_p1(void) {
   __asm__ __volatile__ ("bcdcfsq.   %0, %1, 1   " : "=v"(vec_xt) : "v"(vec_xb));
}

static test_list_t testgroup_bcd_misc[] = {
   { &test_bcdadd_p0   , "bcdadd. p0"    },
   { &test_bcdadd_p1   , "bcdadd. p1"    },
   { &test_bcdsub_p0   , "bcdsub. p0"    },
   { &test_bcdsub_p1   , "bcdsub. p1"    },
   { &test_bcdcfn_p0   , "bcdcfn. p0"    },
   { &test_bcdcfn_p1   , "bcdcfn. p1"    },
   { &test_bcdcfz_p0   , "bcdcfz. p0"    }, /* The p0, p1 substrings are used later */
   { &test_bcdcfz_p1   , "bcdcfz. p1"    }, /* " " */
   { &test_bcdctn      , "bcdctn.   "    },
   { &test_bcdctz_p0   , "bcdctz. p0"    }, /* note: p0, p1 substrings are used later */
   { &test_bcdctz_p1   , "bcdctz. p1"    }, /* " " */
   { &test_bcdcpsgn    , "bcdcpsgn."     },
   { &test_bcdsetsgn_p0, "bcdsetsgn. p0" },
   { &test_bcdsetsgn_p1, "bcdsetsgn. p1" },
   { &test_bcds_p0     , "bcds. p0"      },
   { &test_bcds_p1     , "bcds. p1"      },
   { &test_bcdus       , "bcdus. "       },
   { &test_bcdsr_p0    , "bcdsr. p0"     },
   { &test_bcdsr_p1    , "bcdsr. p1"     },
   { &test_bcdtrunc_p0 , "bcdtrunc. p0"  },
   { &test_bcdtrunc_p1 , "bcdtrunc. p1"  },
   { &test_bcdutrunc   , "bcdutrunc. "   },
   { &test_vmul10uq    , "vmul10uq "     },
   { &test_vmul10cuq   , "vmul10cuq "    },
   { &test_vmul10euq   , "vmul10euq "    },
   { &test_vmul10ecuq  , "vmul10ecuq "   },
   { &test_bcdctsq     , "bcdctsq."      },
   { &test_bcdcfsq_p0  , "bcdcfsq. p0"   },
   { &test_bcdcfsq_p1  , "bcdcfsq. p1"   },
   { NULL              , NULL            },
};

static void test_wait(void) {
   __asm__ __volatile__ ("wait 0" : :);
}

static test_list_t testgroup_noop_misc[] = {
   { &test_wait, "wait ",},
   { NULL      , NULL,   },
};

/* The significance field can be any values within bits 10-15 of the
 * instruction.  For this test, limiting the values to one per bit location.
 */
static void test_dtstsfi() {
   _Decimal128 df14 = dfp_value.dec_val128;
   switch(dfp_significance) {
   case 0x00: __asm__ __volatile__ ("dtstsfi 3, 0x00, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x01: __asm__ __volatile__ ("dtstsfi 3, 0x01, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x02: __asm__ __volatile__ ("dtstsfi 3, 0x02, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x03: __asm__ __volatile__ ("dtstsfi 3, 0x03, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x04: __asm__ __volatile__ ("dtstsfi 3, 0x04, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x06: __asm__ __volatile__ ("dtstsfi 3, 0x06, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x08: __asm__ __volatile__ ("dtstsfi 3, 0x08, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x0c: __asm__ __volatile__ ("dtstsfi 3, 0x0c, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x10: __asm__ __volatile__ ("dtstsfi 3, 0x10, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x18: __asm__ __volatile__ ("dtstsfi 3, 0x18, %0" : : "f" (df14));
      GET_CR(local_cr); break;

   case 0x20: __asm__ __volatile__ ("dtstsfi 3, 0x20, %0" : : "f" (df14));
      GET_CR(local_cr); break;
   }
}

static void test_dtstsfiq() {
   _Decimal128 df14 = dfp_value.dec_val128;
   switch(dfp_significance) {
   case 0x00: SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x00, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x01:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x01, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x02:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x02, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x03:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x03, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x04:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x04, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x06:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x06, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x08:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x08, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x0c:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x0c, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x10:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x10, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x18:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x18, %0" : : "d" (df14));
      GET_CR(local_cr); break;

   case 0x20:  SET_CR_ZERO;
      __asm__ __volatile__ ("dtstsfiq 3, 0x20, %0" : : "d" (df14));
      GET_CR(local_cr); break;
   }
}

static test_list_t testgroup_dfp_significance[] = {
   { &test_dtstsfi , "dtstsfi"  },
   { &test_dtstsfiq, "dtstsfiq" },
   { NULL          , NULL       },
};

static void test_addpcis(void)  {
   switch (x_index) {
   case 0x0: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x0) ); break;
   case 0x1: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x1) ); break;
   case 0x2: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x2) ); break;
   case 0x3: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x40) ); break;
   case 0x4: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x800) ); break;
   case 0x5: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x2000) ); break;
   case 0x6: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x7fff) ); break;
   case 0x7: __asm__ __volatile__ ("addpcis 14, %0"  : : "i"(0x8000) ); break;
   case 0x8: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x0) ); break;
   case 0x9: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x1) ); break;
   case 0xa: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x2) ); break;
   case 0xb: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x40) ); break;
   case 0xc: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x800) ); break;
   case 0xd: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x2000) ); break;
   case 0xe: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x7fff) ); break;
   case 0xf: __asm__ __volatile__ ("addpcis 14, -%0" : : "i"(0x8000) ); break;
   }
}

static void test_subpcis(void)  {
   switch (x_index) {
   case 0x0:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x0) ); break;
   case 0x1:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x1) ); break;
   case 0x2:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x2) ); break;
   case 0x3:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x40) ); break;
   case 0x4:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x800) ); break;
   case 0x5:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x2000) ); break;
   case 0x6:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x7fff) ); break;
   case 0x7:  __asm__ __volatile__ ("subpcis 14, %0"  : : "i"(0x8000) ); break;
   case 0x8:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x0) ); break;
   case 0x9:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x1) ); break;
   case 0xa:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x2) ); break;
   case 0xb:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x40) ); break;
   case 0xc:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x80) ); break;
   case 0xd:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x200) ); break;
   case 0xe:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x7fff) ); break;
   case 0xf:  __asm__ __volatile__ ("subpcis 14, -%0" : : "i"(0x8000) ); break;
   }
}

static test_list_t testgroup_pc_immediate_misc[] = {
   { &test_addpcis, "addpcis " },
   { &test_subpcis, "subpcis " },
   { NULL         , NULL       },
};

static void test_xsiexpdp(void) {
   __asm__ __volatile__ ("xsiexpdp   %x0, %1, %2 " : "+wa" (vec_xt): "r" (r14), "r" (r15));
}

static void test_xscvhpdp(void) {
   __asm__ __volatile__ ("xscvhpdp %x0, %x1 " : "+wa" (vec_xt) : "wa" (vec_xb));
}

static void test_xscvdphp(void) {
   __asm__ __volatile__ ("xscvdphp %x0, %x1 " : "+wa" (vec_xt) : "wa" (vec_xb));
}

static void test_xvcvhpsp(void) {
   __asm__ __volatile__ ("xvcvhpsp %x0, %x1 " : "+wa" (vec_xt) : "wa" (vec_xb));
}

static void test_xvcvsphp(void) {
   __asm__ __volatile__ ("xvcvsphp %x0, %x1 " : "+wa" (vec_xt) : "wa" (vec_xb));
}

static test_list_t testgroup_vector_scalar_two_double[] = {
   { &test_xsiexpdp, "xsiexpdp" },
   { &test_xscvhpdp, "xscvhpdp" },
   { &test_xscvdphp, "xscvdphp" },
   { &test_xvcvhpsp, "xvcvhpsp" },
   { &test_xvcvsphp, "xvcvsphp" },
   { NULL          , NULL       },
};


static void test_xsabsqp(void) {
   __asm__ __volatile__ ("xsabsqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvdpqp(void) {
   __asm__ __volatile__ ("xscvdpqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvqpdp(void) {
   __asm__ __volatile__ ("xscvqpdp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvqpdpo(void) {
   __asm__ __volatile__ ("xscvqpdpo %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvqpsdz(void) {
   __asm__ __volatile__ ("xscvqpsdz %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvqpswz(void) {
   __asm__ __volatile__ ("xscvqpswz %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvqpudz(void) {
   __asm__ __volatile__ ("xscvqpudz %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvqpuwz(void) {
   __asm__ __volatile__ ("xscvqpuwz %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvsdqp(void) {
   __asm__ __volatile__ ("xscvsdqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xscvudqp(void) {
   __asm__ __volatile__ ("xscvudqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xsxexpqp(void) {
   __asm__ __volatile__ ("xsxexpqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xsxsigqp(void) {
   __asm__ __volatile__ ("xsxsigqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xsnegqp(void) {
   __asm__ __volatile__ ("xsnegqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xsnabsqp(void) {
   __asm__ __volatile__ ("xsnabsqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xssqrtqp(void) {
   __asm__ __volatile__ ("xssqrtqp %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static void test_xssqrtqpo(void) {
   __asm__ __volatile__ ("xssqrtqpo %0, %1" : "+v"(vec_xt) : "v"(vec_xb));
}

static test_list_t testgroup_vector_scalar_two_quad[] = {
   { &test_xsabsqp  , "xsabsqp "   },
   { &test_xscvdpqp , "xscvdpqp "  },
   { &test_xscvqpdp , "xscvqpdp "  },
   { &test_xscvqpdpo, "xscvqpdpo " },
   { &test_xscvqpsdz, "xscvqpsdz " },
   { &test_xscvqpswz, "xscvqpswz " },
   { &test_xscvqpudz, "xscvqpudz " },
   { &test_xscvqpuwz, "xscvqpuwz " },
   { &test_xscvsdqp , "xscvsdqp "  },
   { &test_xscvudqp , "xscvudqp "  },
   { &test_xsxexpqp , "xsxexpqp "  },
   { &test_xsxsigqp , "xsxsigqp "  },
   { &test_xsnegqp  , "xsnegqp "   },
   { &test_xsnabsqp , "xsnabsqp "  },
   { &test_xssqrtqp , "xssqrtqp "  },
   { &test_xssqrtqpo, "xssqrtqpo " },
   { NULL           , NULL         },
};

static void test_xsaddqp(void) {
   __asm__ __volatile__ ("xsaddqp    %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsaddqpo(void) {
   __asm__ __volatile__ ("xsaddqpo   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xscpsgnqp(void) {
   __asm__ __volatile__ ("xscpsgnqp  %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsdivqp(void) {
   __asm__ __volatile__ ("xsdivqp    %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsdivqpo(void) {
   __asm__ __volatile__ ("xsdivqpo   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsiexpqp(void) {
   __asm__ __volatile__ ("xsiexpqp   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsmaddqp(void) {
   __asm__ __volatile__ ("xsmaddqp   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsmaddqpo(void) {
   __asm__ __volatile__ ("xsmaddqpo  %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsmsubqp(void) {
   __asm__ __volatile__ ("xsmsubqp   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsmsubqpo(void) {
   __asm__ __volatile__ ("xsmsubqpo  %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsmulqp(void) {
   __asm__ __volatile__ ("xsmulqp   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsmulqpo(void) {
   __asm__ __volatile__ ("xsmulqpo  %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsnmaddqp(void) {
   __asm__ __volatile__ ("xsnmaddqp  %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsnmaddqpo(void) {
   __asm__ __volatile__ ("xsnmaddqpo %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsnmsubqp(void) {
   __asm__ __volatile__ ("xsnmsubqp  %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xsnmsubqpo(void) {
   __asm__ __volatile__ ("xsnmsubqpo %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xssubqp(void) {
   __asm__ __volatile__ ("xssubqp    %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static void test_xssubqpo(void) {
   __asm__ __volatile__ ("xssubqpo   %0, %1, %2" : "+v"(vec_xt) : "v"(vec_xa), "v"(vec_xb));
}

static test_list_t testgroup_vector_three_quad[] = {
   { &test_xsaddqp   , "xsaddqp "    },
   { &test_xsaddqpo  , "xsaddqpo "   },
   { &test_xscpsgnqp , "xscpsgnqp "  },
   { &test_xsdivqp   , "xsdivqp "    },
   { &test_xsdivqpo  , "xsdivqpo "   },
   { &test_xsiexpqp  , "xsiexpqp "   },
   { &test_xsmaddqp  , "xsmaddqp "   },
   { &test_xsmaddqpo , "xsmaddqpo "  },
   { &test_xsmsubqp  , "xsmsubqp "   },
   { &test_xsmsubqpo , "xsmsubqpo "  },
   { &test_xsmulqp   , "xsmulqp "    },
   { &test_xsmulqpo  , "xsmulqpo "   },
   { &test_xsnmaddqp , "xsnmaddqp "  },
   { &test_xsnmaddqpo, "xsnmaddqpo " },
   { &test_xsnmsubqp , "xsnmsubqp "  },
   { &test_xsnmsubqpo, "xsnmsubqpo " },
   { &test_xssubqp   , "xssubqp "    },
   { &test_xssubqpo  , "xssubqpo "   },
   { NULL            , NULL          },
};

#define XSCMPEXPQP(x)							\
   SET_FPSCR_ZERO                                                       \
   SET_CR_ZERO                                                          \
   __asm__ __volatile__                                                 \
      ("xscmpexpqp %0, %1, %2" :: "i"(x), "v"(vec_xa), "v"(vec_xb));    \
   GET_CR(local_cr);                                                    \
   GET_FPSCR(local_fpscr);

#define XSCMPOQP(x)                                                     \
   SET_FPSCR_ZERO                                                       \
   SET_CR_ZERO                                                          \
   __asm__ __volatile__                                                 \
      ("xscmpoqp %0, %1, %2" :: "i"(x), "v"(vec_xa), "v"(vec_xb));      \
   GET_CR(local_cr);                                                    \
   GET_FPSCR(local_fpscr);

#define XSCMPUQP(x)                                                     \
   SET_FPSCR_ZERO                                                       \
   SET_CR_ZERO                                                          \
   __asm__ __volatile__                                                 \
      ("xscmpuqp %0, %1, %2"::"i"(x), "v"(vec_xa), "v"(vec_xb));        \
   GET_CR(local_cr);                                                    \
   GET_FPSCR(local_fpscr);

static void test_xscmpexpqp(void) {
   switch(x_index) {
   case 0: XSCMPEXPQP(0); break;
   case 1: XSCMPEXPQP(1); break;
   case 2: XSCMPEXPQP(2); break;
   case 3: XSCMPEXPQP(3); break;
   case 4: XSCMPEXPQP(4); break;
   case 5: XSCMPEXPQP(5); break;
   case 6: XSCMPEXPQP(6); break;
   case 7: XSCMPEXPQP(7); break;
   default:
      printf("Unhandled shift value for %s %x\n", __FUNCTION__, x_index);
   };
}

static void test_xscmpoqp(void) {
   switch(x_index) {
   case 0: XSCMPOQP(0); break;
   case 1: XSCMPOQP(1); break;
   case 2: XSCMPOQP(2); break;
   case 3: XSCMPOQP(3); break;
   case 4: XSCMPOQP(4); break;
   case 5: XSCMPOQP(5); break;
   case 6: XSCMPOQP(6); break;
   case 7: XSCMPOQP(7); break;
   default:
      printf("Unhandled shift value for %s %x\n", __FUNCTION__, x_index);
   };
}

static void test_xscmpuqp(void) {
   switch(x_index) {
   case 0: XSCMPUQP(0); break;
   case 1: XSCMPUQP(1); break;
   case 2: XSCMPUQP(2); break;
   case 3: XSCMPUQP(3); break;
   case 4: XSCMPUQP(4); break;
   case 5: XSCMPUQP(5); break;
   case 6: XSCMPUQP(6); break;
   case 7: XSCMPUQP(7); break;
   default:
      printf("Unhandled shift value for %s %x\n", __FUNCTION__, x_index);
   };
}

static test_list_t testgroup_vector_scalar_compare_quads[] = {
   { &test_xscmpexpqp, "xscmpexpqp" },
   { &test_xscmpoqp  , "xscmpoqp  " },
   { &test_xscmpuqp  , "xscmpuqp  " },
   { NULL            , NULL         },
};

#define XSRQPI(R,RMC)                                                        \
   SET_FPSCR_ZERO                                                            \
   SET_CR_ZERO                                                               \
   __asm__ __volatile__                                                      \
      ("xsrqpi %1, %0, %2, %3" : "=v"(vec_xt) : "i"(R), "v"(vec_xb), "i"(RMC)); \
   GET_CR(local_cr);                                                         \
   GET_FPSCR(local_fpscr);

#define XSRQPIX(R,RMC)                                                       \
   SET_FPSCR_ZERO                                                            \
   SET_CR_ZERO                                                               \
   __asm__ __volatile__                                                      \
      ("xsrqpix %1, %0, %2, %3" : "=v"(vec_xt) : "i"(R), "v"(vec_xb), "i"(RMC));\
   GET_CR(local_cr);                                                         \
   GET_FPSCR(local_fpscr);

#define XSRQPXP(R,RMC)                                                       \
   SET_FPSCR_ZERO                                                            \
   SET_CR_ZERO                                                               \
   __asm__ __volatile__                                                      \
      ("xsrqpxp %1, %0, %2, %3" : "=v"(vec_xt) : "i"(R), "v"(vec_xb), "i"(RMC));\
   GET_CR(local_cr);                                                         \
   GET_FPSCR(local_fpscr);

/* For the scalar round to quad instructions, x_index is used to key into
 * two fields; x_index bit [2] becomes the one-bit 'R' and x_index bits [0, 1]
 * becomes the two-bit 'RMC'.
 */
static void test_xsrqpi(void) {
   switch(x_index) {
   case 0: XSRQPI(0, 0); break;
   case 1: XSRQPI(0, 1); break;
   case 2: XSRQPI(0, 2); break;
   case 3: XSRQPI(0, 3); break;
   case 4: XSRQPI(1, 0); break;
   case 5: XSRQPI(1, 1); break;
   case 6: XSRQPI(1, 2); break;
   case 7: XSRQPI(1, 3); break;
   }
}
static void test_xsrqpix(void) {
   switch(x_index) {
   case 0: XSRQPIX(0, 0); break;
   case 1: XSRQPIX(0, 1); break;
   case 2: XSRQPIX(0, 2); break;
   case 3: XSRQPIX(0, 3); break;
   case 4: XSRQPIX(1, 0); break;
   case 5: XSRQPIX(1, 1); break;
   case 6: XSRQPIX(1, 2); break;
   case 7: XSRQPIX(1, 3); break;
   }
}

static void test_xsrqpxp(void) {
   switch(x_index) {
   case 0: XSRQPXP(0, 0); break;
   case 1: XSRQPXP(0, 1); break;
   case 2: XSRQPXP(0, 2); break;
   case 3: XSRQPXP(0, 3); break;
   case 4: XSRQPXP(1, 0); break;
   case 5: XSRQPXP(1, 1); break;
   case 6: XSRQPXP(1, 2); break;
   case 7: XSRQPXP(1, 3); break;
   }
}

static test_list_t testgroup_vector_scalar_rounding_quads[] = {
   { &test_xsrqpi , "xsrqpi " },
   { &test_xsrqpix, "xsrqpix" },
   { &test_xsrqpxp, "xsrqpxp" },
   { NULL         , NULL      },
};

/* Move From FPSCR variants:
 * Move From FpScr [ &
 *                    Clear Enables |
 *                    Lightweight |
 *                    Control    [&
 *                                 set DRN [ Immediate] |
 *                                 set RN  [ Immediate ] ]]
 */
/* mffs FRT # Move From FPSCR*/
static void test_mffs (void) {
   SET_FPSCR_ZERO
   __asm__ __volatile__ ("mffs %0"  : "=f"(f14) );
   GET_FPSCR(local_fpscr);
   SET_FPSCR_ZERO
}

/* mffsce FRT # Move From FPSCR and Clear Enables */
static void test_mffsce (void) {
   SET_FPSCR_ZERO
   __asm__ __volatile__ ("mffsce %0"  : "=f"(f14) );
   GET_FPSCR(local_fpscr);
   SET_FPSCR_ZERO
}

/* mffscdrn FRT,FRB # Move From FpScr and Control &set DRN */
static void test_mffscdrn (void) {
   SET_FPSCR_ZERO
   __asm__ __volatile__ ("mffscdrn %0,%1"  : "=f"(f14): "f"(f15) );
   GET_FPSCR(local_fpscr);
   SET_FPSCR_ZERO
}

/* mffscdrni FRT,DRM # Move From FpScr & Control &set DRN Immediate*/
static void test_mffscdrni (void) {
   switch(x_shift) {
      default:
      case 0:
         SET_FPSCR_ZERO
         __asm__ __volatile__ ("mffscdrni %0,0"  : "=f"(f14) );
         GET_FPSCR(local_fpscr);
         SET_FPSCR_ZERO
         break;
      case 1:
         SET_FPSCR_ZERO
         __asm__ __volatile__ ("mffscdrni %0,1"  : "=f"(f14) );
         GET_FPSCR(local_fpscr);
         SET_FPSCR_ZERO
         break;
      case 2:
         SET_FPSCR_ZERO
         __asm__ __volatile__ ("mffscdrni %0,2"  : "=f"(f14) );
         GET_FPSCR(local_fpscr);
         SET_FPSCR_ZERO
         break;
   }
}

/* mffscrn FRT,FRB # Move From FpScr and Control &set RN*/
static void test_mffscrn (void) {
   SET_FPSCR_ZERO
   __asm__ __volatile__ ("mffscrn %0,%1"  : "=f"(f14):"f"(f15));
   GET_FPSCR(local_fpscr);
   SET_FPSCR_ZERO
}

/* mffscrni FRT,RM # Move from FpScr and Control &set RN Immediate*/
static void test_mffscrni (void) {
   switch(x_shift) {
      case 0:
         SET_FPSCR_ZERO
         __asm__ __volatile__ ("mffscrni %0,0"  : "=f"(f14) );
         GET_FPSCR(local_fpscr);
         SET_FPSCR_ZERO
         break;
      case 1:
         SET_FPSCR_ZERO
         __asm__ __volatile__ ("mffscrni %0,1"  : "=f"(f14) );
         GET_FPSCR(local_fpscr);
         SET_FPSCR_ZERO
         break;
      case 2:
         SET_FPSCR_ZERO
         __asm__ __volatile__ ("mffscrni %0,2"  : "=f"(f14) );
         GET_FPSCR(local_fpscr);
         SET_FPSCR_ZERO
         break;
   }
}

/* mffsl FRT  # Move From FpScr Lightweight */
static void test_mffsl (void) {
   SET_FPSCR_ZERO
   __asm__ __volatile__ ("mffsl %0"  : "=f"(f14) );
   GET_FPSCR(local_fpscr);
   SET_FPSCR_ZERO
}

/* mffs* instructions using FRT only. */
/* Note to self - Watch DRM,RM fields. */
static test_list_t testgroup_mffs_misc[] = {
   { &test_mffsce,    "mffsce" },
   { &test_mffsl,     "mffsl" },
   { &test_mffs,      "mffs" },
   { NULL               , NULL      },
};

/* mffs* instructions using FRT,FRB. */
static test_list_t testgroup_mffs_misc_one[] = {
   { &test_mffscdrni, "mffscdrni" },
   { &test_mffscdrn,  "mffscdrn" },
   { &test_mffscrni,  "mffscrni" },
   { &test_mffscrn,   "mffscrn" },
   { NULL               , NULL      },
};


/* ###### begin all_tests table.  */

/* table containing all of the instruction groups */
struct test_group_table_t {
   test_list_t *tests;
   const char *name;
   unsigned int flags;
};

typedef struct test_group_table_t test_group_table_t;

static test_group_table_t all_tests[] = {
   {
      testgroup_ia_ops_two,
      "PPC integer arith instructions with two args",
      PPC_INTEGER | PPC_ARITH | PPC_TWO_ARGS,
   },
   {
      testgroup_shifted_one,
      "ppc one argument plus shift",
      PPC_MISC | PPC_CR | PPC_TWO_ARGS,
   },
   {
      testgroup_three_args,
      "ppc three parameter ops",
      PPC_INTEGER | PPC_ARITH | PPC_THREE_ARGS,
   },
   {
      testgroup_logical_one,
      "ppc count zeros",
      PPC_INTEGER | PPC_LOGICAL | PPC_ONE_ARG,
   },
   {
      testgroup_set_boolean,
      "ppc set boolean",
      PPC_INTEGER | PPC_LOGICAL | PPC_ONE_IMM,
   },
   {
      testgroup_char_compare,
      "ppc char compare",
      PPC_INTEGER | PPC_COMPARE,
   },
   {
      testgroup_vsx_absolute,
      "ppc vector absolutes",
      PPC_ALTIVEC | PPC_ARITH | PPC_TWO_ARGS,
   },
   {
      testgroup_vector_immediate,
      "ppc vector logical immediate",
      PPC_ALTIVEC | PPC_LOGICAL | PPC_ONE_IMM,
   },
   {
      testgroup_vector_logical_one,
      "ppc vector logical one",
      PPC_ALTIVEC | PPC_LOGICAL | PPC_ONE_ARG,
   },
   {
      testgroup_gpr_vector_logical_one,
      "ppc gpr vector logical one",
      PPC_ALTIVEC | PPC_LOGICAL | PPC_ONE_GPR_ONE_VEC,
   },
   {
      testgroup_vector_extend_sign,
      "ppc vector extend sign",
      PPC_ALTIVEC | PPC_LOGICAL | PPC_TWO_ARGS,
   },
   {
      testgroup_vector_three_quad,
      "ppc vector three quad",
      PPC_ALTIVEC | PPC_LOGICAL | PPC_THREE_ARGS,
   },
   {
      testgroup_vector_scalar_two_quad,
      "ppc vector scalar quad",
      PPC_ALTIVEC_QUAD | PPC_LOGICAL | PPC_TWO_ARGS,
   },
   {
      testgroup_vector_scalar_compare_quads,
      "ppc vector scalar compare exponents quads",
      PPC_ALTIVEC_QUAD | PPC_COMPARE | PPC_COMPARE_ARGS,
   },
   {
      testgroup_vector_scalar_rounding_quads,
      "ppc vector scalar rounding quads",
      PPC_ALTIVEC_QUAD | PPC_ROUND,
   },
   {
      testgroup_vsx_xxpermute,
      "ppc vector permutes",
      PPC_ALTIVEC | PPC_PERMUTE,
   },
   {
      testgroup_vector_four,
      "ppc vector three args + dest",
      PPC_ALTIVEC | PPC_LOGICAL | PPC_FOUR_ARGS,
   },
   {
      testgroup_vector_inserts,
      "ppc vector inserts",
      PPC_ALTIVEC | PPC_INSERTEXTRACT | PPC_ONE_IMM,
   },
   {
      testgroup_vector_extract,
      "ppc vector extract from vector to reg",
      PPC_ALTIVEC | PPC_INSERTEXTRACT | PPC_TWO_ARGS,
   },
   {
      testgroup_vector_count_bytes,
      "ppc vector count leading/trailing bytes",
      PPC_ALTIVEC | PPC_POPCNT,
   },
   {
      testgroup_vector_scalar_loadstore_length,
      "ppc vector load/store",
      PPC_ALTIVEC | PPC_LDST | PPC_ONE_IMM,
   },
   {
      testgroup_vector_loadstore,
      "ppc vector load/store",
      PPC_ALTIVEC | PPC_LDST | PPC_TWO_ARGS,
   },
   {
      testgroup_vectorscalar_move_tofrom,
      "ppc vector scalar move to/from",
      PPC_MISC | PPC_TWO_ARGS,
   },
   {
      testgroup_vector_scalar_compare_double,
      "ppc vector scalar compare doubles",
      PPC_ALTIVEC_DOUBLE | PPC_COMPARE | PPC_COMPARE_ARGS,
   },
   {
      testgroup_vector_scalar_data_class,
      "ppc vector scalar test data class tests",
      PPC_ALTIVEC_DOUBLE | PPC_COMPARE | PPC_ONE_ARG,
   },
   {
      testgroup_vector_scalar_two_double,
      "ppc vector scalar tests against float double two args ",
      PPC_ALTIVEC_DOUBLE | PPC_COMPARE | PPC_TWO_ARGS,
   },
   {
      testgroup_dfp_significance,
      "ppc dfp significance",
      PPC_DFP,
   },
   {
      testgroup_bcd_misc,
      "ppc bcd misc",
      PPC_BCD,
   },
   {
      testgroup_noop_misc,
      "ppc noop misc",
      PPC_NO_OP,
   },
   {                                                                           
      testgroup_pc_immediate_misc,
      "ppc addpc_misc",
      PPC_PC_IMMEDIATE,
   },
   {
      testgroup_mffs_misc,
      "ppc mffpscr",
      PPC_MFFS,
   },
   {
      testgroup_mffs_misc_one,
      "ppc mffpscr",
      PPC_MFFS,
   },
   { NULL,                   NULL,               0x00000000, },
};

#define instruction_touches_xer(instruction_name) \
   (strncmp(instruction_name, "addex", 5) == 0)

static void testfunction_int_two_args (const char* instruction_name,
                                       test_func_t func,
                                       unsigned int test_flags)
{
   volatile HWord_t res;
   volatile unsigned int cr;
   int i, j;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_iargs; i++) {
      for (j = 0; j < nb_iargs; j++) {

         r14 = iargs[i];
         r15 = iargs[j];

         SET_CR_ZERO;
         (*func)();
         GET_CR(cr);
         GET_XER(local_xer);
         res = r17;

         printf("%s %016lx, %016lx => %016lx (%08x)",
                instruction_name, (long unsigned)iargs[i],
                (long unsigned)iargs[j], (long unsigned)res,
                cr);
         if (instruction_touches_xer(instruction_name)) {
            dissect_xer(local_xer);
         }
         printf("\n");
      }
      if (verbose) printf("\n");
   }
}

#define instruction_sets_cr0_to_zero(inst_name)   \
   ( (strncmp(inst_name, "cnttzw.", 7) == 0 ) ||   \
     (strncmp(inst_name, "cnttzd.", 7) == 0 ) )

static void testfunction_logical_one (const char* instruction_name,
                                      test_func_t func,
                                      unsigned int test_flags) {
   int i;
   volatile HWord_t res;
   volatile unsigned int cr;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_iargs; i++) {

      r14 = iargs[i];

      /* The logical instructions will set CR fields to zero, so
       * lets start with some non zero content in CR0.
       */
      SET_CR0_FIELD(0xF);

      (*func)();

      res = r17;
      GET_CR(cr);

      printf("%s %016lx => %016lx",
             instruction_name, (long unsigned)iargs[i], (long unsigned)res);

      if (instruction_sets_cr0_to_zero(instruction_name)
          && ((cr & 0xF0000000) != 0 )) {
         /* The dotted version sets the CR0 to 0, verify */
         printf(" Expected cr0 to be zero, it is (%08x)\n", cr & 0xF0000000);
      }
      printf("\n");

      if (verbose) printf("\n");
   }
}

void testfunction_one_arg_with_shift (const char* instruction_name,
                                      test_func_t test_function,
                                      unsigned int ignore_test_flags)
{
   /*This function uses global variable x_shift */
   volatile HWord_t res;
   volatile unsigned int cr;
   int i, j;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < SHIFT_VALUES_SIZE; i++) {
      for (j = 0; j < SHIFT_ARRAY_SIZE; j++) {

         r14 = values_to_shift[i];
         x_shift = shift_amounts[j];

         SET_CR_ZERO;

         (*test_function)();

         GET_CR(cr);
         res = r17;

         printf("%s %016lx, %016lx => %016lx (%08x)\n",
                instruction_name, (long unsigned)values_to_shift[i],
                (long unsigned)x_shift, (long unsigned)res, cr);
      }
   }
}

static void testfunction_three_args (const char* instruction_name,
                                     test_func_t test_function,
                                     unsigned int ignore_test_flags)
{
   volatile HWord_t res;
   volatile unsigned int cr;
   int i, j, l;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_iargs; i++) {
      for (j = 0; j < nb_iargs; j++) {
         for (l = 0; l < nb_iargs; l++) {
            r14 = iargs[i];
            r15 = iargs[j];
            r16 = iargs[l];

            SET_CR_ZERO;

            (*test_function)();

            GET_CR(cr);
            res = r17;

            printf("%s %016lx, %016lx, %016lx  => %016lx (%08x)\n",
                   instruction_name,
                   (long unsigned)r14, (long unsigned)r15,
                   (long unsigned)r16, (long unsigned)res,
                   cr);
         }
      }
   }
}

static void testfunction_vector_absolute (const char* instruction_name,
                                          test_func_t test_function,
                                          unsigned int ignore_test_flags)
{
   /* Notes:
    *   iterate across xa, xb values.
    *   Results are in xt.
    */
   volatile unsigned int cr;
   int i, j;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 4) {
      /* patterns more interesting when shifted like so.. */
      for (j = 0; j < nb_vpcv; j += 2) {

         vec_xa = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
         vec_xb = (vector unsigned long){vsxargs[j], vsxargs[j]};
         vec_xt = (vector unsigned long){0, 0};

         printf("%s xa:%016lx %016lx xb:%016lx %016lx ",
                instruction_name,
                (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                (unsigned long)vec_xb[1], (unsigned long)vec_xb[0]
                );
         printf(" => ");

         SET_CR_ZERO;

         (*test_function)();

         GET_CR(cr);

         printf(" xt:%016lx %016lx (%08x)\n", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0], cr);
      }
      if (verbose) printf("\n");
   }
}

static void testfunction_vector_xxpermute (const char* instruction_name,
                                           test_func_t test_function,
                                           unsigned int ignore_test_flags)
{
   /* Notes:
    *   VSX permute uses both xt and xa as source registers.
    *   Permute control vector is in xb.
    *   Results are in xt.
    */
   volatile unsigned int cr;
   int i, j;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 4) {
      for (j = 0; j < nb_vpcv; j += 2) {

         vec_xa = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
         vec_xt = (vector unsigned long){vsxargs[i+2], vsxargs[i+3]};
         vec_xb = (vector unsigned long){vpcv[j], vpcv[j+1]};

         printf("%s %016lx %016lx %016lx %016lx, pcv[%016lx %016lx] => ",
                instruction_name,
                (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                (unsigned long)vec_xt[1], (unsigned long)vec_xt[0],
                (unsigned long)vec_xb[1], (unsigned long)vec_xb[0]);

         SET_CR_ZERO;

         (*test_function)();

         GET_CR(cr);

         printf(" %016lx %016lx (%08x)\n", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0], cr);

#if defined (DEBUG_VECTOR_PERMUTE)
         printf("DEBUG:%s %016lx %016lx %016lx %016lx, pcv[%016lx %016lx]\n",
                ignore_name,
                (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                (unsigned long)vec_xt[1], (unsigned long)vec_xt[0],
                (unsigned long)vec_xb[1], (unsigned long)vec_xb[0]);
#endif
      }
      if (verbose) printf("\n");
   }
}

static void testfunction_vector_logical_one (const char* instruction_name,
                                             test_func_t test_function,
                                             unsigned int ignore_test_flags)
{
   /* Notes:
    *   vector instructions with one input, one output.
    *   xt, xa
    */
   int i;
   int t;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {

      vec_xa = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      for (t = 0; t < 2; t++) {
         vec_xt[0] = (t == 0) ? 0 : 0xffffffffffffffff;
         vec_xt[1] = (t == 0) ? 0 : 0xffffffffffffffff;

         printf("%s xa:%016lx %016lx xt:%016lx %016lx => ",
                instruction_name,
                (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                (unsigned long)vec_xt[1], (unsigned long)vec_xt[0]);

         (*test_function)();

         printf(" xt:%016lx %016lx\n",
                (unsigned long)vec_xt[1], (unsigned long)vec_xt[0]);
      }
   }
   if (verbose) printf("\n");
}

static void testfunction_gpr_vector_logical_one (const char* instruction_name,
						 test_func_t test_function,
						 unsigned int ignore_test_flags)
{
   /* Notes:
    *   vector instructions with one vector input, one GPR output.
    *   rt, xa
    */
   int i;
   volatile HWord_t res;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {

      vec_xa = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      r17 = 0;
      res = r17;

      printf("%s rt xt:%016lx %016lx => ",
	     instruction_name,
	     (unsigned long)vec_xa[1], (unsigned long)vec_xa[0]);

      (*test_function)();

      printf(" rt: %016lx\n", (long unsigned)res);
   }
   if (verbose) printf("\n");
}

static void testfunction_vector_logical_four (const char* instruction_name,
                                              test_func_t test_function,
                                              unsigned int ignore_test_flags) {
   /* Notes:
    *   vector instructions with three input arguments, one output.
    *   xt, xa, xb, xc.
    *   Permute control vector is in xc.
    *   Results are in xt.
    */
   volatile unsigned int cr;
   int i, j;
   int p;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 4) {
      for (j = 0; j < nb_vargs; j += 4) {
         for (p = 0; p < nb_vpcv; p += 2) {

            vec_xa = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
            vec_xb = (vector unsigned long){vsxargs[j], vsxargs[j+1]};
            vec_xc = (vector unsigned long){vpcv[p], vpcv[p+1]};

            printf("%s %016lx %016lx %016lx %016lx, pcv[%016lx %016lx] => ",
                   instruction_name,
                   (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                   (unsigned long)vec_xb[1], (unsigned long)vec_xb[0],
                   (unsigned long)vec_xc[1], (unsigned long)vec_xc[0]);

            SET_CR_ZERO;

            (*test_function)();

            GET_CR(cr);

            printf(" %016lx %016lx (%08x)\n", (unsigned long)vec_xt[1],
                   (unsigned long)vec_xt[0], cr);
         }
      }

      if (verbose) printf("\n");
   }
}

static
void testfunction_vector_insert_or_extract_immediate (const char* instruction_name,
                                                      test_func_t test_function,
                                                      unsigned int ignore_test_flags) {
   /* Uses global variable x_index */
   /* uses global variable insert_extract_error */
   int i;
   int t;

   VERBOSE_FUNCTION_CALLOUT

   /* for the insert and extract tests, we deliberately use only a
    * subset of the vsxargs array as input data.
    */
   for (i = 2; i < 9 ; i += 4) { /* index into vsxargs[] array */

      /* Note:
       * Determining the proper offset for {extract, insert} byte, halfword,
       * word, double would complicate things.  For simplicity, allow the
       * sub-functions to ignore input that would be invalid.  Catch and
       * suppress output for those cases per the global variable.
       */
      for (x_index = 0; x_index < 16 ; x_index++) {
         vec_xb[0] = (unsigned long)vsxargs[i];
         vec_xb[1] = (unsigned long)vsxargs[i+1];

         /* Run each test against all zeros and then all ones,
          * This is intended to help any bitfield changes stand out.
          */
         for (t = 0; t < 2; t++) {

            vec_xt[0] = (t == 0) ? 0 : 0xffffffffffffffff;
            vec_xt[1] = (t == 0) ? 0 : 0xffffffffffffffff;

            insert_extract_error = 0;

            (*test_function)();

            if (!insert_extract_error) {
               printf("%s %016lx %016lx [%d] (into%s) => ",
                      instruction_name, (unsigned long)vec_xb[1],
                      (unsigned long)vec_xb[0], x_index,
                      (t == 0 ? " zeros" : "  ones") );

               printf("%016lx %016lx\n", (unsigned long)vec_xt[1],
                      (unsigned long)vec_xt[0]);
            }
         }
      }
   }
}


static void testfunction_vector_immediate (const char * instruction_name,
                                           test_func_t test_function,
                                           unsigned int ignore_test_flags) {
   /* Uses global variable x_splat */
   int i;
   int t;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < SPLAT_ARRAY_SIZE; i++) {
      x_splat = splat_values[i];
      for (t = 0; t < 2; t++) {
         vec_xt[0] = (t == 0) ? 0 : 0xffffffffffffffff;
         vec_xt[1] = (t == 0) ? 0 : 0xffffffffffffffff;

         printf("%s %016lx %016lx [%2x] => ",
                instruction_name, (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0], x_splat);

         (*test_function)();

         printf("%016lx %016lx\n", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);
      }
   }
}

static void testfunction_vector_loadstore (const char* instruction_name,
                                           test_func_t test_function,
                                           unsigned int ignore_flags) {
   /* exercises vector loads from memory, and vector stores from memory.
    * <load or store instruction>  XS, RA, RB
    * For these tests, RA will be zero.
    * EA is then, simply, RB.
    */
   int i;
   int buffer_pattern;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {

      vec_xt = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      r14 = 0;
      r15 = (unsigned long) & buffer;

      for (buffer_pattern = 0; buffer_pattern < MAX_BUFFER_PATTERNS;
           buffer_pattern++) {

         /* set patterns on both ends */
         initialize_buffer(buffer_pattern);

         printf("%s ", instruction_name);
         printf("%016lx %016lx ", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);
         dump_small_buffer();
         printf(" =>\n");

         (*test_function)();

         printf("    %016lx %016lx ", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);
         dump_small_buffer();
         printf("\n");
      }
   }
}


static void testfunction_vectorscalar_move_tofrom (const char * instruction_name,
                                                   test_func_t test_function,
                                                   unsigned int ignore_test_flags) {
   /* Move to / move from vector scalar.  spin through simple variants of
    * both the VSR and the register.
    * for simplicity, RA from 'mtvsrdd xt, ra, rb' is 0.
    */
   int i, v;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < PATTERN_SIZE; i++) {
      for (v = 0; v < PATTERN_SIZE; v++) {
         /* if i==v, patterns will match, so just skip these as
          * non-interesting..
          */
         if (i == v) continue;
         r14 = 0;
         r15 = pattern[i%PATTERN_SIZE];
         vec_xt[0] = pattern[v%PATTERN_SIZE];
         vec_xt[1] = pattern[v%PATTERN_SIZE];

         printf("%s ", instruction_name);
         printf("%016lx %016lx %lx %016lx ", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0],
                (long unsigned)r14,  (long unsigned)r15 );

         (*test_function)();

         printf("=> %016lx %016lx %lx %016lx", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0],
                (long unsigned)r14,  (long unsigned)r15 );
         printf("\n");
      }
   }
}

/* Some of the load/store vector instructions load 64 bits, upper 64 bits
 * are undefined.
 */
#define load_4_bytes(instruction_name) (                     \
           (strncmp(instruction_name, "lxssp ",5) == 0) )

/* Some of the load/store vector instructions use a length value that
 * is stored in bits 0:7 of RB.  */
#define uses_bits_0to7(instruction_name) (                  \
           (strncmp(instruction_name, "lxvl "  ,5) == 0) || \
           (strncmp(instruction_name, "lxvll " ,6) == 0) || \
           (strncmp(instruction_name, "stxvl " ,6) == 0) || \
           (strncmp(instruction_name, "stxvll ",7) == 0) )

static void testfunction_vector_scalar_loadstore_length (const char* instruction_name,
                                                         test_func_t test_function,
                                                         unsigned int ignore_flags) {
   /* exercises vector loads from memory, and vector stores from memory.
    * with length specification.
    * <load or store instruction>  XS, RA, RB
    * For these tests, RA (i.e. EA) is address of source/dest and can
    * not be zero.
    * The length value is in rb.  For a subset of these instructions,
    * the length is stored in bits 0:7, versus 56:63 of r15.
    */
   int i;
   unsigned long l;
   int buffer_pattern;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {
      for (l = 0; l <= 16; l += 4) {

         for (buffer_pattern = 0; buffer_pattern < MAX_BUFFER_PATTERNS;
              buffer_pattern++) {

           /* set patterns on both ends */
            vec_xt = (vector unsigned long){vsxargs[i], vsxargs[i+1]};

            initialize_buffer(buffer_pattern);

            printf("%s ", instruction_name);
            printf("%016lx %016lx ", (unsigned long)vec_xt[1],
                   (unsigned long)vec_xt[0] );
            if (uses_bits_0to7(instruction_name)) {
               printf(" 0x%2lx ", (long unsigned)r15>>56 );

            } else {
               printf(" 0x%2lx ", (long unsigned)r15 );
            }

            dump_small_buffer();

            if (uses_bits_0to7(instruction_name)) {
               /* length is stored in bits 0:7 of gpr[r15]. */
               r15 = (unsigned long)((0xff & l) << 56);

            } else {
               /* length is stored in gpr[r15]. */
               r15 = l;
            }
            r14 = (unsigned long) & buffer;

            (*test_function)();

            if (load_4_bytes(instruction_name)) {
	      /* Double word element 1 (BE numbering) is undefined, clear for
		 consistency. Only loaded bits [0:31], mask out rest of
		 element 0.
	      */
	      vec_xt[0] = 0;
	      vec_xt[1] &= 0xFFFFFFFF00000000;
	    }

            printf("=> %016lx %016lx & %16lx", (unsigned long)vec_xt[1],
                   (unsigned long)vec_xt[0], (long unsigned)r15 );
            dump_small_buffer();

            printf("\n");
         }
      }
   }
}

static void testfunction_vector_count_bytes (const char* instruction_name,
                                             test_func_t test_function,
                                             unsigned int ignore_flags)
{
   int i;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {
      vec_xb = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      r14 = 0;

      printf("%s ", instruction_name);
      printf("%016lx %016lx %2d ", (unsigned long)vec_xb[1],
             (unsigned long)vec_xb[0], (unsigned)r14);

      (*test_function)();

      printf("=> %2d\n", (unsigned)r14 );
   }
}

static void testfunction_vector_extract (const char* instruction_name,
                                         test_func_t test_function,
                                         unsigned int ignore_flags)
{
   int i;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {
      vec_xb = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      for (r15 = 0; r15 < 16; r15++) {
      r14 = 0;

      printf("%s ", instruction_name);
      printf("%016lx %016lx %2d ", (unsigned long)vec_xb[1],
             (unsigned long)vec_xb[0], (unsigned)r15);

      (*test_function)();

      printf("=> %16lx\n", (long unsigned)r14 );
      }
   }
}

static void testfunction_vector_extend_sign (const char* instruction_name,
                                             test_func_t test_function,
                                             unsigned int ignore_flags)
{
   int i;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {
      vec_xb = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      vec_xt = (vector unsigned long){0, 0};

      printf("%s ", instruction_name);
      printf("%016lx %016lx ",(unsigned long) vec_xb[1],
             (unsigned long)vec_xb[0]);

      (*test_function)();

      printf("=> %016lx %016lx\n", (unsigned long)vec_xt[1],
             (unsigned long)vec_xt[0]);
   }
}

/* packed binary decimal misc */

#define convert_from_zoned(instruction_name) (strncmp(instruction_name, "bcdcfz", 6) ==0 )

#define convert_from_national(instruction_name) (strncmp(instruction_name, "bcdcfn", 6) == 0)

#define convert_to_zoned(instruction_name) (strncmp(instruction_name, "bcdctz", 6) == 0)

#define convert_to_national(instruction_name) (strncmp(instruction_name, "bcdctn", 6) == 0)

#define shift_or_truncate(instruction_name)                \
         (strncmp(instruction_name, "bcds", 4)  == 0    || \
          strncmp(instruction_name, "bcdus", 5) == 0    || \
          strncmp(instruction_name, "bcdsr", 5) == 0    || \
          strncmp(instruction_name, "bcdtrunc", 8) == 0 || \
          strncmp(instruction_name, "bcdutrunc", 9) == 0)


/* Helper function - returns 1 or 0 per whether the p1 or p0 string
 *  exists in the instruction name passed in.  The PS indicates preferred
 *  sign, and has meaning for some of the BCD instructions.
 */
static inline int p_value(const char * instruction_name) {
   char * found_p0;
   char * found_p1;

   found_p1 = strstr(instruction_name, "p1");
   found_p0 = strstr(instruction_name, "p0");

   if (found_p1) return 1;

   if (found_p0) return 0;

   if (verbose) printf("p* substring not found in (%s)\n", instruction_name);

   return 0;
}

/* bcd test has been split out a bit..  a few bcd specific global vars here
 * to help keep that clean.
 */
long shift_or_truncate_instruction;
int xa_sign, xb_sign, xt_sign;
int short_circuit;

/* testfunction_bcd_setup_inputs
 * This is a helper function that sets up the vec_xa, vec_xb values for
 * use in the bcd tests.
 */
static inline void testfunction_bcd_setup_inputs(const char * instruction_name,
                                                 int i, int j) {
   short_circuit=0;

   if (shift_or_truncate_instruction) {
      if (i >= nb_decimal_shift_entries - 2) {
         short_circuit = 1;
         return;
      }
      vec_xa = (vector unsigned long) {decimal_shift_table[i+1],
                                       decimal_shift_table[i]};

   } else {
      if (i >= nb_decimal_shift_entries - 2) {
         short_circuit = 1;
         return;
      }

      vec_xa = (vector unsigned long) {packed_decimal_table[i+1],
                                       packed_decimal_table[i]};
      xa_sign = extract_packed_decimal_sign(vec_xa[0], vec_xa[1]);
   }

   if (convert_from_zoned(instruction_name)) { /* convert from zoned */
      if (j >= nb_zoned_decimal_entries - 2) {
         short_circuit = 1;
         return;
      }

      vec_xb = (vector unsigned long) {zoned_decimal_table[j+1],
                                       zoned_decimal_table[j]};
      xb_sign = extract_zoned_decimal_sign(vec_xb[0], vec_xb[1]);

   } else if (convert_from_national(instruction_name)) {
      /* convert from national */
      if (j >= nb_national_decimal_entries - 2) {
         short_circuit = 1;
         return;
      }
      vec_xb = (vector unsigned long) {national_decimal_table[j+1],
                                       national_decimal_table[j]};
      xb_sign = extract_national_decimal_sign(vec_xb[0], vec_xb[1]);

   } else {
      /* packed decimal entries */
      if (j >= nb_packed_decimal_entries - 2) {
         short_circuit = 1;
         return;
      }
      vec_xb = (vector unsigned long) {packed_decimal_table[j+1],
                                       packed_decimal_table[j]};
      xb_sign = extract_packed_decimal_sign(vec_xb[0], vec_xb[1]);
   }
}

static inline void testfunction_bcd_display_outputs(const char * instruction_name) {

   printf(" xt:%016lx %016lx", (unsigned long)vec_xt[1],
          (unsigned long)vec_xt[0] );

   if (convert_to_zoned(instruction_name)) {
      /* convert to zoned */
      xt_sign = extract_zoned_decimal_sign(vec_xt[0], vec_xt[1]);
      dissect_zoned_decimal_sign(xt_sign, p_value(instruction_name));

   } else if (convert_to_national(instruction_name)) {
      /* convert to national */
      xt_sign = extract_national_decimal_sign(vec_xt[0], vec_xt[1]);
      dissect_national_decimal_sign(xt_sign);

   } else {
      /* packed decimal entries, or shift/truncate */
      if (!shift_or_truncate_instruction) {
         xt_sign = extract_packed_decimal_sign(vec_xt[0], vec_xt[1]);
         dissect_packed_decimal_sign(xt_sign);
      }
   }
   printf("\n");
}

#define uses_half_precision_input(instruction_name) (  \
   (strncmp(instruction_name, "xscvhpdp", 8) == 0) ||  \
   (strncmp(instruction_name, "xvcvhpsp", 8) == 0) )

#define uses_single_precision_input(instruction_name) ( \
   (strncmp(instruction_name, "xvcvsphp", 8) == 0) )

#define uses_double_precision_input(instruction_name) ( \
   (strncmp(instruction_name, "xscvdphp", 8) == 0) )

#define uses_half_precision_output(instruction_name) (  \
   (strncmp(instruction_name, "xscvdphp", 8) == 0) ||   \
   (strncmp(instruction_name, "xvcvsphp", 8) == 0) )

#define is_half_precision_instruction(instruction_name) ( \
    uses_half_precision_input(instruction_name) ||        \
    uses_half_precision_output(instruction_name) )

/* Helper for those instructions with an unused second dword, indicating
 * the outer loop can be short-circuited after one pass.
 */
#define unused_second_dword(instruction_name) (       \
   (strncmp(instruction_name, "xscvhpdp", 8) == 0) || \
   (strncmp(instruction_name, "xscvdphp", 8) == 0) )

static void testfunction_vector_scalar_two_quad (const char* instruction_name,
                                                 test_func_t test_function,
                                                 unsigned int ignore_flags)
{
   int i;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_vargs; i += 2) {
      if (uses_half_precision_input(instruction_name)) {
         vec_xb = (vector unsigned long){binary16_float_vsxargs[i],
                                         binary16_float_vsxargs[i+1]};
      } else {
         vec_xb = (vector unsigned long){vsxargs[i], vsxargs[i+1]};
      }

      vec_xt = (vector unsigned long){0, 0};

      printf("%s ", instruction_name);
      printf("%016lx %016lx ", (unsigned long)vec_xb[1],
             (unsigned long)vec_xb[0]);

      SET_FPSCR_ZERO

      (*test_function)();

      GET_FPSCR(local_fpscr);

      printf("=> %016lx %016lx", (unsigned long)vec_xt[1],
             (unsigned long)vec_xt[0]);
      dissect_fpscr(local_fpscr);
      printf("\n");
   }
}

/* helper macro.  Use below to limit output to only dword[0] for the inputs
 * to the instructions listed here.  */
#define instruction_only_uses_dword0_inputs(instruction_name) \
	((strncmp(instruction_name, "xscmpeqdp",9) == 0) || \
	 (strncmp(instruction_name, "xscmpgtdp",9) == 0) || \
	 (strncmp(instruction_name, "xscmpgedp",9) == 0) || \
	 (strncmp(instruction_name, "xsmincdp",8) == 0) || \
	 (strncmp(instruction_name, "xsmaxcdp",8) == 0) )

static void
testfunction_vector_scalar_compare_double (const char* instruction_name,
                                               test_func_t test_function,
                                               unsigned int ignore_test_flags){
   int i,j;
   /* Uses global variable x_index */

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_float_vsxargs - 1; i++) {
      for (j = 0; j < nb_float_vsxargs - 1; j++) {
         for (x_index = 2; x_index < 3; x_index++) {

            /* TODO FIXME- there was a casting issue below.  This incantation
             * works, but I suspect can be simplified...
             */
            vec_xa = (vector unsigned long){(unsigned long)binary64_float_vsxargs[i+1], (unsigned long)binary64_float_vsxargs[i]};

            vec_xb = (vector unsigned long){(unsigned long)binary64_float_vsxargs[j], (unsigned long)binary64_float_vsxargs[j+1]};

            /* run each test against cleared CR and FPSCR */
            /* Note that the SET_*_ZERO calls are not actually sufficient here,
             * due to infrastructure between here and there that also set some
             * of the CR bits. The condition regs are cleared here, but are
             * also both cleared and read within the to-be-tested asm chunk to
             * get accurate results.
             */
            SET_CR_ZERO
            SET_FPSCR_ZERO
            if (instruction_only_uses_dword0_inputs(instruction_name)) {
               printf("%s %016lx %016lx",
                      instruction_name, (unsigned long)vec_xa[1],
                      (unsigned long)vec_xb[1]);
            } else {
               printf("%s %016lx %016lx %016lx %016lx",
                      instruction_name,
                      (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                      (unsigned long)vec_xb[1], (unsigned long)vec_xb[0]);
            }

            if (verbose) printf(" cr#%d ", x_index);

            printf(" => ");

            (*test_function)();

            if (instruction_only_uses_dword0_inputs(instruction_name)) {
               printf("%016lx %016lx", (unsigned long)vec_xt[1],
                      (unsigned long)vec_xt[0]);
            }

            dissect_fpscr(local_fpscr);
            dissect_fpscr_result_value_class(local_fpscr);
            dissect_cr_rn(local_cr, x_index);
            printf("\n");
         }
      }
   }
}

/* These instructions set the floating point condition codes. */
/* verify logic reversal */
#define does_not_set_floating_point_cc(instruction_name) \
   (strncmp(instruction_name, "xvtstdcdp", 9) == 0) |    \
   (strncmp(instruction_name, "xvtstdcsp", 9) == 0)

static void
testfunction_vector_scalar_data_class (const char* instruction_name,
                                       test_func_t test_function,
                                       unsigned int ignore_test_flags) {
   int j;
   /* x_index is used as a key into the DCMX value.
    *
    *   BF, XB, DCMX
    * For instruction tests called through this function, note that we are only
    * utilizing bf (condition register) #3; where 3 was mostly randomly
    * chosen, and has no special meaning.
    */

   VERBOSE_FUNCTION_CALLOUT

   for (j = 0; j < nb_float_vsxargs - 1; j++) {
      /* for dcmx field, start with x_index=1 to skip the 'all' dcmx entry. */
      for (x_index = 1; x_index < 8; x_index++) {
         vec_xb[0] = float_vsxargs[j];
         vec_xb[1] = float_vsxargs[j+1];
         vec_xt[0] = 0x0a0a0a0a0a0a0a0a;
         vec_xt[1] = 0x0505050505050505;
         SET_CR_ZERO
         SET_FPSCR_ZERO

         dcmx_match = 0;

         (*test_function)();

         /* the local_fpscr value is gathered within the test_function call. */
         dcmx_match = (local_fpscr & FPCC_FE_BIT);

         if (dcmx_match || (verbose>2)) {
            printf("%s %016lx, %016lx ",
                   instruction_name, (unsigned long)vec_xb[1],
                   (unsigned long)vec_xb[0]);

            print_dcmx_field(x_index);

            if (dcmx_match)
               printf(" => Match.  ");

            printf(" %016lx, %016lx ", (unsigned long)vec_xt[1],
                   (unsigned long)vec_xt[0]);

            dissect_cr_rn(local_cr,3);
            dissect_fpscr_dcmx_indicator(local_fpscr);
            printf("\n");
         }

         printf("%s %016lx, %016lx => ",
                instruction_name, (unsigned long)vec_xb[1],
                (unsigned long)vec_xb[0]);

         printf(" %016lx, %016lx\n", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);
      }
   }
}

static void testfunction_vector_scalar_compare_quads (const char* instruction_name,
                                                      test_func_t test_function,
                                                      unsigned int ignore_test_flags) {
   /* Uses global variable x_index */
   int i,j;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_float_vsxargs - 1; i++) {
      for (j = 0; j < nb_float_vsxargs - 1; j++) {
         for (x_index = 0; x_index < 3 ; x_index++) {
            vec_xa[0] = float_vsxargs[i];
            vec_xa[1] = float_vsxargs[i+1];
            vec_xb[0] = float_vsxargs[j];
            vec_xb[1] = float_vsxargs[j+1];

            /* run each test against cleared CR and FPSCR */
            /* Note that the SET_*_ZERO calls are not actually sufficient here,
             * due to infrastructure between here and there that also set some
             * of the CR bits. The condition regs are cleared here, but are
             * also both cleared and read within the to-be-tested asm chunk
             * to get accurate results.
             */
            printf("%s %016lx%016lx %016lx%016lx (cr#%d) => ",
                   instruction_name,
                   (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                   (unsigned long)vec_xb[1], (unsigned long)vec_xb[0],
                   x_index);

            SET_CR_ZERO
            SET_FPSCR_ZERO

            (*test_function)();

            GET_CR(local_cr);
            GET_FPSCR(local_fpscr);

            dissect_fpscr(local_fpscr);
            dissect_cr_rn(local_cr, x_index);
            printf("\n");
         }
      }
   }
}

static void testfunction_vector_scalar_rounding_quads (const char* instruction_name,
                                                       test_func_t test_function,
                                                       unsigned int ignore_test_flags) {
   /* Uses global variable x_index */
   /* For this function, x_index is used as a key into R and RMC values.
    * Also note, the fpscr.rn value may be used to affect the rounding mode.
    * that variation is not evaluated here. */
   int j;

   VERBOSE_FUNCTION_CALLOUT

   for (j = 0; j < nb_float_vsxargs - 1; j++) {
      for (x_index = 0; x_index < 8; x_index++) {
         vec_xb[0] = float_vsxargs[j];
         vec_xb[1] = float_vsxargs[j+1];

         printf("%s %016lx%016lx (R=%x) (RMC=%x) => ",
                instruction_name,
                (unsigned long)vec_xb[1], (unsigned long)vec_xb[0],
                (x_index & 0x4) >> 2, x_index & 0x3);

         SET_CR_ZERO
         SET_FPSCR_ZERO

         (*test_function)();

         GET_FPSCR(local_fpscr);

         printf("%016lx%016lx", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);
         dissect_fpscr(local_fpscr);
         printf("\n");
      }
   }
}

static void testfunction_vector_three_special (const char* instruction_name,
                                               test_func_t test_function,
                                               unsigned int ignore_test_flags){
   /* Notes:
    *   vector instructions with two inputs, one output.
    *   vrt, vra, vrb
    */
   int i, j;
   int t;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_float_vsxargs - 1; i++) {
      for (j = 0; j < nb_float_vsxargs - 1; j++) {
         vec_xa[0] = float_vsxargs[i];
         vec_xa[1] = float_vsxargs[i+1];
         vec_xb[0] = float_vsxargs[j];
         vec_xb[1] = float_vsxargs[j+1];

         for (t = 0; t < 2; t++) {
            vec_xt[0] = (t == 0) ? 0 : 0xffffffffffffffff;
            vec_xt[1] = (t == 0) ? 0 : 0xffffffffffffffff;

            SET_FPSCR_ZERO;
            printf("%s %016lx%016lx %016lx%016lx %016lx%016lx => ",
                   instruction_name,
                   (unsigned long)vec_xa[1], (unsigned long)vec_xa[0],
                   (unsigned long)vec_xb[1], (unsigned long)vec_xb[0],
                   (unsigned long)vec_xt[1], (unsigned long)vec_xt[0]);

            (*test_function)();

            GET_FPSCR(local_fpscr);

            printf(" %016lx%016lx", (unsigned long)vec_xt[1],
                   (unsigned long)vec_xt[0]);
            dissect_fpscr(local_fpscr);
            printf("\n");
         }
      }
   }
}

#define vector_instruction_is_xvcvhpsp(instruction_name) \
   (strncmp(instruction_name, "xvcvhpsp", 8) == 0)

static void testfunction_vector_scalar_two_double(const char* instruction_name,
                                                  test_func_t test_function,
                                                  unsigned int ignore_test_flags) {
   /* Notes:
    *   iterate across double values stored in xa, xb.
    *   Or, on half-word values in vec_xb.
    *   Results are in vec_xt.
    */
   int i, j;

   VERBOSE_FUNCTION_CALLOUT

   for (i = 0; i < nb_float_vsxargs - 1; i += 2) {
      for (j = 0; j < nb_float_vsxargs - 1; j += 2) {
         /* vec_xb is only used by the convert instructions, the other callers
          * use the r14, r15 fields.
          * The 16-bit converts reference every other half-word in the vector.
          * For this reason, populate the input field with a cross-section of
          * values.
          */
         printf("%s ",instruction_name);

         if (uses_half_precision_input(instruction_name)) {
            vec_xb = (vector unsigned long) {
               binary16_float_vsxargs[i]         |
               binary16_float_vsxargs[j]   << 16 |
               binary16_float_vsxargs[i+1] << 32 |
               binary16_float_vsxargs[j+1] << 48,
               binary16_float_vsxargs[(nb_float_vsxargs - 1) - j - 1 ]       |
               binary16_float_vsxargs[(nb_float_vsxargs - 1) - i - 1] << 16  |

               binary16_float_vsxargs[(nb_float_vsxargs - 1) - j ] << 32  |
               binary16_float_vsxargs[(nb_float_vsxargs - 1) - i ] << 48
            };
            printf("   vec_xb[1] = 0x%lx, vec_xb[0] = 0x%lx ",
                   (unsigned long)vec_xb[1], (unsigned long)vec_xb[0]);

         } else if (uses_single_precision_input(instruction_name)) {
            vec_xb = (vector unsigned long) {
               binary32_float_vsxargs[i]         |
               binary32_float_vsxargs[i+1] << 32,
               binary32_float_vsxargs[nb_float_vsxargs - 1 - j ]         |
               binary32_float_vsxargs[nb_float_vsxargs - 1 - j ] << 32
            };
            printf("   vec_xb[1] = 0x%lx, vec_xb[0] = 0x%lx ",
                   (unsigned long)vec_xb[1], (unsigned long)vec_xb[0]);

         } else { /* uses double */
            r14 = binary64_float_vsxargs[i];
            r15 = binary64_float_vsxargs[j];
            printf("   r14 = 0x%lx, r15 = 0x%lx ", r14, r15);
         }

         vec_xt = (vector unsigned long){0, 0};

         printf("%016lx %016lx ", (unsigned long)vec_xb[1],
                (unsigned long)vec_xb[0] );

         if ((verbose > 2) && uses_double_precision_input(instruction_name)) {
            dissect_binary64_float(vec_xb[1]);
            dissect_binary64_float(vec_xb[0]);
         }

         printf(" => ");
         SET_FPSCR_ZERO

        (*test_function)();

         GET_FPSCR(local_fpscr);
         printf(" %016lx %016lx", (unsigned long)vec_xt[1],
                (unsigned long)vec_xt[0]);

         if ((verbose > 2) && uses_half_precision_output(instruction_name)) {
            dissect_double_as_16s(vec_xt[1]);
            dissect_double_as_16s(vec_xt[0]);
         }

         /* The xvcvhpsp instruction does not set the C and FPCC fields */
         if (!vector_instruction_is_xvcvhpsp(instruction_name))
            dissect_fpscr(local_fpscr);

         printf("\n");
      } // j

      /* If we are doing half precision conversions, the i-loop can be
       * short-circuited to avoid duplicate input values.  */
      if (unused_second_dword(instruction_name))
         i = nb_float_vsxargs+1;
   } // i
}

static void testfunction_set_boolean (const char* instruction_name,
                                      test_func_t test_function,
                                      unsigned int ignore_test_flags)
{
   int cr_base_value;
   /* Notes:
    * Set RT to values 0, -1, 1 depending on what bits are set in the specified
    * CR field.  x_index references here reflect the cr_field number.
    */

   VERBOSE_FUNCTION_CALLOUT

   for (x_index = 0; x_index <= 7; x_index++) {
      for (cr_base_value = 0; cr_base_value < 8; cr_base_value++) {
         cr_value = (0x11111111 * cr_base_value)
            & (0xf << (4 * (7 - x_index))) ;

         r14 = 0xa5a5a5a5c7c7c7c7;

         printf("%s cr_field:%1x cr_value::%08x",
                instruction_name, x_index,cr_value);
         printf(" => ");

         (*test_function)();

         printf(" %016lx\n", r14);
      }
   }
}


static void testfunction_char_compare (const char* instruction_name,
                                       test_func_t test_function,
                                       unsigned int ignore_test_flags)
{
   /* Notes:
    *   iterate through char values stored in RA, RB.
    *   Results stored in cr field BF.
    */
   int i, j;
   int local_crf;

   VERBOSE_FUNCTION_CALLOUT

   for (x_index = 0; x_index <= 7; x_index++) {
      for (i = 0; i < nb_char_ranges; i += 4) {
         for (j = 0; j < nb_char_args; j++) {
            r14 = char_args[j];

            /* For cmprb*, only needs the lower characters. */
            r15 = char_ranges[i] | (char_ranges[i+1] << 8) |
                  char_ranges[i+2] << 16 | (char_ranges[i+3] << 24);

            /* For cmpeqb, also load the rest of the range field, shift allk
             * chars up by one.
             */
            r15 |= (r15 + 0x01010101) << 32;
            printf("%s 0x%02lx (%c) (cmpeq:0x%016lx) (cmprb:src22(%c-%c) src21(%c-%c))",
                   instruction_name,
                   r14, (int)r14,
                   r15, (int)(r15 & 0xff), (int)((r15 >> 8) & 0xff),
                   (int)((r15 >> 16) & 0xff), (int)((r15 >> 24) & 0xff)
                   );

            printf(" =>");

	    /* Clear condition code reg (CR) immediately before test
	     * instruction, read CR and clear immediately after test
	     * instruction.  Otherwise, the CR gets corrupted and depending
	     * on optimization level, strange loop control flow issues
	     * occur because CR has been messed with.
	     */
            (*test_function)();

	    // GET_CR(local_cr);  done in test case
            local_crf = extract_cr_rn(local_cr, x_index);

            if (verbose)
               printf(" %s found or in range (%x)",
                      (cr_positive_set(local_crf))?"   " : " not", local_crf);
            else
               if (cr_positive_set(local_crf)) printf(" in range/found");

            printf("\n");
         }
      }
   }
}

#define instruction_uses_quads(instruction_name) (strncmp(instruction_name, "dtstsfiq", 8) == 0)

static void testfunction_dfp_significance (const char* instruction_name,
                                           test_func_t test_function,
                                           unsigned int ignore_test_flags)
{
   int local_crf;
   int i;
   int num_dfp_vals;

   VERBOSE_FUNCTION_CALLOUT

   if (instruction_uses_quads(instruction_name)) {
      num_dfp_vals = nb_dfp128_vals/2;  //Next loop uses two at a time
   } else {
      num_dfp_vals = nb_dfp64_vals;
   }

   for (i = 0; i < num_dfp_vals; i++) {
      if (instruction_uses_quads(instruction_name)) {
         dfp_value.u128.vall = dfp128_vals[i * 2];
         dfp_value.u128.valu = dfp128_vals[(i * 2) + 1];

      } else {
         // could rework this to use u64.val, but...
         dfp_value.u128.vall = dfp128_vals[i ];
         dfp_value.u128.valu = dfp128_vals[i ];
      }

      /* Keeping test simpler, always using cr3 within test_dtstsfi* */
      for (dfp_significance = 0; dfp_significance <= 63;) {
         /* Todo: tweak output here, or input values so the generated content
          * looks better.
          */
         printf("%s significance(0x%02x) ",
                instruction_name, dfp_significance);

         if (instruction_uses_quads(instruction_name)) {
            dissect_dfp128_float(dfp_value.u128.vall, dfp_value.u128.valu);

            if (verbose > 6)
               printf("(RAW) value = %16lx,%016lx ",
                      dfp_value.u128.vall, dfp_value.u128.valu /*f14, f15 */);

         } else {
            dissect_dfp64_float(dfp_value.u128.vall);

            if (verbose > 6)
               printf("(RAW) value = %16lx ", dfp_value.u128.vall /*f14 */);
         }

         (*test_function)();

         //  GET_CR(local_cr); done in test_function

         local_crf = extract_cr_rn(local_cr, /* hardcoded cr3 */ 3);
         dissect_cr_rn(local_cr, /* hardcoded cr3 */ 3);

         printf(" (%x)", local_crf);
         printf("\n");

         /* Special case for incrementation of the significance checking
          * value.
          */
         if (dfp_significance < 8)
            dfp_significance += 4;  /* 0, 4, 8 */

         else if (dfp_significance < 32)
            dfp_significance += 8; /* 16, 24, 32 */

         else if (dfp_significance < 48)
            dfp_significance += 16; /* 48 */

         else
            dfp_significance += 15; /* 63 */
      }
   }
}

/* packed binary decimal misc */

#define convert_tofrom_instruction(instruction_name)      \
        ( (strncmp(instruction_name, "bcdcf", 5) == 0) || \
          (strncmp(instruction_name, "bcdct", 5) == 0) )

static void testfunction_bcd_misc (const char* instruction_name,
                                   test_func_t test_function,
                                   unsigned int ignore_test_flags)
{
   int i, j;
   int local_crf;
   long max_xa_entries = 0;
   long max_xb_entries = 0;

   VERBOSE_FUNCTION_CALLOUT

   shift_or_truncate_instruction = shift_or_truncate(instruction_name);

   max_xa_entries = MAX(max_xa_entries, nb_decimal_shift_entries);
   max_xa_entries = MAX(max_xa_entries, nb_packed_decimal_entries);

   max_xb_entries = MAX(max_xb_entries, nb_zoned_decimal_entries);
   max_xb_entries = MAX(max_xb_entries, nb_national_decimal_entries);
   max_xb_entries = MAX(max_xb_entries, nb_packed_decimal_entries);

   for (i = 0; i < (max_xa_entries - 1); i += 2) {
      for (j = 0; j < (max_xb_entries - 1); j += 2) {
         testfunction_bcd_setup_inputs(instruction_name, i, j);

         if (short_circuit) continue;

         printf("%s ",  instruction_name);
         printf("xa:%016lx %016lx ", (unsigned long)vec_xa[1],
                (unsigned long)vec_xa[0]);

         if (!shift_or_truncate_instruction)
            dissect_packed_decimal_sign(xa_sign);

         printf(" xb:%016lx %016lx ", (unsigned long)vec_xb[1],
                (unsigned long)vec_xb[0]);

         if (convert_from_zoned(instruction_name)) {
            /* convert from zoned */
            dissect_zoned_decimal_sign(xb_sign, p_value(instruction_name));

         } else if (convert_from_national(instruction_name)) {
            /* convert from national */
            dissect_national_decimal_sign(xb_sign);

         } else {
            /* packed decimal entries */
            if (!shift_or_truncate_instruction)
               dissect_packed_decimal_sign(xb_sign);
         }

         printf(" => ");
         SET_CR_ZERO

         (*test_function)();

         GET_CR(local_cr);

         /* note: the bcd instructions are hard wired to use cr6. */
         local_crf = extract_cr_rn(local_cr, 6);
         dissect_cr_rn(local_cr, 6);
         printf(" (%x)", local_crf);

         if (cr_overflow_set(local_crf)) {
            /* If overflow (S0)  is set the results are undefined.  Force the
             * output to print as zeros so we have consistent results for
             * comparison.
             */
            printf(" xt:%016lx %016lx", 0UL, 0UL);

         } else {
            testfunction_bcd_display_outputs(instruction_name);
         }

         printf("\n");
      } // j = xb loop.

   /* Since the bcdct* convert_tofrom instructions do not use the xa
    * field, we will short-circuit the xa (i=*) loop here.
    */
    if (convert_tofrom_instruction(instruction_name))
       i = nb_packed_decimal_entries;
   } //i = xa loop.
}


static void testfunction_noop_misc (const char* instruction_name,
                                    test_func_t test_function,
                                    unsigned int ignore_test_flags)
{
   VERBOSE_FUNCTION_CALLOUT

   printf("%s ",  instruction_name);
   printf(" =>");

   (*test_function)();

   printf("\n");
}

static void testfunction_pc_immediate_misc (const char* instruction_name,
                                    test_func_t test_function,
                                    unsigned int ignore_test_flags)
{
   VERBOSE_FUNCTION_CALLOUT

   for (x_index = 0; x_index < 16; x_index++) {
      printf("%s ",  instruction_name);
      printf(" %016x ", x_index);
      printf(" => ");
      (*test_function)();
      /*      printf(" %016lx\n", r14); */
      printf(" %016x\n", 0);  /* test is not portable just print zero */
   }
}

/* Identify those mffs* variants that take additional arguments.
 * This includes the immediate mffs*i variants. */
#define is_not_simple_mffs_instruction(instruction_name) \
	( (strncmp(instruction_name,"mffscdrn",8)==0) || \
	  (strncmp(instruction_name,"mffscrn",7)==0) )

/* Because some of the mffs* variants here are updating the fpscr as part
 * of the read, be sure to dissect both the retrieved (f14) and the updated
 * (local_fpscr) fpscr values. */
static void testfunction_mffs(const char* instruction_name,
                                    test_func_t test_function,
                                    unsigned int ignore_test_flags)
{
   union reg_t {
      unsigned long int uli;
      double            dble;
   } f14_reg, f15_reg;

   /*This function uses global variable x_shift */
   VERBOSE_FUNCTION_CALLOUT

   if (is_not_simple_mffs_instruction(instruction_name)) {
      /* iterate x_shift across values used for RN,RM */
      for (x_shift = 0; x_shift < 3; x_shift++) {
         printf("%s ",  instruction_name);
         /* make sure bits in f14 get cleared so we can
            see correct resulg*/
         f14_reg.uli = 0x3FFFFFFFFUL;

         if (strcmp("mffscdrn", instruction_name) == 0) {
            /* instruction uses input reg f15 as input for DRN field */
            f15_reg.uli = (unsigned long int)x_shift << 32;
            printf(" f15 0X%lx  ", f15_reg.uli);

            /* Setup input register value */
            f15 = f15_reg.dble;

         } else if (strcmp("mffscrn", instruction_name) == 0) {
            /* instruction uses input reg f15 as input for RN field */
            f15_reg.uli = (unsigned long int)x_shift;
            printf(" f15 0X%lx  ", f15_reg.uli);

            /* Setup input register value */
            f15 = f15_reg.dble;

         } else {
            printf(" %x ", x_shift);
         }


         (*test_function)();
         printf(" => ");
         f14_reg.dble = f14;
         printf(" 0X%lx\n", f14_reg.uli);
         printf(" fpscr: f14 ");
         dissect_fpscr(f14);
         printf(" local_fpscr: ");
         dissect_fpscr(local_fpscr);
         printf("\n");
      }
   } else {
         printf("%s ",  instruction_name);
         printf(" => ");
         (*test_function)();
         printf(" %016f\n", f14);
         printf(" fpscr: f14 ");
         dissect_fpscr(f14);
         printf("\n");
         printf(" local_fpscr: ");
         dissect_fpscr(local_fpscr);
         printf("\n");
   }
}

/* ######## begin grand testing loops. */
typedef struct insn_sel_flags_t_struct {
   unsigned int one_arg, two_args, three_args, four_args, cmp_args, ld_args, st_args,
      one_imed_args;
   unsigned int arith, logical, compare, popcnt, ldst, insert_extract, permute, round;
   unsigned int integer, altivec, altivec_quad, altivec_double, dfp, bcd, misc, mffs,
      no_op, pc_immediate;
   unsigned int cr;
} insn_sel_flags_t;

static void do_tests ( insn_sel_flags_t seln_flags)
{
   test_group_t group_function;
   test_list_t *tests;
   unsigned int nb_args, type, family;
   int i, j, n;

   n = 0;
   group_function = NULL;

   /* self-test of some utility functions. */
   if (verbose > 1) {
      printf("fpscr zero'd out:");
      dissect_fpscr(0);
      printf("\n");
      printf("fpscr all ones:");
      dissect_fpscr(0xffffffffffffffff);
      printf("\n");
      printf("fpscr RN bits:");
      dissect_fpscr_rounding_mode(0x0000000000000003);
      dissect_fpscr_rounding_mode(0x0000000000000002);
      dissect_fpscr_rounding_mode(0x0000000000000001);
      dissect_fpscr_rounding_mode(0x0000000000000000);
      printf("\n");
      printf("XER bits: (64)");
      dissect_xer(0xffffffffffffffff);
      printf("\n");
      printf("XER bits: (32)");
      dissect_xer(0xffffffff);
      printf("\n\n");
   }

   for (i=0; all_tests[i].name != NULL; i++) {
      nb_args = all_tests[i].flags & PPC_NB_ARGS_MASK;
      /* Check number of arguments */
      if ((nb_args == 1 && !seln_flags.one_arg)    ||
          (nb_args == 2 && !seln_flags.two_args)   ||
          (nb_args == 3 && !seln_flags.three_args) ||
          (nb_args == 4 && !seln_flags.four_args)  ||
          (nb_args == 5 && !seln_flags.cmp_args)   ||
          (nb_args == 6 && !seln_flags.ld_args)    ||
          (nb_args == 7 && !seln_flags.st_args)    ||
          (nb_args == 8 && !seln_flags.one_imed_args))
         continue;

      /* Check instruction type */
      type = all_tests[i].flags & PPC_TYPE_MASK;
      if ((type == PPC_ARITH   && !seln_flags.arith)   ||
          (type == PPC_LDST    && !seln_flags.ldst)    ||
          (type == PPC_LOGICAL && !seln_flags.logical) ||
          (type == PPC_COMPARE && !seln_flags.compare) ||
          (type == PPC_POPCNT  && !seln_flags.compare) ||
          (type == PPC_INSERTEXTRACT && !seln_flags.insert_extract))
         continue;

      /* Check instruction family */
      family = all_tests[i].flags & PPC_FAMILY_MASK;

      /* do each check each case individually to reduce computation */
      if (family == PPC_INTEGER  && seln_flags.integer == 0) continue;
      if (family == PPC_ALTIVEC  && seln_flags.altivec == 0) continue;
      if (family == PPC_DFP   && seln_flags.dfp == 0)        continue;
      if (family == PPC_BCD   && seln_flags.bcd == 0)        continue;
      if (family == PPC_NO_OP && seln_flags.no_op == 0)      continue;
      if (family == PPC_MISC  && seln_flags.misc == 0)       continue;
      if (family == PPC_MFFS  && seln_flags.mffs == 0)       continue;
      if (family == PPC_ALTIVEC_DOUBLE  && seln_flags.altivec_double == 0)
         continue;

      if (family == PPC_ALTIVEC_QUAD  && seln_flags.altivec_quad == 0)
         continue;

      if (family == PPC_PC_IMMEDIATE && seln_flags.pc_immediate == 0)
         continue;

      /* Check flags update */
      if (((all_tests[i].flags & PPC_CR)  && seln_flags.cr == 0) ||
          (!(all_tests[i].flags & PPC_CR) && seln_flags.cr == 1))
         continue;

      /* All criteria validation passed, do the tests */
      tests = all_tests[i].tests;

      /* Select the test group */
      switch (family) {
      case PPC_MFFS:
         group_function = &testfunction_mffs;
         break;

      case PPC_INTEGER:
         switch(type) {
         case PPC_ARITH:
            switch(nb_args) {
            case PPC_TWO_ARGS:
               group_function = &testfunction_int_two_args;
               break;

            case PPC_THREE_ARGS:
               group_function = &testfunction_three_args;
               break;

            default:
               printf("ERROR: PPC_INTEGER, unhandled number of arguments. 0x%08x\n",
                      nb_args);
            }
            break;

         case PPC_LOGICAL:
            switch(nb_args) {
            case PPC_ONE_IMM:
               group_function = &testfunction_set_boolean;
               break;

            case PPC_ONE_ARG:
               group_function = &testfunction_logical_one;
               break;

            default:
               printf("ERROR: PPC_LOGICAL, unhandled number of arguments. 0x%08x\n",
                      nb_args);
            }
            break;

         case PPC_COMPARE:
            group_function = &testfunction_char_compare;
            break;

         default:
            printf("ERROR: PPC_INTEGER, unhandled type  0x%08x\n", type);
            continue;
         } /* switch (type) */
         break;

      case PPC_ALTIVEC:
         switch(type) {
         case PPC_ARITH:
            switch(nb_args) {
            case PPC_TWO_ARGS:
               group_function = &testfunction_vector_absolute;

               break;
            default:
               printf("ERROR: PPC_ALTIVEC, PPC_ARITH, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            } /* switch (PPC_ARITH, nb_args) */
            break;

         case PPC_LOGICAL:
            switch(nb_args) {
            case PPC_ONE_IMM:
               group_function = &testfunction_vector_immediate;
               break;

            case PPC_ONE_ARG:
               group_function = &testfunction_vector_logical_one;
               break;

            case PPC_TWO_ARGS:
               group_function = &testfunction_vector_extend_sign;
               break;

            case PPC_THREE_ARGS:
               group_function = &testfunction_vector_three_special;
               break;

            case PPC_FOUR_ARGS:
               group_function = &testfunction_vector_logical_four;
               break;

            case PPC_ONE_GPR_ONE_VEC:
               group_function = &testfunction_gpr_vector_logical_one;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC, PPC_LOGICAL, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_INSERTEXTRACT, nb_args) */
            break;

         case PPC_INSERTEXTRACT:
            switch(nb_args) {
            case PPC_ONE_IMM:
               group_function = &testfunction_vector_insert_or_extract_immediate;
               break;

            case PPC_TWO_ARGS:
               group_function = testfunction_vector_extract;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC, PPC_INSERTEXTRACT, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_INSERTEXTRACT, nb_args) */
            break;

         case PPC_PERMUTE:
            group_function = &testfunction_vector_xxpermute;
            break;

         case PPC_LDST:
            switch(nb_args) {
            case PPC_ONE_IMM:
               /* Register holds immediate length value */
               group_function = &testfunction_vector_scalar_loadstore_length;
               break;

            case PPC_TWO_ARGS:
               /* Register holds address of buffer */
               group_function = &testfunction_vector_loadstore;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC, PPC_LDST, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_LDST, nb_args) */
            break;

         case PPC_POPCNT:
            group_function = &testfunction_vector_count_bytes;
            break;

         default:
            printf("ERROR: PPC_ALTIVEC, unhandled type. %d\n", type);
            continue;
         } /* switch (PPC_ALTIVEC, type) */
         break;

      case PPC_MISC:
         switch(nb_args) {
            case PPC_TWO_ARGS:
               group_function = &testfunction_vectorscalar_move_tofrom;
               break;
            case PPC_THREE_ARGS:
               group_function = &testfunction_one_arg_with_shift;
               break;
            default:
               printf("ERROR: PPC_MISC, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_MISC, nb_args) */
         break;

      case PPC_ALTIVEC_QUAD:
         switch(type) {
         case PPC_LOGICAL:
            switch(nb_args) {
            case PPC_TWO_ARGS:
               group_function = &testfunction_vector_scalar_two_quad;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC_QUAD, PPC_LOGICAL, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_LOGICAL, nb_args) */
            break;

         case PPC_COMPARE:
            group_function = &testfunction_vector_scalar_compare_quads;
            break;

         case PPC_ROUND:
            group_function = &testfunction_vector_scalar_rounding_quads;
            break;

         default:
            printf("ERROR: PPC_ALTIVEC_QUAD, unhandled type. %d\n", type);
            continue;
         } /* switch(type) */
         break;

      case PPC_ALTIVEC_DOUBLE:
         switch(type) {
         case PPC_COMPARE:
            switch(nb_args) {
            case PPC_ONE_ARG:
               group_function = &testfunction_vector_scalar_data_class;
               break;

            case PPC_TWO_ARGS:
               group_function = &testfunction_vector_scalar_two_double;
               break;

            case PPC_COMPARE_ARGS:
               group_function = &testfunction_vector_scalar_compare_double;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC_DOUBLE, PPC_COMPARE, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_COMPARE, nb_args) */
            break;

         default:
            printf("ERROR: PPC_ALTIVEC_DOUBLE, unhandled type. %d\n", type);
            continue;

         }   /* switch(type) */
         break;

      case PPC_DFP:
         group_function = &testfunction_dfp_significance;
         break;

      case PPC_BCD:
         group_function = &testfunction_bcd_misc;
         break;

      case PPC_NO_OP:
         group_function = &testfunction_noop_misc;
         break;

      case PPC_PC_IMMEDIATE:
         group_function = &testfunction_pc_immediate_misc;
         break;

      default:
         printf("ERROR: unknown instruction family %08x\n", family);
         continue;
      } /* switch(family) */

      printf("%s:\n", all_tests[i].name);

      printf("Test instruction group [%s]\n", all_tests[i].name);
      /* Now, spin through all entries in the group_function to
       * run the individual instruction tests.
       */
      for (j = 0; tests[j].name != NULL; j++) {
         if (verbose > 1)
            printf("Test instruction %s\n", tests[j].name);
         (*group_function)(tests[j].name, tests[j].func, all_tests[i].flags);
         printf("\n");
         n++;
      }

      if (verbose) printf("\n");

      printf("All done. Tested %d different instructions\n", n);
   }  /* for (i = 0; all_tests[i].name...) */
}

static void usage (void)
{
   fprintf(stderr,
           "Usage: test_isa_3_0 [OPTIONS]\n"
           "\t-i: test integer instructions (default)\n"
           "\t-a: test altivec instructions\n"
           "\t-d: test altivec double instructions\n"
           "\t-q: test altivec quad instructions\n"
           "\t-D: test DFP instructions\n"
           "\t-B: test BCD instructions\n"
           "\t-N: test No Op instructions\n"
           "\t-P: test PC Immediate Shifted instructions\n"
           "\t-m: test miscellaneous instructions\n"
           "\t-M: test MFFS instructions\n"
           "\t-v: be verbose\n"
           "\t-h: display this help and exit\n"
           );
}

#endif   // HAS_ISA_3_00
int main (int argc, char **argv)
{

#ifndef HAS_ISA_3_00
   printf("NO ISA 3.00 SUPPORT\n");
   return 0;

#else
   insn_sel_flags_t flags;
   int c;


   // Args
   flags.one_arg         = 1;
   flags.two_args        = 1;
   flags.three_args      = 1;
   flags.four_args       = 1;
   flags.cmp_args        = 1;
   flags.ld_args         = 1;
   flags.st_args         = 1;
   flags.one_imed_args   = 1;

   // Type
   flags.arith           = 1;
   flags.logical         = 1;
   flags.compare         = 1;
   flags.ldst            = 1;
   flags.popcnt          = 1;
   flags.insert_extract  = 1;
   flags.permute         = 1;
   flags.round           = 1;

   // Family
   flags.integer         = 0;
   flags.altivec         = 0;
   flags.altivec_double  = 0;
   flags.altivec_quad    = 0;
   flags.dfp             = 0;
   flags.bcd             = 0;
   flags.misc            = 0;
   flags.mffs            = 0;
   flags.no_op           = 0;
   flags.pc_immediate    = 0;

   // Flags
   flags.cr              = 2;

   while ((c = getopt(argc, argv, "ifmadqhvADBMNP")) != -1) {
      switch (c) {
      case 'i':
         flags.integer  = 1;
         break;

      case 'a':
         flags.altivec  = 1;
         break;

      case 'd':
         flags.altivec_double  = 1;
         break;

      case 'q':
         flags.altivec_quad  = 1;
         break;

      case 'D':
         flags.dfp      = 1;
         break;

      case 'B':
         flags.bcd      = 1;
         break;

      case 'm':
         flags.misc     = 1;
         break;

      case 'M':
         flags.mffs     = 1;
         break;

      case 'N':
         flags.no_op    = 1;
         break;

      case 'P':
         flags.pc_immediate = 1;
         break;

      case 'h':
         usage();
         return 0;

      case 'v':
         verbose++;
         break;

      default:
         usage();
         fprintf(stderr, "Unknown argument: '%c'\n", c);
         return 1;
      }
   }

   build_iargs_table();
   build_vsx_table();
   build_float_vsx_tables();
   build_vector_permute_table();
   build_char_table();
   build_char_range_table();
   build_packed_decimal_table();
   build_national_decimal_table();
   build_zoned_decimal_table();
   build_decimal_shift_table();

   if (verbose>2) {
      dump_char_table();
      dump_char_range_table();
      dump_float_vsx_table();
      dump_packed_decimal_table();
      dump_national_decimal_table();
      dump_zoned_decimal_table();
      dump_decimal_shift_table();
      dump_dfp64_table();
      dump_dfp128_table();
   }

   if (verbose > 1) {
      printf("\nInstruction Selection:\n");
      printf("  n_args: \n");
      printf("    one_arg        = %d\n", flags.one_arg);
      printf("    two_args       = %d\n", flags.two_args);
      printf("    three_args     = %d\n", flags.three_args);
      printf("    four_args      = %d\n", flags.four_args);
      printf("    cmp_args       = %d\n", flags.cmp_args);
      printf("    load_args      = %d\n", flags.ld_args);
      printf("    store_args     = %d\n", flags.st_args);
      printf("    one_im_args    = %d\n", flags.one_imed_args);
      printf("  type: \n");
      printf("    arith          = %d\n", flags.arith);
      printf("    logical        = %d\n", flags.logical);
      printf("    popcnt         = %d\n", flags.popcnt);
      printf("    compare        = %d\n", flags.compare);
      printf("    inset/extract  = %d\n", flags.insert_extract);
      printf("  family: \n");
      printf("    integer        = %d\n", flags.integer);
      printf("    altivec        = %d\n", flags.altivec);
      printf("    altivec double = %d\n", flags.altivec_double);
      printf("    altivec quad   = %d\n", flags.altivec_quad);
      printf("    DFP            = %d\n", flags.dfp);
      printf("    BCD            = %d\n", flags.bcd);
      printf("    PC immediate shifted = %d\n", flags.pc_immediate);
      printf("    misc           = %d\n", flags.misc);
      printf("  cr update: \n");
      printf("    cr             = %d\n", flags.cr);
      printf("\n");
      printf("  num args: \n");
      printf("    iargs      - %d\n", nb_iargs);
      printf("\n");
   }

   do_tests( flags );
#endif

   return 0;
}
