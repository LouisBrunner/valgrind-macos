/* HOW TO COMPILE:
 * 64bit build:
 *    gcc -Winline -Wall -g -O -mregnames -maltivec -m64
 */

/*
 * test_isa_3_0.c:
 * Copyright (c) 2016 Carl Love <cel@us.ibm.com>
 * Copyright (c) 2016 Will Schmidt <will_schmidt@vnet.ibm.com>
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
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
#ifdef HAS_ISA_3_0

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
   PPC_MISC           = 0x00080000,
   PPC_FAMILY_MASK    = 0x000F0000,

   /* Flags: these may be combined, so use separate bit-fields. */
   PPC_CR             = 0x01000000,
   PPC_XER_CA         = 0x02000000,
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

static test_list_t testgroup_ia_ops_two[] = {
    { &test_modsw, "modsw" },
    { &test_moduw, "moduw" },
    { &test_modsd, "modsd" },
    { &test_modud, "modud" },
    { NULL       , NULL             },
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

static test_list_t testgroup_vsx_absolute[] = {
   { &test_vabsdub        , "vabsdub"   },
   { &test_vabsduh        , "vabsduh"   },
   { &test_vabsduw        , "vabsduw"   },
   { NULL                 , NULL        },
};

static void test_vpermr(void)
{ /* vector permute right-indexed */
    __asm__ __volatile__ ("vpermr   %0, %1, %2, %3 " : "+v" (vec_xt): "v" (vec_xa), "v" (vec_xb), "v" (vec_xc));
}

/* vector, 3->1 unique; four arguments. xt, xa, xb, xc (xc = permute) */
static test_list_t testgroup_vector_four[] = {
   { &test_vpermr, "vpermr" },
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

#define XXINSERTW(X)   __asm__ __volatile__ ("xxinsertw   %0, %1, %2" : "+wa" (vec_xt) : "wa" (vec_xb), "i"(X));

#define XXEXTRACTUW(X) __asm__ __volatile__ ("xxextractuw %0, %1, %2" : "+wa" (vec_xt) : "wa" (vec_xb), "i"(X));

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

static test_list_t testgroup_vector_logical_one[] = {
   { &test_xxbrh   , "xxbrh"    },
   { &test_xxbrw   , "xxbrw"    },
   { &test_xxbrd   , "xxbrd"    },
   { &test_xxbrq   , "xxbrq"    },
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
   __asm__ __volatile__ ("stxvx %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_stxvh8x(void)  {
   __asm__ __volatile__ ("stxvh8x %x0, 14, 15" : "=wa" (vec_xt));
}

static void test_stxvb16x(void) {
   __asm__ __volatile__ ("stxvb16x %x0, 14, 15" : "=wa" (vec_xt));
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
   __asm__ __volatile__ ("mtvsrws %0, 15" : "=wa" (vec_xt));
};

static test_list_t testgroup_vectorscalar_move_tofrom[] = {
   { &test_mfvsrld, "mfvsrld" }, /* RA, XS */
   { &test_mtvsrdd, "mtvsrdd" }, /* XT, RA, RB */
   { &test_mtvsrws, "mtvsrws" }, /* XT, RA */
   { NULL         , NULL      },
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
      testgroup_three_args,
      "ppc three parameter ops",
      PPC_INTEGER | PPC_ARITH | PPC_THREE_ARGS,
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
      testgroup_vsx_xxpermute,
      "ppc vector permutes",
      PPC_ALTIVEC | PPC_PERMUTE | PPC_THREE_ARGS,
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
      testgroup_vector_loadstore,
      "ppc vector load/store",
      PPC_ALTIVEC | PPC_LDST | PPC_TWO_ARGS,
   },
   {
      testgroup_vectorscalar_move_tofrom,
      "ppc vector scalar move to/from",
      PPC_MISC,
   },
   { NULL,                   NULL,               0x00000000, },
};

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
         res = r17;

         printf("%s %016lx, %016lx => %016lx (%08x)\n",
                instruction_name, (long unsigned)iargs[i],
                (long unsigned)iargs[j], (long unsigned)res,
                cr);
      }
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
                vec_xa[1],vec_xa[0],
                vec_xb[0],vec_xb[1]
                );
         printf(" => ");

         SET_CR_ZERO;

         (*test_function)();

         GET_CR(cr);

         printf(" xt:%016lx %016lx (%08x)\n", vec_xt[0], vec_xt[1], cr);
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
                vec_xa[1], vec_xa[0],
                vec_xt[1], vec_xt[0],
                vec_xb[0], vec_xb[1]);

         SET_CR_ZERO;

         (*test_function)();

         GET_CR(cr);

         printf(" %016lx %016lx (%08x)\n", vec_xt[0], vec_xt[1], cr);

#if defined (DEBUG_VECTOR_PERMUTE)
         printf("DEBUG:%s %016lx %016lx %016lx %016lx, pcv[%016lx %016lx]\n",
                ignore_name,
                vec_xa[0], vec_xa[1],
                vec_xt[0], vec_xt[1],
                vec_xb[0], vec_xb[1]);
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
                vec_xa[0], vec_xa[1],
                vec_xt[0], vec_xt[1]);

         (*test_function)();

         printf(" xt:%016lx %016lx\n",
                vec_xt[0], vec_xt[1]);
      }
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
                   vec_xa[1], vec_xa[0],
                   vec_xb[1], vec_xb[0],
                   vec_xc[0], vec_xc[1]);

            SET_CR_ZERO;

            (*test_function)();

            GET_CR(cr);

            printf(" %016lx %016lx (%08x)\n", vec_xt[0], vec_xt[1], cr);
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
         vec_xb[0] = (unsigned long) vsxargs[i];
         vec_xb[1] = (unsigned long) vsxargs[i+1];

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
                      instruction_name, vec_xb[1], vec_xb[0], x_index,
                      (t == 0 ? " zeros" : "  ones") );

               printf("%016lx %016lx\n", vec_xt[1], vec_xt[0]);
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
                instruction_name, vec_xt[1], vec_xt[0], x_splat);

         (*test_function)();

         printf("%016lx %016lx\n", vec_xt[1], vec_xt[0]);
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
         printf("%016lx %016lx ", vec_xt[1], vec_xt[0]);
         dump_small_buffer();
         printf(" =>\n");

         (*test_function)();

         printf("    %016lx %016lx ", vec_xt[1], vec_xt[0]);
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
         printf("%016lx %016lx %lx %016lx ", vec_xt[1], vec_xt[0],
                (long unsigned)r14,  (long unsigned)r15 );

         (*test_function)();

         printf("=> %016lx %016lx %lx %016lx ", vec_xt[1], vec_xt[0],
                (long unsigned)r14,  (long unsigned)r15 );
         printf("\n");
      }
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

   printf(" xt:%016lx %016lx", vec_xt[0], vec_xt[1] );

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

/* ######## begin grand testing loops. */
typedef struct insn_sel_flags_t_struct {
   int one_arg, two_args, three_args, four_args, cmp_args;
   int arith, logical, compare, popcnt, ldst, insert_extract;
   int integer, floats, p405, altivec, altivec_double, altivec_quad;
   int faltivec, vector, misc, dfp, bcd, no_op, pc_immediate;
   int cr;
} insn_sel_flags_t;

static void do_tests ( insn_sel_flags_t seln_flags)
{
   test_group_t group_function;
   test_list_t *tests;
   int nb_args, type, family;
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
   }

   for (i=0; all_tests[i].name != NULL; i++) {
      nb_args = all_tests[i].flags & PPC_NB_ARGS_MASK;
      /* Check number of arguments */
      if ((nb_args == 1 && !seln_flags.one_arg)    ||
          (nb_args == 2 && !seln_flags.two_args)   ||
          (nb_args == 3 && !seln_flags.three_args) ||
          (nb_args == 4 && !seln_flags.four_args)  ||
          (nb_args == 5 && !seln_flags.cmp_args))
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
      if ((family == PPC_INTEGER  && !seln_flags.integer) ||
          (family == PPC_ALTIVEC  && !seln_flags.altivec) ||
          (family == PPC_MISC  && !seln_flags.misc))
         continue;

      /* Check flags update */
      if (((all_tests[i].flags & PPC_CR)  && seln_flags.cr == 0) ||
          (!(all_tests[i].flags & PPC_CR) && seln_flags.cr == 1))
         continue;

      /* All criteria validation passed, do the tests */
      tests = all_tests[i].tests;

      /* Select the test group */
      switch (family) {
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

         default:
            printf("ERROR: PPC_INTEGER, unhandled type  0x%08x\n", type);
            continue;
         } /* switch (nb_args) */
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

            case PPC_FOUR_ARGS:
               group_function = &testfunction_vector_logical_four;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC, PPC_LOGICAL, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_LOGICAL, nb_args) */
            break;

         case PPC_INSERTEXTRACT:
            switch(nb_args) {
            case PPC_ONE_IMM:
               group_function = &testfunction_vector_insert_or_extract_immediate;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC, PPC_INSERTEXTRACT, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_ALTIVEC, nb_args) */
            break;

         case PPC_PERMUTE:
            group_function = &testfunction_vector_xxpermute;
            break;

         case PPC_LDST:
            switch(nb_args) {
            case PPC_TWO_ARGS:
               /* Register holds address of buffer */
               group_function = &testfunction_vector_loadstore;
               break;

            default:
               printf("ERROR: PPC_ALTIVEC, PPC_LDST, unhandled number of arguments. 0x%08x\n", nb_args);
               continue;
            }  /* switch(PPC_LDST, nb_args) */
            break;

         case PPC_MISC:
            group_function = &testfunction_vectorscalar_move_tofrom;
            break;

         default:
            printf("ERROR: PPC_ALTIVEC, unhandled type. %d\n", type);
            continue;
         } /* switch (PPC_ALTIVEC, type) */
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
           "\t-m: test miscellaneous instructions\n"
           "\t-v: be verbose\n"
           "\t-h: display this help and exit\n"
           );
}

#endif   // HAS_ISA_3_0
int main (int argc, char **argv)
{

#ifndef HAS_ISA_3_0
   printf("NO ISA 3.0 SUPPORT\n");
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

   // Type
   flags.arith           = 1;
   flags.logical         = 1;
   flags.popcnt          = 1;
   flags.compare         = 1;
   flags.ldst            = 1;
   flags.insert_extract  = 1;

   // Family
   flags.integer         = 0;
   flags.misc            = 0;
   flags.altivec         = 0;

   // Flags
   flags.cr              = 2;

   while ((c = getopt(argc, argv, "ifmadqhvADBNP")) != -1) {
      switch (c) {
      case 'i':
         flags.integer  = 1;
         break;

      case 'a':
         flags.altivec  = 1;
         break;

      case 'm':
         flags.misc     = 1;
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
      printf("  type: \n");
      printf("    arith          = %d\n", flags.arith);
      printf("    logical        = %d\n", flags.logical);
      printf("    popcnt         = %d\n", flags.popcnt);
      printf("    compare        = %d\n", flags.compare);
      printf("    inset/extract  = %d\n", flags.insert_extract);
      printf("  family: \n");
      printf("    integer        = %d\n", flags.integer);
      printf("    altivec        = %d\n", flags.altivec);
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
